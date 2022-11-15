(ns org.parkerici.rawsugar.updown
  (:require [me.raynes.fs :as fs]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [inflections.core :as inflect]
            [org.parkerici.rawsugar.excel :as excel]
            [org.parkerici.rawsugar.blob :as blob]
            [org.parkerici.rawsugar.files :as files]
            [org.parkerici.rawsugar.datomic :as d]
            [org.parkerici.rawsugar.history :as h]
            [org.parkerici.rawsugar.sheet :as sheet]
            [org.parkerici.rawsugar.slack :as slack]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.multitool.cljcore :as ju]
            [taoensso.timbre :as log])
  (:import java.util.UUID)
  )

;;; This file exists because otherwise blob and sheet would be codependent which clojure does not allow.
;;; IOW, this contains exactly those fns that need to depend on both blob and sheet.

;; Heuristic hack to  pull the right temp id out of txn results
(defn- get-tempid
  [txn-results]
  (some (fn [[k v]] (and (number? k) v)) (:tempids txn-results)))

(defn upload
  "Upload a file.
  file-path: local or gs: file-path
  base: the base directory aka prefix to be removed
  damp-run: when true, skip actual upload, just create file entity
  resume: check if file with same hash exists and if so use it.
  name: the name to use for the file (default is to use the file's real name)
  Returns: the created (or pre-existing) entity.
  "
  [project-id batch-id file-path base {:keys [damp-run resume name]}]
  (when-not damp-run
    (when-not (blob/exists? file-path)
      (throw (ex-info (str "File " file-path " not found.") {:type :not-found
                                                            :file file-path}))))
  ;; TODO reexamine resume logic
  (if-let [exists
           (and resume
                (files/file-with-hash project-id (blob/file-hash file-path)))]
    (do
      (log/warnf "File %s already uploaded, skipping" exists)
      (first exists))
    (do
      (log/infof "Uploading %s" file-path)
      (let [file-name (or name file-path)
            orig-fname (fs/base-name file-name)
            relative-path (if base (subs file-name (count base)) file-name) ;TODO should check that prefix is equal
            extension (blob/extended-extension file-name)
            key (UUID/randomUUID)
            location (blob/generate-path project-id batch-id key orig-fname)
            ;; Actually do the copy
            hash (if damp-run
                   "fake-hash" ; was (file-hash file-path), but that takes too long!
                   (blob/copy (blob/get-absolute-file file-path) location))
            existing (files/file-with-relative-path batch-id relative-path)
            blob (blob/blob location)
            do-it (fn []
                    (let [new (u/clean-map            
                               {:db/id (:db/id existing) 
                                :batch/_files batch-id
                                :file/pathname relative-path
                                :file/extension extension
                                :file/location location
                                :file/created (java.util.Date. (.getCreateTime blob))
                                :file/size (.getSize blob)
                                }
                               nil?)
                          txn [(if hash (assoc new :file/hash hash) new)]
                          ;; TODO probably want to combine multiple files into one transaction
                          tx-results (h/transact [project-id :upload-file file-name]
                                                 txn)
                          ent (or existing (assoc new :db/id (get-tempid tx-results)))
                          aug-ent (if (blob/local-path? file-path)
                                    (assoc ent :local-file file-path)
                                    ent)
                          ]
                      aug-ent))
            ]
        ;; TODO HHH this doesn't respect resume option, not sure it should, argh 
        (if existing
          (if (= hash (:file/hash existing))
            (do
              (log/warnf "File already uploaded: %s" relative-path)
              existing)
            (do
              (log/warnf "Writing a new version of %s" relative-path)
              (do-it)))
          ;; Doesn't exist
          (do-it))))))


(def upload-in-background-threshold 35)

;;; TODO discipline around directory separators
(defn upload-directory
  "Returns a seq of entity ids. Directory is a path; can be to single file.
  OR if continuation is supplied, calls it on the above seq. For use in background tasks."

  [project batch directory {:keys [continuation] :as options}]
  (if (blob/is-directory? directory)
    (let [files (blob/content-files directory options)]
      (if (and continuation (> (count files) upload-in-background-threshold))
        (slack/start-background-task
         (format "upload from %s" directory)
         (fn []
           (:message
            (continuation
             (doall
              (for [file files]
                (upload project batch file directory options)))))))
        ((or continuation identity)
         (doall
          (for [file files]
            (upload project batch file directory options))))))
    ;; A single file (TODO does this really have to be a separate branch?)
    (let [file directory
          directory (subs directory 0 (- (count directory) (count (fs/base-name directory))))]
      (list (upload project batch file directory options)))))


;;; ⩽⪀ file input ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾

;;; TODO does anything check for duplicate (or otherwise hinky) column names?
(defn- csv-coerce-values
  [csv-data]
  (cons (first csv-data)
        (map (fn [row] (map u/coerce-numeric row)) (rest csv-data))))

(defn- csv-data->map
  [csv-data]
  (let [cols (first csv-data)] ;; First row is the header
    {:columns cols
     :data (doall
            (map zipmap
                 (->> cols 
                      repeat)
                 (rest csv-data)))}))

(defn- read-text-file
  [fname separator]
  (with-open [reader (io/reader fname)]
    (->> (csv/read-csv reader :separator separator)
         csv-coerce-values              ;if -s flag is set, we co coerce and then uncoerce, which is inefficient but works
         csv-data->map
         )))

;;; Simplest thing: https://github.com/clj-commons/ordered.
;;; Better would be to have a record or something
;;; Change: sheets are map {:columns [] :data [{}]}

(defn- file-sheets
  "Returns a map of sheetnames to data. file-path is actual path to file (gs: or local), filename is optional name to use for sheets"
  [file-ent & [filename]]
  (let [filename (or filename (:file/pathname file-ent))
        local-file (blob/localize-file file-ent) 
        extension (last (str/split filename #"\."))
        base-filename (fs/base-name filename true)]
    (case extension
      "tsv" {base-filename (read-text-file local-file \tab)}
      "txt" {base-filename (read-text-file local-file \tab)} ;interpret .txt as .tsv
      "csv" {base-filename (read-text-file local-file \,)}
      "xls" (excel/read-excel-file local-file filename)
      "xlsx" (excel/read-excel-file local-file filename)
      "xlsm" (excel/read-excel-file local-file filename)
      (throw (ex-info (str "Can't handle files of type " extension)
                      {:type :user-input
                       :file filename}
                      )))))

;;; ⩽⪀ upload ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾⩽⪀ ⩿⩾

(defn extract-sheets
  "Pull sheet data out of an uploaded file and creates them, attaching to batch. Returns seq of sheet ids"
  ;; Accepts :strings? or :strings for cli convenience
  [project-id batch-id file-id & {:keys [file strings? strings filename sheet-name]}] ;TODO -name consistency
  (let [file-ent (if (map? file-id) file-id (d/get-entity file-id))
        file (or file (:file/location file-ent))
        filename (or filename (:file/pathname file-ent))
        sheets (file-sheets file-ent filename)]
    (when (empty? sheets)                 
      (throw (ex-info "No valid sheets found" {:file file}))) ;TODO probably wrong for bulk uploads? Should just be no-op for non-sheet files?
    (when (and sheet-name (> (count sheets) 1))
      (throw (ex-info "More than one sheet found when not expected" {:file file :sheets (keys sheets)})))
    (doseq  [[innate-sheet-name _] sheets
             :let [sheet-name (or sheet-name innate-sheet-name)]]
      (sheet/check-not-exists batch-id sheet-name))
    (apply
     concat
     (for [[innate-sheet-name data] sheets
           :let [sheet-name (or sheet-name innate-sheet-name)]]
       ;; Transact, Return the ids of created sheets
       (map
        second
        (filter (fn [[var _]]
                  (re-matches #"new_obj_sheet_.*" var))
                (:tempids
                 (h/transact [project-id :upload-sheets file-ent sheet-name]
                             (sheet/create-sheet-txn project-id batch-id sheet-name data (:db/id file-ent) strings?)))))))))

;;; Called from CLI and cellengine
(defn upload-sheets
  "Upload a sheet file, creating a new sheet[s] in project/batch.
  Options:
  strings? or strings: take values as strings rather than attempting to coerce to numbers (accepts either key for cli convenience)
  filename: name to use for for file entity
  sheet-name: name to use for sheet
  Returns: seq of sheet ids."

  ;; Accepts :strings? or :strings for cli convenience
  [project-id batch-id file-path & [{:keys [strings? strings filename]}]]
  (let [file-ent (upload project-id batch-id file-path nil {:name (or filename file-path)})]
    (extract-sheets project-id batch-id file-ent :strings? (or strings? strings))))

;;; Called from CLI
;;; TODO what if name of sheet has changed, sigh.
;;; Returns id of new sheet
(defn update-sheet-from-file
  [project batch sheet file]
  (let [file-ent (upload project batch file nil {})
        file-sheets (file-sheets file-ent)
        new-sheet (if (= 1 (count file-sheets))
                   (first (vals file-sheets))
                   (get file-sheets sheet))
        new-data (:data new-sheet)]
    (sheet/update-sheet-data project sheet new-data file)))

;;; TODO Clojure heresy, but I need and want circular namespace refs and this is the ugly way to make them work
;;; this should at least be in a utility ns I suppose.
(defn do-op
  [params]
  ((resolve 'org.parkerici.rawsugar.ops/do-op) params))

;;; TODO will make two sheets...probably not what a user would expect
;;; Should be a way to actually update a sheet in place for such special occasions
;;; Called from CLI, mostly redundant with upload-sheet-and-files
(defn upload-and-match
  [project batch sheet directory column options]
  (let [files (upload-directory project batch directory options)] 
    (when sheet
      (d/wrap-datomic-fn
       (fn []
         (let [old-sheet-name (:sheet/name (d/get-entity sheet '[:sheet/name]))
               new-sheet-name (or (:sheet-name options) (str old-sheet-name "-matched"))
               op-results
               (do-op
                {:op :match-files
                 :project project
                 :batch batch
                 :sheet sheet
                 :files files ;TODO expects entities, probably shouldn't
                 :columns nil
                 :new-sheet-name new-sheet-name}
                )
               ]
           (log/infof (or (:message op-results)
                          (:error op-results)))))))))

;;; TODO RRR express as 2 separate sheet ops (I guess)
;;; Oddly this did not exist. TODO bring out to CLI

;;; NEW, go through ops system, simplifies things
(defn upload-sheet-and-files
  "See upload-sheets; this also will upload a directory of data files, and try to match them."
  [project batch sheet-file directory options]
  (let [sheets (upload-sheets project batch sheet-file options)
        files (when directory (upload-directory project batch directory options))
        sheet (first sheets)]            ;TODO errcheck
    (log/infof "%s; %s uploaded" (inflect/pluralize (count sheets) "sheet") (inflect/pluralize (count files) "file"))
    (d/wrap-datomic-fn
     (fn []
       (let [upload-sheet-name (:sheet/name (d/get-entity sheet '[:sheet/name]))
             new-sheet-name (or (:match-sheet-name options) (str upload-sheet-name "-matched"))
             op-results
             (do-op
              {:op :match-files
               :project project
               :batch batch
               :sheet sheet
               :files files ;TODO expects entities, probably shouldn't
               :columns nil
               :new-sheet-name new-sheet-name}
              )
             new-sheet (get-in op-results [:tempids "new_obj_sheet"])]
         (log/infof (or (:message op-results)
                        (:error op-results)))
         (list new-sheet))))))

(defn upload-api
  [request]
  (let [{:keys [project batch name sheet]} (:params request)
        file (fs/temp-file "rawsugar-api")]
    (io/copy (:body request) file)
    (upload project batch (str file) nil {:name name :sheet? sheet})))
