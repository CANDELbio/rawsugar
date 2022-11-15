(ns org.parkerici.rawsugar.glue.cellengine
  (:require [clj-http.client :as client]
            [clj-http.cookies :as cookies]
            [org.parkerici.rawsugar.sheet :as sheet]
            [org.parkerici.rawsugar.blob :as blob]
            [org.parkerici.rawsugar.datomic :as d]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.rawsugar.updown :as updown]
            [taoensso.timbre :as log]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [org.parkerici.rawsugar.slack :as slack]
            [org.parkerici.multitool.core :as u]))

"
CELLENGINE GLUE:

cellengine->sheet
Downloads annotations table from cellengine experiment,
and uploads as a sheet to a project, then downloads Fcs files from experiment,
uploads to project and matches to newly made sheet of annotations.

sheet->cellengine
Take a sheet from rawsugar and uploads to cellengine as a new experiment.
Creates a new experiment, downloads fcs files and uploads them to the experiment.
Pulls the metadata from the sheet and attaches to the fcs files in the experiment as annotations
"


; Initialize the cookie storage as a global variable
;; TODO use bindings instead of this global var.
(def cookie-store (clj-http.cookies/cookie-store))

;; TODO use api-post function to reduce repeated code
(defn get-cellengine-cookie
  ; Get authentication cookie from cellengine and set
  ; to the cookie-store for the session.
  [username password]
  (client/post
    "https://cellengine.com/api/v1/signin"
    {:content-type :json
     :cookie-store cookie-store
     :form-params  {:username username :password password}}))

(defn check-cookie-exists
  "See if there is a cookie in the cookie-store"
  []
  (contains? (cookies/get-cookies cookie-store) "token"))

(defn authenticate
  "Login to cellengine"
  [username password]
  ;(def cookie-store (clj-http.cookies/cookie-store))
  (get-cellengine-cookie username password)
  ;(clojure.pprint/pprint (clj-http.cookies/get-cookies cookie-store))
  )

(defn api-get
  "GET a url. Handles fetching cookies for the session
  Set args to {:as :json} to get in json format"
  ([url]
   (api-get url {:accept :json}))
  ([url args]
   (check-cookie-exists)
   (client/get url (merge args {:cookie-store cookie-store}))))

(defn api-patch
  "PATCH a url. BODY takes in a map which will get converted to a json."
  [url body]
  (check-cookie-exists)
  (client/patch
    url {:cookie-store cookie-store
         :as           :json
         :content-type :json
         :body         (json/write-str body)}))

(defn api-post
  "POST a url.
  To upload an fcs,
  Takes {:multipart [{:name \"file\" :content \"(io/file \"<PATH>\")}
                     {:name \"Content/type\" :content \"multipart/form-data\"}]} "
  [url params]
  (check-cookie-exists)
  (client/post url (merge params {:cookie-store cookie-store})))

(defn parse-response
  "Returns the body of a response"
  ;; TODO: add error checks.
  [res]
  (res :body))



;;; ⩏⩎⩎ API Calls ⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏

(def ce-base-url "https://cellengine.com/api/v1")

(defn- get-workspaces
  "Get all workspaces user is authorized to read
  on cellengine"
  []
  (parse-response
    (api-get (format "%s/experiments" ce-base-url)
             {:as :json})))

(defn- get-fcs-files
  "Gets a list of fcs-file entities"
  [expt-id]
  (parse-response
    (api-get
      (format
        "%s/experiments/%s/fcsfiles" ce-base-url expt-id)
      {:as :json :query-params {"includeControls" true}
       })))

(defn- download-fcs
  "Gets the actual fcs file."
  [expt-id fcs-file-id save-file-as]
  (let [body
        (parse-response
          (api-get
            (format
              "%s/experiments/%s/fcsfiles/%s.fcs" ce-base-url expt-id fcs-file-id) {:accept :json :as :byte-array :query-params {"original" true}}))]
    ;; TODO: Add query parameters like {:query-params {"postSubsampleN" 1000} } to options
    (with-open [w (io/output-stream save-file-as)]
      (.write w body))))

(defn- patch-fcs
  "Update FCS metadata.
  BODY is a map containing field to update.
  Example: {:annotations [{:name 'MyColumnA' :value '100'}]}
  "
  [expt-id fcs-file-id body]
  (api-patch
    (format "%s/experiments/%s/fcsfiles/%s" ce-base-url expt-id fcs-file-id) body))

(defn fcs->cellengine
  "Upload an fcs file to cellengine"
  [expt-id file]
  (let [multipart {:multipart [{:name "file" :content (io/file file)}
                               {:name "Content/type" :content "multipart/form-data"}]
                   :as :json}
        url (format "%s/experiments/%s/fcsfiles" ce-base-url expt-id)]
    (api-post url multipart)))



;;; ⩏⩎⩎ Cellengine ⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏

(defn- get-workspace-by-name
  "Get a workspace entity by entity name"
  [workspace-name]
  (let
    [ws (get-workspaces)]
    (some
      #(if (= workspace-name (% :name)) %) ws)))

(defn- workspace-exists?
  [workspace-name]
  (some?
    (get-workspace-by-name workspace-name)))

(defn workspace-id-exists?
  [workspace-id]
  (contains? (set (map :_id (get-workspaces))) workspace-id))

(defn get-workspace-id-by-name
  "Get a workspace id by workspace name; error if not found"
  [workspace-name]
  (if-let [workspace (get-workspace-by-name workspace-name)]
    (workspace :_id)
    (throw (ex-info (str "Can't find Cellengine workspace named " workspace-name) {:name workspace-name}))))

(defn- get-workspace-name-by-id
  ;; TODO: Does not currently work with revision IDs
  [workspace-id]
  (let [ws (get-workspaces)]
    ((some
       #(if (= workspace-id (% :_id)) %) ws) :name)))

(defn- get-fcs-by-name
  "Takes in a list of fcs entities and a filename.
  Returns a single fcs entity."
  [fcs-list fcs-name]
  (first
    (filter #(= fcs-name (% :filename)) fcs-list)))

(defn- get-fcs-id-by-name
  "Takes in a list of fcs entities and a filename.
  Returns an id"
  [fcs-list fcs-name]
  ((get-fcs-by-name fcs-list fcs-name) :_id))

(defn- create-ce-experiment
  "Create a new cellengine experiment"
  [experiment-name]
  (if (string? experiment-name)
    (if-not (workspace-exists? experiment-name)
      (api-post
        (format "%s/experiments" ce-base-url)
        {:content-type :json :form-params {:name experiment-name}}) ;;TODO: Option to change :primaryResearcher :uploader :comments
      (throw (IllegalArgumentException. (str "Cellengine experiment exists already!: " experiment-name))))
    (throw (IllegalArgumentException. (str "Argument must be a string: " experiment-name)))))

;;; ⩏⩎⩎ Munge ⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏

(defn- fcs->annotation-table
  "Convert a list of fcs entities to a list of
  annotations"
  [fcs-entity]
  (let
    [annotation (fcs-entity :annotations)                   ;; get annotations
     annotation (into {} (map #(hash-map (% :name) (% :value)) annotation))] ;; transform to key value pairs
    (merge
      annotation
      {"Filename"     (fcs-entity :filename)
       "isControl"    (fcs-entity :isControl)
       "CellengineID" (fcs-entity :_id)})))                 ;; add filename and isControl boolean to annotations

(defn- write-csv
  "Used to write annotation-table to csv file.
  Takes row-data in format [{colname value colname2 value}]"
  [path row-data]
  (let [columns (distinct (flatten (map keys row-data)))
        headers (map name columns)
        rows (mapv #(mapv % columns) row-data)]
    (with-open [file (io/writer path)]
      (csv/write-csv file (cons headers rows) :separator \,))))

(defn- empty-annotation?
  "Is the annotation entity empty?"
  [n] (= (:value n) ""))

(defn- filename-check
  "strip leading / character from filename"
  [filename]
  (if (= (get filename 0) \/) (subs filename 1) filename))

;;; ⩏⩎⩎ Upload ⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏

(defn- upload-annotations
  "Write a csv file of annotations and upload
   to project as a new sheet."
  [PROJECT BATCH annotations new-sheet-name]
  (d/wrap-datomic-fn
    #(let
       [csvfile-name (str new-sheet-name ".csv")
        existing-sheet? (sheet/sheet-exists? BATCH new-sheet-name)]
       (if existing-sheet?
         (do
           (log/warn "SHEET EXISTS, SKIPPING SHEET UPLOAD")
           existing-sheet?)
         (do
           (write-csv
             csvfile-name annotations)
           (blob/check-path csvfile-name)
           (first (updown/upload-sheets PROJECT BATCH csvfile-name))))
       ; TODO: don't download csv, upload directly to rawsugar.
       )))

(defn match-fcs-file-to-cellengine-id-column
  "Matches an fcs file ent to a sheet by the Cellengine ID column
  ce-id: CellengineID of the file.
  "
  [sheet file-ent ce-id]
  (let [rows (sheet/get-sheet-data sheet :row-id? true)
        ceid-col-id (sheet/sheet-column-entity-id sheet "CellengineID")]
    [file-ent
     (or (some #(when (= ce-id (% ceid-col-id)) %)
               rows)
         (throw (ex-info "Failed find match" {:sheet sheet :file file-ent :ce-id ce-id})))]
    )
  )

(defn upload-fcs
  "Upload a local fcs file to project and match to sheet"
  [PROJECT BATCH SHEET fcs-file options]
  (try
    (let [ent (d/wrap-datomic-fn #(updown/upload PROJECT BATCH fcs-file "" options))] ;HHH
      (when SHEET
        ;; TODO re-examine file matching
        (d/wrap-datomic-fn #(sheet/transact-matches
                             PROJECT
                             (sheet/match-files-columns SHEET [ent] [(sheet/lookup-column SHEET "Filename")])
                             [SHEET :cli-upload]))))))


; ⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏
; ALL WRAPPED UP INTO ONE FUNCTION;
(defn upload-cellengine-project
  "Downloads annotations table from cellengine experiment,
  and uploads as a sheet to a project, then downloads Fcs files from experiment,
  uploads to project and matches to newly made sheet of annotations."
  [username password ce-experiment-name project batch options]
  (do
    (let
      [auth (authenticate username password)
       workspace-id (get-workspace-id-by-name ce-experiment-name)
       new-sheet-name (str (name (u/keyword-safe ce-experiment-name)) "_cellengine")
       fcs (get-fcs-files workspace-id)
       annotations (map fcs->annotation-table fcs)]
      (let
        ;; TODO: skip upload annotations if exist already.
        [sheet (upload-annotations project batch annotations new-sheet-name)]
        (doseq
          [f fcs]
          ;; TODO: Check md5 of fcs (included in fcs entity from CE) and compare to rawsugar before downloading.
          (log/infof "Downloading %s" (f :filename))
          (download-fcs workspace-id (f :_id) (f :filename))
          (upload-fcs project batch sheet (f :filename) options)
          (io/delete-file (f :filename)))))))


(defn- rs-fcs->ce-fcs
  "Download fcs files associated with a sheet and upload them to an existing cellengine experiment"
  [SHEET CEPROJECT]
  (d/wrap-datomic-fn
    #(let [ce-expt-id (get-workspace-id-by-name CEPROJECT)
           entids (map :db/id (sheet/sheet-files SHEET))    ; TODO: Add datalog filter.
           ents (map (fn [fileid] (d/get-entity fileid)) entids)
           ents (remove (fn [x] (not= (:file/extension x) "fcs")) ents) ;filter only fcs files.
           ]
       (doseq [ent ents]
         (blob/download ent "." {})
         (log/infof "Uploading %s" (:file/pathname ent))
         (fcs->cellengine ce-expt-id (filename-check (:file/pathname ent))) ; TODO: check it doesn't already exist. Duplicate names could funk things up.
         (io/delete-file (filename-check (:file/pathname ent)))))))



(defn- rs-sheet->ce-annotations
  "Download a sheet and upload the metadata to a cellengine experiment with matching fcs files as annotations"
  [SHEET CEPROJECT]
  (let [sheet-data (d/wrap-datomic-fn #(sheet/get-sheet-data SHEET :files? true :row-id? true :column-format :name))
        ce-expt-id (get-workspace-id-by-name CEPROJECT)
        fcs (get-fcs-files ce-expt-id)]
    (doseq [row-data sheet-data
            :when (some #(= (:file/extension %) "fcs") (:files row-data))]
      (let [ann (mapv (fn [[x y]] (hash-map :name x :value y)) (dissoc row-data :files)) ; format for cellengine annotations
            ann (remove empty-annotation? ann)              ; TODO; move "isControl" from :annotations to :isControl
            fcs-files (remove (fn [x] (not= (:file/extension x) "fcs")) (:files row-data)) ; get fcs files
            filenames (map :file/pathname fcs-files)        ; get pathnames
            filenames (map filename-check filenames)        ; filter stupid names (remove leading /)
            body (hash-map :annotations ann)]               ; construct body of annotations for POST
        (doseq [filename filenames]
          (log/infof "Uploading Annotation: %s" filename)
          (patch-fcs
            ce-expt-id
            (get-fcs-id-by-name fcs filename)
            body)
          )))))

(defn cli-upload-sheet->cellengine
  "Take a sheet from rawsugar and uploads to cellengine as a new experiment.
  Creates a new experiment, downloads fcs files and uploads them to the experiment.
  Pulls the metadata from the sheet and attaches to the fcs files in the experiment as annotations
  "
  [ce-username ce-password CE-PROJECT RS-PROJECT RS-BATCH RS-SHEET]
  (do
    (authenticate ce-username ce-password)
    (create-ce-experiment CE-PROJECT)
    (rs-fcs->ce-fcs RS-SHEET CE-PROJECT)
    (log/infof "Transferring Annotations")
    (rs-sheet->ce-annotations RS-SHEET CE-PROJECT)
    (log/infof "DONE Transferring Annotations")))

(defn- files->cellengine
  "Download fcs files from a file-map and upload them to an existing cellengine experiment"
  [list-of-files cellengine-experiment-name]
  (let [ce-expt-id (get-workspace-id-by-name cellengine-experiment-name)
        list-of-fcs-files (remove (fn [x] (not= (:file/extension x) "fcs")) list-of-files) ;filter only fcs files.
        ]
    (for [file list-of-fcs-files]
      (let [_ (blob/download file "." {})
            _ (log/infof "Uploading %s" (:file/pathname file))
            response (fcs->cellengine ce-expt-id (filename-check (:file/pathname file)))
            fcs-file-id (get-in response [:body :_id])]
        (io/delete-file (filename-check (:file/pathname file)))
        fcs-file-id)
      )))

(defn- annotate-fcs-file-with-rowdata
  "Add annotations to an fcs file on cellengine with annotations from a rawsugar row"
  [row fcs-file-id cellengine-experiment-name]
  (let [ce-expt-id (get-workspace-id-by-name cellengine-experiment-name)
        ann (mapv (fn [[x y]] (hash-map :name x :value y)) (dissoc row :files "row-id")) ; format for cellengine annotations
        ann (remove empty-annotation? ann)                  ; TODO; move "isControl" from :annotations to :isControl
        body (hash-map :annotations ann)]                   ; construct body of annotations for POST
    (log/infof "Annotating %s" fcs-file-id)
    (patch-fcs
      ce-expt-id
      fcs-file-id
      body)
    ))


(defn sheet-files->cellengine
  [SHEET cellengine-experiment-name]
  (let [
        sheet-data (sheet/get-sheet-data SHEET :files? true :row-id? true :column-format :name)
        ;; TODO Cellengine API for updating permissions is still WIP. Only pici-engineering will have access to new expts by default.
        ;; TODO following line looks wrong, Robin pinged.
        workspace_ (if workspace-exists? nil (create-ce-experiment cellengine-experiment-name))
        slack-id (slack/slack-id)]
    (future
      (try
        (doseq [row sheet-data
                :let [files (row :files)
                      fcs-ids (files->cellengine files cellengine-experiment-name)]]
          (if (empty? fcs-ids) nil (doseq [fcs-id fcs-ids] (annotate-fcs-file-with-rowdata row fcs-id cellengine-experiment-name) ))
          )
        (slack/post-message (format "Hey %s, your transfer of fcs files to %s is complete!
         If you cannot see this workspace in cellengine, you may not have permission to view it.
         Please contact an admin to get permission transfered to your cellengine account." slack-id cellengine-experiment-name))
        (catch Throwable e
          (slack/post-message (format "Error! %s %s, your transfer failed :cry: "
                                      slack-id (.getMessage e)))))
      )
    ))

;;; TODO use Multitoool
(defn collecting
  "Exec is a fn of one argument, which is called and passed another fn it can use to collect values; the collection is returned."
  [exec]
  (let [acc (atom [])
        collect #(swap! acc conj %)]
    (exec collect)
    @acc))

(defn cellengine-files->sheet
  [project batch cellengine-experiment-id]
  (let [new-sheet-name (str (name (u/keyword-safe (get-workspace-name-by-id cellengine-experiment-id)))
                            "_cellengine")
        all-fcs (get-fcs-files cellengine-experiment-id)
        not-deleted-fcs (filter #(nil? (:deleted %)) all-fcs)
        annotations (map fcs->annotation-table not-deleted-fcs)
        sheet (upload-annotations project batch annotations new-sheet-name)
        slack-id (slack/slack-id)]
    (slack/post-message (format "%s, your transfer of %s is starting"
                                slack-id new-sheet-name))
    (future
      (try
        (let [matches
              (collecting
               (fn [collect]
                 (doseq [fcs not-deleted-fcs]
                   (download-fcs cellengine-experiment-id (fcs :_id) (fcs :filename)))
                 (d/wrap-datomic-fn
                  #(doseq [fcs not-deleted-fcs]
                   (let [ent (updown/upload project batch (fcs :filename) "" nil)
                         match (match-fcs-file-to-cellengine-id-column sheet ent (fcs :_id))]
                     (collect match))))))]
          (d/wrap-datomic-fn
           #(sheet/transact-matches project sheet matches (str new-sheet-name " + matches") [:sheet sheet]))
          (doseq [fcs not-deleted-fcs]
            (io/delete-file (fcs :filename)))
          (slack/post-message (format "hey %s, your transfer of %s is complete!" slack-id new-sheet-name)))
        (catch Throwable e
          (slack/post-message (format "error! %s %s, your transfer failed :cry: "  slack-id (print-str e)))
          )))))

