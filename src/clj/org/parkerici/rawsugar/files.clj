(ns org.parkerici.rawsugar.files
  (:require [org.parkerici.rawsugar.datomic :as d]
            [org.parkerici.rawsugar.history :as h]
            [org.parkerici.rawsugar.blob :as blob]
            [org.parkerici.rawsugar.sheet :as sheet]
            [me.raynes.fs :as fs]
            [org.parkerici.multitool.core :as u])
  )

;;; Code dealing with Datomic file representation (see blob.clj for stuff dealing with actual file contents)

;;; See batches/batch-files

(defn file-with-hash
  [project hash]
  (first (d/q `[:find (pull ?file [*])
                 :in $ ?project ?hash
                 :where
                 [?project :project/batches ?batch]
                 [?batch :batch/files ?file]
                 [?file :file/hash ?hash]]
               project hash)))

(defn file-with-relative-path
  [batch rel-path]
  (d/q11 `[:find (pull ?file [*])
           :in $ ?batch ?rel-path
           :where
           [?batch :batch/files ?file]
           [?file :file/pathname ?rel-path]]
         batch rel-path))

;;; Stuff below transfered from blob.clj

(defn delete-file
  [project ent]
  (let [location (:file/location ent)]
    (h/transact [project :delete-file (:db/id ent)]
                [[:db.fn/retractEntity (:db/id ent)]])
    (blob/gs-delete location)))

;;; API support

(defn rows
  [file]
  (map first (d/q '[:find ?row :in $ ?file :where [?row :row/files ?file]] file)))

(defn rows-in-sheet
  [file sheet]
  (map first
       (d/q '[:find ?row
              :in $ ?file
              :where
              [?row :row/files ?file]
              [?sheet :sheet/rows ?row]
              ]
            file)))

;;; → multitool for sure
(defn ldisj
  [list item]
  (remove #(= % item) list))

;;; matches are [row file] pairs
(defn link-txn
  [sheet matches new-sheet-name]
  (-> (sheet/derived-sheet-txn sheet new-sheet-name)
      (sheet/derived-sheet-change-values
       sheet
       (reduce (fn [m [row file]]
                 (update-in m [row :files] conj file))
               {}
               matches))))

(defn unlink-txn
  [sheet files new-sheet-name]
  (-> (sheet/derived-sheet-txn sheet new-sheet-name)
      (sheet/derived-sheet-change-values
       sheet
       (let [m (atom {})]
         (doseq [file files
               row (rows-in-sheet file sheet)]
           (let [row-files (or (get-in @m [row :files])
                               (sheet/row-files row))]
             (swap! m assoc-in [row :files] (ldisj row-files file))))
         @m))))

(defn file-versions
  "Return a temporally-ordered seq of version info for a file. Each entry is a map, fields are :location, :txn, and :time."
  [file-ent-id]
  (let [raw 
        (d/q-history `[:find ?gs ?txn 
                       :where [~file-ent-id :file/location ?gs ?txn true]])]
    (sort-by :txn
             (map (fn [[loc txn]]
                    {:location loc
                     :txn txn
                     :time (:db/txInstant (d/pull '[*] txn))})
                  raw))))

(defn move-txn
  [files batch]
  (into []
        (mapcat (fn [file]
                  ;; There must be a better way to do this (transaction fn?)
                  (let [oldbatch (d/q11 '[:find ?batch :in $ ?file :where [?batch :batch/files ?file]] file)]
                    [[:db/retract oldbatch :batch/files file]
                     [:db/add batch :batch/files file]]))
                files)))

(defn templated-file
  [template file row]
  (let [row-maps (u/map-keys sheet/column-name row)]
    (str "/"
         (u/expand-template-string template row-maps) ;TODO should error if there are unmatched template fields
         "."
         (:file/extension file))))

(defn list-dir-recursive
  "Like fs/list-dir, but will recurse down directories. Only returns non-directory files"
  [path]
  (mapcat
   #(if (fs/directory? %)
      (list-dir-recursive %)
      (list %))
   (fs/list-dir path)))

;;; This used to do more, but was too slow
(defn augment
  "Adds metadata information from gs: to a file entity"
  [file]
  (-> file
      (assoc :sheets (get-in file [:row/_files 0 :sheet/_rows])) ;weird but it works
      ))

