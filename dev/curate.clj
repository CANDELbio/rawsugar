(ns org.parkerici.rawsugar.dev.curate
  (:require [org.parkerici.rawsugar.datomic :as d]
            [org.parkerici.rawsugar.history :as h]
            [org.parkerici.rawsugar.projects :as projects]
            [org.parkerici.rawsugar.batches :as batches]
            [org.parkerici.rawsugar.sheet :as sheet]
            [org.parkerici.rawsugar.blob :as blob]
            [org.parkerici.rawsugar.ops :as ops] 
            [org.parkerici.multitool.core :as u]
            [org.parkerici.multitool.cljcore :as ju]
            [org.parkerici.multitool.math :as um]
            [clojure.set :as set]))

;;; Aka devops_tools
;;; Curation tools. Functions here are generally not exposed to the CLI or web UI.
;;; And they may be out of date, so please check carefully before using them.

(defn project-file-count
  []
  (d/q '[:find ?p-name (count ?file)
         :in $ 
         :where
         [?project :project/name ?p-name]
         [?project :project/files ?file]]
       ))

;;; TODO breakdown by sheet

(defn file-census
  "Shows count of files by count of row associations"
  []
  (for [project (projects/projects)
        batch (projects/project-batches (:db/id project))]
    [(:project/name project)
     (:batch/name batch)
     (frequencies (map (comp count :row/_files) (batches/batch-files (:db/id batch))))]))


(defn sheet-match-census
  []
  (for [project (projects/projects)
        batch (projects/project-batches (:db/id project))
        sheet (batches/batch-sheets (:db/id batch))
        :let [data (sheet/get-sheet-data (:db/id sheet) :files? true)]]
    [(:project/name project)
     (:batch/name batch)
     (:sheet/name sheet)
     (count data)
     (count (filter #(empty? (:files %)) data))]))


#_
(defn delete-all-files
  [project]
  (d/transact
   (map (fn [ent] [:db.fn/retractEntity (:db/id ent)])
        (sheet/project-files project))))

(defn destroy
  "Delete everything! Note, can't rely on component relationships any more"
  []
  (let [all (fn [att] (ffirst (d/q `{:find [(distinct ?s)] :where [[?s ~att _]]})))
        call (fn [att] (let [res (all att)] (prn :x att (count res)) res))
        nuke-all (fn [att]
                   (d/transact (map (fn [eid] `[:db.fn/retractEntity ~eid])
                                    (call att))))]
    ;; Could derive this from schema I suppose
    (nuke-all :project/name)
    (nuke-all :batch/name)
    (nuke-all :sheet/name)
    ;;; Gets inexplicable error: "Boot datoms cannot be altered: [:db/add :db/ident :db/add 13194142595052 false]",
;    (nuke-all :row/cells)
;;; Blows out Datomic Transactor, apparently.
;    (nuke-all :cell/column)
;    (nuke-all :column/name)
    (nuke-all :file/pathname)))
    


;;; Smart clustering (obs)
;;; Note: this was all useless, easier to split by assayType field. Oh well.

(defn distance
  [r1 r2]
  (/ (count (remove (fn [key] (= (nil? (get r1 key))
                                 (nil? (get r2 key))))
                    (keys r1)))
     (count r1)))

(def distance-memoized (memoize distance))

(defn set-match
  [set row]
  (um/mean
   (map #(distance-memoized % row) set)))

;;; don't forget to call sheet/convert-pds
;;; use (get-sheet-data ... :include-nils? true)
;;; Sadly, too slow for most purposes
(defn cluster-rows
  [sheet-data threshold]
  (reduce (fn [sets row]
            (let [match (and (not (empty? sets))
                             (u/min-by #(set-match % row) sets))
                  level (and match (set-match match row))]
              (if (and match (< level threshold))
                (conj (disj sets match)
                      (cons row (get sets match)))
                (conj sets (list row))
                )))
          #{}
          sheet-data))


;;; This did ok on 0715
#_
(def clusters2 (cluster-rows data 0.1))



(defn nonnull-keys
  [hmap]
  (set (map first (remove (fn [[k v]] (nil? v)) hmap))))

(defn real-columns
  [sheet-data]
  (reduce (fn [acc row]
            (set/union acc (nonnull-keys row)))
          #{}
          sheet-data))

(defn write-cluster
  [project batch sheet data source-file-ent]
  (let [real-cols (real-columns data)
        trimmed (map #(select-keys % real-cols) data)]
    (prn "Creating " sheet (count trimmed) :rows (count real-cols) :columns)
    ;; TODO dry run flag
    (h/transact [project :split sheet]
                (sheet/create-sheet-txn project batch nil sheet trimmed source-file-ent false))))

(defn dumb-cluster-rows
  [project batch sheet column]
  (let [source-file-ent (-> (sheet/lookup-sheet batch sheet)
                             d/get-entity
                             :sheet/file
                             first
                             :db/id)]
    (doseq [[val cluster] (group-by #(get % column) (sheet/get-sheet-data project sheet :files? true :include-nils? true))]
      (write-cluster project batch val cluster source-file-ent))))

(defn rewrite-history
  "Change every item in a project history to have specified agent (TODO generalize)"
  [project new-agent]
  (doseq [hitem (h/history :project project)]
    (d/transact [[:db/add (:db/id hitem) :operation/agent new-agent]])))

#_
(defn empty-sheets
  []
  (d/q '[:find ?sheet ?project-name ?sheet-name
         :in $
         :where
         [?sheet :sheet/name ?sheet-name]
         [?project :project/sheet ?sheet]
         [?project :project/name ?project-name]
         (not [?sheet :sheet/rows _])
         ]))

;;; The test db got into a state where there were duplicate sheets. Not sure how
;;; TODO other checks here.
(defn integrity-check
  []
  (let [results
        (group-by rest
                  (d/q '[:find ?sheet  ?project-name ?sheet-name
                         :in $
                         :where
                         [?sheet :sheet/name ?sheet-name]
                         [?project :project/sheet ?sheet]
                         [?project :project/name ?project-name]]))]
    (filter (fn [[k v]] (> (count v) 1)) results)
    ))

#_
(defn map-all-sheets
  [f]
  (for [project (projects/projects)
        sheet (projects/project-sheets project)]
    (f project (:sheet/name sheet))))

#_
(defn map-all-sheets-to-map
  [f]
  (into {}
        (map-all-sheets
         (fn [proj sheet]
           [[proj sheet] (f proj sheet)]))))

(defn sheet-distinct-census
  "Returns a map of column name -> distinct values present"
  [proj sheet]
  (let [cols (sheet/sheet-columns sheet)]
    (zipmap cols (map (comp count (partial sheet/sheet-column-values sheet)) cols))))


(defn delink-all-matches
  [project]
  (h/transact
   [project :delink-all]
   (map (fn [[row file]]
          [:db/retract row :row/files file])
        (d/q '[:find ?row ?file
               :in $ ?p-name
               :where
               [?project :project/name ?p-name]
               [?project :project/sheet ?sheet]
               [?sheet :sheet/rows ?row]
               [?row :row/files ?file]]
             project))))


(defn delete-test-projects
  []
  (doseq [project (vals (projects/project-data))]
    (when (re-matches #"proj\S+" (:project/name project))
      (prn "Deleting" (:project/name project))
      (projects/delete (:db/id project)))))

(defn delete-entity
  [id]
  (d/transact `[[:db.fn/retractEntity ~id]]))

;;; Export/import

(defn deidentify
  [struct]
  (clojure.walk/postwalk (fn [thing]
                           (if (map? thing)
                             (assoc thing :db/id (str "temp" (:db/id thing)))
                             thing))
                         struct))

(defn export-project
  [project]
  (-> (d/q11 '[:find (pull ?p [*]) :in $ ?pn :where [?p :project/name ?pn]] project)
      deidentify
      (dissoc :db/history)))

(defn import-project
  [project-edn name]
  (d/transact [(assoc project-edn :project/name name)]))

#_
(defn transfer-sheet-txn
  "Transfer a sheet and any included files to a different project"
  [from-project sheet to-project]
  (let [from-project-eid (projects/project-id from-project)
        to-project-eid (projects/project-id to-project)
        sheet-id (:db/id (sheet/sheet-entity from-project sheet))
        files (sheet/project-sheet-files from-project sheet nil)
        transfer (fn [reln thing]
                   [[:db/retract from-project-eid reln thing]
                    [:db/add to-project-eid reln thing]])]
    ;; Make sure files are in exactly 1 row, else mayhem might ensue
    (doseq [file files]
      (assert (= 1 (count (:row/_files file)))))
    (into []
          (concat
           (transfer :project/sheet sheet-id)
           (mapcat #(transfer :project/files (:db/id %)) files)))))


;;; Integrity check: look for weird names (non-printing chars eg)
(def name-re #"\p{Alpha}\p{Print}*")

#_
(defn check-db-names
  []
  (let [check (fn [s c] (when-not (re-matches name-re s) (prn :disallowed s c)))]
    (doseq [p (projects/projects)]
      (check p nil)
      (doseq [s (projects/project-sheet-names p)]
        (check s [p])
        (doseq [c (sheet/sheet-column-names s)]
          (check c [p s]))))))
    

;;; TODO integrity check: make sure sheet → row → cell → col → sheet is correctly circular

;;; Integrity check: check that files actually exist on gs:
;;; extremely slow
(defn check-project-files []
  (doseq [project (projects/projects)
          batch (projects/project-batches (:db/id project))]
    (prn :checking (:project/name project) (:batch/name batch))
    (doseq [f (batches/batch-files (:db/id batch))]
      (when-not (blob/gs-exists? (:file/location f))
        (prn :not-found (:project/name project) (:batch/name batch) f)))))

(defn entity-name [eid]
  (d/q11 '[:find ?name
           :in $ ?ent
           :where [(get-some $ ?ent :project/name :batch/name :sheet/name :column/name) [_ ?name]]]
         eid))

;;; TODO use this for humanizing history entries
(defn humanize [thing]
  (clojure.walk/postwalk #(if (d/eid? %) (or (entity-name %) %) %) thing))


(defn integrity-check-sheet-columns [sheet]
  (let [columns (u/index-by :db/id (sheet/sheet-columns sheet))]
    (doseq [[row col val] (sheet/get-sheet-raw sheet)]
      (when-not (get columns col)
        (prn :bad! row col val)))))

(defn for-all-sheets [f]
  (doseq [sheet (map first (d/q '[:find ?sheet :where [?sheet :sheet/name _]]))]
    (do
      (prn sheet)
      (integrity-check-sheet-columns sheet))))
  
    
(defn batches-with-dashboards []
  (d/q '[:find ?p ?pname ?b ?bname
         :where [?b :batch/dashboard _]
         [?p :project/batches ?b]
         [?p :project/name ?pname]
         [?b :batch/name ?bname]]))

;;; Regnerate dashboard vega-lite specs from args (in retrospect, should have just persisted the args )

(defn respec-dashboard
  [batch]
  (let [args (:args (read-string (:batch/dashboard (d/get-entity batch [:batch/dashboard]))))]
    (ops/dashboard-graph (assoc args :headless? true))))

(defn respec-dashboards
  []
  (doseq [[_ pname b bname] (batches-with-dashboards)]
    (prn :respec pname bname)
    (respec-dashboard b)))


#_
(defn shared-files []
  (let [file->batches
        (group-by second
                  (-> (d/q '[:find ?b ?f :where [?b :batch/files ?f]])))]
    (filter (fn [[k v]] (> (count v) 1))
            file->batches)))

#_
(def batches (distinct (mapcat (fn [[k v]] (map first v)) s)))

#_
(def batch-details
  (d/wrap-datomic-fn #(map (fn [b] (d/pull [:batch/name {:project/_batches [:project/name]}] b)) batches)))

(defn sheets-by-row-count
  []
  (->> (d/q '[:find ?s ?sn (count ?r)
             :where
             [?s :sheet/rows ?r]
             [?s :sheet/name ?sn]])
       (sort-by #(nth % 2))
       reverse))

(defn sheets-by-col-count
  []
  (->> (d/q '[:find ?s ?sn (count ?c)
             :where
             [?s :sheet/columns ?c]
             [?s :sheet/name ?sn]])
       (sort-by #(nth % 2))
       reverse))

(defn sheets-by-size
  []
  (let [rows (u/index-by first (sheets-by-row-count))
        cols (u/index-by first (sheets-by-col-count))]
    (->> (zipmap (keys rows)
                 (map (fn [eid]
                        {:eid eid
                         :name (get-in rows [eid 1])
                         :rows (get-in rows [eid 2])
                         :cols (get-in cols [eid 2])
                         :cells (* (get-in rows [eid 2]) (get-in cols [eid 2]))
                         }
                        )
                      (keys rows)))
         vals
         (sort-by :cells)
         reverse)))

    
(defn create-fake-files
  [sheet]
  (apply
   concat
  (let [batch (sheet/sheet-batch sheet)]
    (for [row (sheet/get-sheet-data sheet :column-format :keyword :row-id? true)
          :let [file (str "t" (:row-id row))]]
    [[:db/add (:row-id row) :row/files file]
     [:db/add batch :batch/files file]
     [:db/add file :file/location (:warehouse row)]
     [:db/add file :file/pathname (:filename row)]
     ;; :file/extension, :file/hash to be really accurante
     ]))))

;;; This doesn't work, the history query times out
(defn sheet-upload-sizes
  []
  (let [h (filter #(= (:operation/cmd %) :upload-sheets)
                  (d/wrap-datomic-fn h/history))]
    (prn (count h) :uploaded-sheets)
    (doseq [he h]
      (d/q-history
       '[:find ?e ?ai ?v ?tx ?added ?time
         :in $ ?tx
         :where
         [?e ?a ?v ?tx ?added]
         [?a :db/ident ?ai]
         [?tx :db/txInstant ?time]
         ]
       (:operation/txn he)))))
    
;;; This will eventually be part of production I guess
#_
(defn gc-dead-sheet-parts
  []
  ;; NOT CORRECT, this returns all rows. Fuck.
  (let [dead-rows (ffirst
                   (d/q '[:find (distinct ?row)
                          :where
                          [?row :row/cells ?cell]
                          [?sheet :sheet/name ?sname]
                          (not [?sheet :sheet/rows ?row])]))
        dead-rows-real
        (fiter ...
        (d/q '[:find (pull ?row [:db/id :sheet/_rows])
               :where
               [?row :row/cells ?cell]]))]
    ))

