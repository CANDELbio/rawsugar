(ns org.parkerici.rawsugar.dev.migrate
  (:require [org.parkerici.rawsugar.datomic :as d]
            [org.parkerici.rawsugar.history :as h]
            [org.parkerici.rawsugar.batches :as batch]
            [org.parkerici.rawsugar.sheet :as sheet]
            [org.parkerici.rawsugar.projects :as projects]
            [org.parkerici.multitool.core :as u]
            [clojure.set :as set]
            )
  )

;;; https://www.youtube.com/watch?v=DBGgPRWu7cM

;;; Run this before updating schema (15 April 2019)
(defn migrate-1
  []
  (d/wrap-datomic-fn
   #(d/transact
     [{:db/id :column/type :db/ident :column/type.old}
      {:db/id :sheet/rowtype :db/ident :sheet/rowtype.old}])))

;;; These will fail running on a v2 database, which is good.
(defn project-sheets-v1
  [project]
  (map first
       (d/q '[:find ?sheet
              :in $ ?project
              :where
              [?project :project/sheet ?sheet]]
            project)))

(defn project-files-v1
  [project]
  (map first
       (d/q '[:find ?file
              :in $ ?project
              :where
              [?project :project/files ?file]]
            project)))

;;; TODO gs: files now  contain a batch, but I think the old ones don't need to be migrated...should make sure
  ;; Create default batches for existing projects, put existing sheets and files under them
(defn migrate-project-v2-txn
  [project]
  (when-not (empty? (projects/project-batches project))
    (throw (ex-info "looks like project already has batches" {:project project})))
  (let [sheets (project-sheets-v1 project)
        unmatched-files (atom (set (project-files-v1 project)))
        sheets-txn
        (doall
         (apply
          concat
          (for [sheet sheets]
            (let [sheet-ent (d/get-entity sheet)
                  sheet-name (:sheet/name sheet-ent)
                  batch-name (str "Batch " sheet-name)
                  batch-var (str "batch" sheet-name)
                  sheet-files (map :db/id (sheet/sheet-files sheet))]
              (swap! unmatched-files set/difference (set sheet-files))
              `[[:db/add ~batch-var :batch/name ~batch-name]
                [:db/add ~project :project/batches ~batch-var]
                [:db/add ~batch-var :batch/sheets ~sheet]
                ~@(for [file sheet-files]
                    `[:db/add ~batch-var :batch/files ~file])]))))
        _ (prn :unmatched-files (count @unmatched-files))
        unmatched-txn
        (when-not (empty? @unmatched-files)
          `[[:db/add "unmatched files" :batch/name "unmatched files"]
            [:db/add ~project :project/batches "unmatched files"]
            ~@(for [file @unmatched-files]
                `[:db/add "unmatched files" :batch/files ~file])])]
    (concat sheets-txn unmatched-txn)))

(defn migrate-v2
  []
  ;; history: write in timestamps, and create :last-op 
  (let [project-data (projects/project-data)]
    (doseq [p (keys project-data)]
      (prn :migrating (get-in project-data [p :project/name]))
      (h/transact [p :migrate]
                  (migrate-project-v2-txn p))
      (prn :done  (get-in project-data [p :project/name]))
      )))

(defn migrate-v2-dry-run
  []
  ;; history: write in timestamps, and create :last-op 
  (let [project-data (projects/project-data)]
    (doseq [p (keys project-data)]
      (prn :migrating (get-in project-data [p :project/name]))
      (clojure.pprint/pprint 
                  (migrate-project-v2-txn p))
      (prn :done  (get-in project-data [p :project/name]))
      )))

;;; BBB migrate v3

