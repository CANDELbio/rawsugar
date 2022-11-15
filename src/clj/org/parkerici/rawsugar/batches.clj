(ns org.parkerici.rawsugar.batches
  (:require [org.parkerici.rawsugar.sheet :as sheet]
            [org.parkerici.rawsugar.files :as files]
            [org.parkerici.rawsugar.recipes :as recipes]
            [org.parkerici.rawsugar.datomic :as d]
            [org.parkerici.rawsugar.history :as h]
            [org.parkerici.multitool.core :as u]
            [clojure.edn :as edn]
            ))

;;; Batch

(defn batch-spec
  [sheet-contents?]
  `[:db/id
    :batch/name
    :batch/description
    :batch/request
    :batch/shipment
    :project/_batches
    :batch/dashboard
    {:batch/datatype [:db/id :db/ident]}
    {:batch/status [:db/id :db/ident]}
    {:batch/sheets ~(sheet/sheet-spec sheet-contents?)}
    {:batch/run-steps [:db/id
                       :run-step/agent
                       :run-step/step-id
                       :run-step/predecessors
                       :run-step/sheet
                       {:run-step/state [:db/ident]}
                       :batch/_run-steps]}]
  )

(defn lookup-batch
  [project batch-name]
  (d/q11 '[:find ?b
           :in $ ?project ?batch-name
           :where
           [?project :project/batches ?b]
           [?b :batch/name ?batch-name]]
         project batch-name))

(defn exists?
  "Returns batch entity id if exists"
  [project batch-name]
  (lookup-batch project batch-name))

(defn add-txn
  [project-id batch-name]
  (let [recipe-txn (recipes/instantiate-recipe-txn "batch0" :datatype.default)]
    (concat
     (u/de-ns
     `[[:db/add ~project-id :project/batches "batch0"]
       [:db/add "batch0" :batch/name ~batch-name]
       ])
     recipe-txn
     )))

(defn batch-sheets
  "Return the sheet metadata for all sheets of batch"
  [batch]
  (map first
       (d/q '[:find (pull ?sheet [:db/id :sheet/name :sheet/columns :sheet/derived-from])
        :in $ ?batch
        :where [?batch :batch/sheets ?sheet]]
       batch)))

(defn batch-sheet-names
  [batch]
  (map :sheet/name (batch-sheets batch)))

(defn add
  "Returns id of new batch"
  [project-id batch-name]
  (-> 
   (h/transact
    [project-id :add-batch project-id batch-name]
    (add-txn project-id batch-name))
   (get-in [:tempids "batch0"])))

(defn delete
  [project batch]
  (h/transact
   [project :delete-batch project batch]
   [[:db/retractEntity batch]]
   ))

(defn merge-txn
  [project batch-into batch-from]
  (let [batch-from-map (d/get-entity batch-from)]
    (concat
     (mapcat (fn [sheet]
               [[:db/retract batch-from :batch/sheets (:db/id sheet)]
                [:db/add batch-into :batch/sheets (:db/id sheet)]])
             (:batch/sheets batch-from-map))
     (mapcat (fn [file]
               [[:db/retract batch-from :batch/files (:db/id file)]
                [:db/add batch-into :batch/files (:db/id file)]])
             (:batch/files batch-from-map))
     [[:db/retract project :project/batches batch-from]]
     )))

(defn check-not-exists
  [project batch-name]
  (when (exists? project batch-name)
    (throw (ex-info "Batch exists: " {:project project :batch batch-name}))))


(defn batch-files
  "matched? can be nil, true, or false. Returns entities."
  [batch & [matched?]]
  ;; Complex pull is to get associated sheets
  (let [raw (d/qfirst (u/de-ns `{:find [(pull ?file [* {:row/_files [{:sheet/_rows [:sheet/name :db/id]} ]}])]
                                :in [$ ?batch]
                                :where
                                [[?batch :batch/files ?file] 
                                 ;; Note: important to include sheet because their can be sheetless rows floating around from deletes.
                                 ;; This used to try to use a (not ) query clause but that has problems, so if ask is for unatched
                                 ;; files, this gets filtered below.
                                 ~@(when matched?
                                     '[[?row :row/files ?file]
                                       [?sheet :sheet/rows ?row]])]
                                })
                      batch)
        filtered
        (if (= matched? false)
          (remove #(get-in % [:row/_files 0 :sheet/_rows]) raw)
          raw)]
    (map files/augment filtered)))

;;; Graph persistence

(defn persist-graph [{:keys [project batch] :as args} spec]
  (let [fspec {:args args
               :spec spec}]
    (h/update-generic project
                      {:db/id batch
                       :batch/dashboard
                       (str fspec)})))

(defn dashboard-graph [batch]
  (when-let [fspec (:batch/dashboard (d/get-entity batch [:batch/dashboard]))]
    (let [{:keys [args spec]} (read-string fspec)
          {:keys [sheet]} args
          data (sheet/get-sheet-data sheet :files? :count :column-format :keyword)]
      (when (not-empty data)
        (merge {:data {:values data}} spec)))))
    

;;; → multitool
;;; See group-by-multiple
(defn invert-dag-relation
  [coll id parent child]  
  ;; logically should just return valus but we are doing more stuff with index
  (identity ; vals 
   (reduce
    (fn [ret x]
      (reduce (fn [ret y]
                (update-in ret [y child] conj (id x)))
              ret
              (parent x)))
    (u/index-by id coll)
    coll)))

;;; Duplicated on client side (and probably diverged there)
(defn reconstruct-provenance
  [batch]
  (let [sheets (u/index-by :db/id (:batch/sheets batch))]
    (for [sheet (vals sheets)]
      (let [creation (:object/creation-op sheet)
            parameters (edn/read-string (:operation/parameters creation))
            cmd (:operation/cmd creation)
            cmd (if (= cmd :op) (first parameters) cmd)]
        (assoc sheet
               :cmd cmd
               :parameters parameters
               :predecessors 
               (when (= :op (:operation/cmd creation))
                 (let [params (vals (second parameters))]
                   (filter (partial contains? sheets) params))))))))

(defn compute-transitive
  [indexed-coll relation transitive]
  (let [trans (u/transitive-closure #(map indexed-coll (relation %)))]
    (u/map-values #(assoc % transitive (trans %)) indexed-coll)))

;;; Kind of ridiculous way to compute this but hey.
(defn sheet-derived-sheets
  [batch-id sheet-id]
  (let [relations (-> (d/pull (batch-spec false) batch-id)
                      reconstruct-provenance
                      (invert-dag-relation :db/id :predecessors :derived)
                      (compute-transitive :derived :all-derived))]
    (:all-derived (get relations sheet-id))))

(defn delete-sheet-and-deriveds-txn
  [batch-id sheet-id files?]
  (mapcat #(sheet/delete-sheet-txn (:db/id %) files?) 
          (sheet-derived-sheets batch-id sheet-id)))

(defn delete-sheet-and-deriveds
  [batch sheet files?]
  (h/transact [nil :cli-delete-sheet]   ;TODO project
              (delete-sheet-and-deriveds-txn batch sheet files?)))
  

;;; Only used for test fixtures
(defn delete-derived-sheets
  [batch]
  (let [sheets (filter :sheet/derived-from (batch-sheets batch))]
    (d/transact
     (map (fn [s] [:db/retractEntity (:db/id s)]) sheets))))
