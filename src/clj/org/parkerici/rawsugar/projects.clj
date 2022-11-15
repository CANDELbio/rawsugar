(ns org.parkerici.rawsugar.projects
  (:require [org.parkerici.rawsugar.datomic :as d]
            [org.parkerici.rawsugar.history :as h]
            [org.parkerici.rawsugar.batches :as batches]
            [org.parkerici.multitool.core :as u]
            [clojure.walk :as walk]
            ))

(defn projects
  "Returns seq of project entities"
  []
  (map first
       (d/q '[:find (pull ?project [:project/name :db/id]) :where
              [?project :project/name ?name]
              ])))

(defn lookup [project-name & [no-error]]
  (or (d/q11 '[:find ?p :in $ ?project-name :where [?p :project/name ?project-name]] project-name)
      (if no-error
        nil
      (throw (ex-info (str "Project does not exist: " project-name)
                      {:type :not-found
                       :project project-name})))))

(defn exists? [project-name]
  (lookup project-name true))

(defn check-not-exists
  [project-name]
  (when (nil? project-name)
    (throw (ex-info "No project specified" {:type :not-found}))) 
  (when (exists? project-name)
    (throw (ex-info (str "Project already exists: " project-name) {:type :user-input}))))

(defn project-batches
  [project]
  (map first
       (d/q '[:find (pull ?batch [*])
              :in $ ?project
              :where
              [?project :project/batches ?batch]]
            project)))

(defn project-batch-names
  [project]
  (map :batch/name (project-batches project)))

(defn add
  "Create a new project, returning the eid"
  [project-name]
  (check-not-exists project-name)
  (->
   (h/transact
    [[:new "project0"] :add-project project-name]
    [[:db/add "project0" :project/name project-name]])
   (get-in [:tempids "project0"])))


(defn delete
  [project-id]
  (let [project-name (:project/name (d/get-entity project-id))]
    (d/transact
     [[:db.fn/retractEntity project-id]])
    ;; create a floating history item, since there is no project for it to live on.
    (h/transact [nil :delete-project project-name]
                [])))

;;: A pull spec for getting project and component data (basically everything except sheet contents)
(u/defn-memoized project-spec
  [& [sheet-contents?]]
  (conj
   `[:db/id
     :project/name
     :project/description
     :project/dashboard
     {:project/batches
      ~(batches/batch-spec sheet-contents?)}]
   '{:project/last-op [*]})               ;Clojure macros are so broken
  )

(defn get-project
  [id]
  (d/pull (project-spec) id))

;;; TODO Utils!
(defn assoc-if
  ([map key val]
   (if (nil? val)
     map
     (assoc map key val)))
  ([map key val & kvs]
   (apply assoc-if (assoc-if map key val) kvs)))
    

;;; Note: as is, this is not particularly scalable.
(defn project-data
  "Project data for home page and other high-level views.
  results is a map of project ids to projects, including the batch/sheet/file metadata"
  []

  ;; Add count fields to batches and sheets
  (let [file-counts (into {}
                          (d/q '[:find ?b (count ?f)
                                 :where
                                 [?b :batch/files ?f]]))
        ;; Wish I could do the next 2 in one query, not sure its possible
        sheet-counts (into {}
                           (d/q '[:find ?s (count ?r)
                                  :where [?s :sheet/rows ?r]]))
        sheet-counts-matched (into {}
                                   (d/q '[:find ?s (count ?r)
                                          :where [?s :sheet/rows ?r] [?r :row/files ?f]
                                          ]))
        augment-sheet (fn [sheet]
                        (-> sheet
                            (assoc-if :sheet/row-count (get sheet-counts (:db/id sheet))
                                      :sheet/row-count-matched (get sheet-counts-matched (:db/id sheet)))))
        augment-batch (fn [batch]
                        (-> batch
                            (assoc-if :batch/file-count (get file-counts (:db/id batch)))))
        ]
    (->>
     (d/qfirst `[:find
                 (pull ?project ~(project-spec))
                 :in $ 
                 :where
                 [?project :project/name _]])
     (walk/prewalk
      (fn [thing]
        (cond (:db/ident thing) (:db/ident thing) ;see d/convert-idents
              (:db/id thing)
              ((case (d/ent-type thing)
                 :sheet augment-sheet
                 :batch augment-batch
                 identity)
               thing)
              :else thing)))
        )))

;;; see batches/batch-files
(defn project-files
  [project & [matched?]]
  (map first
       (d/q `{:find [(pull ?file [* :row/_files])]
              :in [$ ?project]
              :where
              [[?project :project/batches ?batch]
               [?batch :batch/files ?file]
               ~@(when (not (nil? matched?))
                   (if matched?
                     '[[_ :row/files ?file]]
                     '[(not [_ :row/files ?file])]))]
              }
              project)))
