(ns org.parkerici.rawsugar.history
  (:require [org.parkerici.rawsugar.datomic :as d]
            [datomic.client.api :as dc]
            [org.parkerici.rawsugar.login :as login]
            [clojure.data.csv :as csv]
            [taoensso.timbre :as log]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.multitool.cljcore :as ju]
            [org.parkerici.rawsugar.slack :as slack]
            [clojure.string :as str]
            ))

(defn- transaction-add-history
  [transaction project cmd & params]
  (let [op-create
        {:db/id "operation"
         :operation/cmd cmd
         :operation/parameters (pr-str params)
         :operation/agent (login/user)
         :operation/time (java.util.Date.)
         }
        project-ref (cond (string? project)
                          [:project/name project]
                          (d/eid? project)
                          project
                          :else
                          (second project))
        ;; Examine transaction for new objects, using a var naming conventions
        new-objects (distinct (u/walk-collect
                               #(and (string? %) (str/starts-with? % "new_obj_") %)
                               transaction))

        ]
    (when-not project-ref
      (when-not (= cmd :delete-project) ;this one gets a pass
        (log/error "No project for history entry" project cmd params))) ;HHH this is mostly for finding cases where this isn't supplied
    (concat
     (if project
       [[:db/add project-ref :project/history "operation"]
        [:db/add project-ref :project/last-op "operation"] ;TODO nothing uses this, maybe flush
        op-create]
       [op-create])
     (map (fn [r] [:db/add r :object/creation-op "operation"]) new-objects)
     transaction)))

(defn transact
  "Add a history entry to a txn and transact it. ARGS is [project cmd ...]as required by history-transaction. PROJECT is either a string, a :db/id, or [:new <new proj tempid>]."
  [args transaction]
  (login/require-login)
  (d/transact
   (apply transaction-add-history transaction args)))

;;; TODO at some point, filter by time or other criteria
(defn history
  "Return sequence of op entities, reverse chronologically.
  :project - filter to given project
  :op - filter to single op"
  [& {:keys [project op since]}]
  (reverse
   (sort-by
    :operation/time 
    (map (fn [[op txn]]
           (assoc op
                  :operation/project (get-in op [:project/_history :db/id])
                  :operation/transaction (:db/id txn)))
         (let [query
               ;; include the name for easier humanization
               '{:find [(pull ?op [* {:project/_history [:db/id :project/name]}]) (pull ?tx [*])]
                 :in [$]
                 :where
                 [[?op :operation/cmd _ ?tx]]
                 :args []}
               query (if project
                       (-> query
                           (update :in conj '?project)
                           (update :args conj project)
                           (update :where conj '[?project :project/history ?op ?tx]))
                       query)
               query (if op
                       (-> query
                           (update :in conj '?op)
                           (update :args conj op))
                       query)
               query (if since
                       (-> query
                                        ;                           (update :where concat `[[?project :operation/time ?time] [(> ?time ~since)]]))
                           (update :where concat (u/de-ns `[[?op :operation/time ?time] [(> ?time ~since)]])))
                       query)]
           (apply d/q (dissoc query :args) (:args query))
           )))))

(defn- summary-text
  [h]
  (str/join
   \newline
   (cons
    "Weekly summary of changes:"
    (apply
     concat
     (for [[_ project-history] (group-by :operation/project h)]
       (cons (str "Project " (slack/project-mrkdwn-link (:db/id (:project/_history (first project-history)))))
             (for [[user ops] (group-by :operation/agent project-history)]
               (str " –  " user ": " (str/join ", " (u/freq-map (map :operation/cmd ops)))))))))))

(defn weekly-summary
  []
  (slack/post-message
   (summary-text (history :since (ju/date+ (ju/now) -7 0 0)))))

(def date-format (java.text.SimpleDateFormat. "yyyy-MM-dd' 'HH:mm"))

;;; Some bad values crept in (blank strings for nil). This compensates.
(defn read-string-safe
  [s]
  (try
    (read-string s)
    (catch Throwable e
      (list (format "Error reading %s: %s" s e)))))

;;; Columns are [label generator-fn css-class], css-class is optional
(def base-columns
  [["date" #(when-let [t (:operation/time %)]
              (.format date-format t)) "nobr"]
   ["user" :operation/agent]
   ["txn" :operation/transaction "hidden"]
   ["cmd" (comp name :operation/cmd) "nobr"]
   ["params" (comp print-str (partial into []) read-string-safe :operation/parameters) "break-all"]])

(defn columns
  [full?]
  (if full?
    (cons ["project" (comp :project/name :project/_history) "nobr"]
          base-columns)
    base-columns))

(defn humanize
  [history full?]
  (let [; full? (:operation/project (first history))
        columns (columns full?)
        data (cons (map first columns)
                   (map (fn [op]
                          (map (fn [col]
                                 ((second col) op))
                               columns))
                        history))]
    ;; TODO stopgap, not ideal for interactive use
    (csv/write-csv *out* data :separator \tab)
    (flush)))

;; project specific?
(defn history-diff
  [start]
  (map first
       (dc/q
        ;; TODO project thing either not working or we aren't recording it properly.
        ;; TODO unstringify parameters
        '[:find (pull ?op [* {:project/_history [:project/name]}])
          :where [?op :operation/cmd]]
        (dc/since (d/latest-db) start))))


(defn raw-entity-history
  "Takes an entity id and shows all the transactions that touched this entity.
  Pairs well with clojure.pprint/print-table"
  [eid]
  (let [raw (concat
             (d/q-history
              '[:find ?e ?ai ?v ?tx ?added ?time
                :in $ ?e
                :where
                [?e ?a ?v ?tx ?added]
                [?a :db/ident ?ai]
                [?tx :db/txInstant ?time]
                ]
              eid)
             (d/q-history
              '[:find ?e ?ai ?v ?tx ?added ?time
                :in $ ?e
                :where
                [?v ?a ?e ?tx ?added]
                [?a :db/ident ?ai]
                [?tx :db/txInstant ?time]
                ]
              eid))
             ]
    (sort-by :tx (map #(into {} (map vector [:e :a :v :tx :added :time] %)) raw))))

(defn txn-history [txid]
  (d/q11 '[:find (pull ?op [*]) :in $ ?txid :where [?project :project/history ?op ?txid]] txid))

(defn simple-entity-history
  [eid]
  (map txn-history
       (distinct (map :tx (raw-entity-history eid)))))

;;; Note: this isn't going to work for batches, since it won't detect changes in sheets underneath them. Damn. 

;;; TODO make more compatible with history, above
;;; Too slow for non-curatorial uses
(defn entity-history
  "Returns a list of ops for an entity, augmented by :txn and :time, in reverse-chron order"
  [eid]
  (let [txns (group-by :tx (raw-entity-history eid))]
    (sort-by :txn >
             (map (fn [txn]
                    (-> txn
                        txn-history
                        (assoc :txn txn)
                        (assoc :time (get-in txns [txn 0 :time ]))))
                  (keys txns)))))

    
(defn update-generic
  "Update any entity. `project` is for history only."
  [project entity]
  (transact
   [project :update-generic entity]
   (d/update->txn (d/get-entity (:db/id entity)) entity)))

;;; Better way to get entity history (does not use datomic history so much faster)

(defn creator-txn
  [eid]
  (u/min* (map first
               (d/q
                '[:find ?txn
                  :in $ ?e
                  :where
                  [_ _ ?e ?txn]]
                eid))))

(defn creator-op
  [eid]
  (u/min-by :db/id
            (map first
                 (d/q
                  '[:find (pull ?op [*])
                    :in $ ?e
                    :where
                    [_ _ ?e ?txn]
                    [?op :operation/agent ?agent ?txn]]
                  eid))))
