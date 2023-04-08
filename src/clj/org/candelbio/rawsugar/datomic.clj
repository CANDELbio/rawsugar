(ns org.candelbio.rawsugar.datomic
  (:require
   [environ.core :refer [env]]
   [datomic.client.api :as d]
   ;; datomic.client.api.sync              ;avoids a weird datomic error
   [org.candelbio.multitool.core :as u]
   [org.candelbio.rawsugar.request :as request]
   [clojure.walk :as walk]
   [clojure.string :as str]
   ))

;;; TODO spin this out into a library, it's mostly indpendent of Rawsugar

;;; Source https://gist.github.com/natural/871d7a3ddfb6ae5f72fb141e549ca3bb
(def ^{:dynamic true :doc "A Datomic database value used over the life of a Ring or CLI request."} *db*)
(def ^{:dynamic true :doc "A Datomic connection bound for the life of a Ring or CLI request."} *connection*)

;;; TODO these should be changed from defaults! And managed in kubernetes secrets
;;; In general, dont do defaulting here, confusing
(def default-db-name "rawsugar")
(def default-access-key "myaccesskey")
(def default-secret "mysecret")

(def query-timeout 60000)         ; Far too long for web app, need TODO performance tuning / paging
(def txn-timeout 120000)

(def config {:server-type :peer-server
             :access-key (or (:datomic-access-key env) default-access-key)
             :secret (or (:datomic-secret env) default-secret)
             :endpoint (:datomic-endpoint env)})

;;; This caches behind the scenes, no need to do that ourselves
(defn conn
  []
  (let [client (d/client config)]
    (d/connect client {:db-name (or (:datomic-db-name env) default-db-name)})))

;;; In general this should not be used; instead, use wrap-datomic-fn or equivalent
(defn latest-db
  []
  (d/db (conn)))

(defn pull
  [spec eid]
  (d/pull *db* spec eid))

(defn q-history
  "Like q, but applied to the history view of the database"
  [query & args]
  (apply d/q query (d/history (d/db *connection*)) args))

(defn q-as-of
  "Like q but on a historical version of the database"
  [as-of query & args]
  (apply d/q query (d/as-of (d/db *connection*) as-of) args))

(defn pull-as-of
  "Like pull but on a historical version of the database"
  [as-of spec eid]
  (d/pull (d/as-of (d/db *connection*) as-of) spec eid))

(defn q
  [query & args]
  (let [db (if request/*as-of*
             (d/as-of (d/db *connection*) request/*as-of*)
             *db*)]
    (d/q {:query (u/de-ns query)
          :args (cons db args)
          :timeout query-timeout})))

(defn qfirst
  "Query with a single result var, strips the useless seq off"
  [query & args]
  (map first (apply q query args)))

(defn q1
  "Query for a single result. Errors if there is more than one row returned."
  [query & args]
  (let [res (apply q query args)]
    (if (> (count res) 1)
      (throw (Error. (str "Multiple results where at most one expected: " query " " res)))
      (first res))))

(defn q11
  "Query for a single value in a single result. Errors if there is more than one row returned."
  [query & args]
  (let [res (apply q1 query args)]
    (if (> (count res) 1)
      (throw (Error. (str "Multiple results where at most one expected: " query " " res)))
      (first res))))

(defn transact
  [txn]
  (if request/*as-of*
    (throw (ex-info "Transactions not allowed in wayback view" {:as-of request/*as-of*}))
    (d/transact *connection* {:tx-data txn
                              :timeout txn-timeout})))

(defn invert-tx-data
  "Given the :tx-data from a txn operation, this produces a txn that will reverse the original.
  Note: this may not work in all cases, currently only used in tests"
  [data]
  (filter
   identity
   (map (fn [[a b c d e]]
          (when-not (= a d)
            (if e
              [:db/retract a b c]
              [:db/add a b c])))
        data)))

;;; NOTE: bad idea to use this without a spec on most things, since it brings back all sheets and sheet content
(defn get-entity
  [id & [spec]]
  (let [db (if request/*as-of*
             (d/as-of (d/db *connection*) request/*as-of*)
             *db*)]
    (d/pull db (or spec '[*]) id)))

;;; Late invention, should be used more. Not sure how efficient it is
(defn get-property
  [id property]
  (first
   (q1 '[:find ?x
         :in $ ?id ?prop
         :where [?id ?prop ?x]]
       id property)))

;;; Just used for dev/curation tasks
(defn get-txn
  [txn-id]
  (first (seq (d/tx-range (conn) {:start txn-id}))))

(defn wrap-datomic
  "A Ring middleware that provides a request-consistent database connection and
  value for the life of a request."
  [handler]
  (fn [request]
    (let [connection (conn)]
      (binding [*connection* connection
                *db*         (d/db connection)]
        (handler request)))))

;;; TODO the doall-safe is to try to make sure lazy lists are realized within the scope of the db binding
;;; but it doesn't really work because inner elements might be lazy. Really needs to do a walk of the structure.
(defn wrap-datomic-fn
  [f]
  ((wrap-datomic (fn [& _] (u/doall-safe (f)))) nil))

;;; For dev fingers
(def w wrap-datomic-fn)

(defn update->txn
  "`entity` is an entity map, update is an updated version of it (can be incomplete). Generates a txn. Not recursive (but maybe should be)."
  [entity update]
  (for [[key val] update
        :when (not (= (get entity key) val))]
    (if val
      [:db/add (:db/id entity) key val]
      (let [oldv (get entity key)
            oldv (if (map? oldv) (:db/id oldv) oldv)] ;argh datomic
        [:db/retract (:db/id entity) key oldv]))))

(defn eid?
  [x]
  (pos-int? x))

(defn coerce-entity
  "Coerce argument to entity map, pulling it if necessary"
  [id-or-entity]
  (if (eid? id-or-entity)
    (get-entity id-or-entity)
    id-or-entity))

;;; WIP https://stackoverflow.com/questions/25389807/how-do-i-undo-or-reverse-a-transaction-in-datomic
#_
(defn revert-txn
  [tx]
  "Reassert retracted datoms and retract asserted datoms in a transaction,
  effectively \"undoing\" the transaction."
  (let [tx-log (-> conn d/log (d/tx-range tx nil) first) ; find the transaction
        txid   (-> tx-log :t d/t->tx) ; get the transaction entity id
        newdata (->> (:data tx-log)   ; get the datoms from the transaction
                     (remove #(= (:e %) txid)) ; remove transaction-metadata datoms
                     ; invert the datoms add/retract state.
                     (map #(do [(if (:added %) :db/retract :db/add) (:e %) (:a %) (:v %)]))
                     reverse)] ; reverse order of inverted datoms.
    @(d/transact conn newdata)))  ; commit new datoms.

;;; Error handling
;;; Doesn't really belong here, but I don't want to create a whole new file. 
(defn humanize-ex-args
  [args]
  (let [{:keys [project batch sheet column]} args
        namify (fn [thing att]
                 (cond (nil? thing) nil
                       (string? thing) ( str (namespace att) ": " thing)
                       (eid? thing) (str (namespace att) ": "
                                         (att (get-entity thing)))))]
    
    (str/join ", "
              (filter identity
                      (list
                       ;; Wouldn't it be nice if these all shared a common abstraction?
                       (namify project :project/name)
                       (namify batch :batch/name)
                       (namify sheet :sheet/name)
                       (namify column :column/name))))))

(defn hex-info
  "Like ex-info, but will translate eids to strings and add them to message"
  [msg map]
  (ex-info (str msg " – " (humanize-ex-args map)) map))

(defn convert-idents
  "Walk Datomic results, converting maps with :db/ident into simple keywords"
  [datomic-results]
  (walk/prewalk #(or (and (map? %) (:db/ident %)) %)
                datomic-results))

(defn ent-type
  "Return the type (kind) of an entity as a keyword. Hacky."
  [entity]
  (and (map? entity)
       (some #(let [ns (namespace %)]
                (and ns (not (= ns "db"))
                     (keyword ns)))
             (keys entity))))

