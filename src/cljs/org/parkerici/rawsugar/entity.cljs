(ns org.parkerici.rawsugar.entity
  (:require [re-frame.core :as rf]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.rawsugar.api :as api]
            [clojure.walk :as walk]
            ))

;;; This implements a general facility to track Datomic entities in the client db. 

;;; Theory: there is an entry :index in the re-frame db, it's a map from Datomic ids to full
;;; entities. This is initially constructed from an API call that gets all the project/batch/sheet
;;; metadata (see projects/:initialize-projects, and on the server side, projects/project-data.

;;; Note that the entities are slightly augmented from their Datomic form.

;;; This might be a better approach: https://github.com/den1k/subgraph

(defn build-index
  [pd]
  (persistent!
   (u/walk-reduce
    (fn [acc thing]
      (if (and (map? thing)
               (:db/id thing)
               (> (count thing) 1)
               (not (:copy? thing)))    ;You can tag entities with this so they don't get indexed
        (assoc! acc (:db/id thing) thing)
        acc))
    pd
    (transient {}))))

(rf/reg-sub
 :index
 (fn
  [db [_ eid]]
   (get db :index)))

(rf/reg-sub
 :entity
 (fn
  [db [_ eid]]
   (get-in db [:index eid])))

(defn entity
  [eid]
  @(rf/subscribe [:entity eid]))

(rf/reg-sub                             ;not sure about this
 :entity-in
 (fn
  [db [_ path]]
   (get-in (:index db) path)))
  

;;; Some local entity refsf are kept in complex form {:db/id ...} or [{:db/id ...}] because
;;; that's how they come out of Datomic. To put them back on the server they need to be simplified.
(defn clean-entity
  [ent]
  (u/map-values
   (fn [v]
     (cond (vector? v)
           (do
             (assert (= 1 (count v)))
             (:db/id (first v)))
           (map? v)
           (:db/id v)
           :else
           v))
   ent))
  

(defn update-generic
  "Update a specific entity. "
  [project entity handler]
  (api/api-post "/entity/update"
            {:params {:project project
                      :entity (clean-entity entity)}
             :handler handler}))

;;; Update an entity on server and in local :index cache (not used very much yet)
;;; TODO replace :sheet/set-sheet-property etc.
;;; TODO option for response handler? Not needed now
(rf/reg-event-fx
 :update-entity
 (fn [{:keys [db]} [_ entity & [local-only?]]]
   (when-not local-only?
     (update-generic nil entity #()))      ;TODO infer project on server
   ;; WARNING, doesn't update inverse links (of course) so db is now onconsistent...right now caller has to handle, see recipes/sheet-chooser TODO
   {:db (update-in db [:index (:db/id entity)] merge entity)}))

(defn coerce
  [thing]
  (cond (map? thing) thing
        (number? thing)
        @(rf/subscribe [:entity thing])))

(defn coerce-eid
  [thing]
  (cond (map? thing) (:db/id thing)
        (number? thing) thing))

;;; The right way to make this work:
;; - index maps ids to bare entities (that is, entity-valued attributes have ids)
;; (this is not what pull gives us but so what, it shouldn't drive)
;; - then all references have to go through index, so updating is not a problem any more
;; - Then something like refreshed above would make sense

(defn refresh
  "Given an entity or id, return the entity with itself and all inner entities refreshed to current version"
  [structure]
  (let [index @(rf/subscribe [:index])
        structure (if (number? structure) (get index structure) structure)]
    (walk/prewalk
     (fn [x]
       (if (and (map? x) (:db/id x) (> (count x) 1))
         (get index (:db/id x))
         x))
     structure)))
