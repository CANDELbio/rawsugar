(ns org.parkerici.rawsugar.recipes
  (:require [org.parkerici.multitool.core :as u]
            [org.parkerici.rawsugar.datomic :as d]
            [org.parkerici.rawsugar.history :as h]
            [me.raynes.fs :as fs]
            [aero.core :as aero]
            [clojure.java.io :as io]))

;;; ⩏⩎⩏ adjustment ⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏

(defn infer-predecessors [{:keys [steps] :as recipe}]
  (assoc recipe :steps
         (map (fn [step pred]
                (if (:predecessors step)
                  step
                  (assoc step :predecessors (and pred (vector (:id pred))))))
              steps (cons nil steps))))

;;; ⩏⩎⩏ recipes  ⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏


;;; Aero can't quite do what I want, so we make an approximation and fix it up here.
;;; Fixes #include to mean "include steps"
(defn fix-includes
  [recipe]
  (update recipe :steps
          (fn [steps]
            (mapcat (fn [step]
                      (if (:steps step)
                        (:steps (fix-includes step))   ;it's a recipe
                        (list step)))
                    steps))))

;;; Important that this be read at compile time. Doesn't work from uberjar.
(def recipe-files (fs/list-dir "resources/recipes/"))

(defn read-recipe
  [r]
  (-> r
      ;; io/resource
      aero/read-config
      fix-includes
      infer-predecessors))

(defn read-recipes
  []
  (->> recipe-files
       (map read-recipe)
       (u/index-by :datatype)))

;; Read this at compile time and not after, or so I hope

(defmacro ct-read-recipes
  []
  (let [the-recipes (read-recipes)]
    `(defonce recipes '~the-recipes)))

(ct-read-recipes)

;;; For development
#_
(def recipes (read-recipes))

;;; ⩏⩎⩏ instantiation ⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏

;;; Instantiate a recipe (for a batch I guess)
;;; Instantiate to in-memory structure for now, Datomic later when I know what I'm doing


;;; TODO change for multirun - pick out latest?
(defn batch-steps
  [b]
  (:batch/run-steps (d/get-entity b '[{:batch/run-steps [* {:run-step/state [:db/ident]}]}])))

;;; TODO change for multirun
(defn delete-recipe-txn
  [batch-id]
  (mapv (fn [step] [:db/retractEntity (:db/id step)])
        (batch-steps batch-id)))

(defn instantiate-recipe-txn
  "Return txn to create the initial steps"
  [batch-id recipe-name]
  ;; Instantiate named or default recipe. See also cljs/recipie/get-recipe
  (let [recipe (or (get recipes recipe-name) (get recipes :datatype.default))]
    (assert recipe (str "Unknown recipe: " recipe-name))
    (mapcat
     (fn [{:keys [id predecessors] :as step}] 
       ;; Not using this att for now, just build on datatype
       [ ; [:db/add batch-id :batch/recipe recipe-name]
        [:db/add batch-id :batch/run-steps (name id)]
        {:db/id (name id)
         :run-step/step-id id
         :run-step/predecessors (map name predecessors)
         :run-step/state (if (empty? predecessors)
                           :run-step-state.todo
                           :run-step-state.blocked)}])
     (:steps recipe))))

;;; Called via eval api when data type changes. 
(defn instantiate-recipe
  [batch-id recipe-name]
  "Create steps for new recipe (recipe-name of nil will just delete the current steps)"
  (d/transact
   (concat
    (delete-recipe-txn batch-id)
    (when recipe-name
      (instantiate-recipe-txn batch-id recipe-name))))
  nil)                                  ;avoid trying to serialize datomic objects

(defn get-run-step
  [eid]
  (d/convert-idents
   (d/get-entity eid '[* :run-step/_predecessors {:run-step/state [:db/ident]} ])))
(defn batch-steps
  "returns step entities"
  [batch-eid]
  (d/convert-idents
   (map first
       (d/q '[:find (pull ?rs [* :run-step/_predecessors {:run-step/state [:db/ident]} ])
        :in $ ?b
         :where [?b :batch/run-steps ?rs]]
       batch-eid))))

(defn initialize-recipe
  "Return txn to reinitialize steps (dev only?)"
  [batch-eid]
  (map (fn [step]
         [:db/add (:db/id step) :run-step/state (if (empty? (:run-step/predecessors step))
                                                  :run-step-state.todo
                                                  :run-step-state.blocked)]
         )
       (batch-steps batch-eid)))


;;; Note: the actual new state is computed on the front end, which I guess is kind of weird but it works

;;; When a step transitions to done, recompute state of downstream notes.
;;;   That means, if all of its predecessors are done, it can transition blocked → todo

;;;   I think this needs to be reversible for UI reasons even if strictly it shouldn't be. 
;;; Just realized this doesn't actually need to recurse at all. Oy talk about conceptual bias.

;;; This is complexicated because trying to build a transaction but also need to keep track of intermediate state. 

(declare run-step-maybe-derive-status)

(defn run-step-update-status
  "Update a run step and its dependencies."
  [run-step-eid new-state & [block-states]] 
  (let [block-states (assoc (or block-states {}) run-step-eid new-state)
        run-step (get-run-step run-step-eid)]
    (conj
     (filter
      identity
     (map (fn [child]
            (run-step-maybe-derive-status child block-states))
          (map :db/id (:run-step/_predecessors run-step))))
     [:db/add (:db/id run-step) :run-step/state new-state])))

(defn run-step-maybe-derive-status
  [run-step-eid block-states]
  (let [run-step (get-run-step run-step-eid)]
    (when (and (= :run-step-state.blocked (:run-step/state run-step))
               (not-any? (fn [predecessor]
                           (not (= (or (get block-states (:db/id predecessor))
                                       (:run-step/state predecessor))
                                   :run-step-state.done)))
                         (map (comp get-run-step :db/id) ;this should be cached
                              (:run-step/predecessors run-step))
                         ))
      [:db/add run-step-eid :run-step/state :run-step-state.todo]
      )))

(defn update-step
    "called from /step api handler, needs to return response"
    [project eid new-state agent]
  (let [txn (conj (run-step-update-status eid new-state)
                  [:db/add eid :run-step/agent agent])]
      (h/transact [project :update-step eid new-state]
                  txn)
      :success))

;;; No longer called? 
;;; Called from ops – probably should return rather than do txn
