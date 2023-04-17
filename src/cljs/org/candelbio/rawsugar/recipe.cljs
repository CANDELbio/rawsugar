(ns org.candelbio.rawsugar.recipe
  (:require [re-frame.core :as rf]
            [reagent.core :as reagent]
            [clojure.string :as str]
            
            [re-com.core :refer [popover-content-wrapper popover-anchor-wrapper] :refer-macros []]
            [re-com.popover :refer []]

            [inflections.core :as inflect]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.rawsugar.sheet :as sheet]
            [org.candelbio.rawsugar.api :as api]
            [org.candelbio.rawsugar.utils :as utils]
            [org.candelbio.rawsugar.web-utils :as wu]))

;;; Reciple UI

;;; No longer actually used, but see css
(def box-width 200)
(def box-height 100)
(def box-spacing 50)

(defn format-time-short
  [inst]
  (wu/format-time inst "MM-dd HH:mm"))

(defn- op-link
  [{:keys [op] :as step} & [disabled?]]
  [:a {:href "#"
       :on-click (when-not disabled?
                   #(rf/dispatch [:op-dialog op nil])) ;TODO namespace
       :class (when disabled? "a-disabled")
       } 
   (get-in @(rf/subscribe [:ops]) [:definitions :operations op :name])
   ])

(defmulti step-custom-ui (fn [{:keys [run-step/step-id status] :as run-step}] [step-id status]))

(defmethod step-custom-ui :default
  [step]
  (or
   (when (:op step)
     (case (:status step)
       :todo (op-link step)
       :in-progress (op-link step)
       :blocked (op-link step true)
       nil))
   ;; Assuming at most one of op and link are set
   (when (:link step)
     [:a {:href (second (:link step)) :target "_ext"}
      (first (:link step))
      (wu/icon "link" "" nil)]
     )))

(defmethod step-custom-ui [:upload-manifest :done]
  [step]
  [:div 
   (for [sheet (:run-step/sheets step)] ;TODO needs rethinking
     (let [sheet @(rf/subscribe [:entity (:db/id sheet)])]
       [:p (sheet/sheet-link sheet) " "     ;
        (utils/comma-list
         (list (utils/pluralize-if (:sheet/row-count sheet) "row")
               (utils/pluralize-if (:sheet/row-count-matched sheet) " matches")))]))])


;;; Don't have :run-step/files anymore
#_
(defmethod step-custom-ui [:upload-files :done]
  [step]
  (let [files (:run-step/files step)]   
    [:span (inflect/pluralize (count files) "file")]))

;;; Step FSM

;;; It might make more sense to do this on the backend
; :blocked (can't change)
; :todo → :in-progress
; :in-progress → :done (and unblock downstream)
; :done → :todo I guess, and create a new branch. 

(defn next-state
  [run-step]
  (case (:run-step/state run-step)
    :run-step-state.blocked :run-step-state.blocked 
    :run-step-state.todo :run-step-state.in-progress
    :run-step-state.in-progress :run-step-state.done
    :run-step-state.done :run-step-state.todo))

(rf/reg-event-fx
 ::toggle-status
 (fn [{:keys [db]} [_ run-step]]
   ;; compute new state
   (let [{:keys [project batch]} db
         new-step (-> run-step
                      (assoc :run-step/state (next-state run-step)))
         update (select-keys new-step [:db/id :run-step/state])]
     {
      :dispatch [:update-step
                 (:db/id new-step)
                 (:run-step/state new-step)
                 #(rf/dispatch [:batches-changed] )
                 ]
      }
     )))

(rf/reg-event-fx
 :update-step
 (fn [{:keys [db]} [_ step new-state handler]]
   (api/api-post "/batch/step/update"
                 {:params {:project (:project db)
                           :step step
                           :state new-state
                           :agent (:user db)}
                  :handler handler})))

(defn- sheet-chooser
  [step]
  [:div.rs-sheet-picker
   (wu/select-widget "sheetpicker"
                     (:db/id (:run-step/sheet step)) 
                     ;; This horrible thing is because I am mutating attributes with an inverse that also needs to be kept up to date...
                     ;; There must be a better way but this at least fixes updates.
                     #(let [old-sheet-id (:db/id (:run-step/sheet step))
                            old-sheet-update {:db/id old-sheet-id
                                              :run-step/_sheet nil}
                            new-sheet-id (u/coerce-numeric %)
                            step-id (:db/id step)
                            step-update {:db/id step-id
                                         :run-step/agent @(rf/subscribe [:login])
                                         :run-step/sheet {:db/id new-sheet-id}}
                            new-sheet-update {:db/id new-sheet-id
                                              :run-step/_sheet [{:db/id step-id}]
                                              }]
                        (rf/dispatch [:update-entity step-update])
                        (rf/dispatch [:update-entity old-sheet-update true])
                        (rf/dispatch [:update-entity new-sheet-update true])
                        )
                     @(rf/subscribe [:sheet-options])
                     "Choose a sheet")])

(defn run-step-ui
  [{:keys [run-step/step-id status x y doc] :as run-step}]
  (let [started (:run-step/start run-step)
        agent (:run-step/agent run-step)
        notes (:run-step/notes run-step)]
    (let [showing? (reagent/atom false)]  ;; controls whether notes are showing TODO rename
      ^{:key (str (:db/id run-step))}
      [:div.run-step {:style {:left x
                              :top y}}
       ;; Doc
       (when doc
         [:div.doc
          (wu/doc-icon doc)
          ])

       ;; Notes widget
       [:div.notes
        [popover-anchor-wrapper
         :showing? showing?
         :position :right-center
         :anchor (wu/icon "create" "notes"  #(swap! showing? not) :class (when notes "yes-notes"))
         :popover [popover-content-wrapper
                   :title (str (name step-id) ": notes")
                   ;; Save on blur. Maybe it should do some little animation. 
                   :body [:textarea {:on-blur  #(let [new-text (-> % .-target .-value)]
                                                  (swap! showing? not) ;hide on blur (?)
                                                  (rf/dispatch [:update-entity {:db/id (:db/id run-step)
                                                                                :run-step/agent @(rf/subscribe [:login])
                                                                                :run-step/notes (if (str/blank? new-text) nil new-text)}]))
                                     :defaultValue notes
                                     }
                          ]]]]

       ;; Name
       [:span.name
        (utils/modest-titleize (name step-id))]

       ;; State/status
       [:span.status {:style {:background-color (case status
                                                  :blocked "tomato" ;TODO better colors
                                                  :done "palegreen"
                                                  :todo "khaki"
                                                  "orange")
                              }
               :on-click #(rf/dispatch [::toggle-status run-step])
               }
        (name status)]

       ;; Metadata and cmd
       [:div.metadata
        [:span (utils/trim-login agent)] (if (and agent started) ", " "") [:span (format-time-short started)]
        [:div (step-custom-ui run-step)]]

       ;; Sheet
       (sheet-chooser run-step)
       ]
      )))

;;; Vertical
(defn path
  [from to]
  (let [fx (+ (:x from) (/ box-width 2))
        fy (+ (:y from) box-height)
        tx (+ (:x to) (/ box-width 2))
        ty (:y to) 
        my (/ (+ fy ty) 2)]
    [:path {:stroke-width 2 :stroke "gray" :fill "none"
            :marker-end "url(#head)"
            :d (str/join \space (list "M" fx fy "C" fx my tx my tx ty))}]))

(defn run-ui
  [steps]
  (let [indexed (u/index-by :db/id steps)]
     [:div.recipe

      ;; The SVG pane contains the arrows between steps
      [:svg.svg
       [:defs
        [:marker {:id "head" :orient "auto"
                  :markerWidth 4 :markerHeight 8
                  :refX 4 :refY 4}
         [:path {:d "M0,0 V8 L4,4 Z" :fill "gray"}]]]
       `[:g ~@(mapcat (fn [step]
                        (map (fn [pred]
                               (path (get indexed (:db/id pred)) step))
                             (:run-step/predecessors step)))
                      steps)]]
      ;; The steps themselves
    (doall
       (for [step steps]
         ^{:key (:db/id step)}
         [run-step-ui step]))
      ]))  

(defn get-recipe
  [datatype]
  (let [recipes (get-in @(rf/subscribe [:ops]) [:definitions :recipes]) ]
    (or (get recipes datatype)
        (get recipes :datatype.default))))  

(defn- merged-recipe-steps
  "Merges the datomic run-steps with their static recipe definitions"
  []
  (let [batch @(rf/subscribe [:batch true])
        raw-steps (:batch/run-steps batch)
        ;; Use the version in :index which gets updated
        cooked-steps (doall (map (fn [step]
                                   @(rf/subscribe [:entity (:db/id step)]))
                                 raw-steps))
        recipe-name (:batch/datatype batch)
        recipe (get-recipe recipe-name)
        steps (u/index-by :id (:steps recipe))
        ]
    (map (fn [datomic-step]
           (assoc
            (merge (get steps (:run-step/step-id datomic-step))
                   datomic-step)
            :status (keyword (last (str/split (name (:run-step/state datomic-step)) #"\.")))))
         cooked-steps)))

(defn- layout
  [steps]
  (map-indexed
   (fn [y step]
     (-> step
          (assoc :x 0)
          (assoc :y (* y (+ box-height box-spacing)))))
   steps))

(defn do-run-ui
  [_]
  [run-ui
   (layout
    (merged-recipe-steps))])

(defn recipe-title
  [_]
  [:h3
   "Recipe"
   (let [datatype (:batch/datatype @(rf/subscribe [:batch true]))
         {:keys [name doc] :as recipe}
         (and datatype (get-recipe datatype))
         ]
     (when doc
       (wu/doc-icon doc (str name " documentation"))))
   ])
