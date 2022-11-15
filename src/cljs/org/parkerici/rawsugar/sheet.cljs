(ns org.parkerici.rawsugar.sheet
  (:require [reagent.core :as reagent]
            [re-frame.core :as rf]
            [org.parkerici.rawsugar.aggrid :as ag]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.rawsugar.web-utils :as wu]
            [org.parkerici.rawsugar.files :as files]
            [org.parkerici.rawsugar.matcher :as matcher]
            [org.parkerici.rawsugar.entity :as entity]
            [org.parkerici.rawsugar.api :as api]
            [org.parkerici.rawsugar.navigate :as nav]
            [inflections.core :as inflect]
            [clojure.string :as str]
            [reagent.dom.server]
            ))

(def special-columns
  {:files
   ;; I wanted this to be index -1, that is, to be leftmost, but
   ;; turns out that aggrid has problems with that, so this is a weird-looking
   ;; compromise
   {:column/name "Files" :column/index 1} 
   :files-warehouse
   {:column/name "Warehouse"
    :column/hidden true}
   :row-id
   {:column/name "Row ID"
    :column/hidden true}
   })

;;; Sheet initialization

;;; Can't run until project data is available, not sure how to manage that...ARGH.
(rf/reg-event-fx
 ::select-sheet
 (fn [_ [_ sheet]]
   (api/api-get "/sheet/data"
                {:params        {:project @(rf/subscribe [:project]) :sheet sheet :start 0 :n 10}
                 :handler       #(rf/dispatch [::got-sheet-data %1])})
   {}))

(rf/reg-event-db
 ::got-sheet-data
 (fn [db [_ [cols & sheet-data]]]
   (let [columns-indexed (map-indexed (fn [i col]
                                        (if (:column/index col)
                                          col
                                          (assoc col :column/index i)))
                                      cols)
         column-keys (map (comp u/keyword-safe :column/name) columns-indexed)
         columns (merge (zipmap column-keys columns-indexed)
                        special-columns)]
   (assoc db
          ::columns columns
          ::displayed-columns (->> columns
                                   (u/self-label :key)
                                   vals
                                   (remove :column/hidden)
                                   (sort-by :column/index) 
                                   (map :key))
          ::sheet-data
          (mapv (fn [row idx] (assoc row :index idx))
                sheet-data
                (range))))))

(rf/reg-event-fx
 ::initialize
 (fn [{:keys [db]} _]
   {:db
    (merge
     db
     {::sheet-data [] 
      ::columns {}                        ; becomes a sorted map
      ::displayed-columns []
      })
    :dispatch [::select-sheet (or (:sheet db)
                                  ;; default to first (this may never be used?)
                                  (first (get-in db [:index (:batch db) :batch/sheets])))
                                  ]}))

(rf/reg-sub
 :sheet-columns
 (fn [db [_ sheet]]
   (get-in db [:project-structure sheet])))

(rf/reg-event-fx
 ::update-sheet
 (fn
  [{:keys [db]} _]
  {:dispatch [::select-sheet (:sheet db)]}
  ))

(rf/reg-event-db
 :row-update
 (fn
  [db [_ row index]]
  (assoc-in db
            [::sheet-data index]
            (assoc row :index index))))

(rf/reg-event-db
 :rows-update
 (fn
  [db [_ rows]]
  (doseq [row rows]
    (let [pos (u/position #(= (:row-id %) (:row-id row)) (::sheet-data db))]
      (rf/dispatch [:row-update row pos])))
  db))

(rf/reg-event-db
 ::hide-col
 (fn
  [db [_ col]]
  (update-in db [::displayed-columns] (partial u/remove= col))))

(rf/reg-event-db
 ::show-all-cols
 (fn
  [db [_]]
  (assoc db ::displayed-columns
         (map first (sort-by (fn [[_ v]] (:db/id v)) (::columns db))))))

;;; Subs

(rf/reg-sub
  ::sheet-data
  (fn [db _]  
    (::sheet-data db)))

;;; May not be needed?
(rf/reg-sub
  ::columns
  (fn [db _]
    (::columns db)))

(rf/reg-sub
  ::column-slot
  (fn [db [_ col slot]]
    (get-in db [::columns col slot])))

(rf/reg-sub
  ::displayed-columns
  (fn [db _]  
    (::displayed-columns db)))

(rf/reg-sub
  ::selection-context
  (fn [db _]
    (::selection-context db)))

(rf/reg-sub
 :sheet-options
 (fn [db _]
   (let [batch (entity/refresh @(rf/subscribe [:entity (:batch db)]))]
     (sort-by (comp str/lower-case :label)
              (map (fn [sheet]
                     {:value (:db/id sheet)
                      :label (:sheet/name sheet)})
                   (entity/refresh (:batch/sheets batch)))))))

;;; Was going to reuse this for recipe steps, but not worth it
(defn- sheet-chooser-innards
  [handler]
  (wu/select-widget "sheetpicker"
                    @(rf/subscribe [:sheet])
                    handler
                    @(rf/subscribe [:sheet-options])
                    nil)
  )

;;; → web-utils
(defn modal-button
  "A button that enters a modal state; clicking it again will leave that state and maybe do something. Used for File matching and Edit cells buttons"
  [in-mode? normal-title modal-title change-event]
  [:span
   [:button.btn.btn-outline-secondary.modal-button
    {:type "button"
     :class (if in-mode? "modal-button-on" nil)
     :title (if in-mode? modal-title normal-title)
     :on-click #(rf/dispatch [change-event])}
    (if in-mode? modal-title normal-title)
    (when in-mode?
      (wu/icon "done" "save" nil :class "md-white"))
    ]
   (when in-mode?
     (wu/icon "cancel" "cancel" #(rf/dispatch [change-event true]) :class "md-light")
     )])

(defn- sheet-chooser
  []
  [:div.input-group.sheet-chooser                     ;bootstrap 5
   [:span.chooser
    (wu/editable-text-widget-flex
     :sheet-name
     {:initial-value (:sheet/name @(rf/subscribe [:sheet true]))
      :normal
      (partial sheet-chooser-innards
               #(rf/dispatch [:navigate-route :sheet
                              :sheet (u/coerce-numeric %)
                              :project @(rf/subscribe [:project])
                              :batch @(rf/subscribe [:batch])]))
      :change-event [::set-sheet-property :sheet/name]})]

   [:span                               ;Expand/contract button 
    [:span.spacer]     ;white space between filter and modal
    [:button.btn.btn-outline-secondary.expand
     {:type "button"
      :title "Expand"
      :on-click  #(rf/dispatch [::ag/expand-rows :sheet])}
     (if @(rf/subscribe [::ag/rows-expanded :sheet])
       "Contract" "Expand")]
    [:span.spacer]]     ;white space between filter and modal
   [ag/filter-ui :sheet]
   ;; TODO both these could be a bit smarter: disable themselves and have better prompt in initial state.
   ;; File match button
   [:span.spacer]     ;white space between filter and modal
   (let [{:keys [matching? to-match]}  @(rf/subscribe [:page-state])]
     [modal-button
      matching?
      "Match files"
      (str "Save " (inflect/pluralize (count to-match) "match"))
      ::toggle-matching])
   ;; Edit button
   (let [{:keys [editing? edits]}  @(rf/subscribe [:page-state])]
     [modal-button
      editing?
      "Edit cells"
      (str "Save " (inflect/pluralize (count edits) " edit"))
      ::toggle-edit-mode])
   ])

(defn ag-sheet-table
  [matching?]
  ;; Idea, we want page-state, modified by modal buttons (Edit cells, File matching) to trigger a refresh...
  (let [
        ]
    [ag/ag-table 
     :sheet
     @(rf/subscribe [::displayed-columns])
     @(rf/subscribe [::columns]) 
     @(rf/subscribe [::sheet-data])
     {:getRowClass (fn [params]
                     (let [data (.-data params)
                           files (and data (.-files data))
                           row (and data (aget data "row-id"))
                           pending-match-rows (and 
                                               matching?
                                               ;; Note: doing a subscribe here seems iffy, but doing it outside the [ag/ag-table ...] doesn't work
                                               (->> @(rf/subscribe [:page-state])
                                                    :to-match
                                                    (map first)))

                           ]
                       (cond (and matching? (some #(= % row) pending-match-rows))
                             "match-pending"
                             (empty? files)
                             "unmatched")))
      :rowSelection (if matching?
                      "single" "multiple")
      :onRowSelected (fn [params]
                       ;; if (params.node.data.updatingFromDetails) return;
                       (let [node (.-node params)
                             data (js->clj (.-data node) :keywordize-keys true)
                             api (.-api params)
                             detail-grid-id (str "detail_" (:row-id data))
                             ;; the result from getDetailGridInfo appears to be clj map object? How is that possible?
                             detail-api (:api (.getDetailGridInfo api detail-grid-id))
                             selected? (.isSelected node)
                             ]
                         (when detail-api
                           (if selected?
                             (.selectAll detail-api)
                             (.deselectAll detail-api)))
                         ))
      }
     :editable? (and @(rf/subscribe [:editing?])
                     (fn [row col value]
                       (rf/dispatch [:cell-edited row col value])))
     ]))

;;; Matcher stuff

;;; TODO could subsume to :page-state
(rf/reg-sub
 :matching?
 (fn [db _]
   (get-in db [:page-state :matching?])))

(rf/reg-event-db
 ::toggle-matching
 (fn [db [_ cancel?]]
   (if (get-in db [:page-state :matching?])
     (do
       (when-not cancel?
         (rf/dispatch [:do-pseudo-op :manual-file-match
                       {:matches (get-in db [:page-state :to-match])}]))
       (-> db 
           (assoc-in [:page-state :matching?] false)
           (assoc-in [:page-state :to-match] nil)))
     (assoc-in db [:page-state :matching?] true))))

(defn ui
  []
  (let [matching? @(rf/subscribe [:matching?])]
    [:div {:style {:padding "10px"}}
     [sheet-chooser]
     (if matching?
       [:div.row {:style {:height "85%"}}
        [:div.col-6
         ;; Note: necessary to give the sheets separate keys so gets rerendered. PITA
         ^{:key "sheetm"}               
         [ag-sheet-table matching?]]
        ;; Middle column
        [:div.col-2.hcenter
         [matcher/link-button]
         [:div "To match files:" [:br][:br]
          "← select one row"  [:br]
          "and one file →" [:br]
          "and click the link icon above." [:br][:br]
          "You can do this repeatedly." [:br][:br]
          "When done, click "
          [:button.btn.btn-outline-secondary.modal-button.modal-button-on
           { :style {:pointer-events "none" :zoom 0.7}}
           "Save matches"]
          " above."]]
        [:div.col-4
         [files/ui true]]]
       ;; Non-matching view
       [:div.row {:style {:height "85%"}}         ;TODO argh just want to fill the screen, apparently that is too hard for css
        [:div.col-12
         ^{:key "sheetnm"}
         [ag-sheet-table matching?]]])]))

(rf/reg-event-fx
 ::set-sheet-property
 (fn [{:keys [db]} [_ property value]]
   (let [sheet-id (:sheet db)
         changes {:db/id sheet-id property value}]
     {:dispatch [:update-entity changes]})))

;;; Nav
  
(defn sheet-url
  [sheet]
  (let [sheet-eid (entity/coerce-eid sheet)
        ;; TODO this used to have an extra 0 term, not sure why or how that worked...
        ;; OK, on dev, :batch/_sheets value is a seq. On production not I think?
        batch @(rf/subscribe [:entity-in [sheet-eid :batch/_sheets 0 :db/id]])
        project @(rf/subscribe [:entity-in [batch :project/_batches :db/id]])]
    (nav/url-for :sheet :project project :batch batch :sheet sheet)))

(defn sheet-link
  [sheet]
  (let [sheet (entity/coerce sheet)]
    (nav/link (sheet-url sheet)
              (:sheet/name sheet))))

;;; Edit mode

;;; TODO maybe not needed
(rf/reg-sub
 :editing?
 (fn [db _]
   (get-in db [:page-state :editing?])))

;;; TODO rob: Why not reg-event-fx with a dispatch-n?
(rf/reg-event-db                        ;TODO -fx
 ::toggle-edit-mode
 (fn [db [_ cancel?]]
   (if (get-in db [:page-state :editing?])
     (do
       (.stopEditing (:sheet @ag/ag-apis)) ;TODO ugly
       ;; Extra step so grid can report a final value if any. TODO not working dammit
       (when-not cancel?
         (rf/dispatch [:do-pseudo-op :update-cells {:edits (get-in db [:page-state :edits])}]))
       (-> db
           (assoc-in [:page-state :edits] [])
           (assoc-in [:page-state :editing?] false)))
     (-> db
         (assoc-in [:page-state :editing?] true)
         ))))

(rf/reg-event-db
 :cell-edited
 (fn [db [_ row col value]]
   ;; TODO update local data structure
   (update-in db [:page-state :edits] conj [(:row-id row) col value])))


