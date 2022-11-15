(ns org.parkerici.rawsugar.aggrid
  (:require [reagent.core :as reagent]
            [re-frame.core :as rf]
            ["ag-grid-community" :as ag]
            ["ag-grid-enterprise" :as age]
            ["ag-grid-react" :as agr]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.rawsugar.web-utils :as wu]
            [reagent.dom.server]
            [clojure.string :as str]
            )
  (:require-macros
   [org.parkerici.rawsugar.macros :refer (ag-grid-license)]))

;;; Wrapping of ag-grid. This is trying to be data and application agnostic, but in practice some
;;; code specific to the two Rawsugar views (sheet and files) has crept in. 

(def ag-adapter (reagent/adapt-react-class agr/AgGridReact))

(def license-key (ag-grid-license))

;;; Supposed to be done once, thus this top-level call.
(when license-key
  (.setLicenseKey age/LicenseManager license-key))

(defmulti ag-col-def (fn [col-id _col-def] col-id))

(defn detail-grid-options [this]
  {:defaultColDef {:sortable true
                   :resizable true
                   }
   :columnDefs [(ag-col-def :pathname nil)
                {:headerName "Location"
                 :field :location
                 :editable false
                 }]
   :onGridReady (fn [params]
                  (let [api (.-api params)
                        detail-grid-id (str "detail_" (.-masterRowId this))
                        master-api (.-masterGridApi this)
                        grid-info {:id detail-grid-id
                                   :api api
                                   :columnApi (.-columnApi params)}]
                    (.addDetailGridInfo master-api detail-grid-id grid-info)
                    (when (.-startSelected this)
                      (.selectAll api))
                    (.sizeColumnsToFit api)
                    ))
   :onFirstDataRendered (fn [params]
                          (let [column-api (.-columnApi params)] 
                            (.autoSizeColumns column-api (clj->js [:pathname :location]))))
   :rowSelection "multiple"
   :rowMultiSelectWithClick true
   :rowDeselection true
   })

;;; A component to pass as renderer to detail views
(deftype DetailCellRenderer []
  Object
  (init [this params]
    (let [params (js->clj params :keywordize-keys true)
          master-node (.-parent (:node params))
          selected? (.isSelected master-node)]
      (set! (.-masterGridApi this) (:api params))
      (set! (.-masterRowId this) (:row-id (:data params))) ;get row-id out of data and use it as masterRowId
;      (set! (.-masterNode this) master-node)
      (set! (.-detailRowSelectedHandler this) (:detailRowSelectedHandler params))
      (set! (.-startSelected this) selected?)
      ;; setupDetailGrid
      (let [egui (.createElement js/document "div")]
        (set! (.-eGui this) egui)
        (.add (.-classList (.-eGui this)) "ag-details-row") ;??? full-width-panel
        (set! (.-innerHTML (.-eGui this)) "<div class=\"full-width-grid\"></div>")
        (set! (.-masterRowData this) (:data params))
        (let [eDetailGrid (.querySelector egui ".full-width-grid")
              files (:files (:data params))]
          (ag/Grid.
           eDetailGrid
           (clj->js
            (merge (detail-grid-options this)
                   {:rowData files}))))))
    nil)
  
  ;; Note: the existence of this method marks it as a component
  (getGui [this]
    (.-eGui this))

  (destroy [this]
    (prn :destroy this)
    )
  )

;;; Keep pointers to API objects for various purposes. This maps a keyword id (:sheet, :files) to the appropriate API object.
;;; Note: this doesn't survive a figwheel reload. Maybe store in the re-frame db instead?
(def ag-apis (atom {}))

;;; Out of service for now
#_
(defn highlight-string
  [string pattern]
  (if pattern
    (into []
          (cons :span
                (u/re-substitute pattern
                                 string
                                 (fn [match] [:b match] ))))
    [:span {:style {:color :orange}} string]))

#_
(defn highlighter
  [regex value]
  (if (and regex value)
    (reagent.dom.server/render-to-string
     (highlight-string (str value) regex))
    value))

(rf/reg-event-db
 ::filter-string-change
 (fn
  [db [_ db-id new-string]]
  (let [api (get @ag-apis db-id)]
    (.setQuickFilter api new-string)
    ;; This was supposed to get highlighting to work, but it never quite did
    #_ (.refreshCells api (clj->js {:force true})))
  (assoc-in db [::grid db-id ::filter-string] new-string)))

(rf/reg-sub
  ::filter-string
  (fn [db [_ db-id]]
    (get-in db [::grid db-id ::filter-string])))

(defn filter-ui
  [db-id]
  [:span 
   [:input.form-control.filter
    {:value @(rf/subscribe [::filter-string db-id])
     :placeholder "Filter"
     :on-change (fn [e]
                  (rf/dispatch
                   [::filter-string-change db-id (-> e .-target .-value)]))}]
   (wu/icon "clear" "clear" #(rf/dispatch [::filter-string-change db-id ""]))
   ])

(defmethod ag-col-def :default [col-id col-def]
  (let [editable? (if (:editable? col-def) true false)
        value-change (when editable? (:editable? col-def))] ;TODO looks funky
    {:headerName (:column/name col-def)
     :field col-id
     :editable editable?
     :valueFormatter (when-let [formatter (:formatter col-def)]
                       #(formatter (.-value %)))
     :onCellValueChanged (fn [data]
                           (let [row (js->clj (.-data data) :keywordize-keys true)
                                 col (:db/id col-def) ; (.-field (.-colDef data)) would work modulo keyword/name massaging
                                 value (u/coerce-numeric (.-newValue data))]
                             (value-change row col value)))
     }
    ))

;;; File display. These work on files with UNNAMESPACED keywords because that's what we get back from ag-grid. See lines with NOTE below

(defn file-url
  [file]
  (str "/file?id=" (:id file)))

(defn download-name
  [file]
  (let [pathname (:pathname file)
        ;; TODO probably a util fn for this
        [_ name ext] (re-matches #".*?([^/]*)\.(.*)$" pathname)]
    (str name "." ext)))

(defn render-file
  "file: file entity
   highlighter: fn that takes a string and renders it with highlights
   row: the row or row entity associated with the file, if any
   warehouse?: show the warehouse location rather than original filepath"
  [file highlighter _row warehouse?]
  [:span
   ^{:key (:id file)}                   ;NOTE TODO might not need this with ag-grid
   [:a {:href (file-url file)
        :download (download-name file)
        }
    (highlighter ((if warehouse? :location :pathname) file)) ;NOTE
    ]])

;;; TODO this and other column-specific stuff should be broken into a seperate file
(defmethod ag-col-def :files [_col-id _col-def]
  {:headerName "Files"
   :cellRenderer
   (fn [params]
     (when-let [data (.-data params)]
       ;; TODO Seems expensive to generate these for a render, esp when not used
       (let [files (js->clj (.-files data) :keywordize-keys true)
             row (js->clj data :keywordize-keys true)
             highlighter identity]
         (when-not (empty? files)
           (reagent.dom.server/render-to-string
            (if (> (count files) 1)
              [:span
               [:span.count (count files) " "]
               (render-file (first files) highlighter row false)]
              (render-file (first files) highlighter row false)))))))
   :comparator
   (fn [_ _ na nb invert?]              ;TODO invert? isn't used, could be a bug
     (let [metric (fn [row-node] (count (.-files (.-data row-node))))]
       (compare (metric na) (metric nb))))
   :filterValueGetter
   (fn [params]
     (when (.-data params)
       ;; TODO Seems expensive to generate these for a render, esp when not used
       (let [files (js->clj (.-files (.-data params)) :keywordize-keys true)]
         (str/join " " (map :pathname files)))))
   })

;;; For file detail views
;;; See also def for :file/pathname in files.cljs
(defmethod ag-col-def :pathname [_ _]
  {:headerName "Pathname"
   :field :pathname
   :cellRenderer
   (fn [data]
     (let [file (js->clj (.-data data) :keywordize-keys true)
           row (:row file)
           highlighter identity]
       (reagent.dom.server/render-to-string
        (render-file file highlighter row false))))
   :checkboxSelection true
   })

;;; Encapsulated crock: gets the row-id for sheets, the file-id for a file view
(defn- row-id
  [row]
  (or (get row "row-id")
      (and (get row "pathname")         ;just make sure its really a file
           (get row "id"))))

(defn ag-table
  "id: a keyword to identify this table
  displayed-columns: a list of column ids (keywords)
  column-defs: a map of column keys to properties. column-defs are column-entities with some optional extra attributes. See ag-col-def.
     :editable? : boolean to control editability
     :formatter : a fn on values to produce their display form
  data: 
  ag-grid-options: a map of values passed to ag-grid
  editable?: nil or a function that gets called with [row col-id value] arguments 
  checkboxes?: control whether checkboxes appear, defaults true
  class: css class to use for grid"
  [id displayed-columns column-defs data ag-grid-options & {:keys [editable? checkboxes? class] :or {checkboxes? true}}]
  (if (empty? displayed-columns)
    (wu/spinner)
    (let [detail? (some #(= % :files) displayed-columns) ;TODO inelegant, should be passed in as option
          column-defs (mapv #(ag-col-def % (assoc (get column-defs %) :editable? editable?)) displayed-columns)
          column-defs (if checkboxes?
                        (-> column-defs
                            (assoc-in [0 :checkboxSelection] true)
                            (assoc-in [0 :headerCheckboxSelection] true)
                            (assoc-in [0 :headerCheckboxSelectionFilteredOnly] true))
                        column-defs)
          column-defs (if detail?
                        ;; Only includes > expander if there are rows, which screws up alignment
                        ;; Want to fix that but doesn't seem like there is an option: https://www.ag-grid.com/javascript-data-grid/group-cell-renderer/
                        (update column-defs 0 merge {:cellRenderer "agGroupCellRenderer"
                                                     :showRowGroup true})
                        column-defs)
          ]
      [:div.ag-container {:class class}
       [:div {:className "ag-theme-balham"}
        (let [grid-options
              (merge                     ;merges grid options, optional detail grid options, and user supplied options
               {:defaultColDef {:sortable true
                                :filter "agTextColumnFilter" ; TODO have type-specific filters, see https://www.ag-grid.com/javascript-grid-filtering/#configuring-filters-to-columns
                                :resizable true
                                :minWidth 55 ;TODO(ag-grid) autoSize doesn't seem to respect this, it should

                                ;; This was a failed attempt to get highlighting working. Almost there but running into
                                ;; lagging echo, infinite react loops, and other unpleasantness. So giving up on it for now.
                                #_ :cellRenderer
                                #_ 
                                (fn [params]
                                  (let [value (.-value params)
                                        filter-string @(rf/subscribe [::filter-string id])
                                        ;; TODO re-pattern needs ignore-errors
                                        regex (let [filter (and filter-string (re-pattern filter-string))]
                                                (and (not (some #(= \( %) filter-string))
                                                     filter))
                                        ]
                                    (highlighter regex value)))
                                }
                :onGridReady (fn [params]
                               (swap! ag-apis assoc id (.-api params)))
                :onFirstDataRendered (fn [params]
                                       (let [column-api (.-columnApi params)] 
                                         (.autoSizeColumns column-api (apply array (map name displayed-columns)))))
                :columnDefs column-defs
                :suppressFieldDotNotation true
                :rowData data
                :rowSelection "multiple"
                :rowMultiSelectWithClick true
                :rowDeselection true
                :pagination true
                :paginationAutoPageSize true
                :onCellContextMenu (fn [params] (prn :right-clicked params)) ; TODO probably not needed?
                :onSelectionChanged (fn [params]
                                      (let [api (.-api params)
                                            rows (js->clj (.getSelectedRows api)) ; TODO could be more efficient
                                            selection (map row-id rows)]
                                        (rf/dispatch [::set-selection id selection])))

                :sideBar {:hiddenByDefault false ; visible but closed
                          :toolPanels [{:id "columns"
                                        :labelDefault "Columns"
                                        :labelKey "columns"
                                        :iconKey "columns"
                                        :toolPanel "agColumnsToolPanel"
                                        ;; Turning these off for now, might want to revisit in the future. Possibly incompatible with the master/detail feature?
                                        :toolPanelParams {:suppressRowGroups true
                                                          :suppressValues true
                                                          :suppressPivots true 
                                                          :suppressPivotMode true}
                                        }
                                       {:id "filters"
                                        :labelDefault "Filters"
                                        :labelKey "filters"
                                        :iconKey "filter"
                                        :toolPanel "agFiltersToolPanel"
                                        }]
                          }
                :animateRows true
                :statusBar {:statusPanels [{:statusPanel "agTotalAndFilteredRowCountComponent"
                                            :align "left"}]}
                }
               (when detail?
                 {:masterDetail true
                  :keepDetailRows true       ;keep the detail rows, to preserve their checkbox state
                  :keepDetailRowsCount 10000 ;all of them, Katie
                  ;; These turn off some performance-enhancing measures, 
                  ;;                 :rowBuffer 0
                  ;;                 :suppressMaxRenderedRowRestriction true
                  :detailCellRenderer DetailCellRenderer

                  ;; Makes expand/contract sticky over sheet changes, which maybe is a bit weird
                  :groupDefaultExpanded (if @(rf/subscribe [::rows-expanded id]) -1 0)

                  :isRowMaster (fn [dataitem]
                                 (not (empty? (.-files dataitem))))
                  :getRowHeight (fn [params]
                                  (if (and (.-node params) (.-detail (.-node params)))
                                    (let [data (.-data params)
                                          n-rows (count (.-files data))
                                          row-height 27 
                                          header-height 27]
                                      (+ 80 header-height (* n-rows row-height)))
                                    ;; default row height
                                    25))
                  })
               ag-grid-options)]
          ;; debug tool, for reporting config to ag-grid.com
          ;;         (print (.stringify js/JSON (clj->js grid-options)))
          [ag-adapter grid-options])
        ]])))

;;; Note: the selection actually gets ignored, but it is still necssary
;;; to mutate the db so link button updates properly
(rf/reg-event-db
 ::set-selection
 (fn [db [_ db-id selection]]
   (assoc-in db [::grid db-id ::selection] selection)))

(rf/reg-event-db
 ::clear-selection
 (fn [db [_ id]]
  (when-let [api (get @ag-apis id)]
    (.deselectAll api))
   (assoc-in db [::grid id ::selection] [])))

;;; Return the selection directly from the grid
(defn get-selection [id]
  (when-let [api (get @ag-apis id)]
    (let [rows (js->clj (.getSelectedRows api)) 
          selection (map row-id rows)]
      selection)))

(rf/reg-sub
 ::selection
 (fn [_ [_ db-id]]
   (get-selection db-id)))

(rf/reg-event-db
 ::expand-rows
 (fn [db [_ grid-id ]]
   (let [api (get @ag-apis grid-id)
         expanded (not (get-in db [::grid grid-id :expanded]))]
     (.forEachNode api (fn [node _index]
                         (when (.-master node)
                           (.setExpanded node expanded))))
     (assoc-in db [::grid grid-id :expanded] expanded)))) 

(rf/reg-sub
 ::rows-expanded
 (fn [db [_ grid-id]]
   (get-in db [::grid grid-id :expanded])))

;;; If row is checked, report all the files, otherwise use detail view
;;; TODO Should also rig it so master row checkbox is cleared when detail is edited.
(defn get-selected-files
  "Returns a seq of file ids"
  [gr-id]
  (let [api (get @ag-apis gr-id)
        collector (atom ())]
    (when api
      (.forEachNode api
                    (fn [node]             ;node==row
                      (let [row-files
                            (if (.-selected node)
                              ;; all files as represented in master row
                              (map #(.-id %) (.-files (.-data node)))
                              ;; get from detail grid – extremely convoluted, grump
                              (when-let [_detail-node (.-detailNode node)]
                                (let [detail-id (str "detail_" (aget (.-data node) "row-id"))
                                      detail-info (.getDetailGridInfo api detail-id)
                                      ;; Inexplicably a clj rather than js structure
                                      detail-api (:api detail-info)]
                                  (map #(.-id %) (.getSelectedRows detail-api)))))]
                        (swap! collector concat row-files)))))
    @collector))


;;; See https://ag-grid.com/react-data-grid/view-refresh/#redraw-rows
;;; This could be smarter and just redraw specific rows, but I'm lazy.
(defn redraw
  [gr-id]
  (.redrawRows (get @ag-apis gr-id)))
