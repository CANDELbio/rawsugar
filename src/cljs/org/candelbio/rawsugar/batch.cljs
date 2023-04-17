(ns org.candelbio.rawsugar.batch
  (:require [re-frame.core :as rf]
            [oz.core :as oz]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.rawsugar.navigate :as nav]
            [org.candelbio.rawsugar.recipe :as recipe]
            [org.candelbio.rawsugar.sheet :as sheet]
            [org.candelbio.rawsugar.aggrid :as ag]
            [org.candelbio.rawsugar.api :as api]
            [org.candelbio.rawsugar.web-utils :as wu]
            [org.candelbio.rawsugar.entity :as entity]
            [org.candelbio.rawsugar.utils :as ru]
            [clojure.string :as str]
            [clojure.edn :as edn]
            ))

;;; Note: these should match the css classes
(def sheet-rs-color "red")
(def sheet-latest-color "#81b309")      ;no this is NOT the css color, which is too light...it's the same hue at least

(def safe-name (u/safely name))

(def none-value "--none--")

(defn status-options []
  (cons {:value none-value :label "<None>"}
        (map (fn [[k doc]]
               {:value k :label doc})
             (get-in @(rf/subscribe [:schema]) [:enums :batch-status :values]))))

(defn dashboard-ui [batch-id header]
  (when-let [dash @(rf/subscribe [::dashboard batch-id])]
    (case dash
      :loading [:span "Loading..."]
      :none [:span "<None>"]
      [:div
       header
       [oz/vega-lite dash]])))

(defn set-datatype
  [batch new-dt]
  (rf/dispatch [::set-batch-property :batch/datatype new-dt])
  ;; ugly
  (rf/dispatch [:eval
                `(org.candelbio.rawsugar.recipes/instantiate-recipe ~(:db/id batch) ~new-dt)
                #(rf/dispatch [:batches-changed])]))

;;; Sheet provenance computation


(defn reconstruct-provenance

  "Given a  batch. return a seq of sheet entities, annotated with :predecessor links and a few other tweaks for display. Uses both :sheet/derived-from and op information"

  [batch]
  (let [sheets (:batch/sheets (entity/refresh batch))
        sheet-ids (set (map :db/id sheets))
        latest (u/max-by #(get-in % [:object/creation-op :operation/time])
                         sheets)]
    (for [sheet sheets]
      (let [creation (:object/creation-op sheet)
            parameters (edn/read-string (:operation/parameters creation))
            cmd (:operation/cmd creation)
            cmd (if (= cmd :op) (first parameters) cmd)]
        (assoc sheet
               :cmd cmd
               :parameters parameters
               :latest (= sheet latest)
               :predecessors 
               ;; This is kind of specifc to :ops, but we are making everything an :op
               (if (:sheet/derived-from sheet)
                 (list (:db/id (:sheet/derived-from sheet)))
                 (when (= :op (:operation/cmd creation))
                   (let [params (vals (second parameters))]
                     (filter (partial contains? sheet-ids) params)))))))))


;;; Provenance outline display

;;; Some graph specs. These control the vertical layout of nodes. Making strenght high push graph down for some unknown reason
(def layer-separation 100)
(def layer-strength 2)

;;; Note: Vega has built-in tree layout machinery that I'm not using because
;;; it doesn't handle DAGs. 

(defn spec
  [data {:keys [controls? link-distance node-charge node-radius] :as options :or {link-distance 60 node-charge -100 node-radius 20}}]
  ;; Reasonable for small depths
  (let [height (* 2 (u/max* (map :depth (get-in data [0 :values]))))]
    `{:$schema "https://vega.github.io/schema/vega/v5.json"
      :data ~data
      :autosize "pad"
      :width 600
      :height ~height
      ;;    :usermeta {:embedOptions {:actions false}} ;this turns off the menu with editor etc.
      ;;    :padding 10
      :scales
      [{:name "color"
        :type "ordinal"
        :domain {:data "node-data" :field "cmd"}
        :range "category"
        }]
      :legends
      [{:fill "color",
        :title "Cmd",
        :fillColor "lightgray"
        :padding 10
        :orient "none"
        :legendY -100
        :legendX 0
        }]
      :marks
      [{:name "nodes"
        :type "symbol"
        :zindex 1
        :from {:data "node-data"}
        :on
        [{:trigger "fix" :modify "node" :values "fix === true ? {fx: node.x, fy: node.y} : {fx: fix[0], fy: fix[1]}"}
         {:trigger "!fix" :modify "node" :values "{fx: null, fy: null}"}]
        :encode
        {:enter {:size {:value 180}
                 :shape {:value "square"}
                 :tooltip {:signal "{title: datum.name, 'via': datum.cmd, 'agent':datum.agent, 'created': timeFormat(datum.time, '%b %d %H:%M'), 'rows': datum.row_count, 'columns':datum.column_count }"}
;                 :tooltip {:signal "datum"} ;for debugging
                 :fill {:scale "color" :field "cmd"}
                 :href {:signal "datum.href"} ;TODO figure out how to do SPA navigation – surprised Vega doesn't make that easier
                 }
         }
        :transform
        [{:type "force"
          :iterations 300
          :restart {:signal "restart"}
          :static {:signal "static"}
          :signal "force"
          :forces
          [{:force "center" :x {:signal "cx"} :y {:signal "cy"}}
           {:force "collide" :radius {:signal "nodeRadius"}}
           {:force "nbody" :strength {:signal "nodeCharge"}}
           {:force "y" :y {:field "datum.depth"} :strength ~layer-strength} 
           {:force "link" :links "link-data" :distance {:signal "linkDistance"}}]}]}

       {:name "nodelabels"
        :type "text"
        :from {:data "nodes"}
        :zindex 2
        :encode
        {:enter {:text {:field "datum.name"}
                 :x {:signal "datum.x + 2 + sqrt(datum.size)/2"}
                 :y {:signal "datum.y + 3"}
                 :fontSize {:value 16}
                 :stroke {:field "datum.font_color"}
                 :fill {:field "datum.font_color"}
                 :fontWeight {:field "datum.font_weight"}
                 ;; Note: vega can't do background colors, underlines, or css
                 }
         :update {
                  :x {:signal "datum.x + 2 + sqrt(datum.size)/2"}
                  :y {:signal "datum.y + 6"}

                  }}}
       {:type "path"
        :from {:data "link-data"}
        :interactive false
        :encode {:update
                 {:stroke {:value "gray"}
;;; this does hover highlighting of links but that is useless;
;;; TODO would be neat to highlight the complete upstream path
                  ;;                :strokeWidth {:signal "datum.source === node || datum.target === node ? 2 : 0.5"}
                ;;; ooh treeAncestors! (https://vega.github.io/vega/docs/expressions/#tree-functions But we don't have a tree god damn it. Except that we do...
                  :shape {:value "arrow"}}
                 }
        :transform
        [{:type "linkpath"
          :require {:signal "force"}
          :shape "diagonal"
          :sourceX "datum.source.x"
          :sourceY "datum.source.y"
          :targetX "datum.target.x"
          :targetY "datum.target.y"}
         ]
        }
       ]
      :signals
      [{:name "cx" :update "width / 2"}
       {:name "cy" :update "height / 4"}
       {:name "hover"
        :value nil
        :on [
             {:events "@nodes:mouseover" :update "datum"}
             {:events "@nodes:mouseout" :update "null"}
             ]
        }
       {:name "nodeRadius" :value ~node-radius :bind ~(and controls? {:input "range" :min 1 :max 50 :step 1})}
       {:name "nodeCharge" :value ~node-charge :bind ~(and controls? {:input "range" :min -100 :max 100 :step 1})}
       {:name "linkDistance" :value ~link-distance :bind ~(and controls? {:input "range" :min -100 :max 100 :step 1})}
       {:name "static" :value false :bind ~(and controls? {:input "checkbox"})}
       {:description "State variable for active node fix status."
        :name "fix"
        :value false
        :on
        [{:events "symbol:mouseout[!event.buttons], window:mouseup" :update "false"}
         {:events "symbol:mouseover" :update "fix || true"}
         {:events "[symbol:mousedown, window:mouseup] > window:mousemove!"
          :update "xy()"
          :force true}]}
       {:description "Graph node most recently interacted with."
        :name "node"
        :value nil
        :on [{:events "symbol:mouseover" :update "fix === true ? item() : node"}]}
       {:description "Flag to restart Force simulation upon data changes."
        :name "restart"
        :value false
        :on [{:events {:signal "fix"} :update "fix && fix.length"}]}]
      }))

(defn sconj
  [coll elt]
  (conj (set coll) elt))

(defn add-parents
  "Given a db (map of maps), and a multi-valued attribute children-at, compute the multi-valued inverse relationship as parent-att"
  [db children-att parent-att]
  (reduce-kv (fn [acc key item]
               (reduce (fn [acc child]
                         (if (contains? acc child)
                           (update-in acc [child parent-att] sconj key)
                           acc))
                       acc
                       (children-att item)))
             db
             db))

;;; Inefficient, needs to memoize
;;; TODO → multitool 0.0.16
(defn stratify
  [g relation]
  (letfn [(depth [node-id]
            (let [predecessors (relation (get g node-id))]
              (if (empty? predecessors)
                0
                (+ 1 (apply max (map depth predecessors))))))]
    ;; TODO would be better to scale this in Vega, but couldn't quite figure that out
    ;; Also, the nodes need have actualy size and then they will repel each other better.

    
    ;; TODO better layout, from bottom or something.

    (u/map-values #(assoc % :depth (* layer-separation (depth (:db/id %))))
                  g)                   

    ))

(defn separate
  [g]
  (let [strata (group-by :depth (vals g))
        separated (mapcat (fn [[depth# nodes]]
                            (map-indexed (fn [i node] (update node :depth + (float (* layer-separation (/ i (count nodes))))))
                                         nodes))
                          strata)]
    (u/index-by :db/id separated))) ;re-index
            
;;; Another idea: adjust depth (before spearate) so parents are always -1 to their deepest children ... would improve distribution between strata
    

(defn add-index
  "Seq is seq of maps, this adds an :index slot"
  [seq]
  (map-indexed (fn [i item] (assoc item :index i))
               seq))

(defn make-graph
  [provenance]
  (let [nodes (add-index provenance)
        nodes (map #(-> %
                        (select-keys [:db/id :index :sheet/name :predecessors :cmd])
                        (assoc :column_count (count (:sheet/columns %)))
                        (assoc :row_count (:sheet/row-count %))
                        (assoc :agent (get-in % [:object/creation-op :operation/agent]))
                        (assoc :time (get-in % [:object/creation-op :operation/time]))
                        (assoc :href (sheet/sheet-url (:db/id %)))

                        ;; This would be cleaner, but I can't get conditionals in vega working
                        ;; (assoc :run_step (not (nil? (:run-step/_sheet %))))
                        ;; highlight step-associated sheets
                        ;; TODO wanted to do this as a computed signal but I can't get that to work
                        (assoc :font_color (cond (:latest %) sheet-latest-color
                                                 (:run-step/_sheet %) sheet-rs-color
                                                 :else "black"))
                        (assoc :font_weight (if (:or (:latest %) (:run-step/_sheet %)) "bold" "normal")
)                        
                        (assoc :font_size (if (:run-step/_sheet %) 24 16))
                        (assoc :font_style (if (:run-step/_sheet %) "italic" nil)))
                   nodes)
        index (u/index-by :db/id nodes)
;;; TODO this isn't quite working yet, but ought to produce a better layout.
;;;        index (add-parent index :predecessors :successors)
        node-index #(get-in index [% :index])
        stratified (separate (stratify index :predecessors))
        links (mapcat (fn [{:keys [db/id predecessors]}]
                        (map (fn [predecessor]
                               {:source (node-index predecessor) :target (node-index id)})
                             predecessors))
                      provenance)]
    ;; TODO note: the sort is necessary apparently order matters and :index does not
    [{:name "node-data" :values (sort-by :index (vals stratified))}
     {:name "link-data" :values links}]))


(defn graph
  [prov]
  [:div.pt-3
  (if (empty? prov)
    "No sheets yet"
    [oz/vega
     (spec (make-graph prov)
           {:controls? false :node-charge 100 :link-distance 100 :node-radius 50})])])

(defn op-pseudo? [cmd]
  (let [ops @(rf/subscribe [:ops])]     ;TODO it occurs to me there is no point putting static structure in the db, just set a variable or atom
    (get-in ops [:definitions :operations cmd :pseudo?])))

(defn op-link
  [cmd parameters]
  (if (op-pseudo? cmd)
    [:i ((u/safely name) cmd)]
    [:a {:href "#" :on-click #(rf/dispatch [:op-dialog cmd parameters])}
     [:i ((u/safely name) cmd)]          
     ]))

(defn abbreviate-agent
  [agent-string]
  (first (str/split agent-string "@")))

;;; Note: shortened the metadata, but might be better to have it just as a popup
;;; Added the counts, but now it is too much
(defn sheet-line
  [{:keys [cmd parameters] :as sheet}]
  ^{:key (:db/id sheet)}
  [:span {:class (cond (:run-step/_sheet sheet) "sheet-rs"
                        (:latest sheet) "sheet-latest"
                        )}
   [:span
    (sheet/sheet-link sheet)]
   " via "
   (op-link cmd (second parameters))
   [:span
    " ["
    (str (:sheet/row-count sheet))
    "×"
    (str (count (:sheet/columns sheet)))
    "]"]
   [:span
    " ("
    (abbreviate-agent (get-in sheet [:object/creation-op :operation/agent]))
    ", "
    (wu/format-time (get-in sheet [:object/creation-op :operation/time])
                   wu/short-time-format)
    ")"]])

(defn sheet-children
  [sheet prov]
  (filter #(= (first (:predecessors %)) (:db/id sheet))
          prov))

(defn sheet-tree
  [sheet prov]
  ^{:key (str (:db/id sheet))}
  [:li (sheet-line sheet)
   [:ul.sheetree
    (doall
     (for [child (sheet-children sheet prov)]
       (sheet-tree child prov)))]])

;;; Top-level UI component
(defn provenance-tree
  [sheets]
  [:div.pt-3
   (if (empty? sheets)
     "No sheets yet"
     [:ul
      (doall
      (for [sheet (filter #(empty? (:predecessors %))
                          sheets)]
        (sheet-tree sheet sheets)))])])

;;; Stolen from Vaguely!
(rf/reg-sub
 :active-tab
 (fn [db [_ id]]
   (get-in db [:active-tab id])))

(rf/reg-event-db
 :choose-tab
 (fn [db [_ id tab]]
   (assoc-in db [:active-tab id] tab)))

(defn tabs
  [id tabs]
  (let [active (or @(rf/subscribe [:active-tab id]) "Outline")]
    [:div
     [:ul.nav.nav-tabs
      (for [[name view] tabs]
        ^{:key name}
        [:li.nav-item
         (if name
           [:a.nav-link {:class (when (= name active) "active")
                         :on-click #(rf/dispatch [:choose-tab id name])}
            name]
           [:a.nav-link.disabled.vtitle view])])]
     ((tabs active))]))

(defmethod ag/ag-col-def :sheet-name [col-id col-def]
  {:headerName "Sheet"
   :field col-id
   :cellClass (fn [x]
                (let [data (.-data x)
                      run-step (aget data "run-step")
                      latest (aget data "latest")]
                  (cond run-step
                        "sheet-rs-ag"
                        latest
                        "sheet-latest"
                        :else
                        nil
                        )))
   }
  )

(def table-columns
  [[:sheet-name :sheet/name]                  ;wanted to make this bold when associated with run-step, that looks a wee bit hard
   ;; OK this is kind of horrific

   [:run-step (fn [sheet]
                (-> sheet
                 :run-step/_sheet
                 first                  ;not clear why this is needed, wrong cardinality???
                 :db/id
                 entity/entity
                 :run-step/step-id
                 ))]

   [:date #(wu/format-time (get-in % [:object/creation-op :operation/time])
                           wu/short-time-format)]
   [:via :cmd]
   [:agent #(get-in % [:object/creation-op :operation/agent])]
   [:row-count :sheet/row-count]
   [:column-count (comp count :sheet/columns)]
   [:latest :latest]])                  ;TODO should hide this one, it's used for highlighting only

(defn table-data
  [provenance]
  (map (fn [prov]
         (reduce (fn [row [colname col-vf]]
                   (assoc row colname (col-vf prov)))
                 {:id (:db/id prov)}
                 table-columns))
       provenance))

(defn table
  [provenance]
  (let [project @(rf/subscribe [:project true])
        batch @(rf/subscribe [:batch true])]
    (ag/ag-table
     :sheet-provenance
     (map first table-columns)
     table-columns
     (table-data provenance)
     {:onCellClicked (fn [e]
                       (let [node (.-data e)
                             id (.-id node)]
                         (rf/dispatch [:navigate-route :sheet :sheet id :project project :batch batch])))}
     :class :prov-grid
     :checkboxes? false
     )))

(defn ui
  [options]
  (let [project @(rf/subscribe [:project true])
        batch @(rf/subscribe [:batch true])
        batch-types (sort-by second (get-in @(rf/subscribe [:schema]) [:enums :datatype :values]))
        datatype (:batch/datatype batch)
        ]
    [:div.page-content
     [:h1 [:span.blackf "Batch "]
      ;; TODO should go through :rename-batch op
      [wu/editable-text-widget ;TODO turn off border
       :name
       {:initial-value (:batch/name batch)
        :change-event [::set-batch-property :batch/name]
        :borderless? true}]]
     [:h3.pt-3 "Properties"]

     [:table.optable.table.responsive
      [:tbody

      [wu/form-row "description"
       [wu/editable-textarea-widget
        :description
        (:batch/description batch)
        #(rf/dispatch [::set-batch-property :batch/description %])]]

      [wu/form-row "datatype"
       (wu/select-widget "btype"
                         (safe-name (:batch/datatype batch))
                         #(when (js/confirm "This will reinitialize recipe steps; are you sure?")
                            (set-datatype batch (if (= % none-value) nil (keyword %))))

                         (cons {:value none-value :label "<None>"}
                               (map (fn [[key label]]
                                      {:value (name key) :label label})
                                    batch-types)) ;TODO order
                         nil)]

      [wu/form-row "status"
       [wu/select-widget
        :status
        (safe-name (:batch/status batch))
        #(rf/dispatch [::set-batch-property :batch/status (keyword %)])
        (status-options)
        nil]]

      [wu/form-row "request"
       [wu/editable-text-widget
        :request
        {:initial-value (:batch/request batch)
         :change-event [::set-batch-property :batch/request]}] ]

      [wu/form-row "shipment"
       [wu/editable-text-widget
        :shipment
        {:initial-value (:batch/shipment batch)
         :change-event [::set-batch-property :batch/shipment]}]]

      ]]
     [:h3 "Files"]
     [:div.container.m-2
      [:a {:href (nav/files-url project batch)} 
       (ru/pluralize (:batch/file-count batch) "file")]]
     [:h3 "Dashboard"]
     [:div.scontent
      #_ [dashboard-ui (:db/id batch) nil]]
     ;; Two column layout
     [:div.container.pt-3
      [:div.row
       ;; Recipes
       (when datatype
         [:div.col-3
          [recipe/recipe-title (:db/id batch)]
          [recipe/do-run-ui (:db/id batch)]])
       [:div.pl-0
        {:class (if datatype "col-9" "col-12")
         :style {:padding-left "0px"}}  ;.pl-0 should do this but not working for some reason
        [:h3 "Sheets"]
        (let [provenance (reconstruct-provenance batch)]
          (tabs :sview
                ;; Thought making this a component might help refresh, it doesn't
                {"Outline" #(provenance-tree provenance)
                 "Graph" #(graph provenance)
                 "Table" #(table provenance)
                 }))
        ]]]
     ]))

;;; No longer really necessary
(rf/reg-event-fx
 ::set-batch-property
 (fn [{:keys [db]} [_ property value]]
   {:dispatch [:update-entity {:db/id (:batch db) property value}]}
   ))

;;; Returns a map which is passed directly to Vega.
;;; The db value of [:batch-dash batch] can be a Vega map, :none, or :loading

(rf/reg-event-db
 ::get-dashboard
 (fn [db [_ batch]]
   (api/api-get "/batch/dashboard"
                {:params {:batch batch}
                 :handler #(rf/dispatch [::got-dashboard batch %1])})
   ;; Ensure request only happens once. 
   (assoc-in db [:batch-dash batch] :loading)))   

(rf/reg-event-db
 ::got-dashboard
 (fn [db [_ batch hiccup]]
   ;; Cant' let this be nil or the API will loop
   (assoc-in db [:batch-dash batch] (or hiccup :none))))

(rf/reg-sub
 ::dashboard
 (fn [db [_ batch]]
   (or (get-in db [:batch-dash batch])
       (do
         (rf/dispatch [::get-dashboard batch])
         nil))))



             


