(ns org.parkerici.rawsugar.ops
  (:require [re-frame.core :as rf]
            [reagent.core :as reagent]
            [org.parkerici.rawsugar.sheet :as sheet]
            [org.parkerici.rawsugar.batch :as batch]
            [org.parkerici.rawsugar.aggrid :as aggrid]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.rawsugar.web-utils :as wu]
            [org.parkerici.rawsugar.api :as api]
            [re-com.core :refer [at v-box tag-dropdown]]
            [inflections.core :as inflect]
            [ajax.url :as aurl]
            [clojure.string :as str]
            ))

;;; Client side of ops machinery. The ops themselves are defined on the server side.

(defn ops-dropdown
  "Generate the ops menu"
  []
  (let [{:keys [menu operations]}
        (:definitions @(rf/subscribe [:ops]))]
    (when-not (empty? menu)
      [:li.nav-item.dropdown
       [:a.nav-link.top-nav-link.dropdown-toggle
        {:id "navbarOps" :href "#" :role "button" :data-toggle "dropdown" :data-bs-toggle "dropdown"
         :aria-haspopup true :aria-expanded false} "ops"]
       `[:div.dropdown-menu.white {:aria-labelledby "navbarOps"}
         ~@(into []
              (mapcat (fn [[groupname & group-ops]]
                        (cons
                         [:h6.dropdown-header groupname]
                         (for [key group-ops]
                           [:a.dropdown-item {:href "#" :on-click #(rf/dispatch [:op-dialog key])}
                            (get-in operations [key :name])])))
                      menu))]])))

;;; Form machinery
;;; TODO this should be a multimethod for consistency
(defn form-default
  [op {:keys [name type default id] :as arg}]
  (or
   (case type
     :project @(rf/subscribe [:project])
     :batch @(rf/subscribe [:batch])
     :sheet @(rf/subscribe [:sheet])
     :rows (seq (aggrid/get-selection :sheet))
     :files (or 
             ;; the idea is to use the :files grid if it exists (on batch page or on file-matcher) and has checks, 
             ;; TODO this should probably not get returned if :files is not visible, ie on sheets page with matching not expanded
             (seq (aggrid/get-selection :files)) ;seq acts as (not (empty? ))
             ;; or else get files out of sheet grid detail views.
             (seq (aggrid/get-selected-files :sheet)))
     default)
   default))


(rf/reg-sub
 :form-field-value
 (fn [db [_ field]]
   (get-in db [::ops :form field])))

(def none-value "--none--")              ;need a non-nil value to mean none

(rf/reg-event-db
 :set-form-field-value
 (fn [db [_ field value]]
   (let [value (if (= value none-value) nil value)]
     (assoc-in db [::ops :form field] value))))

;;; Create a UI for a form field.
;;; First arg is a map, options are pretty obvious (TODO)
;;;  :type datatype keyword (eg :numeric or :project), used for dispatch
;;; Second arg (value) is typically a subscription to [:form-field]

(defmulti form-field-ui (fn [{:keys [name doc type hidden] :as arg}] type))

(defmethod form-field-ui :default [{:keys [name read-only]}]
  [:input.form-control
   {:name name
    :value @(rf/subscribe [:form-field-value name])
;    :disabled false
    :on-change (fn [e]
                 (rf/dispatch
                  [:set-form-field-value name (-> e .-target .-value)]))
    :on-key-press (fn [evt]
                    (when (= "Enter" (.-key evt))
                      ;; Note: should be same as modal ok button 
                      (rf/dispatch [::op-execute])
                      nil))
    }])

;;; TODO identical to default except for coercion
(defmethod form-field-ui :number [{:keys [name read-only]}]
  [:input.form-control
   {:name name
    :value @(rf/subscribe [:form-field-value name])
;    :disabled false
    :on-change (fn [e]
                 (rf/dispatch
                  [:set-form-field-value name (u/coerce-numeric (-> e .-target .-value))]))
    }])

(defmethod form-field-ui :new-sheet-name [{:keys [name]}]
  [:input.form-control
   {:name name
    :value
    ;; Default to <sheet>+
    ;; TODO would be good if it was smart enought to update default if (a) sheet changes and (b)
    ;; user hasn't changed default. But that's hairy and I don't think really necesssary.
    (or @(rf/subscribe [:form-field-value name]) 
        (let [sheet-name (:sheet/name @(rf/subscribe [:entity @(rf/subscribe [:form-field-value :sheet])]))
              new-name (str sheet-name  "+")]
          (when sheet-name (rf/dispatch [:set-form-field-value name new-name]))
          new-name
        ))
    :on-change (fn [e]
                 (rf/dispatch
                  [:set-form-field-value name (-> e .-target .-value)]))
    :on-key-press (fn [evt]
                    (when (= "Enter" (.-key evt))
                      ;; Note: should be same as modal ok button 
                      (rf/dispatch [::op-execute])
                      nil))
    }]
  )

(defn- select-option
  [label-attribute entity]
  {:value (:db/id entity)
   :label (label-attribute entity)})

(defn- select-options
  [label-attribute entities optional?]
  (let [base (sort-by (comp str/lower-case :label)
                      (map (partial select-option label-attribute) entities))]
    (if optional?
      (cons {:value none-value :label  "<None>"} base)
      base)))

;;; This is to make sure that the default select option, visible to the user, is also
;;; set in the database. It's a hack, maybe should be combined with wu/select-widget
(defn- magic-select-widget 
  [id value dispatch options & [disabled?]]
  (when (not (some #(= value (:value %)) options))
    (dispatch (:value (first options))))
  (if (empty? options)
    [:div.alert.alert-warning "You need to create a " (name id) " first!" ] ;TODO should disable perform button
    (wu/select-widget id value dispatch options nil disabled?)))

(defmethod form-field-ui :project [{:keys [name optional?]}]
  (magic-select-widget
   name
   @(rf/subscribe [:form-field-value name])
   #(rf/dispatch [:set-form-field-value name (u/coerce-numeric %)])
   (select-options :project/name  @(rf/subscribe [:projects]) optional?)
   false))

(defmethod form-field-ui :batch [{:keys [name optional?]}]
  (magic-select-widget
   name
   @(rf/subscribe [:form-field-value name])
   #(rf/dispatch [:set-form-field-value name (u/coerce-numeric %)])
   (select-options :batch/name (:project/batches @(rf/subscribe [:entity @(rf/subscribe [:form-field-value :project])]))
                   optional?)
   false))

(defmethod form-field-ui :sheet [{:keys [name optional?]}]
  (magic-select-widget
   name
   @(rf/subscribe [:form-field-value name])
   #(rf/dispatch [:set-form-field-value name (u/coerce-numeric %)])
   (select-options :sheet/name (:batch/sheets @(rf/subscribe [:entity @(rf/subscribe [:form-field-value :batch])]))
                   optional?)
   false))

(defmethod form-field-ui :column [{:keys [name read-only depends-on optional?]}]
  (let [sheet @(rf/subscribe [:form-field-value (or (:sheet depends-on) :sheet)]) ;??? always dependent
        cols (:sheet/columns @(rf/subscribe [:entity sheet]))]
    (magic-select-widget
     name
     @(rf/subscribe [:form-field-value name])
     #(rf/dispatch [:set-form-field-value name (u/coerce-numeric %)])
     (select-options :column/name cols optional?)
     read-only)))

;;; React multiselect widget
;;; TODO make order editable. This is possible but hairy: https://github.com/reagent-project/reagent/blob/master/examples/react-sortable-hoc/src/example/core.cljs

(defn multi-select-widget [name options selected-values]
  (let [model (reagent/atom (set selected-values))]
    [v-box
     :src (at)
     :align :start
     :children
     [[tag-dropdown :src (at)
       :model model
       :choices options
       :on-change (fn [choices]
                    (reset! model choices)
                    (rf/dispatch [:set-form-field-value name (seq @model)])) ;passing a set is problematic
       ;; seems to not work, and not really needede
       ;; :unselect-buttons? true
       ]]]
    )
  )

(defmethod form-field-ui :columns [{:keys [name read-only depends-on optional?]}]
  (let [sheet @(rf/subscribe [:form-field-value (or (:sheet depends-on) :sheet)]) ;??? always dependent
        cols (:sheet/columns @(rf/subscribe [:entity sheet]))
        options (mapv (fn [col] {:id (:db/id col) :label (:column/name col)}) cols)
        current @(rf/subscribe [:form-field-value name])]
    ;; TODO sequencify oughtn't be necessary
    (multi-select-widget name options (u/sequencify current))))

(defmethod form-field-ui :column-values [{:keys [name read-only depends-on optional?]}]
  (let [sheet @(rf/subscribe [:form-field-value (or (:sheet depends-on) :sheet)]) ;??? always dependent
        column @(rf/subscribe [:form-field-value (or (:column depends-on) :column)])
        ;; Warning: this relies on the "pun" that sheet abbreviates same thing on front and back end
        ;; The (take 100 ...) is to protect against user accidently selecting some data column with thousands of distinct values
        ;; Note that backquote screws up clojure.core namespace so we have to use list
        values (and sheet column
                    @(rf/subscribe [:eval (list 'clojure.core/take 100 `(sheet/sheet-column-values ~sheet ~column))]))
        options (mapv (fn [v] {:id v :label v}) values)
        current  @(rf/subscribe [:form-field-value name])]
    (multi-select-widget name options current)))

;;; TODO producing react warnings
(defmethod form-field-ui :boolean [{:keys [name read-only doc type hidden]}]
  [:input.form-check-input
   {:name name
    :type "checkbox"
    :checked @(rf/subscribe [:form-field-value name])
    :disabled read-only
    :on-change (fn [e]
                 (rf/dispatch
                  [:set-form-field-value name (-> e .-target .-checked)]))}])


;;; For upload
(defmethod form-field-ui :local-files [{:keys [name read-only doc type hidden]}]
  [:input.form-control
   {:name name
    :type "file"
    :multiple "true"
    :id "upload-file"
    :on-change (fn [e]
                 (rf/dispatch
                  [:set-form-field-value name (-> e .-target .-value)]))
    }])

(defmethod form-field-ui :local-directory [{:keys [name read-only doc type hidden]}]
  [:input.form-control
   {:name name
    :type "file"
    :multiple "true"
    :webkitdirectory "true"               ;black magic to enable folder uploads
    :mozdirectory "true"
    :directory "true"
    :id "upload-directory"
    }])
 
;;; Assumes all :hidden fields are sequences from checkboxes, which is true for now
(defn hidden-ui [{:keys [read-only doc type hidden] :as arg}]
  (let [arg-name (:name arg)
        value @(rf/subscribe [:form-field-value arg-name])]
    [:span.form-control.read-only
     (inflect/pluralize (count value) (inflect/singular (name arg-name))) " checked"]
    ))

(defn form
  [{:keys [args doc op]}]
  [:div#opform                          ; Was :form, but this way Enter key doesn't cause the apocalypse
   {:enc-type "multipart/form-data"
    :method "POST"}
   (when doc
     [:div.alert.alert-piciblue doc])
   [:table.optable.table.responsive
    [:tbody
     (doall                             ;re-frame requirement
      (for [{:keys [doc hidden indent] :as arg} args]
        (let [argname (:name arg)]      ;Avoid colliding with core/name
          [:tr {:key argname}
           [:th 
            (when indent "・ ") (name argname)]
           [:td 
            (if hidden
              (hidden-ui arg)
              (form-field-ui (assoc arg :read-only false)))]
           [:td {:style {:padding-left "5px" :min-width "200px"}} doc]]
          )))]]

   ])

;;; TODO complete → valid

(defmulti field-valid? :type)

(defmethod field-valid? :default
  [arg]
  @(rf/subscribe [:form-field-value (:name arg)]))

(defmethod field-valid? :string
  [arg]
  (not (empty? @(rf/subscribe [:form-field-value (:name arg)]))))

(defmethod field-valid? :columns
  [arg]
  (not (empty? @(rf/subscribe [:form-field-value (:name arg)]))))

(defmethod field-valid? :local-files
  [arg]
  (not (empty? @(rf/subscribe [:form-field-value (:name arg)]))))

(defmethod field-valid? :boolean
  [_]
  true)                                 ;booleans are always valid

;;; TODO file fields

(defmulti form-valid? :id)

(defmethod form-valid? :default 
  [{:keys [args] :as op-def}]
  (every?
   identity
   (for [arg args]
     (or (:optional? arg)
         (field-valid? arg)))))

;;; → multitool!
(defn xor
  [a b]
  (or (and a (not b))
      (and b (not a))))

;;; → multitool!
(defn oneof
  [& things]
  (= 1 (count (filter identity things))))

(defmethod form-valid? :upload-sheets 
  [{:keys [args] :as op-def}]
  ;; Note: the other fields should take care of themselves
  (let [arg (fn [n] (u/some-thing #(= (:name %) n) args))]
    (oneof
     (field-valid? (arg :gs-path))
     (field-valid? (arg :local-files)))))

(defmethod form-valid? :upload-files 
  [{:keys [args] :as op-def}]
  ;; Note: the other fields should take care of themselves
  (let [arg (fn [n] (u/some-thing #(= (:name %) n) args))]
    (oneof
     (field-valid? (arg :gs-path))
     (field-valid? (arg :local-files))
     (field-valid? (arg :local-directory)))))

(defmulti op-ui :id)

(defmethod op-ui :default
  [op-def] 
  (form op-def))

(defmethod op-ui :download-sheet
  [op-def]
  [:div
   (form op-def)
   (let [sheet-id @(rf/subscribe [:form-field-value :sheet])
         sheet @(rf/subscribe [:entity sheet-id])
         sheet-name (get sheet :sheet/name)]
     [:a.float-end
      {:href (str "/download-sheet?"
                  (aurl/params-to-str :java
                                      {:sheet sheet-id}))
       ;; TODO prob want to set content-type somehow
       ;; TODO this sometimes makes a double .tsv which is ugly
       :download (str sheet-name ".tsv")}
      "Click here to download"])   
   ])

(defmethod op-ui :delete-sheet
  [op-def]
  [:div
   (form op-def)
   (let [sheet @(rf/subscribe [:form-field-value :sheet])
         batch @(rf/subscribe [:form-field-value :batch])
         batch-ent @(rf/subscribe [:entity batch])
         sheet-ent @(rf/subscribe [:entity sheet])]
     (when sheet
       [:div
        [:hr]
        [:div.alert.alert-warning
         "Warning: this will delete sheet and all of its derived sheets:"]
        ;; TODO better modularization here
        (batch/sheet-tree sheet-ent (batch/reconstruct-provenance batch-ent))]))
   ])

;;; TODO looks like this is overkill
(defmulti op-execute (fn [op-def parameters] (:id op-def)))

(defn file-upload-data
  [element-id]
  (let [el (.getElementById js/document element-id)]
    (when el
      (let [name (.-name el)
            files (array-seq (.-files el))
            form-data (js/FormData.)]
        (when (not (empty? files))
          (doseq [file files]
            ;; Skip 0-length files, which often are nonexistant and cause errors.
            ;; TODO not sure this is right thing
            (when (> (.-size file) 0)
              (.append form-data (.-name file) file)))
          form-data)))))

(defmethod op-execute :default [op-def parameters] 
  (let [{:keys [args id]} op-def
        ;; TODO enforce that only one of these is set
        file-upload (or (file-upload-data "upload-file")
                        (file-upload-data "upload-directory"))]
    ;; TODO this is nice, but create operations go to a new page with a different spinner, looks weird. Sigh
    (rf/dispatch [:flash {:class "alert-warning" :message  [wu/spinner 2]}])
    ;; Ops will always put up a success or error message, so no need to clear the above...?

    (let [handler
          (fn [{:keys [message error html updates navigate]}]
            (cond message
                  (rf/dispatch [:flash {:class "alert-success" :message message}])
                  error
                  (rf/dispatch [:flash {:class "alert-warning" :message error}])                  )
            ;; If an HTML response, open in a new browser window
            (when html
              (.write (.-document (.open js/window)) html))
            (when updates
              (doseq [u updates]
                (rf/dispatch u)))
            (when navigate
              (rf/dispatch `[:navigate-route ~@navigate])))
          ]
      (if file-upload
        ;; This form needed for uploads
        (api/api-post "/ops/do" {:url-params (u/clean-map (assoc parameters :op id) nil?)
                                 :body file-upload
                                 :handler handler})
        ;; This form can handle larger parameters (like a 1000 rows)
        (api/api-post "/ops/do" {:params (u/clean-map (assoc parameters :op id) nil?)
                                 :handler handler})
        ))))

;;; This is weird...
(defmethod op-execute :download-sheet [op-def parameters]
  )

(rf/reg-event-db
 ::op-execute
 (fn [db _]
   (let [op (get-in db [::ops :op])]
     (rf/dispatch [:modal {:show? false}]) ;TODO I suppose doing this on completion would make more sense...
     (let [op-def (get-in db [::ops :definitions :operations op])]
       (op-execute op-def (get-in db [::ops :form])))
     (assoc-in db [::ops :form] nil))))

;;; Puts up the modal ops dialog.
(rf/reg-event-db           
 :op-dialog
 (fn [db [_ op & [parameters]]]               
   (let [{:keys [name args ok] :as op-def} (get-in db [::ops :definitions :operations op])
         defaults (merge (zipmap (map :name args) (map (partial form-default op-def) args))
                         parameters)]
     (rf/dispatch [:modal {:show? true
                           :title name
                           :contents #(op-ui op-def)
                           :ok-enabled? #(form-valid? op-def)
                           :ok-handler (when-not (= ok :none)
                                         #(rf/dispatch [::op-execute]))
                           :ok-text (or ok "Perform operation")
                           }])
     (-> db
         (assoc-in [:flash :show?] false) ;clear the flash msg 
         (assoc-in [::ops :op] op)
         (assoc-in [::ops :form] defaults)
         ))))

(rf/reg-event-db
 ::get-ops
 (fn [db _]
   (when-not (::ops db)                 ;only get once
     (api/api-get "/ops/list"
                  {:handler #(rf/dispatch [::got-ops %1])})
     db)))

(rf/reg-event-db
  ::got-ops
  (fn
    [db [_ definitions]]
    (assoc-in db [::ops :definitions] definitions)))

(rf/reg-sub
  :ops
  (fn
    [db _]
    (::ops db)))

;;; Events called by ops handlers. Right now they just redirect to other events, but could be more refined.

(rf/reg-event-fx
 :sheet-changed
 (fn [{:keys [db]} _]
   (if (:sheet db)
     {:dispatch-n [[::sheet/update-sheet]
                   ;; TODO this is kind of heavyweight.
                   ;; Would be good to have a more focused call to refresh from server
                   [:initialize-projects]]} 
     {})))

;;; These three all reduce to the same thing, but could possibly get more specific in the future
(rf/reg-event-fx
 :sheets-changed
 (fn [_ _]
   {:dispatch [:initialize-projects]}))

(rf/reg-event-fx
 :batches-changed
 (fn [_ _]
   {:dispatch [:initialize-projects]}))

(rf/reg-event-fx
 :projects-changed
 (fn [_ _]
   {:dispatch [:initialize-projects]}))

(rf/reg-event-fx
 :files-changed
 (fn [_ _]
   {:dispatch [:get-files]}))

;;; TODO Should be smarter, but this will at least update the right things I think
(rf/reg-event-fx
 :recipe-changed
 (fn [_ _]
   {:dispatch [:initialize-projects]}))

(rf/reg-event-db
 :do-pseudo-op
 (fn [db [_ op params]]
   (let [op-def (get-in db [::ops :definitions :operations op])
         params (merge (select-keys db [:project :batch :sheet]) params)]
     (op-execute op-def params)
     db)))
