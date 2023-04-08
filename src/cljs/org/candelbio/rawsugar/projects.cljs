(ns org.candelbio.rawsugar.projects
  (:require [re-frame.core :as rf]
            [org.candelbio.rawsugar.batch :as batch]
            [org.candelbio.rawsugar.web-utils :as wu]
            [org.candelbio.rawsugar.utils :as utils]
            [org.candelbio.rawsugar.api :as api]
            [org.candelbio.rawsugar.navigate :as nav]
            [org.candelbio.rawsugar.entity :as entity]
            [inflections.core :as inflect]
            [clojure.string :as str]
            ))

(rf/reg-event-db
 :initialize-projects
 (fn
   [db _]
   (api/api-get "/projects/data"
                {:params        {}
                 :handler       #(rf/dispatch [::projects-ready %1])})
   (assoc db :update-in-progress? true)))

(rf/reg-event-db
 ::projects-ready
 (fn [db [_ data]]
   (assoc db
          :index (entity/build-index data)
          :update-in-progress? false)))

(rf/reg-sub
 :update-in-progress?
 (fn
  [db _]
   (:update-in-progress? db)))

(rf/reg-sub
 :projects
 (fn
  [db _]
   (filter :project/name (vals (:index db)))))

(defn metadata-ui
  [project]
  (when-let [op (:project/last-op project)]
    [:span.proj-metadata
     (str
      "   Last edited on "
      (wu/format-time (:operation/time op))
      " by "
      (utils/trim-login (:operation/agent op)))]))


(defn project-entry
  [project]
  (let [batches (sort-by (comp str/lower-case :batch/name)
                         (entity/refresh (:project/batches project)))
        schema @(rf/subscribe [:schema])
        datatypes (get-in schema [:enums :datatype :values])
        statuses (get-in schema [:enums :batch-status :values])]
    [:ul
     (doall 
      (for [batch batches]    
        ^{:key (:db/id batch)}
        [:li 
         [:span {:style {:color "black"}}
          (nav/batch-link project batch)
          (when (:batch/datatype batch)
            [:i " " (get datatypes (:batch/datatype batch))]) 
          [:span " " (inflect/pluralize (count (:batch/sheets batch)) "sheet")]
          (when (:batch/file-count batch)
            [:span ", " (inflect/pluralize (:batch/file-count batch) "file")])
          (when (:batch/status batch)
            [:i ", " (get statuses (:batch/status batch))])
          ;; Sheets
          [:ul
           (for [sheet (sort-by (comp str/lower-case :sheet/name)
                                (entity/refresh (:batch/sheets batch)))]
             ^{:key (:db/id sheet)}
             [:li (nav/sheet-link project batch sheet)])]]]))]))

;;; TODO realized it would make more sense to do all this sorting
;;; on load of the census, then order would be right in select widgets and elsewhere

(rf/reg-sub
 :expanded-projects
 (fn [db _]
   (:expanded-projects db)))

(rf/reg-event-db
 :project-toggle
 (fn [db [_ project]]
   (update-in db [:expanded-projects project] not)))

(defn home-ui
  []
  (let [projects (sort-by (comp str/lower-case :project/name)
                          @(rf/subscribe [:projects]))
        expanded-projects @(rf/subscribe [:expanded-projects])]
    (if (empty? projects)
      [:div "No projects!"]
      [:div.black
       (doall
        (for [project projects]
          (let [project-id (:db/id project)
                expanded? (get expanded-projects project-id)]
            ^{:key project-id}
            [:div.project.card 
             [:div.card-header.white    ;TODO maybe flush bottom border
              {:on-click #(rf/dispatch [:project-toggle project-id])} ;make the whole header clickable, not just the icon
              (nav/project-link project)
              (metadata-ui project)
              (wu/icon (if expanded? "expand_less" "expand_more")
                       (if expanded? "Contract" "Expand")
                       #()              ;moved to header
                       :class "orangef")]
             [:div.collapse {:aria-labelledby "compacted-head"
                             :class (if expanded? "show" nil)
                             :id project-id}
              (project-entry project)]])))])))

(rf/reg-sub
 :project
 (fn [db [_ full?]]                     ;experimental, maybe extend this to sheet/batch etc
   (if full?
     (entity/refresh @(rf/subscribe [:entity (get db :project)]))
     (get db :project))))

(rf/reg-sub
 :batch
 (fn [db [_ full?]]
   (if full?
     (entity/refresh @(rf/subscribe [:entity (get db :batch)]))
     (:batch db))))

(rf/reg-sub
 :sheet
 (fn [db [_ full?]]
   (if full?
     (entity/refresh @(rf/subscribe [:entity (get db :sheet)]))
     (get db :sheet))))


;;; Single project page

(defn dashboards
  [project]
  [:div 
   (for [batch (sort-by :batch/name (:project/batches project))]
     (when (:batch/dashboard batch)
       [batch/dashboard-ui (:db/id batch) [:h4.dashitem (:batch/name batch)]]))])

(defn ui
  []
  (let [project @(rf/subscribe [:project true])]
    [:div.page-content
     [:h1 [:span.blackf "Project "]
      [wu/editable-text-widget
       :name
       {:initial-value (:project/name project)
        ;; TODO should go through :rename-project op via ::op-execute
        :change-event [::set-project-property :project/name]
        :borderless? true}]]
     [:h3.pt-3 "Properties"]
     [:table.optable.table.responsive
      [:tbody
       [wu/form-row "description "
        [wu/editable-textarea-widget
         :description
         (:project/description project)
         [::set-project-property :project/description]]]

       [wu/form-row "dashboard"
        [wu/editable-url-widget-form
         :dashboard
         {:link-text "Dashboard"
          :initial-value (:project/dashboard project)
          :change-event [::set-project-property :project/dashboard]}]]
       ]]
     [:h3.pt-3 "Batches"]
     (project-entry project)
     [:h3.pt-3 "Dashboards"]
     [dashboards project]
     ]))

(rf/reg-event-fx
 ::set-project-property
 (fn [{:keys [db]} [_ property value]]
   (let [project-id (:project db)
         changes {:db/id project-id property value}]
     {:dispatch [:update-entity changes]})))
