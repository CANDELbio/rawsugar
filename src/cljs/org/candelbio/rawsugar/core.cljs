(ns ^:figwheel-hooks org.candelbio.rawsugar.core
  (:require
   [reagent.dom :as rdom]
   [re-frame.core :as rf]
   [org.candelbio.multitool.core :as u]
   [org.candelbio.rawsugar.asof :as asof]
   [org.candelbio.rawsugar.navigate :as nav]
   [org.candelbio.rawsugar.flash :as flash]
   [org.candelbio.rawsugar.modal :as modal]
   [org.candelbio.rawsugar.projects :as projects]
   [org.candelbio.rawsugar.sheet :as sheet]
   [org.candelbio.rawsugar.schema :as schema]
   [org.candelbio.rawsugar.batch :as batch]
   [org.candelbio.rawsugar.files :as files]
   [org.candelbio.rawsugar.cnavigate :as cnav]
   [org.candelbio.rawsugar.ops :as ops]
   org.candelbio.rawsugar.entity
   [org.candelbio.rawsugar.web-utils :as wu]
   [clojure.string :as str]
   )) 

(def debug?
  ^boolean goog.DEBUG)

(defn waiting-ui []
  [:div [:h1 "Waiting"]])

(defn invalid-ui []
  [:div.page-content
   (if @(rf/subscribe [:update-in-progress?])
     (wu/spinner)
     [:h1 "Page does not exist"])])

;;; Note: see cnavigate.cljc when adding new page
(defn page
  [page]
  (if (= :sheet page)
    [:div.container-fluid.main  ;make a wide page for sheet
     [:div.margins
      [:div.white
       [sheet/ui]]]]
    [:div.container.main                ;others get the standard narrow container
     (case page
       :home      [projects/home-ui]
       :project   [projects/ui]
       :batch     [batch/ui]
       :files     [files/ui false]
       :invalid   [invalid-ui]
       nil        [waiting-ui]
       )]))

(defn titles
  "Returns a sequence of elements to make up the page title"
  [page-name]
  (let [project @(rf/subscribe [:project true])]
    (interpose
     "/"
     (cons (nav/home-link)
           (case page-name
             :home      []
             :project   [(nav/project-link project)]
             :batch     [(nav/project-link project) 
                         (nav/batch-link project @(rf/subscribe [:batch true]))]
             :files     [(nav/project-link project) 
                         (nav/batch-link project @(rf/subscribe [:batch true]))
                         "files"] ;Files are per batch
             :sheet     [(nav/project-link project) 
                         (nav/batch-link project @(rf/subscribe [:batch true]))
                         (nav/sheet-link project @(rf/subscribe [:batch true]) @(rf/subscribe [:sheet true]))]
             :invalid   [])))))

(defn title
  "Convert sequence above back into string for <title>"
  [titles]
  (str/join " " (map #(if (string? %) % (nth % 2)) titles)))

(defn nav-item
  [page label]
  [:li.nav-item {:class (when (= @(rf/subscribe [:page]) page) "active")}
   [:a.nav-link.top-nav-link {:href (nav/url-for page)}
    label
    ]])

;;; Damn this was hard to get (semi-)working
(defn non-spa-nav
  [url]
  {:href "#" :on-click #(do
                          (.preventDefault %)
                          (rf/dispatch [:non-spa-navigate url]))})

(defn old-nav-item
  [name url active?]
  [:li.nav-item {:class (when active? "active")}
   [:a.nav-link.top-nav-link (non-spa-nav url)
    name
    ]])

(defn doc-item
  []
  [:li.nav-item.dropdown
   [:a.nav-link.top-nav-link.dropdown-toggle {:id "navbarHelp" :href "#" :role "button" :data-bs-toggle "dropdown" :data-toggle "dropdown"
                                 :aria-haspopup true :aria-expanded false} "Help"]
   [:div.dropdown-menu {:aria-labelledby "navbarHelp"}
    ;; TODO opensource these will need to change probably
    [:a.dropdown-item (assoc (non-spa-nav "/user-guide") :target "_guide")
     "User Guide"]
    [:a.dropdown-item {:href "https://dataplatform.candelbio.org" :target "_guide"}
     "Data Platform"]
    [:a.dropdown-item {:href "https://github.com/Candelbio/rawsugar/releases/latest" :target "_github"}
     "Download CLI"]
    [:a.dropdown-item {:href "https://github.com/Candelbio/rawsugar/issues/new" :target "_github"}
     "Report an issue"]
    [:a.dropdown-item {:href "https://github.com/Candelbio/rawsugar" :target "_guide"}
     "Github"]
    ]])

;;; This annoyingly has to get copied to html.clj for server-based pages. Probably could extract a .cljc version.
(defn header
  []
  (let [project @(rf/subscribe [:project])
        page @(rf/subscribe [:page])
        titles (titles page)]
    (set! (.-title js/document) (title titles))
    [:div.header
     [:div.header-ic]
     (when @asof/as-of
       [:div.asof
        [:h2 "As of version " (str @asof/as-of)]
        [:a {:href "#" :on-click asof/clear} "Back to the present"] ;TODO
        ])
     [:h1.titles (wu/keyify titles "title")]
     (when-let [login @(rf/subscribe [:login])]
       [:span.welcome "Welcome " login])
     cnav/logo
     [:nav.navbar.navbar-expand-lg.bg-dark.navbar-dark
      [:ul.navbar-nav.mr-auto
       (nav-item :home "Projects")
       ;; out-of-spa link, shoot me
       (old-nav-item "history" (str "/history" (if project (str "?project=" project) "")) false) ;TODO make this active for history
       (ops/ops-dropdown)
       (doc-item)
       ]]]))

(defn app-ui
  []
  (when @(rf/subscribe [:projects])                 ;don't do anything until project data is loaded
    [:div
     [modal/modal]
     [header]
     [flash/flash]
     [page @(rf/subscribe [:page])]
;;;     [footer]
     ]))

(defn mount-root
  []
  (rf/clear-subscription-cache!)
  (rdom/render [app-ui]
    (.getElementById js/document "app")))

(rf/reg-event-db
 :login
 (fn [db [_ user]]
   (assoc db :user user)))

(rf/reg-sub
 :login
 (fn [db _]
   (:user db)))

(defn ^:export init
  [user]
  (asof/initialize)
  (rf/dispatch [:login user])
  (nav/start!)
  (rf/dispatch [::ops/get-ops]) ;TODO elsewhere?
  (rf/dispatch [:initialize-projects])
  (rf/dispatch [::schema/get-schema])
  (mount-root))


(rf/reg-event-fx
 :non-spa-navigate
 (fn [_ [_ url]]
   (.assign (.-location js/window) url)
   {}))

;;; The batch page insists on rendering scrolled; this fixes it. There's probably a better way...
(rf/reg-event-fx
 :fix-scroll
 (fn [_ _]
   (.scrollTo js/window 0 0 )))

(rf/reg-event-fx
 :set-active-page
 (fn
   [{:keys [db]} [_ {:keys [handler route-params] :as route}]] ;; destructure 2nd parameter to obtain keys
   (cond (empty? route)
         {:dispatch [:error "Invalid route"]
          :db       (assoc db :page :invalid)}
         (and (= handler (:page db))
              (= route-params (:page-params db)))
         {}       ;don't do anything if we are already on page
         :else
         (let [project (u/coerce-numeric (:project route-params))
               batch (u/coerce-numeric (:batch route-params))
               sheet (u/coerce-numeric (:sheet route-params))]
           ;; clear the flash on navigate.
           ;; this isn't a good thing anymore, ops responses appear then get cleared
           #_ (rf/dispatch [:flash {:show? false}])
           (try 
             (let [set-page (assoc db
                                   :page handler
                                   :project project
                                   :batch batch
                                   :sheet sheet
                                   :page-params route-params
                                   :page-state {} ;Store things here that should be cleared on page navigation, like widget state
                                   )]
               (case handler
                 :home {:db        set-page
                        :dispatch  [:initialize-projects]}

                 :project {:db     set-page}
                 :batch {:db       set-page
                         :dispatch-n [[:fix-scroll] [:get-files]]}
                 :files {:db       set-page
                         :dispatch [:get-files]}
                 :sheet {:db       set-page
                         :dispatch [::sheet/initialize]}
                 ))
             ;; Not sure this try/catch is needed or useful
             (catch :default e
               {:dispatch [:error (str e)]
                :db       (assoc db :page :invalid)
                }))))))

(rf/reg-sub
 :page-state
 (fn [db _] (get db :page-state)))

(defn- valid-route-params?
  "This examimes project, batch, sheet and makes sure they are valid."
  [{:keys [project batch sheet index]}]
  (and (or (not project)
           (get index project))
       (or (not batch)
           (get index batch))
       (or (not sheet)
           (get index sheet))))

(rf/reg-sub
 :page
 (fn [db _]
   (if (and (not (empty? (:index db)))  ;wait for projects to be loaded
            (valid-route-params? db))   ;ensure params are valid
     (:page db)
     :invalid)))

;;; Enable figwheel hot reload
(defn ^:after-load re-render []
  (mount-root))

