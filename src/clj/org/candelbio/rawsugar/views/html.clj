(ns org.candelbio.rawsugar.views.html
  (:require [clojure.string :as str]
            [environ.core :as env]
            [org.candelbio.rawsugar.login :as login]
            [org.candelbio.rawsugar.projects :as projects]
            [org.candelbio.rawsugar.cnavigate :as cnav]
            [trptcolin.versioneer.core :as version]
            )
  (:use [hiccup.core]))

;;; can't believe this isn't built into hiccup
(defn style-arg
  [m]
  (str/join (map (fn [[p v]] (format "%s: %s;" (name p) v)) m)))

(defn html-flatten
  [html]
  (if (string? html)
    html
    (str/join " " (filter string? (flatten html)))))

(defn nav-item
  [page & params]
  [:li.nav-item 
   [:a.nav-link.u {:href (apply cnav/url-for page params)}
    (name page)
    ]])

(defn old-nav-item
  [name url active?]
  [:li.nav-item {:class (when active? "active")}
   [:a.nav-link {:href url}
    name
    ]])

(defn home-link []
  [:a {:href "/"} "Rawsugar"])

(defn html-frame
  [{:keys [page project]} title contents]
  ;; should be a template I suppose but this was faster
  (html
   [:html
    [:head
     [:title (str "Rawsugar: " (html-flatten title))]
     [:meta {:charset "UTF-16"}]
     [:link {:href "https://fonts.googleapis.com/icon?family=Material+Icons"
             :rel "stylesheet"}]
     [:link {:rel "stylesheet"
             :href "https://cdn.jsdelivr.net/npm/bootstrap@5.1.1/dist/css/bootstrap.min.css" 
             :integrity "sha384-F3w7mX95PdgyTmZZMECAngseQB83DfGTowi0iMjiWaeVhAn4FJkqJByhZMI3AhiU"
             :crossorigin "anonymous"}]
     [:link {:rel "stylesheet"
             :href "/css/rawsugar2.css"}]
     [:link {:rel "stylesheet"
             :href "/css/re-com.css"}]
     [:link {:href "https://fonts.googleapis.com/css2?family=Roboto&display=swap"
             :rel"stylesheet"}]
     [:link {:href "https://fonts.googleapis.com/icon?family=Material+Icons"
             :rel "stylesheet"}]
     ]
    [:body 
     [:div.header
      [:div.header-ic]
      [:h1.titles (home-link) "/" title]
      (when-let [email (login/user)]
        [:span "Hello, " email ])
      cnav/logo
      (when-not (= page :login)
        [:nav.navbar.navbar-expand-lg.bg-dark.navbar-dark
         [:ul.navbar-nav.mr-auto
          (nav-item :home)
          ;; out-of-spa link, shoot me
          (old-nav-item "history" (str "/history" (if project (str "?project=" project) "")) (= page :history))
          
          ;; experimental out-of-spa link, shoot me
          ;; TODO match CSS
          ;; TODO nav menu for server pages
                                        ;       (old-nav-item "history" (str "/history?project=" project) false) ;TODO make this active for history
          ]])
      ]
     [:div.container.main
      contents]
     [:script {:src "https://code.jquery.com/jquery-3.5.1.slim.min.js"
               :integrity "sha384-DfXdz2htPH0lsSSs5nCTpuj/zy4C+OGpamoFVy38MVBnE+IbbVYUew+OrCXaRkfj"
               :crossorigin "anonymous"}]
     [:script {:src "https://cdn.jsdelivr.net/npm/bootstrap@5.1.1/dist/js/bootstrap.bundle.min.js"
               :integrity "sha384-/bQdsTh/da6pkI1MST/rWKFNjaCP5gBSY4sEBT38Q/9RBh9AH40zEOg7Hlq2THRZ"
               :crossorigin "anonymous"}]
     ]]))

(defn app-url []
  (format "/cljs-out/%s-main.js?v=%s" (:tier env/env) (version/get-version "rawsugar" "rawsugar")))

(defn app
  []
  [:script {:src (app-url)}])



(defn html-frame-spa
  []
  (html
   [:html
    [:head
     [:title "Rawsugar"]
     [:meta {:charset "UTF-16"}]
     [:link {:href "https://fonts.googleapis.com/icon?family=Material+Icons"
             :rel "stylesheet"}]
     [:link {:rel "stylesheet"
             :href "https://cdn.jsdelivr.net/npm/bootstrap@5.1.1/dist/css/bootstrap.min.css" 
             :integrity "sha384-F3w7mX95PdgyTmZZMECAngseQB83DfGTowi0iMjiWaeVhAn4FJkqJByhZMI3AhiU"
             :crossorigin "anonymous"}]
     [:link {:rel "stylesheet" :href "/css/rawsugar2.css"}]
     [:link {:rel "stylesheet" :href "/css/re-com.css"}]
     ;; Seems to not work with bootstrap?
     [:link {:href "https://fonts.googleapis.com/icon?family=Material+Icons"
             :rel "stylesheet"}]
     ;; argh, need to load these explictly now for some reason
     ;; TODO Either get from cdn or have to arrange a link
     ;; https://www.ag-grid.com/archive/25.0.0/documentation/javascript/themes-provided/
     [:link {:href "/css/ag-grid/ag-grid.css"
             :rel "stylesheet"}]
     [:link {:href "/css/ag-grid/ag-theme-balham.css"
             :rel "stylesheet"}]

     ]

    [:body 
     [:div#app]
     (app)
     [:script {:src "https://code.jquery.com/jquery-3.5.1.slim.min.js"
               :integrity "sha384-DfXdz2htPH0lsSSs5nCTpuj/zy4C+OGpamoFVy38MVBnE+IbbVYUew+OrCXaRkfj"
               :crossorigin "anonymous"}]
     [:script {:src "https://cdn.jsdelivr.net/npm/bootstrap@5.1.1/dist/js/bootstrap.bundle.min.js"
               :integrity "sha384-/bQdsTh/da6pkI1MST/rWKFNjaCP5gBSY4sEBT38Q/9RBh9AH40zEOg7Hlq2THRZ"
               :crossorigin "anonymous"}]
     [:script (format "window.onload = function() { org.candelbio.rawsugar.core.init('%s'); }" (login/user))]
     ]]))

;;; Things like this need to live here to avoid circular dependencies, grumble

(defn project-url
  [project]
  (str "/project/" project))

(defn project-link
  [project]
  [:a {:href (project-url project)} (:project/name (projects/get-project project))])
