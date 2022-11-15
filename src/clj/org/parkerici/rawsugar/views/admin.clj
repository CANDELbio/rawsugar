(ns org.parkerici.rawsugar.views.admin
  (:require [environ.core :as env]
            [trptcolin.versioneer.core :as version]
            [org.parkerici.rawsugar.config :as config]
            [org.parkerici.rawsugar.server-utils :as su]
            [org.parkerici.rawsugar.views.html :as html]
            [hiccup.util :as hu]
            )
  )

;;; TODO git commit information, etc

(defn map-table
  [name map]
  [:div [:h2 name]
   [:table.table-bordered
    (for [key (sort (keys map))]
      [:tr
       [:th (str key)]
       [:td (if (map? (get map key))
              (map-table "" (get map key))
              (hu/escape-html (str (get map key))))]])]])

;;; Git is not installed on the app server machines, so turning this off for now
#_
(defn git-info []
  {:commit (:out (sh/sh "git" "log" "-1" "--format=short"))
   :branch (:out (sh/sh "git" "rev-parse" "--abbrev-ref" "HEAD"))})

(defn git-info [] {})

(defn view
  [req]
  (html/html-frame
   {}
   "Admin"
   [:div
    (map-table "Version" (merge {:rawsugar-version (version/get-version "rawsugar" "rawsugar")}
                                (git-info)))
    (map-table "Server" {:started (su/start-time)})
    (map-table "Config" (config/config))
    (map-table "Env" env/env)
    (map-table "System/getenv" (System/getenv))
    (map-table "HTTP req" req)
    ]))
