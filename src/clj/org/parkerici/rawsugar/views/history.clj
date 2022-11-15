(ns org.parkerici.rawsugar.views.history
  (:require [org.parkerici.rawsugar.history :as h]
            [org.parkerici.rawsugar.views.html :as html]
            [org.parkerici.multitool.core :as u]
            ))

(def limit 300)

(defn as-of-url [op]
  (str "/?asof=" (:operation/transaction op))) ;idea to change this to :db/id to work in terms of operations instead of txns

;;; TODO probably want to hide txn column
;;; TODO links to view and revert actions, and project when that is displayed
(defn history-view
  [project]
  (let [project (u/coerce-numeric project)
        history (h/history :project project)
        columns (h/columns (not project))]
    (html/html-frame
     {:project project :page :history}
     [:span
      (when project [:span (html/project-link project) "/"])
      [:span.whitef "history"]]
     [:div.page-content
      [:table.table
       [:thead
        [:tr
         [:th {:scope "col"}]
         (for [[col-name _ col-class] columns]
           [:th {:class col-class :scope "col"} col-name])]]
       [:tbody
        (for [op (take limit history)]
          [:tr
           [:td.nobr [:a {:href (as-of-url op)} "As of"]]
           (for [[_ col-val-f col-class] columns]
             [:td {:class col-class} (col-val-f op)]
             )])]
       [:tfoot
        (when (> (count history) limit)
          [:tr [:td (format "Truncated to %s rows" limit)]])]
       ]])))
      
     


