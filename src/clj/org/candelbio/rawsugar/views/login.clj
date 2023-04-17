(ns org.candelbio.rawsugar.views.login
  (:require [org.candelbio.rawsugar.views.html :as html]
            )
  )

(defn login-view
  []
  (html/html-frame
   {:page :login}
   "Login"
   [:div.black
    [:div.login-panel.p-4
     [:table
      [:tr
       [:td
        [:h4 "Welcome to RawSugar, a raw data handling tool for data-intensive biology"]]
       [:td
        [:div {:style (html/style-arg {:margin-left "60px"}) }
         [:a {:href "/oauth2/google"}
          [:img {:src "/img/google-signin.png"}]]]]]]]]))
