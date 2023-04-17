(ns org.candelbio.rawsugar.flash
  (:require
   [re-frame.core :as rf]))

;;; Flash message component (the colored temporary banner, displayed in response to an action)

(rf/reg-event-db
 :flash
 (fn [db [_ data]]
   (assoc-in db [:flash] (merge {:show? true} data))))

(rf/reg-event-fx
 :error
 (fn [_ [_ message]]
   {:dispatch [:flash (if message
                        {:class "alert-danger" :message message}
                        {:show? false})]}))


(rf/reg-sub
 :flash
 (fn [db _] (:flash db)))

(defn flash
  "Render the flash message if any."
  []
  (let [{:keys [show? message class]} @(rf/subscribe [:flash])]
    (when show?
      [:div.alert {:class class :style {:margin "10px"}}
       [:button.close.float-end
        {:type "button" :title "Close"
         :on-click #(rf/dispatch [:flash {:show? false}])}
        [:i {:class "material-icons"} "close"]]
       (if (some #(= % \newline) message)
         [:pre message]
         message)])))


 
