(ns org.parkerici.rawsugar.schema
  (:require [re-frame.core :as rf]
            [org.parkerici.rawsugar.api :as api]
            ))

(rf/reg-event-db
 ::get-schema
 (fn [db _]
   (when-not (::schema db)              ;only get once
     (api/api-get "/schema"
                  {:handler #(rf/dispatch [::got-schema %1])})
     db)))

(rf/reg-event-db
  ::got-schema ;; dispatched when schema is available
  (fn [db [_ schema]]
    (assoc db ::schema schema)))

(rf/reg-sub
  :schema
  (fn [db _]
    (::schema db)))

