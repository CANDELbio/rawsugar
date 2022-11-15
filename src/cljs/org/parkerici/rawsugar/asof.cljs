(ns org.parkerici.rawsugar.asof
  (:require [org.parkerici.rawsugar.web-utils :as wu]
            [org.parkerici.multitool.core :as u]
            ))

(def as-of (atom nil))

;;; Called on initial page load
(defn initialize
  []
  (let [url (wu/browser-url)
        as-of-v (u/coerce-numeric (get-in url [:query "asof"]))]
    (reset! as-of as-of-v)
    ))

(defn clear
  []
  (reset! as-of nil)
;  (.reload (.-location js/window) false))
  ;; this forces a reload as well as clearing the param which might be hanging around
  (set! (.-search (.-location js/window)) "")
  )
 
