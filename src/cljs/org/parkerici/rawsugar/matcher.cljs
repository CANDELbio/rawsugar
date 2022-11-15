(ns org.parkerici.rawsugar.matcher
  (:require [re-frame.core :as rf]
            [org.parkerici.rawsugar.web-utils :as wu]
            [org.parkerici.rawsugar.aggrid :as ag]
            ))

;;; TODO would be nice if this did something when pressed like a real button
(defn link-button
  []
  (let [active? 
        (and (not (empty? @(rf/subscribe [::ag/selection :sheet])))
             (not (empty? @(rf/subscribe [::ag/selection :files]))))]
    (wu/icon "link" "link row with file"
             (fn [e] (rf/dispatch [::link]))
             :class "md-48 md-dark"
             :disabled? (not active?))
    ))

;;; This feels vaguely illegal.
(rf/reg-event-db
 :funcall
 (fn [db [_ f]]
   (f)
   db))

;;; TODO rob: Why not reg-event-fx with a dispatch-n?
(rf/reg-event-db
 ::link
 (fn
  [db _]
   ;; Single selection is enforced, so there should be only one row and file
  (let [row (first @(rf/subscribe [::ag/selection :sheet]))
        file (first @(rf/subscribe [::ag/selection :files]))]
    (rf/dispatch [::ag/clear-selection :sheet])
    (rf/dispatch [::ag/clear-selection :files])
    ;; This has to happen after db changes, this seems to work. My only defense: Less ugly than previous solution 
    (rf/dispatch [:funcall #(do (ag/redraw :sheet)
                                (ag/redraw :files) )])
    (-> db
        (update-in [:page-state :to-match] conj [row file])
        ))))





