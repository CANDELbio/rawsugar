(ns org.candelbio.rawsugar.views.user-guide
  (:require [org.candelbio.rawsugar.views.html :as html]
            ))

;;; IFrame is easiest way to get org output + Rawsugar styling. Alt would be
;;; to use the body-only arg to org-html-export-to-html and slurp the result.
(defn user-guide
  []
  (html/html-frame
   {:page :user-guide}
   "Rawsugar User Guide"
   [:iframe {:src "doc/user-guide.html"
             :scrolling "no"
             :style (html/style-arg
                     {:width "100%"
                      :height "1400%"})}] ;TODO couldn't figure out CSS to do the right thing
   ))
