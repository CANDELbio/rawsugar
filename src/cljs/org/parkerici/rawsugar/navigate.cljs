(ns org.parkerici.rawsugar.navigate
  (:require [bidi.bidi :as bidi]
            [pushy.core :as pushy]
            [re-frame.core :as rf]
            [org.parkerici.rawsugar.cnavigate :as c]
            [org.parkerici.multitool.core :as u]
            [clojure.string :as str]
            [cemerick.url :as url]
            ))

(defn- parse-url
  [url]
  ;; Do URL decode on components
  (u/map-values #(if (string? %)
                   (url/url-decode %)
                   %)
                (bidi/match-route c/routes url)))

(defn url-for
  [page & args]
  (apply c/url-for page (map #(if (map? %) (:db/id %) %) args)))

(defn- dispatch-route
  [matched-route]
  (rf/dispatch [:set-active-page matched-route]))

(def non-naviable-urls #{"/download-sheet" "/file"})

;;; Copy of native version from pushy, but will skip non-naviable-urls.
(defn- processable-url?
  [uri]
  (and (not (str/blank? uri)) ;; Blank URLs are not processable.
       (or (and (not (.hasScheme uri)) (not (.hasDomain uri))) ;; By default only process relative URLs + URLs matching window's origin
           (some? (re-matches (re-pattern (str "^" (.-origin js/location) ".*$"))
                              (str uri))))
       (not (get non-naviable-urls (.getPath uri)))
       ))


(defn start!
  []
  ;; pushy is here to take care of nice looking urls. Normally we would have to
  ;; deal with #. By using pushy we can have '/about' instead of '/#/about'.
  ;; pushy takes three arguments:
  ;; dispatch-fn - which dispatches when a match is found
  ;; match-fn - which checks if a route exist
  ;; identity-fn (optional) - extract the route from value returned by match-fn
  (pushy/start! (pushy/pushy dispatch-route
                             parse-url
                             :processable-url? processable-url?)))

(def history (pushy/pushy dispatch-route (partial bidi/match-route c/routes)))

;;; way harder than it should be. I hate bidi.

(defn navigate-to-url [url]
  (let [real-route (parse-url url)]
    (dispatch-route real-route)
    (pushy/set-token! history url)))

(defn navigate-to-route [route]
  (navigate-to-url (apply url-for route)))

(rf/reg-event-fx
 :navigate-url
 (fn [_ [_ url]]
   (navigate-to-url url)
   {}))

(rf/reg-event-fx
 :navigate-route
 (fn [_ [_ & route]]
   (navigate-to-route route)
   {}))

;;; TODO make these consistent. Probably -link fns should take entities, 

;;; Convention: -link fns take entities, -url fns take ids or entities

(defn project-url
  [project]
  (url-for :project :project project))

(defn batch-url
  [project batch]
  (url-for :batch :project project :batch batch))

(defn files-url
  [project batch]
  (url-for :files :project project :batch batch))

(defn sheet-url
  [project batch sheet]
  (url-for :sheet :project project :batch batch :sheet sheet))

(defn link
  [url label]
  [:a {:href url}
   label])

(defn home-link
  []
  (link (url-for :home) "Rawsugar"))

(defn project-link
  [project-ent]
  (link (project-url project-ent) (:project/name project-ent)))

(defn batch-link
  [project-ent batch-ent]
  (link (batch-url (:db/id project-ent) (:db/id batch-ent))
        (:batch/name batch-ent)))

(defn sheet-link
  [project-ent batch-ent sheet-ent]
  (link (sheet-url project-ent batch-ent sheet-ent)
        (:sheet/name sheet-ent)))





