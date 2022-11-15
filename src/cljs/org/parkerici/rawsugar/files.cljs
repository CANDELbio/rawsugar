(ns org.parkerici.rawsugar.files
  (:require [org.parkerici.rawsugar.aggrid :as ag]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.rawsugar.api :as api]
            [org.parkerici.rawsugar.web-utils :as wu]
            [re-frame.core :as rf]
            [reagent.dom.server :as dom]
            [clojure.string :as str]
            ))

;;; Due to the way things are set up now, this has to be the same for files page and sheets. Could fix that with some work
(defmethod ag/ag-col-def :file/pathname [_col-id _col-def]
  {:headerName "Files"
   :cellRenderer
   (fn [data]
     (let [file (js->clj (.-data data) :keywordize-keys true)
           row (:_files file)
           highlighter identity] ;TODO
       (dom/render-to-string
        (ag/render-file file highlighter row false))))
;;; Don't want this for file view, not sure about sheets
;   :flex 1
;   :width 450
   :comparator (fn [_ _ na nb invert?]
                 (let [metric (fn [row-node] (count (.-_files (.-data row-node))))]
                   (compare (metric na) (metric nb))))
   :filterValueGetter
   (fn [params]
     (when (.-data params)
       (let [file (js->clj (.-data params) :keywordize-keys true)]
         (:pathname file))))
   })

(defmethod ag/ag-col-def :sheets [col-id col-def]
  {:headerName "Sheets"
   :valueGetter (fn [args]
                  (let [data (.-data args)
                        sheets (.-sheets data)]
                    (str/join ", " (map #(.-name %)
                                        ;; Hack so these are sorted by most recent first (assuming :db/ids are increasing, which they are)
                                        (reverse (sort-by :id sheets))))))
   }
  )

(defmethod ag/ag-col-def :file/created [col-id col-def]
  {:headerName "Created"
   :field "created"
   :valueFormatter #(wu/format-time (.-value %) wu/short-time-format)
   })

(def file-columns [:file/pathname :sheets :file/size :file/created :extension :file/location])
(def file-columns-matching [:file/pathname :file/size :file/created :extension :sheets])

;;; Note: This is the same view for Files page and matching rhs, slightly tweaked
;;; by matching? parameter. 
(defn ui
  [matching?]
  (rf/dispatch [::maybe-get-files])
  (ag/ag-table :files
               (if matching? file-columns-matching file-columns)
               {:created {:formatter #(wu/format-time % wu/short-time-format)}}
               @(rf/subscribe [::files])
               {:rowSelection (if @(rf/subscribe [:matching?])
                                "single" "multiple")
                :getRowClass            ;color the row appropriately
                (fn [params]
                  (let [data (.-data params)
                        sheets (and data (.-sheets data))
                        row-id (.-id data)
                        pending-match-files (and matching?
                                                (->> @(rf/subscribe [:page-state])
                                                    :to-match
                                                    (map second)))
                        ]
                    (cond (and pending-match-files (some #(= row-id %) pending-match-files))
                          "match-pending"
                          (empty? sheets)
                          "unmatched"
                          :else "matched1")))
                }))

;;; TODO not sure if this is good react
(rf/reg-event-fx
 ::maybe-get-files
 (fn [{:keys [db]} _]
   ;; :batch is global and means the batch the user is currently looking at; ::batch is local to
   ;; this namespace and means, batch we have current file info for. This event handler updates the
   ;; latter when necessary.
   (if (= (:batch db) (::batch db))     
     {}
     {:dispatch [:get-files]})))

;;; Has to be non-namespaced for ops 
(rf/reg-event-db
 :get-files
 (fn [db _]
   (api/api-get "/file/list"
                {:params {:project (:project db) :batch (:batch db)}
                 :handler #(rf/dispatch [::got-files %1]) 
                 })
   (assoc db
          ::batch (:batch db)
          ::files nil
          )))

(rf/reg-event-db
 ::got-files
 (fn
  [db [_ files]]
   (assoc db ::files files)))

;;; subs
(rf/reg-sub
 ::files
 (fn [db _]
   (::files db)))
          
(defn update-files
  [files new-files]
  (vals
   (merge (u/index-by :db/id files)
          (u/index-by :db/id new-files))))

(rf/reg-event-db
 :file-update
 (fn
  [db [_ new-files]]
  (assoc db
         ::files
         (update-files (::files db) new-files))))




