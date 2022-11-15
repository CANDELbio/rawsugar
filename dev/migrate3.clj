(ns org.parkerici.rawsugar.dev.migrate3
  (:require [org.parkerici.rawsugar.datomic :as d]
            [org.parkerici.rawsugar.history :as h]
            [org.parkerici.rawsugar.batches :as batch]
            [org.parkerici.rawsugar.blob :as blob]
            [org.parkerici.rawsugar.sheet :as sheet]
            [org.parkerici.rawsugar.projects :as projects]
            [org.parkerici.multitool.core :as u]
            [clojure.set :as set]
            )
  )

;;; BBB migrate v3. Almost all of this has been integrated into batch.cljs, save the actual migration strategy.

;;; Sheet provenance recreations
;;; Fast so no longer memoized
(defn raw-provenance
  [batch]
  (u/index-by (comp :db/id first)
              (for [sheet (batch/batch-sheets batch)]
                (let [creation (h/creator-op (:db/id sheet))]
                  [sheet creation]))))


#_
(do
  (def b 17592187733012)
  
  (def bp (d/wrap-datomic-fn #(reconstruct-provenance b)))

  (oz.core/view! (spec (make-graph bp) {}) :port 1811)

  ;; pretty good
  (oz.core/view! (spec (make-graph bp) {:controls? true :node-charge 2 :link-distance 5 :node-radius 40}) :port 1812))


#_
(17592187733008 has some anomaly?
                also 17592188048577)




;;; Adds the new :object/creation-op attribute to eid, if the info is available in Datomic history.
;;; Duh, not going to work for copies! Fuck. Need to do this on production then copy over I guess...watta pain.
(defn add-creator-op
  [eid]
  (when-let [creation (h/creator-op eid)]
    [:db/add eid :object/creation-op (:db/id creation)]))

(defn augment-project
  [project]
  (u/walk-collect
   (fn [thing]
     (when (:sheet/name thing)          ;only care about sheets
       (add-creator-op (:db/id thing))))
   (d/pull (projects/project-spec true) project)))

(defn augment-projects
  []
  (doseq [p (projects/projects)]
    (prn p)
    (d/transact (augment-project (:db/id p)))))

;;; Note: for sheets, this is always (or almost always) upload-sheets, so no useful information is gotten this way...argh. I guess you want the latest sheet? There's won't be relationships in most cases...thought I had some way of reconstructing them thoug?
  
  
;; TODO For batches with a datatype, instantiate recipe

;;; Add new metadata fields to all files


(defn upgrade-project-files
  [p]
  (let [files (d/q '[:find (pull ?f [*]) ?n
                   :in $ ?p
                   :where
                   [?p :project/batches ?b]
                   [?b :batch/files ?f]
                   [?f :file/pathname ?n]
                     ]
                   p)]
    (prn (count files) (first files))))

(defn upgrade-files
  []
  (let [files (d/qfirst '[:find (pull ?f [*])
                     :where
                     [?f :file/pathname _]]
                   )
        txn
        (mapcat (fn [file]
                  (u/ignore-report
                   (let [blob (blob/blob (:file/location file))
                         id (:db/id file)]
                     (prn (:file/pathname file))
                     [[:db/add id :file/size (.getSize blob)]
                      [:db/add id :file/created (java.util.Date. (.getCreateTime blob))]])))
                 files)]
    (d/transact txn)))

