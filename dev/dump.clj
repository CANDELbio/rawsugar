(ns org.parkerici.rawsugar.dev.dump
  (:require [org.parkerici.rawsugar.datomic :as d]
            [org.parkerici.rawsugar.history :as h]
            [org.parkerici.rawsugar.projects :as projects]
            [me.raynes.fs :as fs]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.multitool.cljcore :as ju]
            [clojure.string :as s]
            [clojure.walk :as walk]
            ))

;;; Dump and restore projects.
;;; Somewhat schema agnostic so could be generalized for multitool

(defn pull-project
  [p]
  (d/pull (projects/project-spec true) p))

;;; → multitool (identical to walk-collect except for initial value)
(defn walk-collect-set
  "Walk f over thing and return a list of unique the non-nil returned values"
  [f thing]
  (persistent!
   (u/walk-reduce (fn [acc elt]
                 (if-let [it (f elt)]
                   (conj! acc it)
                   acc))
               thing
               (transient #{}))))

;;; Different than walk-collect  in multitool, better I think
(defn walk-collecting
  "f takes a collector and a substructure. Init can be vector or set"
  [f thing init]
  (let [acc (transient init)
        collector (fn [x] (conj! acc x))
        ff (partial f collector)]
    (u/side-walk ff thing)
    (persistent! acc)))

;;; Fucking idiot, I already wrote this code, in migrate3/add-creator-op...I really must be getting senile.

(defn enhance-with-creator
  [thing]
  (assoc thing :object/creation-op (h/creator-op (:db/id thing))))

(defn enhance-project
  [p]
  (walk/postwalk
   (fn [thing]
     (if (:sheet/name thing)
       (enhance-with-creator thing)
       thing))
   p))

;;; Seems like this should exist?
;;; TODO :batch/datatype has to dump as keyword not id. Others (:batch/status)
(defn dump-project-datoms
  [p]
  (into []
        (walk-collecting
         (fn [collect thing]
           (when (map? thing)
             (let [this-id (:db/id thing)]
               (doseq [[att val] thing]
                 (cond
                   ;; Don't want to dump these
                   (= att :db/id) nil
                   (= att :db/ident) nil
                   
                   ;; Idents need to be dumped as keywords
                   (:db/ident val)
                   (collect [this-id att (:db/ident val)])

                   ;; Change the name. TODO Only want to do this sometimes. and logically should be done on dwnp.
                   ;; Note that because :project/name is marked as a unique id, if you DON'T do this or something similar, you will link back to original entity instead of making a copy!!!!!
                   (= att :project/name) (collect [this-id att (str val " copy")])
                   
                   ;; Don't dump inverse relations
                   (= \_ (first (name att))) nil

                   ;; Map values assumed to be entities; pull the id out
                   (map? val)
                   (collect [this-id att (or (:db/id val) (:db/ident val))])
                   
                   ;; Vector of values, needs to be re-spread to datoms (assuming no actual vector values)
                   (vector? val)          
                   (doseq [elt val]
                     (collect [this-id att (or (:db/id elt) elt)]))
                   :else
                   ;; Just a plain old assertion
                   (collect [this-id att val]))))))
         ;; Note: probably don't want to do the enhance, it will screw up re-import
         (enhance-project (pull-project p))
         #{})))
                   
(defn dump-project
  [p out]
  (let [datoms (dump-project-datoms p)]
    (ju/schppit out datoms)))

(defn dump-projects
  []
  (doseq [p (projects/projects)]
    (dump-project (:db/id p) (str (:project/name p) ".edn"))))
        
;;; → Multitool, maybe
(defn multi-replace [string pairs]
  (reduce (fn [s [re replacement]] (s/replace s re (str replacement)))
          string
          pairs))

(defn dwnp-project
  [in]
  (let [raw (ju/read-from-file in)
        entities (set (map first raw))
        txn-res
        (-> raw 
            (u/subst-gen {} #(if (contains? entities %) (str "v" %) %))
            ((u/invert mapv) #(vec (cons :db/add %)))
            d/transact
            )
        ;; PKM
        tempids (:tempids txn-res)
        substs (map (fn [[var val]] [(re-pattern (subs var 1)) val]) tempids)
        ;; TODO same for :batch/dashboard
        param-datoms (filter #(= :operation/parameters (second %)) raw)
        fix-params-txn (map (fn [[s v o]]
                              [:db/add (get tempids (str "v" s)) v (multi-replace o substs)])
                            param-datoms)]
    (d/w
     #(d/transact fix-params-txn))))


(defn dwnp-projects
  [dir]
  (doseq [file (fs/list-dir "from-prod/")]
    (prn :z file)
    (d/w #(dwnp-project file))))


;;; From production
; 17592191471232 Bruce
; 17592188636437 PICI0009
; 17592191471232 pici0001

;;; Some of these are too big, this trims out the sheet contents
(defn trim-project
  [in]
  (let [raw (ju/read-from-file in)
        filtered
        (remove #(contains? #{:cell/value_string
                              :cell/column
                              :cell/value_float
                              :cell/value_long
                              :cell/value_instant
                              :row/cells
                              } (second %))
                raw)
        out (str in ".filtered")]
    (ju/schppit out filtered)))
