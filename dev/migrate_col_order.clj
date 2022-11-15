(ns org.parkerici.rawsugar.dev.migrate-col-order
  (:require 
   [me.raynes.fs :as fs]
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [org.parkerici.multitool.core :as mt]
   [org.parkerici.rawsugar.blob :as blob]
   [org.parkerici.rawsugar.updown :as updown]
   [org.parkerici.rawsugar.datomic :as d]))

(def existing (d/w #(d/q '{:find [(pull ?sheet [* {:sheet/columns [*]}])
                                  (pull ?file [*])]
                           :where [[?sheet :sheet/file ?file]]})))

;;; Note: to run this, make updown/file-sheets public

(defn convert-one
  [[sheet file]]
  (let [file-contents (mt/ignore-report  ;There are some files with unknown extension, just skip
                       (updown/file-sheets file))]
    (cond (> (count file-contents) 1)
          (prn "more than one sheet for file" file)
          :else
          (let [file-data (first (vals file-contents))
                columns (:columns file-data)]
            (map (fn [col]
                   (if-let [pos  (mt/position= (:column/name col) columns)]
                     [:db/add (:db/id col) :column/index pos]
                     (do (prn :no-pos col columns)
                         nil)))

                 (:sheet/columns sheet))))))


;;; Pretty slow since it reads a lot of sheet files
(def txn (filter identity (mapcat convert-one existing)))

(defn exp-col
  [col]
  (d/w #(d/q '{:find [?pn ?bn ?sn (pull ?col [*])]
              :in [$ ?col]
              :where [[?s :sheet/columns ?col]
                      [?b :batch/sheets ?s]
                      [?p :project/batches ?b]
                      [?p :project/name ?pn]
                      [?b :batch/name ?bn]
                      [?s :sheet/name ?sn]]
              }
             col)))
