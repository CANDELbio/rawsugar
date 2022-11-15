(ns org.parkerici.rawsugar.batches-test
  (:require [clojure.set :as set]
            [clojure.test :refer :all]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.rawsugar.batches :as sut]
            [org.parkerici.rawsugar.datomic :as d]
            [org.parkerici.rawsugar.test-utils :as tu]
            [org.parkerici.rawsugar.updown :as updown])
  )

(deftest merge-batch-test
  (tu/with-project-batch project batch1
    (updown/upload-sheet-and-files project batch1 "test/resources/proj0/meta2.tsv" "test/resources/proj0/files" {})
    (tu/with-batch project batch2
      (updown/upload-sheet-and-files project batch2 "test/resources/proj2/meta.tsv" "test/resources/proj2/files" {})
      (tu/with-datomic-context
        (let [batch1-files (set (sut/batch-files batch1))
              batch2-files (set (sut/batch-files batch2))]
          (d/transact (sut/merge-txn project batch1 batch2))
          (tu/with-datomic-context
            (let [merged (d/get-entity batch1 (sut/batch-spec true))]
              (is (= batch1 (:db/id merged))) ;should merge into first batch
              (is (= #{"meta" "meta2" "meta-matched" "meta2-matched"}
                     (set (map :sheet/name (:batch/sheets merged)))))
              (is (= (set/union batch1-files batch2-files)
                     (set (sut/batch-files batch1) ))))))))))
            

      
