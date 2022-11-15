(ns org.parkerici.rawsugar.projects-test
  (:require [clojure.test :refer :all]
            [org.parkerici.rawsugar.projects :refer :all]
            [org.parkerici.rawsugar.test-utils :as tu]
            [org.parkerici.rawsugar.updown :as updown]
            [org.parkerici.multitool.core :as u])
  (:import clojure.lang.ExceptionInfo))

(deftest create-and-delete-test
  (tu/with-datomic-context
    (let [project-list (map :project/name (projects))]
      (tu/with-project-name new-project
        (add new-project)
        (tu/with-datomic-context
          (is (= (set (map :project/name (projects))) (set (cons new-project project-list))))
          (testing "delete"
            (delete (lookup new-project))
            (tu/with-datomic-context
              (is (= (set (map :project/name (projects))) (set project-list))))))))))

(deftest duplicate-name-check-tests
  (tu/with-datomic-context
    (tu/with-project-name p
      (add p)
      (tu/with-datomic-context
        (is (thrown-with-msg?
             ExceptionInfo #"already exists"
             (add p)))))))

(deftest delete-name-check-test
  (tu/with-datomic-context
    (tu/with-project-name p
      (is (thrown-with-msg?
           ExceptionInfo #"does not exist"
           (delete (lookup p)))))))
    
(deftest project-data-test
  (tu/with-project-batch project batch
    (updown/upload-sheets project batch "test/resources/proj0/meta.tsv")
    (tu/with-datomic-context
      (let [census (project-data)
            census-batch (u/walk-find #(= batch (:db/id %)) census)
            census-sheet (first (get census-batch :batch/sheets))]
        (is (= batch-name (:batch/name census-batch)))
        (is (= "meta" (:sheet/name census-sheet)))
        (is (= 4 (count (:sheet/columns census-sheet))))))))
