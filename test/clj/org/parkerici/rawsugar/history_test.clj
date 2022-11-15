(ns org.parkerici.rawsugar.history-test
  (:require [clojure.test :refer :all]
            [org.parkerici.rawsugar.history :refer :all]
            [org.parkerici.rawsugar.login :as login]
            [org.parkerici.rawsugar.sheet :as sheet]
            [org.parkerici.rawsugar.test-utils :as tu]
            [org.parkerici.rawsugar.updown :as updown]))

;;; Tests basic round-trip history
(deftest basic-history-test
  (login/login! "tester")
  (tu/with-project project
    (let [history (history :project project)
          add-project (first history)]
      (is (= 1 (count history)))
      (is (= :add-project (:operation/cmd add-project)))
      (is (= "tester" (:operation/agent add-project)))
      (is (= project (:operation/project add-project)))
      (is (= (list project-name) (read-string (:operation/parameters add-project))))
      (is (instance? java.util.Date (:operation/time add-project)))
      (is (instance? java.lang.Long (:operation/transaction add-project))))))


(deftest multiple-items-test
  (login/login! "tester")
  (tu/with-project-batch project batch
    (updown/upload-sheets project batch "test/resources/proj0/meta.tsv")
    (tu/with-datomic-context
      (let [sheet (sheet/lookup-sheet batch "meta")]
        (sheet/update-sheet project sheet {:sheet/name  "a"})
        (sheet/update-sheet project sheet {:sheet/name "b"})
        (tu/with-datomic-context
          (let [history (history :project project)]
            (is (= '(:add-project :add-batch :upload-file :upload-sheets :update-sheet :update-sheet)
                   (reverse (map :operation/cmd history))))))))))

(deftest full-history-test
  (tu/with-project project
    (is (= project (:operation/project (first (history)))))))

(deftest single-op-history-test
  (tu/with-project-batch project batch
    (let [full (history)
          single (history :op (:db/id (first full)))]
      (is (not (= 1 (count full))))
      (is (= 1 (count single)))
      (is (= (first single) (first full))))))

;;; humanize is tested by cli-test/history-test

