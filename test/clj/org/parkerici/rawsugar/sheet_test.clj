(ns org.parkerici.rawsugar.sheet-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]
            [org.parkerici.rawsugar.sheet :refer :all]
            [org.parkerici.rawsugar.test-utils :as tu]
            [org.parkerici.rawsugar.updown :as updown]
            [org.parkerici.rawsugar.datomic :as d]
            [org.parkerici.multitool.core :as u])
  (:import clojure.lang.ExceptionInfo))



(deftest sheet-roundtrip-test
  (tu/with-project-batch project batch
    (updown/upload-sheets project batch "test/resources/proj0/meta.tsv")
    (tu/with-datomic-context
      (let [data (get-sheet-data (lookup-sheet batch "meta") :column-format :name)]
        (is (= 2 (count data)))
        (is (= #{2 4} (set (map #(get % "c") data))))))))

(deftest sheet-type-test
  (tu/with-project-batch project batch
    (updown/upload-sheets project batch "test/resources/proj1/meta.tsv")
    (updown/upload-sheets project batch "test/resources/proj1/meta.tsv" {:filename "stringy.tsv" :strings? true}) ;TODO not sure how to move into new framework
    (tu/with-datomic-context
      (let [data (get-sheet-data (lookup-sheet batch "meta") :column-format :name)
            stringy-data (get-sheet-data (lookup-sheet batch "stringy") :column-format :name)]
        ;; Note: would be nicer if c was a string here, but can't see a way to make that work
        (is (= {"a" 1, "b" 1.0, "c" 1} (first data)))
        (is (= {"a" "1", "b" "1.0", "c" "1"} (first stringy-data)))))))

;;; Out of service
#_
(deftest add-row-test
  (tu/with-project-batch project batch
    (updown/upload-sheets project batch "test/resources/proj0/meta.tsv")
    (tu/with-datomic-context
      (let [sheet (lookup-sheet batch "meta")]
        (is (= 2 (count (get-sheet-data sheet))))           ;sanity check
        (d/transact (add-row-txn sheet {"a" "wow", "b" 999}))
        (tu/with-datomic-context
          (let [data (get-sheet-data sheet :column-format :keyword)]
            (is (= 3 (count data)))
            (is (some #(= "wow" (get % :a)) data))))))))


(def pseudo-files
  [{:file/pathname "/this/is/a/long/pathname/fred.txt"}
   {:file/pathname "/this/is/a/long/pathname/elroy.txt"}
   {:file/pathname "/this/is/a/long/pathname/sigmund.txt"}])

(defn- match=
  [matches filename sheet col val]
  (let [col (lookup-column sheet col)]
    (some #(and (= (:file/pathname (first %)) filename)
                (= val (get (second %) col)))
          matches)))

(deftest match-test
  (tu/with-project-batch-new [project batch]
    (let [sheet (tu/test-sheet batch "test/resources/proj2/meta.tsv")]
      (tu/with-datomic-context
        (testing "basic"
          (let [matches (match-files-columns sheet pseudo-files nil)]
            (is (= 2 (count matches)))
            (is (match= matches "/this/is/a/long/pathname/fred.txt" sheet "a" "fred"))
            (is (match= matches "/this/is/a/long/pathname/elroy.txt" sheet "c" "elroy"))))
        (testing "column"
          (let [col (lookup-column sheet "a")
                matches (match-files-columns sheet pseudo-files [col])]
            (is (= 1 (count matches)))
            (is (match= matches "/this/is/a/long/pathname/fred.txt" sheet "a" "fred"))))
        (testing "threshold"
          (let [matches (match-files-columns sheet pseudo-files nil {:match-threshold 2})]
            (is (= 3 (count matches)))))))))

(def pseudo-files-exact
  [{:file/pathname "/this/is/a/long/pathname/fred.txt"}
   {:file/pathname "/this/is/a/long/pathname/leroy.txt"}
   {:file/pathname "/this/is/a/long/pathname/sigmund.txt"}])

(deftest exact-match-test
  (tu/with-project-batch project batch
    (updown/upload-sheets project batch "test/resources/proj2/meta.tsv")
    (tu/with-datomic-context
      (let [sheet (lookup-sheet batch "meta")]
        (testing "basic"
          (let [matches (match-files-columns sheet pseudo-files-exact nil {:exact-match? true})]
            (is (= 1 (count matches)))
            (is (match= matches "/this/is/a/long/pathname/fred.txt" sheet "a" "fred"))))
        (testing "column"
          (let [column (lookup-column sheet "a")
                matches (match-files-columns sheet pseudo-files-exact [column] {:exact-match? true})]
            (is (= 1 (count matches)))
            (is (match= matches "/this/is/a/long/pathname/fred.txt" sheet "a" "fred"))))
        ))))

;;; Tests basic exact join without column collisions
;;; New test scheme!
(deftest simple-join-test
  (tu/with-project-batch-new [project batch]
    (let [sheet1 (tu/test-sheet batch "test/resources/proj0/meta.tsv")
          sheet2 (tu/test-sheet batch "test/resources/proj0/meta2a.tsv")]
      (tu/with-datomic-context
        (let [col1 (lookup-column sheet1 "a")
              col2 (lookup-column sheet2 "a")
              txn (join-sheets sheet1 col1 sheet2 col2 false false)
              new-sheet (tu/transact-to-new-sheet txn)]
          (tu/with-datomic-context
            (let [new-sheet (get-sheet-data new-sheet :column-format :name)]
              (is (some #(= {"a" "foo" "b" 1 "c" 2
                             "fileName" "foo.txt"
                             "bx" 23 "cx" 2}
                            %)
                        new-sheet)))))))))
        

(deftest complex-join-test
  (tu/with-project-batch-new [project batch]
    (let [sheet1 (tu/test-sheet batch "test/resources/proj0/meta.tsv")
          sheet2 (tu/test-sheet batch "test/resources/proj0/meta2.tsv")]
      (tu/with-datomic-context
        (let [col1 (lookup-column sheet1 "a")
              col2 (lookup-column sheet2 "a")]
          (testing "exact join with collision"
            (let [txn
                  (join-sheets sheet1 col1 sheet2 col2 false false)]
              #_ (is (= (count txn) (* 2 (+ 3 3)))) ; 3 cols, 3 cells, 2 txn elements each
              (let [new-sheet (tu/transact-to-new-sheet txn)]
                (tu/with-datomic-context
                  (let [new-sheet-data (get-sheet-data new-sheet :column-format :name)]
                    (is (some #(= {"a" "foo" "b" 1 "c" 2
                                   "fileName" "foo.txt"
                                   "bx" 23 "c-2" 2 "fileName-2" "foo.txt"}
                                  %)
                              new-sheet-data)))))))
      (testing "fuzzy join"
        (let [txn (join-sheets sheet1 col1 sheet2 col2 true false)]
          #_ (is (= (count txn) (* 2 (+ 4 8)))) ; 4 cols, 8 cells, 2 txn elements each
          (let [new-sheet (tu/transact-to-new-sheet txn)]
            (tu/with-datomic-context
              (let [new-sheet-data (get-sheet-data new-sheet :column-format :name)]
                (is (some #(= {"a" "bar" "b" 3 "c" 4
                               "fileName" "bar.txt"
                               "a-2" "bad" "bx" 99 "c-2" 2
                               "fileName-2" "bad.txt"}
                              %)
                          new-sheet-data)))))

          )
        ))))))

(deftest join-with-files-test
  (tu/with-project-batch-new [project batch]
    (let [sheet1 (tu/test-sheet-files batch "test/resources/proj2/meta.tsv" "test/resources/proj2/files")
          sheet2 (tu/test-sheet-files batch "test/resources/proj0/meta2.tsv" "test/resources/proj0/files")]
      (tu/with-datomic-context
        ;; sanity checks
        (let [sheet1-data (get-sheet-data sheet1 :files? true)
              sheet2-data (get-sheet-data sheet2 :files? true)]
          (is (= 3 (count sheet1-data)))
          (is (= 3 (count sheet2-data)))
          ;; TODO test files are there
          )
        (let [col1 (lookup-column sheet1 "a")
              col2 (lookup-column sheet2 "a")
              txn (join-sheets sheet1 col1 sheet2 col2 true "joined")
              new-sheet (tu/transact-to-new-sheet txn)]
          (tu/with-datomic-context
            (let [new-sheet-data (get-sheet-data new-sheet :column-format :name :files? true)
                  fred-row (u/some-thing #(= "fred" (get % "a") ) new-sheet-data)
                  fred-files (map :file/pathname (:files fred-row))]
              (is (= (set fred-files) #{"/fredo-corleone.txt" "/foo.txt"})
                  ))))))))

(deftest union-test-no-files
  (tu/with-project-batch-new [project batch "union"]
    (let [sheet1 (tu/test-sheet batch "test/resources/proj0/meta.tsv")
          sheet2 (tu/test-sheet batch "test/resources/proj0/meta2.tsv")]
      (tu/with-datomic-context
        (let [txn (union-sheets sheet1 sheet2 "union")
              sheet-1-data (get-sheet-data sheet1 :column-format :name)
              sheet-2-data (get-sheet-data sheet2 :column-format :name)
              new-sheet (tu/transact-to-new-sheet txn)]
          (tu/with-datomic-context
            (let [new-sheet-data (get-sheet-data new-sheet :column-format :name )]
              (is (= (set new-sheet-data)
                     (set/union (set sheet-1-data)
                                (set sheet-2-data))))
              )))
        ))))

;;; TODO check that this actually tests the right thing
(deftest union-test-with-files
  (tu/with-project-batch-new [project batch "union-files"]
    (let [sheet1 (tu/test-sheet-files batch "test/resources/proj0/meta.tsv" "test/resources/proj0/files")
          sheet2 (tu/test-sheet-files batch  "test/resources/proj0/meta2.tsv" "test/resources/proj0/files")]
      (tu/with-datomic-context
        (let [txn (union-sheets sheet1 sheet2 "union")
              sheet-1-data (get-sheet-data sheet1 :column-format :name :files? :ids)
              sheet-2-data (get-sheet-data sheet2 :column-format :name :files? :ids)
              new-sheet (tu/transact-to-new-sheet txn)]
          (tu/with-datomic-context
            (let [new-sheet-data (get-sheet-data new-sheet :column-format :name :files? :ids)]
              (testing "without files"
                (is (= (set new-sheet-data)
                       (set/union (set sheet-1-data)
                                  (set sheet-2-data)))))
              )))
        ))))

(deftest update-cells-test
  (tu/with-project-batch-new [project batch "update-cells"]
    (let [sheet (tu/test-sheet batch "test/resources/proj0/meta.tsv")
          new-content "it's crackers to slip a rozzer the dropsy in snide"]
      (tu/with-datomic-context
        (let [sheet-data (get-sheet-data sheet :row-id? true)
              row (u/some-thing #(contains? (set (vals %)) "foo.txt") sheet-data)
              row-id (:row-id row)
              col (first (u/some-thing #(= "foo" (second %)) row))
              updates {row-id {col new-content}}
              txn (-> (derived-sheet-txn sheet "revised")
                      (derived-sheet-change-values sheet updates))]
          (let [new-sheet (tu/transact-to-new-sheet txn)]
            (tu/with-datomic-context
              (let [new-sheet-data (get-sheet-data new-sheet :row-id? true)]
                (is (= 2 (count new-sheet-data)))
                (let [new-row (u/some-thing #(= #{new-content "foo.txt" 1 2}
                                               (set (vals (dissoc % :row-id))))
                                           new-sheet-data)]
                  (is new-row)
                  (is (not (contains? (set (map :row-id sheet-data))
                                      (:row-id new-row))))
                  (is (= (set (keys new-row))
                         (set (keys (first sheet-data))))))))))))))

