(ns org.candelbio.rawsugar.updown-test
  (:require [environ.core :refer [env]]
            [clojure.test :refer :all]
            [clojure.string :as str]
            [org.candelbio.rawsugar.files :as files]
            [org.candelbio.rawsugar.blob :as blob]
            [org.candelbio.rawsugar.sheet :as sheet]
            [org.candelbio.rawsugar.batches :as batches]
            [org.candelbio.rawsugar.datomic :as d]
            [org.candelbio.rawsugar.test-utils :as tu]
            [org.candelbio.rawsugar.updown :refer :all]
            [taoensso.timbre :as log]))


;;; Sanity checking
(log/infof "Config: %s" (select-keys env [:gcs-project :gcs-bucket :datomic-endpoint]))

(deftest upload-test
  (tu/with-project-batch project batch
    (let [entity-id (:db/id (upload project batch "test/resources/randomfile.txt" "" {}))]
      (tu/with-datomic-context
        (let [entity (d/get-entity entity-id)]
          (is (= "test/resources/randomfile.txt" (:file/pathname entity)))
          (is (= "txt" (:file/extension entity)))
          (is (= "9BFC2F23DD61E70F186401FAB8D0F26DD5B27D434A65E661E056619AF50A513A" (:file/hash entity)))
          ;; eg "gs://pici-data-warehouse-test/test-project/79aad731-6683-4178-a666-e930d970db37.txt"))
          (is (re-matches #"gs\://(.*)/(.*)\/(.*)\.txt" (:file/location entity))))))))

(deftest upload-directory-test
  (tu/with-project-batch project batch
    (let [path "test/resources/proj0/files/"]
      (upload-directory project batch path {})
      (tu/with-datomic-context
        (let [files (batches/batch-files batch)
              foo-file (some #(and (= (:file/pathname %) "foo.txt") %) files)]
          ;; Working locally, failing big on CI for some reaons
          (is (= 3 (count files)))
          (is (= "txt" (:file/extension foo-file)))
          (map (partial files/delete-file project) files))))))

(deftest upload-single-file-test
  (tu/with-project-batch project batch
    (let [path "test/resources/proj0/files/unmatched.txt"]
      (upload-directory project batch path {})
      (tu/with-datomic-context
        (let [files (batches/batch-files batch)]
          (is (= 1 (count files)))
          (is (= "unmatched.txt" (:file/pathname (first files))))
          (is (= "txt" (:file/extension (first files))))
          (map (partial files/delete-file project) files))))))

(def meta-sheet-data
  '({:b "wilma", :a "fred", :c "pebbles"}
    {:a "george", :c "elroy", :b "jane"}
    {:c "bam-bam", :b "betty", :a "barney"}))

(deftest upload-sheet-test
  (tu/with-project-batch project batch
    (let [path "test/resources/proj2/meta.tsv"]
      (upload-sheets project batch path {})
      (tu/with-datomic-context
        (let [sheets (batches/batch-sheets batch)]
          (is (= 1 (count sheets)))
          (is (= "meta" (:sheet/name (first sheets))))
          (is (tu/set= meta-sheet-data (sheet/get-sheet-data (:db/id (first sheets)) :column-format :keyword)))
          (testing "column indexing"
            (let [data (sheet/get-sheet-data (:db/id (first sheets)) :column-format :entity)
                  columns (keys (first data))]
              (is (every? number? (map :column/index columns)))
              (is (= '("a" "b" "c")
                     (map :column/name (sort-by :column/index columns))                     )))))))))


;;; Tests upload from a gs: path
(deftest gs-upload-test
  (tu/with-project-batch project batch
    (let [gs-path (format "gs://%s/%s/" (:gcs-bucket env) project)]
      (blob/copy-directory "test/resources/proj0/files/" gs-path)
      (upload-directory project batch gs-path {})
      (tu/with-datomic-context
        (let [files (batches/batch-files batch nil)
              foo-file (some #(and (= (:file/pathname %) "foo.txt") %) files)]
          (is (= 3 (count files)))
          (is (= "txt" (:file/extension foo-file)))
          (map (partial files/delete-file project) files))))))

(deftest gs-upload-sheet-test
  (tu/with-project-batch project batch
    (let [local-path "test/resources/proj2/meta.tsv"
          gs-path (format "gs://%s/%s/meta.tsv" (:gcs-bucket env) project)]
      (blob/copy local-path gs-path)
      (upload-sheets project batch gs-path {})
      (tu/with-datomic-context
        (let [sheets (batches/batch-sheets batch)]
          (is (= 1 (count sheets)))
          (is (= "meta" (:sheet/name (first sheets))))
          (is (tu/set= meta-sheet-data (sheet/get-sheet-data (:db/id (first sheets)) :column-format :keyword))))))))

(deftest gs-upload-single-file-test
  (tu/with-project-batch project batch
    (let [gs-path (format "gs://%s/%s/unmatched.txt" (:gcs-bucket env) project)]
      (blob/copy-directory "test/resources/proj0/files/unmatched.txt" gs-path)
      (upload-directory project batch gs-path {})
      (tu/with-datomic-context
        (let [files (batches/batch-files batch)]
          (is (= 1 (count files)))
          (is (= "unmatched.txt" (:file/pathname (first files))))
          (is (= "txt" (:file/extension (first files))))
          (map (partial files/delete-file project) files))))))

(deftest upload-sheet-and-files-test
  (tu/with-project-batch project batch
    (upload-sheet-and-files project batch "test/resources/proj0/meta2.tsv" "test/resources/proj0/files" {})
    (tu/with-datomic-context
      (let [project-all (d/get-entity project)]
        (is (= 1 (count (:project/batches project-all))))
        (let [batch-all (first (:project/batches project-all))]
          (is (= batch (:db/id batch-all)))
          ;; TODO More 
          )))))


;;; Utils? Doesn't this exist?
(defmacro with-output-to-file [f & body]
  `(with-open [o# (clojure.java.io/writer ~f)]
     (binding [*out* o#]
       ~@body)))

(deftest update-sheet-test
  (tu/with-project-batch project batch
    (upload-sheets project batch "test/resources/proj0/meta.tsv")
    (tu/with-datomic-context
      (testing "change value"
        (let [file (tu/temp-file-path ".tsv")
              sheet (sheet/lookup-sheet batch "meta")
              original-data (vec (sheet/get-sheet-data sheet :column-format :name :row-id? true))
              new-value "zorch"
              changed-data (assoc-in original-data [0 "a"] new-value)]
          (tu/with-datomic-context
            (with-output-to-file file
              (sheet/export-sheet changed-data))
            ;; TODO this needs to produce new sheet
            (let [new-sheet
                  (update-sheet-from-file project batch sheet file)]
              (is (not (= new-sheet sheet)))
              (tu/with-datomic-context        
                (let [data (sheet/get-sheet-data new-sheet :column-format :name :row-id? true)]
                  (is (some #(= new-value (get % "a")) data)))
                )))))
      ;; TODO same changes as above
      (testing "add column"
        (tu/with-datomic-context
          (let [sheet (sheet/lookup-sheet batch "meta")
                export 
                (with-out-str
                  (sheet/export-sheet (sheet/get-sheet-data sheet :column-format :name :row-id? true)))
                lines (str/split export #"\n")
                with-new-col (str/join "\n"
                                       (list 
                                        (str (nth lines 0) "\tnoocol")
                                        (str (nth lines 1) "\tfred")
                                        (str (nth lines 2) "\tethel")))
                file (tu/temp-file-path ".tsv")]
            (spit file with-new-col)
            (let [new-sheet
                  (update-sheet-from-file project batch sheet file)]
              (is (not (= new-sheet sheet)))
              (tu/with-datomic-context
                (let [new-sheet (sheet/get-sheet-data new-sheet :column-format :name)]
                  (is (= #{"fred" "ethel"}
                         (set (map #(get % "noocol") new-sheet)))))))))))))

(deftest upload-and-match-test
  (tu/with-project-batch project batch
    (upload-sheets project batch "test/resources/proj4/meta.tsv")
    (tu/with-datomic-context
      (let [sheet-meta (sheet/lookup-sheet batch "meta")]
        (upload-and-match project batch sheet-meta "test/resources/proj4/files/" nil {})
        (tu/with-datomic-context
          (let [sheet-matched (sheet/lookup-sheet batch "meta-matched")
                rows (sheet/get-sheet-data sheet-matched :column-format :name :files? true)]
            (is (= 2 (count rows)))
            (is (some #(and (= "foo" (get % "a"))
                            (= "foo.txt" (get-in % [:files 0 :file/pathname])))
                      rows))
            (is (some #(and (= "bar" (get % "a"))
                            (= "bar.xtx" (get-in % [:files 0 :file/pathname])))
                      rows))
            ))))))

