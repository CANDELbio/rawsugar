(ns org.parkerici.rawsugar.cli-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [mock-clj.core :as mc]
            [org.parkerici.rawsugar.cli :refer :all]
            [org.parkerici.rawsugar.history :as h]
            [org.parkerici.rawsugar.sheet :as sheet]
            [org.parkerici.rawsugar.batches :as batches]
            [org.parkerici.rawsugar.updown :as updown]
            [org.parkerici.rawsugar.projects :as projects]
            [org.parkerici.rawsugar.test-utils :as tu]
            [org.parkerici.multitool.core :as u]
            ))

;;; Run main with error handling turned off, so errors will be caught be test framework. Return output as a lista
(defn mainly [command & args]
  (tu/capture-output (apply main-no-error-handling command args)))

(defn mainly-no-logging [command & args]
  (tu/capture-output (apply -main command "-q" args)))

;;; Run main with normal error handling
(defn mainly-err [command & args]
  (apply -main command args))

(deftest upload-download-filter-test
  (tu/with-project-batch project batch
    (mainly "upload-sheets" project-name batch-name "test/resources/proj0/meta.tsv")
    (tu/with-datomic-context
      (let [projects (mainly "list-projects")
            sheet-name "meta"    
            sheets (mainly-no-logging "list-sheets" project-name batch-name)]
        (is (get (set projects) project-name))
        (is (= sheets (list sheet-name)))
        (let [unfiltered (mainly-no-logging "download-sheet" project-name batch-name sheet-name)
              filtered (mainly-no-logging "download-sheet" project-name batch-name sheet-name "--filter" "a=foo")]
          (is (= 3 (count unfiltered)))
          (is (= 2 (count filtered))))))))

(deftest upload-download-full-template-test
  (tu/with-project-batch project batch
    (mainly "upload" project-name batch-name "test/resources/proj0/meta.tsv" "test/resources/proj0/files/")
    (tu/with-datomic-context
      (is (= 2 (count (sheet/sheet-files (sheet/lookup-sheet batch "meta-matched")))))
      (is (= 2 (count (mainly-no-logging "list-files" project-name batch-name "meta-matched")))) ;in 2 ways!

    (let [sheet-name "meta-matched"]  
      (testing "no template"
        (let [download-dir (tu/temp-dir-path)
              download-transcript (mainly "download-files" download-dir project-name batch-name sheet-name)]
          ;; 1 headers, 2 sheet contents rows; 1 footer
          (is (= 4 (count download-transcript)))
          ;; Test files in download directory
          (is (tu/file-exists? (str download-dir "/foo.txt")))))

      (testing "with template"
        (let [download-dir (tu/temp-dir-path)    
              download-transcript (mainly "download-files" download-dir project-name batch-name sheet-name "-t" "{a}-{b}")]
          (is (= 4 (count download-transcript)))
          ;; Test files in download directory
          (is (tu/file-exists? (str download-dir "/foo-1.txt")))))
      ))))


(deftest upload-download-flat-test
  (tu/with-project-batch project batch
    (mainly "upload" project-name batch-name "test/resources/proj5/meta.tsv" "test/resources/proj5/files/")
    (tu/with-datomic-context
      (is (= 3 (count (batches/batch-files batch)))) ;sanity check: make sure files exist 
      (let [sheet-name "meta-matched"]  
        (testing "with paths"
          (let [download-dir (tu/temp-dir-path)
                download-transcript (mainly "download-files" download-dir project-name batch-name sheet-name)]
            ;; 1 headers, 2 sheet contents rows
            ;; Test files in download directory
            (is (tu/file-exists? (str download-dir "/foo/a.txt")))))
        (testing "flattened"
          (let [download-dir (tu/temp-dir-path)    
                download-transcript (mainly "download-files" download-dir project-name batch-name sheet-name "--flatten" )]
            ;; Test files in download directory
            (is (tu/file-exists? (str download-dir "/a.txt")))
            (is (tu/file-exists? (str download-dir "/b.txt")))))
        ))))

(deftest catch-duplicate-sheet-test
  (tu/with-project-batch project batch
    (mainly "upload" project-name batch-name "test/resources/proj0/meta.tsv")
    (tu/with-log-test [:error #"sheet already exists"]
      (mainly-err "upload" project-name batch-name "test/resources/proj0/meta.tsv"))))

(deftest string-flag-test
  ;; normal case
  (tu/with-project-batch project batch
    (mainly "upload-sheets" project-name batch-name "test/resources/proj0/meta.tsv")
    (tu/with-datomic-context
    (let [sheet-name "meta"
          sheet (sheet/lookup-sheet batch sheet-name)
          col (sheet/lookup-column sheet "b")
          values (map second (sheet/sheet-column-all-values sheet col))]
      (is (= 2 (count values)))
      (is (every? number? values)))))
  ;; -s flag
  (tu/with-project-batch project batch
    (mainly "upload-sheets" "-s" project-name batch-name "test/resources/proj0/meta.tsv" )
    (tu/with-datomic-context
    (let [sheet-name "meta"
          sheet (sheet/lookup-sheet batch sheet-name)
          col (sheet/lookup-column sheet "b")
          values (map second (sheet/sheet-column-all-values sheet col))]
      (is (= 2 (count values)))
      (is (every? string? values))))))

(deftest filter-to-map-test
  (is (= {"a" "b"} (filter-to-map "a=b  ")))
  (is (= {"a" 1, "b" [2 3]} (filter-to-map "a=1, b=[2 , 3]")))
  ;; real bug, tests handing - in values
  (is (= {"subjectId" "840-100100-001"} (filter-to-map "subjectId=840-100100-001")))
  )

(deftest update-test
  (tu/with-project-batch project batch
    (mainly "upload-sheets" project-name batch-name "test/resources/proj0/meta.tsv")
    (tu/with-datomic-context
      (let [sheet-name "meta"
            downloaded
            ;; rest because there's an info line now
            (rest (mainly "download-sheet" project-name batch-name sheet-name))
            modified (map #(str/replace % "bar" "quux") downloaded)
            temp-file (tu/temp-file-path ".tsv")]
        (tu/file-lines-out temp-file modified)
        (let [output (mainly "update" project-name batch-name sheet-name temp-file)
              new-sheet-line (u/some-thing #(re-find #"New sheet created" %) output)
              new-sheet-name (second (re-find #"New sheet created: (.*)" new-sheet-line))]
          (tu/with-datomic-context
            (let [sheet (sheet/lookup-sheet batch new-sheet-name)
                  result (sheet/get-sheet-data sheet :column-format :name)]
              (is (= 2 (count result)))
              (is (= #{"foo" "quux"} (set (map #(get % "a") result)))))))))))
    
(deftest project-commands-test
  (tu/with-project-name project-name
    (mainly "add-project" project-name)
    (tu/with-datomic-context
      (let [projects (mainly-no-logging "list-projects")]
        (is (some #(= project-name %) projects))
        (tu/with-log-test [:info #"Project .* deleted"]
          (mainly "delete" project-name))
        (tu/with-datomic-context
          (let [projects (mainly-no-logging "list-projects")]
            (is (not (some #(= project-name %) projects)))))))))

(deftest batch-commands-test
  (tu/with-project project
    (mainly "add-batch" project-name "mybatch")
    (tu/with-datomic-context
      (is (= '("mybatch") 
             (mainly-no-logging "list-batches" project-name)))
      (tu/with-log-test [:info #"Batch .* deleted"]
        (mainly "delete" project-name "mybatch"))
      (tu/with-datomic-context
        (is (empty?
             (mainly-no-logging "list-batches" project-name)))
        ))))

(deftest error-handling-test
  (tu/with-log-test [:error #"Unknown command"]
    (mainly-err "transmogrify" "something"))
  (tu/with-log-test [:error #"Project not found"]
    (mainly-err "list-sheets" "nonproject23"))
  (tu/with-log-test [:error #"Project not specified"]
    (mainly-err "list-sheets"))
  (tu/with-log-test [:error #"Unknown option"]
    (mainly-err "list-sheets" "-x" "men"))
  (tu/with-project-batch project batch
    (tu/with-log-test [:error #"Batch not found.*notthebatch"]
      (mainly-err "list-sheets" project-name "notthebatch"))
    (tu/with-log-test [:error #"Sheet not found.*notthesheet"]
      (mainly-err "list-columns" project-name batch-name "notthesheet"))
    (tu/with-log-test [:error #"Project already exists"]
      (mainly-err "add-project" project-name))
    (tu/with-log-test [:error #"Batch already exists"]
      (mainly-err "add-batch" project-name batch-name))
    ))

(deftest delete-sheet-test
  (tu/with-project-batch project batch
    (mainly "upload-sheets" project-name batch-name "test/resources/proj0/meta.tsv")
    (tu/with-datomic-context
      (is (= 1 (count (mainly-no-logging "list-sheets" project-name batch-name))))
      (let [sheet-name "meta"]
        (mainly "delete" project-name batch-name sheet-name)
        (tu/with-datomic-context
          (let [post-delete-sheets (mainly-no-logging "list-sheets" project-name batch-name)]
            (is (= 0 (count (mainly-no-logging "list-sheets" project-name batch-name))))))))))
 
(deftest list-values-filtered-test
  (tu/with-project-batch project batch
    (mainly "upload-sheets" project-name batch-name "test/resources/proj0/meta.tsv")
    (tu/with-datomic-context
      (let [sheet-name "meta"
            unfiltered
            (set (mainly-no-logging "list-column-values" project-name batch-name sheet-name "b"))
            filtered
            (set (mainly-no-logging "list-column-values" project-name batch-name sheet-name "b" "-f" "c=2"))]
        (is (= #{"1" "3"} unfiltered))
        (is (= #{"1"} filtered))))))

(deftest delete-file-test
  (tu/with-project-batch project batch
    (mainly "upload" project-name batch-name "test/resources/proj0/meta.tsv" "test/resources/proj0/files/")
    (tu/with-datomic-context
      (let [files (batches/batch-files batch)]
        (is (= 4 (count files)))        ;sheet, foo, bar, unmatched
        (mainly "delete-files" project-name batch-name "meta-matched")
        (tu/with-datomic-context
          (is (= 2 (count (batches/batch-files batch)))) ;foo and bar should be deleted
          (testing "history entry"
            (let [op (first (h/history :project project))]
              (is (= :delete-file (:operation/cmd op))))))))))

(deftest match-test
  (tu/with-project-batch project batch
    (mainly "upload" project-name batch-name "test/resources/proj2/meta.tsv" "test/resources/proj2/files/")
    (tu/with-datomic-context
      (let [data (sheet/get-sheet-data (sheet/lookup-sheet batch "meta-matched") :column-format :name :files? true)
            matched (fn [col val file]
                      (some
                       (fn [row] (and (= val (get row col))
                                      (= file (get-in row [:files 0 :file/pathname]))))
                       data))]
        (is (= 3 (count data)))
        (is (matched "a" "fred" "fredo-corleone.txt"))
        (is (matched "b" "betty" "bettie-page.txt"))
        (is (matched "a" "george" "george-tirebiter.txt"))
        ;; TODO test that syzygy.txt is unmatched
        ))))

(deftest upload-named-files-test
  (tu/with-project-batch project batch
    (mainly "upload" project-name batch-name "test/resources/proj4/meta.tsv" "test/resources/proj4/files/")
    (tu/with-datomic-context
      (is (= 1 (count (projects/project-batches project))))
      (let [sheet (sheet/lookup-sheet batch "meta-matched")
            data (sheet/get-sheet-data sheet :column-format :name :files? true)
            files (sheet/sheet-files sheet)]
        ;; 2 data files
        (is (= 2 (count files)))
        (is (= #{"foo.txt" "bar.xtx"} (set (map :file/pathname files))))))))

(deftest history-test
  (tu/with-project-batch project batch
    (let [out (map #(str/split % #"\t")
                   (mainly-no-logging "history" project-name))
          add-batch (second out)]
      (is (= ["date" "user" "txn" "cmd" "params"] (first out)))
      (is (= "tester" (nth add-batch 1)))
      (is (re-matches #"\d+" (nth add-batch 2)))
      (is (= "add-batch" (nth add-batch 3)))
      ;; TODO params is inconsistent, not sure what right thing is
      ;; "[17592186180912 batch33309]"
      )))
      
(deftest upload-files-test
  (tu/with-project-batch project batch
    (let [dir "test/resources/proj4/files/"]
      (mainly "upload-files" project-name batch-name dir "sheeeit")
      (testing "no sheet arg"
        ;; TODO macroize this into expect-called-with or somesuch
        (mc/with-mock [updown/upload-and-match nil]
          (mainly "upload-files" project-name batch-name dir)
          (let [calls (mc/calls updown/upload-and-match)]
            (is (= 1 (count calls)))
            (is (= (list project batch nil dir nil)
                   (take 5 (first calls)))))))
      (testing "new batch arg"
        (mc/with-mock [updown/upload-and-match nil]
          (mainly "upload-files" project-name "newbatch" dir)
          (let [calls (mc/calls updown/upload-and-match)]
            (is (= 1 (count calls)))
            (is (= project (first (first calls))))
            ;; It should create a new batch
            (is (and (number? (second (first calls)))
                     (not (= batch (second (first calls)))))))))
      ;; TODO test new project, new sheet
      )))

(deftest list-download-files-test
  (tu/with-project-batch project batch
    (mainly "upload" project-name batch-name "test/resources/proj4/meta.tsv" "test/resources/proj4/files/")
    (testing "list-files"
      (let [out-project (mainly-no-logging "list-files" project-name)
            out-batch (mainly-no-logging "list-files" project-name batch-name)
            out-sheet (mainly-no-logging "list-files" project-name batch-name "meta-matched")]
        (is (= 4 (count out-project)))
        (is (= 4 (count out-batch)))
        (is (= 2 (count out-sheet)))))
    (testing "download-files"
      (let [out-dir (tu/temp-dir-path)
            out-project (mainly "download-files" out-dir project-name )
            out-batch (mainly "download-files" out-dir project-name batch-name)
            out-sheet (mainly "download-files" out-dir project-name batch-name "meta-matched")]
        ;; line per file + header and footer lines
        (is (= (+ 4 2) (count out-project)))
        (is (= (+ 4 2) (count out-batch)))
        (is (= (+ 2 2) (count out-sheet))))
      )))

