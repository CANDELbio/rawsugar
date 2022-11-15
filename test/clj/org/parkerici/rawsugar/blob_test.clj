(ns org.parkerici.rawsugar.blob-test
  (:require 
            [clojure.test :refer :all]
            [org.parkerici.rawsugar.blob :refer :all]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.multitool.cljcore :as ju]
            [org.parkerici.rawsugar.test-utils :as tu]
            ))

;;; Everything moved to updown-test, so TODO add some here

(deftest filter-template-test
  (let [files '("foo.txt" "bar.png" "quux.jpg")]
    (is (= '("foo.txt") (filter-template "*.txt" files)))
    (is (= '("bar.png" "quux.jpg") (filter-template "*.{png,jpg}" files)))
    (is (= '("bar.png" "quux.jpg") (filter-template "*p*" files)))
    ))
