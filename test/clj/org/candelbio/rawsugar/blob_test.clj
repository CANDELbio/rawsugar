(ns org.candelbio.rawsugar.blob-test
  (:require 
            [clojure.test :refer :all]
            [org.candelbio.rawsugar.blob :refer :all]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju]
            [org.candelbio.rawsugar.test-utils :as tu]
            ))

;;; Everything moved to updown-test, so TODO add some here

(deftest filter-template-test
  (let [files '("foo.txt" "bar.png" "quux.jpg")]
    (is (= '("foo.txt") (filter-template "*.txt" files)))
    (is (= '("bar.png" "quux.jpg") (filter-template "*.{png,jpg}" files)))
    (is (= '("bar.png" "quux.jpg") (filter-template "*p*" files)))
    ))
