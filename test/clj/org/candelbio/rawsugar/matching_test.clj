(ns org.candelbio.rawsugar.matching-test
  (:require [clojure.test :refer :all]
            [org.candelbio.rawsugar.test-utils :as tu]
            [org.candelbio.rawsugar.matching :refer :all]
            ))

(deftest scored-matches-test
  (is (= '([3 "foo" "ship of fools"] [2 "oo" "good loving"] [0 nil "dark star"])
         (scored-matches "foo" #{"good loving" "ship of fools" "dark star"}))))

(deftest get-best-threshold-test
  (is (= [:metatron :c 3]
         (get-best-threshold {:beniel :a
                              :rubiel :b
                              :metatron :c}
                             "tatler" nil)))
  (is (= nil
         (get-best-threshold {:beniel :a
                              :rubiel :b
                              :metatron :c}
                             "tatler" 4)))
  )
  
