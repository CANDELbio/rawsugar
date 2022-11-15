(ns org.parkerici.rawsugar.recipe-test
  (:require [clojure.set :as set]
            [clojure.test :refer :all]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.rawsugar.recipes :as recipes]
            [org.parkerici.rawsugar.ops :as ops]
            [org.parkerici.rawsugar.test-utils :as tu]
            )
  )

(deftest validate-recipe-defs
  (let [recipes recipes/recipes]
    (doseq [[id {:keys [steps name doc] :as recipe-def}] recipes]
      (testing (str "recipe " id)
        (is name)                       ; required
        #_ (is doc)                     ; probably should be required
        (doseq [{:keys [op id] :as step} steps]
          (testing (str "step" id))
        ;; validate a step
        (when op
          (testing (str "operation" op)
            (is (contains? (:operations ops/ops) op)))))))))

