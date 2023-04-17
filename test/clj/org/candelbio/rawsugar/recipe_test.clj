(ns org.candelbio.rawsugar.recipe-test
  (:require [clojure.set :as set]
            [clojure.test :refer :all]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.rawsugar.recipes :as recipes]
            [org.candelbio.rawsugar.ops :as ops]
            [org.candelbio.rawsugar.test-utils :as tu]
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

