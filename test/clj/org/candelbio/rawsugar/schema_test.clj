(ns org.candelbio.rawsugar.schema-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [org.candelbio.rawsugar.schema :refer :all]
            [org.candelbio.alzabo.schema :as alz]
            [org.candelbio.rawsugar.test-utils :as tu]))

(deftest validate-schema
  ;; will throw error if invalid
  (alz/validate-schema schema))

(deftest test-datomic-schema
  (tu/with-datomic-context
    (transact-schema)))

;;; No, this hasn't been regularized to above schema yet, but should be
#_
(deftest validate-candel-schema
  (is (s/valid? ::schema cschema/schema)))

