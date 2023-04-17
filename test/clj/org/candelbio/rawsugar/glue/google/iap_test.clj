(ns org.candelbio.rawsugar.glue.google.iap-test
  (:require [clojure.test :refer :all]
            [org.candelbio.rawsugar.glue.google.iap :as google]))

;;; Temp turning this off since we don't have credentials
#_
(deftest credentials-test
  (is (not (nil? (google/get-service-account-credentials google/service-account-credentials)) ))
  )
