(ns org.parkerici.rawsugar.glue.google.iap-test
  (:require [clojure.test :refer :all]
            [org.parkerici.rawsugar.glue.google.iap :as google]))

;;; HHH Temp turning this off since we don't have credentials
#_
(deftest credentials-test
  (is (not (nil? (google/get-service-account-credentials google/service-account-credentials)) ))
  )
