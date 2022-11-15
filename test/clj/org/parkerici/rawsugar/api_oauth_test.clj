(ns org.parkerici.rawsugar.api-oauth-test
  (:require [clojure.test :refer :all]
            [org.parkerici.rawsugar.server :as server]
            [org.parkerici.rawsugar.api-test :refer :all]))

;;; Reuses some of the machinery in api_test, but leaves Oauth on

(defn with-authenticated-server [f]
  ;; Test with authentication ON
  (server/start test-port)
  (f)
  (server/stop))

(use-fixtures :once with-authenticated-server)

(deftest unauthenticated-get
  (is (thrown-with-msg?
       Exception #"API error"           ;TODO check for 401 in the ex-data? Not sure how.
       (api-get "/api/list-projects" {}))))

;;; TODO test authenticated request
