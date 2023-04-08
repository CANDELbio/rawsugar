(ns org.candelbio.rawsugar.server-utils
  (:require [clojure.data.json :as json]
            )
    (:import [org.apache.commons.codec Charsets]
             [org.apache.commons.codec.binary Base64])
    )

(defn base64-json->
  [base64-str]
  (-> base64-str
      Base64/decodeBase64
      (String. Charsets/UTF_8)
      (json/read-str :key-fn keyword)))

;;; Here for namespace reasons
(def server-start (atom nil))

(defn save-start-time []
  (reset! server-start (java.util.Date.)))

(defn start-time []
  @server-start)
