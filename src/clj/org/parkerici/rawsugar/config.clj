(ns org.parkerici.rawsugar.config
  (:require [environ.core :as env]
            [taoensso.timbre :as log]
            [clojure.set :as set]
            ))

;;; The config is set up by lein and the build scripts; this is the app's interface to it
(def config-fields [:tier :gcs-project :gcs-bucket :web-endpoint
                    :datomic-endpoint :datomic-db-name
                    :slack-posting-url :slack-api-token
                    :oauth-client-id :oauth-client-secret :oauth-callback
                    :desktop-oauth-client-id :desktop-oauth-client-secret
                    :temp-directory])

;;; If a config field is not in here, then it must have a value defined by the environment or an error will be thrown
;; TODO maybe add cellengine creds (currently passed through secret / env variables CELLENGINE_USERNAME CELLENGINE_PASSWORD
(def config-defaults
  {:datomic-db-name "rawsugar"
   :temp-directory "/tmp/storage"
   :slack-posting-url false
   :slack-api-token false
   :web-endpoint false
   :oauth-client-id false
   :oauth-client-secret false
   :oauth-callback ""                   ;can't be false
   :desktop-oauth-client-id false
   :desktop-oauth-client-secret false
   })

(defn config
  []
  (merge config-defaults
         (select-keys env/env config-fields)))

;;; Build reality check, enforces that all config fields are set.
(defn check-config
  []
  (log/debugf "Config: %s" (config))
  (log/tracef "Full config: %s" env/env)
  (assert (every? #(contains? (config) %) config-fields)
          (str "Missing config parameters: "
               (set/difference (set config-fields)
                               (set (keys (config)))))))
  
