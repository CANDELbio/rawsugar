(ns org.candelbio.rawsugar.server
  (:require [ring.adapter.jetty :as jetty]
            [taoensso.timbre :as log]
            [trptcolin.versioneer.core :as version]
            [org.candelbio.rawsugar.server-utils :as su]
            [org.candelbio.rawsugar.handler :as handler]))

(def server (atom nil))

(defn stop
  []
  (when @server
    (.stop @server)))

(defn start
  ([port] (start port handler/app))
  ([port handler]
   (log/infof "Starting rawsugar server version %s at port %s" (version/get-version "rawsugar" "rawsugar") port)
   (stop)
   (su/save-start-time)
   (reset! server (jetty/run-jetty handler {:port port :join? false}))))

;;; For use only for dev/testing purposes
(defn start-insecure
  [port]
  (start port handler/insecure-app))
  






