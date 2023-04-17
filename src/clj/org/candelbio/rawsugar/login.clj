(ns org.candelbio.rawsugar.login
  (:require [me.raynes.fs :as fs]
            [environ.core :refer [env]]
            [org.candelbio.rawsugar.request :as request]
            [org.candelbio.multitool.cljcore :as ju]))

;;; Credential management.

(def cred-file "./.rawsugar-creds")
(def credentials (atom nil))

(defn save-credentials
  [new-credentials]
  (reset! credentials new-credentials)
  (spit cred-file new-credentials))

;;; for testing
(defn delete-credentials
  []
  (reset! credentials nil)
  (fs/delete cred-file))

(defn user
  []
  (or (and request/*request*
           (get-in request/*request* [:login :email]))
      (:user @credentials)
      (:user env)
      ))

(defn login!
  [user]
  (save-credentials {:user user
                     :login-time (ju/now)}))

(defn require-login
  []
  (when-not (user)
    (throw (ex-info (str "Please log in with the `login <username>` ") {:type :login}))))

;;; Maybe better way to ensure this? Not sure what good practice is TODO cognitect guys say use sierra components
(when (fs/exists? cred-file)
  (reset! credentials (read-string (slurp cred-file))))



