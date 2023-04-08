(ns org.candelbio.rawsugar.request
  )

;;; Vars that get dynamically bound around web request handlers
;;; In their own package to avoid circular dependency problems

(def ^:dynamic *request* nil)           ;Holds the http request object

(def ^:dynamic *as-of* nil)             ;An optional txn id to go back in time

(defn origin
  []
  (get-in *request* [:headers "origin"]))
