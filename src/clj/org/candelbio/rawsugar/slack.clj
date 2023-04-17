(ns org.candelbio.rawsugar.slack
  (:require [org.candelbio.rawsugar.config :as config]
            [org.candelbio.rawsugar.login :as login]
            [org.candelbio.rawsugar.request :as request]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.rawsugar.datomic :as d]
            [org.candelbio.rawsugar.cnavigate :as c]
            [taoensso.timbre :as log]
            [clj-http.client :as client]
            [clojure.data.json :as json]))

;;; Minimal Slack API client. 

(defn post-message
  "Post a message on the Rawsugar Slack channel"
  [text]
  (log/info "Slack: " text)
  (when (:slack-posting-url (config/config))
    (client/post (:slack-posting-url (config/config))
                 {:content-type :json
                  :body (json/write-str {:text text})})))

;;; Slack user name is different from the display name, and at PICI
;;; seems to be = to official email name.
(defn- user-id-map
  "Returns a map of slack user names to slack ids."
  []
  (when (:slack-api-token (config/config))
    (let [raw
          (-> "https://slack.com/api/users.list"
              (client/get {:content-type :json
                           :query-params {:token (:slack-api-token (config/config))}})
              :body
              (json/read-str :key-fn keyword))]
      (into {}
            (for [{:keys [name id]} (:members raw)]
              [name id])))))

(u/def-lazy user-ids (or (user-id-map) {}))

(defn slack-id
  "Return a string representing the current web user, using a Slack ID which triggers notifications if possible"
  []
  (if-let [uname (second (re-find #"(.*)@" (login/user)))] ;parse the user name out of (login/user) which is an email address.
    (if-let [slack-id (get @user-ids uname)]
      (format "<@%s>" slack-id)
      (format "@%s" uname))
    ""))

;;; Slack uses a weird, pseudo-markdown markup syntax: https://www.markdownguide.org/tools/slack/

(defn mrkdwn-link
  [url name]
  (format "<%s|%s>" (str (:web-endpoint (config/config)) url) name))

(defn project-mrkdwn-link
  [project]
  (if project
  (let [project (d/pull '[:db/id :project/name] project)]
    (mrkdwn-link (c/url-for :project :project (:db/id project))
                 (:project/name project)))
  "<unknown>"))

(defn sheet-mrkdwn-link
  [sheet]
  (let [[project batch sheet]
        (d/q1
         '[:find
           (pull ?project [:db/id :project/name])
           (pull ?batch [:db/id :batch/name])
           (pull ?sheet [:db/id :sheet/name])
           :in $ ?sheet
           :where 
           [?project :project/batches ?batch]
           [?batch :batch/sheets ?sheet]]
         sheet)
        url
        (c/url-for :sheet :project (:db/id project) :batch (:db/id batch) :sheet (:db/id sheet))]
    (mrkdwn-link url (:sheet/name sheet))))

;;; TODO the background tasks in glue/cellengine should use this
(defn start-background-task
  [task-description worker]
  (let [slack-id (slack-id)]
    (post-message
     (format "%s, your %s is starting"  slack-id task-description))
    ;; Start the export job in a background thread
    (future
      (try
        (let [result (worker)]
          (post-message
           (format "%s, your %s is finished; %s" ;TODO newline
                   slack-id
                   task-description
                   result)))
        (catch Throwable e
          (post-message
           (format "Error! %s, your %s failed :cry: : %s"
                   slack-id
                   task-description
                   (.getMessage e))
           ))))
    nil                                 ;Return nil, callers check this 
    ))
