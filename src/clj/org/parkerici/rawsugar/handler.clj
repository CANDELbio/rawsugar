(ns org.parkerici.rawsugar.handler
  (:require [compojure.core :refer [defroutes context GET POST make-route routes]]
            [compojure.route :as route]
            [ring.middleware.format :refer [wrap-restful-format]]
            [ring.middleware.format-response :refer [wrap-restful-response]]
            [clj-http.client :as client]
            [org.parkerici.rawsugar.datomic :as d]
            [org.parkerici.rawsugar.projects :as project]
            [org.parkerici.rawsugar.files :as files]
            [org.parkerici.rawsugar.sheet :as sheet]
            [org.parkerici.rawsugar.batches :as batches]
            [org.parkerici.rawsugar.recipes :as recipes]
            [org.parkerici.rawsugar.blob :as blob]
            [org.parkerici.rawsugar.schema :as schema]
            [org.parkerici.rawsugar.updown :as updown]
            [org.parkerici.rawsugar.ops :as ops]
            [me.raynes.fs :as fs]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.multitool.cljcore :as ju]
            [org.parkerici.rawsugar.server-utils :as wu]
            [org.parkerici.rawsugar.history :as h]
            [org.parkerici.rawsugar.request :as request]
            [org.parkerici.rawsugar.views.html :as html]
            [org.parkerici.rawsugar.views.history :as history-view]
            [org.parkerici.rawsugar.views.admin :as admin-view]
            [org.parkerici.rawsugar.views.login :as login-view]
            [org.parkerici.rawsugar.views.user-guide :as user-guide]
            [ring.logger :as logger]
            [ring.middleware.session.memory :as ring-memory]
            [ring.middleware.oauth2 :refer [wrap-oauth2]]
            [ring.middleware.gzip :refer [wrap-gzip]]
            [ring.middleware.resource :as resource]
            [taoensso.timbre :as log]
            [ring.middleware.defaults :as middleware]
            [ring.util.response :as response]
            [clojure.string :as str]
            [org.parkerici.rawsugar.config :as config]
            [environ.core :as env])
  )

(defn- parse-jwt
  [token]
  ;; Could also include header and/or verify signature
  (let [[_header payload _signature] (str/split token #"\.")]
    (wu/base64-json-> payload)))

(defn- sheet-data
  [sheet columns?]
  (let [rows (sheet/get-sheet-data sheet :column-format :keyword :files? true :row-id? true)]
    (if columns?
      (cons (sheet/sheet-columns sheet) rows)
      rows)))

(defn download-file
  [id]
  (response/file-response (blob/localize-file id)))

;;; Mostly a no-op now, transit wrapper handles the hard work
(defn content-response
  [data & [status]]
  ;; Try to return vectors for consistency. list? is false for lazy seq. Note doesn't do anything about internal lists.
  (let [data (if (and (sequential? data) (not (vector? data)))
               (into [] data)
               data)]
    {:status (or status 200)
     :headers {}
     ;; Warning: this breaks the file-upload response because it isn't under wrapper
     :body data}))

;;; should match project/files
(defn file-data
  [file-id]
  (d/pull '[* :row/_files] file-id))


(defn spa
  []
  (response/content-type
   (content-response
    (html/html-frame-spa))
   "text/html"))

(defroutes site-routes
  (GET "/" [] (spa))                    ;index handled by spa
  ;; NOT handled by SPA
  (GET "/login" [] (login-view/login-view))
  (GET "/user-guide" [] (user-guide/user-guide))
  ;; these should maybe be subviews of project
  (GET "/authenticated" req
       (let [original-page (get-in req [:cookies "rawsugar_landing" :value])]
         (response/redirect (if (empty? original-page) "/" original-page))))
  (GET "/history" [project]
       (history-view/history-view project))
  (GET "/file" [id]
       (download-file (Long. id)))
  (GET "/download-sheet" [sheet]
       (sheet/export-sheet-to-string
        (sheet/get-sheet-data (Long. sheet) :files? true :row-id? true :column-format :name)))
  (GET "/export" [file]
       (let [path (ju/base64-decode file)
             name (fs/base-name path)]
         (-> path
             response/file-response
             (response/header "content-disposition"
                              (format "attachment; filename=%s" name)))))
  (GET "/admin" req
       (admin-view/view req))
  (GET "*" [] (spa))                    ;default is handled by spa
  (route/not-found "Not found")         ;TODO this  will never be reached? But spa should do something reasonable with bad URLs
  )

(defn wrap-bind-request
  [handler]
  (fn [request]
    (binding [request/*request* request]
      (handler request))))

(defn wrap-bind-as-of
  [handler]
  (fn [request]
    (binding [request/*as-of* (u/coerce-numeric-hard (get-in request [:params :asof]))]
      (handler request))))

;;; Urls that do not require login. 
(def open-uris #{"/oauth2/google"
                 "/oauth2/google/callback"
                 "/login"
                 "/css/rawsugar2.css"        ;TODO find a way to do all static files
                 "/img/Header 1.png"
                 "/img/google-signin.png"
                 })

;;; Note: here's a lib to do somet of this, prob should use it: http://funcool.github.io/buddy-auth/latest/
;;; If oauth2 has blessed us, add token info to the request
(defn wrap-jwt
  [handler]
  (fn [request]
    (handler
     (if (open-uris (:uri request))  ; Open (allowed) URI
       request
       (if-let [token (get-in request [:oauth2/access-tokens :google :id-token])]
         (let [_expires [get-in request [:oauth2/access-tokens :google :expires]]
               claims (parse-jwt token)
               now (/ (System/currentTimeMillis) 1000)]
           (if (< (:iat claims) now (:exp claims)) 
             (assoc request :oauth2/claims claims)
             (response/redirect "/login"))) ; Token expired
         request)))))

;;; Set or bind this false to bypass oauth and use local info instead
(def ^:dynamic *oauth?* true)

(defn wrap-enforce-login
  [handler responder]
  (fn [request]
    (let [oauth-email (if *oauth?* 
                        (get-in request [:oauth2/claims :email])
                        (or (env/env :email)
                            (env/env :user)
                            (env/env :user-name)))]
      (cond (open-uris (:uri request))  ; Open (allowed) URI
            (handler request)
            oauth-email                 ; This request is supplying identity (or simulation thereof)
            (handler (assoc-in request [:login :email] oauth-email)) ; add info to request
            :else                                                    ; No id
            (responder request)         ; call the responder (which can (eg) return an error response)
            ))))

(defn wrap-oauth-off
  "Include as wrapper to disable Oauth."
  [handler]
  (fn [request]
    (binding [*oauth?* false]
      (handler request))))

;;; Weird that this isn't a standard part of ring
(defn wrap-exception-handling
  [handler]
  (fn [request]
    (try
      (handler request)
      (catch clojure.lang.ExceptionInfo e
        {:status 400 :headers {} :body (str "Error: " (ex-message e))})
      (catch Throwable e
        {:status 500 :headers {} :body (print-str e)}))))

(defn wrap-api-exception-handling
  [handler]
  (fn [request]
    (try
      (handler request)
      (catch clojure.lang.ExceptionInfo e
        {:status 400 :headers {} :body {:error (ex-message e)}})
      (catch Throwable e
        {:status 500 :headers {} :body (print-str e)}))))

;;; Weird that this isn't a standard part of ring
(defn wrap-no-read-eval
  [handler]
  (fn [request]
    (binding [*read-eval* false]
      (handler request))))

;;; Ensure API and site pages use the same store, so authentication works for API.
(def common-store (ring-memory/memory-store))

(def site-defaults
  (-> middleware/site-defaults
      (assoc-in [:security :anti-forgery] false)          ;necessary for upload (TODO not great from sec viewpoint)
      (assoc :cookies true)
      (assoc-in [:session :cookie-attrs :same-site] :lax) ;for oauth
      (assoc-in [:session :store] common-store)))

(def oauth2-params
  {:google
    {:authorize-uri    "https://accounts.google.com/o/oauth2/auth"
     :access-token-uri "https://accounts.google.com/o/oauth2/token"
     :client-id        (:oauth-client-id (config/config))
     :client-secret    (:oauth-client-secret (config/config))
     :scopes           ["https://www.googleapis.com/auth/userinfo.email"]
     :launch-uri       "/oauth2/google"
     :redirect-uri     (:oauth-callback (config/config))
     :landing-uri      "/authenticated"
     }})


;;; Note: static resources are handled by middleware, see middleware/site-defaults
(def site
  (-> site-routes
      (wrap-restful-response)
      wrap-bind-request                 ;has to come early (late) as possible to get all the wrap effects
      wrap-bind-as-of
      (wrap-enforce-login (fn [req]
                            (response/set-cookie
                             (response/redirect "/login")
                             "rawsugar_landing"
                             (:uri req) ;TODO this leaves out query params, see enflame for better way (but rawsugar does not actually use query params)
                             {:same-site :lax :path "/"}
                             )))                ;has to come before (that is, after) wrap-jwt and wrap-session
      wrap-jwt                                  ;has to come before (that is, after) wrap-oauth2
      (wrap-oauth2 oauth2-params)       
      (resource/wrap-resource "public" {:allow-symlinks? true}) ;allow symlinks in static dir
      (middleware/wrap-defaults site-defaults)                                  ;TODO turn off static thing in here
      d/wrap-datomic
      wrap-no-read-eval
      wrap-exception-handling
      (logger/wrap-with-logger          ;hook Ring logger to Timbre
       {:log-fn (fn [{:keys [level throwable message]}]
                  (log/log level throwable message))})
      ))

;; TODO not really used yet, so maybe flush
(defn with-history-reply
  [f]
  (let [start (:t d/*db*)]
    (f)
    (content-response
     (h/history-diff start))))

(defroutes api-routes  
  (context "/api/v2" []
           (GET "/schema" []
                (content-response (schema/merged-schema)))
           ;; Note: this is somewhat dangerous, and should definitely
           ;; be removed if there are ever security issues.
           (GET "/eval" [exp]
                (content-response (d/wrap-datomic-fn #(eval (read-string exp)))))
           (context "/projects" []
                    (GET "/data" [] (content-response (project/project-data))) 
                    )
           (context "/entity" []
                    ;; get an arbitrary entity (unused)
                    (GET "/get" [id]
                         (content-response (d/get-entity (u/coerce-numeric id))))
                    ;; update an arbitrary entity
                    (POST "/update" [project entity]
                          (h/update-generic project entity)))
           (context "/batch" []
                    (GET "/dashboard" [batch]
                         ;; TODO temp
                         (content-response (batches/dashboard-graph (u/coerce-numeric batch))))
                    (context "/step" []
                             ;; Was using generic update, but need special handling for step state propagation
                             (POST "/update" [project step state agent]
                                   (content-response
                                    (recipes/update-step project (u/coerce-numeric step) state agent))
                                   )))
           (context "/sheet" []
                    (GET "/data" [sheet columns]
                         (content-response (sheet-data (u/coerce-numeric sheet) (not (= columns "false")))))
                    (POST "/update" [project sheet update]
                          (with-history-reply
                            #(sheet/update-sheet project sheet update)))
                    ;; Out of service
                    ;; Needs to be changed for Sheet Provenance
                    #_
                    (POST "/add-row" [project sheet data]
                          (with-history-reply
                            #(h/transact [project :add-row sheet data]
                                         (sheet/add-row-txn sheet (read-string data))))))
           
           (context "/file" []
                    (GET "/contents" [id]
                         (download-file (Long. id)))
                    ;; This might more logically be /batch/files
                    (GET "/list" [batch] (content-response (batches/batch-files (u/coerce-numeric batch))))
                    (POST "/upload" request
                          ;; TODO might want a more structured response? Whole file entity?
                          (content-response
                           (updown/upload-api request)))
                    
                    )
           (context "/ops" []
                    (GET "/list" [] (content-response (ops/get-ops)))
                    (POST "/do" request
                          ;; Always does a full update of current sheet, so no need for contentful response
                          (content-response
                           (ops/do-op (:params request)))
                          ))
           (route/not-found (content-response {:error "Not Found"}))
           ))

(def api-defaults
  (-> middleware/api-defaults
      (assoc :cookies true)
      (assoc-in [:session :flash] false)
      (assoc-in [:params :multipart] true)                ;to support file uploads
      (assoc-in [:session :cookie-attrs] {:http-only true, :same-site :lax})
      (assoc-in [:session :store] common-store)))

;;; Must be something built-in for this?
(defn wrap-filter
  [handler path]
  (make-route nil path handler))

;;; Code is what the user copies off the Google Sign in page
;;; Memoization makes this sticky so user only has to authenticate once.
(u/defn-memoized validate-oauth-code
  [code]
  (:body
   (client/post
    "https://www.googleapis.com/oauth2/v4/token"
    {:as :json
     :form-params {:code code
                   :redirect_uri "urn:ietf:wg:oauth:2.0:oob"
                   :client_id (:desktop-oauth-client-id (config/config))
                   :client_secret (:desktop-oauth-client-secret (config/config))
                   :scope ""
                   :grant_type "authorization_code" }})))

(defn wrap-oauth-code
  [handler]
  (fn [request]
    (let [code (get-in request [:params :oauth-code])
          token (and code (validate-oauth-code code))]
      (handler
       (if token
         (assoc-in request [:oauth2/access-tokens :google]
                   {:token (:access_token token)
                    :id-token (:id_token token)
                    ;; :expires ...
                    })
         request)))))

(def rest-api
  (-> api-routes
      wrap-bind-request                 ;not currently needed except for debugging
      wrap-bind-as-of
      (wrap-enforce-login (fn [_] (content-response {:error "Unauthorized"} 401)))
      wrap-jwt                          ;has to come before (that is, after) wrap-oauth2
      (wrap-oauth2 oauth2-params)       
      wrap-oauth-code
      (middleware/wrap-defaults api-defaults)
      wrap-no-read-eval
      d/wrap-datomic
      wrap-api-exception-handling
      (logger/wrap-with-logger          ;hook Ring logger to Timbre
       {:log-fn (fn [{:keys [level throwable message]}]
                  (log/log level throwable message))})
      (wrap-restful-format)
      wrap-gzip
      (wrap-filter "/api/*")            ;filter early so edn responses don't go to regular site requests
      ))

(def app
  (routes rest-api site))

(def insecure-app
  (wrap-oauth-off app))
