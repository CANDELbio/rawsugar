; IAP stands for Identity-Aware Proxy.
; IAP allows us to access applications running on GCP and protected by IAP https://cloud.google.com/iap/docs/concepts-overview.
; Ported from the Java example on https://cloud.google.com/iap/docs/authentication-howto#iap_make_request-java
; and configured with the instructions at
; https://medium.com/google-cloud/using-airflow-experimental-rest-api-on-google-cloud-platform-cloud-composer-and-iap-9bd0260f095a
(ns org.candelbio.rawsugar.glue.google.iap
  (:import [com.google.api.client.auth.oauth2 Credential]
           [com.google.auth.oauth2 GoogleCredentials ServiceAccountCredentials]
           [com.google.api.client.util GenericData]
           [com.google.api.client.http UrlEncodedContent GenericUrl]
           [com.google.api.client.http.javanet NetHttpTransport]
           [com.google.api.client.json JsonObjectParser]
           [com.google.api.client.json.jackson2 JacksonFactory]
           [java.time Instant Clock]
           [java.util Date]
           [com.nimbusds.jose JWSAlgorithm JWSHeader JWSHeader$Builder]
           [com.nimbusds.jose.crypto RSASSASigner]
           [com.nimbusds.jwt SignedJWT JWTClaimsSet JWTClaimsSet$Builder])
  (:require [clj-http.client :as client]
            [environ.core :as env]
            [clojure.java [io :as io]]))

(def iam-scope "https://www.googleapis.com/auth/iam")
(def oauth-token-uri "https://www.googleapis.com/oauth2/v4/token")
(def jwt-bearer-token-grant-type "urn:ietf:params:oauth:grant-type:jwt-bearer")
(def expiration-time-in-seconds 3600)

(def service-account-credentials (env/env :google-airflow-dag-credentials))

; If no path is passed in, gets the application default credentials on the system.
; Otherwise gets the credentials for the credentials path passed in.
(defn get-credentials
  [credentials-path]
  (let [unscoped-credentials (if (nil? credentials-path)
                               (GoogleCredentials/getApplicationDefault)
                               (GoogleCredentials/fromStream (io/input-stream credentials-path)))]
    (.createScoped unscoped-credentials [iam-scope])))

; Gets the credentials for the application default/current system service account
(defn get-service-account-credentials
  ([credentials-path]
  (let [credentials (get-credentials credentials-path)]
    (if (or (nil? credentials) (not (instance? ServiceAccountCredentials credentials)))
      (throw (Exception. "Google credentials : service accounts credentials expected"))
      credentials)))
  ([]
   (get-service-account-credentials nil)))

(defn get-signed-jwt
  [credentials iap-client-id]
  (let [now (Instant/now (Clock/systemUTC))
        expiration-time (+ (.getEpochSecond now) expiration-time-in-seconds)
        jws-header (-> (new JWSHeader$Builder (JWSAlgorithm/RS256))
                       (.keyID (.getPrivateKeyId credentials))
                       (.build))
        claims (-> (new JWTClaimsSet$Builder)
                   (.audience oauth-token-uri)
                   (.issuer (.getClientEmail credentials))
                   (.subject (.getClientEmail credentials))
                   (.issueTime (Date/from now))
                   (.expirationTime (Date/from (Instant/ofEpochSecond expiration-time)))
                   (.claim "target_audience" iap-client-id)
                   (.build))
        signer (new RSASSASigner (.getPrivateKey credentials))
        signed-jwt (new SignedJWT jws-header claims)]
    (.sign signed-jwt signer)
    (.serialize signed-jwt)))

(defn get-google-id-token
  [jwt]
  (let [token-request (-> (new GenericData)
                          (.set "grant_type" jwt-bearer-token-grant-type)
                          (.set "assertion" jwt))
        content (new UrlEncodedContent token-request)
        http-transport (new NetHttpTransport)]
    (-> (.createRequestFactory http-transport)
        (.buildPostRequest (new GenericUrl oauth-token-uri) content)
        (.setParser (new JsonObjectParser (JacksonFactory/getDefaultInstance)))
        (.execute)
        (.parseAs GenericData)
        (.get "id_token"))))

(defn iap-authorization-headers
  [iap-client-id]
    (let [credentials (get-service-account-credentials service-account-credentials)
          jwt (get-signed-jwt credentials iap-client-id)
          id-token (get-google-id-token jwt)]
      {"Authorization" (str "Bearer " id-token)}))

; Get the iap-client-id for your project by visiting the OAuth page you want to use the iap with
; in an incognito window. You should be redirected to a Google authorization page that will have
; the iap-client-id as the client_id argument in the URL.
; Example usage:
; (iap-get "https://y2767f87a6d85ff8a-tp.appspot.com/api/experimental/test" integrators-prod-iap-client-id)
(defn iap-get
  [url iap-client-id]
    (client/get url {:headers (iap-authorization-headers iap-client-id) :throw-entire-message? true}))

(defn iap-post
  [url iap-client-id body]
  (client/post url {:headers (iap-authorization-headers iap-client-id) :form-params body :content-type :json :throw-entire-message? true}))
