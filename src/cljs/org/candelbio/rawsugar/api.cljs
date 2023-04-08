(ns org.candelbio.rawsugar.api
  (:require [re-frame.core :as rf]
            [ajax.core :as ajax]
            [org.candelbio.rawsugar.asof :as asof]
            ))

(def as-of (atom nil))

;;; API utils
;;; Note: transit decoding happens magically

(defn standard-ajax-options
  [uri params]
  {:error-handler #(rf/dispatch [::bad-response-for uri params %1])
   })

(def api-prefix "/api/v2")

(defn ajax-get
  [uri options]
  (ajax/GET uri
            (merge (standard-ajax-options uri (:params options)) options)))

(defn ajax-post
  [uri options]
  (ajax/POST uri
             (merge (standard-ajax-options uri (:params options)) options)))

(defn api-get
  [uri options]
  (let [options (if-let [as-of @asof/as-of]
                  (assoc-in options [:params :asof] as-of)
                  options)]
    (ajax-get (str api-prefix uri) options)))

(defn api-post
  [uri options]
  (let [options (assoc-in options [:params :asof] @asof/as-of)]
    (ajax-post (str api-prefix uri) options)))

(rf/reg-event-fx
 ::bad-response
 (fn [_ [_ {:keys [status response]}]]
   (if (= status 401)
     {:dispatch [:non-spa-navigate "/login"]}
     {:dispatch [:error (str status ": " (print-str response))]})))

(rf/reg-event-fx
 ::bad-response-for
 (fn [_ [_ uri params {:keys [status response]}]]
   (if (= status 401)
     {:dispatch [:non-spa-navigate "/login"]}
     {:dispatch [:error (str status ": " (print-str response)
                             " for " uri " " params)]})))

;;; Higher level but still generic API stuff



;;; do an eval on the back end, and produce the result
;;; Result is cached, which could cause problems given mutability
;;; TODO (idea: record the current datomic txn, so cache gets flushed on any changes)
;;; Also could conceivably eat memory, so keep values small.
(rf/reg-sub
 :eval
 (fn [db [_ exp]]
   (or (get-in db [:eval-cache exp])
       (do
         (api-get "/eval"
                  {:params {:exp (str exp)}
                   :handler (fn [response]
                              (rf/dispatch [:eval-result exp response]))})
         nil))))

;;; This is when it's more for side-effects than value, which is probably most of the time!
(rf/reg-event-fx
 :eval
 (fn [_ [_ exp continuation]]
   (api-get "/eval"
            {:params {:exp (str exp)}
             :handler (fn [response]
                        (when continuation
                          (continuation response)))})
   {}))

(rf/reg-event-db
 :eval-result
 (fn [db [_ exp result]]
   (assoc-in db [:eval-cache exp] result)))

