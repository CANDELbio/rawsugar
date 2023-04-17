(ns org.candelbio.rawsugar.api-test
  (:require [ajax.core :as client]
            [clojure.test :refer :all]
            [org.candelbio.rawsugar.updown :as updown]
            [org.candelbio.rawsugar.sheet :as sheet]
            [org.candelbio.rawsugar.server :as server]
            [org.candelbio.rawsugar.test-utils :as tu]
            [org.candelbio.multitool.core :as u])
  (:import clojure.lang.ExceptionInfo))

(def test-port 1222)

(def base-api "/api/v2")

(defn api-url [url]
  (str "http://localhost:" test-port base-api url))

(defn with-server [f]
  ;; Test with authentication off
  (server/start-insecure test-port)
  (f)
  (server/stop))

(use-fixtures :once with-server)

(defn api-do [func url params extra]
  (let [p (promise)]
    (func (api-url url)
          (merge {:params params
                  :handler (fn [resp] (deliver p resp))
                  :error-handler (fn [resp] (deliver p {:error resp}))}
                 extra))
    (let [resp @p]
      (when (:error resp)
        (throw (ex-info (str "API error: " url) {:resp resp})))
      resp)))

(defn api-get [url params]
  (api-do client/GET url params {}))

(defn api-get-json [url params]
  (api-do client/GET url params {:format :json}))

(defn api-post [url params]
  (api-do client/POST url params {}))

(deftest projects-data-test
  (tu/with-project-batch project batch
    ;; TODO better way to do this in tests, maybe just once since we use it all over the place
    (updown/upload project batch "test/resources/proj0/meta.tsv" "test/resources/proj0/files/" {})
    (let [all-project-data (api-get "/projects/data" {})
          this-project-data (u/some-thing #(= project (:db/id %)) all-project-data)]
      (is (= project (:db/id this-project-data)))
      (is (= project-name (:project/name this-project-data)))
      (is (= :upload-file (get-in this-project-data [:project/last-op :operation/cmd])))

      (let [batch-ent (u/some-thing #(= batch (:db/id %)) (:project/batches this-project-data))]
        (is batch-ent)
        (is (= batch-name (:batch/name batch-ent))))
      )))

;;; TODO test for 404 status
(deftest sheet-api-error-handling
  (is (thrown-with-msg?
       ExceptionInfo #"API error"
       (api-get "/list-sheets" {:project "not a project"}))))

;; TODO all the other APIs, especially POSTs

;;; Out of service
#_
(deftest update-cell-api
  (tu/with-project-batch project batch
    (updown/upload-sheet-and-files project batch "test/resources/proj0/meta.tsv" "test/resources/proj0/files/" {})
    (tu/with-datomic-context
      (let [sheet (sheet/lookup-sheet batch "meta")
            [col-data & sheet-data] (api-get "/sheet/data" {:sheet sheet})
            row-id (:row-id (first sheet-data))
            col-id (some #(and (= (:column/name %) "a") (:db/id %)) col-data)
            new-value 23]
        ;; TODO this fails nondeterministally
        (api-post "/sheet/cell/update" {:project project :sheet sheet :row row-id :column col-id :value new-value})
        (let [[_ & new-sheet-data] (api-get "/sheet/data" {:sheet sheet})]
          (is (= new-value (get (first new-sheet-data) :a))))))))

(deftest update-entity-api
  (tu/with-project-batch project batch
    (updown/upload-sheet-and-files project batch "test/resources/proj0/meta.tsv" "test/resources/proj0/files/" {})
    (tu/with-datomic-context
      (let [sheet (sheet/lookup-sheet batch "meta")
            col-data (first (api-get "/sheet/data" {:sheet sheet}))
            column (some #(and (= (:column/name %) "a") %) col-data)
            new-column (assoc column :column/name "dobbs")]
        (api-post "/entity/update" {:entity new-column})
        (let [new-col-data (first (api-get "/sheet/data" {:sheet sheet}))]
          (is (some #(= (:column/name %) "dobbs") new-col-data)))))))


;;; TODO
;;; non-api
;; /history
;; /file
;; /download-sheet
;; ;;; API
;; /file (everything)
;; /update

