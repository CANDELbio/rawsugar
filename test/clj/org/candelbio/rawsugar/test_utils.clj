(ns org.candelbio.rawsugar.test-utils
  (:require [clojure.test :refer :all]
            [me.raynes.fs :as fs]
            [mock-clj.core :as mc]
            [org.candelbio.rawsugar.datomic :as d]
            [org.candelbio.rawsugar.config :as c]
            [org.candelbio.rawsugar.login :as login]
            [org.candelbio.rawsugar.projects :as projects]
            [org.candelbio.rawsugar.batches :as batches]
            [org.candelbio.rawsugar.sheet :as sheets]
            [org.candelbio.rawsugar.cli :as cli]
            [org.candelbio.rawsugar.updown :as updown]
            [environ.core :as env]
            [taoensso.timbre :as log]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(log/set-level! :debug)

;;; Sanity checking
(log/infof "General config: %s" (c/config))
(log/infof "Datomic config: %s" d/config)

(defmacro capture-output
  "Returns a seq of output lines"
  [form]
  `(try (remove empty? (str/split (with-out-str ~form) #"\n"))
        (catch Exception e#
          (prn "error within capture-output" e#))))

(defn synthetic-project-name
  []
  (let [tentative (str "proj" (rand 10000))]
    (if (projects/exists? tentative)
      (synthetic-project-name)
      tentative)))

(defn synthetic-batch-name
  []
  (name (gensym "batch")))

(defn synthetic-sheet-name
  []
  (name (gensym "sheet")))

(defmacro with-project-name
  [projvar & body]
  `(with-datomic-context
     (let [~projvar (synthetic-project-name)]
       ~@body)))

(defmacro with-datomic-context
  [& body]
  `(d/wrap-datomic-fn (fn [] ~@body)))

;;; TODO these should probably be more widely used

(defmacro with-txn-results
  "Do the txn, execute body with the results bound to results-var, then reverse the txn"  
  [txn results-var & body]
  `(let [~results-var (d/transact ~txn)]
     (with-datomic-context
       ~@body
       (d/transact (d/invert-tx-data (:tx-data ~results-var))))))

(defmacro with-txn
  "Do the txn, execute body with the results, then reverse the txn"
  [txn & body]
  (let [txn-results# (symbol "foo")]
    `(with-txn-results ~txn ~txn-results# ~@body)))

(defmacro with-txn-sheet
  "Do the txn, bind sheet-var to the newly created sheet, and execute body; then reverse the txn"
  [txn sheet-var & body]
  (let [txn-results# (gensym "txnres")]
    `(with-txn-results ~txn ~txn-results#
       (let [~sheet-var (get-in ~txn-results# [:tempids "new_obj_sheet"])]
         ~@body))))

(defmacro with-project
  "Bind projvar to eid of a new project, and <projvar>-name to its name"
  [projvar & body]
  (let [projname (symbol (str projvar "-name"))]
    `(with-datomic-context
       (login/login! "tester")
       (let [~projname (synthetic-project-name)
             ~projvar (projects/add ~projname)]
         (with-datomic-context
           (try
             ~@body
             (finally
               (projects/delete ~projvar))))))))

;;; For use in debugging
;;; like with-project, but prints the project and leaves it around for autopsy
;;; Should not be called from checked-in code
(defmacro with-project-debug
  [projvar & body]
  (let [projname# (symbol (str projvar "-name"))]
    `(with-datomic-context
       (login/login! "tester")
       (let [~projname# (synthetic-project-name)
             ~projvar (projects/add ~projname#)]
         (prn :project ~projvar ~projname#)
         (with-datomic-context
           (try
             ~@body))))))

(defmacro with-batch
  [projvar batchvar & body]
  (let [batchname (symbol (str batchvar "-name"))]
    `(with-datomic-context
       (let [~batchname (synthetic-batch-name)
             ~batchvar (batches/add ~projvar ~batchname)]
         ~@body
         ))))

;;; TODO should use with-batch
(defmacro with-project-batch
  [projvar batchvar & body]
  "See with-project, does the same thing but adds a batch"
  (let [batchname (symbol (str batchvar "-name"))]
    `(with-project ~projvar
       (let [~batchname (synthetic-batch-name)
             ~batchvar (batches/add ~projvar ~batchname)]
         (with-datomic-context
           ~@body
           )))))

(defmacro with-project-batch-debug
  [projvar batchvar & body]
  (let [batchname# (symbol (str batchvar "-name"))]
    `(with-project-debug ~projvar
       (let [~batchname# (synthetic-batch-name)
             ~batchvar (batches/add ~projvar ~batchname#)]
         (prn :batch ~batchvar ~batchname#)
         (with-datomic-context
           ~@body
           )))))

(defmacro with-project-batch-sheet
  [projvar batchvar sheetvar & body]
  "See with-project, does the same thing but adds a batch and sheet"
  (let [sheetname (symbol (str sheetvar "-name"))]
    `(with-project-batch ~projvar ~batchvar
       (let [~sheetname (synthetic-sheet-name)
             ~sheetvar (sheets/create-blank ~projvar ~batchvar ~sheetname)]
         (with-datomic-context
           ~@body)))))


(defn temp-file-path
  [& [suffix]]
  (str (fs/temp-file "rawsugar_test" (or suffix ""))))

(defn temp-dir-path
  []
  (str (fs/temp-dir "rawsugar_test")))

(defn file-exists?
  [f]
  (fs/exists? f))

(defn file-lines-out
  [file seq]
  (let [w (io/writer file)]
    (binding [*out* w]
      (doseq [l seq]
        (println l)))))

(defmacro with-log
  [& body]
  `(mc/with-mock [log/-log! nil]
     ~@body
     (map (fn [[_config# level# _ns-str# _file# _line# _msg-type# _err# vargs# _base-data# _callsite-id#]]
            {:level level# :message (apply str @vargs#)}
            )
          (mc/calls log/-log!))))

(defmacro with-log-test
  "Tests that the execution of body produces a log message of the specified severity and matching regex."
  [[severity regex] & body]
  `(is (some (fn [entry#]
               (and (= (:level entry#) ~severity)
                    (re-find ~regex (:message entry#))))
             (with-log ~@body))
       ~(format "No log matching %s %s found" severity (prn-str regex))))

;;; Not used, mainly-no-logging is more concise
(defmacro without-logging
  [& body]
  `(log/with-level :report
     ~@body))

(defn set= [a b]
  (= (set a) (set b)))

;;; New regime

(def test-project-name "test-fixtures-")

(defn test-project
  []
  (cli/resolve-project (str test-project-name (:rawsugar-version env/env)) :new-ok))

(defn test-batch
  [batch-or-name]
  (if (number? batch-or-name)
    batch-or-name
    (cli/resolve-batch (test-project) batch-or-name :new-ok)))

(defn test-sheet
  [batch-or-name path]
  (or (cli/resolve-sheet (test-project) (test-batch batch-or-name) path :ok) 
      (first                            ; assuming a single sheet
       (updown/upload-sheets (test-project) (test-batch batch-or-name) path {:sheet-name path}))))

;;; TODO This is going to need more thinking with new system, esp if upload-sheet-and-files makes two sheets
(defn test-sheet-files
  [batch-or-name path files]
  (let [project (test-project)
        org-sheet-name (str path "-" files) ;ugly but unique at least
        match-sheet-name (str org-sheet-name  "-matched")] 
    (or (cli/resolve-sheet project (test-batch batch-or-name) match-sheet-name :ok) 
        (first                            ; assuming a single sheet
         (updown/upload-sheet-and-files project (test-batch batch-or-name) path files {:sheet-name org-sheet-name :match-sheet-name match-sheet-name})))))

;;; hey guess what this was a REALLY REALLY BAD IDEA
#_
(defmacro with-project-batch-new
  [[projvar batchvar & [batchname]] & body]
  "New: Like old one, but reuses project/batch/sheets where it can"
  `(with-datomic-context
     (let [~projvar test-project-name
           ~batchvar (test-batch ~(or batchname "generic"))] ;TODO separate batches for separate test files
       (try
         ~@body
         ;; Delete al the derived sheets in the batch (keeping the uploads)
         (finally (batches/delete-derived-sheets ~batchvar))))))

;;; Reduce to old style
(defmacro with-project-batch-new
  [[projvar batchvar & [batchname]] & body]
  `(with-project-batch ~projvar ~batchvar
     ~@body))

(defn transact-to-new-sheet
  [txn]
  (let [{:keys [tempids]}
        (d/transact txn)]
    (get tempids "new_obj_sheet")))
