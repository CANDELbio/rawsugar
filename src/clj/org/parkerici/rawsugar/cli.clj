(ns org.parkerici.rawsugar.cli
  (:gen-class)
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [environ.core :as env]
            [me.raynes.fs :as fs]
            [org.parkerici.rawsugar.config :as config]
            [org.parkerici.rawsugar.blob :as blob]
            [org.parkerici.rawsugar.datomic :as d]
            [org.parkerici.rawsugar.history :as h]
            [org.parkerici.rawsugar.login :as login]
            [org.parkerici.rawsugar.projects :as project]
            [org.parkerici.rawsugar.batches :as batches]
            [org.parkerici.rawsugar.files :as files]
            [org.parkerici.rawsugar.schema :as schema]
            [org.parkerici.rawsugar.server :as server]
            [org.parkerici.rawsugar.sheet :as sheet]
            [org.parkerici.rawsugar.updown :as updown]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.multitool.cljcore :as ju]
            [org.parkerici.rawsugar.glue.cellengine :as ce]
            [taoensso.timbre :as log]
            [trptcolin.versioneer.core :as version]
            #_ [clojure.tools.nrepl.server :as serv]
            )
  (:import clojure.lang.ExceptionInfo))

;;; TODO mutating CLI ops should go through the ops mechanism.
;;; And provenance!

(def default-port 1777)

;;; To bypass queries, eval: (autorespond! \y)
;;; Env forces chars to strings, this forces it back
(def autorespond? (atom (first (:autorespond? env/env))))

(defn autorespond!
  [char]
  (reset! autorespond? char))

(defn ask-user
  [prompt responses]
  (or @autorespond?
      (do
        (println prompt responses)
        (let [c (first (read-line))]
          (if (get (set responses) c)
            c
            (throw (ex-info "Invalid response"
                      {:type :user-input})))
          ))))

(defn confirm
  [prompt]
  (or (= \y (ask-user prompt [\y \n]))
      (throw (ex-info "Aborted"
                      {:type :user-abort}))))
 

;;; NOTE: commands that mutate the database should all go through history/transact, which
;;; will enforce login

;;; TODO these really should be command specific.
(def cli-options
  [
   ["-s" "--strings" "Store all values as strings (by default numbers will be parsed)"] 
   ["-r" "--resume" "Resume an upload that was previously interrupted"]
   ["-f" "--filter FILTER" "Limit results to rows matching the FILTER string (eg 'tissue=blood')"]
   ["-t" "--template TEMPLATE" "Template string for naming downloaded files"]
   ["-d" "--damp-run" "For uploads, do everything but actually transfer blob contents"]
   ["-l" "--flatten" "For downloads, flatten directory structure"]
   ;; No tab/comma flag...
   ;; These are global
   ["-y" "--yes" "Assume yes responses to any user prompts."]
   ["-v" "--verbose" "Print extra information (can be repeated for even more)"
    :default 0
    :assoc-fn (fn [m k _] (update-in m [k] inc))]
   ["-q" "--quiet" "Print less information"]
   ])

;;; Options that are always legal
(def global-options #{:verbose :quiet :yes})

;;; ⩏⩎⩎ Utilities ⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏

(defn filter-to-map
  "Translate filter command-line argument into a usable map"
  [s]
  ;; parse into clauses of param=val, where val can be simple or a bracketed list. 
  (when s
    (let [clauses (re-seq #"([\w_-]+)=([\w_-]+|\[[[\w_-], ]+\])" s)]
      (when (and (empty? clauses)
                 (not (empty? s)))
        (throw (ex-info "Filter arg format is <param1>=<val1>,<param2>=<val2>..."
                        {:type :user-input})))
      (into {}
            (map (fn [[_ param val]]
                   [param
                    (if (= \[ (get val 0))
                      ;; If a bracketed list, parse into vector
                      (map u/coerce-numeric (str/split (subs val 1 (- (count val) 1)) #"\s*\,\s*"))
                      (u/coerce-numeric val))])
                 clauses)))))

(defmulti command (fn [[command & _args] _options _summary] command))

(defn check-options
  [options allowed]
  (let [bad (set/difference (set (keys options)) (set/union allowed global-options))]
    (when-not (empty? bad)
      (throw (ex-info (str "Invalid options: " bad)
                    {:type :user-input
                     :options bad}
                    )))))

;;; These fns are used for resolving names (in cli inpute) into datomic eids.

;;; not-found is optional, specifies what to do if the item is not found
;;; nil - error
;;; :ok - missing is ok
;;; :new - must be new
;;; :new-ok ask user if should be created


(defn resolve-project
  [project-name & [not-found]]
  (if project-name
    (let [project (project/lookup project-name true)]
      (if project
        (if (= not-found :new)
          (throw (d/hex-info "Project already exists" {:type :user-input :project project :name project-name}))
          project)
        (case not-found
          (nil :ok) (throw (d/hex-info "Project not found" {:type :user-input :project
                                                      project-name}))
          :new (project/add project-name)
          :new-ok
          (if (confirm (format "Project %s doesn't exist, create it?" project-name))
            (project/add project-name)
            (throw (ex-info "Not confirmed" {:type :user-input}))))))
    ;; project name not supplied
    (if (= not-found :ok)
      nil
      (throw (ex-info "Project not specified" {:type :user-input})))))

(defn resolve-batch
  [project batch-name & [not-found]]
  (if batch-name 
    (let [batch (batches/lookup-batch project batch-name)]
      (if batch
        (if (= not-found :new)
          (throw (d/hex-info "Batch already exists" {:type :user-input :project project :batch batch :name batch-name}))
          batch)
        ;; Not found
        (case not-found
          (nil :ok) (throw (d/hex-info "Batch not found" {:type :user-input :project project :batch batch-name}))
          :new (batches/add project batch-name)
          :new-ok
          (if (confirm (format "Batch %s doesn't exist, create it?" batch-name))
            (batches/add project batch-name)
            (throw (ex-info "Not confirmed" {:type :user-input}))))))
    ;; batch name missing
    (if (= not-found :ok)
      nil
      (throw (d/hex-info "Batch not specified" {:type :user-input :project project})))))

(defn resolve-sheet
  [project batch sheet-name & [not-found]]
  (if sheet-name
    (or (and batch (sheet/lookup-sheet batch sheet-name))
        (case not-found
          :ok nil
          nil (throw (d/hex-info "Sheet not found" {:type :user-input :project project :batch batch :sheet sheet-name}))
          :new (sheet/create-blank project batch sheet-name)
          :new-ok
          (if (confirm (format "Sheet %s doesn't exist, create it?" sheet-name))
            (sheet/create-blank project batch sheet-name)
            (throw (ex-info "Not confirmed" {:type :user-input})))))
    (if (= not-found :ok)
      nil
      (throw (d/hex-info "Sheet not specified" {:type :user-input :project project :batch batch})))))

(defn resolve-project-batch
  [project-name batch-name & [not-found]]
  ;; TODO :new → :new-ok has to be done elsewhere as well
  (let [project (resolve-project project-name (if (= not-found :new) :new-ok not-found))
        batch (resolve-batch project batch-name not-found)]
    [project batch]))

(defn resolve-project-batch-sheet
  [project-name batch-name sheet-name & [not-found]]
  (let [project (resolve-project project-name not-found)
        batch (resolve-batch project batch-name not-found)
        sheet (resolve-sheet project batch sheet-name not-found)]
    [project batch sheet]))

(defn resolve-column [sheet col-name]
  (or (sheet/lookup-column sheet col-name)
      (throw (d/hex-info "Column not found" {:sheet sheet :column col-name}))))

(defn resolve-pathname
  [path]
  (assert path "Pathname not specified")
  (-> path
      fs/expand-home
      str))

(defn seq-out
  "Utility for outputing a sequence as separate lines."
  [seq]
  (doseq [elt (sort seq)]
    (println elt)))

;;; ⩏⩎⩎ Commands ⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏⩎⩏

(def commands (atom (sorted-map)))

(defn print-command-doc
  [command]
  (let [{:keys [args options doc]} (get @commands command)]
    (println (format "%s %s %s"
                     command
                     (str/join " " (map str/upper-case args))
                     (str/join " " (map #(format "--%s" %) options))))
    (when doc
      (println doc))))

(defn print-doc
  []
  (println "")
  (println "Commands:")
  (println "")
  (doseq [command (keys @commands)]
    (print-command-doc command)
    (println "")))

(defmacro defcommand [name args allowed-options & body]
  `(do
     (swap! commands assoc ~name '{:args ~args :options ~allowed-options})
     (defmethod command ~name
       [[~(u/de-ns '_) ~@args] options# ~(u/de-ns '_)]
       (check-options options# ~(set (map keyword allowed-options)))
       (let [{:keys ~(u/de-ns allowed-options)} options#]
         ~@body))))

(defn doc-command
  [name doc]
  (swap! commands assoc-in [name :doc] doc))

(defcommand "login" [username] []
  (when-not username
    (throw (ex-info "User name not specified"
                    {:type :user-input})))
  (login/login! username)
  (log/infof "Logged in as %s" username))

(doc-command "login" "Login as USERNAME (use your email, currently no actual credential checking happens)")

(defcommand "history" [project] []
  (let [project (resolve-project project :ok)]
    (h/humanize (h/history :project project) (not project))))

(doc-command "history" "Print out project history (or all history entries if project is omitted)")

(defcommand "list-projects" [] []
  (seq-out (map :project/name (project/projects))))

(doc-command "list-projects" "Lists the project names.")

(defcommand "list-batches" [project] []
  (let [project (resolve-project project)]
    (seq-out (project/project-batch-names project))))

(doc-command "list-batches" "Lists the batches of a  project.")

(defcommand "list-sheets" [project batch] []
  (let [[project_ batch] (resolve-project-batch project batch)]
    (seq-out (batches/batch-sheet-names batch))))

(doc-command "list-sheets" "Lists the sheets of a  project.")

(defcommand "list-columns" [project batch sheet] []
  (let [[project_ batch_ sheet] (resolve-project-batch-sheet project batch sheet)]
    (seq-out (sheet/sheet-column-names sheet))))

(doc-command "list-columns" "List the columns of the given sheet.")

(defcommand "list-column-values" [project batch sheet column] [filter]
  (let [[project_ batch_ sheet] (resolve-project-batch-sheet project batch sheet)
        column (resolve-column sheet column)]
    ;; TODO: values can be of mixed type, which can cause an error on sort
    (seq-out
     (sheet/sheet-column-values sheet column (filter-to-map filter)))))

(doc-command "list-column-values" "Lists the distinct values found in the given column.")

(defn files
  "Helper for list-files and download. Sheet and batch are optional"
  [project batch sheet filter]
  (cond sheet
        (sheet/sheet-files sheet (filter-to-map filter))
        batch
        (batches/batch-files batch)
        :else
        (project/project-files project)))

(defcommand "list-files" [project batch sheet] [filter verbose]
  (let [[project batch sheet] (resolve-project-batch-sheet project batch sheet :ok)
        files (files project batch sheet filter)]
    (if (> verbose 0)
      (doseq [file files]
        (println (:file/pathname file))
        (doseq [{:keys [location time txn]} (files/file-versions (:db/id file))]
          (println "  " (str time) "  " location "  " txn)))
      (seq-out
       (map :file/pathname files)))))

(doc-command "list-files" "(BATCH and SHEET are optional) List files associated with a batch or a particular sheet. With VERBOSE flag, include file details and versions")

(defcommand "add-project" [project] []
  (resolve-project project :new)
  (project/add project)
  (log/info "Created."))

(doc-command "add-project" "Create a new project. Usually this is unnecessary; projects will be created on demand.")

;;; This command atypically has flexible arguments
;;; I guess this particular command doesn't need to go through
;;; Ops...it has no product.
(defcommand "delete" [project-name batch-name sheet-name] []
  (let [project (resolve-project project-name)
        batch (and batch-name (resolve-batch project batch-name))
        sheet (and batch sheet-name (resolve-sheet project batch sheet-name))]
    (if batch
      (if sheet
        ;; delete sheet
        (do
          (confirm (format "Delete sheet %s and its derivatives from project %s?" sheet-name project-name)) ;TODO should be smart and warn about derivatives
          (batches/delete-sheet-and-deriveds batch sheet false)
          (log/infof "Sheet %s deleted." sheet))
        ;; delete batch
        (do
          (confirm (format "Delete batch %s from project %s?" batch-name project-name))
          (batches/delete project batch)
          (log/infof "Batch %s deleted." batch-name))
          )
      ;; delete project
      (do
        (confirm (format "Delete project %s?" project-name))
        (project/delete project)
        (log/infof "Project %s deleted." project-name)
        ))))

(doc-command "delete" "(BATCH and SHEET are optional) Delete the given project, batch, or sheet.")

(defcommand "add-batch" [project batch] []
  ;; resolve-project-batch actually does the creation
  (resolve-project-batch project batch :new)
  (log/info "Created."))

(doc-command "add-batch" "Create a new batch.")

(defn complete
  []
  (log/info "Upload complete."))

;;; Needs to have a sheet name option I guess, can default to file name
;;; How to deal with data files? Maybe have a column (specified as option or defulat to file) in a particualr sheet
;;; HHH TODO isn't this the same as upload without a dir arg?
(defcommand "upload-sheets" [project batch file] [strings]
  (let [[project batch] (resolve-project-batch project batch :new-ok)
        file (resolve-pathname file)]
    (blob/check-path file)
    (updown/upload-sheets project batch file {:strings strings})) ;TODO other options passed through?
  (complete))

(doc-command "upload-sheets" "Upload the given spreadsheet file to PROJECT.")

(defcommand "upload-files" [project batch directory sheet column] [resume damp-run]
  (let [[project batch] (resolve-project-batch project batch :new-ok)
        sheet (when sheet (resolve-sheet project batch sheet :new-ok))
        directory (resolve-pathname directory)]
    (updown/upload-and-match project batch sheet directory column {:resume resume :damp-run damp-run})
    (complete)))

;;; TODO+ not sure this is still accurate – also, should make column requirement, full sheet matching is too $$$
;;; Can sheet be nil?
(doc-command "upload-files" "Upload files to a project/batch, if SHEET is supplied, try to match them with the rows of the sheet using fuzzy matching of column values to filenames.")

;;; TODO
;;; match-files project batch sheet [columns] [file-pattern] [threshold]


;;; Combined - directory is optional
(defcommand "upload" [project batch sheet-file directory] [strings resume]
  (let [sheet-file (resolve-pathname sheet-file)
        directory (and directory (resolve-pathname directory))]
    (blob/check-path sheet-file)
    (when directory (blob/check-path directory))
    (let [[project batch] (resolve-project-batch project batch)]
      (updown/upload-sheet-and-files project batch sheet-file directory {})
      (complete))))

(doc-command "upload" "Upload the given spreadsheet file to PROJECT. If any of the sheets have a fileName column and DIRECTORY is specified, upload and link the associated files. DIRECTORY can be a local file path or a Google Storage address.")

(defcommand "update" [project batch sheet file] []
  (let [[project batch sheet] (resolve-project-batch-sheet project batch sheet)
        file (resolve-pathname file)]
    (blob/check-path file)
    (let [new-sheet (updown/update-sheet-from-file project batch sheet file)]
      (d/wrap-datomic-fn
       #(do (log/info "New sheet created:" (:sheet/name (d/get-entity new-sheet)))
            (complete))))))

(doc-command "update" "Update the given sheet based on FILE. File must contain a single sheet with a row-id column, which is matched against the existing sheet rows.")

;;; TODO this logic duplicated in export/export-to-zip, may belong in updown
(defn do-download
  [project batch sheet directory {:keys [filter template sheet? files?] :as options}]
  (check-options options #{:filter :template :files? :sheet? :flatten?})
  (if sheet
    (let [sheet-data (sheet/get-sheet-data sheet :column-format :entity :filter (filter-to-map filter) :files? true :row-id? true)]
      (when files?
        (when-not directory
          (throw (ex-info "No directory specified"
                          {:type :user-input})))
        (fs/mkdirs directory)
        (doseq [row sheet-data]
          (doseq [file (:files row)] ;TODO what about filter
            (blob/download file
                           directory
                           (assoc options :to (and template (files/templated-file template file row)))))))
      (when sheet?
        (sheet/export-sheet sheet-data)))
    ;; No sheet specified
    (do
      (assert (nil? filter) "Use of --filter requires a sheet")
      (doseq [file (files project batch sheet nil)]
        (blob/download file
                       directory
                       options)))))

(defcommand "download-sheet" [project batch sheet] [filter template]
  (let [[project batch sheet] (resolve-project-batch-sheet project batch sheet)]
    (do-download project batch sheet nil (assoc {} :filter filter :sheet? true :template template))))

(doc-command "download-sheet" "Download the given sheet.")      

(defcommand "download-files" [directory project batch sheet] [template flatten]
  (let [[project batch sheet] (resolve-project-batch-sheet project batch sheet :ok)
        directory (resolve-pathname directory)]
    (blob/check-path directory)
    (do-download project batch sheet directory {:files? true :template template :flatten? flatten})
    (log/info "Download complete.")))

(doc-command "download-files" "(SHEET and BATCH are optional). If SHEET is specified, download all files associated with that sheet, otherwise download all files from PROJECT")

;;; HHH TODO Batch/sheet should be optional?
(defcommand "delete-files" [project batch sheet] [filter]
  (let [[project batch sheet] (resolve-project-batch-sheet project batch sheet)]
    (doseq [file (sheet/sheet-files sheet (filter-to-map filter))]
      (files/delete-file project file)
      (log/infof "Deleted %s" (:file/pathname file)))))

(doc-command "delete-files" "Delete files referenced by named PROJECT and SHEET")

(defcommand "server" [port] []
  (let [port (if port (Integer. port) default-port)]
    (server/start port)
    ;; TODO flag to suppress this
    (ju/open-url (format "http://localhost:%s" port))))

;;; TODO should be a flag I suppose
(defcommand "insecure-server" [port] []
  (let [port (if port (Integer. port) default-port)]
    (server/start-insecure port)
    ;; TODO flag to suppress this
    (ju/open-url (format "http://localhost:%s" port))))

(doc-command "insecure-server" "Start a local web server.")

(defcommand "initialize-schema" [] []
  (schema/transact-schema))

(doc-command "initialize-schema" "Initialize or update the database schema")

(defcommand "upload-from-cellengine" [username password ce-experiment-name PROJECT BATCH] [resume damp-run]
            (let [[project batch] (resolve-project-batch PROJECT BATCH)]
              (ce/upload-cellengine-project
                username password ce-experiment-name project batch {:resume resume :damp-run damp-run})))


(doc-command "upload-from-cellengine" "Passes cellengine annotations as sheet to PROJECT, then downloads Fcs files from experiment and uploads to project and matches to newly made sheet of annotations.")

(defcommand "rawsugar-to-cellengine" [ce-username ce-password CE-EXPERIMENT RS-PROJECT RS-BATCH RS-SHEET] []
            (let [[project batch sheet] (resolve-project-batch-sheet RS-PROJECT RS-BATCH RS-SHEET)]
              (ce/cli-upload-sheet->cellengine
                ce-username ce-password CE-EXPERIMENT project batch sheet)))

(doc-command "rawsugar-to-cellengine" "Take a sheet from rawsugar and uploads to cellengine as a new experiment. Creates a new experiment, downloads fcs files and uploads them to the experiment.  Pulls the metadata from the sheet and attaches to the fcs files in the experiment as annotations")

(defcommand "status-report" [] []
  (h/weekly-summary))

(defn all-commands
  []
  (sort (keys (dissoc (methods command) :default))))

(defn usage
  [options-summary]
  (->> [""
        (format "Rawsugar version %s" (version/get-version "rawsugar" "rawsugar"))
         ""
        "Usage: ./rawsugar[-dev] [OPTION]... [ACTION] [PARAMETERS]"
        ""
        "Actions:"
        (print-str (all-commands))
        ""
        ;; TODO opensource 
        "See user documentation at https://github.com/ParkerICI/rawsugar/blob/master/doc/user-guide.org"
        ""
        "Options:"
        options-summary]
       (str/join \newline)))

(defmethod command "help"
  [[_ & [command]] _ summary]
  (if command
    (print-command-doc command)
    (do 
      (println (usage summary))
      (print-doc))))

(defmethod command :default
  [[command & _] _ _]
  (log/errorf "Unknown command: %s" command)
  (println (usage nil)))

;;; For debugging
#_
(defmethod command "repl"
  [[command & _] _ _]
  (serv/start-server :port 7888))

(defn log-output-fn
  "A more minimal logging output fn. When -v specified, use the default."
  [data]
  (let [{:keys [level _?err _vargs msg_ _ns-str _file _hostname timestamp_ _line]} data]
    (str/join " "
              [(force timestamp_)
               (str/upper-case (name level))
               (force msg_)])))

;;; for debugging
(defn set-log-level
  [level]
  (log/merge-config! {:level level}))

(defn -main-wrapper [args do-it]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)]
    ;; TODO different defaults for dev mode
    (log/merge-config! {:level (if (:quiet options)
                                 :error
                                 (get {0 :info 1 :debug 2 :trace} (:verbose options) :trace))
                        :output-fn (if (> (:verbose options) 0)
                                     log/default-output-fn
                                     log-output-fn)})
    (when (:yes options)
      (autorespond! \y))
    (log/infof "Rawsugar version %s" (version/get-version "rawsugar" "rawsugar"))
    (config/check-config)
    (cond errors
          (log/error errors)
          :else
          (d/wrap-datomic-fn
           (partial do-it arguments options summary)))))

(defn -main
  [& args]
  (-main-wrapper
   args
   (fn [arguments options summary]
     (try 
       (when (login/user)
         (log/debugf "Logged in as %s" (login/user)))
       (command arguments options summary)
       (catch ExceptionInfo e
         (case (:type (ex-data e))
           :user-abort (log/info "Goodbye")
           (if (> (:verbose options) 0)
             (log/error e)
             (log/error (.getMessage e)  (str (.getData e))))))
       (catch Throwable e
         (if (> (:verbose options) 0)
           (log/error e)
           (log/error (.getMessage e))))))))

;;; For testing – should be just like -main but no error handling
(defn main-no-error-handling
  [& args]
  (-main-wrapper
   args
   (fn [arguments options summary]
       (when (login/user)
         (log/debugf "Logged in as %s" (login/user)))
       (command arguments options summary)
       )))




