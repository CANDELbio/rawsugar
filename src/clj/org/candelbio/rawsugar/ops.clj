(ns org.candelbio.rawsugar.ops
  (:require [org.candelbio.rawsugar.sheet :as sheet]
            [org.candelbio.rawsugar.batches :as batches]
            [org.candelbio.rawsugar.projects :as projects]
            [org.candelbio.rawsugar.files :as files]
            [org.candelbio.rawsugar.history :as h]
            [org.candelbio.rawsugar.datomic :as d]
            [org.candelbio.rawsugar.request :as request]
            [org.candelbio.rawsugar.updown :as updown]
            [org.candelbio.rawsugar.recipes :as recipes]
            [org.candelbio.rawsugar.export :as export]
            [org.candelbio.multitool.core :as u]
            [inflections.core :as inflect]
            [clojure.string :as str]
;            [org.parkerici.rawsugar.glue.terra :as terra]
;            [org.parkerici.terranigma.api :as terranigma]
            [org.candelbio.rawsugar.glue.cellengine :as cellengine]
            [oz.core :as oz]
            [environ.core :as env]))

;;; A general framework for specialized operations.

(defn safe-split
  [thing splitter]
  (cond (nil? thing) []
        (string? thing)
        (str/split thing (u/re-pattern-literal splitter))
        :else
        (list thing)))
    
;;; Operations take a map of parameters and return a transaction. 

;;; TODO Current multitool (0.0.15)is broken
(defn safe-nth
  [coll n]
  (and (< n (count coll)) (nth coll n)))

(defn split-column
  "Splitter is a string; gets turned into regex in safe-split"
  [{:keys [sheet column splitter new-sheet-name]}]
  (when (empty? splitter)
    (throw (ex-info "Splitter can't be blank"
                    {:type :user-input})))
  (let [sheet-data (sheet/get-sheet-data sheet :row-id? true)
        splits (zipmap (map :row-id sheet-data)
                       (map #(safe-split (get % column) splitter)
                            sheet-data))
        column (d/get-entity column)
        new-cols (u/uncollide (map #(str (:column/name column) %) (range (apply max (map count (vals splits)))))
                              :existing (sheet/sheet-column-names sheet)
                              :new-key-fn #(str % "-1"))
        txn (reduce
             (fn [txn i]
               (sheet/derived-sheet-add-column-values
                txn
                (nth new-cols i)
                (zipmap (keys splits) (map #(safe-nth % i) (vals splits)))))
             (sheet/derived-sheet-txn sheet new-sheet-name)
             (range (count new-cols)))
        ]
    {:txn txn
     :updates [[:sheets-changed]]}
    ))

(defn new-sheet-by-filetype
  [{:keys [batch sheet filetype new-sheet-name]}]
  (sheet/check-not-exists batch new-sheet-name)
  (let [sheet-data (sheet/get-sheet-data sheet :row-id? true :files? true :column-format :name)
        non-rows (remove (fn [row]
                           (and (some (fn [file]
                                        (= (:file/extension file) filetype))
                                      (:files row))
                                (:row-id row)))
                         sheet-data)]
    (when (= (count non-rows) (count sheet-data))
      (throw (ex-info "No rows match, so no sheet created"
                      {:type :user-input}) ))
    {:txn (-> (sheet/derived-sheet-txn sheet new-sheet-name)
              (sheet/derived-sheet-delete-rows (map :row-id non-rows)))
     :updates [[:sheets-changed]]
     }
    ))

"OK this is hairy:
for each row

  if no files do nothing (lets say)
  if exactly one file
    just link the old row, adding row@

  else (multiple files_
    for each file 
     new row entity, that links all the old cells, and has new row# cell and file"
(defn separate-files
  [{:keys [batch sheet file-num-col new-sheet-name]}]
  (let
      [sheet-data (sheet/get-sheet-data sheet :row-id? true :files? true :cells? true)
       colvar "new-column"
       txn (atom (-> (sheet/derived-sheet-txn sheet new-sheet-name :rows? false)
                     (sheet/derived-sheet-add-column file-num-col colvar)))
       add-datom (fn [datom] (swap! txn conj (cons :db/add datom)))]
    (doseq [{:keys [files row-id] :as row} sheet-data]
      (cond (empty? files) []
            (= 1 (count files))
            (let [cell-id (str "row-n-cell-" row-id)]
              (add-datom ["new_obj_sheet" :sheet/rows row-id])
              (add-datom [row-id :row/cells cell-id])
              (add-datom [cell-id :cell/column colvar])
              (add-datom [cell-id :cell/value_long 1]))
            :else
            (u/doseq* [file files
                       row-n (range 1 100000)]
                      (let [new-row-id (str "row-" row-id "-" row-n)
                            cell-id (str new-row-id "-cell")]
                        (add-datom ["new_obj_sheet" :sheet/rows new-row-id])
                        (add-datom [new-row-id :row/files (:db/id file)])
                        (add-datom [new-row-id :row/cells cell-id])
                        (add-datom [cell-id :cell/column colvar])
                        (add-datom [cell-id :cell/value_long row-n])
                        (doseq [[col_ cell] row]
                          (when (:db/id cell) (add-datom [new-row-id :row/cells (:db/id cell)]))
                          )))))
    (sheet/check-not-exists batch new-sheet-name)
    {:txn @txn
     :updates [[:sheets-changed]]}
    ))

(defn add-column
  [{:keys [sheet name new-sheet-name]}]
  (let [existing-cols (sheet/sheet-column-names sheet)]
    (when (empty? name)
      (throw (ex-info "New name can't be blank"
                      {:type :user-input})))
    (when (contains? (set existing-cols) name)
      (throw (ex-info (format "Column %s already exists" name)
                      {:type :user-input})))
    {:txn
     (-> (sheet/derived-sheet-txn sheet new-sheet-name)
         (sheet/derived-sheet-add-column name))
     :message (format "Column %s created" name)
     :updates [[:sheets-changed]]}
    ))

(defn check-new-column
  [sheet column-name]
  (when (empty? column-name)
    (throw (ex-info "Column name can't be blank" {})))
  (when (sheet/lookup-column sheet column-name)
    (throw (ex-info "Column already exists" {:column column-name
                                             :type :user-input})))
  )

(defn concatenate-two-columns-values
  [{:keys [sheet column1 column2 separator new-sheet-name new-col-name]}]
  (check-new-column sheet new-col-name)
  (let [sheet-data (sheet/get-sheet-data sheet :row-id? true)
        new-data (zipmap (map :row-id sheet-data)
                         (map (fn [row] (str (get row column1) separator (get row column2)))
                              sheet-data))]
    {:txn (-> (sheet/derived-sheet-txn sheet new-sheet-name)
              (sheet/derived-sheet-add-column-values new-col-name new-data))
     :updates [[:sheets-changed]]}))

(defn duplicate-column
  [{:keys [sheet column new-name new-sheet-name]}]
  (check-new-column sheet new-name)
  (let [sheet-data (sheet/get-sheet-data sheet :row-id? true)
        new-data (zipmap (map :row-id sheet-data)
                         (map (fn [row] (get row column)) 
                              sheet-data))]
    {:txn (-> (sheet/derived-sheet-txn sheet new-sheet-name)
              (sheet/derived-sheet-add-column-values new-name new-data))
     :updates [[:sheets-changed]]}))

(defn rename-column
  [{:keys [sheet column new-name new-sheet-name]}]
  (let [existing-cols (sheet/sheet-column-names sheet)]
    (when (empty? new-name)
      (throw (ex-info "New name can't be blank"
                      {:type :user-input})))
    (when (contains? (set existing-cols) new-name)
      (throw (ex-info (format "Column %s already exists" new-name)
                      {:type :user-input})))
    {:txn (-> (sheet/derived-sheet-txn sheet new-sheet-name)
              (sheet/derived-sheet-modify-column column {:column/name new-name} ))
     :updates [[:sheets-changed]]}))

(defn delete-columns
  [{:keys [sheet columns new-sheet-name]}]
  {:txn (-> (sheet/derived-sheet-txn sheet new-sheet-name)
            (sheet/derived-sheet-delete-columns columns))
   :updates [[:sheets-changed]]})

;;; TODO needs a confirm step! Or at least a working revert.
(defn delete-project
  [{:keys [project]}]
  {:txn [[:db.fn/retractEntity project]]
   :updates [[:projects-changed]]})

(defn rename-project
  [{:keys [project new-name]}]
  {:txn `[[:db/add ~project :project/name ~new-name]]
   :updates [[:projects-changed]]})

(defn create-project
  [{:keys [project]}]
  (projects/check-not-exists project)
  {:txn `[[:db/add "project0" :project/name ~project]]
   :message (format "Project %s created" project)
   :project [:new "project0"]
   :updates [[:projects-changed]]
   :navigate [:project :project "project0"] 
   })

(defn delete-sheet
  [{:keys [batch sheet files?]}]
  {:txn (batches/delete-sheet-and-deriveds-txn batch sheet files?)
   :updates [[:sheets-changed :files-changed]]
   ; TODO :navigate home if deleting current sheet
   }
  )

(defn rename-sheet
  [{:keys [batch sheet new-name]}]
  (sheet/check-not-exists batch new-name)
  {:txn [[:db/add sheet :sheet/name new-name]]
   :updates [[:sheets-changed]]})

(defn rename-batch
  [{:keys [project batch new-name]}]
  (batches/check-not-exists project new-name)
  {:txn [[:db/add batch :batch/name new-name]]
   :updates [[:batches-changed]]})      ;HHH

(defn delete-batch
  [{:keys [batch]}]
  {:txn [[:db/retractEntity batch]]
   :updates [[:batches-changed]]}
  )

(defn create-batch
  [{:keys [project batch]}]
  (batches/check-not-exists project batch)
  {:txn (batches/add-txn project batch)
   :message (format "Batch %s created" batch)
   :updates [[:batches-changed]]
   :navigate [:batch :project project :batch "batch0"]   ;TODO depends on innards of add-txn
   })

(defn merge-batches
  [{:keys [project batch-into batch-from]}]
  (assert (not (= batch-into batch-from)))
  {:txn (batches/merge-txn project batch-into batch-from)
   :message (format "Batch %s and %s merged" batch-into batch-from) ;HHH no way jose
   :updates [[:batches-changed]]})

;;; TODO assert produces a scary backtrace, should be a calmer user-level error feedback

(defn delete-checked-rows
  [{:keys [sheet rows new-sheet-name]}]
  (assert (not (empty? rows)) "No rows selected") ;TODO for symmetry, should error if ALL rows are selected
  (let [txn (sheet/delete-rows-txn sheet rows new-sheet-name)]
    {:txn txn
     :updates [[:sheets-changed]]}))

(defn keep-checked-rows
  [{:keys [sheet rows new-sheet-name]}]
  (assert (not (empty? rows)) "No rows selected") ;TODO for symmetry, should error if ALL rows are selected
  (let [txn (sheet/delete-rows-txn sheet (sheet/invert-row-set sheet rows) new-sheet-name)]
    {:txn txn
     :updates [[:sheets-changed]]}))

;;; TODO would be nice if this added at the end, but no good way to make that happen
;;; TODO possible extension: add n rows
(defn add-row
  [{:keys [sheet new-sheet-name]}]
  {:txn (-> (sheet/derived-sheet-txn sheet new-sheet-name)
            (sheet/derived-sheet-add-row sheet))
   :updates [[:sheets-changed]]})

(defn delete-checked-files
  [{:keys [files]}]
  (assert (not (empty? files)) "No files specified")
  (let [txn (mapv (fn [id] [:db/retractEntity id]) files)]
    {:txn txn
     :updates [[:files-changed] [:sheets-changed]]}))

(defn unlink-checked-files
  [{:keys [sheet files new-sheet-name]}]
  (assert (not (empty? files)) "No files specified")
  (let [txn (files/unlink-txn sheet files new-sheet-name)]
    {:txn txn
     :message (str (inflect/pluralize (count files) "file") " unlinked.")
     :updates [[:sheet-changed] [:files-changed]]}))

(defn move-checked-files
  [{:keys [batch files]}]
  (assert (not (empty? files)) "No files specified")
  (assert (every? #(empty? (files/rows %)) files) "All files must be unlinked first")
  (let [txn (files/move-txn files batch)]
    {:txn txn
     :updates [[:files-changed :batches-changed]]}))

(defn new-sheet-merge-rows-by-unique-columns
  [{:keys [project batch sheet columns new-sheet-name]}]
  {:txn (sheet/new-sheet-merge-rows-by-unique-columns project batch sheet columns new-sheet-name)})

#_
(defn fastq-r1-r2->terra
  [{:keys [sheet
           terra-namespace terra-workspace
           R1_Pattern R2_Pattern
           participants
           samples]}]
  (let [participant_metadata [{:colname "participant" :rs-colname participants}]
        metadata (if samples
                   (concat participant_metadata [{:colname "entity:sample_id" :rs-colname samples}])
                   participant_metadata)]
    (terra/fastq-r1-r2->terra sheet terra-namespace terra-workspace R1_Pattern R2_Pattern metadata)))

#_
(defn sheet-files->terra
  [{:keys [sheet terra-namespace terra-workspace terra-column-name participants-column sample-id-column]}]
  (terra/sheet-files->terra sheet terra-namespace terra-workspace terra-column-name participants-column sample-id-column))

(defn get-terra-workspace-permissions
  [{:keys [terra-namespace terra-workspace]}]
  (let [res #_ (terranigma/get-workspace terra-namespace terra-workspace) nil]
    {:message (get-in res [:body :accessLevel])}))

(defn match-files
  [{:keys [project batch sheet columns new-sheet-name files] :as options}]
  (let [files (or files (batches/batch-files batch false))
        matches (sheet/match-files-columns sheet files columns options)]
    (if (empty? matches)
      {:error "No matches, so no sheet created"}
      {:txn (sheet/matches->txn sheet new-sheet-name matches)
       :project project
       :message (format "%s matched out of %s"  (inflect/pluralize (count matches) "file") (count files))
       ;; TODO now autmatically goes to new sheet; this stuff might be unnecessary
       :updates [[:sheet-changed] [:files-changed]]})))

(defn sheet->cellengine
  [{:keys [sheet cellengine-experiment-name]}]
  (cellengine/authenticate (env/env :cellengine-username) (env/env :cellengine-password))
  (cellengine/sheet-files->cellengine sheet cellengine-experiment-name )
  {:message (format "Submitted")}
  )

(defn cellengine->sheet
  [{:keys [project batch cellengine-experiment-id]}]
  (assert (not (empty? cellengine-experiment-id)) "Cellengine experiment ID required.")
  (assert (= 24 (count cellengine-experiment-id)) "Please enter a valid 24 character hexidecimal cellengine experiment ID.")
  (cellengine/authenticate (env/env :cellengine-username) (env/env :cellengine-password))
  (assert (cellengine/workspace-id-exists? cellengine-experiment-id) "Cellengine experiment id does not exist or rawsugar does not have permission to edit.")
  (cellengine/cellengine-files->sheet project batch cellengine-experiment-id)
  {:message (format "Submitted")}
  )


(defn replace-values
  [{:keys [project sheet new-sheet-name column find replace case-insensitive? match-substrings? replace-substrings? invert? regex?]}]
  (when (and replace-substrings? (not match-substrings?))
    (throw (ex-info "Can't replace substrings unless you match substrings" {})))
  (let [find (if regex? find (u/re-quote find))
        replace (or replace "")
        pattern
        (try
          (case [(true? match-substrings?) (true? case-insensitive?)]
            [false false] (re-pattern (str "^" find "$"))
            [false true] (re-pattern (str "(?i)^" find "$"))
            [true false] (re-pattern find)
            [true true] (re-pattern (str "(?i)" find)))
          (catch java.util.regex.PatternSyntaxException e
            (throw (ex-info (str "Malformed regex: " (.getMessage e)) {:e e
                                                                       :type :user-input}))))
        pred (partial re-find pattern)
        pred (if invert?
               (comp not pred)
               pred)
        row-map (atom {})
        counter (atom 0)]
    (doseq [row (sheet/get-sheet-data sheet :row-id? true)
            column (if column (list column) (keys (dissoc row :row-id)))]
      (let [value (get row column)]
        (when (pred value)
          (let [new-value (if replace-substrings?
                            (str/replace value pattern replace)
                            replace)]
            (swap! row-map assoc-in [(:row-id row) column] new-value)
            (swap! counter inc)))))
    {:txn (-> (sheet/derived-sheet-txn sheet new-sheet-name)
              (sheet/derived-sheet-change-values sheet @row-map))     :project project
     :message (format "%s replaced" (inflect/pluralize @counter "value"))
     :updates [[:sheets-changed]] }))

(defn oz-graph [sheet spec]
  (let [data (sheet/get-sheet-data sheet :files? :count :column-format :keyword)
        defaults {:data {:values data}} ;:width 600 :height 400 it's better to let Vega default these
        spec (merge defaults spec)]
    {:txn nil
     :message "Graph generated."
     :html (oz/html spec)}))

(defn- col->field [col]
  (and col
       (-> col
           d/get-entity
           :column/name
           u/keyword-safe)))

(defn line-graph
  [{:keys [sheet color-column time-column value-column]}]
  (oz-graph sheet
            ;; the points are necessary for tooltip to work
            {:mark {:type "line" :tooltip {:content "data"} :point true} 
             :encoding {:x {:field (col->field time-column)
                            :type "ordinal"}
                        :y {:field (col->field value-column)
                            :type "quantitative"} ;This might want to be "temporal"
                        :color {:field (col->field color-column)
                                :type "nominal"}}
             }))

(defn dot-graph
  [{:keys [sheet color-column x-column y-column]}]
  (oz-graph sheet
            {:mark {:type "point" :filled true}
             :encoding {:x {:field (col->field x-column)
                            :type "ordinal"
                            :axis {:grid true}}
                        :y {:field (col->field y-column)
                            :type "ordinal"
                            :axis {:grid true}}
                        :color {:field (col->field color-column)
                                :type "nominal"}
                        :size {:aggregate "count"}
                        }}))

(defn dashboard-graph
  [{:keys [sheet color-column facet-column timepoint-column subject-column timepoints headless?] :as args}]
  (let [spec
        {:transform [{:calculate "datum.files != 0" :as "has files"}]
         :mark {:type "point" :filled true :tooltip {:content "data"} :size 150}
         :encoding {:x {:field (col->field timepoint-column)
                        :type "ordinal"
                        :sort (or timepoints "ascending")
                        :axis {:grid true
                               :orient "top"}}
                    :y {:field (col->field subject-column)
                        :type "ordinal"
                        :axis {:grid true}}
                    :columns {:field (col->field facet-column)
                              :type "nominal"}
                    :shape {:field "has files"
                            :scale {:domain [false, true]
                                    :range ["triangle-down", "circle"]}}}}
        spec (if color-column
               (-> spec
                   (assoc :selection
                          {:colors {:type "multi" :bind :legend :fields [(col->field color-column)]}})
                   (assoc-in [:encoding :color]
                             {:field (col->field color-column)
                              :type "nominal"})
                   (assoc-in [:encoding :opacity]
                             {:condition {:selection :colors :value 1}
                              :value 0.2})
                   (assoc-in [:encoding :size]
                             {:condition {:selection :colors :value 150}
                              :value 300}))
               spec)
        spec (if facet-column
               (assoc-in spec
                         [:encoding :column]
                         {:field (col->field facet-column)
                          :type "nominal"})
               spec)]
    (batches/persist-graph args spec)
    (when-not headless?
      (oz-graph sheet spec))))

(defn- upload-gs-files
  [{:keys [project batch gs-path template]} cont]
  (or (updown/upload-directory project batch gs-path {:template template :continuation cont})
      ;; nil result menas its doing it in the background
      {:message "Upload started in background, you will be notified on Slack when it's done."}))

(defn- upload-local-files
  [{:keys [project batch]}]
  (let [multipart-params (:multipart-params request/*request*)
        uploads (vals multipart-params)
        files (for [{:keys [filename tempfile]} uploads]
                (updown/upload project batch (str tempfile) "" {:name filename}))]
    (doall files)))

(defn upload-files
  [{:keys [gs-path sheets? project batch] :as args}]
  ;; TODO check that exactly 1 of gs/local specified
  (let [cont
        (fn [files]
          (let [sheets (when sheets?
                         (mapcat (partial updown/extract-sheets project batch) files))]
            {:message (format "Uploaded %s%s"
                              (inflect/pluralize (count files) "file")
                              (if (empty? sheets) ""
                                  (format ", %s"
                                          (inflect/pluralize (count sheets) "sheet"))))
             :updates (if sheets?
                        [[:files-changed] [:batches-changed] [:sheets-changed]]
                        [[:files-changed] [:batches-changed]])
             }))]
    (if gs-path
      (upload-gs-files args cont)
      (cont (upload-local-files args)))))

     
;;; TODO right now, sheets have to be from same batch, is that good?
(defn join-sheets
  [{:keys [project sheet1 column1 sheet2 column2 fuzzy? fuzzy-threshold new-sheet-name]}]
  {:txn (sheet/join-sheets sheet1 column1 sheet2 column2
                           (and fuzzy? (u/coerce-numeric fuzzy-threshold))
                           new-sheet-name)
   :project project
   :updates [[:sheets-changed]]}
  )

(defn union-sheets
  [{:keys [project sheet1 sheet2 new-sheet-name]}]
  {:txn (sheet/union-sheets sheet1 sheet2 new-sheet-name)
   :project project
   :updates [[:sheets-changed]]}
  )

(defn update-cells
  [{:keys [project sheet edits]}]
  (let [updates (reduce (fn [acc [row col value]] (assoc-in acc [row col] value))
                        {}
                        edits)
        new-sheet-name (str (:sheet/name (d/get-entity sheet)) " + edit cells") ;see opps.cljs/form-default
        ]
    (if (empty? edits)
      {:message "No cells changed"}
      {:txn (-> (sheet/derived-sheet-txn sheet new-sheet-name)
                (sheet/derived-sheet-change-values sheet updates))
       :project project
       :message (format "%s edited." (inflect/pluralize (count edits) "cell"))
       :updates [[:sheets-changed]]})))

(defn manual-file-match
  [{:keys [sheet matches]}]
  (let [new-sheet-name (str (:sheet/name (sheet/sheet-name sheet)) "+ manual match")]
    {:txn (files/link-txn sheet matches new-sheet-name)
     :message (format "%s matched." (inflect/pluralize (count matches) "file"))
     :updates [[:sheet-changed]]
     }))

;;; TODO make sheet optional in which case all batch files get exported
(defn export
  [{:keys [sheet template]}]
  (export/start-export-job sheet template)
  {:message "Export job started, will post on Slack when zip file is ready." ;TODO should be a check-back page I suppose
   })


#_
"The ops structure defines the Ops user interface commands. 
Transmitted to the client, so has to be serializable.

Note: in a few cases, the client has specialized ui code, see ops.cljs

Note: if this was CL I'd have a macro that defined the fn and this structure together, which would avoid some errors and redundancy. Harder to do in CLJ, though I imagine its possible


:operations   defines the available operations. 
              Each entry is a map of an op id (keyword) to an op definition (see below)
:menu         defines the organization of the Ops menu. 
              A vector of submenus; is [<name> :op1 ....]

An operation definiton is a map:
:name
:fn           the name of the handler fn. 
              Note: has to be serializable, so name of fn, not fn itself
:args         vector of arg definitions
              fields:
                 :name
                 :doc (optional)
                 :type
                 :hidden (optional)
                 :default (optional)
                 :optional? (optional)
:doc
:pseudo?      true for pseudo-ops that are not accessed through the ops modal
:ok           (optional) text for the action button in the UI, or :none to omit it


Op fns return either a transaction or a map containing fields (all optional):
:txn         a Datomic txn to execute. 
             Ideally an op will be implemented as a single txn, although not always possible
:project     a project value to give to h/transact
:message     a string to give back to the user
:html        an html page to display
:updates     seq of events to trigger when compolete


"


;;; Save some typing
(def batch-args
  [{:name      :project
    :type      :project}
   {:name      :batch
    :type      :batch}
   ])

(def sheet-args
  [{:name      :project
    :type      :project}
   {:name      :batch
    :type      :batch}
   {:name      :sheet
    :type      :sheet}
   {:name      :new-sheet-name
    :type      :new-sheet-name}
   ])

(def sheet-args-no-new-sheet
  (butlast sheet-args))

;;; TODO → multitool (partly done)
(defn self-label
  "Given a map HASHMAP with maps as values, adds the index to each value as the value of attriute ATTR"
  [attr hashmap]
  (reduce-kv (fn [acc k v] (assoc acc k (assoc v attr k)))
             {}
             hashmap))

;; OK this is insanely complicated. 
(defn doc-link
  "Make a link that opens an external URL in a separate pane"
  [url text]
  `[:span
    {:dangerouslySetInnerHTML
     {:__html
      ~(str
        (format "<a href='#' onclick=\"window.open('%s').location.assign('%s')\";>"
                "_popout"
                url)
        text
        "</a>")}}])

(def ops
  (u/de-ns                               ;TODO may no longe be necessary
   `{:operations
     ~(self-label
       :id

       `{:update-cells
         {:name "Update cells pseudo op" ; in that it is not invoked through modal form
          :pseudo? true   
          :fn update-cells
          }
         :manual-file-match
         {:name "Manual file match pseudo op" ; in that it is not invoked through modal form
          :pseudo? true   
          :fn manual-file-match
          }
         :union-sheets
         {:name "Union sheets"
          :fn union-sheets
          :args [{:name      :project
                  :type      :project}
                 {:name      :batch
                  :type      :batch}
                 {:name      :sheet1
                  :doc       "First sheet (will be changed)"
                  :type      :sheet}
                 {:name      :sheet2
                  :doc       "Second sheet"
                  :type      :sheet}
                 {:name      :new-sheet-name
                  :type      :string
                  :doc       "Name for new sheet"
                  }
                 ]
          :doc        "Add rows of second sheet into first sheet (not a JOIN!)"
          }
         :join-sheets
         {:name "Join sheets"
          :fn join-sheets
          :args [{:name      :project
                  :type      :project
                  }
                 {:name      :batch
                  :type      :batch
                  }
                 {:name      :sheet1
                  :type      :sheet
                  :doc       "Sheet to join to"
                  }
                 {:name      :column1
                  :doc       "Column in sheet1"
                  :type      :column
                  :depends-on    {:sheet :sheet1}}
                 {:name      :sheet2
                  :type      :sheet
                  :doc       "Sheet to join from"
                  }
                 {:name      :column2
                  :doc       "Column in sheet2"
                  :type      :column
                  :depends-on    {:sheet :sheet2}}
                 {:name      :fuzzy?
                  :doc       "Join uses fuzzy match rather than exact match"
                  :type      :boolean}
                 {:name      :fuzzy-threshold
                  :doc       "Optional threshold for fuzzy matching (min length of common substring)"
                  :default   ~sheet/default-match-threshold
                  :optional?  true
                  :type      :number}
                 {:name      :new-sheet-name 
                  :type      :string
                  :doc       "Name for new sheet"
                  :default   "New joined sheet"
                  }
                 ]
          :doc "LEFT OUTER JOIN two sheets, optionally with fuzzy matching (for just simply combining two sheets, see Union sheets op) "
          }
         :delete-checked-rows
         {:name "Delete checked rows"
          :fn delete-checked-rows
          :doc "Makes a new sheet with the checked rows deleted"
          :args [~@sheet-args              ;TODO these should be locked; doesn't make sense to change these
                 {:name      :rows
                  :type      :rows
                  :multiple  true
                  :hidden    true 
                  :doc       "Rows to delete"
                  }
                 ]
          }
         :keep-checked-rows
         {:name "Keep checked rows"
          :fn keep-checked-rows
          :doc "Makes a new sheet with only the checked rows"
          :args [~@sheet-args              ;TODO these should be locked; doesn't make sense to change these
                 {:name      :rows
                  :type      :rows
                  :multiple  true
                  :hidden    true 
                  :doc       "Rows to keep"
                  }
                 ]
          }

         :delete-checked-files
         {:name "Delete checked files"
          :fn delete-checked-files
          :args [{:name      :files
                  :type      :files
                  :hidden    true
                  :doc       "Files to delete"
                  }]
          }
         :move-checked-files
         {:name "Move checked files"
          :fn move-checked-files
          :args [~@batch-args
                 {:name      :files
                  :type      :files
                  :hidden    true
                  :doc       "Files to move"
                  }]
          :doc "Move the files to the specified batch. Files must be unlinked first"
          }
         :unlink-checked-files
         {:name "Unlink checked files"
          :fn unlink-checked-files
          :args [~@sheet-args           ;TODO lock or hide these, doesn't make sense to change them in this context
                 {:name      :files
                  :type      :files
                  :hidden    true
                  :doc       "Files to unlink"
                  }]
          }
         :split-column
         {:name "Split column"
          :fn   split-column
          :args [~@sheet-args
                 {:name :column
                  :type :column
                  :doc  "Existing column containing the values to split"}
                 {:name :splitter
                  :doc  "String to split on, eg \"-\"."}
                 ]}

         ;; Formerly "New sheet for files of type"
         :new-sheet-for-type
         {:name "Separate files by type"
          :fn   new-sheet-by-filetype
          :args [~@sheet-args
                 {:name      :filetype
                  :type      :string
                  :doc       "File type (extension), eg \"fcs\"."}
                 ]}


         ;; Formerly :new-sheet-split-files
         :separate-files
         {:name "Separate files into individual rows"
          :fn   separate-files
          :args [~@sheet-args
                 {:name :file-num-col
                  :type :string
                  :default "file #"
                  :doc  "New column to be created with file number (0,1,2....). ROI for images."}
                 ]}

         :add-column
         {:name "Add column"
          :fn   add-column
          :args [~@sheet-args
                 {:name      :name
                  :type      :string
                  :doc       "Name for new column"}
                 ]}

         :rename-column
         {:name "Rename column"
          :fn   rename-column
          :args [~@sheet-args
                 {:name      :column
                  :type      :column
                  :doc       "Existing column"}
                 {:name      :new-name
                  :type      :string
                  :doc       "New name for column"}
                 ]}

         :delete-column
         {:name "Delete columns"
          :fn   delete-columns
          :args [~@sheet-args
                 {:name      :columns
                  :multiple  true
                  :type      :columns}
                 ]}

         :duplicate-column
         {:name "Duplicate column"
          :fn   duplicate-column
          :args [~@sheet-args
                 {:name      :column
                  :type      :column}
                 {:name      :new-name    
                  ;; TODO should default and let the user know they can do that
                  :doc       "Name for new column"}
                 ]}

         :delete-project
         {:name          "Delete project"
          :fn            delete-project
          :args          [{:name      :project
                           :type      :project
                           }]
          }

         :delete-sheet
         {:name          "Delete sheet"
          :fn            delete-sheet
          :args          [~@sheet-args-no-new-sheet
                          {:name      :files?
                           :type      :boolean
                           :doc       "Also delete any files associated with the sheet"}
                          ]
          }

         :rename-sheet
         {:name          "Rename sheet"
          :doc           [:span "Rename a sheet (note: unlike most sheet ops, this does " [:b "not"] " make a new sheet)"]
          :fn            rename-sheet
          :args          [~@sheet-args-no-new-sheet
                          {:name      :new-name
                           :type      :string
                           }]
          }

         :add-row
         {:name "Add row"
          :doc "Add a blank row to a sheet"
          :fn   add-row
          :args [~@sheet-args]
          }

         :delete-batch
         {:name          "Delete batch"
          :fn            delete-batch
          :args          [{:name      :project
                           :type      :project
                           }
                          {:name      :batch
                           :type      :batch
                           }
                          ]
          :doc          "Delete batch, including its contained sheets and files."
          }

         :rename-batch
         {:name          "Rename batch"
          :fn            rename-batch
          :args          [{:name      :project
                           :type      :project
                           }
                          {:name      :batch
                           :type      :batch
                           }
                          {:name      :new-name
                           :type      :string}]
          }

         :create-batch
         {:name          "Create new batch"
          :fn            create-batch
          :args          [{:name      :project
                           :type      :project
                           }
                          {:name      :batch
                           :type      :string}]
          }

         :merge-batches 
         {:name          "Merge batches"
          :fn            merge-batches
          :args          [{:name      :project
                           :type      :project
                           }
                          {:name      :batch-into
                           :type      :batch
                           :doc       "Batch to merge into"
                           }
                          {:name      :batch-from
                           :type      :batch
                           :doc       "Batch to merge from (will be removed)"
                           }]
          }
         :create-project
         {:name          "Create new project"
          :fn            create-project
          :args          [{:name      :project
                           :type      :string}]
          }

         :rename-project
         {:name          "Rename project"
          :fn            rename-project
          :args          [{:name      :project
                           :type      :project
                           }
                          {:name      :new-name
                           :type      :string}]
          }

         :match-files
         {:name "Match files"
          :fn match-files
          :doc "Attempt to match any unmatched files in the project to rows of the designated sheet"
          :args [~@sheet-args
                 {:name      :columns
                  :type      :columns
                  :multiple  true
                  :doc       "Columns to match on, or blank for all columns"}
                 {:name      :filename-only?
                  :type      :boolean
                  :doc       "Use only base filename rather than full path including directories"}
                 {:name      :exact-match?
                  :type      :boolean
                  :doc       "Column value must be exact substring of the file name/path"}
                 {:name      :match-threshold
                  :type      :number
                  :default   ~sheet/default-match-threshold
                  :doc       "Threshold for matching (lower for more matches). Ignored if exact-match? is selected."}
                 ]
          }
         :upload-files
         {:name "Upload files"
          :args [{:name      :project
                  :type      :project}
                 {:name      :batch
                  :type      :batch}
                 {:name      :gs-path
                  :doc       "path to gs file or directory, eg \"gs://bucket/dir\""}
                 {:name      :local-files
                  :type      :local-files
                  :doc       "Local files to upload"}
                 {:name      :local-directory
                  :type      :local-directory
                  :doc       "Local directory to upload"}
                 {:name      :template     ;TODO make sure this applies to local file uploads (or change comments)
                  :optional?  true
                  :doc       "optional – a string, possibly with wildcards; only matching files will be uploaded"}
                 ]
          :fn         upload-files
          :doc        [:span "Upload data files. Specify exactly one of "
                       [:b "gs-path"] ", " [:b "local-files"] ", or " [:b "local-directory"]] ;TODO make button match
          :params     {:sheets? false}}

         :upload-sheets
         {:name "Upload sheets"             ;TODO see above
          :args [{:name      :project
                  :type      :project}
                 {:name      :batch
                  :type      :batch}
                 {:name      :gs-path
                  :type      :string
                  :doc       "path to gs file or directory, eg \"gs://bucket/dir\""}
                 {:name      :local-files
                  :type      :local-files
                  :doc       "Local files to upload"}              
                 ;; Hm, Could have a :local-directory, but I would guess you would never want to add a directory sheets
                 ]
          :fn         upload-files         ;note: same fn as :upload-files
          :doc        [:span "Upload sheet files. Specify exactly one of "
                       [:b gs-path] " or " [:b "local-files"]]
          
          :params     {:sheets? true}}

         :download-sheet
         {:name "Download sheet"
          :args [~@sheet-args-no-new-sheet]
          :doc "Download sheet as tsv"
          :ok   :none
          }
         :export
         {:name "Export sheet and files"
          :args [~@sheet-args-no-new-sheet
                 ;; TODO select columns
                 {:name    :template
                  :type    :string
                  :optional?  true
                  :doc     [:span "Optional template for output files ["
                            ~(doc-link "/doc/cli.html#templates" "format description")
                            "]"]
                  }]
          :fn export
          :doc "Download sheet and associated files in a .zip, asynchronously."
          }
         
;;; Terranigma stuff removed

         :sheet->cellengine
         {:name "sheet->cellengine"
          ;; TODO opensource
          :doc ~(format "Send a sheet of FCS files and annotations to cellengine. REQUIRES: %s has basic read/write access to cellengine experiment." (:cellengine-username env/env))
          :fn sheet->cellengine
          :args [~@sheet-args
                 {:name      :cellengine-experiment-name
                  :type      :string
                  :doc       ~(format "Cellengine experiment name. Make sure %s is listed as a user for the cellengine experiment" (:cellengine-username env/env))}]
          }
         :cellengine->sheet
         {:name "cellengine->sheet"
          :doc "Send a cellengine experiment to a Rawsugar batch as a new sheet."
          :fn cellengine->sheet
          :args [~@batch-args
                 {:name :cellengine-experiment-id
                  :type :string
                  :doc "Cellengine experiment ID: 24 character hex string. Found in URL of experiment on cellengine."}]
          }
         :new-sheet-merge-rows-by-unique-columns


         ;; Formerly "Merge rows to new sheet"
         {:name "Merge rows"
          :fn   new-sheet-merge-rows-by-unique-columns
          :args [~@sheet-args
                 {:name      :columns
                  :multiple  true
                  :type      :columns}
                 ]}
         :concatenate-two-columns-values
         {:name "Concatenate two columns' values"
          :fn concatenate-two-columns-values
          :doc "Concatenate the values by row across two columns and make a new column with the result"
          :args [~@sheet-args
                 {:name :column1
                  :type :column
                  :doc  "First column to merge"}
                 {:name :column2
                  :type :column
                  :doc  "Second column to merge"}
                 {:name :separator
                  :optional?  true
                  :type :string
                  :doc "Optional separater between columns"}
                 {:name :new-col-name
                  :type :string
                  :doc "Name of new merged column"}]}
         :replace
         {:name "Replace values"
          :doc "Find and replace values"
          :fn replace-values
          :args [~@sheet-args
                 {:name :column
                  :type :column
                  :doc  "Column to find/replace"} ;TODO allow blank for all or multiple column chooser
                 {:name :find
                  :type :string
                  :doc  "String to search for"} ;TODO would be nice to enforce a value here
                 {:name :regex?
                  :type :boolean
                  :indent true
                  :doc  [:span "Search string is regex ["
                         [:a {:href "https://cheatography.com/davechild/cheat-sheets/regular-expressions/" :target "_regex"}
                          "syntax reference"]
                         "]"]
                  }
                 {:name :case-insensitive?
                  :type :boolean
                  :indent true
                  :doc  "Use case-insensitive match"}
                 {:name :match-substrings?
                  :type :boolean
                  :indent true
                  :doc  "Match substrings (otherwise whole cell is matched)"}
                 {:name :invert?
                  :type :boolean
                  :indent true
                  :doc  "Replace cells that DO NOT match the find string"}
                 {:name :replace
                  :type :string
                  :doc  "String to replace"}
                 {:name :replace-substrings?
                  :type :boolean
                  :indent true
                  :doc  "Replace the matched substring (rather than whole string)"}
                 ]
          }
         :line-graph 
         {:name "Line graph"
          :fn   line-graph
          :args [~@sheet-args-no-new-sheet
                 {:name      :color-column
                  :doc       "subject column (color, nominal)"
                  :type      :column}
                 {:name      :time-column
                  :doc       "time column (x, ordinal)"
                  :type      :column}
                 {:name      :value-column
                  :doc       "value column (y, quantitative)"
                  :type      :column}
                 ]
          }
         :dot-graph 
         {:name "Dot graph"
          :fn   dot-graph
          :args [~@sheet-args-no-new-sheet
                 {:name      :color-column
                  :doc       "color column (nominal)"
                  :type      :column}
                 {:name      :x-column
                  :doc       "x (ordinal)"
                  :type      :column}
                 {:name      :y-column
                  :doc       "y column (ordinal)"
                  :type      :column}
                 ]}
         :dashboard-graph 
         {:name "Dashboard graph"
          :fn   dashboard-graph
          :doc  "Similar to dot-graph, but makes a persistent data dashboard that appears on the batch page."
          :args [~@sheet-args-no-new-sheet
                 {:name      :subject-column
                  :doc       "Subject (y) column"
                  :type      :column}
                 {:name      :timepoint-column
                  :doc       "Timepoint (x) column"
                  :type      :column}

                 {:name      :timepoints   
                  :depends-on {:column :timepoint-column}
                  :type      :column-values
                  :optional?  true
                  :doc       "(optional) an ordered list of timepoints"}


                 {:name      :color-column
                  :doc       "color column (optional)" ;TODO probably no way to not spec a column
                  :type      :column
                  :optional?  true}
                 {:name      :facet-column
                  :doc       "facet column (optional)" ;TODO probably no way to not spec a column
                  :type      :column
                  :optional?  true}
                 ]}
         })
     :menu
     [["Projects" :create-project :delete-project :rename-project] 
      ["Batches"  :create-batch :delete-batch :rename-batch :merge-batches]
      ["Sheets (basic)" :upload-sheets :download-sheet :delete-sheet :rename-sheet]
      ["Sheets (advanced)" :delete-checked-rows :keep-checked-rows :join-sheets :new-sheet-for-type :separate-files :new-sheet-merge-rows-by-unique-columns :union-sheets :replace :add-row]
      ["Columns" :add-column :delete-column :rename-column :duplicate-column :split-column :concatenate-two-columns-values]
      ["Files" :upload-files :export :match-files :unlink-checked-files :delete-checked-files :move-checked-files]
      ["Graphing" :line-graph :dot-graph :dashboard-graph]
      ["Terra" :fastq-r1-r2->terra :sheet-files->terra :get-workspace-permissions]
      ["Cellengine" :sheet->cellengine :cellengine->sheet]
      ]
     }))

(defn get-ops
  []
  ;; Piggybacking recipes on ops, its the same kind of static data.
  ;; Maybe rename :ops to :static-defs or something
  (let [recipes recipes/recipes]
    (assoc ops :recipes recipes)))

;;; The :fn field in op def is a symbol (so it can be serialized)
;;; this gets the value out of it.
(defn symbol-value
  [s]
  (assert (symbol? s))                  ;makes the eval slightly less dangerous
  (binding [*ns* (find-ns 'org.candelbio.rawsugar.ops)]
    (eval s)))

;;; TODO the one in multitool 0.0.15 has poor behavior on nil
(defn sequencify
  [thing]
  (when thing
    (if (sequential? thing)
      thing
      (list thing))))

;;; Now that we are forced to use url params, they get stringified. This undoes that, but may not always be right
(defn retype-params
  [p]
  ;; Turn columns into seq if it isn't already
  (let [p (update p :columns sequencify)
        op (keyword (:op p))
        args (u/index-by :name (get-in ops [:operations op :args]))]
    (assoc
     (reduce-kv (fn [p k v]
                  (let [type (get-in args [k :type])
                        multiple? (get-in args [k :multiple])
                        v (if multiple? (sequencify v) v)]
                    (assoc p k
                           (cond (= type :boolean) (u/coerce-boolean v)
                                 (= type :string) v
                                 (sequential? v) (map u/coerce-numeric v)
                                 :else
                                 (u/coerce-numeric v)))))
                p p)
     :op op)))
        
;; Debugging only
(defn do-op-raw
  [params]
  (let [{:keys [op project batch] :as params} (retype-params params)
        opdef  (get-in ops [:operations op])
        params (merge params (:params opdef))
        function (symbol-value (:fn opdef))]
    (function params)))

(defn do-op
  [params]
  (let [{:keys [op project batch] :as params} (retype-params params)
        opdef  (get-in ops [:operations op])
        params (merge params (:params opdef))
        _ (prn :do-op params)
        function (symbol-value (:fn opdef))]
    (try
      (let [result (function params)        ;do it!
            result (if (map? result) result {:txn result}) ;coerce result to map
            {:keys [txn message error html project updates navigate]
             :or {project project
                  message (and (not (:error result)) (str (:name opdef) " successful!"))}}
            result 
            tempids
            (when txn
              (:tempids
               (h/transact [project :op op params]
                           txn)))
            ]
        ;; Response to client
        {:message message
         :error error
         :html html
         :updates updates
         :navigate (if navigate
                     (u/substitute navigate tempids)
                     ;; If a new sheet was created, navigate to it. Bit of a hack!
                     (and (get tempids "new_obj_sheet")
                          [:sheet :project project :batch batch :sheet (get tempids "new_obj_sheet")]))
         :tempids tempids
         }
        )
      (catch clojure.lang.ExceptionInfo e
        (if (= :user-input (:type (ex-data e)))
          {:error (ex-message e)}
          {:error (str "Unexpected error: "
                       (str e))
           }
          )))))

;;; Debugging use only
(defn op-results
  [params]
  (let [{:keys [op] :as params} (retype-params params)
        function (symbol-value (get-in ops [:operations op :fn]))
        result (function params)]        ;do it!
    result))

