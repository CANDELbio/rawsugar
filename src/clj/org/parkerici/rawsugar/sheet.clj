(ns org.parkerici.rawsugar.sheet
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [me.raynes.fs :as fs]
            [clojure.data.csv :as csv]
            [org.parkerici.rawsugar.datomic :as d]
            [org.parkerici.rawsugar.history :as h]
            [org.parkerici.rawsugar.matching :as matching]
            [org.parkerici.multitool.core :as u]))

(defn sheet-spec
  [sheet-contents?]
  (let [base 
        '[:db/id
          :sheet/name
          {:sheet/columns [:db/id :column/name]}
          {:object/creation-op [*]}
          :sheet/derived-from
          :batch/_sheets
          :run-step/_sheet]]
    (if sheet-contents?
      (conj base
            '{:sheet/rows [:db/id {:row/cells [*]}]})
      base)))


(def value-attributes
  #{:cell/value_string
    :cell/value_long
    :cell/value_float
    :cell/value_boolean
    :cell/value_instant
    :cell/value_ref})

(defn cell-value
  [cell]
  (some #(get cell %) value-attributes))

(defn value-attribute
  "Choose the appropriate attribute for a given value v"
  [v]
  (cond (string? v) :cell/value_string
        (integer? v) :cell/value_long
        (float? v) :cell/value_float
        (boolean? v) :cell/value_boolean
        (instance? java.util.Date v) :cell/value_instant
        :else (throw (ex-info "Can't store value" {:value v}))))

(defn add-value
  "Add {<attribute> <v>} to map"
  [map v]
  (if (nil? v)
    map
    (assoc map
           (value-attribute v)
           v)))


(defn filter-clause
  [column-name value]
  (let [cell-var (gensym "?cell")
        col-var (gensym "?col")]
    (cond (= column-name :row-id)
          `[[(ground ~value) ?row]]
          :else
          `[[?row :row/cells ~cell-var]            
            [~cell-var :cell/column ~col-var]
            [~col-var :column/name ~column-name]
            [~cell-var ~(value-attribute value) ~value]])))

(defn filter-clauses
  "Generate extra query clauses to filter rows by values. Assumes ?row variable."
  [filter-map]
  (mapcat (fn [[k v]] (filter-clause k v)) filter-map))

(defn lookup-sheet
  [batch sheet-name]
  (first
   (d/q1 `[:find ?sheet
           :in $ ?batch ?sheet-name
           :where
           [?batch :batch/sheets ?sheet]
           [?sheet :sheet/name ?sheet-name]]
         batch sheet-name)))

(defn lookup-column
  [sheet column-name]
  (first
   (d/q1 `[:find ?col
           :in $ ?sheet ?column-name
           :where
           [?sheet :sheet/columns ?col]
           [?col :column/name ?column-name]]
         sheet column-name)))

(defn sheet-exists?
  [batch sheet-name]
  (and batch sheet-name (lookup-sheet batch sheet-name)))

(defn check-not-exists
  [batch sheet]
  (when (nil? sheet)
    (throw (ex-info "No sheet specified" {:type :user-input})))
  (when (sheet-exists? batch sheet)
    (throw (d/hex-info "sheet already exists in batch"
                    {:type :user-input
                     :batch batch
                     :sheet sheet}
                    ))))

(defn sheet-batch
  [sheet]
  (d/q11
   '[:find ?batch
     :in $ ?sheet
     :where
     [?batch :batch/sheets ?sheet]]
   sheet))

(defn filtered-query
  [base filter]
  (assert (map? base))                  ;only works on map-format queries
  (if filter
    (update base :where concat (filter-clauses filter))
    base))

;;; Returns seq of column entity maps
(defn sheet-columns
  [sheet-id]
  (map first
       (d/q `[:find (pull ?col [*])
              :in $ ?sheet
              :where
              [?sheet :sheet/columns ?col]
              ]
            sheet-id)))

(defn sheet-row-ids
  [sheet-id]
  (map first
       (d/q `[:find ?row
              :in $ ?sheet
              :where
              [?sheet :sheet/rows ?row]
              ]
            sheet-id)))

(defn invert-row-set
  [sheet-id rows]
  (set/difference (set (sheet-row-ids sheet-id))
                  (set rows)))

(defn sheet-column-names
  [sheet]
  (map :column/name (sheet-columns sheet)))

(defn check-column-exists
  [sheet column]
  (when-not (or (get #{:row-id :files} column) ;special column?
                (get (set (sheet-column-names sheet)) column))
    (throw (d/hex-info "column does not exist"
                    {:type :user-input
                     :sheet sheet
                     :column column}))))

;;; TODO including rows in pull, so :rows? arg is no longer necessary

;;; formerly project-sheet-files-worker
;;; no longer supports null sheet, we'll do that some other way
(defn sheet-files-worker
  [sheet & [{:keys [rows? matched? unmatched? filter regex]}]]  ;; TODO? extension 
  (doseq [col (keys filter)]            ;HHH should probably be done by caller
    (check-column-exists sheet col))
  (d/q (u/de-ns
        (filtered-query
         `{:find [(pull ?file [* :row/_files]) ~@(if rows? '[?row] [])]
           :in [$ ?sheet]
           :where
           [[?sheet :sheet/rows ?row]
            [?row :row/files ?file]
             ~@(when regex
                 `[[?file :file/pathname ?path]
                   [(re-find (re-pattern ~regex) ?path)]])
             ~@(when unmatched? '[(not [_ :row/files ?file])])
             ~@(when (or rows? matched?) '[[?row :row/files ?file]])]}
         filter))
       sheet))

;; see project/project-files

(defn sheet-files
  "Return files from a project/sheet (sheet can be nil)"
  [sheet & [filter]]
  (map first (sheet-files-worker sheet {:filter filter})))


;;; Called only from get-sheet-data, so not really independent
;;; Note: cells with nil value won't return a row
;;; Note: doing a (pull ?sheet [*]) returns basically the same stuff, wonder if that might be more efficient. Can't do filtering that way though.
;;;
(defn get-sheet-raw
  "Returns a seq of [row-id column-id value] tuples"
  [sheet-id & [{:keys [filter as-of cells?]}]]
  (let [base-query                      ;query for the cell
         (filtered-query
          `{:find [?row ?col (pull ?cell [*])]
            :in [$ ?sheet]
            :where
            [[?sheet :sheet/rows ?row]
             [?row :row/cells ?cell]
             [?cell :cell/column ?col]]}
          filter)
        query
        (if cells?
          base-query
          ;; augment the query to return values instead of cells
          (-> base-query
              (assoc :find '[?row ?col ?value])
              (update :where conj '[(get-some $ ?cell :cell/value_float :cell/value_long :cell/value_string :cell/value_ref) [_ ?value]])))]
    ((if as-of (partial d/q-as-of as-of) d/q) (u/de-ns query) sheet-id)))

(defn get-sheet-data
  "Returns a seq of rows. Each row is a map from column ids to values.
  Options:
  filter:
  column-format: :name, :keyword, :entity, :id (default)

  row-id?: if true include a :row-id column (which will be filled with row entity ids)
  files?: If non-nil, include a :files column. Values are a sequence of file entities,
        or ids if value is :ids (used only for tests)
        or count of files if value is :count or 
  as-of: Use Datomic db as of transaction.
  cells?: Return full cell entities rather than values
  "
  [sheet & {:keys [filter column-format files? row-id? _as-of _cells?] :as options}]
  (let [raw (group-by first (get-sheet-raw sheet (select-keys options [:filter :as-of :cells?])))
        ;; TODO this looks like its doing the query twice???
        files-raw (when files? (group-by second (sheet-files-worker sheet {:filter filter :rows? true})))
        files-raw (if (= files? :ids) (map :db/id files-raw) files-raw)
        output-key (fn [col]
                     (case column-format
                       :name (:column/name col)
                       :keyword (u/keyword-safe (:column/name col))
                       :entity col
                       (:id nil) (:db/id col)))
        column-map (u/index-by :db/id (map #(assoc % :output-key (output-key %))
                                           (sheet-columns sheet)))
        ]
    (for [[row-id row-contents] raw]
      (let [base 
            ;; TODO this might be slow, maybe use transients?
            (reduce (fn [acc [_ col val]]
                      (if (contains? column-map col)
                        (assoc acc (get-in column-map [col :output-key]) val)
                        acc))
                    {}
                    row-contents)
            row (if files?
                  (assoc base :files ((if (= files? :count)
                                        count vec)
                                      (map first (get files-raw row-id))))
                  base)
            row (if row-id?
                  (assoc row :row-id row-id)
                  row)]
        row))))

(defn sheet-column-entity-id
  [sheet column-name]
  (d/q11
   '[:find ?col
     :in $ ?sheet ?column-name
     :where
     [?sheet :sheet/columns ?col]
     [?col :column/name ?column-name]]
   sheet column-name))

(defn sheet-column-values
  "Return a seq of distinct values in a column"
  [sheet column & [filter]]
  (first
   (d/q1 (filtered-query
          `{:find [(distinct ?val)]
            :in [$ ?sheet ?column]
            :where
            [[?sheet :sheet/rows ?row]
            [?row :row/cells ?cell]
            [?cell :cell/column ?column]
            [(get-some $ ?cell :cell/value_float :cell/value_long :cell/value_string :cell/value_ref) [_ ?val]]]}
          filter)
         sheet column
         )))

(defn sheet-column-all-values
  "Return a seq of [rowId value] pairs, where value is the value in the given row and column"
  [sheet column]
  (d/q `[:find ?row ?val
         :in $ ?sheet ?col
         :where
         [?sheet :sheet/rows ?row]
         [?row :row/cells ?cell]
         [?cell :cell/column ?col]
         [(get-some $ ?cell :cell/value_float :cell/value_long :cell/value_string :cell/value_ref) [_ ?val]]
         ]
        sheet column
       ))

;;; REPL only
;;; Wanted to do this inside Datomic but couldn't make it work
;;; used only by infer...
#_
(defn set-column-type
  [project sheet column type]
  (let [col (sheet-column-entity-id sheet column)]
    (h/transact [project :set-column-type sheet column type]
                [[:db/add col :column/type type]])))

;;; TODO this returns ids, might want full objects for files
;;; TODO needs to take filter and/or merge with project-sheet-files above
;;; Nothing actually uses this? No cellengine does
;;; → utils?
(defn all-keys
  "Given a seq of maps, return the union of all keys"
  [sheet-data]
  (reduce set/union (map (comp set keys) sheet-data)))

;;; TODO? possibly wont need tempids for cells if we make cell → row link
;;; Called from updown
;;; If batch-id is nil a new batch will be created  ; PPP creating two objects complicates changes h/transact, but so what. Hm, how about use special var names
;;; sheet-data must have string column names (ie from (get-sheet-data s :column-format :name))
(defn create-sheet-txn
  [project-id batch-id sheet-name sheet-data file-ent strings?]
  (let [{:keys [columns data]} sheet-data
        sheet-var (str "new_obj_sheet_" sheet-name)
        batch-var (or batch-id (str "new_obj_batch_" sheet-name))
        batch-txn (when-not batch-id
                    [[:db/add project-id :project/batches batch-var]
                     [:db/add batch-var :batch/name sheet-name]])
        sheet-txn [[:db/add batch-id :batch/sheets sheet-var]
                   [:db/add sheet-var :sheet/name sheet-name]]
        sheet-file-txn (if file-ent [[:db/add sheet-var :sheet/file file-ent]] [])
        col-txn (mapcat (fn [colname i]
                          [[:db/add sheet-var :sheet/columns colname]
                           [:db/add colname :column/name colname]
                           [:db/add colname :column/index i]])
                        columns (range))
        row-txn
        (mapcat (fn [row rowi]
                  (let [rowname (str "row" rowi)]
                    (cons [:db/add sheet-var :sheet/rows rowname]
                          (mapcat (fn [[colname v]]
                                    (if (= colname :files)
                                      ;; :files col handled specially
                                      (map (fn [file]
                                             [:db/add rowname :row/files (:db/id file)])
                                           v)
                                      ;; regular columns (the "%%" is to avoid certain collisions, should use gensyms)
                                      (let [cellname (str "cell" colname "%%" rowi)]
                                        [(add-value {:cell/column colname
                                                     :db/id cellname}
                                                    (if strings? (str v) v))
                                         [:db/add rowname :row/cells cellname]])))
                                  row))))
                data (range))]
    (concat sheet-txn sheet-file-txn batch-txn col-txn row-txn)))

;; Called from CLI
(defn create-blank [project batch sheet-name]
  (-> (h/transact [project :create-sheet batch sheet-name]
                  (create-sheet-txn project batch sheet-name [] nil false))
      (get-in [:tempids (str "new_obj_sheet_" sheet-name)])))

(defn delete-sheet-txn
  "Delete a single sheet. You probably don't want to call this, use batch/delete-sheet-and-deriveds"
  [sheet files?]
  `[[:db/retractEntity ~sheet]
    ~@(when files?
        (for [file (sheet-files sheet)
              ;; Don't delete file if its pointed to from another row
              :when (= 1 (count (:row/_files file)))]
          [:db/retractEntity (:db/id file)]))])

(defn sheet-cols
  "Compute list of col names from row data"
  [maps]
  (into [] (reduce set/union (map (comp set keys) maps))))

(defn column-name
  [col]
  (name 
   (if (map? col)
     (:column/name col)
     col)))

;;; Now expects column entities as keys in rows, for oreder-preservation
(defn export-sheet
  "Write sheet content to *out*. Assumes row-maps have :column-format :name."
  [row-maps]
  (let [row-maps (map #(-> %
                           ;; TODO broken if :files? not present
                           (assoc "filename" (str/join "," (map :file/pathname (:files %)))) ;TODO seems broken?
                           (assoc "warehouse" (str/join "," (map :file/location (:files %))))
                           (dissoc :files))
                      row-maps)
        columns (sort-by :column/index (sheet-cols row-maps))
        csv-data (cons (map column-name columns)
                       (map (fn [ent-map]
                              (map (fn [col] (or (get ent-map col) ""))
                                   columns))
                            row-maps))]
    (csv/write-csv *out* csv-data
                   :separator \tab)
    (flush)))

;;; TODO probably not how you want to do this
(defn export-sheet-to-string
  [row-maps]
  (with-out-str (export-sheet row-maps)))

(defn get-cell
  "returns [cell-id val-attribute-id value]"
  [row-id col-id]
  (d/q1 '[:find ?cell ?att ?value
          :in $ ?row-id ?col-id
          :where
          [?row-id :row/cells ?cell]
          [?cell :cell/column ?col-id]
          [(get-some $ ?cell :cell/value_float :cell/value_long :cell/value_string :cell/value_ref) [?att ?value]]
          ]
        row-id col-id))

(defn- get-cells
  "Get cells of given row. Returns seq of [cell-id col-id val-attribute-id value]"
  ;; Needs the sheet argument because of possible sharing 
  [sheet-id row-id]
  (d/q '[:find ?cell ?col ?att ?value 
         :in $ ?sheet ?row
         :where
         [?row :row/cells ?cell]
         [?cell :cell/column ?col]
         [?sheet :sheet/columns ?col]
         [(get-some $ ?cell :cell/value_float :cell/value_long :cell/value_string :cell/value_ref) [?att ?value]]
         ]
       sheet-id row-id))

(defn update-cell-txn
  [row-id col-id value]
  (let [[cell old-attribute old-value]
        (if (string? col-id)            ;string means its a temp id for a column created in the same txn
          [nil nil nil]
          (get-cell row-id col-id))]
    (if cell
      [[:db/retract cell old-attribute old-value]
       [:db/add cell (value-attribute value) value]]
      (let [cell (name (gensym "cell"))]
        [[:db/add cell (value-attribute value) value]
         [:db/add cell :cell/column col-id]
         [:db/add row-id :row/cells cell]]))))

;;; Matching

;;; This is the default value for the match command threshold argument
;;; Higher means a longer string match is required to declare a match
;;; Unfortunately this doesn't have a simple human interpretation.
(def default-match-threshold 10)

(defn- remove-ext [path]
  (subs path 0 (- (count path) (count (fs/extension path)))))

(defmulti match-row-filename
  (fn [method _row _filename _columns _column-weights] method))

(defmethod match-row-filename :soft
  [_ row filename columns column-weights]  
  (let [match-filename (matching/stripped filename)]
    (reduce +
            (for [col columns]
              ;; TODO this formula needs to be justified or at least documented!
              (* (Math/pow (count (matching/biggest-shared-substring
                                   (matching/stripped (str (get row col)))
                                   match-filename))
                           2)
                 (get column-weights col))))))

(defmethod match-row-filename :exact
  [_ row filename columns _]
  (reduce +
          (for [col columns
                :let [val (get row col)]]
            (if (and (string? val)
                     (str/includes? filename val))
              1 0))))

(defn match-files-columns
;;; 1) for each file, compute either
;;;    a) a weighted match score based on |largest-shared-substring|^2 * distinct-values
;;;    b) an exact match score
;;; 2) pick the max over the threshold
;;; Args: project, sheet as usual
;;;  files: list of file entities to match
;;;  columns: list of columns to match on, or nil/empty to match on all
;;; Returns a seq of [file-ent row-ent] pairs
  [sheet files columns
   & [{:keys [filename-only? exact-match? match-threshold]}]]
  (let [real-columns (if (empty? columns)
                       (map :db/id (sheet-columns sheet))
                       columns)
        col-distinct-counts (zipmap real-columns (map #(count (sheet-column-values sheet %)) real-columns)) ;TODO this could be done more efficiently using datomic count-distinct
        sheet-data (get-sheet-data sheet :row-id? true)
        row-count (double (count sheet-data))
        column-weights (zipmap real-columns (map (fn [col] (/ (get col-distinct-counts col) row-count)) real-columns))
        match-threshold (if exact-match?
                          ;; For exact match, use a computed threshold based on # of columns
                          (if (empty? columns)
                            1
                            (count columns))
                          (or match-threshold default-match-threshold))]
    (filter
     identity
     (for [file files]
       (let [match-filename (-> (if filename-only?
                                  (fs/base-name (:file/pathname file))
                                  (:file/pathname file))
                                remove-ext)
             scored-rows (map (fn [row]
                           (assoc row
                                  :score
                                  (match-row-filename
                                   (if exact-match? :exact :soft)
                                   row match-filename real-columns column-weights)))
                         sheet-data)
             max (u/max-by :score
                           (filter #(>= (:score %) match-threshold) scored-rows))]
         (if max
           [file max]
           nil))))))

(defn row-files
  [row]
  (d/qfirst
   '[:find ?file
     :in $ ?row
     :where [?row :row/files ?file]]
   row))

;;; This stuff no longer used

;;; Dumb matcher function
;;; Note: row requires string keys, might want to extend to allow keywords for serialization
#_
(defn add-row-txn
  [sheet-id row]
  (let [column-map (column-entity-map sheet-id)
        rowname "row"]                  ;might want to be uniqified if we are adding multiple rows
    (assert sheet-id "Sheet not found")
    (cons [:db/add sheet-id :sheet/rows rowname]
          (mapcat (fn [[colname v]]
                    (if (= colname :file-id)
                      [[:db/add rowname :row/files (Long. v)]]
                      (let [colname (name colname)] ;turn keywords into strings
                        (when-not (get #{"row-id" "files"} colname)
                          (let [cellname colname
                                col (get column-map colname)]
                            (assert col (str "Column not found: " colname))
                            [(add-value {:cell/column col
                                         :db/id cellname}
                                        v)
                             [:db/add rowname :row/cells cellname]])))))
                  row))))

;;; Must be some built-in way to do this.
;;; TODO extend to handle column changes.
(defn update-sheet
  [project sheet update]
  (h/transact
   [project :update-sheet project sheet update]
   (d/update->txn (d/get-entity sheet) update)))

(defn new-sheet-merge-rows-by-unique-columns
  [project batch sheet columns new-sheet-name]
  (let [sheet-data (get-sheet-data sheet :row-id? true :files? true)
        subset-file-empty (group-by #(empty? (% :files))
                                    sheet-data)
        subset-files (subset-file-empty false)
        subset-nofiles (map
                        (fn [x]
                          (select-keys x columns))
                        (subset-file-empty true))
        combined (vals (group-by
                         #(select-keys % columns)
                         subset-files))
        merged-files (map
                       (fn [row]
                         (assoc (select-keys (first row) columns)
                           :files (into [] (mapcat :files row))))
                       combined)]
    (when (= (count subset-files) (count combined))
      (throw (ex-info "Columns are too unique. No rows to combine."
                      {:type :user-input}) ))
    (check-not-exists project new-sheet-name)
    (create-sheet-txn project batch new-sheet-name (concat merged-files subset-nofiles) nil false)))

;;; Only used by CLI update
;;; TODO I suppose logically there should be a web version of this 
;;; File is just for logging/auditing purposes
;;; Returns id of new sheet


(defn sheet-name [sheet-id]
  (:sheet/name (d/get-entity sheet-id)))

;;; Maybe have a copy without changes, then make the changes, easier. Of course need to do mods in
;;; txn-space, kind of ugly.

;;; Doesn't actually delete anything!
;;; Makes a new sheet, copying the structure of the old one, but leaving out the designated rows
(defn delete-rows-txn
  [sheet rows new-sheet-name]
  (let [sheet-ent (d/get-entity sheet '[:sheet/name
                                        :batch/_sheets
                                        {:sheet/columns [:db/id]}
                                        ])
        new-sheet-var "new_obj_sheet"
        batch (get-in sheet-ent [:batch/_sheets 0 :db/id])
        all-rows (sheet-row-ids sheet)  ;can't use pull which only returns 1000 elts
        retained-rows (u/lset-difference all-rows rows)]
    (concat
     [[:db/add new-sheet-var :sheet/name new-sheet-name]
      [:db/add batch :batch/sheets new-sheet-var]]
     (for [column (:sheet/columns sheet-ent)]
       [:db/add new-sheet-var :sheet/columns (:db/id column)])
     (for [row retained-rows]
       [:db/add new-sheet-var :sheet/rows row]))))
    

;;; Functions to build a txn that creates a derived sheet. You usually call derived-sheet-txn to
;;; built the basic txn, then pass the txn through (as the first arg) of functions that modify the
;;; new sheet in some way, eg. derived-sheet-delete-rows.

;;; The intent of these is to reuse structure of the parent sheet whenever possible, or IOW they only
;;; creates column, row, and cell objects when they have to.

;;; Passing around and modifying a txn is kind of a wacky way to do things, since the txn lacks
;;; structure.

(defn- temp-var
  [type]
  (name (gensym type)))

;;; In theory this is not going to be used much, the defaulting usually happens in the UI.
;;; See also ops.cljs/form-default
(defn- default-derived-sheet-name
  [orig-name]
  (str orig-name "+"))                  ;TODO better

(defn- txn-replace
  [txn old new]
  (u/substitute txn {old new}))

(defn datoms
  [txn]
  (filter vector? txn))


;;; This builds the basics of a derived sheet txn, usually you call this first and pass the results through other fns
;;; - creating a new sheet entity
;;; - wiring it to batch and original sheet
;;; - optionally copying the row and column entities from the original
(defn derived-sheet-txn
  [sheet new-sheet-name & {:keys [columns? rows?] :or {columns? true rows? true}}]
  (let [sheet-ent (d/get-entity sheet '[:sheet/name
                                        :batch/_sheets
                                        {:sheet/columns [:db/id]}])
        rows (sheet-row-ids sheet)      ;Pull doesn't work for large rowsets
        new-sheet-name (or new-sheet-name (default-derived-sheet-name (:sheet/name sheet-ent)))
        new-sheet-var "new_obj_sheet"
        batch (get-in sheet-ent [:batch/_sheets 0 :db/id])]
    (concat
     [[:db/add new-sheet-var :sheet/name new-sheet-name]
      [:db/add batch :batch/sheets new-sheet-var]
      [:db/add new-sheet-var :sheet/derived-from sheet]]
     (if columns?
          (for [column (:sheet/columns sheet-ent)]
            [:db/add new-sheet-var :sheet/columns (:db/id column)])
          [])
     (if rows?
          (for [row rows]
            [:db/add new-sheet-var :sheet/rows row])
          []))))

(defn derived-sheet-add-column
  [txn new-col-name & [new-col-var]]
  (concat txn
          (let [new-col-var (or new-col-var new-col-name)] ;; By default, use the column name for a variable name. 
            [[:db/add "new_obj_sheet" :sheet/columns new-col-var]
             [:db/add new-col-var  :column/name new-col-name]])))

(defn check-column
  [txn col]
  (assert (some (fn [[_ _ att val]] (and (= att :sheet/columns)
                                 (= col val)))
                (datoms txn))
          "Column not in txn"))

;;; Cell column must be in txn
(defn derived-sheet-add-value
  [txn row col value]
  ;; either row or col has to be new
  (assert (or (string? row) (string? col)) "Attempt to mutate cell")
  (check-column txn col)
  (concat txn
          (let [cell-id (temp-var "cell")]
            [{:db/id cell-id
              :cell/column col
              (value-attribute value) value}
             [:db/add row :row/cells cell-id]])
          ))

;;; values is {<row id> <value>...}
(defn derived-sheet-add-column-values
  [txn column values]
  (reduce (fn [txn [row value]]
            (if value
              (derived-sheet-add-value txn row column value)
              txn))
          (derived-sheet-add-column txn column)
          values))

;;; I think a better convention would be to make these singular and use reduce in caller, but
;;; keeping it this way since it works.

;;; Note: this is used for both delete-columns and delete-rows

(defn derived-sheet-delete-entities
  [txn ids]
  (let [ids (set ids)]
    (remove (fn [[_ _ _ id]]
              (contains? ids id))
            txn)))                      ;TODO will fail if txn has non-datoms

(defn derived-sheet-delete-columns
  [txn col-ids]
  (derived-sheet-delete-entities txn col-ids))

(defn derived-sheet-delete-rows
  [txn row-ids]
  (derived-sheet-delete-entities txn row-ids))

;;; row map is {<row> {<col> <val> ...} ..}
;;; rows can be:
;;; - inherited: new clone row created
;;; - already in txn: just add new cells
;;; - brand-new: create row and cells

;;; Note: this can only work once IOW only when the txn still has original columns
;;;  If you create a new column, you need to add new cells for each row (that point to new column)
;;;  If you create a new row you can reuse cells and only create new ones as required


(defn derived-sheet-change-values
  "Change existing values, creating new rows when necessary.
  new-row-map is a map of ther form {rowId: {colId1: v1...}}
  colId can be :files iwc value is a seq of file-entity-ids"
  [txn sheet new-row-map]
  (let [ntxn (atom txn)]
    (doseq [[old-row col-map] new-row-map]
      (let [new-row-var (str "row-" old-row)
            row-cells (atom (u/index-by second (get-cells sheet old-row)))]
        (swap! ntxn txn-replace old-row new-row-var)
        (doseq [[col value] col-map]
          (if (= col :files)
            ;; We are changing the files
            (doseq [file value]
              (swap! ntxn conj [:db/add new-row-var :row/files file]))
            ;; regular column
            (let [new-cell-id (temp-var "cell")]
              (swap! ntxn conj {:db/id new-cell-id
                                :cell/column col
                                (value-attribute value) value})
              (swap! row-cells assoc col [new-cell-id])
              )))
        ;; Files not specified but new row, so copy the old ones
        (when-not (contains? col-map :files)
          (doseq [file (row-files old-row)]
            (swap! ntxn conj [:db/add new-row-var :row/files file])))
        (swap! ntxn concat
               (map (fn [[col_ [cell]]]
                      [:db/add new-row-var :row/cells cell])
                    @row-cells))))
    @ntxn))

(defn derived-sheet-add-row
  [txn sheet]
  (let [col (:db/id (first (sheet-columns sheet)))
        row-id (temp-var "row")
        cell-id (temp-var "cell")]  ;need to add an empty cell for Datomic
    (concat txn
            [[:db/add "new_obj_sheet" :sheet/rows row-id]
             [:db/add row-id :row/cells cell-id]
             [:db/add cell-id :cell/column col]
             [:db/add cell-id :cell/value_string ""] ;TODO  necessary? but shouldn't be
             ])))

;;; - do a query for cells that refer to that column (could be in any sheet)
;;; - go through txn and do subs
(defn derived-sheet-modify-column
  [txn column new-col-map]
  (let [column-entity (d/get-entity column)
        new-col-tempid (str "new_col_" column)
        new-col (assoc (merge column-entity new-col-map)
                       :db/id new-col-tempid)
        old-cells (u/index-by :db/id
                              (d/qfirst `[:find (pull ?cell [*])
                                          :where [?cell :cell/column ~column]]))
        ]
    (cons new-col
          (loop [base-txn (txn-replace txn column new-col-tempid)
                 new-txn []]
            (if (empty? base-txn)
              new-txn
            (let [datom (first base-txn)
                  [_ obj att val] (if (vector? datom) datom nil)]
              (if (and (= att :row/cells)
                       (contains? old-cells val))
                (let [new-cell-name (str "new_obj_cell-" val)]
                  (recur (rest base-txn)
                         ;; Note: does NOT make a new row, don't think we need to.
                         (concat new-txn
                                 [[:db/add obj :row/cells new-cell-name]
                                  (assoc (get old-cells val)
                                         :cell/column new-col-tempid
                                         :db/id new-cell-name)])))
                (recur (rest base-txn)
                       (conj new-txn datom)))))))))


;;; Resolve column name collisions.
;;; For a derived-sheet txn, if two columns have the same name, replace one with
;;; a newly created one. This is complex but the complexity is mostly in
;;; derived-sheet-modify-column
(defn resolve-column-collisions
  [base-txn cols1 cols2]
  (let [col-names1 (set (map :column/name cols1))]
    (loop [txn base-txn
           cols2 cols2]
      (if (empty? cols2)
        txn
        (let [col2 (first cols2)]
          (recur (if (contains? col-names1 (:column/name col2))
                   (derived-sheet-modify-column txn (:db/id col2) {:column/name (u/unique-relative-to (:column/name col2) col-names1 "-2")})
                   txn)
                 (rest cols2)))))))

;;; Pulls both sheets into memory, which is not great. 
;;; TODO the fuzzy join is not right, because it can reuse rows in sheet2 (because it is best-match). Really need a bipartite graph weighted match algorithm.
;;; TODO this interacts with the :column/index attribute. You might end up with a sheet with duplicate column indicies. This is weird but shouldn't cause
;;; any real problems, and fixing it is hard.
(defn join-sheets
  "LEFT OUTER JOIN sheet2 to sheet1. Returns a txn.
  fuzzy?: non-nil use fuzzy matching, if a number, use that as match threshold
  new-sheet?: if non-nil, create a new sheet (rather than mutating sheet1). If a string, use that for new sheet name"
  [sheet1 col1 sheet2 col2 fuzzy? new-sheet-name]
  (let [rows1 (get-sheet-data sheet1 :row-id? true :files? true :cells? true)
        indexed-rows2 (u/index-by #(cell-value (get % col2)) (get-sheet-data sheet2 :files? true :cells? true))
        cols1 (sheet-columns sheet1) 
        cols2 (sheet-columns sheet2)
        cols2 (if fuzzy? cols2 (u/remove= col2 cols2 :db/id))
        sheet!-id "new_obj_sheet"
        new-sheet-name (or new-sheet-name (str "Join of " (sheet-name sheet1) " and " (sheet-name sheet2)))
        ;; txn modulo renaming columns
        base-txn 
        (concat
         (derived-sheet-txn sheet1 new-sheet-name :rows? false)
         ;; Create new sheet (TODO other metadata)
         ;; Add columns from sheet1
         ;; Add columns from sheet2 (sheet1 supplied by derived-sheet-txn)
         ;; TODO need to uniquify names
         (mapcat (fn [col2]
                   [[:db/add sheet!-id :sheet/columns (:db/id col2)]])
                 cols2)
         ;; Create new rows
         (apply
          concat
          (for [row1 rows1]
            (let [join-value (cell-value (get row1 col1))
                  join-row (and join-value
                                (if fuzzy?
                                  (second (matching/get-best-threshold
                                           indexed-rows2
                                           join-value
                                           (if (number? fuzzy?) fuzzy? nil)))
                                  (get indexed-rows2 join-value)))
                  ;; This might not even be necessary
                  row!-id (str "new_obj_row-" (get row1 :row-id))]
              (if join-row
                ;; Add row and copy row1 cells
                (concat [[:db/add sheet!-id :sheet/rows row!-id]]
                        (mapcat (fn [[col1 _]]
                                  (let [cell (get row1 col1)]
                                    [[:db/add row!-id :row/cells (:db/id cell)]]))
                                (dissoc row1 :row-id :files)) 
                        (map (fn [file] [:db/add row!-id :row/files (:db/id file)]) (:files row1))
                        ;; Add the joined cells
                        (mapcat (fn [[col2 _]]
                                  (let [cell (get join-row col2)]
                                    [[:db/add row!-id :row/cells (:db/id cell)]]))
                                (dissoc join-row
                                        :files
                                        ;; if exact match, we can omit col2
                                        (if fuzzy? :nada col2)))
                        ;; add joined files
                        (map (fn [file] [:db/add row!-id :row/files (:db/id file)]) (:files join-row)))

                ;; no join row, reuse original row
                [[:db/add sheet!-id :sheet/rows (:row-id row1)]])))))]
    (resolve-column-collisions base-txn cols1 cols2)
    ))

;;; TODO file handling, argh
(defn union-sheets
  "Union sheet1 and sheet2. Returns a txn.  "
  [sheet1 sheet2 new-sheet-name]
  (let [cols1 (sheet-columns sheet1) 
        cols2 (sheet-columns sheet2)
        ;; A map of col-name to [col1, col2] pairs where the same column appears in both sheets
        shared-cols (u/dissoc-if (fn [[_ v]] (not (vector? v)))
                                 (merge-with vector (u/index-by :column/name cols1) (u/index-by :column/name cols2)))
        cols2-unique (u/lset-difference cols2 (map second (vals shared-cols)))

        rows1 (sheet-row-ids sheet1)
        rows2 (get-sheet-data sheet2 :cells? true :row-id? true)
        base-txn
        (concat
         (derived-sheet-txn sheet1 new-sheet-name)
         (map (fn [col] [:db/add "new_obj_sheet" :sheet/columns col]) (map :db/id cols2-unique))
         (map (fn [row] [:db/add "new_obj_sheet" :sheet/rows row]) (concat rows1 (map :row-id rows2))))
        ;; Sorry to do it this way but the nested loops were painfully verbose
        row-map (atom {})]
    (doseq [row rows2
            [col1 col2] (vals shared-cols)] 
      (when-let [old-cell (get row (:db/id col2))]
        (swap! row-map assoc-in [(:row-id row) (:db/id col1)] (cell-value old-cell))))
    (derived-sheet-change-values base-txn sheet2 @row-map)))


;;; Because row->file connection is NOT through a column, we need
;;; to create new row objects when we do this.
;;; This means this can't be called multiple times for same txn
;;; Matches is seq of [row-ent file-ent] pairs

;;; Matches are [file-ent row-seq] pairs
(defn matches->txn
  [sheet new-sheet-name matches]    
  (let [updates (reduce (fn [updates [file row]] (update-in updates [(:row-id row) :files] conj (:db/id file))) {} matches)]
    (-> (derived-sheet-txn sheet new-sheet-name)
        (derived-sheet-change-values sheet updates))))

;;; Currently unused except in tests, so moved to test_utils
#_
(defn transact-to-new-sheet
  [history-args txn]
  (let [{:keys [tempids]}
        (h/transact history-args txn)]
    (get tempids "new_obj_sheet")))

;;; Called from Cellengine
(defn transact-matches
  [project sheet matches new-sheet-name extras]
  (h/transact `[~project :file-match ~@extras]
              (matches->txn sheet new-sheet-name matches)))


(defn update-sheet-data
  [project-id sheet-id new-data file]
  (let [old-data (u/index-by :row-id (get-sheet-data sheet-id :row-id? true :column-format :name))
        sheet (d/get-entity sheet-id)
        sheet-columns (u/index-by :column/name (sheet-columns sheet-id))
        new-sheet-name (str (:sheet/name sheet) "+update")
        txn (derived-sheet-txn sheet-id new-sheet-name)
        ]
    (when-not new-data
      (throw (ex-info (str "Couldn't find sheet " sheet " in file " file)
                      {:type :user-input
                       :file file
                       :sheet sheet}
                      )))
    (when-not (get (first new-data) "row-id")
      (throw (ex-info "File data does not contain row-ids, can't update"
                      {:type :user-input
                       :file file})))
    ;; Create any new columns if needed
    (let [txn
          (loop [txn txn
                 ;; iterates through these
                 [new-col & rest-cols] (seq (set/difference (set (keys (first new-data)))
                                                            (set (conj (keys sheet-columns) "row-id"))))
                 ]
            (if (nil? new-col)
              txn
              ;; add columns
              (recur (loop [txn (derived-sheet-add-column txn new-col)
                            [row & rest-rows] new-data]
                       (if (nil? row)
                         txn
                         (if-let [value (get row new-col)]
                           (recur (derived-sheet-add-value txn (get row "row-id") new-col value)
                                  rest-rows)
                           (recur txn rest-rows))))
                     rest-cols)))]
      
      ;; Not sure this is right for new columns, might need to do those above
      (let [cell-changes
            (zipmap (map #(get % "row-id") new-data)
                    (map
                     (fn [new-row]
                       (let [row-id (get new-row "row-id")
                             old-row (get old-data row-id)]
                         (into {}
                               (map
                                (fn [[col new-value]]
                                  (when-not (= new-value (get old-row col))
                                    (when-let [col-id (get-in sheet-columns [col :db/id])]
                                      [col-id new-value])))
                                new-row))))
                     new-data))
            txn (derived-sheet-change-values txn sheet-id cell-changes)]
        (-> (h/transact [project-id :update-sheet sheet-id file]
                        txn)
            ;; Return new sheet id
            (get-in [:tempids "new_obj_sheet"]))))))
