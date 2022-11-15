(ns org.parkerici.rawsugar.ops-test
  (:require  [clojure.test :refer :all]
             [org.parkerici.rawsugar.test-utils :as tu]
             [org.parkerici.rawsugar.ops :refer :all]
             [org.parkerici.rawsugar.updown :as updown]
             [org.parkerici.rawsugar.sheet :as sheet]
             [org.parkerici.rawsugar.history :as h]
             [org.parkerici.rawsugar.datomic :as d]
             [org.parkerici.multitool.core :as u]
             ))

(deftest split-col-test
  (tu/with-project-batch project batch
    (updown/upload-sheets project batch "test/resources/proj3/split-p.tsv")
    (tu/with-datomic-context
      (let [sheet (sheet/lookup-sheet batch "split-p")
            cols (sheet/sheet-columns sheet)
            split-col (u/some-thing #(= "b" (:column/name %)) cols)]
        (is (= 2 (count cols)))
        (let [txn (:txn (split-column {:project project :batch batch :sheet sheet :column (:db/id split-col) :splitter "-"}))
              sheet (tu/transact-to-new-sheet txn)]
          (tu/with-datomic-context
            (is (= 6 (count (sheet/sheet-columns sheet))))
            (let [data (sheet/get-sheet-data sheet :column-format :name)]
              (is (some #(= "multiple" (get % "b2")) data))
              )))))))

(deftest new-sheet-by-filetype-test
  (tu/with-project-batch project batch
    (updown/upload-sheets project batch "test/resources/proj4/meta.tsv")
    (tu/with-datomic-context
      (let [sheet-meta (sheet/lookup-sheet batch "meta")]
        (updown/upload-and-match project batch sheet-meta "test/resources/proj4/files/" nil {})
        (tu/with-datomic-context
          (let [sheet-matched (sheet/lookup-sheet batch "meta-matched")
                new-sheet-name "xtx files"
                txn (:txn (new-sheet-by-filetype {:project project :batch batch :sheet sheet-matched :filetype "xtx" :new-sheet-name new-sheet-name}))]
            (h/transact [project :test] txn)
            (tu/with-datomic-context
              (let [new-sheet (sheet/lookup-sheet batch new-sheet-name)
                    new-sheet-data (sheet/get-sheet-data new-sheet :column-format :name :files? true)
                    row (first new-sheet-data)
                    files (:files row)]
                (is (= 1 (count new-sheet-data)))
                (is (= {"a" "bar" "b" 3 "c" 4  "fileName" "bar.xtx"} (dissoc row :files)))
                (is (= 1 (count files)))
                (is (= "bar.xtx" (:file/pathname (first files)))))
              (testing "Prevent duplicate sheetnames"
                (is (thrown-with-msg?
                     Exception #"already exists"
                     (let [txn2 (:txn (new-sheet-by-filetype {:project project :batch batch :sheet sheet-matched :filetype "xtx" :new-sheet-name new-sheet-name}))]
                       (h/transact [project :test] txn2))))))))))))

;;; Out of service, the whole nature of component relations has changed so this needs rethinking
#_
(deftest delete-sheet-test
  (let [batch-files-fn
        (fn [batch]
          (set (map first (d/q '[:find ?path :in $ ?b :where [?b :batch/files ?f] [?f :file/pathname ?path]] batch))))]
    (testing "without file deletion"
      (tu/with-project-batch project batch
        (updown/upload-sheets project batch "test/resources/proj4/meta.tsv")
        (tu/with-datomic-context
          (let [sheet-meta (sheet/lookup-sheet batch "meta")]
            (updown/upload-and-match project batch sheet-meta "test/resources/proj4/files/" nil {})
            (tu/with-datomic-context
              (let [txn (:txn (delete-sheet {:batch batch :sheet sheet-meta :files? false}))]
                (h/transact [project :delete-project-test] txn)
                (tu/with-datomic-context
                  ;; the .tsv file is deleted, data files stay around
                  (is (= #{"bar.xtx" "unmatched.txt" "foo.txt"}
                         (batch-files-fn batch)))))))))
      (testing "with file deletion"
        (tu/with-project-batch project batch
          (updown/upload-sheets project batch "test/resources/proj4/meta.tsv")
          (tu/with-datomic-context
            (let [sheet-meta (sheet/lookup-sheet batch "meta")]
              (updown/upload-and-match project batch sheet-meta "test/resources/proj4/files/" nil {})
              (tu/with-datomic-context
                (let [txn (:txn (delete-sheet {:batch batch :sheet sheet-meta :files? true}))]
                  (h/transact [project :delete-project-test] txn)
                  (tu/with-datomic-context
                    (is (= #{"unmatched.txt"} (batch-files-fn batch)))))))))))))

(defn do-op-new-sheet
  [args]
  (get-in (do-op args) [:tempids "new_obj_sheet"]))

(deftest column-op-tests
  (tu/with-project-batch-sheet project batch sheet
    (testing "add column"
      (let [new-sheet (do-op-new-sheet {:op :add-column :project project :batch batch :sheet sheet :name "newcol"})]
        (tu/with-datomic-context
          (is (= '("newcol") (sheet/sheet-column-names new-sheet))))
        (testing "rename column"
          (tu/with-datomic-context
            (let [col-id (:db/id (first (sheet/sheet-columns new-sheet)))
                  new-sheet (do-op-new-sheet {:op :rename-column :project project :batch batch :sheet new-sheet :column col-id :new-name "garmonbozia"})]
              (tu/with-datomic-context
                (is (= '("garmonbozia") (sheet/sheet-column-names new-sheet))))
              (testing "delete columns"
                (tu/with-datomic-context
                  (let [col-id (:db/id (first (sheet/sheet-columns new-sheet)))
                        new-sheet (do-op-new-sheet {:op :delete-column :project project :batch batch :sheet new-sheet :columns (list col-id)})]
                    (tu/with-datomic-context
                      (is (= '() (sheet/sheet-column-names new-sheet))))))))))))))

(deftest ops-consistency
  (let [defined-ops (keys (get ops :operations))
        real-ops (remove #(get-in ops [:operations % :pseudo?]) defined-ops)
        menu-ops (mapcat rest (get ops :menu))]
    ;; Bag-equality
    (is (= (set real-ops) (set menu-ops)))
    (is (= (count real-ops) (count menu-ops)))
    (doseq [op real-ops]
      (let [f (get-in ops [:operations op :fn])]
        (when f
          (is (function? (symbol-value f))))))))

;;; Skip these ops for various reasons
(def excused #{:merge-batches          ;correctly gets an error because both batch args are the same
               :get-workspace-permissions ;TODO these are Robin's and should be made to work eventually
               :cellengine->sheet
               :sheet->cellengine
               :fastq-r1-r2->terra
               :sheet-files->terra
               :new-sheet-merge-rows-by-unique-columns
               :move-checked-files      ;needs unlinked file
               :update-cells
               :manual-file-match
               :download-sheet          ;works through separate mechanism
               })

;;; Test (almost) all ops short of actually doing them. Tests that their arguments are in the right format and that their fns generate reasonable output.
(deftest ops-roll-call
  (tu/with-project-batch project batch 
    (updown/upload-sheet-and-files project batch "test/resources/proj4/meta.tsv" "test/resources/proj4/files" {})
    (tu/with-datomic-context
      (let [sheet-id (sheet/lookup-sheet batch "meta-matched")
            sheet (d/get-entity sheet-id)
            column (:db/id (first (:sheet/columns sheet)))
            column-name (:column/name (first (:sheet/columns sheet)))
            row (:db/id (first (:sheet/rows sheet)))
            file (:db/id (first (sheet/sheet-files sheet-id)))  ;  (updown/upload project batch "test/resources/randomfile.txt" "" {})
            gs-path (get-in sheet [:sheet/file 0 :file/location]) ;TODO why is :sheet/file multivalent???
            ]
        (doseq [[op {:keys [args contents] :as opdef}] (apply dissoc (:operations ops) excused)]
          (testing (str "testing " op)
            (prn :testing op)
            (let [real-args
                  (reduce (fn [acc {:keys [name type]}]
                            (assoc acc name
                                   ;; this checks that argumen types are valid
                                   (case type
                                     :project project
                                     :batch batch
                                     :step nil ;TODO?
                                     :sheet sheet-id
                                     :column column
                                     :columns (list column)
                                     :column-values '("a" "b" "c")
                                     :files (list file)
                                     :local-files nil
                                     :local-directory nil
                                     :rows (list row)
                                     :new-sheet-name "New sheet"
                                     (:string nil) ;:string is default
                                     (case name
                                       :filetype "txt"
                                       :gs-path gs-path
                                       :columns column-name
                                       :template "{c}-{a}"
                                       (str (gensym)))
                                     :boolean false ; Should try both values
                                     :number (rand-int 10)
                                     )))
                          {:op op}
                          args)]
              (is (symbol? (:fn opdef))) ;if not, it should probably be excluded
                ;; run the fn
              (let [results 
                    ((symbol-value (:fn opdef)) real-args)]
                (is (map? results))
                (is (or (sequential? (:txn results))
                        (string? (:message results))
                        (string? (:error results))
                        ))
                )
              )))))))

(deftest replace-values-test
  (tu/with-project-batch project batch 
    (updown/upload-sheets project batch "test/resources/proj2/meta.tsv")
    (tu/with-datomic-context
      (let [sheet-id (sheet/lookup-sheet batch "meta")
            column-a (sheet/lookup-column sheet-id "a")
            column-b (sheet/lookup-column sheet-id "b")]
        (testing "default"
          (let [txn (:txn (replace-values {:sheet sheet-id :column column-b :find "betty" :replace "sabrina"}))]
            (tu/with-txn-sheet txn new-sheet
              (is (tu/set= '({:a "george", :b "jane", :c "elroy"}
                             {:a "barney", :b "sabrina", :c "bam-bam"}
                             {:a "fred", :b "wilma", :c "pebbles"})
                           (sheet/get-sheet-data new-sheet :column-format :keyword))))))
        (testing "inverted"
          (let [txn (:txn (replace-values {:sheet sheet-id :column column-b :find "betty" :replace "sabrina" :invert? true}))]
            (tu/with-txn-sheet txn new-sheet
              (is (tu/set= '({:a "george", :b "sabrina", :c "elroy"}
                             {:a "barney", :b "betty", :c "bam-bam"}
                             {:a "fred", :b "sabrina", :c "pebbles"})
                           (sheet/get-sheet-data new-sheet :column-format :keyword))))))
        (testing "case-insensitive"
          (let [txn (:txn (replace-values {:sheet sheet-id :column column-b :find "Betty" :replace "sabrina" :case-insensitive? true}))]
            (tu/with-txn-sheet txn new-sheet
              (is (tu/set= '({:a "george", :b "jane", :c "elroy"}
                             {:a "barney", :b "sabrina", :c "bam-bam"}
                             {:a "fred", :b "wilma", :c "pebbles"})
                           (sheet/get-sheet-data new-sheet :column-format :keyword))))))
        ;; Susbtring match and substitution
        (testing "substrings"
          (testing "match substrings"
            (let [txn (:txn (replace-values {:sheet sheet-id :column column-a :find "e" :replace "ex" :match-substrings? true}))]
              (tu/with-txn-sheet txn new-sheet
                (is (tu/set= '({:a "ex", :b "jane", :c "elroy"}
                               {:a "ex", :b "betty", :c "bam-bam"}
                               {:a "ex", :b "wilma", :c "pebbles"})
                             (sheet/get-sheet-data new-sheet :column-format :keyword))))))
          (testing "match and replace substrings"
            (let [txn (:txn (replace-values {:sheet sheet-id :column column-a :find "e" :replace "ex" :match-substrings? true :replace-substrings? true}))]
              (tu/with-txn-sheet txn new-sheet
                (is (tu/set= '({:a "gexorgex", :b "jane", :c "elroy"}
                               {:a "barnexy", :b "betty", :c "bam-bam"}
                               {:a "frexd", :b "wilma", :c "pebbles"})
                             (sheet/get-sheet-data new-sheet :column-format :keyword))))))
          (testing "replace substrings sanity check"
            (is (thrown? Exception
                         (replace-values {:sheet sheet-id :column column-a :find "e" :replace "ex" :replace-substrings? true})
                         ))))

        (testing "regex"
          (let [txn (:txn (replace-values {:sheet sheet-id :column column-a :find "[aeiou]" :replace "x" :regex? true :match-substrings? true :replace-substrings? true}))]
            (tu/with-txn-sheet txn new-sheet
              (is (tu/set= '({:a "gxxrgx", :b "jane", :c "elroy"}
                             {:a "bxrnxy", :b "betty", :c "bam-bam"}
                             {:a "frxd", :b "wilma", :c "pebbles"})
                           (sheet/get-sheet-data new-sheet :column-format :keyword)))))
          )

        ;; TODO this option no longer accessible from UI!
        (testing "all columns"
          (let [txn (:txn (replace-values {:sheet sheet-id :column nil :find "a" :replace "sabrina" :match-substrings? true}))]
            (tu/with-txn-sheet txn new-sheet
              (is (tu/set= '({:a "george", :b "sabrina", :c "elroy"}
                             {:a "sabrina", :b "betty", :c "sabrina"}
                             {:a "fred", :b "sabrina", :c "pebbles"})
                           (sheet/get-sheet-data new-sheet :column-format :keyword))))))

        
        ))))
    


;;; Dev helper. 
;;; Pass the real-args from above in here to get a useful error backtrace
(defn- debug [real-args]
  (let [f (symbol-value (get-in ops [:operations (:op real-args) :fn]))]
    (d/wrap-datomic-fn
     #(f real-args))))
    
