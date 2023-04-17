(ns org.candelbio.rawsugar.schema
  (:require [org.candelbio.multitool.core :as u]
            [org.candelbio.alzabo.datomic :as alzd]
            [org.candelbio.alzabo.output :as alzo]
            [org.candelbio.rawsugar.datomic :as d]
            [org.candelbio.multitool.cljcore :as ju]
            [clojure.java.io :as io]
            )
  )

;;; NOTE: whenever this changes, run the function (transact-schema) to update all databases.
;;; Also note that some schema changes are outlawed by Datomic, so verify it works.
;;; NOTE: additional fields need to be added to projects/project-spec
;;; to be visible by default on the client.
;;; NOTE: In retrospect it would have been better to have component → owner links rather than the inverse. 


(def schema
  {:title "Rawsugar Schema"
   :version "3.0"
   :kinds
   {:object
    {;; TODO Make Alzabo support this
     #_ :abstract #_ true
     :fields
     {:creation-op {:type :operation}}}
    :project
    {:fields
     {:name {:type :string
             :unique-id true} 
      :description {:type :string}
      :batches {:type :batch
                :component true
                :cardinality :many
                }
      :history {:type :operation
                :component true
                :cardinality :many}
      :dashboard {:type :string
                  :doc "Link to Smartsheet dashboard for project"}
      ;; A redundant pointer to most recent history entry
      :last-op {:type :operation}
      }}

    :batch
    {:fields
     {
      ;; Basic identity and metadata
      :name {:type :string}      
      :description {:type :string}
      :owner {:type :user}                ;or just email
      :date-created {:type :date}         ;or not, it's in history after all
      :datatype {:type :datatype :doc "Biological data type eg :datatype.vectra"}

      ;; Batch contents (sheets and files)
      :files {:type :file ; TODO Turned OFF :component true, may be hard to change in existing db schema>
              :cardinality :many} 
      :sheets {:type :sheet             ; same
               :cardinality :many}

      ;; Research ops fields, these are mostly for 
      :status {:type :batch-status :doc "Research ops status eg :batch-status.shipped" }
      :request {:type :string :doc "Reasearch Ops request number"}
      :shipment {:type :string :doc "Reasearch Ops shipment number"}
      :dashboard {:type :string :doc "EDN for dashboard spec"}


      ;; Recipe and runs
      :recipe {:type :keyword}          ;Recipes are in edn, not datomic. TODO this may not be needed if datatype/recipe is 1-1
;;;    :runs {:type :run :cardinality :many}
      :run-steps {:type :run-step :cardinality :many}

      ; :derived-from {:type :batch}

      }
     }


    :run-step
    {:fields
     {
      ;; Identity 
      :step-id {:type :keyword}
      :predecessors {:type :run-state :cardinality :many}

      ;; state
      :state {:type :run-step-state} ;TODO
      ;;       :locked?   conceptually, but would be redundnt with state
      ;;   TODO how are multiple versions and different run-tree represented?

      ;; Metadata
      :agent {:type :string}
      :start {:type :instant}
      :end {:type :instant}
      :notes {:type :string}
;;; RS3
      ;; :files {:type :file
      ;;         :component true 
      ;;         :cardinality :many}
      ;; For now, singular
      ;; :sheets {:type :sheet              
      ;;          :cardinality :many}
      :sheet {:type :sheet}

      ;; TODO add
      ;; input-sheet
      ;; output-sheet
      }}

    :sheet
    ;; Note: any changes here should be reflected in sheet/get-entity
    {:fields
     {:name {:type :string}
      :derived-from {:type :sheet}      ;optional
      :file {:type :file                ; the spreadsheet file itself, if sheet is from an upload
             :component false           ;Was component
             :cardinality :many          ; Due to updates; will only be singular in the more typical case.
             }
      :columns {:type :column
                :component false        ;was true
                :cardinality :many}
      :rows {:type :row
             :component false           ;was true
             :cardinality :many}
      }}
    :column
    {:fields
     {:name {:type :string}
      :index {:type :long}
      }}
    :row
    {:fields {:cells {:type :cell
                      :component false  ;was true
                      :cardinality :many}
              :files {:type :file
                      :cardinality :many}
              }}
    :cell
    {:fields {:column {:type :column
                       :required true}
              ;; TODO :annotation 
              :value_string {:type :string }
              :value_long {:type :long}
              :value_float {:type :float}
              ;; TODO Nothing generates the following yet, but here for future expansion
              :value_instant {:type :instant} 
              :value_boolean {:type :boolean}
              :value_ref {:type :ref}        
              }}
    :file
    {:fields
     {:pathname {:type :string}              ;full original pathname
      :extension {:type :string}
      :location {:type :string :unique-id true} ;cloud storage location
      :hash {:type :string}                  
      ;; other metadata, like size, time, type?
      ;; New in 3.0 to enrich file display
      :size {:type :long}
      :created {:type :instant}

      ; :derived-from {:type :sheet}      ;optional
      }}
    :operation
    {:fields {:cmd {:type :keyword}     ;TODO prob want an enum type for this
              :parameters {:type :string} ;TODO maybe a more detailed representaion, but for now can be serialzied map
              :time {:type :instant}      ;new in 2.0
              :agent {:type :string}}     ;make this an object?
     }}
   :enums
   {:datatype
    {:doc "Translational data type"
     :values
     {
      :datatype.default "Default"
      :datatype.personalis "Personalis Sequencing"
      :datatype.isoplexis "Isoplexis"
      :datatype.atac-seq "ATAC-seq"
      :datatype.sc-atac-seq "scATAC-seq"
      :datatype.sn-atac-seq "snATAC-seq"
      :datatype.chip-seq "Chip-seq/ATAC"
      :datatype.hichip "HiChIP"
      :datatype.wes "Whole Exome Sequencing"
      :datatype.wgs "Whole Genome Sequencing"
      :datatype.genotyping "Genotyping"
      :datatype.rna-seq "RNA-seq"
      :datatype.sc-rna-seq "scRNA-seq"
      :datatype.sn-rna-seq "snRNA-seq"
      :datatype.nanostring "Nanostring"
      :datatype.visium "Spatial Transcriptomics (Visium)"
      :datatype.dsp "Nanostring DSP"
      :datatype.vectra "Vectra"
      :datatype.imc "Imaging Mass Cytometry (IMC)"
      :datatype.mibi "Multiplexed Ion Beam Imaging (MIBI)"
      :datatype.codex "CO-Detection by indEXing (CODEX)"
      :datatype.metabolomics "Metabolomics"
      :datatype.16s "16S Microbiome Sequencing"
      :datatype.wms "Whole Metagenome Sequencing (WMS)"
      :datatype.flow-cytometry "Flow Cytometry"
      :datatype.cytof "CyToF"
      :datatype.cite-seq "CITE-seq"
      :datatype.luminex "Luminex"
      :datatype.tcr-b "TCR beta sequencing"
      :datatype.tcr-a "TCR alpha sequencing" 
      :datatype.tcr "TCR sequencing"
      }}
    :batch-status
    {:values
     {
      :batch-status.prepare-for-shipment  "Shipment in preparation"
      :batch-status.shipped "Sample shipped to vendor"
      :batch-status.data-received "Data received"
      :batch-status.candelized "Data in CANDEL"
      }}
    :run-step-state
    {:values
     {:run-step-state.done "Done"
      :run-step-state.in-progress "In progress"
      :run-step-state.failed "Failed"                  ;?
      :run-step-state.todo "To do"
      :run-step-state.blocked "Blocked"                ;better name?
      }}
    }})
   

(defn candel-schema
  []
  (read-string (slurp (io/resource "candel-schema.edn"))))

(defn merged-schema
  "A schema that has both Candel and Rawsugar definitions. This may be a bad idea long-term, but works for now."
  []
  (u/merge-recursive schema (dissoc (candel-schema) :title :version)))

;;; Should be in Alzabo
(defn write-alz-schema [file]
  (ju/schppit file schema))

(defn transact-schema
  []
  (let [datomic-schema (alzd/datomic-schema schema)]
    (alzo/write-schema schema "resources/alz-schema.edn")
    (alzo/write-schema datomic-schema "resources/schema.edn")                   ;record for posterity
    (d/wrap-datomic-fn
     #(d/transact datomic-schema))))




