(ns org.candelbio.rawsugar.glue.utils
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]
            ))

(defn write-tsv
  "Write a vector of maps to a tsv. [{:sample A :value B}]
   Pass header-keys of the column names [:sample :value]"
  [data path header-keys]
  (let [columns header-keys
        headers (map name columns)
        rows (mapv #(mapv % columns) data)]
    (with-open [file (io/writer path)]
      (csv/write-csv file (cons headers rows) :separator \tab))))

(defn clean-string-for-terra
  "Replace special characters terra can't parse with underscore."
  ([s]
   (if (string? s)
     (str/replace s #"[ /]" "_")
     s)))
