(ns org.candelbio.rawsugar.export
  (:require [me.raynes.fs :as fs]
            [clojure.java.io :as io]
            [org.candelbio.rawsugar.blob :as blob]
            [org.candelbio.rawsugar.files :as files]
            [org.candelbio.rawsugar.datomic :as d]
            [org.candelbio.rawsugar.sheet :as sheet]
            [org.candelbio.rawsugar.request :as request]
            [org.candelbio.rawsugar.slack :as slack]
            [org.candelbio.rawsugar.cnavigate :as c]
            [org.candelbio.multitool.core :as u]
            [org.candelbio.multitool.cljcore :as ju])
  (:import [java.util.zip ZipOutputStream ZipEntry]))

(defn zip-directory
  "Zip a directory into a tmp file, return the path"
  [dir filename]
  (let [path (fs/temp-file filename ".zip")]
    (with-open [s (io/output-stream path)
                zip (ZipOutputStream. s)
                wrt (io/output-stream zip)]
      (binding [*out* wrt]
        (doseq [file (files/list-dir-recursive dir)]
          (.putNextEntry zip (ZipEntry. (subs (str file) (count (str dir)))))
          (io/copy (io/input-stream file) wrt)
          (.flush wrt)
          )))
    path))

(defn export-to-zip
  [sheet template]
  (let [directory (fs/temp-dir "rawsugar")
        sheet-data (sheet/get-sheet-data sheet :files? true :column-format :entity)
        sheet-name (:sheet/name (d/get-entity sheet))
        sheet-path (str directory "/" sheet-name)]
    (with-open [s (io/writer sheet-path)]
      (binding [*out* s]
        (sheet/export-sheet sheet-data)))
    (doseq [row sheet-data
            file (:files row)]
      (blob/download file
                     directory
                     {:to (and template (files/templated-file template file row))}))
    (let [zip (zip-directory directory (str "rawsugar-export-" sheet-name "-"))]
      (fs/delete-dir directory)
      zip)))

(defn export-url
  [zipfile]
  (format "%s/export?file=%s" (request/origin) (ju/base64-encode (str zipfile))))
    
(defn start-export-job
  [sheet template]
  (when template
    (u/validate-template template (set (sheet/sheet-column-names sheet))))
  (slack/start-background-task
   (format "export of %s" (slack/sheet-mrkdwn-link sheet))
   (fn []
     (let [zip (export-to-zip sheet template)]
       (format "resuts are <%s|downloadable here>." (export-url zip))))
   ))
    
