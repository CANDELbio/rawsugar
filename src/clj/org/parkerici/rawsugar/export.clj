(ns org.parkerici.rawsugar.export
  (:require [me.raynes.fs :as fs]
            [clojure.java.io :as io]
            [org.parkerici.rawsugar.blob :as blob]
            [org.parkerici.rawsugar.files :as files]
            [org.parkerici.rawsugar.datomic :as d]
            [org.parkerici.rawsugar.sheet :as sheet]
            [org.parkerici.rawsugar.request :as request]
            [org.parkerici.rawsugar.slack :as slack]
            [org.parkerici.rawsugar.cnavigate :as c]
            [org.parkerici.multitool.core :as u]
            [org.parkerici.multitool.cljcore :as ju])
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
    
