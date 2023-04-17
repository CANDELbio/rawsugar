(ns org.candelbio.rawsugar.blob
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [environ.core :as env]
            [me.raynes.fs :as fs]
            [org.candelbio.rawsugar.datomic :as d]
            [org.candelbio.multitool.core :as u]
            [taoensso.timbre :as log])
  (:import [com.google.cloud.storage Blob$BlobSourceOption BlobId BlobInfo
            Storage$BlobListOption Storage$BlobSourceOption Storage$BlobWriteOption StorageOptions]
           java.nio.ByteBuffer
           [java.nio.channels Channels ReadableByteChannel WritableByteChannel]
           java.security.MessageDigest
           javax.xml.bind.DatatypeConverter))

;;; Code dealing with mechanics of files and file transfers.
;;; See also updown.clj (upload logic) and files.clj (file entities)

(u/def-lazy service
  (try
    (let [transport-options
          (-> (StorageOptions/getDefaultHttpTransportOptions)
              (.toBuilder)
              (.setConnectTimeout 60000)
              (.setReadTimeout 60000)
              (.build))]
      (-> (StorageOptions/newBuilder)
          (.setTransportOptions transport-options)
          (.setProjectId (:gcs-project env/env))
          (.build)
          (.getService)))
    (catch Throwable e (log/error (str "Couldn't connect to Google Storage: " e)))))

;;; Utilities

(defn gs-path?
  [path]
  (and path (re-find #"^gs://" path)))

;;; Note: this will have to change if there are >2 kinds of supported paths
(defn local-path?
  [path]
  (not (gs-path? path)))

(defn parse-gcs-path
  [path]
  (let [s (str/replace path #"gs://" "")
        v (str/split s #"/")
        bucket-name (first v)
        blob-path (str/join "/" (rest v))]
    [bucket-name blob-path]))

(defn- gs-directory
  [dir]
  (let [[bucket-name path] (parse-gcs-path dir)
        page (.list @service bucket-name (into-array Storage$BlobListOption [(Storage$BlobListOption/prefix path)]))
        blobs (iterator-seq (.iterator (.iterateAll page)))
        filtered (remove #(= 0 (.getSize %)) blobs)] ;Remove zero-length files
    ;; might want option to not make full path, since we just pull it apart later
    (map #(str "gs://" bucket-name "/" (.getName %)) filtered)))


(declare gs-exists?)

(defn exists?
  [dir]
  (if (gs-path? dir)
    (gs-exists? dir)
    (fs/exists? dir)))

(defn check-path
  [dir]
  (when-not dir
    (throw (ex-info (str "Path not specified") {})))
  (when-not (exists? dir)
    (throw (ex-info (str "Path does not exist: " dir) {:type :not-found
                                                       :file dir}))))

;;; Ignore files that start with . (extend to other patterns as need arises)
(def ignored-files-regex #"^\.")

(defn filter-template
  [template seq]
  (if template
    (filter (comp (partial re-matches (u/glob->regex template)) fs/base-name) seq)
    seq))

(defn content-files
  [dir {:keys [template]}]
  (filter-template
   template
   (if (gs-path? dir)
     ;; removes dir listing when gcp returns the dir as a part of the content-files
     (remove #(str/ends-with? % "/") 
             (gs-directory dir))
     (map str
          (filter #(and (not (.isDirectory %))
                        (not (re-find ignored-files-regex (.getName %))))
                  (file-seq (io/file dir)))))))
        

(defn blob-info
  ([bucket-name remote-path mime-type]
   (-> (BlobInfo/newBuilder bucket-name remote-path)
       (.setContentType (String. mime-type))
       (.build)))
  ([bucket-name remote-path]
   (-> (BlobInfo/newBuilder bucket-name remote-path)
       (.build)))
  ([fullpath]
   (apply blob-info (parse-gcs-path fullpath))))

(defn blob-id
  ([bucket path]
   (BlobId/of bucket path))
  ([fullpath]
   (let [[bucket path] (parse-gcs-path fullpath)]
     (BlobId/of bucket path))))

(defn id->blob
  [blob-id]
  (.get @service blob-id))

(defn blob
  ([path]
   (id->blob (apply blob-id (parse-gcs-path path))))
  ([bucket name]
   (id->blob (blob-id bucket name))))

(defn gs-exists?
  [path]
  (try (blob path)
       (catch Throwable _ false)))

(defn gs-delete
  [path]
  (let [blob (blob path)]
    (if blob
      (.delete blob (into-array Blob$BlobSourceOption []))
      (throw (ex-info (str "Not found: " path) {:type :not-found
                                                :file path})))))

(defn get-channel
  ([path mode mime-type]
   (cond
     (gs-path? path)
     (let [v (parse-gcs-path path)
           bucket-name (v 0)
           blob-path (v 1)]
       (cond
         (= mode :writer) (.writer @service (if mime-type
                                             (blob-info bucket-name blob-path mime-type)
                                             (blob-info bucket-name blob-path)) (into-array Storage$BlobWriteOption []))
         (= mode :reader) (.reader @service (blob-id bucket-name blob-path) (into-array Storage$BlobSourceOption []))))
     :else
     (cond (= mode :reader) (Channels/newChannel (io/input-stream path))
           (= mode :writer) (Channels/newChannel (io/output-stream path)))))
  ([path mode]
   (get-channel path mode nil)))

(defn gs-copy
  [source-path target-path]
  (let [source (blob source-path)
        target-id (apply blob-id (parse-gcs-path target-path))
        _ (when-not source
            (throw (ex-info (str "Not found: " source-path)
                            {:type :not-found
                             :file source-path})))
        writer (.copyTo source target-id (into-array Blob$BlobSourceOption []))]
    (while (not (.isDone writer))
      (.copyChunk writer))))

(defonce tika (org.apache.tika.Tika.))

(defn mime-type-of
  [filename]
  (.detect tika filename))

; returns the SHA2 digest of the source or nil if it's a bucket-to-bucket copy
(defn copy
  [source-path target-path]
  (check-path source-path)
  ;; Ensure target dir exists
  (when-not (gs-path? target-path)
    (fs/mkdirs (fs/parent target-path)))
  (let [md (MessageDigest/getInstance "SHA-256")
        buf-size (* 1024 1024)
        buf (ByteBuffer/wrap (byte-array buf-size))]
    (if (and (gs-path? source-path) (gs-path? target-path))
      (do
        ;; in this mode, don't compute hash
        (gs-copy source-path target-path)
        nil)
      ;; else
      (with-open [^WritableByteChannel target-channel (get-channel target-path :writer (mime-type-of source-path))
                  ^ReadableByteChannel source-channel (get-channel source-path :reader)]
        (loop [limit (.read source-channel buf)
               total 0]
          (if (< limit 0)
            (do
              (DatatypeConverter/printHexBinary (.digest md)))
            (do
              (.flip buf)
              (.update md (.duplicate buf)) ; this is necessary not to change the position in the buffer
              (.write target-channel buf)
              (.clear buf)
              (recur (.read source-channel buf) (+ total limit) 
                     ))))))))


;;; Just the hash (not currently used)
;;; TODO there is a DigestInputStream that might make this simpler/more efficient
;;; See also https://github.com/tebeka/clj-digest
;;; Takes about 1.2 seconds for a 150M file (laptop). MD5 is about half that
(defn file-hash
  [source-path]
  (let [md (MessageDigest/getInstance "SHA-256")
        buf-size (* 1024 1024)
        buf (ByteBuffer/wrap (byte-array buf-size))]
    (with-open [^ReadableByteChannel source-channel (get-channel source-path :reader)]
        (loop [limit (.read source-channel buf)
               total 0
               ]
          (if (< limit 0)
            (do
              (DatatypeConverter/printHexBinary (.digest md)))
            (do
              (.flip buf)
              (.update md (.duplicate buf)) ; this is necessary not to change the position in the buffer
              (.clear buf)
              (recur (.read source-channel buf) (+ total limit) 
                     )))))))

(defn extended-extension
  "Returns all extension components, eg for 'dna_reads2.fastq.gz' will return 'fastq.gz'"
  [fname]
  (second (re-matches #".*?\.([\w\.]*)$" fname)))

(defn generate-path
  ([project batch id original-fname]
   (let [project-name (d/get-property project :project/name)
         batch-name (d/get-property batch :batch/name)]
     (if-let [extension (extended-extension original-fname)] 
       (format "gs://%s/%s/%s/%s.%s" (:gcs-bucket env/env) project-name batch-name id extension)
       (format "gs://%s/%s/%s/%s" (:gcs-bucket env/env) project-name batch-name id)))))

(defn get-absolute-file
  [path]
  (if (gs-path? path)
    path
    (str (fs/absolute path))))

(defn is-directory? [path]
  (if (gs-path? path)
    (not (= (list path) (gs-directory path))) ;a somewhat cruddy way to determine if gs: path is directory
    (.isDirectory (io/file path))))

;;; For tests only
(defn copy-directory
  [from to]
  (doseq [file (content-files from {})]
    (copy file (str to (subs file (count from ))))))
  
;;; changed, caller needs to compute dest

(defn download
  "File is entity map. Destination is in to-base, pathname from the file entity unless a :to option is supplied. A :flat? option means ignore the file's path"
  [file to-base {:keys [flatten?] :as options}] 
  (let [path (:file/pathname file)
        path (if flatten? (fs/base-name path) path)
        to (str to-base "/" (or (:to options) path))]
    (if (exists? to)
      (log/infof "Destination %s already exists, skipping" to) ;TODO hash check?
      (do 
        (log/infof "Downloading %s to %s" path to)
        (let [hash (copy (:file/location file) to)]
          (when (and hash (:file/hash file) (not (= hash (:file/hash file))))
            (throw (ex-info (str "Hash mismatch for download: "  file) {:type :not-found
                                                                  :file file}))))))))
  
;;; In general you will not want to call this, because files can be huge. For debugging mostly.
(defn file-contents
  [id]
  (let [entity (d/get-entity id)
        location (:file/location entity)
        temp-file (str (fs/temp-file "rawsugar"))]
    (copy location temp-file)
    (slurp temp-file)))


;;; Memoized so we download a file no more than once.
;;; Called from handler/download and updown/file-sheets
(u/defn-memoized localize-file
  [file-id-or-ent]
  (let [file-ent (d/coerce-entity file-id-or-ent)]
    (or (:local-file file-ent)
        (let [temp-file (str (fs/temp-file "rawsugar"))]
          (copy (:file/location file-ent) temp-file)
          temp-file))))

  

