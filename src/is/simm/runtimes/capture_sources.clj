(ns is.simm.runtimes.capture-sources
  "HOST INTERNAL: verify legacy capture sources before native file publication.
   No endpoints, authorization, inference or publication. Caller retains the
   original App operation through synchronous consumption; host file values
   must never be returned to a client or persisted in a preview."
  (:require [clojure.java.io :as io]
            [datahike.blob :as blob]
            [dvergr.drive.blobs :as blobs]
            [is.simm.runtimes.media-cache :as cache])
  (:import [java.io File FileInputStream]
           [java.security DigestInputStream MessageDigest]
           [org.apache.commons.io.input BoundedInputStream]))

(def image-limit (* 8 1024 1024))
(def image-total-limit (* 64 1024 1024))

(defn- refuse! [reason]
  (throw (ex-info "Capture source unavailable" {:type ::refused :reason reason})))

(defn- digest-hex [^MessageDigest digest]
  (apply str (map #(format "%02x" %) (.digest digest))))

(defn- valid-source! [{:keys [file size legacy-blob]}]
  (when-not (and (instance? File file) (integer? size) (<= 0 size Integer/MAX_VALUE)
                 (string? legacy-blob) (re-matches #"[0-9a-f]{64}" legacy-blob))
    (refuse! :invalid-source)))

(defn- with-source! [{:keys [file size legacy-blob] :as source} consume!]
  (valid-source! source)
  (let [digest (MessageDigest/getInstance "SHA-256")]
    (with-open [raw (FileInputStream. ^File file)
                hashed (DigestInputStream. raw digest)
                bounded (.get (doto (BoundedInputStream/builder)
                                (.setInputStream hashed) (.setMaxCount (inc size))))]
      (let [result (consume! bounded)]
        (when-not (= size (.getCount bounded)) (refuse! :size-mismatch))
        (when-not (= legacy-blob (digest-hex digest)) (refuse! :legacy-digest-mismatch))
        result))))

(defn inspect-file!
  "Read a fixed trusted legacy recording through a bounded stream. Return
   HOST-only source facts with its canonical logical content identity."
  [source]
  (assoc source :content (with-source! source blob/blob-id)))

(defn with-staged-file!
  "Copy and reverify an inspected recording into the App's private cache.
   Only that immutable copy, never the mutable legacy path, reaches consume!."
  [resource {:keys [content size] :as source} consume!]
  (when-not (uuid? content) (refuse! :uninspected-source))
  (valid-source! source)
  (cache/with-materialized-file!
    resource content size
    (fn [out] (with-source! source #(io/copy % out)))
    consume!))

(defn stage-image!
  "Bound a legacy image before hydration. Return only canonical identity and
   size; its bytes remain in the private App cache. Caller accounts each size
   against the batch remainder and keeps at most 32 selected observations."
  [resource legacy-blob remaining-bytes]
  (when-not (and (integer? remaining-bytes) (<= 0 remaining-bytes image-total-limit))
    (refuse! :invalid-remainder))
  (let [bytes (blobs/get-bytes-bounded legacy-blob (min image-limit remaining-bytes))]
    (when-not bytes (refuse! :missing-image))
    (let [content (blob/blob-id bytes) size (alength ^bytes bytes)
          prefix? (fn [offset expected]
                    (and (<= (+ offset (count expected)) size)
                         (= expected (mapv #(bit-and 255 (aget ^bytes bytes (+ offset %))) (range (count expected))))))
          extension (cond
                      (prefix? 0 [255 216 255]) "jpg"
                      (prefix? 0 [137 80 78 71 13 10 26 10]) "png"
                      (and (prefix? 0 [82 73 70 70]) (prefix? 8 [87 69 66 80])) "webp"
                      :else (refuse! :unsupported-image))]
      (cache/materialize! resource content size #(.write % ^bytes bytes))
      {:content content :size size :extension extension})))
