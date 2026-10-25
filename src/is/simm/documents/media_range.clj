(ns is.simm.documents.media-range
  "Bounded single-byte-range response planning. No I/O or authorization."
  (:require [clojure.string :as str])
  (:import [java.math BigInteger]))

(defn- natural [value]
  (when (and (string? value) (re-matches #"[0-9]+" value))
    (BigInteger. value)))

(defn plan
  "HEAD ignores Range. Multipart/unknown range units fall back to a full
   response. Malformed or unsatisfiable single byte ranges return416.
   If-Range only matches this resource's strong ETag; other validators request
   the full representation. The caller supplies current authorized metadata."
  [size method range-header if-range etag]
  (when-not (and (integer? size) (<= 0 size Integer/MAX_VALUE)
                 (#{:get :head} method) (string? etag))
    (throw (ex-info "Unsupported media response metadata" {:type ::invalid})))
  (let [full {:status 200 :offset 0 :length size}
        unsatisfiable {:status 416 :offset 0 :length 0 :content-range (str "bytes */" size)}]
    (cond
      (or (= :head method) (nil? range-header) (and if-range (not= if-range etag))) full
      (not (string? range-header)) unsatisfiable
      (> (count range-header) 256) unsatisfiable
      (not (str/starts-with? (str/lower-case (str/trim range-header)) "bytes=")) full
      (str/includes? range-header ",") full
      :else
      (if-let [[_ start end] (re-matches #"(?i)bytes=([0-9]*)-([0-9]*)" (str/trim range-header))]
        (if (or (zero? size) (and (empty? start) (empty? end)))
          unsatisfiable
          (let [a (natural start) b (natural end)
                offset (if a a (max 0 (- size b)))
                last-byte (if a (if b (min b (dec size)) (dec size)) (dec size))]
            (if (or (and (nil? a) (zero? b)) (>= offset size) (> offset last-byte))
              unsatisfiable
              (let [offset (long offset) length (long (inc (- last-byte offset)))]
                {:status 206 :offset offset :length length
                 :content-range (str "bytes " offset "-" (+ offset length -1) "/" size)}))))
        unsatisfiable))))
