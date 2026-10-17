(ns is.simm.ops.worker-frames
  "Bounded LF-delimited JSON for private worker process pipes.

   A guest can forge every guest frame, including ready/result. Framing is
   parsing, not executor attestation. Only the external controller establishes
   physical shutdown; the broker independently authorizes inference requests.
   The owner must impose deadlines and close pipes/stop processes on failure."
  (:require [jsonista.core :as json])
  (:import [com.fasterxml.jackson.core JsonParser$Feature StreamReadConstraints]
           [com.fasterxml.jackson.databind DeserializationFeature]
           [java.io BufferedInputStream ByteArrayOutputStream InputStream OutputStream]
           [java.nio ByteBuffer]
           [java.nio.charset CodingErrorAction StandardCharsets]))

(def policy {:version 1 :max-frame-bytes (+ 4194304 4096) :max-depth 32
             :max-nodes 50000 :max-tokens 100000})

(defn- refuse! []
  ;; Never expose input, parser errors, paths or credentials in the exception.
  (throw (ex-info "Invalid local worker process frame" {:type ::refused})))

(def ^:private mapper
  (let [m (json/object-mapper)]
    (.configure m JsonParser$Feature/STRICT_DUPLICATE_DETECTION true)
    (.configure m DeserializationFeature/FAIL_ON_TRAILING_TOKENS true)
    (.setStreamReadConstraints
     (.getFactory m)
     (-> (StreamReadConstraints/builder)
         (.maxNestingDepth (:max-depth policy))
         (.maxStringLength (:max-frame-bytes policy))
         (.maxNumberLength 64)
         (.build)))
    m))

(defn channel
  "Create ONE wrapper per pipe pair; never recreate between frames (buffered
   reads may already contain the next frame). Does not own/close either pipe.
   Session-local I/O state is external to forkable application state."
  [^InputStream in ^OutputStream out]
  (when-not (and in out) (refuse!))
  {:in (BufferedInputStream. in) :out out :closed? (atom false)
   :read-lock (Object.) :write-lock (Object.)})

(defn- limit! [limit]
  (when-not (and (integer? limit) (<= 1 limit (:max-frame-bytes policy))) (refuse!)))

(defn- string! [^String s limit]
  (when (> (.length s) limit) (refuse!))
  ;; JSON escapes can encode lone UTF-16 surrogates even when the raw frame
  ;; is valid UTF-8. Reject them rather than silently changing names/content
  ;; when a later UTF-8 encoder replaces an invalid code unit.
  (loop [i 0]
    (when (< i (.length s))
      (let [c (.charAt s i)]
        (cond
          (Character/isHighSurrogate c)
          (if (and (< (inc i) (.length s)) (Character/isLowSurrogate (.charAt s (inc i))))
            (recur (+ i 2)) (refuse!))
          (Character/isLowSurrogate c) (refuse!)
          :else (recur (inc i)))))))

(defn- tokens! [^String text]
  ;; Bound the object graph BEFORE databinding allocates maps/vectors. The
  ;; byte cap alone would allow millions of tiny array members in one frame.
  (with-open [parser (.createParser (.getFactory mapper) text)]
    (loop [remaining (:max-tokens policy)]
      (when (.nextToken parser)
        (when (zero? remaining) (refuse!))
        (recur (dec remaining))))))

(defn- data! [value limit]
  (let [nodes (volatile! (:max-nodes policy))]
    (letfn [(visit [v depth]
              (when (or (> depth (:max-depth policy)) (neg? (vswap! nodes dec))) (refuse!))
              (cond
                (or (nil? v) (boolean? v)) nil
                (string? v) (string! v limit)
                (integer? v) (when-not (<= Long/MIN_VALUE v Long/MAX_VALUE) (refuse!))
                (float? v) (when-not (Double/isFinite (double v)) (refuse!))
                (vector? v) (doseq [item v] (visit item (inc depth)))
                (map? v) (doseq [[k item] v]
                           ;; Keep all keys as strings, including namespaced or
                           ;; leading-colon filenames. No keyword/name coercion.
                           (when-not (string? k) (refuse!))
                           (visit k (inc depth))
                           (visit item (inc depth)))
                :else (refuse!)))]
      (when-not (map? value) (refuse!))
      (visit value 0)))
  value)

(defn read!
  "Read one object or return nil on clean EOF. Truncated/invalid/oversized
   frames permanently poison the channel. No recovery or resynchronization."
  [ch limit]
  (locking (:read-lock ch)
    (try
      (limit! limit)
      (when @(:closed? ch) (refuse!))
      (let [^InputStream in (:in ch)
            buffer (ByteArrayOutputStream.)
            payload (loop [size 0]
                      (let [b (.read in)]
                        (cond
                          (= b -1) (if (zero? size) nil (refuse!))
                          (= b 10) (.toByteArray buffer)
                          (>= size limit) (refuse!)
                          :else (do (.write buffer b) (recur (inc size))))))]
        (if (nil? payload)
          (do (reset! (:closed? ch) true) nil)
          (let [decoder (doto (.newDecoder StandardCharsets/UTF_8)
                          (.onMalformedInput CodingErrorAction/REPORT)
                          (.onUnmappableCharacter CodingErrorAction/REPORT))
                text (str (.decode decoder (ByteBuffer/wrap payload)))]
            (tokens! text)
            (data! (json/read-value text mapper) limit))))
      (catch Throwable _
        (reset! (:closed? ch) true)
        (refuse!)))))

(defn write!
  "Encode fully into a bounded buffer BEFORE writing any bytes to the pipe.
   Validation refusal writes nothing. A pipe failure may leave a partial
   frame and poisons the channel. Values must use string object keys."
  [ch value limit]
  (locking (:write-lock ch)
    (try
      (limit! limit)
      (when @(:closed? ch) (refuse!))
      (data! value limit)
      (let [buffer (ByteArrayOutputStream.)
            bounded (proxy [OutputStream] []
                      (write
                        ([b]
                         (when (>= (.size buffer) limit) (refuse!))
                         (.write buffer (int b)))
                        ([b offset length]
                         (when (> (+ (.size buffer) length) limit) (refuse!))
                         (.write buffer ^bytes b (int offset) (int length)))))
            ^OutputStream out (:out ch)]
        (json/write-value bounded value mapper)
        (.write out (.toByteArray buffer))
        (.write out (int 10))
        (.flush out))
      nil
      (catch Throwable _
        (reset! (:closed? ch) true)
        (refuse!)))))
