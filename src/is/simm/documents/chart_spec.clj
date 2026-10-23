(ns is.simm.documents.chart-spec
  "Closed, bounded data-only chart codec. No source resolution, evaluation,
   URLs, schema installation or authorization. Keys stay strings while parsing."
  (:require [clojure.string :as str]
            [is.simm.documents.chart-contract :as contract]
            [jsonista.core :as json])
  (:import [com.fasterxml.jackson.core JsonFactory JsonParser$Feature StreamReadConstraints]
           [com.fasterxml.jackson.databind DeserializationFeature ObjectMapper]
           [java.nio ByteBuffer CharBuffer]
           [java.nio.charset StandardCharsets CodingErrorAction]))

(def profile contract/data-profile)
(def max-bytes contract/max-bytes)
(def max-points contract/max-points)
(def max-series contract/max-series)
(def max-magnitude 1.0e12)

(def ^:private mapper
  ;; This immutable-after-construction codec is shared, not chart/session state.
  (let [constraints (-> (StreamReadConstraints/builder)
                        (.maxNestingDepth 8) (.maxStringLength 256)
                        (.maxNameLength 32) (.maxNumberLength 64)
                        (.maxTokenCount 20000) (.build))
        factory (doto (JsonFactory.)
                  (.setStreamReadConstraints constraints)
                  (.enable JsonParser$Feature/STRICT_DUPLICATE_DETECTION))]
    (doto (json/object-mapper {:factory factory :bigdecimals true})
      (.enable DeserializationFeature/FAIL_ON_TRAILING_TOKENS))))

(defn- refuse! [reason]
  (throw (ex-info "Chart data was refused" {:type ::refused :reason reason})))

(defn- input-bytes [source]
  (let [data (cond
               (bytes? source) source
               (and (string? source) (<= (count source) max-bytes))
               (try
                 (let [encoder (doto (.newEncoder StandardCharsets/UTF_8)
                                 (.onMalformedInput CodingErrorAction/REPORT)
                                 (.onUnmappableCharacter CodingErrorAction/REPORT))
                       buffer (.encode encoder (CharBuffer/wrap ^String source))
                       result (byte-array (.remaining buffer))]
                   (.get buffer result)
                   result)
                 (catch java.nio.charset.CharacterCodingException _ (refuse! :invalid-utf8)))
               :else (refuse! :invalid-or-oversized-input))]
    (when (> (alength ^bytes data) max-bytes) (refuse! :oversized-input))
    data))

(defn- closed! [value required optional]
  (when-not (and (map? value) (every? #(contains? value %) required)
                 (every? (into required optional) (keys value)))
    (refuse! :unsupported-shape)))

(defn- label! [value]
  (when-not (and (string? value) (<= 1 (count value) 128)
                 (not (str/blank? value))
                 (not (re-find #"[\p{Cntrl}\p{Cs}]" value)))
    (refuse! :invalid-label))
  value)

(defn- number! [value]
  (when-not (number? value) (refuse! :invalid-coordinate))
  (let [n (double value)]
    (when-not (and (Double/isFinite n) (<= -1000000000000M value 1000000000000M)
                   ;; Never silently turn a tiny nonzero measurement into zero.
                   (or (not (zero? n)) (zero? value)))
      (refuse! :invalid-coordinate))
    n))

(defn decode
  "Decode at most 64 KiB of UTF-8 JSON into a fixed projection DTO. Strings and
   byte arrays only: never a File, URL, Reader or arbitrary Jsonista input.
   Line/point x values are numbers; bar x values are category labels. Point
   order is retained. Missing categories do not become zero-valued data."
  [source]
  (let [data (input-bytes source)
        ;; Jackson's byte parser auto-detects UTF-16/32. Decode ourselves and
        ;; feed its STRING parser so the format really is UTF-8 only.
        text (try (str (.decode (doto (.newDecoder StandardCharsets/UTF_8)
                                 (.onMalformedInput CodingErrorAction/REPORT)
                                 (.onUnmappableCharacter CodingErrorAction/REPORT))
                               (ByteBuffer/wrap data)))
                  (catch java.nio.charset.CharacterCodingException _ (refuse! :invalid-utf8)))
        raw (try (json/read-value text ^ObjectMapper mapper)
                 (catch com.fasterxml.jackson.core.JacksonException _ (refuse! :invalid-json)))]
    (closed! raw #{"version" "kind" "series"} #{"title" "x-label" "y-label"})
    (when-not (and (integer? (get raw "version")) (= 1 (get raw "version")))
      (refuse! :unsupported-version))
    (let [kind (case (get raw "kind") "line" :line "bar" :bar "point" :point
                     (refuse! :unsupported-kind))
          series (get raw "series")]
      (when-not (and (vector? series) (<= 1 (count series) max-series))
        (refuse! :invalid-series))
      (let [series (mapv (fn [row]
                           (closed! row #{"label" "points"} #{})
                           (let [points (get row "points")]
                             (when-not (and (vector? points) (<= 1 (count points) max-points))
                               (refuse! :invalid-points))
                             {:label (label! (get row "label"))
                              :points (mapv (fn [point]
                                              (when-not (and (vector? point) (= 2 (count point)))
                                                (refuse! :invalid-point))
                                              [(if (= :bar kind) (label! (first point))
                                                   (number! (first point)))
                                               (number! (second point))]) points)})) series)]
        (when (> (reduce + (map (comp count :points) series)) max-points)
          (refuse! :too-many-points))
        (when-not (= (count series) (count (set (map :label series))))
          (refuse! :duplicate-series-label))
        (when (and (= :bar kind)
                   (some #(not= (count (:points %)) (count (set (map first (:points %))))) series))
          (refuse! :duplicate-category))
        (reduce (fn [result [source-key key]]
                  (if (contains? raw source-key)
                    (assoc result key (label! (get raw source-key))) result))
                {:profile profile :kind kind :series series}
                [["title" :title] ["x-label" :x-label] ["y-label" :y-label]])))))
