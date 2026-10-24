(ns is.simm.documents.cell-result
  "Optional, closed data-only presentation of an explicit SCI cell result.
   Call inside the owned evaluation worker. No arbitrary object serialization."
  (:require [clojure.string :as str]
            [is.simm.documents.chart-spec :as chart])
  (:import [java.nio.charset StandardCharsets]))

(def profile {:id :simmis/cell-result :version 1})
(def max-columns 20)
(def max-rows 50)
(def max-cell-chars 512)
(def max-text-bytes 32768)

(defn- refuse! [reason]
  (throw (ex-info "Result presentation was refused" {:type ::refused :reason reason})))

(defn- closed! [value keys]
  (when-not (and (map? value) (not (record? value))
                 (= (count value) (count keys)) (every? #(contains? value %) keys))
    (refuse! :unsupported-shape)))

(defn- text! [value limit]
  (when-not (and (string? value) (<= (count value) limit)) (refuse! :oversized-text))
  ;; Accept intact surrogate pairs, never silently replace malformed UTF-16.
  (loop [i 0]
    (when (< i (count value))
      (let [c (.charAt ^String value i)]
        (cond
          (Character/isHighSurrogate c)
          (if (and (< (inc i) (count value)) (Character/isLowSurrogate (.charAt ^String value (inc i))))
            (recur (+ i 2)) (refuse! :invalid-text))
          (Character/isLowSurrogate c) (refuse! :invalid-text)
          (and (Character/isISOControl c) (not (#{\tab \newline \return} c))) (refuse! :invalid-text)
          :else (recur (inc i))))))
  value)

(def ^:private numeric-classes
  #{Byte Short Integer Long Float Double java.math.BigInteger java.math.BigDecimal clojure.lang.BigInt})

(defn- cell-text! [value]
  (cond
    (nil? value) ""
    (string? value) (text! value max-cell-chars)
    (boolean? value) (if value "true" "false")
    (contains? numeric-classes (class value))
    (do
      ;; Bound conversion BEFORE decimal expansion/printing. Class checks keep
      ;; arbitrary Number.toString/doubleValue implementations out of the worker adapter.
      (when (or (and (instance? java.math.BigInteger value)
                     (> (.bitLength ^java.math.BigInteger value) 1701))
                (and (instance? clojure.lang.BigInt value)
                     (> (.bitLength (.toBigInteger ^clojure.lang.BigInt value)) 1701))
                (and (instance? java.math.BigDecimal value)
                     (or (> (.bitLength (.unscaledValue ^java.math.BigDecimal value)) 1701)
                         (> (.precision ^java.math.BigDecimal value) max-cell-chars)))
                (and (or (instance? Float value) (instance? Double value))
                     (not (Double/isFinite (double value)))))
        (refuse! :invalid-number))
      (text! (str value) max-cell-chars))
    :else (refuse! :unsupported-cell)))

(defn- table! [value]
  (closed! value #{:version :kind :columns :rows})
  (let [{:keys [columns rows]} value]
    (when-not (and (vector? columns) (<= 1 (count columns) max-columns)
                   (vector? rows) (<= (count rows) max-rows)
                   (every? #(and (vector? %) (= (count columns) (count %))) rows))
      (refuse! :invalid-table))
    (let [columns (mapv #(let [label (text! % 128)]
                           (when (str/blank? label) (refuse! :invalid-column)) label) columns)
          ;; At most 1,000 cells; validation never realizes a user sequence.
          rows (mapv #(mapv cell-text! %) rows)
          _ (reduce (fn [n text]
                          (let [n (+ n (alength (.getBytes ^String text StandardCharsets/UTF_8)))]
                            (when (> n max-text-bytes) (refuse! :oversized-table)) n))
                        0 (concat columns (mapcat identity rows)))]
      {:profile profile :kind :table :columns columns :rows rows})))

(defn presentation
  "Return nil for an ordinary result, or a bounded presentation DTO for the
   exact opt-in envelope. Malformed opt-in throws ::refused; callers may attach
   its static diagnostic without changing computation success. No raw value
   escapes this function's successful DTO. Unknown errors are not swallowed."
  [value]
  (when (and (map? value) (contains? value :simmis/presentation))
    (closed! value #{:simmis/presentation})
    (let [data (:simmis/presentation value)]
      (when-not (and (map? data) (not (record? data)) (= 1 (:version data))
                     (integer? (:version data)))
        (refuse! :unsupported-version))
      (case (:kind data)
        :table (table! data)
        :chart (do
                 (closed! data #{:version :kind :data})
                 (try
                   {:profile profile :kind :chart :data (chart/validate-data (:data data))}
                   (catch clojure.lang.ExceptionInfo error
                     (if (= :is.simm.documents.chart-spec/refused (:type (ex-data error)))
                       (refuse! :invalid-chart) (throw error)))))
        (refuse! :unsupported-kind)))))
