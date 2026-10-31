(ns is.simm.model.summary-envelope
  "Pure retained-byte validation shared by admission and native publication."
  (:require [clojure.edn :as edn]
            [datahike.blob :as blob]
            [is.simm.model.room-summaries :as summaries])
  (:import [java.io PushbackReader StringReader]
           [java.nio CharBuffer]
           [java.nio.charset StandardCharsets CodingErrorAction]))

(defn- refuse! [reason]
  (throw (ex-info "Retained summary inputs are invalid" {:type ::invalid :reason reason})))

(defn text-bytes [text]
  (when-not (string? text) (refuse! :missing-original))
  (let [buffer (.encode (doto (.newEncoder StandardCharsets/UTF_8)
                         (.onMalformedInput CodingErrorAction/REPORT)
                         (.onUnmappableCharacter CodingErrorAction/REPORT))
                       (CharBuffer/wrap ^String text))
        bytes (byte-array (.remaining buffer))]
    (.get buffer bytes) bytes))

(defn read-value [text limit]
  (when-not (and (string? text) (<= (count text) limit)) (refuse! :invalid-value))
  (with-open [reader (PushbackReader. (StringReader. text))]
    (let [eof (Object.) opts {:eof eof :readers {} :default (fn [& _] (refuse! :invalid-value))}
          value (edn/read opts reader)]
      (when (or (identical? eof value) (not (identical? eof (edn/read opts reader))))
        (refuse! :invalid-value))
      value)))

(defn extract [selection entries]
  (select-keys entries (into #{summaries/manifest-path}
                            (map #(str "/" (:path %)))
                            (into (:coverage selection) (:context selection)))))

(defn validate! [selection inputs]
  (summaries/validate-selection! selection)
  (when-not (and (map? inputs) (= inputs (extract selection inputs))
                 (= selection (read-value (get inputs summaries/manifest-path) 65536)))
    (refuse! :invalid-manifest))
  (let [total
        (reduce (fn [n source]
                  (let [text (get inputs (str "/" (:path source)))
                        _ (when-not (and (string? text) (<= (count text) summaries/max-input-bytes))
                            (refuse! :missing-or-oversized-original))
                        bytes (text-bytes text)]
                    (when-not (= (:file-content source) (blob/blob-id bytes)) (refuse! :changed-original))
                    (+ n (alength bytes))))
                0 (into (:coverage selection) (:context selection)))]
    (when (> total summaries/max-input-bytes) (refuse! :period-byte-limit)))
  selection)
