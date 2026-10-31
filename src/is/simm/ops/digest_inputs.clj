(ns is.simm.ops.digest-inputs
  "Pure opt-in admission check for ordinary optional digest workflows. No source
   discovery, provider, knowledge selection, cursors or authorization."
  (:require [clojure.edn :as edn]
            [datahike.blob :as blob]
            [is.simm.documents.markdown :as markdown]
            [is.simm.model.conversation-digest :as digest])
  (:import [java.io PushbackReader StringReader]))

(def declaration-value {:version 1 :output "/out/digest.edn"})
(def max-original-bytes (* 256 1024))

(defn- refuse! [] (throw (ex-info "Select exact frozen originals before running the digest" {:type ::held})))

(defn- read-value [text limit]
  (when-not (and (string? text) (<= (count text) limit)) (refuse!))
  (try
    (with-open [reader (PushbackReader. (StringReader. text))]
      (let [eof (Object.) options {:eof eof :readers {} :default (fn [& _] (refuse!))}
            value (edn/read options reader)]
        (when (or (identical? eof value) (not (identical? eof (edn/read options reader)))) (refuse!))
        value))
    (catch StackOverflowError _ (refuse!))
    (catch Exception _ (refuse!))))

(defn declaration [source]
  (when (contains? source "digest-policy.edn")
    (when-not (= declaration-value (read-value (get source "digest-policy.edn") 1024)) (refuse!))
    declaration-value))

(defn validate-inputs!
  "Returns the exact selection for opted-in source, nil for historical/generic
   source. All selected files need mandatory retained file-content IDs matching
   exact frozen UTF-8 bytes. Supplied source attribution is informational only."
  [source params inputs]
  (when (declaration source)
    (let [selection (digest/validate-selection!
                     (read-value (or (get params "evidence-manifest") (:evidence-manifest params))
                                 digest/max-selection-characters))]
      (when-not (map? inputs) (refuse!))
      (reduce (fn [total {:keys [path file-content]}]
                (let [text (get inputs (str "/" path))]
                  (when-not (and (uuid? file-content) (string? text) (<= (count text) max-original-bytes))
                    (refuse!))
                  (let [bytes (markdown/source-bytes text) total (+ total (alength bytes))]
                    (when-not (and (<= total max-original-bytes) (= file-content (blob/blob-id bytes))) (refuse!))
                    total))) 0 (:sources selection))
      selection)))
