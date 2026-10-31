(ns is.simm.ops.room-collection-policy
  "Explicit source-bundle collection opt-in; parsing has no effects."
  (:require [clojure.edn :as edn])
  (:import [java.io PushbackReader StringReader]))

(def policy {:version 1 :sources [:current-room-conversation :archived-capture-files]
             :max-messages 20 :max-bytes 524288 :max-scan-messages 1000})

(defn declaration [source]
  (when-let [text (get source "collection-policy.edn")]
    (let [fail #(throw (ex-info "Invalid room collection policy" {:type ::invalid}))]
      (when-not (and (string? text) (<= (count text) 4096)) (fail))
      (try
        (with-open [r (PushbackReader. (StringReader. text))]
          (let [eof (Object.) opts {:eof eof :readers {} :default (fn [& _] (fail))}
                value (edn/read opts r)]
            (when-not (and (= policy value) (identical? eof (edn/read opts r))) (fail))
            value))
        (catch Exception _ (fail))
        (catch StackOverflowError _ (fail))))))
