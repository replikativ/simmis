(ns is.simm.starter.conversation-digest
  "Optional ordinary editable summarize-room workflow. No installation, provider
   calls, ledger, knowledge selection, cursor or live publication effects."
  (:refer-clojure :exclude [name])
  (:require [clojure.java.io :as io]
            [is.simm.documents.markdown :as markdown]))

(def name "summarize-room")
(def version 1)
(def paths ["workflow.edn" "digest-policy.edn" "checker.clj" "fixtures/workflow-context/conversation-digest.txt"])

(defn source []
  (:files
   (reduce (fn [{:keys [files total]} path]
             (let [resource (or (io/resource (str "is/simm/workflows/summarize-room/v1/" path))
                                (throw (ex-info "Missing conversation digest source" {:path path})))
                   bytes (with-open [stream (io/input-stream resource)] (.readNBytes stream 65537))
                   length (alength bytes) total (+ total length)]
               (when (or (> length 65536) (> total 131072))
                 (throw (ex-info "Conversation digest source exceeds bounds" {:path path})))
               {:files (assoc files path (markdown/decode bytes)) :total total}))
           {:files (sorted-map) :total 0} paths)))
