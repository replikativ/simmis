(ns is.simm.starter.daily-room-summary
  "Inactive ordinary source. Loading it selects no model and enables no work."
  (:refer-clojure :exclude [name])
  (:require [clojure.java.io :as io]
            [is.simm.documents.markdown :as markdown]))

(def name "summarize-room-daily")
(def version 2)
(def paths ["workflow.edn" "summary-policy.edn" "fixtures/workflow-context/daily-summary.txt"])

(defn source []
  (into (sorted-map)
        (map (fn [path]
               (let [resource (or (io/resource (str "is/simm/workflows/summarize-room/v2/" path))
                                  (throw (ex-info "Daily summary source is missing" {:path path})))
                     bytes (with-open [stream (io/input-stream resource)] (.readNBytes stream 65537))]
                 (when (> (alength bytes) 65536)
                   (throw (ex-info "Daily summary source exceeds its bound" {:path path})))
                 [path (markdown/decode bytes)])) paths)))
