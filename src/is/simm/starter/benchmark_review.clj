(ns is.simm.starter.benchmark-review
  "Inert advisory review source; confers no grading or publication authority."
  (:require [clojure.java.io :as io]
            [is.simm.documents.markdown :as markdown]))

(def version 1)
(def paths ["workflow.edn" "diagram.mmd" "README.md" "DEVELOPMENT.md"
            "checker.clj" "calibration.edn" "gold.edn"
            "fixtures/inputs/review-packet.edn"])

(defn source []
  (:files
   (reduce (fn [{:keys [files total]} path]
             (let [resource (or (io/resource (str "is/simm/workflows/benchmark-review/v1/" path))
                                (throw (ex-info "Missing benchmark-review source" {:path path})))
                   bytes (with-open [stream (io/input-stream resource)] (.readNBytes stream 65537))
                   length (alength bytes)
                   total (+ total length)]
               (when (or (> length 65536) (> total 131072))
                 (throw (ex-info "Benchmark-review source exceeds bounds" {:path path})))
               {:files (assoc files path (markdown/decode bytes)) :total total}))
           {:files (sorted-map) :total 0} paths)))
