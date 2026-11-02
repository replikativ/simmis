(ns is.simm.starter.evidence-check
  "Inert editable evidence-check workflow and synthetic evaluation example."
  (:refer-clojure :exclude [name])
  (:require [clojure.java.io :as io]
            [is.simm.documents.markdown :as markdown]))

(def name "evidence-check")
(def version 1)
(def paths ["workflow.edn"
             "diagram.mmd"
             "benchmark.edn"
             "checker.clj"
             "README.md"
             "cases/supported/fixtures/inputs/note.txt"
             "cases/supported/fixtures/inputs/claim.txt"
             "cases/supported/gold.edn"
             "cases/refuted/fixtures/inputs/note.txt"
             "cases/refuted/fixtures/inputs/claim.txt"
             "cases/refuted/gold.edn"
             "cases/unknown/fixtures/inputs/note.txt"
             "cases/unknown/fixtures/inputs/claim.txt"
             "cases/unknown/gold.edn"
             "cases/held-out/fixtures/inputs/note.txt"
             "cases/held-out/fixtures/inputs/claim.txt"
             "cases/held-out/gold.edn"
             "calibration.edn"])

(defn source []
  (:files
   (reduce (fn [{:keys [files total]} path]
             (let [resource (or (io/resource (str "is/simm/workflows/evidence-check/v1/" path))
                                (throw (ex-info "Missing evidence-check source" {:path path})))
                   bytes (with-open [stream (io/input-stream resource)] (.readNBytes stream 65537))
                   length (alength bytes) total (+ total length)]
               (when (or (> length 65536) (> total 131072))
                 (throw (ex-info "Evidence-check source exceeds bounds" {:path path})))
               {:files (assoc files path (markdown/decode bytes)) :total total}))
           {:files (sorted-map) :total 0} paths)))
