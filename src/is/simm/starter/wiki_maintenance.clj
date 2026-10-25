(ns is.simm.starter.wiki-maintenance
  "An ordinary editable workflow, not a scheduler, installer or spending grant.
   Loading its source does not read a room or execute the authored checker."
  (:refer-clojure :exclude [name])
  (:require [clojure.java.io :as io]
            [is.simm.documents.markdown :as markdown]))

(def name "maintain-wiki")
(def version 3)
(def policy {:max-file-bytes 524288 :max-bundle-bytes 2097152})

(def ^:private paths
  ["workflow.edn" "input-selection.edn" "checker.clj"
   "fixtures/workflow-context/wiki-maintenance.txt"])

(defn- packaged-resource [version path]
  (io/resource (str "is/simm/workflows/maintain-wiki/v" version "/" path)))

(defn- resource-stream [resource] (io/input-stream resource))

(defn source
  "Return the shipped source files relative to /workflows/maintain-wiki/.
   Fixed classpath resources only: no discovery, external paths, room writes,
   schedule creation, model selection or allowance. Call the existing owned
   workflow writer for existing rooms, or the exclusive new-repository
   initializer; never overwrite user edits implicitly."
  ([] (source version))
  ([version]
   (when-not (#{1 2 3} version)
     (throw (ex-info "Unknown shipped wiki workflow version" {})))
   (:files
    (reduce (fn [{:keys [files size]} path]
              (let [resource (packaged-resource version path)]
                (when-not resource
                  (throw (ex-info "Missing shipped wiki-maintenance resource" {:path path})))
                (with-open [stream (resource-stream resource)]
                  (let [bytes (.readNBytes stream (int (inc (:max-file-bytes policy))))
                        length (alength bytes)
                        total (+ size length)]
                    (when (or (> length (:max-file-bytes policy))
                              (> total (:max-bundle-bytes policy)))
                      (throw (ex-info "Shipped wiki-maintenance source exceeds byte limits" {:path path})))
                    {:files (assoc files path (markdown/decode bytes)) :size total}))))
            {:files (sorted-map) :size 0} paths))))
