(ns is.simm.ops.knowledge-workspace
  "Owned input staging in disposable workflow/benchmark worlds. Never clear
   a canonical source room as a substitute for constructing an isolated Run."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.catalog.workspace :as room-workspace]
            [dvergr.rooms.files :as files]
            [dvergr.substrate.geschichte :as workspace]
            [org.replikativ.spindel.engine.core :as ec]))

(def cleanup-policy
  {:version 2 :roots ["in/knowledge" "out/wiki"] :max-paths files/max-batch-files})

(defn validate-inputs!
  "Knowledge inputs cannot masquerade as newly generated deliverables."
  [inputs]
  (doseq [path (keys inputs)]
    (when-not (and (string? path)
                   (try (= path (files/workspace-path path)) (catch Exception _ false)))
      (throw (ex-info "Knowledge inputs require canonical workspace paths" {}))))
  (when (some #(or (= % "/out") (= % "/out/wiki") (str/starts-with? % "/out/wiki/"))
              (keys inputs))
    (throw (ex-info "Knowledge inputs occupy the reserved output namespace" {})))
  inputs)

(defn- owned-rows [db attr root]
  (concat (d/datoms db :avet attr root)
          (d/index-range db {:attrid attr :start (str root "/") :end (str root "0")})))

(defn- cleanup-state [world]
  (binding [ec/*execution-context* (:ctx world)]
    (let [conn (:conn (workspace/current-workspace))
          _ (when-not conn (throw (ex-info "Knowledge staging has no workspace" {})))
          db @conn
          paths (fn [attr] (vec (take (inc (:max-paths cleanup-policy))
                                      (map :v (mapcat #(owned-rows db attr %) (:roots cleanup-policy))))))
          file-paths (paths :geschichte.work/path)
          dirs (paths :geschichte.work-dir/path)]
      (when (or (> (count file-paths) (:max-paths cleanup-policy))
                (> (count dirs) (:max-paths cleanup-policy)))
        (throw (ex-info "Inherited knowledge staging exceeds cleanup bounds" {})))
      (when (seq (d/datoms db :avet :geschichte.work/path "out"))
        (throw (ex-info "Knowledge output ancestor is a file" {})))
      {:files file-paths :dirs dirs})))

(defn clear-owned!
  "Clear only disposable knowledge-world staging/output. Empty directories
   without files cannot be removed by the canonical writer: refuse them before
   editing. Re-scan after writing so skipped effects cannot certify stale output."
  [world]
  (let [{:keys [files dirs]} (cleanup-state world)]
    (doseq [dir dirs]
      (when-not (some #(str/starts-with? % (str dir "/")) files)
        (throw (ex-info "Inherited knowledge staging contains an unsupported empty directory"
                        {:path dir}))))
    (when (seq files)
      (files/write! world (into {} (map #(vector (str "/" %) nil)) files)
                    {:message "Reset isolated knowledge staging and outputs"}))
    (let [remaining (cleanup-state world)]
      (when (or (seq (:files remaining)) (seq (:dirs remaining)))
        (throw (ex-info "Knowledge staging cleanup did not empty its owned subtrees" {}))))
    true))

(defn with-frozen-staging
  "Wrap fixture setup so every benchmark backend clears stale owned inputs
   before seeding its verified case. No global cache or new world authority."
  [base]
  (evaluation/make-world-setup
   {:id :simmis/knowledge-fixtures :version 2
    :basis {:base (evaluation/world-setup-ref base) :cleanup cleanup-policy}
    :prepare (fn [{world :room :as context}]
               (room-workspace/ensure-workspace! world)
               (clear-owned! world)
               ((:prepare base) context))}))
