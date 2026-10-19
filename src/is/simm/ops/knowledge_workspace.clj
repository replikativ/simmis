(ns is.simm.ops.knowledge-workspace
  "Owned input staging in disposable workflow/benchmark worlds. Never clear
   a canonical source room as a substitute for constructing an isolated Run."
  (:require [datahike.api :as d]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.catalog.workspace :as room-workspace]
            [dvergr.rooms.files :as files]
            [dvergr.substrate.geschichte :as workspace]
            [is.simm.ops.workflows :as workflows]
            [org.replikativ.spindel.engine.core :as ec]))

(defn staging-removals
  "Bounded metadata-only edits for the collector-owned subtree."
  [world]
  (binding [ec/*execution-context* (:ctx world)]
    (let [conn (:conn (workspace/current-workspace))
          _ (when-not conn (throw (ex-info "Knowledge staging has no workspace" {})))
          rows (vec (take (inc workflows/max-list-entries)
                          (d/index-range @conn {:attrid :geschichte.work/path
                                                :start "in/knowledge/" :end "in/knowledge0"})))]
      (when (> (count rows) workflows/max-list-entries)
        (throw (ex-info "Inherited collector staging exceeds the metadata bound" {})))
      (into {} (map #(vector (str "/" (:v %)) nil)) rows))))

(defn with-frozen-staging
  "Wrap fixture setup so every benchmark backend clears stale owned inputs
   before seeding its verified case. No global cache or new world authority."
  [base]
  (evaluation/make-world-setup
   {:id :simmis/knowledge-fixtures :version 1
    :basis {:base (evaluation/world-setup-ref base) :prefix "/in/knowledge/"
            :max-staging-paths workflows/max-list-entries}
    :prepare (fn [{world :room :as context}]
               (room-workspace/ensure-workspace! world)
               (when-let [edits (not-empty (staging-removals world))]
                 (files/write! world edits {:message "Replace frozen knowledge staging"}))
               ((:prepare base) context))}))
