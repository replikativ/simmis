(ns is.simm.ops.native-execution-return
  "Native facts for fixed HOST recovery, not generic Job settlement authority."
  (:require [dvergr.agent.run :as run]
            [dvergr.jobs :as jobs]
            [dvergr.room.store :as store]))

(defn state
  "Classify an already bounded, domain-validated graph. A returned execution
   family permits its fixed domain reconciler to inspect cleanup obligations;
   it does not certify or recreate the Job's configured cleanup callbacks."
  [room job graph]
  (let [observation (jobs/observe-settlement room job)
        active (set (map :run/id (run/active-runs)))
        ids (map :run/id graph)]
    (cond
      (or (= :live (:owner-state observation))
          (some active ids)
          (some #(not (contains? store/terminal-run-statuses (:run/status %))) graph)) :live
      (and (= :missing (:owner-state observation))
           (= :absent (:execution-lease observation))
           (= :acknowledged (get-in observation [:family-return :state]))
           (seq graph) (some #{job} ids) (= (count ids) (count (set ids)))
           (every? #(and (= (:id room) (:run/room %))
                         (store/family-return-acknowledged? %)) graph)) :returned
      :else :unknown)))
