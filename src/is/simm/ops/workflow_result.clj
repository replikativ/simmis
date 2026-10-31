(ns is.simm.ops.workflow-result
  "Shared certified native completion identity. Caller owns current Room
   authority and retained input validation; no hydration or live fallback."
  (:require [dvergr.agent.attempt :as attempt]
            [dvergr.agent.episode :as episode]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.run :as run]
            [dvergr.room.store :as store]
            [hasch.core :as hasch]
            [is.simm.model.access :as access]
            [is.simm.ops.work :as work]
            [org.replikativ.spindel.engine.core :as ec]))

(defn completed-attempt [room row input]
  (binding [ec/*execution-context* (:ctx room)]
    (let [job-id (:work.request/job row) job (run/run room job-id)
          runs (vec (take 101 (run/runs room {:root-run-id job-id :limit 101})))
          direct (filterv #(= job-id (:run/parent %)) runs)
          candidate (when (= 1 (count direct)) (run/run room (:run/id (first direct))))
          certified (when candidate (episode/attempt room (:run/id candidate)))
          _ (when certified (attempt/validate-attempt certified))
          agent (:candidate input) agent-hash (hasch/uuid agent)
          receipt (:attempt/receipt certified) metrics (:attempt/metrics receipt)
          trace (vec (take 101 (get-in certified [:attempt/evidence :trace :runs])))
          trace-candidates (filterv #(= (:run/id candidate) (:run/id %)) trace)]
      (when (and (<= (count runs) 100) (= :workflow (:run/kind job))
                 (= job-id (:run/id job)) (nil? (:run/parent job))
                 (= (store/conversation-id room) (:run/room job) (:run/room candidate))
                 (= (:work.request/by row) (access/actor->party-uuid (:run/actor job)))
                 (= :completed (:run/status job) (:run/status candidate))
                 (inst? (:run/ended-at job)) (inst? (:run/ended-at candidate))
                 (= :agent-task (:run/kind candidate)) (= job-id (:run/parent candidate))
                 (= (:run/id (first direct)) (:run/id candidate))
                 (= (:run/id candidate) (:attempt/run-id certified))
                 (= (:environment input) (:attempt/environment certified))
                 (= (:agent/id agent) (:run/actor candidate))
                 (= agent (:attempt/agent certified))
                 (= agent-hash (:run/agent-def-hash candidate) (:attempt/agent-def-hash certified)
                    (:agent-def-hash metrics))
                 (= (:agent/version agent) (:run/agent-version candidate))
                 (= (get-in agent [:agent/program :kind]) (:run/program-kind candidate))
                 (pos-int? (:run/interpreter-version candidate))
                 (= (:run/interpreter-version candidate) (:interpreter-version metrics))
                 (= (:work.request/by row) (:requester-party metrics))
                 (= (:work.request/id row) (:request-id metrics))
                 (= :saved-workflow (:execution-kind metrics))
                 (= (evaluation/evaluator-tier work/completion) (:verifier-trust metrics))
                 (= :completed (:attempt/status receipt))
                 (= {:completed? true} (:attempt/checks receipt))
                 (= 1.0 (:attempt/reward receipt))
                 (<= (count trace) 100) (= 1 (count trace-candidates))
                 (= (select-keys candidate [:run/id :run/parent :run/actor :run/status :run/agent-def-hash])
                    (select-keys (first trace-candidates) [:run/id :run/parent :run/actor :run/status :run/agent-def-hash])))
        certified))))
