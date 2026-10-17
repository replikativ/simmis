(ns is.simm.ops.worker-recovery
  "Host reconciliation after Dvergr's orphan reconciliation. A terminal Run
   does not prove that an external lease stopped. Never executes or imports."
  (:require [datahike.api :as d]
            [dvergr.agent.attempt :as attempt]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.run :as run]
            [dvergr.jobs :as jobs]
            [dvergr.room.registry :as registry]
            [dvergr.room.store :as store]
            [dvergr.rooms.forks :as forks]
            [dvergr.resource :as resource]
            [hasch.core :as hasch]
            [is.simm.model.work-requests :as requests]
            [is.simm.model.workers :as workers]
            [is.simm.ops.worker-files :as files]
            [is.simm.ops.worker-work :as local]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util Date]))

(defn- refuse! [message]
  (throw (ex-info message {:type ::inconsistent})))

(defn- associated [conn request job]
  (let [db @conn]
    (mapv #(d/pull db '[*] %)
          (set (concat
                (d/q '[:find [?e ...] :in $ ?request
                       :where [?e :simmis.worker.assignment/request ?request]] db request)
                (d/q '[:find [?e ...] :in $ ?job
                       :where [?e :simmis.worker.assignment/job ?job]] db job))))))

(defn- intent! [conn row]
  (let [payload (requests/payload conn row)
        {:keys [submission frozen policy environment team]} (:input payload)]
    (when-not (and (= (:work.request/intent-id row) (hasch/uuid payload))
                   (= :local-worker (:kind payload))
                   (m/validate local/task-schema submission)
                   (= :simmis/local-worker-v1 (:policy policy))
                   (= (:worker submission) (:worker policy)) (pos-int? (:generation policy))
                   (= (:input-id frozen) (:input-id policy))
                   (= (:input-id frozen) (hasch/uuid [:simmis/worker-input-v1 (dissoc frozen :input-id)]))
                   (= (:task submission) (:task frozen)) (= (:limits submission) (:limits frozen))
                   (= files/policy (:policy frozen))
                   (= (set (:paths submission)) (set (keys (:files frozen))))
                   (= {:protocol/id :simmis/local-worker :protocol/version 1 :protocol/basis policy}
                      (get-in environment [:environment/world :protocol])))
      (refuse! "Worker recovery requires the exact supported frozen request"))
    {:assignment {:worker (:worker policy) :generation (:generation policy) :input frozen}
     :environment environment :agent (get-in team [:roster/agents :local-worker])}))

(defn- settle-world! [room child {:keys [environment agent]}]
  (let [id (:run/id child)
        certified (store/-load-attempt (:store room) (store/conversation-id room) id)
        settled (:run/settlement-status child)]
    (when certified
      (attempt/validate-attempt certified)
      (when-not (and (= id (:attempt/id certified) (:attempt/run-id certified))
                     (= environment (:attempt/environment certified))
                     (= agent (:attempt/agent certified))
                     (= (hasch/uuid agent) (:attempt/agent-def-hash certified))
                     (= (:run/world child) (get-in certified [:attempt/evidence :worker :world]))
                     (some #(= (select-keys child [:run/id :run/parent :run/actor :run/agent-def-hash])
                               (select-keys % [:run/id :run/parent :run/actor :run/agent-def-hash]))
                           (get-in certified [:attempt/evidence :trace :runs])))
        (refuse! "Certified Attempt does not match its frozen worker Run")))
    (cond
      ;; A matching certified world is retained for its ordinary reviewer.
      (and certified (= :review (:attempt/settlement-intent certified))
           (= :completed (:run/status child)) (contains? #{:review :merged :adopted} settled)) true
      ;; Do not convert an interrupted certification into review authority.
      (and certified (= :review (:attempt/settlement-intent certified))) false
      (contains? #{:merged :adopted} settled)
      (refuse! "Uncertified worker world was promoted outside its recovery policy")
      :else
      (if-let [fork (registry/lookup (:run/world child))]
        (let [metadata @(:meta fork)]
          (when-not (and (identical? fork (registry/lookup (:id fork)))
                         (= (:run/world child) (:id fork)) (= (:id room) (:parent-id fork))
                         (forks/fork? fork) (forks/ctx-fork? fork)
                         (:run-world? metadata) (= id (:run-id metadata))
                         (= (:id room) (:record-room-id metadata))
                         (= :deferred (:settlement-policy metadata)))
            (refuse! "Orphaned worker world does not match its canonical Run"))
          (true? (:ok? ((if (forks/deferred? fork) forks/discard-deferred! forks/discard!)
                        fork :local-worker-recovery))))
        ;; The process-local registry disappearing is not substrate cleanup.
        false))))

(defn- return-wallet! [room id parent]
  (when-let [balance (resource/run-wallet-balance room id)]
    (when-let [remaining (not-empty (into {} (filter (comp pos? val)) balance))]
      (resource/return! room id parent remaining))))

(defn- assignment-matches? [conn row expected digest]
  (and (= (:id expected) (:simmis.worker.assignment/id row))
       (= (:worker expected) (:simmis.worker.assignment/worker row))
       (= (:generation expected) (:simmis.worker.assignment/generation row))
       (= (:room expected) (:simmis.worker.assignment/room row))
       (= (:request expected) (:simmis.worker.assignment/request row))
       (= (:job expected) (:simmis.worker.assignment/job row))
       (= digest (:simmis.worker.assignment/intent-id row))
       (= (:input expected) (workers/payload conn row :simmis.worker.assignment/input-ref))))

(defn- finish-recovery! [conn row room child pinned]
  (let [job (:work.request/job row)
        id (:run/id child)
        done?
        (binding [ec/*execution-context* (:ctx room)]
          ;; A terminal Job can outlive a failed/timed-out settlement callback.
          ;; Detached certification/capture still owns the world until joined.
          (evaluation/await-cleanups-for! room job 30000)
          (let [graph (run/runs room {:root-run-id job :limit 3})
                expected-ids (cond-> #{job} child (conj id))
                current (when child (run/run room id))]
            ;; In particular, zero children BEFORE the join does not prove a
            ;; detached admission could not have published a child meanwhile.
            (when-not (and (= expected-ids (set (map :run/id graph)))
                           (= (count expected-ids) (count graph))
                           (every? #(contains? #{:completed :failed :cancelled} (:run/status %)) graph)
                           (every? #(= id (:simmis.worker.assignment/id %))
                                   (associated conn (:work.request/id row) job)))
              (refuse! "Worker ownership graph changed during cleanup"))
            (when (and child
                       (not= (select-keys child store/immutable-run-keys)
                             (select-keys current store/immutable-run-keys)))
              (refuse! "Worker Run identity changed during cleanup"))
            (when (or (nil? child) (settle-world! room current pinned))
              (when child (return-wallet! room id job))
              (return-wallet! room job nil)
              true)))]
    (if done?
      (do (requests/mark-returned! conn (:work.request/id row)) {:state :returned})
      {:state :awaiting-world-cleanup :run id})))

(defn recover-request!
  "Host only. Resolve the canonical durable room; call after orphan Run
   reconciliation, or an authenticated assignment's completion/stop receipt.
   Live adapters own their cleanup. Unacknowledged leases remain pending.
   Unexpected graphs throw without refund. Idempotent ledger returns precede
   mark-returned!. This never promotes, imports or recertifies remote output."
  [conn host-ctx resolve-room id]
  (binding [ec/*execution-context* host-ctx]
    (when-let [row (requests/get-request conn id)]
      (when (= :local-worker (:work.request/kind row))
        (if-let [job (:work.request/job row)]
          (when (:work.request/return-pending? row)
            (when-let [room (resolve-room (:work.request/room row))]
              (let [status (jobs/status job)
                    graph (binding [ec/*execution-context* (:ctx room)]
                            (run/runs room {:root-run-id job :limit 3}))
                    root (first (filter #(= job (:run/id %)) graph))
                    children (remove #(= job (:run/id %)) graph)
                    active (set (map :run/id (run/active-runs)))]
                (when-not (and root (= :workflow (:run/kind root))
                               (= (:id room) (:run/room root))
                               (= (:id room) (:room status))
                               (< (count graph) 3)
                               (<= (count children) 1)
                               (every? #(= job (:run/parent %)) children))
                  (refuse! "Worker recovery requires one complete Job/Run graph"))
                (if (or (not (:settled? status))
                        (some #(active (:run/id %)) graph)
                        (not-every? #(contains? #{:completed :failed :cancelled} (:run/status %)) graph))
                  {:state :live}
                  (let [pinned (intent! conn row)
                        assignment (:assignment pinned)
                        rows (associated conn id job)
                        child (first children)
                        run-id (:run/id child)
                        own (when child (workers/assignment conn run-id))
                        expected (when child (merge assignment {:id run-id :room (:work.request/room row)
                                                                :request id :job job}))
                        digest (when child (hasch/uuid [:simmis/worker-assignment-v1
                                                        (:worker assignment) (:generation assignment)
                                                        (:work.request/room row) id job (:input assignment)]))]
                    (when-not (and (= (keyword "party" (str (:work.request/by row))) (:run/actor root))
                                   (or (nil? child)
                                       (and (= :agent-task (:run/kind child))
                                            (= (:id room) (:run/room child))
                                            (= :local-worker (:run/actor child))
                                            (= (hasch/uuid (:agent pinned)) (:run/agent-def-hash child))
                                            (= :ctx (:run/isolation child))
                                            (= :deferred (:run/settlement-policy child))
                                            (some? (:run/world child)))))
                      (refuse! "Worker Run provenance differs from its pinned admission"))
                    (when-not (and (every? #(assignment-matches? conn % expected digest) rows)
                                   (or (nil? own) (assignment-matches? conn own expected digest)))
                      (refuse! "External assignments do not match the frozen Job/Run intent"))
                    (let [closed (when child
                                   (if (:simmis.worker.assignment/stop-ack-at own) own
                                       ;; Absence is not closure. This installs a
                                       ;; fence, or quarantines the existing lease.
                                       (workers/withdraw! conn expected #(Date.))))]
                      (if (and child (nil? (:simmis.worker.assignment/stop-ack-at closed)))
                        {:state :awaiting-stop :run run-id}
                        (finish-recovery! conn row room child pinned))))))))
          ;; Evaluation cannot publish before the request owns its Job.
          (do (requests/interrupt! conn id (random-uuid)) {:state :interrupted}))))))

(defn recover! [conn host-ctx resolve-room]
  (binding [ec/*execution-context* host-ctx]
    (mapv #(recover-request! conn host-ctx resolve-room (:work.request/id %))
          (filter #(= :local-worker (:work.request/kind %)) (requests/requests conn)))))
