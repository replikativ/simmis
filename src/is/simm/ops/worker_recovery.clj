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
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.worker-benchmarks :as worker-benchmarks]
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
        {:keys [submission frozen policy environment team snapshot workflow]} (:input payload)
        kind (:work.request/kind row)
        selection-ok?
        (case kind
          :local-worker (and (m/validate local/task-schema submission)
                             (= (:task submission) (:task frozen))
                             (= (set (:paths submission)) (set (keys (:files frozen)))))
          :local-worker-workflow
          (let [prepared (local/workflow-input snapshot submission (:config frozen))]
            (and (= frozen (:frozen prepared)) (= workflow (:workflow prepared))
                 (= workflow (get-in environment [:environment/metadata :workflow]))))
          false)]
    (files/validate-frozen! frozen)
    (when-not (and (= (:work.request/intent-id row) (hasch/uuid payload))
                   (= kind (:kind payload)) selection-ok?
                   (= :simmis/local-worker-v1 (:policy policy))
                   (= (:worker submission) (:worker policy)) (pos-int? (:generation policy))
                   (= (:input-id frozen) (:input-id policy))
                   (= (:input-id frozen) (hasch/uuid [:simmis/worker-input-v1 (dissoc frozen :input-id)]))
                   (= (:limits submission) (:limits frozen))
                   (= files/policy (:policy frozen))
                   (= {:protocol/id :simmis/local-worker :protocol/version 1 :protocol/basis policy}
                      (get-in environment [:environment/world :protocol])))
      (refuse! "Worker recovery requires the exact supported frozen request"))
    {:assignment {:worker (:worker policy) :generation (:generation policy) :input frozen}
     :environment environment :agent (get-in team [:roster/agents :local-worker])}))

(defn- settle-world! [room child {:keys [environment agent world-parent registry-context]}]
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
      (if-let [fork (when (or (nil? world-parent) registry-context)
                     (binding [ec/*execution-context* (or registry-context (:ctx room))]
                       (registry/lookup (:run/world child))))]
        (binding [ec/*execution-context* (or registry-context (:ctx room))]
          (let [metadata @(:meta fork)]
            (when-not (and (identical? fork (registry/lookup (:id fork)))
                           (= (:run/world child) (:id fork)) (= (or world-parent (:id room)) (:parent-id fork))
                           (forks/fork? fork) (forks/ctx-fork? fork)
                           (:run-world? metadata) (= id (:run-id metadata))
                           (= (:id room) (:record-room-id metadata))
                           (= :deferred (:settlement-policy metadata)))
              (refuse! "Orphaned worker world does not match its canonical Run"))
            (true? (:ok? ((if (forks/deferred? fork) forks/discard-deferred! forks/discard!)
                          fork :local-worker-recovery)))))
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
       (= (:cell expected) (:simmis.worker.assignment/cell row))
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

(defn- benchmark-intent! [conn row]
  (let [payload (requests/payload conn row)
        {:keys [submission snapshot frozen policy] :as basis} (:input payload)
        config (get-in (val (first frozen)) [:frozen :config])
        prepared (worker-benchmarks/prepare snapshot submission config
                                           {:simmis.worker/generation (:generation policy)
                                            :simmis.worker/config-id (:config-id policy)})]
    (when-not (and (= :local-worker-benchmark (:kind payload))
                   (= (:work.request/intent-id row) (hasch/uuid payload))
                   (= basis (:basis prepared)))
      (refuse! "Benchmark recovery requires the exact pinned experiment and inputs"))
    prepared))

(defn- benchmark-children! [conn host-ctx row room children prepared]
  (let [{:keys [submission frozen policy team]} (:basis prepared)
        envs (into {} (map (fn [env] [(get-in env [:environment/metadata :worker-cell]) env]))
                   (get-in prepared [:plan :environments]))
        counts (frequencies (map #(get (workers/assignment conn (:run/id %)) :simmis.worker.assignment/cell) children))
        parent-id (benchmarks/fixture-room-id (:work.request/id row))
        parent (binding [ec/*execution-context* host-ctx] (registry/lookup parent-id))]
    (when-not (and (every? #(contains? frozen %) (keys counts))
                   (every? #(<= % (:repetitions submission)) (vals counts)))
      ;; A missing prepared binding is NOT permission to guess a case from
      ;; task text (different cases can deliberately have identical tasks).
      (refuse! "Benchmark child lacks a unique bounded prepared case binding"))
    (mapv (fn [child]
            (let [own (workers/assignment conn (:run/id child))
                  cell (:simmis.worker.assignment/cell own)
                  expected {:id (:run/id child) :worker (:worker policy) :generation (:generation policy)
                            :room (:work.request/room row) :request (:work.request/id row)
                            :job (:work.request/job row) :cell cell :input (get-in frozen [cell :frozen])}
                  agent (get-in team [:roster/agents :local-worker])]
              (when-not (and (= :agent-task (:run/kind child)) (= (:id room) (:run/room child))
                             (= :local-worker (:run/actor child)) (= (hasch/uuid agent) (:run/agent-def-hash child))
                             (= :ctx (:run/isolation child)) (= :deferred (:run/settlement-policy child))
                             (some? (:run/world child))
                             (assignment-matches? conn own expected (workers/assignment-digest expected)))
                (refuse! "Benchmark child differs from its immutable experiment binding"))
              {:child child :own own :expected expected
               :pinned {:environment (get envs cell) :agent agent
                        :world-parent parent-id :registry-context (:ctx parent)}})) children)))

(defn- recover-benchmark! [conn host-ctx row room]
  (let [id (:work.request/id row) job (:work.request/job row)
        status (jobs/status job)
        graph (binding [ec/*execution-context* (:ctx room)] (run/runs room {:root-run-id job :limit 10}))
        root (first (filter #(= job (:run/id %)) graph))
        children (vec (remove #(= job (:run/id %)) graph))
        active (set (map :run/id (run/active-runs)))
        prepared (benchmark-intent! conn row)
        maximum (long (get (:resources prepared) local/admission-unit))]
    (when-not (and root (= :experiment (:run/kind root))
                   (= (:id room) (:run/room root) (:room status))
                   (= (keyword "party" (str (:work.request/by row))) (:run/actor root))
                   (<= (count children) maximum) (< (count graph) 10)
                   (= (count graph) (count (set (map :run/id graph))))
                   (every? #(= job (:run/parent %)) children))
      (refuse! "Benchmark recovery requires the complete bounded experiment graph"))
    (if (or (not (:settled? status)) (some #(active (:run/id %)) graph)
            (not-every? #(contains? #{:completed :failed :cancelled} (:run/status %)) graph))
      {:state :live}
      (let [fenced
            ;; Prove/fence siblings independently. One pre-binding crash must
            ;; not leave another, fully identified external lease running.
            ;; Any unresolved child still prevents ALL refunds/retirement.
            (mapv (fn [child]
                    (try
                      (let [{:keys [own expected]}
                            (first (benchmark-children! conn host-ctx row room [child] prepared))]
                        (when-not (:simmis.worker.assignment/stop-ack-at own)
                          (workers/withdraw! conn expected #(Date.)))
                        {:bound? true})
                      (catch clojure.lang.ExceptionInfo error {:error error}))) children)
            _ (when-let [errors (seq (keep :error fenced))]
                (let [error (ex-info "Benchmark graph remains unresolved after fencing known children" {:type ::inconsistent})]
                  (doseq [cause errors] (.addSuppressed error cause))
                  (throw error)))
            bindings (benchmark-children! conn host-ctx row room children prepared)
            ids (set (map :run/id children))
            assignments (associated conn id job)]
        (when-not (= ids (set (map :simmis.worker.assignment/id assignments)))
          (refuse! "Benchmark assignments do not match its complete child graph"))
        (let [closed (mapv (fn [{:keys [own expected]}]
                             (if (:simmis.worker.assignment/stop-ack-at own) own
                                 (workers/withdraw! conn expected #(Date.)))) bindings)]
          (if-not (every? :simmis.worker.assignment/stop-ack-at closed)
            {:state :awaiting-stop :runs (mapv :simmis.worker.assignment/id
                                              (remove :simmis.worker.assignment/stop-ack-at closed))}
            (let [clean?
                  (binding [ec/*execution-context* (:ctx room)]
                    (evaluation/await-cleanups-for! room job 30000)
                    (let [current (run/runs room {:root-run-id job :limit 10})]
                      (when-not (and (= (set (map #(select-keys % store/immutable-run-keys) graph))
                                                       (set (map #(select-keys % store/immutable-run-keys) current)))
                                     (= (count graph) (count current))
                                     (every? #(contains? #{:completed :failed :cancelled} (:run/status %)) current)
                                     (= ids (set (map :simmis.worker.assignment/id (associated conn id job)))))
                        (refuse! "Benchmark graph changed during physical cleanup"))
                      (let [settled (mapv (fn [{:keys [child pinned]}]
                                            (settle-world! room (run/run room (:run/id child)) pinned)) bindings)]
                        (when (every? true? settled)
                          (doseq [child children] (return-wallet! room (:run/id child) job))
                          (return-wallet! room job nil)
                          true))))]
              (if clean?
                (do (benchmarks/retire-request! host-ctx id)
                    (requests/mark-returned! conn id) {:state :returned})
                {:state :awaiting-world-cleanup}))))))))

(defn recover-request!
  "Host only. Resolve the canonical durable room; call after orphan Run
   reconciliation, or an authenticated assignment's completion/stop receipt.
   Live adapters own their cleanup. Unacknowledged leases remain pending.
   Unexpected graphs throw without refund. Idempotent ledger returns precede
   mark-returned!. This never promotes, imports or recertifies remote output."
  [conn host-ctx resolve-room id]
  (binding [ec/*execution-context* host-ctx]
    (when-let [row (requests/get-request conn id)]
      (when (local/request-kinds (:work.request/kind row))
        (when (= :local-worker-benchmark (:work.request/kind row))
          (benchmarks/recover-request! host-ctx row))
        (if-let [job (:work.request/job row)]
          (when (:work.request/return-pending? row)
            (when-let [room (resolve-room (:work.request/room row))]
              (if (= :local-worker-benchmark (:work.request/kind row))
                (recover-benchmark! conn host-ctx row room)
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
                        (finish-recovery! conn row room child pinned)))))))))
          ;; Evaluation cannot publish before the request owns its Job.
          (do (requests/interrupt! conn id (random-uuid)) {:state :interrupted}))))))

(defn recover! [conn host-ctx resolve-room]
  (binding [ec/*execution-context* host-ctx]
    (mapv #(recover-request! conn host-ctx resolve-room (:work.request/id %))
          (filter #(local/request-kinds (:work.request/kind %)) (requests/requests conn)))))
