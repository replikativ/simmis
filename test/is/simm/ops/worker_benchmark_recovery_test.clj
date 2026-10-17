(ns is.simm.ops.worker-benchmark-recovery-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.agent.run :as run]
            [dvergr.agent.world :as run-world]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.resource :as resource]
            [dvergr.room.registry :as registry]
            [dvergr.room.store.memory :as memory]
            [hasch.core :as hasch]
            [is.simm.authority.world :as world]
            [is.simm.model.work-requests :as requests]
            [is.simm.model.workers :as workers]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.worker-benchmarks :as local-benchmarks]
            [is.simm.ops.worker-benchmarks-test :as benchmark-fixture]
            [is.simm.ops.worker-files-test :as file-fixture]
            [is.simm.ops.worker-recovery :as recovery]
            [is.simm.ops.worker-work :as local]
            [is.simm.ops.worker-work-test :as fixture]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg])
  (:import [java.util Date]))

(defn- with-orphan [conn w {:keys [room host-ctx enrolled] :as options} bind-second? f]
  (let [input (benchmark-fixture/input (:worker enrolled))
        id (requests/request-id (:owner w) (:room w) (:request-id input))
        enrollment (workers/enrollment conn (:worker enrolled))
        prepared (local-benchmarks/prepare {:files benchmark-fixture/source} input file-fixture/config enrollment)
        request (:request (requests/claim! conn {:party (:owner w) :room (:room w)
                                                 :client-id (:request-id input) :kind :local-worker-benchmark
                                                 :intent (:basis prepared)
                                                 :limits (assoc (:limits input) :settlement :discard :max-worker-admissions 2)}))
        job (:run/id (run/start! room (keyword "party" (str (:owner w))) (random-uuid) nil {:kind :experiment}))
        clean (discourse/make-room {:id (benchmarks/fixture-room-id id) :store (memory/make)
                                    :ctx (:child-ctx (ygg/fork! {:systems :none :purpose :room}))})]
    (try
      (requests/record-job! conn id (:work.request/claim request) job)
      (binding [ec/*execution-context* host-ctx] (registry/register! clean))
      (binding [ec/*execution-context* (:ctx clean)]
        (registry/register! clean)
        (workspace/ensure-workspace! clean))
      (resource/install-unit! room (first local/units))
      (resource/mint! room {:id (random-uuid) :resources {local/admission-unit 2M}})
      (resource/allocate-run! room job nil {local/admission-unit 2M})
      (let [children
            (mapv (fn [[index [cell {:keys [frozen]}]]]
                    (let [child (random-uuid)
                          world (binding [ec/*execution-context* (:ctx clean)]
                                  (run-world/open! clean child :deferred room))
                          assignment {:id child :worker (:worker enrolled) :generation 1 :room (:room w)
                                      :request id :job job :cell cell :input frozen}]
                      (run/start! room :local-worker (random-uuid) nil
                                  {:id child :kind :agent-task :parent job
                                   :provenance {:run/agent-def-hash (hasch/uuid (get-in prepared [:basis :team :roster/agents :local-worker]))
                                                :run/isolation :ctx :run/settlement-policy :deferred
                                                :run/world (get-in world [:work :id]) :run/settlement-status :open}})
                      (resource/allocate-run! room child job {local/admission-unit 1M})
                      (when (or (zero? index) bind-second?) (workers/prepare! conn assignment))
                      {:id child :assignment assignment})) (map-indexed vector (get-in prepared [:basis :frozen])))
            finish! #(do (doseq [{:keys [id]} children] (run/finish! id :failed {})) (run/finish! job :failed {}))]
        (f (assoc options :clean clean :request id :job job :children children :finish! finish!
                  :recover! #(recovery/recover-request! conn host-ctx (fn [r] (when (= r (:room w)) room)) id))))
      (finally
        (binding [ec/*execution-context* (:ctx room)]
          (doseq [entry (run/runs room {:root-run-id job :limit 10})
                  :when (= :running (:run/status entry))]
            (run/finish! (:run/id entry) :failed {})))
        (binding [ec/*execution-context* (:ctx clean)] (registry/unregister! (:id clean)))
        (binding [ec/*execution-context* host-ctx] (registry/unregister! (:id clean)))
        (discourse/close-room! clean)))))

(deftest all-prepared-cells-are-fenced-and-wallets-return-only-after-world-cleanup
  (world/with-world [conn w]
    (@#'fixture/with-worker
     conn w
     #(with-orphan conn w % true
        (fn [{:keys [room clean request children finish! recover!]}]
          (finish!)
          (let [retired (atom 0)
                lookup registry/lookup
                worlds (set (map (fn [child] (:run/world (run/run room (:id child)))) children))]
            ;; Make registry-context divergence observable even though this
            ;; fixture shares the real daemon root. Every lookup of a work
            ;; world must use its fixture's context, including revalidation.
            (with-redefs [benchmarks/retire-fixture! (fn [& _] (swap! retired inc))
                          registry/lookup (fn [id]
                                            (when (worlds id)
                                              (is (identical? (:ctx clean) ec/*execution-context*)))
                                            (if (and (worlds id) (not (identical? (:ctx clean) ec/*execution-context*)))
                                              nil (lookup id)))]
              (is (= {:state :returned} (recover!)))
              (is (= 1 @retired))))
          (doseq [{:keys [id]} children]
            (is (= :withdrawn (:simmis.worker.assignment/state (workers/assignment conn id))))
            (is (:simmis.worker.assignment/stop-ack-at (workers/assignment conn id))))
          (is (= 2M (get (resource/balance room) local/admission-unit)))
          (is (false? (:work.request/return-pending? (requests/get-request conn request)))))))))

(deftest one-unbound-child-still-fences-the-proven-leased-sibling-without-refunds
  (world/with-world [conn w]
    (@#'fixture/with-worker
     conn w
     #(with-orphan conn w % false
        (fn [{:keys [room request children finish! recover!]}]
          (let [{:keys [id assignment]} (first children)
                _ (workers/enqueue! conn assignment)
                claimed (workers/claim! conn id (:worker assignment) 1 (random-uuid) (workers/new-secret) (Date.) 30000)]
            (finish!)
            (is (thrown? clojure.lang.ExceptionInfo (recover!)))
            (is (= :quarantined (:simmis.worker.assignment/state (workers/assignment conn id))))
            (is (:simmis.worker.assignment/cancel-at (workers/assignment conn id)))
            (is (nil? (:simmis.worker.assignment/stop-ack-at (workers/assignment conn id))))
            (is (some? (:simmis.worker.assignment/lease claimed)))
            (is (:work.request/return-pending? (requests/get-request conn request)))
            (is (zero? (get (resource/balance room) local/admission-unit 0M)))))))))
