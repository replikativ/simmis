(ns is.simm.ops.worker-recovery-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.run :as run]
            [dvergr.agent.world :as run-world]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.resource :as resource]
            [dvergr.room.registry :as registry]
            [dvergr.rooms.forks :as forks]
            [hasch.core :as hasch]
            [is.simm.authority.world :as world]
            [is.simm.model.work-requests :as requests]
            [is.simm.model.workers :as workers]
            [is.simm.ops.worker-recovery :as recovery]
            [is.simm.ops.worker-service :as service]
            [is.simm.ops.worker-work :as local]
            [is.simm.ops.worker-work-test :as fixture]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util Date]
           [java.util.concurrent CountDownLatch TimeUnit]))

(defn- orphan [conn w {:keys [room host-ctx input] :as options} child?]
  (let [id (requests/request-id (:owner w) (:room w) (:request-id input))
        prepared (@#'local/prepare conn host-ctx (:room w) room (:owner w) id input)
        row (:request (requests/claim! conn {:party (:owner w) :room (:room w)
                                             :client-id (:request-id input) :kind :local-worker
                                             :intent (:basis prepared)
                                             :limits (assoc (:limits input) :settlement :discard
                                                            :max-worker-admissions 1)}))
        job (:run/id (run/start! room (keyword "party" (str (:owner w))) (random-uuid) nil {:kind :workflow}))
        _ (requests/record-job! conn id (:work.request/claim row) job)
        _ (resource/install-unit! room (first local/units))
        _ (resource/mint! room {:id (random-uuid) :resources {local/admission-unit 2M}})
        _ (resource/allocate-run! room job nil {local/admission-unit 1M})
        child-id (when child? (random-uuid))
        run-world (when (:live-world? options) (run-world/open! room child-id :deferred room))
        child (when child?
                (:run/id (run/start! room :local-worker (random-uuid) nil
                                     {:id child-id :kind :agent-task :parent job
                                      :provenance {:run/agent-def-hash (hasch/uuid (get-in prepared [:team :roster/agents :local-worker]))
                                                   :run/isolation :ctx :run/settlement-policy :deferred
                                                  ;; A discarded projection without a live handle
                                                  ;; is deliberately NOT physical cleanup proof.
                                                   :run/world (or (:id run-world) (keyword (str "retired-" (random-uuid))))
                                                   :run/settlement-status (if run-world :open :discarded)}})))
        _ (when child (resource/allocate-run! room child job {local/admission-unit 1M}))
        pinned (get-in prepared [:basis :policy])
        assignment (when child {:id child :worker (:worker pinned) :generation (:generation pinned)
                                :room (:room w) :request id :job job
                                :input (get-in prepared [:basis :frozen])})]
    (assoc options :id id :job job :child child :world (:work run-world) :assignment assignment
           :recover! #(recovery/recover-request! conn host-ctx (fn [r] (when (= r (:room w)) room)) id)
           :finish! #(do (when child (run/finish! child :failed {})) (run/finish! job :failed {})))))

(deftest missing-and-queued-publications-are-fenced-without-inventing-world-cleanup
  (doseq [queued? [false true]]
    (world/with-world [conn w]
      (@#'fixture/with-worker conn w
                              (fn [options]
                                (let [{:keys [room assignment id child recover! finish!]} (orphan conn w options true)]
                                  (when queued? (workers/enqueue! conn assignment))
                                  (finish!)
                                  (workspace/seed! room {"/docs/input.md" "changed after original admission"})
                                  (is (= {:state :awaiting-world-cleanup :run child} (recover!)))
                                  (is (= :withdrawn (:simmis.worker.assignment/state (workers/assignment conn (:id assignment)))))
                                  (is (= (:input assignment)
                                         (workers/payload conn (workers/assignment conn (:id assignment)) :simmis.worker.assignment/input-ref)))
                                  (is (= :withdrawn (:simmis.worker.assignment/state (workers/enqueue! conn assignment))))
                                  (is (= 1M (get (resource/balance room) local/admission-unit)))
                                  (is (:work.request/return-pending? (requests/get-request conn id)))
                                  (is (= {:state :awaiting-world-cleanup :run child} (recover!)))))))))

(deftest expired-orphaned-lease-remains-exposed-until-physical-stop
  (world/with-world [conn w]
    (@#'fixture/with-worker conn w
                            (fn [options]
                              (let [{:keys [room assignment id child recover! finish!]} (orphan conn w (assoc options :live-world? true) true)
                                    secret (workers/new-secret)]
                                (workers/enqueue! conn assignment)
                                (let [claimed (workers/claim! conn child (:worker assignment) (:generation assignment)
                                                              (random-uuid) secret (Date. 1000) 1000)]
                                  (finish!)
                                  (is (= {:state :awaiting-stop :run child} (recover!)))
                                  (is (= :quarantined (:simmis.worker.assignment/state (workers/assignment conn child))))
                                  (is (= 1M (get (resource/balance room) local/admission-unit)))
                                  (is (:work.request/return-pending? (requests/get-request conn id)))
                                  (workers/revoke! conn (:worker assignment))
                                  (workers/acknowledge-stop! conn child (:simmis.worker.assignment/lease claimed) secret (Date.))
                                  (is (= {:state :returned} (recover!)))
                                  (is (= 2M (get (resource/balance room) local/admission-unit)))))))))

(deftest a-zero-child-orphan-returns-only-when-no-assignment-was-published
  (world/with-world [conn w]
    (@#'fixture/with-worker conn w
                            (fn [options]
                              (let [{:keys [room recover! finish!]} (orphan conn w options false)]
                                (finish!)
                                (is (= {:state :returned} (recover!)))
                                (is (= 2M (get (resource/balance room) local/admission-unit))))))))

(deftest detached-certification-must-physically-finish-before-world-disposal
  (world/with-world [conn w]
    (@#'fixture/with-worker conn w
                            (fn [options]
                              (let [{:keys [room job world recover! finish!]} (orphan conn w (assoc options :live-world? true) true)
                                    release (promise) joining (promise)
                                    worker (@#'evaluation/start-task! room job #(deref release))
                                    join evaluation/await-cleanups-for!]
                                (finish!)
                                (with-redefs [evaluation/await-cleanups-for! (fn [& args] (deliver joining true) (apply join args))]
                                  (let [result (future (recover!))]
                                    (try
                                      (is (= true (deref joining 10000 nil)))
                                      (is (identical? world (registry/lookup (:id world))))
                                      (is (not (realized? result)))
                                      (is (= 1M (get (resource/balance room) local/admission-unit)))
                                      (deliver release true)
                                      (is (= {:state :returned} (deref result 20000 ::timeout)))
                                      (is (nil? (registry/lookup (:id world))))
                                      (finally
                                        (deliver release true)
                                        (deref (:gate worker) 10000 nil)
                                        (deref result 20000 nil)
                                        (when (registry/lookup (:id world))
                                          (forks/discard-deferred! world :test-cleanup)))))))))))

(deftest exact-certified-review-world-is-preserved-not-imported-or-reexecuted
  (world/with-world [conn w]
    (@#'fixture/with-worker conn w
                            (fn [{:keys [room host-ctx principal enrolled input allowance published]}]
                              (local/provision! conn host-ctx principal (:room w) room allowance)
                              (let [input (assoc-in input [:limits :settlement] "review")
                                    submitted (local/submit! conn host-ctx principal (:room w) room input)
                                    id (deref published 10000 nil)
                                    lease (@#'fixture/claim! conn enrolled id)]
                                (service/complete! conn lease {:run (str id) :output {:files {} :summary "review fixture" :exit-code 0}})
                                (is (:settled? (dvergr.jobs/status (:job submitted) 25000)))
                                (let [child (run/run room id) fork (registry/lookup (:run/world child))]
                                  (try
                                    (is (= :review (:run/settlement-status child)))
                                    (is (some? fork))
                                    (d/transact conn [{:db/id (:db/id (requests/get-request conn (:id submitted)))
                                                       :work.request/return-pending? true}])
                                    (is (= {:state :returned}
                                           (recovery/recover-request! conn host-ctx (fn [_] room) (:id submitted))))
                                    (is (identical? fork (registry/lookup (:id fork))))
                                    (is (= :review (:run/settlement-status (run/run room id))))
                                    (finally (when fork (forks/discard! fork :test-cleanup))))))))))

(deftest a-live-adapter-keeps-its-own-cleanup-and-wallet
  (world/with-world [conn w]
    (@#'fixture/with-worker conn w
                            (fn [options]
                              (let [{:keys [room child recover! finish!]} (orphan conn w options true)]
                                (try
                                  (is (= {:state :live} (recover!)))
                                  (is (nil? (workers/assignment conn child)))
                                  (is (= 1M (get (resource/balance room) local/admission-unit)))
                                  (finally (finish!))))))))

(deftest mismatched-or-extra-assignment-refuses-refund
  (doseq [change [#(assoc % :id (random-uuid)) #(update % :generation inc) #(assoc % :input {:wrong true})]]
    (world/with-world [conn w]
      (@#'fixture/with-worker conn w
                              (fn [options]
                                (let [{:keys [room assignment recover! finish!]} (orphan conn w options true)]
           ;; Host closure can record mismatched historical identities, without
           ;; granting current enrollment authority or claiming a process.
                                  (workers/withdraw! conn (change assignment) #(Date.))
                                  (finish!)
                                  (is (thrown? clojure.lang.ExceptionInfo (recover!)))
                                  (is (= 1M (get (resource/balance room) local/admission-unit)))))))))

(deftest a-lost-return-response-can-be-reconciled-without-duplicate-credits
  (world/with-world [conn w]
    (@#'fixture/with-worker conn w
                            (fn [options]
                              (let [{:keys [room recover! finish!]} (orphan conn w options false)
                                    transfer resource/return! first? (atom true)]
                                (finish!)
                                (with-redefs [resource/return! (fn [& args]
                                                                 (let [receipt (apply transfer args)]
                                                                   (when (compare-and-set! first? true false)
                                                                     (throw (ex-info "Return response lost" {})))
                                                                   receipt))]
                                  (is (thrown? clojure.lang.ExceptionInfo (recover!))))
                                (is (= {:state :returned} (recover!)))
                                (is (= 2M (get (resource/balance room) local/admission-unit))))))))

(deftest acknowledged-uncertified-world-is-discarded-through-its-existing-capability
  (world/with-world [conn w]
    (@#'fixture/with-worker conn w
                            (fn [options]
                              (let [{:keys [room child world recover! finish!]} (orphan conn w (assoc options :live-world? true) true)]
                                (try
                                  (finish!)
                                  (is (identical? world (registry/lookup (:id world))))
                                  (is (= {:state :returned} (recover!)))
                                  (is (nil? (registry/lookup (:id world))))
                                  (is (= :discarded (:run/settlement-status (run/run room child))))
                                  (is (= :failed (:run/status (run/run room child))))
                                  (finally
                                    (when (registry/lookup (:id world))
                                      (forks/discard-deferred! world :test-cleanup)))))))))

(deftest missing-world-handle-is-not-proof-of-substrate-cleanup
  (world/with-world [conn w]
    (@#'fixture/with-worker conn w
                            (fn [options]
                              (let [{:keys [room child world id recover! finish!]} (orphan conn w (assoc options :live-world? true) true)]
                                (finish!)
                                (registry/unregister! (:id world))
                                (try
                                  (is (= {:state :awaiting-world-cleanup :run child} (recover!)))
                                  (is (:work.request/return-pending? (requests/get-request conn id)))
                                  (is (= 1M (get (resource/balance room) local/admission-unit)))
                                  (finally
                                    (registry/register! world)
                                    (forks/discard-deferred! world :test-cleanup))))))))

(deftest mismatched-run-provenance-and-truncated-graphs-cannot-close-or-refund
  (world/with-world [conn w]
    (@#'fixture/with-worker conn w
                            (fn [options]
                              (let [{:keys [room child job recover! finish!]} (orphan conn w options true)
                                    runs run/runs
                                    changes [(fn [r] (if (= job (:run/id r)) (assoc r :run/actor :wrong) r))
                                             (fn [r] (if (= child (:run/id r)) (assoc r :run/kind :workflow) r))
                                             (fn [r] (if (= child (:run/id r)) (assoc r :run/room :wrong) r))
                                             (fn [r] (if (= child (:run/id r)) (assoc r :run/actor :wrong) r))
                                             (fn [r] (if (= child (:run/id r)) (assoc r :run/agent-def-hash (random-uuid)) r))
                                             (fn [r] (if (= child (:run/id r)) (assoc r :run/isolation :none) r))
                                             (fn [r] (if (= child (:run/id r)) (assoc r :run/settlement-policy :review) r))]]
                                (finish!)
                                (doseq [change changes]
                                  (with-redefs [run/runs (fn [& args] (mapv change (apply runs args)))]
                                    (is (thrown? clojure.lang.ExceptionInfo (recover!)))))
                                (with-redefs [run/runs (fn [& args] (conj (vec (apply runs args)) {:run/id (random-uuid) :run/parent job}))]
                                  (is (thrown? clojure.lang.ExceptionInfo (recover!))))
                                (is (nil? (workers/assignment conn child)))
                                (is (= 1M (get (resource/balance room) local/admission-unit)))
                                (is (= {:state :awaiting-world-cleanup :run child} (recover!))))))))

(deftest concurrent-recovery-and-lost-return-marker-responses-do-not-duplicate-credit
  (world/with-world [conn w]
    (@#'fixture/with-worker conn w
                            (fn [options]
                              (let [{:keys [room job id recover! finish!]} (orphan conn w options false)
                                    returning (CountDownLatch. 2) transfer resource/return! mark requests/mark-returned!]
                                (finish!)
                                (with-redefs [resource/return! (fn [r run parent balance]
                                                                 (when (= job run)
                                                                   (.countDown returning)
                                                                   (when-not (.await returning 10 TimeUnit/SECONDS)
                                                                     (throw (ex-info "Test did not reach concurrent return" {}))))
                                                                 (transfer r run parent balance))]
                                  (let [recover (fn [] (try (recover!) (catch Exception error error)))
                                        left (future (recover)) right (future (recover))
                                        results [(deref left 20000 ::timeout) (deref right 20000 ::timeout)]]
                                    (is (some #{{:state :returned}} results))
             ;; Kontor may reject a simultaneous replay before the winner's
             ;; committed receipt becomes readable. Such a caller fails closed
             ;; and explicitly retries; do not swallow unrelated failures.
                                    (doseq [result results]
                                      (is (or (= {:state :returned} result)
                                              (and (instance? Exception result)
                                                   (some #(contains? #{:sealing/silent-retract-of-posted :kontor.resource/insufficient}
                                                                     (:type (ex-data %)))
                                                         (take-while some? (iterate ex-cause result)))))))))
                                (is (nil? (recover!)))
                                (is (= 2M (get (resource/balance room) local/admission-unit)))
         ;; Simulate a lost marker reply separately, after reopening only the
         ;; host journal flag. It must not mint or re-execute any resource step.
                                (d/transact conn [{:db/id (:db/id (requests/get-request conn id))
                                                   :work.request/return-pending? true}])
                                (with-redefs [requests/mark-returned! (fn [& args] (apply mark args)
                                                                        (throw (ex-info "Marker response lost" {})))]
                                  (is (thrown? clojure.lang.ExceptionInfo (recover!))))
                                (is (nil? (recover!)))
                                (is (= 2M (get (resource/balance room) local/admission-unit))))))))

(deftest a-child-published-during-the-cleanup-join-requires-a-fresh-recovery-pass
  (world/with-world [conn w]
    (@#'fixture/with-worker conn w
                            (fn [options]
                              (let [{:keys [room job id recover! finish!]} (orphan conn w options false)
                                    joined? (atom false) join evaluation/await-cleanups-for! runs run/runs]
                                (finish!)
                                (with-redefs [evaluation/await-cleanups-for! (fn [& args] (apply join args) (reset! joined? true))
                                              run/runs (fn [& args]
                                                         (cond-> (vec (apply runs args))
                                                           @joined? (conj {:run/id (random-uuid) :run/parent job :run/status :failed})))]
                                  (is (thrown? clojure.lang.ExceptionInfo (recover!))))
                                (is (:work.request/return-pending? (requests/get-request conn id)))
                                (is (= 1M (get (resource/balance room) local/admission-unit))))))))
