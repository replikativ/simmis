(ns is.simm.ops.worker-wait-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.run :as run]
            [dvergr.jobs :as jobs]
            [dvergr.resource :as resource]
            [is.simm.authority.world :as world]
            [is.simm.model.workers :as workers]
            [is.simm.model.workers-test :as fixture]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.work :as work]
            [is.simm.ops.work-test :as work-fixture]
            [is.simm.ops.worker-wait :as wait]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util Date]
           [java.util.concurrent CountDownLatch]))

(defn- start-waiter [conn id options]
  (let [started (promise) closed (promise)
        result (future
                 (let [watch (wait/watch! conn id options)]
                   (deliver started (Thread/currentThread))
                   (try (wait/await-stop! watch)
                        (finally
                          (wait/close! watch)
                          (deliver closed (.isInterrupted (Thread/currentThread)))))))]
    {:result result :thread (deref started 10000 nil) :closed closed}))

(defn- stop! [conn {:keys [assignment lease secret]}]
  (workers/acknowledge-stop! conn (:id assignment) lease secret (Date. 12000)))

(deftest acknowledgement-before-the-first-read-is-observed-without-an-event
  (fixture/with-store
    (fn [conn]
      (let [{:keys [assignment] :as owned} (fixture/claimed conn)
            stopped (stop! conn owned)
            baseline (count @(:listeners (meta conn)))
            watch (wait/watch! conn (:id assignment) {})]
        (try
          (is (= stopped (:assignment (wait/await-stop! watch))))
          (finally (wait/close! watch)))
        (is (= baseline (count @(:listeners (meta conn)))))))))

(deftest a-preexisting-interrupt-is-remembered-cleared-for-cleanup-and-restored
  (fixture/with-store
    (fn [conn]
      (let [{:keys [assignment] :as owned} (fixture/claimed conn)
            _ (stop! conn owned)
            result (future
                     (.interrupt (Thread/currentThread))
                     (let [watch (wait/watch! conn (:id assignment) {})
                           receipt (try (wait/await-stop! watch) (finally (wait/close! watch)))]
                       {:cancelled? (:cancelled? receipt) :interrupted? (Thread/interrupted)}))]
        (is (= {:cancelled? true :interrupted? true} (deref result 10000 nil)))))))

(deftest watch-before-publication-sees-and-withdraws-a-never-claimed-assignment
  (fixture/with-store
    (fn [conn]
      (let [{:keys [assignment]} (fixture/fixture conn)
            pending (assoc assignment :id (random-uuid))
            {:keys [result]} (start-waiter conn (:id pending) {:cancelled? (constantly true)})]
        (is (not (realized? result)))
        (workers/enqueue! conn pending)
        (let [stopped (deref result 10000 nil)]
          (is (= :withdrawn (get-in stopped [:assignment :simmis.worker.assignment/state])))
          (is (get-in stopped [:assignment :simmis.worker.assignment/stop-ack-at]))
          (is (:cancelled? stopped)))))))

(deftest acknowledgement-between-read-and-take-cannot-be-missed
  (fixture/with-store
    (fn [conn]
      (let [{:keys [assignment] :as owned} (fixture/claimed conn)
            initial (workers/assignment conn (:id assignment))
            watch (wait/watch! conn (:id assignment) {:clock #(Date. 11000)})
            read workers/assignment first? (atom true)]
        (try
          (with-redefs [workers/assignment (fn [c id]
                                             (if (compare-and-set! first? true false)
                                               (do (stop! conn owned) initial)
                                               (read c id)))]
            (is (:simmis.worker.assignment/stop-ack-at (:assignment (wait/await-stop! watch)))))
          (finally (wait/close! watch)))))))

(deftest interrupt-requests-cancel-but-retains-the-owner-until-physical-stop
  (fixture/with-store
    (fn [conn]
      (let [{:keys [assignment] :as owned} (fixture/claimed conn)
            cancelled (promise) cancel workers/cancel!
            baseline (count @(:listeners (meta conn)))]
        (with-redefs [workers/cancel! (fn [& args] (let [row (apply cancel args)] (deliver cancelled row) row))]
          (let [{:keys [result thread closed]} (start-waiter conn (:id assignment) {:clock #(Date. 11000)})]
            (try
              (is (instance? Thread thread))
              (.interrupt ^Thread thread)
              (is (:simmis.worker.assignment/cancel-at (deref cancelled 10000 nil)))
              (is (not (realized? result)))
              (is (nil? (:simmis.worker.assignment/stop-ack-at (workers/assignment conn (:id assignment)))))
              (stop! conn owned)
              (is (:cancelled? (deref result 10000 nil)))
              (is (= true (deref closed 10000 nil)))
              (finally (stop! conn owned) (deref result 10000 nil)))))
        (is (= baseline (count @(:listeners (meta conn)))))))))

(deftest expiry-quarantines-but-does-not-release-or-spin-on-the-expired-deadline
  (fixture/with-store
    (fn [conn]
      (let [{:keys [assignment] :as owned} (fixture/claimed conn)
            cancelled (promise) waiting (promise) cancel workers/cancel!
            event @#'wait/await-event! calls (atom 0)]
        (with-redefs [workers/cancel! (fn [& args] (swap! calls inc)
                                        (let [row (apply cancel args)] (deliver cancelled row) row))
                      wait/await-event! (fn [wake interrupted? deadline]
                                          (deliver waiting deadline)
                                          (event wake interrupted? deadline))]
          (let [{:keys [result]} (start-waiter conn (:id assignment) {:clock #(Date. 20000)})]
            (try
              (is (:simmis.worker.assignment/cancel-at (deref cancelled 10000 nil)))
              (is (nil? (deref waiting 10000 ::timeout)) "quarantine waits on events, not an expired timeout")
              (is (not (realized? result)))
              (is (= 1 @calls))
              (stop! conn owned)
              (is (:cancelled? (deref result 10000 nil)))
              (finally (stop! conn owned) (deref result 10000 nil)))))))))

(deftest failed-cancellation-write-cannot-declare-the-remote-process-stopped
  (fixture/with-store
    (fn [conn]
      (let [{:keys [assignment] :as owned} (fixture/claimed conn)
            attempted (promise) waiting (promise) event @#'wait/await-event!]
        (with-redefs [workers/cancel! (fn [& _] (deliver attempted true) (throw (ex-info "writer unavailable" {})))
                      wait/await-event! (fn [wake interrupted? deadline]
                                          (deliver waiting true) (event wake interrupted? deadline))]
          (let [{:keys [result]} (start-waiter conn (:id assignment) {:cancelled? (constantly true)
                                                                      :clock #(Date. 11000)})]
            (try
              (is (= true (deref attempted 10000 nil)))
              (is (= true (deref waiting 10000 nil)))
              (is (not (realized? result)))
              (stop! conn owned)
              (is (:cancelled? (deref result 10000 nil)))
              (finally (stop! conn owned) (deref result 10000 nil)))))))))

(deftest temporarily-unreadable-assignment-holds-ownership-until-a-real-db-event
  (fixture/with-store
    (fn [conn]
      (let [{:keys [assignment lease secret] :as owned} (fixture/claimed conn)
            read workers/assignment first? (atom true) waiting (promise)
            cancelled (promise) event @#'wait/await-event! cancel workers/cancel!]
        (with-redefs [workers/assignment (fn [c id]
                                           (if (compare-and-set! first? true false)
                                             (throw (ex-info "temporarily unavailable" {}))
                                             (read c id)))
                      workers/cancel! (fn [& args] (let [row (apply cancel args)] (deliver cancelled row) row))
                      wait/await-event! (fn [wake interrupted? deadline]
                                          (deliver waiting true) (event wake interrupted? deadline))]
          (let [{:keys [result]} (start-waiter conn (:id assignment) {:clock #(Date. 11000)})]
            (try
              (is (= true (deref waiting 10000 nil)))
              (is (not (realized? result)))
              (workers/heartbeat! conn (:id assignment) lease secret (Date. 11000) 10000)
              (is (:simmis.worker.assignment/cancel-at (deref cancelled 10000 nil)))
              (is (not (realized? result)))
              (stop! conn owned)
              (is (:cancelled? (deref result 10000 nil)))
              (finally (stop! conn owned) (deref result 10000 nil)))))))))

(deftest wrapped-interruption-during-a-db-write-is-not-forgotten
  (fixture/with-store
    (fn [conn]
      (let [{:keys [assignment] :as owned} (fixture/claimed conn)
            waiting (promise) event @#'wait/await-event!]
        (with-redefs [workers/cancel! (fn [& _] (throw (ex-info "wrapped interruption" {} (InterruptedException.))))
                      wait/await-event! (fn [wake interrupted? deadline]
                                          (deliver waiting true) (event wake interrupted? deadline))]
          (let [{:keys [result closed]} (start-waiter conn (:id assignment) {:cancelled? (constantly true)
                                                                             :clock #(Date. 11000)})]
            (try
              (is (= true (deref waiting 10000 nil)))
              (is (not (realized? result)))
              (stop! conn owned)
              (is (:cancelled? (deref result 10000 nil)))
              (is (= true (deref closed 10000 nil)))
              (finally (stop! conn owned) (deref result 10000 nil)))))))))

(deftest lost-cancel-response-is-reobserved-without-releasing-or-repeating-the-write
  (fixture/with-store
    (fn [conn]
      (let [{:keys [assignment] :as owned} (fixture/claimed conn)
            committed (promise) cancel workers/cancel! calls (atom 0)]
        (with-redefs [workers/cancel! (fn [& args]
                                        (swap! calls inc)
                                        (let [row (apply cancel args)]
                                          (deliver committed row)
                                          (throw (ex-info "lost response" {}))))]
          (let [{:keys [result]} (start-waiter conn (:id assignment) {:cancelled? (constantly true)
                                                                      :clock #(Date. 11000)})]
            (try
              (is (:simmis.worker.assignment/cancel-at (deref committed 10000 nil)))
              (is (not (realized? result)))
              (stop! conn owned)
              (is (:cancelled? (deref result 10000 nil)))
              (is (= 1 @calls))
              (finally (stop! conn owned) (deref result 10000 nil)))))))))

(deftest interruption-during-cancel-publication-is-resolved-without-early-exit
  (fixture/with-store
    (fn [conn]
      (let [{:keys [assignment] :as owned} (fixture/claimed conn)
            publishing (promise) waiting (promise) release (CountDownLatch. 1) cancel workers/cancel!
            first? (atom true) event @#'wait/await-event!]
        (with-redefs [workers/cancel! (fn [& args]
                                        (when (compare-and-set! first? true false)
                                          (deliver publishing (Thread/currentThread))
                                          (.await release))
                                        (apply cancel args))
                      wait/await-event! (fn [wake interrupted? deadline]
                                          (deliver waiting true) (event wake interrupted? deadline))]
          (let [{:keys [result closed]} (start-waiter conn (:id assignment) {:cancelled? (constantly true)
                                                                             :clock #(Date. 11000)})]
            (try
              (.interrupt ^Thread (deref publishing 10000 nil))
              (is (= true (deref waiting 10000 nil)))
              (is (not (realized? result)))
              (stop! conn owned)
              (is (:cancelled? (deref result 10000 nil)))
              (is (= true (deref closed 10000 nil)))
              (finally (.countDown release) (stop! conn owned) (deref result 10000 nil)))))))))

(deftest real-run-cancellation-keeps-supervisor-and-wallet-until-external-stop
  (world/with-world [conn w]
    (@#'work-fixture/with-room
     (fn [room]
       (let [[input prepared] (@#'work-fixture/fixture)
             worker (random-uuid) secret (workers/new-secret)
             enrollment {:id worker :by (:owner w) :room (:room w)
                         :secret (workers/new-secret) :config {:adapter :fixture}}
             _ (workers/enroll! conn enrollment)
             entered (promise) cancelled (promise) cancel workers/cancel!
             protocol (evaluation/make-protocol
                       {:id :worker-test/physical-stop :version 1
                        :run (fn [{id :run/id :keys [cancelled?]}]
                               (let [watch (wait/watch! conn id {:cancelled? cancelled?})]
                                 (try
                                   (workers/enqueue! conn {:id id :worker worker :generation 1 :room (:room w)
                                                           :request (random-uuid) :job (random-uuid)
                                                           :input {:task "deterministic remote fixture"}})
                                   (deliver entered id)
                                   (let [receipt (wait/await-stop! watch)]
                                     ;; The private assignment contains hashes
                                     ;; and refs: never persist it as Run value.
                                     {:stopped? (boolean (get-in receipt [:assignment :simmis.worker.assignment/stop-ack-at]))
                                      :cancelled? (:cancelled? receipt)})
                                   (finally (wait/close! watch)))))})
             env (environment/make-environment
                  {:id :worker-test/cancel :task "remote ownership fixture"
                   :verifier {:id :simmis/work-completion :version 1 :basis "simmis-work-completion-v1"}
                   :limits {:timeout-ms 60000 :cancel-timeout-ms 1000}
                   :world {:isolation :ctx :settlement :discard :effects work/effects
                           :protocol (evaluation/protocol-ref protocol)
                           :resources (work/resources (:limits input))}})
             prepared (assoc prepared :environment env :protocol protocol
                             :basis {:environment env :protocol (evaluation/protocol-ref protocol)})
             owner (ops/party-principal (:owner w)) host-ctx ec/*execution-context*
             grant (work/resources (:limits input))
             owned (atom nil)]
         (with-redefs [workers/cancel! (fn [& args] (let [row (apply cancel args)] (deliver cancelled row) row))]
           (try
             (resource/mint! room {:id (random-uuid) :resources grant})
             (let [request (work/submit-prepared! conn host-ctx owner (:room w) room input prepared)
                   id (deref entered 10000 nil)
                   row (workers/claim! conn id worker 1 (random-uuid) secret (Date.) 30000)
                   lease (:simmis.worker.assignment/lease row)]
               (reset! owned [id lease])
               (run/cancel-run! (:job request))
               (is (:simmis.worker.assignment/cancel-at (deref cancelled 10000 nil)))
               (is (thrown? clojure.lang.ExceptionInfo
                            (evaluation/await-cleanups-for! room (:job request) 1)))
               (is (:return-pending? (work/status conn host-ctx owner (:room w) (:id request))))
               (is (every? zero? (vals (resource/balance room))))
               (workers/acknowledge-stop! conn id lease secret (Date.))
               (is (:settled? (jobs/status (:job request) 25000)))
               (is (= grant (resource/balance room)))
               (is (false? (:return-pending? (work/status conn host-ctx owner (:room w) (:id request))))))
             (finally
               (when-let [[id lease] @owned]
                 (workers/acknowledge-stop! conn id lease secret (Date.)))))))))))
