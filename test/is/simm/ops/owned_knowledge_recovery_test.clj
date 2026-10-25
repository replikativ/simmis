(ns is.simm.ops.owned-knowledge-recovery-test
  "Ready-only recovery through real frozen artifacts, App/Room lifetimes and
   EACL authority. No source recapture, Job, model, refund or default activation."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-batch-recovery :as recovery]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.workflow-schedules :as schedules]
            [is.simm.ops.knowledge-schedule-proof-test :as scheduled-fixture]
            [is.simm.ops.owned-knowledge-schedule-test :as dispatch-fixture]
            [is.simm.ops.workflow-schedules :as operations]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.dvergr :as principals]
            [is.simm.ops.knowledge-batch-proof :as proof]
            [is.simm.ops.knowledge-batch-proof-test :as fixture]
            [is.simm.ops.owned-knowledge-discovery :as discovery]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.work :as work])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- with-ready [f]
  (@#'fixture/fixture
   (fn [{:keys [conn w ready] :as data}]
     (d/transact conn recovery/schema)
     (let [b (batches/batch @conn (:batch ready))]
       (f (assoc data :principal (principals/party-principal (:owner w)) :handle (str (:room w))
                      :request {:batch (:batch ready) :expected-claim (:knowledge.batch/claim b)
                                :expected-frozen (:knowledge.batch/frozen-id b)
                                :request-id (random-uuid) :reason "Skip this test input version"}))))))

(defn- abandon! [{:keys [host principal handle request]}]
  (discovery/abandon-ready! host principal handle request))

(deftest fixed-host-skip-is-source-free-and-replay-keeps-original-audit
  (with-ready
    (fn [{:keys [conn ready request] :as data}]
      (let [forbidden (fn [& _] (throw (AssertionError. "Recovery must not open today's source or prepare a model")))
            before (streams/stream @conn (:stream ready))]
        (with-redefs [reads/resolve-host-read! forbidden work/prepare forbidden discovery/prepare-batch! forbidden]
          (let [result (abandon! data) original (batches/batch @conn (:batch ready))]
            (is (= :abandoned (:status result)))
            (is (= :skip (:mode result)))
            (is (= result (abandon! data)))
            (is (= original (batches/batch @conn (:batch ready))))
            (is (= (dissoc before :knowledge.stream/active-batch) (streams/stream @conn (:stream ready))))
            (is (= (:request-id request) (:knowledge.batch/recovery-client original)))
            (is (empty? (requests/requests conn)))))))))

(deftest missing-merge-authority-never-reaches-frozen-artifacts
  (with-ready
    (fn [{:keys [w host handle request conn]}]
      (let [calls (atom 0) before (d/commit-id @conn)]
        (with-redefs [proof/retained! (fn [& _] (swap! calls inc) (throw (AssertionError. "Unauthorized artifact read")))]
          (doseq [party [(:member w) (:non-member w) (:admin w) (:team-agent w)]]
            (is (thrown? Exception (discovery/abandon-ready! host (principals/party-principal party) handle request)))))
        (is (zero? @calls))
        (is (= before (d/commit-id @conn)))))))

(deftest current-authority-is-rechecked-by-the-serialized-writer
  (with-ready
    (fn [{:keys [conn w ready] :as data}]
      (let [retained! proof/retained!]
        (with-redefs [proof/retained!
                      (fn [& args]
                        (let [value (apply retained! args)]
                          (d/transact conn [{:room/id (:room w) :room/owner [:party/id (:member w)]}])
                          value))]
          (is (thrown? Exception (abandon! data))))
        (is (= :ready (:knowledge.batch/state (batches/batch @conn (:batch ready)))))
        (is (= (:batch ready) (:knowledge.stream/active-batch (streams/stream @conn (:stream ready)))))))))

(deftest lost-abandonment-ack-is-recovered-only-by-the-same-complete-intent
  (with-ready
    (fn [{:keys [conn ready] :as data}]
      (let [transact! d/transact lost? (atom false)]
        (with-redefs [d/transact
                      (fn [c tx]
                        (let [report (transact! c tx)]
                          (when (and (identical? c conn)
                                     (= :abandoned-skip (:knowledge.batch/outcome (batches/batch @conn (:batch ready))))
                                     (compare-and-set! lost? false true))
                            (throw (ex-info "Lost abandonment ACK" {})))
                          report))]
          (is (thrown? Exception (abandon! data))))
        (is @lost?)
        (is (= :abandoned (:status (abandon! data))))
        (is (thrown? Exception (abandon! (assoc-in data [:request :reason] "Different intent"))))))))

(deftest a-canonical-request-winning-the-writer-race-prevents-abandonment
  (with-ready
    (fn [{:keys [conn ready] :as data}]
      (let [transact! d/transact raced? (atom false)]
        (with-redefs [d/transact
                      (fn [c tx]
                        (when (and (identical? c conn) (compare-and-set! raced? false true))
                          (@#'fixture/joint-claim! data))
                        (transact! c tx))]
          (is (thrown? Exception (abandon! data))))
        (is @raced?)
        (is (= :submitted (:knowledge.batch/state (batches/batch @conn (:batch ready)))))
        (is (nil? (:knowledge.batch/recovery-id (batches/batch @conn (:batch ready)))))))))

(deftest a-replacement-owner-uses-the-original-authors-physical-discovery-gate
  (with-ready
    (fn [{:keys [conn host w handle request ready]}]
      (let [entered (promise) release (CountDownLatch. 1) holder (atom nil) recovery-worker (atom nil)]
        (try
          (reset! holder
                  (future
                    (try
                      (@#'discovery/with-room!
                       host (principals/party-principal (:owner w)) handle
                       (fn [_ _ _ check!]
                         (deliver entered true)
                         (when-not (.await release 15 TimeUnit/SECONDS) (throw (ex-info "Fixture release timeout" {})))
                         (check!)))
                      (catch clojure.lang.ExceptionInfo error
                        (if (= :is.simm.ops.owned-reads/refused (:type (ex-data error))) :revoked (throw error))))))
          (is (= true (deref entered 15000 :timeout)))
          (d/transact conn [{:room/id (:room w) :room/owner [:party/id (:member w)]}])
          (reset! recovery-worker
                  (future (try (discovery/abandon-ready! host (principals/party-principal (:member w)) handle request)
                               (catch Exception _ :held))))
          (deref @recovery-worker 150 :still-blocked)
          (is (= :ready (:knowledge.batch/state (batches/batch @conn (:batch ready))))
              "The original author's live physical gate must prevent recovery")
          (.countDown release)
          (deref @holder 15000 :timeout)
          (deref @recovery-worker 15000 :timeout)
          (is (= :abandoned (:status (discovery/abandon-ready! host (principals/party-principal (:member w)) handle request))))
          (let [b (batches/batch @conn (:batch ready))]
            (is (= (:owner w) (:knowledge.batch/by b)))
            (is (= (:member w) (:knowledge.batch/recovery-by b))))
          (finally
            (.countDown release)
            (when @holder (deref @holder 15000 nil))
            (when @recovery-worker (deref @recovery-worker 15000 nil))))))))


(deftest scheduled-abandonment-resolves-its-slot-without-resuming-or-overwriting-later-work
  (@#'scheduled-fixture/with-schedule
   (fn [{:keys [conn host w value activation batch-input]}]
     (d/transact conn recovery/schema)
     (let [bid (get-in value [:batch :batch]) oid (:occurrence value)
           b (batches/batch @conn bid)
           request {:batch bid :expected-claim (:knowledge.batch/claim b) :expected-frozen (:knowledge.batch/frozen-id b)
                    :request-id (random-uuid) :reason "Decline this interrupted synthetic batch"}
           principal (principals/party-principal (:owner w)) handle (str (:room w))]
       (schedules/interrupt-owned! conn activation oid (:claim value) :interrupted)
       (is (= :abandoned (:status (discovery/abandon-ready! host principal handle request))))
       (let [a (schedules/activation conn (:activation value)) o (schedules/occurrence conn oid)]
         (is (false? (:schedule.activation/enabled? a)))
         (is (nil? (:schedule.activation/active-occurrence a)))
         (is (= :skipped (:schedule.occurrence/state o)))
         (is (= :batch-abandoned (:schedule.occurrence/reason o)))
         (is (nil? (:schedule.activation/last-processed-input a))))
       ;; Explicit user resume and a genuine later due slot are separate actions.
       (schedules/set-enabled! conn (:activation value) true)
       (schedules/claim-due! conn (schedules/activation conn (:activation value)) {:interval-ms 60000} (java.util.Date. 120000))
       (let [later (discovery/prepare-batch! host principal handle batch-input)
             before-a (schedules/activation conn (:activation value))
             before-stream (streams/stream @conn (:knowledge.batch/stream b))]
         (is (= :ready (:status later)))
         (is (not= oid (:schedule.activation/active-occurrence before-a)))
         (is (= :abandoned (:status (discovery/abandon-ready! host principal handle request))))
         (schedules/interrupt-owned! conn activation oid (:claim value) :restart)
         (schedules/block-owned! conn activation :owned-dispatch-unavailable)
         (is (= before-a (schedules/activation conn (:activation value))))
         (is (= before-stream (streams/stream @conn (:knowledge.batch/stream b)))))))))


(defn- skip-request [conn bid]
  (let [b (batches/batch @conn bid)]
    {:batch bid :expected-claim (:knowledge.batch/claim b) :expected-frozen (:knowledge.batch/frozen-id b)
     :request-id (random-uuid) :reason "Explicitly skip the in-flight test batch"}))

(deftest abandonment-winning-against-late-provider-failure-is-not-overwritten
  (@#'dispatch-fixture/with-schedule
   (fn [{:keys [conn host row due principal handle]}]
     (d/transact conn recovery/schema)
     (let [entered (promise) release (CountDownLatch. 1) worker (atom nil) submissions (atom 0)]
       (try
         (with-redefs [work/prepare
                       (fn [& _]
                         (let [a (schedules/activation conn (:schedule.activation/id row))
                               o (schedules/occurrence conn (:schedule.activation/active-occurrence a))]
                           (deliver entered o)
                           (when-not (.await release 15 TimeUnit/SECONDS) (throw (ex-info "Fixture release timeout" {})))
                           (throw (ex-info "Preparation failed after abandonment won" {}))))
                       work/submit-prepared! (fn [& _] (swap! submissions inc) (throw (AssertionError. "Abandoned work may not submit")))]
           (reset! worker (future (operations/tick-owned! host due) :returned))
           (let [o (deref entered 15000 :timeout)]
             (is (map? o))
             (when (map? o)
               (is (= :abandoned (:status (discovery/abandon-ready! host principal handle
                                            (skip-request conn (:schedule.occurrence/knowledge-batch o)))))))
               (.countDown release)
               (is (= :returned (deref @worker 15000 :timeout)))
               (let [a (schedules/activation conn (:schedule.activation/id row)) after (schedules/occurrence conn (:schedule.occurrence/id o))]
                 (is (true? (:schedule.activation/enabled? a)))
                 (is (nil? (:schedule.activation/active-occurrence a)))
                 (is (= :skipped (:schedule.occurrence/state after)))
                 (is (= :batch-abandoned (:schedule.occurrence/reason after))))))
         (is (zero? @submissions))
         (is (empty? (requests/requests conn)))
         (finally
           (.countDown release)
           (when @worker (deref @worker 15000 nil))))))))

(deftest stale-startup-reconciliation-cannot-undo-a-ready-batch-abandonment
  (@#'scheduled-fixture/with-schedule
   (fn [{:keys [conn host w value]}]
     (d/transact conn recovery/schema)
     (let [entered (promise) release (CountDownLatch. 1) worker (atom nil)
           admitting schedules/admitting-occurrences]
       (try
         (with-redefs [schedules/admitting-occurrences
                       (fn [c]
                         (let [snapshot (vec (admitting c))]
                           (deliver entered true)
                           (when-not (.await release 15 TimeUnit/SECONDS) (throw (ex-info "Fixture release timeout" {})))
                           snapshot))]
           (reset! worker (future (operations/recover-owned! host) :returned))
           (is (= true (deref entered 15000 :timeout)))
           (is (= :abandoned (:status (discovery/abandon-ready! host (principals/party-principal (:owner w))
                                        (str (:room w)) (skip-request conn (get-in value [:batch :batch]))))))
           (.countDown release)
           (is (= :returned (deref @worker 15000 :timeout)))
           (let [a (schedules/activation conn (:activation value)) o (schedules/occurrence conn (:occurrence value))]
             (is (true? (:schedule.activation/enabled? a)))
             (is (nil? (:schedule.activation/active-occurrence a)))
             (is (= :skipped (:schedule.occurrence/state o)))
             (is (= :batch-abandoned (:schedule.occurrence/reason o)))))
         (finally
           (.countDown release)
           (when @worker (deref @worker 15000 nil))))))))
