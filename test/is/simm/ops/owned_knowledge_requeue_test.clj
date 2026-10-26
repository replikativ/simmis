(ns is.simm.ops.owned-knowledge-requeue-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.repo :as repo]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-batch-recovery :as recovery]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.workflow-schedules :as schedules]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.dvergr :as principals]
            [is.simm.ops.knowledge-batch-proof :as proof]
            [is.simm.ops.knowledge-batch-proof-test :as fixture]
            [is.simm.ops.knowledge-schedule-proof-test :as scheduled-fixture]
            [is.simm.ops.owned-capturing-recovery-test :as capturing-fixture]
            [is.simm.ops.owned-knowledge-batches-test :as batch-fixture]
            [is.simm.ops.owned-knowledge-discovery-test :as source-fixture]
            [is.simm.ops.owned-knowledge-discovery :as discovery])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- with-ready [f]
  (@#'fixture/fixture
   (fn [{:keys [conn w ready] :as data}]
     (d/transact conn recovery/schema)
     (let [b (batches/batch @conn (:batch ready))]
       (f (assoc data :batch b :proof (proof/ready! conn (:owner w) (:room w) (:batch-origin ready))
                      :principal (principals/party-principal (:owner w)) :handle (str (:room w))
                      :request {:batch (:batch ready) :expected-claim (:knowledge.batch/claim b)
                                :expected-frozen (:knowledge.batch/frozen-id b)
                                :request-id (random-uuid) :reason "Restore source-version eligibility"}))))))

(defn- requeue! [{:keys [host principal handle request]}]
  (if (:expected-frozen request)
    (discovery/requeue-ready! host principal handle request)
    (discovery/requeue-capturing! host principal handle request)))

(deftest unchanged-source-requeue-has-a-new-batch-identity-with-exactly-one-generation-step
  (doseq [with-batch [with-ready @#'capturing-fixture/with-capturing]]
    (with-batch
      (fn [{:keys [conn batch host principal handle batch-input] :as data}]
        (let [sid (:knowledge.batch/stream batch) before (streams/stream @conn sid)
              old-item (first (filter #(= (:knowledge.batch/id batch) (:knowledge.item/batch %)) (streams/items @conn sid)))
              result (requeue! data) closed (batches/batch @conn (:knowledge.batch/id batch))]
          (is (= :requeue (:mode result)))
          (is (= :abandoned-requeue (:knowledge.batch/outcome closed)))
          (is (= [0 1] (mapv closed [:knowledge.batch/recovery-generation-before :knowledge.batch/recovery-generation-after])))
          (is (= (-> before (dissoc :knowledge.stream/active-batch) (assoc :knowledge.stream/batch-generation 1))
                 (streams/stream @conn sid)))
          (let [restored (d/pull @conn '[*] (:db/id old-item))]
            (is (= (-> old-item (dissoc :knowledge.item/batch)
                       (assoc :knowledge.item/state :pending :knowledge.item/pending-stream sid)) restored)))
          (is (= result (requeue! data)))
          (let [later (discovery/prepare-batch! host principal handle batch-input)
                snapshot (streams/stream @conn sid)
                selected (:selection (proof/ready! conn (:knowledge.batch/by batch) (:knowledge.batch/room batch) (:batch-origin later)))]
            (is (= :ready (:status later)))
            (is (not= (:knowledge.batch/id batch) (:batch later)))
            (is (= (:knowledge.item/id old-item) (get-in selected [:items 0 :id])))
            (is (= result (requeue! data)))
            (is (= snapshot (streams/stream @conn sid)))
            (is (= closed (batches/batch @conn (:knowledge.batch/id batch))))))))))

(deftest lost-ack-and-conflicting-decisions-cannot-increment-twice
  (with-ready
    (fn [{:keys [conn batch host principal handle request] :as data}]
      (let [transact! d/transact lost? (atom false) sid (:knowledge.batch/stream batch)]
        (with-redefs [d/transact
                      (fn [c tx]
                        (let [result (transact! c tx)]
                          (when (and (identical? c conn)
                                     (= :abandoned-requeue (:knowledge.batch/outcome (batches/batch @conn (:batch request))))
                                     (compare-and-set! lost? false true))
                            (throw (ex-info "Lost requeue ACK" {})))
                          result))]
          (is (thrown? Exception (requeue! data))))
        (is @lost?)
        (is (= :requeue (:mode (requeue! data))))
        (is (= 1 (:knowledge.stream/batch-generation (streams/stream @conn sid))))
        (is (thrown? Exception (requeue! (assoc-in data [:request :request-id] (random-uuid)))))
        (is (thrown? Exception (discovery/abandon-ready! host principal handle request)))
        (is (= 1 (:knowledge.stream/batch-generation (streams/stream @conn sid))))))))

(deftest concurrent-duplicate-intent-has-one-generation-increment
  (with-ready
    (fn [{:keys [conn batch] :as data}]
      (let [start (CountDownLatch. 1)
            workers (mapv (fn [_] (future (.await start 15 TimeUnit/SECONDS)
                                         (try (requeue! data) (catch Exception _ :held)))) (range 2))]
        (try
          (.countDown start)
          (let [results (mapv #(deref % 15000 :timeout) workers)]
            (is (not-any? #{:timeout} results))
            (is (some #(= :requeue (:mode %)) results)))
          (is (= :requeue (:mode (requeue! data))))
          (is (= 1 (:knowledge.stream/batch-generation (streams/stream @conn (:knowledge.batch/stream batch)))))
          (finally (.countDown start) (doseq [worker workers] (deref worker 15000 nil))))))))

(deftest original-requeue-replay-preserves-genuine-removal-restoration-and-new-order
  (with-ready
    (fn [{:keys [conn owned proof batch host principal handle batch-input] :as data}]
      (let [sid (:knowledge.batch/stream batch) selected (first (get-in proof [:selection :items]))
            path (:path selected) text (get-in proof [:frozen :captured :inputs (str "/in/knowledge/sources/" path)])
            result (requeue! data)]
        (repo/remove! owned path) (repo/stage-all! owned) (repo/commit! owned {:message "Remove requeued source"})
        (discovery/refresh! host principal handle batch-input)
        (let [removed (d/pull @conn '[*] [:knowledge.item/id (:id selected)]) snapshot @conn]
          (is (= :removed (:knowledge.item/state removed)))
          (is (= result (requeue! data)))
          (is (= (d/commit-id snapshot) (d/commit-id @conn))))
        (@#'source-fixture/write! owned path text)
        (discovery/refresh! host principal handle batch-input)
        (let [restored (d/pull @conn '[*] [:knowledge.item/id (:id selected)]) snapshot @conn]
          (is (= :pending (:knowledge.item/state restored)))
          (is (> (:knowledge.item/order restored) (:order selected)))
          (is (= 1 (:knowledge.stream/batch-generation (streams/stream @conn sid))))
          (is (= result (requeue! data)))
          (is (= (d/commit-id snapshot) (d/commit-id @conn))))))))

(deftest history-is-independent-of-later-item-state-and-order
  (with-ready
    (fn [{:keys [conn batch] :as data}]
      (let [sid (:knowledge.batch/stream batch)
            i (first (filter #(= (:knowledge.batch/id batch) (:knowledge.item/batch %)) (streams/items @conn sid)))
            result (requeue! data)]
        ;; Metadata-only historical observations, not processing certificates.
        (doseq [state [:claimed :processed :superseded :removed :pending]]
          (d/transact conn [{:db/id (:db/id i) :knowledge.item/state state
                             :knowledge.item/order (+ 100 (:knowledge.item/order i))}])
          (let [before @conn]
            (is (= result (requeue! data)))
            (is (= (d/commit-id before) (d/commit-id @conn)))))))))

(deftest canonical-request-wins-before-requeue-without-generation-or-wallet-changes
  (with-ready
    (fn [{:keys [conn batch] :as data}]
      (let [transact! d/transact raced? (atom false) before (streams/stream @conn (:knowledge.batch/stream batch))]
        (with-redefs [d/transact
                      (fn [c tx]
                        (when (and (identical? c conn) (compare-and-set! raced? false true))
                          (@#'fixture/joint-claim! data))
                        (transact! c tx))]
          (is (thrown? Exception (requeue! data))))
        (is @raced?)
        (is (= before (streams/stream @conn (:knowledge.batch/stream batch))))
        (is (= :submitted (:knowledge.batch/state (batches/batch @conn (:knowledge.batch/id batch)))))))))

(deftest partial-generation-receipts-and-residual-ownership-refuse-history
  (with-ready
    (fn [{:keys [conn batch proof w request] :as data}]
      (let [selected (first (get-in proof [:selection :items]))
            expected (recovery/expected proof nil)
            value (recovery/decision {:batch (:batch request) :room (:room w) :by (:owner w)
                                      :client (:request-id request) :reason (:reason request)
                                      :at (java.util.Date. 0) :mode :requeue})]
        (requeue! data)
        (is (= [] (recovery/requeue-ready-tx @conn expected value)))
        (doseq [tx [[[:db/retract (:db/id batch) :knowledge.batch/recovery-generation-before 0]]
                    [[:db/add (:db/id batch) :knowledge.batch/recovery-generation-before 1]]
                    [[:db/retract (:db/id batch) :knowledge.batch/recovery-generation-after 1]]
                    [[:db/add (:db/id batch) :knowledge.batch/recovery-generation-after 2]]
                    [[:db/add [:knowledge.stream/id (:knowledge.batch/stream batch)] :knowledge.stream/batch-generation 0]]
                    [[:db/add [:knowledge.item/id (:id selected)] :knowledge.item/batch (:knowledge.batch/id batch)]]]]
          (let [projected (:db-after (d/with @conn tx)) before (d/commit-id @conn)]
            (is (thrown? Exception (recovery/requeue-ready-tx projected expected value)))
            (is (= before (d/commit-id @conn)))))))))

(deftest generation-overflow-holds-before-mutation
  (@#'batch-fixture/with-fixture
   (fn [{:keys [conn host w batch-input] :as data}]
     (d/transact conn (concat recovery/schema requests/schema))
     (let [principal (principals/party-principal (:owner w)) handle (str (:room w))
           refreshed (discovery/refresh! host principal handle batch-input) sid (:stream refreshed)]
       (d/transact conn [[:db/add [:knowledge.stream/id sid] :knowledge.stream/batch-generation Long/MAX_VALUE]])
       (let [ready (discovery/prepare-batch! host principal handle batch-input)
             b (batches/batch @conn (:batch ready)) before @conn
             request {:batch (:batch ready) :expected-claim (:knowledge.batch/claim b) :expected-frozen (:knowledge.batch/frozen-id b)
                      :request-id (random-uuid) :reason "Must refuse overflow"}]
         (is (= :generation-exhausted
                (try (discovery/requeue-ready! host principal handle request) nil
                     (catch clojure.lang.ExceptionInfo error (:reason (ex-data error))))))
         (is (= (d/commit-id before) (d/commit-id @conn))))))))

(deftest scheduled-requeue-withdraws-only-its-slot-and-does-not-resume
  (@#'scheduled-fixture/with-schedule
   (fn [{:keys [conn host w value activation batch-input]}]
     (d/transact conn recovery/schema)
     (schedules/set-enabled! conn (:schedule.activation/id activation) false)
     (let [bid (get-in value [:batch :batch]) b (batches/batch @conn bid)
           principal (principals/party-principal (:owner w)) handle (str (:room w))
           request {:batch bid :expected-claim (:knowledge.batch/claim b) :expected-frozen (:knowledge.batch/frozen-id b)
                    :request-id (random-uuid) :reason "Restore scheduled source eligibility"}
           result (discovery/requeue-ready! host principal handle request)]
       (is (false? (:schedule.activation/enabled? (schedules/activation conn (:activation value)))))
       (is (= :skipped (:schedule.occurrence/state (schedules/occurrence conn (:occurrence value)))))
       (is (nil? (:schedule.activation/active-occurrence (schedules/activation conn (:activation value)))))
       (schedules/set-enabled! conn (:activation value) true)
       (schedules/claim-due! conn (schedules/activation conn (:activation value)) {:interval-ms 60000} (java.util.Date. 120000))
       (let [later (discovery/prepare-batch! host principal handle batch-input)
             a (schedules/activation conn (:activation value)) s (streams/stream @conn (:knowledge.batch/stream b))]
         (is (not= bid (:batch later)))
         (is (= result (discovery/requeue-ready! host principal handle request)))
         (is (= a (schedules/activation conn (:activation value))))
         (is (= s (streams/stream @conn (:knowledge.batch/stream b)))))))))
