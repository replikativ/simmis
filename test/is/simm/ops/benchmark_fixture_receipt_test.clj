(ns is.simm.ops.benchmark-fixture-receipt-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.system.db :as control]
            [dvergr.system.rooms :as native]
            [is.simm.authority.world :as world]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.benchmark-fixture :as fixture]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.dvergr :as ops]
            [org.replikativ.spindel.engine.context :as context]))

(defn- with-request [f]
  (world/with-world [conn w]
    (d/transact conn requests/schema)
    (d/transact conn [{:party/id (:owner w) :party/role :admin}])
    (let [request (:request (requests/claim! conn {:party (:owner w) :room (:room w)
                                                   :client-id (random-uuid) :kind :benchmark
                                                   :intent {:test :fixture-receipt}
                                                   :limits {:timeout-ms 1000}}))
          receipt {:room (Object.) :persistent-id (random-uuid)}]
      (f conn w (ops/party-principal (:owner w)) request receipt))))

(deftest changed-request-is-refused-before-native-receipt-inspection
  (with-request
    (fn [conn w principal request receipt]
      (with-redefs [fixture/validate-receipt! (fn [& _] (throw (AssertionError. "No native read before Request preflight")))]
        (doseq [changed [(assoc request :work.request/claim (random-uuid))
                         (assoc request :work.request/by (:member w))
                         (assoc request :work.request/room (:other-room w))]]
          (is (thrown? clojure.lang.ExceptionInfo
                       (benchmarks/record-fixture! conn nil principal (:room w) changed receipt))))
        (is (nil? (:work.request/fixture-room (requests/get-request conn (:work.request/id request)))))))))

(deftest lost-write-ack-requires-exact-positive-receipt-readback
  (with-request
    (fn [conn w principal request receipt]
      (let [transact! d/transact writes (atom 0)]
        (with-redefs [fixture/validate-receipt! (fn [_ _ actual] (is (identical? receipt actual)) actual)
                      d/transact (fn [c tx]
                                   (swap! writes inc)
                                   (transact! c tx)
                                   (throw (ex-info "Lost acknowledgement" {})))]
          (let [observed (benchmarks/record-fixture! conn nil principal (:room w) request receipt)]
            (is (= (:persistent-id receipt) (:work.request/fixture-room observed)))
            (is (= 1 @writes))
            (is (nil? (:work.request/job observed)))))))))

(deftest silent-write-noop-is-not-a-publication-acknowledgement
  (with-request
    (fn [conn w principal request receipt]
      (with-redefs [fixture/validate-receipt! (fn [_ _ actual] actual)
                    d/transact (fn [& _] nil)]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no positive acknowledgement"
                              (benchmarks/record-fixture! conn nil principal (:room w) request receipt)))
        (is (nil? (:work.request/fixture-room (requests/get-request conn (:work.request/id request)))))))))

(deftest revocation-during-native-readback-refuses-admission-after-committed-write
  (with-request
    (fn [conn w principal request receipt]
      (let [reads (atom 0)]
        (with-redefs [fixture/validate-receipt!
                      (fn [_ _ actual]
                        (when (= 2 (swap! reads inc))
                          (d/transact conn [[:db/retract [:party/id (:owner w)] :party/role :admin]]))
                        actual)]
          (is (thrown? clojure.lang.ExceptionInfo
                       (benchmarks/record-fixture! conn nil principal (:room w) request receipt)))
          (is (= (:persistent-id receipt)
                 (:work.request/fixture-room (requests/get-request conn (:work.request/id request))))
              "the pointer stays available to the original cleanup owner"))))))

(deftest writer-rechecks-authority-before-publishing
  (with-request
    (fn [conn w principal request receipt]
      (let [transact! d/transact]
        (with-redefs [fixture/validate-receipt! (fn [_ _ actual] actual)
                      d/transact (fn [c tx]
                                   (transact! conn [[:db/retract [:party/id (:owner w)] :party/role :admin]])
                                   (transact! c tx))]
          (is (thrown? clojure.lang.ExceptionInfo
                       (benchmarks/record-fixture! conn nil principal (:room w) request receipt)))
          (is (nil? (:work.request/fixture-room (requests/get-request conn (:work.request/id request))))))))))

(deftest interruption-after-commit-is-never-recovered-as-admission
  (doseq [wrapped? [false true]]
    (with-request
      (fn [conn w principal request receipt]
        (let [transact! d/transact
              interruption (InterruptedException. "Stop after commit")
              failure (if wrapped? (ex-info "Wrapped interruption" {} interruption) interruption)
              reads (atom 0)]
          (try
            (let [observed
                  (with-redefs [fixture/validate-receipt! (fn [_ _ actual] (swap! reads inc) actual)
                                d/transact (fn [c tx] (transact! c tx) (throw failure))]
                    (try (benchmarks/record-fixture! conn nil principal (:room w) request receipt)
                         nil (catch Exception error [error (.isInterrupted (Thread/currentThread))])))]
              (Thread/interrupted)
              (is (identical? failure (first observed)))
              (is (true? (second observed)))
              (is (= 1 @reads) "interruption never enters recovery readback"))
            (finally (Thread/interrupted))))))))

(deftest interrupted-native-readback-takes-precedence-over-lost-write-ack
  (with-request
    (fn [conn w principal request receipt]
      (let [transact! d/transact reads (atom 0)
            failure (ex-info "Interrupted native readback" {} (InterruptedException.))]
        (try
          (let [observed
                (with-redefs [fixture/validate-receipt!
                              (fn [_ _ actual] (when (= 2 (swap! reads inc)) (throw failure)) actual)
                              d/transact (fn [c tx] (transact! c tx) (throw (ex-info "Lost ACK" {})))]
                  (try (benchmarks/record-fixture! conn nil principal (:room w) request receipt)
                       nil (catch Exception error [error (.isInterrupted (Thread/currentThread))])))]
            (Thread/interrupted)
            (is (identical? failure (first observed)))
            (is (true? (second observed))))
          (finally (Thread/interrupted)))))))

(deftest boot-recovery-holds-missing-receipt-and-continues-valid-retirement
  (with-request
    (fn [conn w _ missing _]
      (let [valid (:request (requests/claim! conn {:party (:owner w) :room (:room w)
                                                   :client-id (random-uuid) :kind :benchmark
                                                   :intent {:test :archived-fixture}
                                                   :limits {:timeout-ms 1000}}))
            persistent-id (random-uuid) repository (random-uuid)
            root (context/create-execution-context)
            resolve! benchmarks/recorded-fixture! visited (atom [])]
        (try
          (d/transact conn [[:db.fn/call requests/record-fixture-tx valid persistent-id]])
          (d/transact conn [{:db/id (:db/id missing) :work.request/state :interrupted}
                            {:db/id (:db/id valid) :work.request/state :interrupted}
                            {:system/id repository :system/type :repo :system/repository-profile fixture/profile}
                            {:room/id persistent-id :room/slug (fixture/slug (:work.request/id valid))
                             :room/default-repository [:system/id repository] :room/archived-at (java.util.Date.)}])
          (let [rows (mapv #(requests/get-request conn (:work.request/id %)) [missing valid])
                before (d/commit-id @conn)]
            (with-redefs [requests/requests (constantly rows)
                          benchmarks/recorded-fixture! (fn [& args] (swap! visited conj (last args)) (apply resolve! args))
                          control/archive-room! (fn [& _] (throw (AssertionError. "No guessed or repeated archival")))]
              (is (nil? (benchmarks/recover! conn root))))
            (is (= (mapv :work.request/id rows) @visited))
            (is (= before (d/commit-id @conn)))
            (is (nil? (:work.request/fixture-room (requests/get-request conn (:work.request/id missing))))))
          (finally (context/close-context! root)))))))

(deftest boot-recovery-preserves-interruption-and-stops-the-pass
  (with-request
    (fn [conn _ _ request _]
      (let [failure (ex-info "Interrupted recovery" {} (InterruptedException.))]
        (try
          (let [observed
                (with-redefs [requests/requests (constantly [(assoc request :work.request/state :interrupted)])
                              benchmarks/retire-request! (fn [& _] (throw failure))]
                  (try (benchmarks/recover! conn nil) nil
                       (catch Exception error [error (.isInterrupted (Thread/currentThread))])))]
            (Thread/interrupted)
            (is (identical? failure (first observed)))
            (is (true? (second observed))))
          (finally (Thread/interrupted)))))))

(deftest archive-does-not-hide-a-retained-native-runtime-owner
  (with-request
    (fn [conn _ _ request receipt]
      (let [id (:work.request/id request)
            persistent-id (:persistent-id receipt)
            repository (random-uuid) root (context/create-execution-context)]
        (try
          (d/transact conn [[:db.fn/call requests/record-fixture-tx request persistent-id]])
          (d/transact conn [{:system/id repository :system/type :repo :system/repository-profile fixture/profile}
                            {:room/id persistent-id :room/slug (fixture/slug id)
                             :room/default-repository [:system/id repository] :room/archived-at (java.util.Date.)}])
          (let [before (d/commit-id @conn)]
            (doseq [observe [(constantly (Object.))
                             (fn [_] (throw (ex-info "Retirement held" {:type ::native/room-runtime-held})))
                             (fn [_] (throw (ex-info "Hydration held" {:type ::native/room-hydration-failed})))]]
              (with-redefs [native/room-ctx-for observe]
                (is (true? (benchmarks/retirement-pending? conn root id)))
                (is (thrown? clojure.lang.ExceptionInfo (benchmarks/recorded-fixture! conn root id)))))
            (with-redefs [native/room-ctx-for (constantly nil)]
              (is (= :retired (benchmarks/recorded-fixture! conn root id)))
              (is (false? (benchmarks/retirement-pending? conn root id))))
            (is (= before (d/commit-id @conn))))
          (finally (context/close-context! root)))))))
