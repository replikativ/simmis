(ns is.simm.ops.owned-knowledge-retirement-test
  "Real HOST/ReBAC/Room and native root IO. Registry->runtime provisioning is
   the shared memory fixture's explicit seam, not persistent App reconstruction."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.gc-roots :as roots]
            [dvergr.system.rooms :as native-rooms]
            [dvergr.runtime.preparation :as preparation]
            [is.simm.model.knowledge-pins :as pins]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.knowledge-stream-retirement-test :as fixture]
            [is.simm.ops.dvergr :as principals]
            [is.simm.ops.knowledge-batch-proof :as proof]
            [is.simm.ops.knowledge-checkpoint-roots :as checkpoints]
            [is.simm.ops.owned-knowledge-discovery :as discovery]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [konserve.core :as k])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- with-clean [f]
  (@#'fixture/with-clean
   (fn [{:keys [decision own-descriptor] :as data}]
     (with-redefs [native-rooms/registry-system-for
                   (fn [db runtime]
                     (is (= (:runtime-system own-descriptor) runtime))
                     (d/pull db '[*] [:system/id (:registry-system own-descriptor)]))]
       (f (assoc data :retirement-request
                      {:stream (:stream decision) :expected-cursor (:root decision)
                       :request-id (:client decision) :reason (:reason decision)}))))))

(defn- retire! [{:keys [host principal handle retirement-request]}]
  (discovery/retire-stream! host principal handle retirement-request))

(defn- state [{:keys [conn stream]}]
  (:knowledge.stream/state (streams/stream @conn (:knowledge.stream/id stream))))

(defn- present? [{:keys [owned decision]}]
  (contains? (roots/roots (:store @owned) {:sync? true}) (:root decision)))

(deftest original-root-release-retains-frozen-proof-and-replay-never-resolves-source
  (with-clean
    (fn [{:keys [conn w ready] :as data}]
      (is (present? data))
      (let [result (retire! data) before @conn]
        (is (= :retired (:status result) (state data)))
        (is (not (present? data)))
        (is (= (:captured ready) (:captured (:frozen (proof/retained! conn (:owner w) (:room w) (:batch-origin ready))))))
        (with-redefs [native-rooms/registry-system-for (fn [& _] (throw (AssertionError. "Retired replay resolved source")))
                      checkpoints/create! (fn [& _] (throw (AssertionError. "Retired replay recreated root")))]
          (is (= result (retire! data))))
        (is (= (d/commit-id before) (d/commit-id @conn)))))))

(deftest changed-default-and-selected-workspace-never-redirect-release
  (with-clean
    (fn [{:keys [conn w attached attached-descriptor] :as data}]
      (let [other @attached registry (roots/roots (:store other) {:sync? true})]
        (d/transact conn [{:room/id (:room w) :room/default-repository [:system/id (:registry-system attached-descriptor)]}])
        (is (= :retired (:status (retire! data))))
        (is (not (present? data)))
        (is (= other @attached))
        (is (= registry (roots/roots (:store @attached) {:sync? true})))))))

(deftest missing-merge-authority-precedes-retained-payload-and-root-io
  (with-clean
    (fn [{:keys [host w handle retirement-request conn] :as data}]
      (let [before @conn calls (atom 0)]
        (with-redefs [pins/payload (fn [& _] (swap! calls inc) (throw (AssertionError. "Unauthorized payload read")))
                      checkpoints/release-retiring! (fn [& _] (throw (AssertionError. "Unauthorized root release")))]
          (doseq [party [(:member w) (:non-member w) (:admin w) (:team-agent w)]]
            (is (thrown? Exception (discovery/retire-stream! host (principals/party-principal party) handle retirement-request)))))
        (is (zero? @calls))
        (is (= (d/commit-id before) (d/commit-id @conn)))
        (is (present? data))))))

(deftest authority-revocation-before-writer-prevents-fencing-and-release
  (with-clean
    (fn [{:keys [conn w] :as data}]
      (let [payload! pins/payload changed? (atom false)]
        (with-redefs [pins/payload
                      (fn [& args]
                        (let [body (apply payload! args)]
                          (when (compare-and-set! changed? false true)
                            (d/transact conn [{:room/id (:room w) :room/owner [:party/id (:member w)]}]))
                          body))]
          (is (thrown? Exception (retire! data))))
        (is @changed?)
        (is (= :active (state data)))
        (is (present? data))))))

(deftest lost-begin-ack-leaves-fenced-rooted-intent-before-any-release
  (with-clean
    (fn [{:keys [conn] :as data}]
      (let [transact! d/transact lost? (atom false) releases (atom 0)]
        (with-redefs [d/transact
                      (fn [c tx]
                        (let [report (transact! c tx)]
                          (when (and (identical? c conn) (= :retiring (state data)) (compare-and-set! lost? false true))
                            (throw (ex-info "Lost retirement begin ACK" {})))
                          report))
                      checkpoints/release-retiring! (fn [& _] (swap! releases inc) (throw (AssertionError. "Lost begin ACK cannot release")))]
          (is (thrown? Exception (retire! data))))
        (is @lost?)
        (is (zero? @releases))
        (is (= :retiring (state data)))
        (is (present? data))
        (is (= :retired (:status (retire! data))))))))

(deftest lost-native-release-ack-can-be-reconciled-by-a-new-owner-with-original-audit
  (with-clean
    (fn [{:keys [conn w decision] :as data}]
      (let [release! roots/release! lost? (atom false)]
        (with-redefs [roots/release!
                      (fn [& args]
                        (let [result (apply release! args)]
                          (when (and (= (:root decision) (second args)) (compare-and-set! lost? false true))
                            (throw (ex-info "Lost native release ACK" {})))
                          result))]
          (is (thrown? Exception (retire! data))))
        (is @lost?)
        (is (= :retiring (state data)))
        (is (not (present? data)))
        (d/transact conn [{:room/id (:room w) :room/owner [:party/id (:member w)]}])
        (is (thrown? Exception (retire! data)))
        (with-redefs [d/commit-as-db (fn [& _] (throw (AssertionError. "Absent-root cleanup loaded old checkpoint")))
                      roots/pin! (fn [& _] (throw (AssertionError. "Absent-root cleanup recreated retention")))]
          (is (= :retired (:status (retire! (assoc data :principal (principals/party-principal (:member w))))))))
        (is (= (:owner w) (:knowledge.pin/retirement-by (pins/intent @conn (:root decision)))))
        (is (= :released (:knowledge.pin/state (pins/intent @conn (:root decision)))))))))

(deftest lost-finish-ack-is-read-only-after-the-original-source-disappears
  (with-clean
    (fn [{:keys [conn] :as data}]
      (let [transact! d/transact lost? (atom false)]
        (with-redefs [d/transact
                      (fn [c tx]
                        (let [report (transact! c tx)]
                          (when (and (identical? c conn) (= :retired (state data)) (compare-and-set! lost? false true))
                            (throw (ex-info "Lost retirement finish ACK" {})))
                          report))]
          (is (thrown? Exception (retire! data))))
        (is @lost?)
        (is (= :retired (state data)))
        (is (not (present? data)))
        (let [before @conn]
          (with-redefs [native-rooms/registry-system-for (fn [& _] (throw (AssertionError. "Finished replay required missing source")))]
            (is (= :retired (:status (retire! data)))))
          (is (= (d/commit-id before) (d/commit-id @conn))))))))

(deftest foreign-root-owner-holds-the-fenced-stream-without-releasing
  (with-clean
    (fn [{:keys [owned decision] :as data}]
      (let [store (:store @owned) original (roots/roots store {:sync? true})]
        (k/update store roots/registry-key #(assoc-in % [(:root decision) :owner :claim] (random-uuid)) {:sync? true})
        (is (thrown? Exception (retire! data)))
        (is (= :retiring (state data)))
        (is (present? data))
        (k/assoc store roots/registry-key original {:sync? true})
        (is (= :retired (:status (retire! data))))))))

(deftest missing-original-registration-holds-and-never-uses-current-default
  (with-clean
    (fn [data]
      (with-redefs [native-rooms/registry-system-for (fn [& _] nil)]
        (is (thrown? Exception (retire! data))))
      (is (= :retiring (state data)))
      (is (present? data))
      (is (= :retired (:status (retire! data)))))))

(deftest fenced-stream-rejects-unchanged-head-refresh-before-checkpoint-lookup
  (with-clean
    (fn [{:keys [host principal handle batch-input] :as data}]
      ;; Leave a canonical intent between begin and release.
      (with-redefs [checkpoints/release-retiring! (fn [& _] (throw (ex-info "Pause after fence" {})))]
        (is (thrown? Exception (retire! data))))
      (is (= :retiring (state data)))
      (is (present? data))
      (with-redefs [checkpoints/verify! (fn [& _] (throw (AssertionError. "Closed stream verified old checkpoint")))
                    checkpoints/create! (fn [& _] (throw (AssertionError. "Closed stream created root")))]
        (is (thrown? Exception (discovery/refresh! host principal handle batch-input)))
        (is (thrown? Exception (discovery/prepare-batch! host principal handle batch-input))))
      (is (= :retired (:status (retire! data)))))))

(deftest blocked-verification-cannot-release-after-authority-or-app-fence
  (doseq [kind [:authority :application :room]]
    (with-clean
      (fn [{:keys [conn w app-owner room] :as data}]
        (let [verify! checkpoints/verify! release! roots/release!
              entered (promise) release (CountDownLatch. 1) worker (atom nil) effects (atom 0)]
          (try
            (with-redefs [checkpoints/verify!
                          (fn [& args]
                            (let [result (apply verify! args)]
                              (deliver entered true)
                              ;; Model native IO that ignores cancellation and
                              ;; still returns a valid result after physical exit.
                              (let [deadline (+ (System/nanoTime) 15000000000)]
                                (loop []
                                  (when-not (try (.await release 100 TimeUnit/MILLISECONDS)
                                                 (catch InterruptedException _ false))
                                    (when (> (System/nanoTime) deadline)
                                      (throw (ex-info "Verification fixture timed out" {})))
                                    (recur))))
                              result))
                          roots/release! (fn [& args] (swap! effects inc) (apply release! args))]
              (reset! worker (future (try (retire! data) (catch Exception _ :held))))
              (is (= true (deref entered 15000 :timeout)))
              (is (= :retiring (state data)))
              (is (present? data))
              (case kind
                :authority (d/transact conn [{:room/id (:room w) :room/owner [:party/id (:member w)]}])
                ;; This fixture's caller itself owns an outer App ticket.
                ;; An independent shutdown thread must initiate the fence.
                :application (is (nil? (deref (future (lifetime/fence! app-owner)) 15000 :timeout)))
                :room (let [leases (vals (get @(:meta room) preparation/leases-key))]
                        (is (= 1 (count leases)))
                        (doseq [lease leases] (preparation/cancel! lease))))
              (.countDown release)
              (is (= :held (deref @worker 15000 :timeout)))
              (is (zero? @effects))
              (is (present? data))
              (is (= :retiring (state data))))
            (when (= :authority kind)
              (is (= :retired (:status (retire! (assoc data :principal (principals/party-principal (:member w))))))))
            (when (= :room kind) (is (= :retired (:status (retire! data)))))
            (finally (.countDown release) (when @worker (deref @worker 15000 nil)))))))))
