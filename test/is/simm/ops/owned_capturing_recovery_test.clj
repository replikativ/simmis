(ns is.simm.ops.owned-capturing-recovery-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [geschichte.content :as content]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-batch-recovery :as recovery]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.dvergr :as principals]
            [is.simm.ops.knowledge-batch-inputs :as inputs]
            [is.simm.ops.owned-knowledge-batches-test :as fixture]
            [is.simm.ops.owned-knowledge-discovery :as discovery])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- batch-row [conn]
  (some->> (first (d/datoms @conn :avet :knowledge.batch/id)) :e (d/pull @conn '[*])))

(defn- context [{:keys [conn w] :as data}]
  (let [b (batch-row conn)]
    (assoc data :batch b :principal (principals/party-principal (:owner w)) :handle (str (:room w))
                :request {:batch (:knowledge.batch/id b) :expected-claim (:knowledge.batch/claim b)
                          :request-id (random-uuid) :reason "Decline an interrupted source capture"})))

(defn- abandon! [{:keys [host principal handle request]}]
  (discovery/abandon-capturing! host principal handle request))

(defn- with-capturing [f]
  (@#'fixture/with-fixture
   (fn [{:keys [conn] :as data}]
     (d/transact conn (concat recovery/schema requests/schema))
     (with-redefs [inputs/capture (fn [& _] (throw (ex-info "Capture failed before frozen publication" {})))]
       (is (thrown? Exception (@#'fixture/prepare data))))
     (f (context data)))))

(deftest failed-capture-can-be-skipped-without-source-reads-and-replayed-after-later-work
  (with-capturing
    (fn [{:keys [conn batch host principal handle batch-input] :as data}]
      (let [before (streams/stream @conn (:knowledge.batch/stream batch))
            result (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Recovery read source bytes")))]
                     (abandon! data))
            closed (batches/batch @conn (:knowledge.batch/id batch))]
        (is (= :abandoned (:status result)))
        (is (= :capturing (:knowledge.batch/recovery-from closed)))
        (is (not (contains? closed :knowledge.batch/frozen)))
        (is (= (dissoc before :knowledge.stream/active-batch)
               (streams/stream @conn (:knowledge.batch/stream batch))))
        (is (thrown? Exception (batches/freeze-tx @conn batch (random-uuid) (random-uuid))))
        (let [later (discovery/prepare-batch! host principal handle batch-input)
              snapshot (streams/stream @conn (:knowledge.batch/stream batch))]
          (is (= :ready (:status later)))
          (is (not= (:knowledge.batch/id batch) (:batch later)))
          (is (= result (abandon! data)))
          (is (= snapshot (streams/stream @conn (:knowledge.batch/stream batch))))
          (is (= closed (batches/batch @conn (:knowledge.batch/id batch)))))))))

(deftest partial-frozen-state-and-wrong-claim-are-held
  (doseq [field [:knowledge.batch/frozen :knowledge.batch/frozen-id :knowledge.batch/scheduled-occurrence]]
    (with-capturing
      (fn [{:keys [conn batch] :as data}]
        (d/transact conn [{:db/id (:db/id batch) field (random-uuid)}])
        (is (thrown? Exception (abandon! data)))
        (is (= :capturing (:knowledge.batch/state (batch-row conn)))))))
  (with-capturing
    (fn [{:keys [conn] :as data}]
      (is (thrown? Exception (abandon! (assoc-in data [:request :expected-claim] (random-uuid)))))
      (is (= :capturing (:knowledge.batch/state (batch-row conn)))))))

(deftest lost-recovery-ack-requires-explicit-identical-retry
  (with-capturing
    (fn [{:keys [conn] :as data}]
      (let [transact! d/transact lost? (atom false)]
        (with-redefs [d/transact
                      (fn [c tx]
                        (let [result (transact! c tx)]
                          (when (and (identical? c conn) (= :closed (:knowledge.batch/state (batch-row conn)))
                                     (compare-and-set! lost? false true))
                            (throw (ex-info "Lost recovery ACK" {})))
                          result))]
          (is (thrown? Exception (abandon! data))))
        (is @lost?)
        (is (= :abandoned (:status (abandon! data))))
        (is (thrown? Exception (abandon! (assoc-in data [:request :reason] "Changed decision"))))))))

(deftest lost-claim-ack-never-grants-body-capture-and-can-be-explicitly-skipped
  (@#'fixture/with-fixture
   (fn [{:keys [conn] :as data}]
     (d/transact conn (concat recovery/schema requests/schema))
     (let [transact! d/transact lost? (atom false) calls (atom 0)]
       (with-redefs [inputs/capture (fn [& _] (swap! calls inc) (throw (AssertionError. "Lost claim ACK must prevent capture")))
                     d/transact
                     (fn [c tx]
                       (let [result (transact! c tx)]
                         (when (and (identical? c conn) (= :capturing (:knowledge.batch/state (batch-row conn)))
                                    (compare-and-set! lost? false true))
                           (throw (ex-info "Lost claim ACK" {})))
                         result))]
         (is (thrown? Exception (@#'fixture/prepare data))))
       (is @lost?)
       (is (zero? @calls))
       (is (= :capturing (:knowledge.batch/state (batch-row conn))))
       (is (= :abandoned (:status (abandon! (context data)))))))))

(deftest lost-freeze-ack-is-ready-and-cannot-be-treated-as-an-interrupted-capture
  (@#'fixture/with-fixture
   (fn [{:keys [conn host w] :as data}]
     (d/transact conn (concat recovery/schema requests/schema))
     (let [transact! d/transact lost? (atom false)]
       (with-redefs [d/transact
                     (fn [c tx]
                       (let [result (transact! c tx)]
                         (when (and (identical? c conn) (= :ready (:knowledge.batch/state (batch-row conn)))
                                    (compare-and-set! lost? false true))
                           (throw (ex-info "Lost freeze ACK" {})))
                         result))]
         (is (thrown? Exception (@#'fixture/prepare data))))
       (is @lost?)
       (let [{:keys [request principal handle]} (context data)
             b (batch-row conn)]
         (is (= :ready (:knowledge.batch/state b)))
         (is (thrown? Exception (discovery/abandon-capturing! host principal handle request)))
         (is (= :abandoned (:status (discovery/abandon-ready! host principal handle
                                      (assoc request :expected-frozen (:knowledge.batch/frozen-id b)))))))))))

(deftest frozen-artifact-publication-retains-exclusion-before-control-freeze
  (@#'fixture/with-fixture
   (fn [{:keys [conn] :as data}]
     (d/transact conn (concat recovery/schema requests/schema))
     (let [publish! artifact/publish-value! entered (promise) exited (promise)
           release (CountDownLatch. 1) worker (atom nil)]
       (try
         (with-redefs [artifact/publish-value!
                       (fn [& args]
                         (if (and (map? (second args)) (contains? (second args) :captured))
                           (apply publish!
                                  (assoc (vec args) 2
                                         (fn [ref]
                                           ;; Frozen bytes exist, but their canonical
                                           ;; batch pointer has not been committed.
                                           (deliver entered ref)
                                           (when-not (.await release 15 TimeUnit/SECONDS)
                                             (throw (ex-info "Publication fixture timed out" {})))
                                           (throw (ex-info "Publication failed before freeze callback" {})))))
                           (apply publish! args)))]
           (reset! worker (future (try (@#'fixture/prepare data)
                                      (catch Exception _ :failed)
                                      (finally (deliver exited true)))))
           (is (uuid? (deref entered 15000 nil)))
           (let [captured (context data)]
             (is (thrown? Exception (abandon! captured)))
             (is (= :capturing (:knowledge.batch/state (batch-row conn))))
             (is (nil? (:knowledge.batch/frozen (batch-row conn))))
             (.countDown release)
             (is (= true (deref exited 15000 :timeout)))
             (is (= :abandoned (:status (abandon! captured))))))
         (finally (.countDown release) (when @worker (deref exited 15000 nil))))))))

(deftest cancelled-capture-retains-physical-exclusion-until-body-read-exits
  (@#'fixture/with-fixture
   (fn [{:keys [conn] :as data}]
     (d/transact conn (concat recovery/schema requests/schema))
     (let [capture inputs/capture read! content/read-by-id
           capturing? (atom false) blocked? (atom false) entered (promise) exited (promise)
           release (CountDownLatch. 1) worker (atom nil)]
       (try
         (with-redefs [inputs/capture (fn [& args] (reset! capturing? true) (apply capture args))
                       content/read-by-id
                       (fn [& args]
                         (if (and @capturing? (compare-and-set! blocked? false true))
                           (do
                             (deliver entered true)
                             ;; Deliberately model IO that outlives future cancellation.
                             (loop []
                               (when-not (try (.await release 15 TimeUnit/SECONDS)
                                              (catch InterruptedException _ false))
                                 (if (pos? (.getCount release)) (recur))))
                             (throw (ex-info "Blocked source read failed after physical release" {})))
                           (apply read! args)))]
           (reset! worker (future (try (@#'fixture/prepare data)
                                      (catch Exception _ :failed)
                                      (finally (deliver exited true)))))
           (is (= true (deref entered 15000 :timeout)))
           (let [captured (context data)]
             (future-cancel @worker)
             (is (not (realized? exited)))
             (is (thrown? Exception (abandon! captured)))
             (is (= :capturing (:knowledge.batch/state (batch-row conn))))
             (.countDown release)
             (is (= true (deref exited 15000 :timeout)))
             (is (= :abandoned (:status (abandon! captured))))))
         (finally (.countDown release) (when @worker (deref exited 15000 nil))))))))
