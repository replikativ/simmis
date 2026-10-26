(ns is.simm.ops.owned-knowledge-preflight-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [dvergr.model.registry :as models]
            [dvergr.resource :as resource]
            [dvergr.runtime.preparation :as preparation]
            [geschichte.repo :as repo]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.dvergr :as principals]
            [is.simm.ops.knowledge-batch-proof-test :as proof-fixture]
            [is.simm.ops.owned-knowledge-batches-test :as batch-fixture]
            [is.simm.ops.owned-knowledge-admission-test :as admission-fixture]
            [is.simm.ops.owned-knowledge-discovery :as discovery]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflow-admission :as admission]
            [is.simm.ops.work :as work]
            [is.simm.ops.work-api :as api]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- with-case [grant f]
  (@#'batch-fixture/with-fixture
   (fn [{:keys [conn batch-input w room] :as data}]
     (d/transact conn requests/schema)
     (when (seq grant)
       (binding [ec/*execution-context* (:ctx room)] (resource/mint! room {:id (random-uuid) :resources grant})))
     (f (assoc data :principal (principals/party-principal (:owner w)) :handle (str (:room w))
                    :input {:request-id (random-uuid) :name "wiki" :expected-bundle (:expected-bundle batch-input)
                            :expected-owned-source (:expected-source batch-input) :model "codex-luna"
                            :limits proof-fixture/limits})))))

(defn- submit! [{:keys [host principal handle input]}] (admission/submit! host principal handle input))
(defn- failure [f] (try (f) nil (catch clojure.lang.ExceptionInfo error error)))

(defn- no-journal? [conn]
  (every? #(empty? (d/datoms @conn :avet %))
          [:knowledge.stream/id :knowledge.pin/id :knowledge.batch/id :work.request/id]))

(deftest both-allowance-dimensions-refuse-before-any-discovery-or-publication
  (doseq [grant [{} {resource/microdollars 9999M resource/model-dispatches 4M}
                    {resource/microdollars 10000M resource/model-dispatches 3M}]]
    (with-case grant
      (fn [{:keys [conn] :as data}]
        (let [before (d/commit-id @conn) calls (atom 0)
              forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Unfunded admission reached fresh work")))]
          (with-redefs [discovery/prepare-batch! forbidden artifact/publish-value! forbidden work/prepare forbidden]
            (is (= :is.simm.ops.owned-workflow-admission/allowance (:type (ex-data (failure #(submit! data)))))))
          (is (zero? @calls))
          (is (no-journal? conn))
          (is (= before (d/commit-id @conn))))))))

(deftest unknown-and-unaccounted-models-refuse-before-balance-or-discovery
  (doseq [definition [nil {:id "metadata-only-unsupported" :provider :claude-code}]]
    (with-case {}
      (fn [{:keys [conn] :as data}]
        (let [calls (atom 0) forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Ineligible model reached native work")))]
          (with-redefs [models/get-model (constantly definition)
                        resource/balance forbidden discovery/prepare-batch! forbidden
                        artifact/publish-value! forbidden work/prepare forbidden]
            (is (= :is.simm.ops.owned-workflow-admission/model (:type (ex-data (failure #(submit! data)))))))
          (is (zero? @calls))
          (is (no-journal? conn)))))))

(deftest aliases-and-original-room-balance-extents-preserve-canonical-retry-order
  (with-case (work/resources proof-fixture/limits)
    (fn [{:keys [room conn owned input] :as data}]
      (let [balance! resource/balance prepare-batch! discovery/prepare-batch! reads (atom 0)]
        (with-redefs [resource/balance
                      (fn
                       ([actual]
                        (is (identical? room actual))
                        (is (identical? (:ctx room) ec/*execution-context*))
                        (is (= 1 (count (get @(:meta room) preparation/leases-key))))
                        (swap! reads inc)
                        (balance! actual))
                       ([actual account] (balance! actual account)))
                      discovery/prepare-batch!
                      (fn [& args]
                        (is (empty? (get @(:meta room) preparation/leases-key)))
                        (apply prepare-batch! args))
                      work/prepare (fn [input]
                                     (is (= "codex-luna" (:model input)))
                                     (is (empty? (get @(:meta room) preparation/leases-key)))
                                     (@#'proof-fixture/model-free-preparation input))
                      work/submit-prepared! @#'admission-fixture/claim-only]
          (let [result (submit! data)]
            (is (= :admitting (:state result)))
            (is (= 2 @reads))
            (repo/write! owned "workflows/wiki/README.md" (.getBytes "Changed current source" "UTF-8"))
            (let [fresh-reads (atom 0) forbidden (fn [& _] (swap! fresh-reads inc) (throw (AssertionError. "Exact retry read current source")))]
              (with-redefs [work/model-eligibility (fn [& _] (swap! fresh-reads inc) {:status :unknown})
                            resource/balance (fn [& _] (swap! fresh-reads inc) {})
                            reads/resolve-host-read! forbidden discovery/prepare-batch! forbidden work/prepare forbidden]
                (is (= result (submit! data)))
                (is (= :is.simm.ops.owned-workflow-admission/conflict
                       (:type (ex-data (failure #(submit! (assoc-in data [:input :limits :max-dispatches] 3)))))))
                (is (zero? @fresh-reads))))))))))

(deftest alias-eligibility-matches-ordinary-native-candidate-policy
  (let [eligibility (work/model-eligibility "codex-luna")
        prepared (work/prepare {:request-id (random-uuid) :task "Metadata-only candidate; never execute"
                                :model "codex-luna" :limits proof-fixture/limits})]
    (is (= :eligible (:status eligibility)))
    (is (= (:policy eligibility) (get-in prepared [:basis :candidate :agent/model-policy])))))

(deftest allowance-loss-after-preflight-holds-the-frozen-batch-without-auto-recovery
  (with-case (work/resources proof-fixture/limits)
    (fn [{:keys [conn] :as data}]
      (let [balance! resource/balance reads (atom 0) prepares (atom 0)]
        (with-redefs [resource/balance (fn
                                        ([room] (if (= 1 (swap! reads inc)) (balance! room) {}))
                                        ([room account] (balance! room account)))
                      work/prepare (fn [& _] (swap! prepares inc) (throw (AssertionError. "Allowance lost before provider preparation")))]
          (is (= :is.simm.ops.owned-workflow-admission/allowance (:type (ex-data (failure #(submit! data)))))))
        (is (= 2 @reads))
        (is (zero? @prepares))
        (is (empty? (requests/requests conn)))
        (is (= [:ready] (mapv #(-> (batches/batch @conn (:v %)) :knowledge.batch/state)
                              (d/datoms @conn :avet :knowledge.batch/id))))))))

(deftest balance-read-retains-original-room-and-refuses-revocation-fencing-or-cancellation
  (doseq [kind [:authority :application :room]]
    (with-case (work/resources proof-fixture/limits)
      (fn [{:keys [conn w room app-owner] :as data}]
        (let [balance! resource/balance entered (promise) release (CountDownLatch. 1)
              worker (atom nil) discoveries (atom 0)]
          (try
            (with-redefs [resource/balance
                          (fn
                           ([actual]
                            (let [result (balance! actual) deadline (+ (System/nanoTime) 15000000000)]
                              (deliver entered true)
                              (loop []
                                (when-not (try (.await release 100 TimeUnit/MILLISECONDS)
                                               (catch InterruptedException _ false))
                                  (when (> (System/nanoTime) deadline) (throw (ex-info "Balance fixture timed out" {})))
                                  (recur)))
                              result))
                           ([actual account] (balance! actual account)))
                          discovery/prepare-batch!
                          (fn [& _] (swap! discoveries inc) (throw (AssertionError. "Invalid original Room reached discovery")))]
              (reset! worker (future (try (submit! data) :unexpected (catch Exception _ :held))))
              (is (= true (deref entered 15000 :timeout)))
              (is (= 1 (count (get @(:meta room) preparation/leases-key))))
              (case kind
                :authority (d/transact conn [{:room/id (:room w) :room/owner [:party/id (:member w)]}])
                :application (is (nil? (deref (future (lifetime/fence! app-owner)) 15000 :timeout)))
                :room (doseq [lease (vals (get @(:meta room) preparation/leases-key))] (preparation/cancel! lease)))
              (.countDown release)
              (is (= :held (deref @worker 15000 :timeout)))
              (is (zero? @discoveries))
              (is (no-journal? conn)))
            (finally (.countDown release) (when @worker (deref @worker 15000 nil)))))))))

(deftest public-preflight-errors-are-actionable-and-do-not-export-native-data
  (doseq [[type status message]
          [[:is.simm.ops.owned-workflow-admission/allowance 409 "This room needs more execution allowance before running this workflow"]
           [:is.simm.ops.owned-workflow-admission/model 400 "Select an available model with native dispatch accounting"]]]
    (let [error (failure #(api/owned-service-result! (fn [] (throw (ex-info "Private native detail" {:type type :secret "not public"})))))]
      (is (= message (ex-message error)))
      (is (= status (:dvergr.ops/status (ex-data error))))
      (is (= #{:type :dvergr.ops/status} (set (keys (ex-data error))))))))

(deftest forged-native-error-tags-cannot-manufacture-known-preflight-refusals
  (doseq [kind [:registry :balance]]
    (with-case (work/resources proof-fixture/limits)
      (fn [{:keys [conn] :as data}]
        (let [lookup! models/get-model balance! resource/balance calls (atom 0)
              forged (fn [] (throw (ex-info "Private injected native diagnostic"
                                     {:type :is.simm.ops.owned-workflow-admission/allowance :secret "not public"})))
              forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Broken preflight reached discovery")))]
          (with-redefs [models/get-model (fn [id] (if (= :registry kind) (forged) (lookup! id)))
                        resource/balance (fn [& args] (if (= :balance kind) (forged) (apply balance! args)))
                        discovery/prepare-batch! forbidden artifact/publish-value! forbidden]
            (let [error (failure #(api/owned-service-result! (fn [] (submit! data))))]
              (is (= 503 (:dvergr.ops/status (ex-data error))))
              (is (= "Owned workflow operation unavailable" (ex-message error)))
              (is (= #{:type :dvergr.ops/status} (set (keys (ex-data error)))))))
          (is (zero? @calls))
          (is (no-journal? conn)))))))
