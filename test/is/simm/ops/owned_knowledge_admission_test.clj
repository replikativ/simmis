(ns is.simm.ops.owned-knowledge-admission-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.jobs :as jobs]
            [dvergr.resource :as resource]
            [dvergr.runtime.preparation :as preparation]
            [geschichte.repo :as repo]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.knowledge-batch-proof :as proof]
            [is.simm.ops.knowledge-batch-proof-test :as proof-fixture]
            [is.simm.ops.owned-knowledge-batches-test :as fixture]
            [is.simm.ops.owned-knowledge-discovery :as discovery]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflow-admission :as admission]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflows :as workflows]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.core :as sp]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- fixture [f]
  (@#'fixture/with-fixture
   (fn [{:keys [conn batch-input w room] :as data}]
     (d/transact conn requests/schema)
     (binding [ec/*execution-context* (:ctx room)]
       (resource/mint! room {:id (random-uuid) :resources (work/resources proof-fixture/limits)}))
     (f (assoc data :input {:request-id (random-uuid) :name "wiki"
                           :expected-bundle (:expected-bundle batch-input)
                           :expected-owned-source (:expected-source batch-input)
                           :model "codex-subscription-luna" :limits proof-fixture/limits}
                    :principal (ops/party-principal (:owner w)) :handle (str (:room w)))))))

(defn- claim-only [conn ctx principal room-id room input prepared]
  (@#'work/claim-batch-prepared! conn ctx principal room-id room input prepared)
  (work/status conn ctx principal room-id
               (requests/request-id (work/authorize-in! @conn principal room-id) room-id (:request-id input))))

(deftest opt-in-owned-admission-links-frozen-inputs-to-the-canonical-request
  (fixture
   (fn [{:keys [conn host room w principal handle input owned]}]
     (let [preparations (atom 0)]
       (with-redefs [work/prepare (fn [input]
                                   (is (empty? (get @(:meta room) preparation/leases-key)))
                                   (swap! preparations inc)
                                   (@#'proof-fixture/model-free-preparation input))
                     work/submit-prepared! claim-only]
         (let [result (admission/submit! host principal handle input)
               row (requests/get-request conn (:id result))
               retained (requests/payload conn row)]
           (is (= :admitting (:state result)))
           (is (uuid? (:work.request/knowledge-batch row)))
           (is (= input (get-in retained [:input :submission])))
           (is (nil? (get-in retained [:input :knowledge/owner])))
           (is (= (:work.request/knowledge-batch row) (get-in (proof/request! conn row) [:origin :batch])))
           (repo/write! owned "workflows/wiki/README.md" (.getBytes "Later program" "UTF-8"))
           (with-redefs [reads/resolve-host-read! (fn [& _] (throw (AssertionError. "Retry may not resolve source")))
                         workflows/snapshot-in (fn [& _] (throw (AssertionError. "Retry may not open source")))
                         work/prepare (fn [& _] (throw (AssertionError. "Retry may not prepare provider")))]
             (is (= result (admission/submit! host principal handle input))))
           (is (= 1 @preparations))))))))

(deftest overrides-are-rejected-before-discovery-or-provider-preparation
  (doseq [override [{:case "not-supported"} {:params {"x" "y"}} {:inputs {"/in/x" "not-supported"}}]]
    (fixture
     (fn [{:keys [conn host principal handle input]}]
       (with-redefs [discovery/prepare-batch! (fn [& _] (throw (AssertionError. "Overrides may not start discovery")))]
         (is (thrown? Exception (admission/submit! host principal handle (merge input override)))))
       (is (empty? (d/q '[:find ?e :where [?e :knowledge.batch/id _]] @conn)))))))

(deftest batch-admission-requires-the-original-app-owner
  (fixture
   (fn [{:keys [conn host principal handle input]}]
     (with-redefs [discovery/prepare-batch! (fn [& _] (throw (AssertionError. "Unowned operation may not start discovery")))]
       (is (thrown? Exception (admission/submit! (update-in host [:config] dissoc :host-lifecycle)
                                                principal handle input))))
     (is (empty? (d/q '[:find ?e :where [?e :knowledge.batch/id _]] @conn))))))

(deftest provider-preparation-failure-leaves-an-explicit-held-batch
  (fixture
   (fn [{:keys [conn host principal handle input]}]
     (let [calls (atom 0)]
       (with-redefs [work/prepare (fn [& _] (swap! calls inc) (throw (ex-info "Preparation failed" {})))]
         (is (thrown? Exception (admission/submit! host principal handle input)))
         (is (= {:status :held :reason :active-batch}
                (select-keys (admission/submit! host principal handle input) [:status :reason])))
         (is (= 1 @calls))
         (is (empty? (d/q '[:find ?e :where [?e :work.request/id _]] @conn)))
         (is (= [:ready] (mapv #(-> (batches/batch @conn (first %)) :knowledge.batch/state)
                               (d/q '[:find ?id :where [_ :knowledge.batch/id ?id]] @conn)))))))))

(deftest owned-batch-can-use-the-existing-native-job-lifecycle
  (fixture
   (fn [{:keys [conn host room principal handle input]}]
     (let [grant (work/resources (:limits input)) started (atom 0)]
       (with-redefs [work/prepare (fn [input]
                                   (assoc (@#'proof-fixture/model-free-preparation input)
                                          :start! (fn [_] (swap! started inc) (sp/spin :done))))]
         (let [result (admission/submit! host principal handle input)
               status (jobs/status (:job result) 15000)]
           (is (= :completed (:status status)))
           (is (:settled? status))
           (is (= 1 @started))
           (is (= grant (binding [ec/*execution-context* (:ctx room)] (resource/balance room))))))))))

(deftest source-read-retains-its-original-room-until-physical-exit
  (fixture
   (fn [{:keys [host room principal handle input conn]}]
     (let [snapshot workflows/snapshot-in entered (promise) release (CountDownLatch. 1)
           worker (atom nil)]
       (try
         (with-redefs [workflows/snapshot-in
                       (fn [& args]
                         (deliver entered true)
                         ;; Teardown cancellation must join physical exit, not
                         ;; infer release from an interruptible logical result.
                         (loop []
                           (when-not (try (.await release 100 TimeUnit/MILLISECONDS)
                                          (catch InterruptedException _ false))
                             (recur)))
                         (apply snapshot args))
                       work/prepare (fn [& _] (throw (AssertionError. "Retired Room may not prepare a provider")))]
           (reset! worker (future (try (admission/submit! host principal handle input) :unexpected
                                      (catch Exception _ :held))))
           (is (= true (deref entered 15000 ::expired)))
           (let [leases (vec (vals (get @(:meta room) preparation/leases-key)))]
             (is (= 1 (count leases)))
             (doseq [lease leases]
               (preparation/cancel! lease)
               (is (= :dvergr.runtime.preparation/unconfirmed-drain
                      (try ((:join! lease) 0) nil
                           (catch clojure.lang.ExceptionInfo error (:type (ex-data error)))))))
             (.countDown release)
             (is (= :held (deref @worker 15000 ::expired)))
             (doseq [lease leases]
               (is (nil? ((:join! lease) 1000))))
             (is (empty? (get @(:meta room) preparation/leases-key)))
             (is (empty? (d/q '[:find ?e :where [?e :work.request/id _]] @conn)))))
         (finally (.countDown release) (when @worker (deref @worker 15000 nil))))))))

(deftest app-fenced-during-proof-read-never-prepares-a-provider
  (fixture
   (fn [{:keys [host principal handle input app-owner conn]}]
     (let [ready! proof/ready!]
       (with-redefs [proof/ready! (fn [& args]
                                  (let [result (apply ready! args)]
                                    (is (nil? (deref (future (lifetime/fence! app-owner)) 5000 ::expired)))
                                    result))
                     work/prepare (fn [& _] (throw (AssertionError. "Fenced App may not prepare provider")))]
         (is (thrown? Exception (admission/submit! host principal handle input))))
       (is (empty? (d/q '[:find ?e :where [?e :work.request/id _]] @conn)))))))

(deftest altered-discovery-replies-are-rejected-before-provider-preparation
  (doseq [alter [(fn [reply] (assoc-in reply [:captured :inputs "/in/forged"] "Forged"))
                 (fn [reply] (assoc-in reply [:source-origin :target-stamp] (random-uuid)))]]
    (fixture
     (fn [{:keys [host principal handle input]}]
       (let [prepare discovery/prepare-batch!]
         (with-redefs [discovery/prepare-batch! (fn [& args] (alter (apply prepare args)))
                       work/prepare (fn [& _] (throw (AssertionError. "Altered reply may not prepare provider")))]
           (is (thrown? Exception (admission/submit! host principal handle input)))))))))
