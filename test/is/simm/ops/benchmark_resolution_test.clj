(ns is.simm.ops.benchmark-resolution-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.blob :as blob]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.experiment :as experiment]
            [dvergr.agent.experiment.runner :as runner]
            [dvergr.agent.workflow :as workflow]
            [dvergr.artifact :as artifact]
            [hasch.core :as hasch]
            [is.simm.authority.world :as world]
            [is.simm.model.benchmark-resolutions :as journal]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.benchmark-resolution :as resolution]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.benchmarks-test :as fixtures]
            [is.simm.ops.knowledge-batch-inputs :as batches]
            [is.simm.ops.knowledge-execution-origin :as origin]
            [is.simm.ops.owned-workflows :as owned-source]
            [is.simm.ops.workflows :as workflows]
            [is.simm.ops.workflows-test :as source-fixtures])
  (:import [java.nio.charset StandardCharsets]
           [java.util.concurrent CountDownLatch TimeUnit]))

(defn- source []
  (assoc source-fixtures/fixture
         "execution-origin.edn" (pr-str origin/policy)
         "input-batching.edn" (pr-str {:version 2 :epoch (random-uuid)
                                       :policy (assoc batches/default-policy :context-paths []
                                                      :max-context-files 0 :max-context-bytes 0)})))

(defn- owned-prepared [room files input prepared]
  (let [manifest (mapv (fn [[path text]]
                         (let [bytes (.getBytes ^String text StandardCharsets/UTF_8)]
                           {:path path :content (blob/blob-id bytes) :size (alength bytes) :mode 33188}))
                       (sort-by key files))
        stamp (random-uuid)
        revision (owned-source/source-id {:room-id room :target-stamp stamp} (:name input) manifest)
        target {:repository (random-uuid) :store (random-uuid) :backend :file :branch :db :ref "main"
                :registry-system (random-uuid) :runtime-system "owned-source-test"}]
    (-> prepared
        (assoc :submission-channel :host)
        (assoc-in [:basis :submission] (assoc input :expected-owned-source revision))
        (assoc-in [:basis :admission-channel] :host)
        (assoc-in [:basis :source-origin] {:revision revision :target-stamp stamp :target target
                                          :manifest manifest}))))

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :keep-history? false :schema-flexibility :write}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (d/transact conn requests/schema)
        (with-redefs [workflow/candidates fixtures/candidates
                      runner/runtime-identity (constantly {:libraries {}})]
          (let [files (source) input (fixtures/input files) room (random-uuid)
                prepared (owned-prepared room files input
                                        (benchmarks/prepare-checkpoint-fixture (fixtures/snapshot files) input))
                claimed (:request (requests/claim! conn {:party (random-uuid) :room room
                                                         :client-id (:request-id input) :kind :benchmark
                                                         :intent (:basis prepared)
                                                         :limits (update (:limits input) :settlement keyword)}))
                request (requests/record-job! conn (:work.request/id claimed)
                                              (:work.request/claim claimed) (random-uuid))]
            (f conn request prepared input files)))
        (finally (d/release conn) (d/delete-database cfg))))))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- replace-basis! [conn request f]
  (let [body (update (requests/payload conn request) :input f)]
    (artifact/publish-value!
     (artifact/datahike-store conn) body
     #(d/transact conn [{:db/id (:db/id request) :work.request/payload-ref %
                        :work.request/intent-id (hasch/uuid body)}]))
    (requests/get-request conn (:work.request/id request))))

(deftest reconstruction-uses-only-retained-source-and-publishes-nothing
  (fixture
   (fn [conn request prepared _ files]
     (with-redefs [workflows/snapshot (fn [& _] (throw (AssertionError. "No authoring read")))
                   artifact/publish-value! (fn [& _] (throw (AssertionError. "No publication before reconstruction")))]
       (let [result (resolution/reconstruct conn request)]
         (is (= files (get-in result [:source :files])))
         (is (= (:basis prepared) (get-in result [:prepared :basis])))
         (is (= :clean-selected-fixture-checkpoint-v1 (get-in result [:plan :substitution])))))
     (is (thrown-with-msg? clojure.lang.ExceptionInfo #"resolved admission"
                          (benchmarks/adapter nil nil nil prepared nil nil))))))

(deftest self-consistent-request-hash-cannot-substitute-team-policy-or-task
  (fixture
   (fn [conn request prepared _ _]
     (let [original (:basis prepared)
           changed-task
           (let [plan (:plan prepared) env (first (:environments plan))
                 env (environment/make-environment
                      {:id (:environment/id env) :version (:environment/version env)
                       :task "This task did not come from retained source"
                       :verifier {:id (get-in env [:environment/verifier :verifier/id])
                                  :version (get-in env [:environment/verifier :verifier/version])
                                  :basis (get-in env [:environment/verifier :verifier/basis])}
                       :limits (:environment/limits env)
                       :world (:environment/world env) :metadata (:environment/metadata env)})]
             (runner/experiment-def
              (assoc plan :id (keyword (str "request-" (get-in original [:submission :request-id])))
                     :environments [env] :repetitions 1
                     :metadata (get-in original [:experiment :experiment/metadata]))))]
       (is (= changed-task (experiment/validate-experiment changed-task)))
       (doseq [basis [(assoc original :team {})
                      (assoc original :policy {:version :forged})
                      (assoc original :experiment changed-task)]]
         (let [current (replace-basis! conn (requests/get-request conn (:work.request/id request))
                                       (constantly basis))]
           (is (= :basis-mismatch (reason #(resolution/reconstruct conn current))))))))))

(deftest original-job-claim-and-request-integrity-remain-fenced
  (fixture
   (fn [conn request _ _ _]
     (doseq [attr [:work.request/job :work.request/claim]]
       (d/transact conn [[:db/add (:db/id request) attr (random-uuid)]])
       (is (= :request-changed (reason #(resolution/reconstruct conn request))))
       (d/transact conn [[:db/add (:db/id request) attr (get request attr)]]))
     (let [changed (replace-basis! conn request #(assoc-in % [:submission :request-id] (random-uuid)))]
       (is (= :request-integrity (reason #(resolution/reconstruct conn changed))))))))

(deftest runtime-drift-holds-and-ordinary-executors-still-refuse-checkpoint-source
  (fixture
   (fn [conn request _ input files]
     (with-redefs [runner/runtime-identity (constantly {:libraries {:runtime "changed"}})]
       (is (= :basis-mismatch (reason #(resolution/reconstruct conn request)))))
     (is (= :checkpoint-execution-unsupported
            (reason #(benchmarks/prepare (fixtures/snapshot files) input))))
     (is (= :checkpoint-execution-unsupported
            (reason #(benchmarks/prepare-cli (fixtures/snapshot files)
                                            (assoc input :limits {:max-cli-runs 1 :max-model-steps 2
                                                                  :timeout-ms 2000 :settlement "discard"}))))))))

(deftest owned-provenance-must-correspond-before-compiler-reconstruction
  (fixture
   (fn [conn request prepared _ _]
     (let [basis (:basis prepared)]
       (doseq [change [#(dissoc % :source-origin)
                       #(assoc % :admission-channel :agent-asserted)
                       #(assoc % :unknown-field true)
                       #(assoc-in % [:source-origin :revision] (random-uuid))
                       #(assoc-in % [:source-origin :target-stamp] (random-uuid))
                       #(assoc-in % [:source-origin :target :authority] true)
                       #(assoc-in % [:submission :expected-owned-source] (random-uuid))]]
         (let [current (replace-basis! conn (requests/get-request conn (:work.request/id request))
                                       (constantly (change basis)))]
           (is (= :source-provenance (reason #(resolution/reconstruct conn current))))))
       (let [bad-manifest (replace-basis! conn (requests/get-request conn (:work.request/id request))
                                          (constantly (update-in basis [:source-origin :manifest 0 :size] inc)))]
         (is (= :source-manifest (reason #(resolution/reconstruct conn bad-manifest)))))))))

(deftest recorded-channel-is-preserved-without-granting-native-or-dispatch-authority
  (fixture
   (fn [conn request prepared _ _]
     (let [basis (:basis prepared)
           current (replace-basis! conn request #(assoc % :admission-channel :mcp))
           rebuilt (resolution/reconstruct conn current)]
       (is (= :mcp (get-in rebuilt [:prepared :submission-channel])))
       (is (= (assoc basis :admission-channel :mcp) (get-in rebuilt [:prepared :basis])))
       (is (not (contains? rebuilt :new?)))))))

(defn- authorized-fixture [f]
  (world/with-world [conn w]
    (d/transact conn (vec (concat requests/schema journal/schema)))
    (d/transact conn [[:db/add [:party/id (:owner w)] :party/role :admin]])
    (with-redefs [workflow/candidates fixtures/candidates
                  runner/runtime-identity (constantly {:libraries {}})]
      (let [files (source) input (fixtures/input files)
            prepared (owned-prepared (:room w) files input
                                    (benchmarks/prepare-checkpoint-fixture (fixtures/snapshot files) input))
            claimed (:request (requests/claim! conn {:party (:owner w) :room (:room w)
                                                     :client-id (:request-id input) :kind :benchmark
                                                     :intent (:basis prepared)
                                                     :limits (update (:limits input) :settlement keyword)}))
            request (requests/record-job! conn (:work.request/id claimed)
                                          (:work.request/claim claimed) (random-uuid))]
        (f conn w request {:principal/id (keyword "party" (str (:owner w)))})))))

(defn- revoke! [conn w]
  (d/transact conn [[:db/retract [:party/id (:owner w)] :party/role :admin]]))

(deftest publication-authorizes-before-any-retained-read
  (authorized-fixture
   (fn [conn w request _]
     (with-redefs [artifact/get-bounded-value (fn [& _] (throw (AssertionError. "Denied caller read source")))]
       (doseq [who [:member :non-member :admin :anon]]
         (is (thrown? clojure.lang.ExceptionInfo
                      (resolution/publish-preparation! conn {:principal/id (when-let [id (get w who)] (keyword "party" (str id)))} (:room w) request)))))
     (is (nil? (journal/resolution @conn (:work.request/id request)))))))

(deftest publication-requires-full-source-and-current-job-before-publishing
  (authorized-fixture
   (fn [conn w request principal]
     (with-redefs [artifact/publish-value! (fn [& _] (throw (AssertionError. "Premature publication")))]
       (doseq [attr [:work.request/job :work.request/claim]]
         (d/transact conn [[:db/add (:db/id request) attr (random-uuid)]])
         (is (= :request-changed
                (reason #(resolution/publish-preparation! conn principal (:room w) request))))
         (d/transact conn [[:db/add (:db/id request) attr (get request attr)]])))
     (let [changed (replace-basis! conn request #(assoc % :team {}))]
       (with-redefs [artifact/publish-value! (fn [& _] (throw (AssertionError. "Forged basis published")))]
         (is (= :basis-mismatch
                (reason #(resolution/publish-preparation! conn principal (:room w) changed)))))))))

(deftest fresh-family-and-progressed-replay-have-different-rights
  (authorized-fixture
   (fn [conn w request principal]
     (let [first (resolution/publish-preparation! conn principal (:room w) request)
           c (clojure.core/first (:cases first))]
       (is (true? (:new? first)))
       (is (= :planned (:benchmark.case/state c)))
       (is (= (get-in first [:plan :cases 0 :ordinal]) (:benchmark.case/ordinal c)))
       (artifact/publish-value!
        (artifact/datahike-store conn) {:selection :test-only}
        #(d/transact conn [{:db/id (:db/id c) :benchmark.case/state :pinning
                           :benchmark.case/selection % :benchmark.case/selection-id (random-uuid)}]))
       (let [second (resolution/publish-preparation! conn principal (:room w) request)]
         (is (false? (:new? second)))
         (is (= (get-in first [:resolution :benchmark.resolution/id])
                (get-in second [:resolution :benchmark.resolution/id])))
         (is (= :pinning (get-in second [:cases 0 :benchmark.case/state]))))))))

(deftest committed-lost-ack-is-read-once-without-republication
  (authorized-fixture
   (fn [conn w request principal]
     (let [publish artifact/publish-value! calls (atom 0)]
       (with-redefs [artifact/publish-value! (fn [& args]
                                             (swap! calls inc)
                                             (apply publish args)
                                             (throw (ex-info "Lost publication ACK" {})))]
         (is (true? (:new? (resolution/publish-preparation! conn principal (:room w) request)))))
       (is (= 1 @calls))
       (is (= 1 (count (journal/cases @conn (:benchmark.resolution/id
                                            (journal/resolution @conn (:work.request/id request)))))))))))

(deftest authority-is-rechecked-at-writer-and-after-ack
  (doseq [when-revoke [:before-writer :after-commit]]
    (authorized-fixture
     (fn [conn w request principal]
       (let [publish artifact/publish-value!]
         (with-redefs [artifact/publish-value!
                       (fn [store value callback]
                         (if (= :before-writer when-revoke)
                           (publish store value (fn [ref] (revoke! conn w) (callback ref)))
                           (let [result (publish store value callback)]
                             (revoke! conn w)
                             result)))]
           (is (thrown? clojure.lang.ExceptionInfo
                        (resolution/publish-preparation! conn principal (:room w) request))))
         (is (= (= :after-commit when-revoke)
                (some? (journal/resolution @conn (:work.request/id request))))))))))

(deftest corrupt-inventory-and-plan-are-never-successful-readback
  (doseq [corrupt [:missing :extra :wrong-id :wrong-plan :wrong-own-claim :wrong-root :wrong-case-claim]]
    (authorized-fixture
     (fn [conn w request principal]
       (let [publish artifact/publish-value!]
         (with-redefs [artifact/publish-value!
                       (fn [store value callback]
                         (let [result (publish store value callback)
                               row (journal/resolution @conn (:work.request/id request))
                               c (first (journal/cases @conn (:benchmark.resolution/id row)))]
                           (case corrupt
                             :missing (d/transact conn [[:db/retractEntity (:db/id c)]])
                             :extra (d/transact conn [(-> c (dissoc :db/id)
                                                        (assoc :benchmark.case/id (random-uuid)
                                                               :benchmark.case/root (random-uuid)
                                                               :benchmark.case/ordinal 7))])
                             :wrong-id (d/transact conn [[:db/add (:db/id c) :benchmark.case/id (random-uuid)]])
                             :wrong-plan (publish store {:version :forged}
                                                  #(d/transact conn [[:db/add (:db/id row) :benchmark.resolution/plan %]]))
                             :wrong-root (d/transact conn [[:db/add (:db/id c) :benchmark.case/root (random-uuid)]])
                             :wrong-case-claim (d/transact conn [[:db/add (:db/id c) :benchmark.case/claim (random-uuid)]])
                             :wrong-own-claim (d/transact conn [[:db/add (:db/id row) :benchmark.resolution/claim (random-uuid)]]))
                           result))]
           (is (some? (reason #(resolution/publish-preparation! conn principal (:room w) request)))
               (str corrupt))))))))

(deftest inventory-transition-during-artifact-read-holds
  (doseq [replay? [false true]]
    (authorized-fixture
     (fn [conn w request principal]
       (when replay? (resolution/publish-preparation! conn principal (:room w) request))
       (let [read artifact/get-bounded-value]
         (with-redefs [artifact/get-bounded-value
                       (fn [store ref limit]
                         (let [result (read store ref limit)]
                           (when (= (* 64 1024) limit)
                             (let [row (journal/resolution @conn (:work.request/id request))
                                   c (first (journal/cases @conn (:benchmark.resolution/id row)))]
                               (d/transact conn [[:db/add (:db/id c) :benchmark.case/claim (random-uuid)]])))
                           result))]
           (is (= :inventory-changed
                  (reason #(resolution/publish-preparation! conn principal (:room w) request))))))))))

(deftest failure-before-publication-is-not-retried
  (authorized-fixture
   (fn [conn w request principal]
     (let [failure (ex-info "No publication ACK" {}) calls (atom 0)]
       (with-redefs [artifact/publish-value! (fn [& _] (swap! calls inc) (throw failure))]
         (is (identical? failure
                         (try (resolution/publish-preparation! conn principal (:room w) request)
                              nil (catch Exception error error)))))
       (is (= 1 @calls))
       (is (nil? (journal/resolution @conn (:work.request/id request))))))))

(deftest interrupted-readback-preserves-interruption-over-lost-write-ack
  (authorized-fixture
   (fn [conn w request principal]
     (let [publish artifact/publish-value! read artifact/get-bounded-value
           failure (ex-info "Interrupted readback" {} (InterruptedException.))]
       (try
         (let [observed
               (with-redefs [artifact/publish-value! (fn [& args]
                                                      (apply publish args)
                                                      (throw (ex-info "Lost write ACK" {})))
                             artifact/get-bounded-value (fn [store ref limit]
                                                          (if (= (* 64 1024) limit)
                                                            (throw failure)
                                                            (read store ref limit)))]
                 (try (resolution/publish-preparation! conn principal (:room w) request)
                      nil (catch Exception error [error (.isInterrupted (Thread/currentThread))])))]
           (Thread/interrupted)
           (is (identical? failure (first observed)))
           (is (true? (second observed))))
         (finally (Thread/interrupted)))))))

(deftest interruption-is-not-converted-to-successful-recovery
  (doseq [wrapped? [false true]]
    (authorized-fixture
     (fn [conn w request principal]
       (let [publish artifact/publish-value!
             interrupted (InterruptedException. "Stop after commit")
             failure (if wrapped? (ex-info "Wrapped interruption" {} interrupted) interrupted)]
         (try
           (let [observed
                 (with-redefs [artifact/publish-value! (fn [& args] (apply publish args) (throw failure))]
                   (try (resolution/publish-preparation! conn principal (:room w) request)
                        nil (catch Exception error
                              ;; Capture before test reporting or nREPL output
                              ;; can perform interruptible IO on this thread.
                              [error (.isInterrupted (Thread/currentThread))])))]
             (Thread/interrupted)
             (is (identical? failure (first observed)))
             (is (true? (second observed))))
           (finally (Thread/interrupted))))))))

(deftest concurrent-publications-reserve-one-family
  (authorized-fixture
   (fn [conn w request principal]
     (let [publish artifact/publish-value! barrier (CountDownLatch. 2) calls (atom 0)]
       (with-redefs [artifact/publish-value!
                     (fn [& args]
                       (swap! calls inc)
                       (.countDown barrier)
                       (when-not (.await barrier 15 TimeUnit/SECONDS)
                         (throw (ex-info "Publication barrier timed out" {})))
                       (apply publish args))]
         (let [a (future (resolution/publish-preparation! conn principal (:room w) request))
               b (future (resolution/publish-preparation! conn principal (:room w) request))
               results [(deref a 30000 ::timeout) (deref b 30000 ::timeout)]]
           (is (= [false true] (sort (map :new? results))))
           (is (apply = (map #(get-in % [:resolution :benchmark.resolution/id]) results)))
           (is (= 2 @calls))
           (is (= 1 (count (journal/cases @conn (get-in (first results) [:resolution :benchmark.resolution/id])))))))))))
