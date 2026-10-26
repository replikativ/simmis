(ns is.simm.ops.benchmark-resolution-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.experiment :as experiment]
            [dvergr.agent.experiment.runner :as runner]
            [dvergr.agent.workflow :as workflow]
            [dvergr.artifact :as artifact]
            [hasch.core :as hasch]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.benchmark-resolution :as resolution]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.benchmarks-test :as fixtures]
            [is.simm.ops.knowledge-batch-inputs :as batches]
            [is.simm.ops.knowledge-execution-origin :as origin]
            [is.simm.ops.workflows :as workflows]
            [is.simm.ops.workflows-test :as source-fixtures]))

(defn- source []
  (assoc source-fixtures/fixture
         "execution-origin.edn" (pr-str origin/policy)
         "input-batching.edn" (pr-str {:version 2 :epoch (random-uuid)
                                       :policy (assoc batches/default-policy :context-paths []
                                                      :max-context-files 0 :max-context-bytes 0)})))

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :keep-history? false :schema-flexibility :write}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (d/transact conn requests/schema)
        (with-redefs [workflow/candidates fixtures/candidates
                      runner/runtime-identity (constantly {:libraries {}})]
          (let [files (source) input (fixtures/input files)
                prepared (benchmarks/prepare-checkpoint-fixture (fixtures/snapshot files) input)
                claimed (:request (requests/claim! conn {:party (random-uuid) :room (random-uuid)
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
                          (benchmarks/adapter nil nil prepared nil))))))

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
