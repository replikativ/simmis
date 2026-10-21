(ns is.simm.ops.benchmarks-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.experiment.runner :as runner]
            [dvergr.agent.experiment :as experiment]
            [dvergr.agent.roster :as roster]
            [dvergr.agent.trajectory :as trajectory]
            [dvergr.agent.workflow :as workflow]
            [dvergr.agent.program :as program]
            [dvergr.catalog.room :as catalog]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.jobs :as jobs]
            [dvergr.model.api.codex-subscription :as codex]
            [dvergr.model.api.claude-code :as claude-code]
            [dvergr.model.chat :as model-chat]
            [dvergr.model.gateway :as gateway]
            [dvergr.model.providers :as providers]
            [dvergr.resource :as resource]
            [dvergr.agent.run :as run]
            [dvergr.room.store :as store]
            [dvergr.room.store.memory :as memory]
            [is.simm.authority.world :as world]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.checkers :as checkers]
            [is.simm.ops.cli-owner :as cli-owner]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.work :as work]
            [is.simm.ops.work-test]
            [is.simm.ops.workflows :as workflows]
            [is.simm.ops.workflows-test :as fixture]
            [jsonista.core :as json]
            [org.replikativ.spindel.engine.core :as ec]))

(use-fixtures :each
  (fn [f]
    ;; Tests may run inside a live dev server; own the physical capacity cell.
    (with-redefs-fn {#'benchmarks/checker-slot (java.util.concurrent.Semaphore. 1)
                     #'cli-owner/ownership! (constantly {:version 1 :unit "fixture.service" :boot "fixture" :invocation "fixture"})} f)))

(defn snapshot [files]
  {:files files :manifest (mapv (fn [[path text]] {:path path :size (count (.getBytes text "UTF-8"))}) files)})

(defn input [files]
  {:request-id (random-uuid) :name "echo" :expected-bundle (:id (catalog/bundle "echo" files))
   :models ["fixture-model"] :repetitions 1
   :limits {:budget-microdollars 10000 :max-dispatches 4 :max-model-steps 3
            :timeout-ms 2000 :settlement "discard"}})

(defn candidates [{:keys [models]}]
  (let [ids (mapv #(keyword (str "candidate-" %)) (range (count models)))]
    {:ids ids :team (reduce (fn [team [id model]]
                              (roster/make-agent team {:id id :program {:kind :echo}
                                                       :model-policy {:model model :provider :openai}}))
                            (roster/make-roster {:id :fixture/team}) (map vector ids models))}))

(defn prepared [files request]
  (with-redefs [workflow/candidates candidates runner/runtime-identity (constantly {:libraries {}})]
    (benchmarks/prepare (snapshot files) request)))

(deftest declared-dataset-splits-are-enforced-and-frozen-in-experiment-metadata
  (let [files (assoc fixture/fixture
                     "cases/a/gold.edn" "{:expected 1}"
                     "cases/b/gold.edn" "{:expected 2}"
                     "benchmark.edn" "{:version 1 :splits {:development [\"a\"] :held-out [\"b\"]}}")
        request (input files)]
    (doseq [bad [request (assoc request :split "development" :cases ["b"])
                 (assoc request :split "held-out" :cases ["a"])]]
      (with-redefs [workflow/candidates (fn [& _] (throw (AssertionError. "No provider preparation before split validation")))]
        (is (thrown? clojure.lang.ExceptionInfo (benchmarks/prepare (snapshot files) bad)))))
    (let [result (prepared files (assoc request :split "development"))
          data (get-in result [:plan :experiment :experiment/metadata :simmis/evaluation])]
      (is (= 1 (:cells result)))
      (is (= "development" (:split data)))
      (is (= ["a"] (:cases data)))
      (is (= {:adapter :native-provider :program :llm}
             (get-in result [:plan :experiment :experiment/metadata :simmis/execution])))
      (is (string? (:id data)))
      (is (= ["a"] (mapv #(get-in % [:environment/metadata :case])
                         (get-in result [:plan :environments])))))))

(deftest cli-text-benchmarks-have-a-distinct-single-use-admission-contract
  (let [files fixture/fixture
        request (assoc (input files) :models ["claude-code-haiku"]
                       :limits {:max-cli-runs 1 :max-model-steps 3 :timeout-ms 30000 :settlement "discard"})
        policy {:version 1 :mode :text :cli-version "fixture"}]
    (with-redefs [claude-code/execution-policy (constantly policy)
                  runner/runtime-identity (constantly {:claude-cli "fixture"})]
      (let [prepared (benchmarks/prepare-cli (snapshot files) request)
            candidate (first (roster/agents (get-in prepared [:plan :team])))]
        (is (= :cli-benchmark (:request-kind prepared)))
        (is (= {"cli-run-admission" 1M} (:resources prepared)))
        (is (= :text (get-in candidate [:agent/program :execution-mode])))
        (is (true? (get-in candidate [:agent/program :single-use?])))
        (is (false? (get-in candidate [:agent/program :auto-compact?])))
        (is (= policy (get-in candidate [:agent/program :provider-policy])))
        (is (= {"cli-run-admission" 1M}
               (get-in prepared [:plan :environments 0 :environment/world :resources]))))
      (doseq [bad [(assoc request :models ["codex-subscription-luna"])
                   (assoc-in request [:limits :max-dispatches] 1)
                   (assoc-in request [:limits :settlement] "review")]]
        (is (thrown? clojure.lang.ExceptionInfo (benchmarks/prepare-cli (snapshot files) bad))))
      (is (thrown? clojure.lang.ExceptionInfo (benchmarks/prepare (snapshot files) request))))))

(defn cli-fixture! [failure-mode]
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [control]
       (let [host-ctx ec/*execution-context*
             clean (discourse/make-room {:id (keyword (str "cli-fixture-" (random-uuid))) :store (memory/make)})
             files (assoc fixture/fixture
                          "workflow.edn" "{:title \"CLI fixture\" :task \"Write /out/result.md\" :capture [\"/out\"]}"
                          "checker.clj" "(ns checker) (defn check [{:keys [files gold]}] (let [ok (= (:expected gold) (get files \"/out/result.md\"))] {:checks {:correct? ok} :reward (if ok 1 0)}))"
                          "gold.edn" "{:expected \"interpreter output\" :private \"PRIVATE-CHECKER-GOLD\"}")
             request (assoc (input files) :models ["claude-code-haiku"]
                            :limits {:max-cli-runs 1 :max-model-steps 3 :timeout-ms 30000 :settlement "discard"})
             calls (atom []) retired (atom 0)
             provider (claude-code/create {})]
         (try
           (workspace/ensure-workspace! control)
           (binding [ec/*execution-context* (:ctx clean)] (workspace/ensure-workspace! clean))
           (workspace/seed! control {"/private-control.md" "PRIVATE-CONTROL-DATA"})
           (workflows/write! control (ops/party-principal (:owner w)) {:name "echo" :files files})
           (d/transact conn [{:party/id (:owner w) :party/role :admin}])
           (resource/install-unit! control {:symbol "cli-run-admission" :name "CLI Run admissions" :precision 0})
           (resource/mint! control {:id (random-uuid) :resources {"cli-run-admission" 1M}})
           (with-redefs-fn {#'providers/providers (atom {:claude-code provider})
                            #'claude-code/execution-policy (constantly {:version 1 :mode :text :cli-version "fixture"})
                            #'runner/runtime-identity (constantly {:claude-cli "fixture"})
                            #'benchmarks/fixture-room! (fn [& _] clean)
                            #'benchmarks/retire-fixture! (fn [& _] (swap! retired inc))
                            #'model-chat/chat
                            (fn [messages opts]
                              (swap! calls conj {:messages messages :opts opts})
                              (if (= 1 (count @calls))
                                {:content "" :stop-reason :tool-use :usage {:input-tokens 20 :output-tokens 5}
                                 :tool-calls [{:id "write" :name "clojure_eval"
                                               :input {:code "(do (babashka.fs/create-dirs \"/out\") (spit \"/out/result.md\" \"interpreter output\"))"}}]}
                               ;; A later missing usage report must invalidate
                               ;; completeness, not retain the first turn's sum.
                                (case failure-mode
                                  :provider-error (throw (ex-info "Fixture provider timed out" {:status 504}))
                                  :cancel (throw (java.util.concurrent.CancellationException. "Fixture cancellation"))
                                  :malformed-usage {:content "Finished" :stop-reason :end-turn
                                                    :usage {:input-tokens "invalid" :output-tokens 0}}
                                  {:content "Finished" :stop-reason :end-turn :usage nil})))}
             (fn []
               (let [principal (ops/party-principal (:owner w))
                     submission (benchmarks/submit-cli! conn host-ctx principal (:room w) control request)
                     status (jobs/status (:job submission) 60000)
                     retry (benchmarks/submit-cli! conn host-ctx principal (:room w) control request)
                     scorecard (first (store/-list-scorecards (:store control) (store/conversation-id control) {}))
                     attempts (store/-list-attempts (:store control) (store/conversation-id control) {})
                     attempt (first attempts)]
                 (is (= :completed (:status status)) (pr-str (select-keys status [:status :error])))
                 (is (:settled? status))
                 (is (= (:job submission) (:job retry)))
                 (is (= 2 (count @calls)))
                 (is (every? #(= :text (get-in % [:opts :execution-mode])) @calls))
                 (is (not (re-find #"PRIVATE-CHECKER-GOLD|PRIVATE-CONTROL-DATA" (pr-str @calls))))
                 (is (= 1 (count attempts)))
                 (if failure-mode
                   ;; Infrastructure/cancellation evidence is retained as an
                   ;; Attempt, never promoted into a model-quality board.
                   (is (nil? scorecard))
                   (do (is (= 1 (get-in scorecard [:scorecard/summary 0 :passed-count])))
                       (is (= :cli-run-admission (get-in scorecard [:scorecard/experiment :experiment/metadata :simmis/execution :accounting])))))
                 (let [spend (get-in attempt [:attempt/receipt :attempt/metrics :spend])]
                   (is (false? (:priced? spend)))
                   (is (nil? (:notional-microdollars spend)))
                   (is (not (contains? (:tokens spend) :input))))
                 (is (= 1 @retired)))))
           (finally (discourse/close-room! clean))))))))

(deftest cli-benchmark-reuses-clean-fixtures-certification-and-at-most-once-requests
  (doseq [failure-mode [nil :provider-error :cancel :malformed-usage]] (cli-fixture! failure-mode)))

(deftest cli-allowance-needs-both-operator-and-room-authority-before-host-access
  (world/with-world [conn w]
    (let [called (atom 0)]
      (with-redefs [cli-owner/ownership! (fn [] (swap! called inc))]
        (doseq [party [(:owner w) (:member w) (:non-member w) (:admin w)]]
          (is (thrown? clojure.lang.ExceptionInfo
                       (benchmarks/provision-cli! conn ec/*execution-context*
                                                  (ops/party-principal party) (:room w) nil
                                                  {:request-id (random-uuid) :max-cli-runs 1}))))
        (is (zero? @called))))))

(defn echo-prepared [files request]
  (let [base (prepared files request)
        [_ echo] (@#'is.simm.ops.work-test/fixture)
        env (first (get-in base [:plan :environments]))
        ref (:environment/verifier env)
        env (environment/make-environment
             {:id :fixture/calibration :task "Deterministic echo"
              :verifier {:id (:verifier/id ref) :version (:verifier/version ref) :basis (:verifier/basis ref)}
              :world (:environment/world env) :limits {:timeout-ms 60000 :cancel-timeout-ms 1000}})
        plan (assoc (:plan base) :team (:team echo) :environments [env])]
    (assoc base :plan (assoc plan :experiment (runner/experiment-def
                                               (assoc plan :id :fixture/calibration :repetitions 1))))))

(deftest benchmark-pins-existing-experiment-definitions-and-conserved-cell-limits
  (let [files (merge fixture/fixture {"cases/a/gold.edn" "{:expected 1}"
                                      "cases/b/gold.edn" "{:expected 2}"})
        request (-> (input files)
                    (assoc :models ["fixture-one" "fixture-two"] :repetitions 2)
                    (assoc-in [:limits :max-dispatches] 8))
        result (prepared files request)
        evaluator (get-in result [:plan :capabilities :evaluator])]
    (is (= {:name "echo" :title (get-in result [:bundle :definition :title])
            :bundle (str (:expected-bundle request))}
           (get-in result [:plan :experiment :experiment/metadata :simmis/workflow])))
    (is (= 8 (:cells result)))
    (is (= 1250 (get-in result [:cell-limits :budget-microdollars])))
    (is (= 1 (get-in result [:cell-limits :max-dispatches])))
    (is (= :ad-hoc (:tier evaluator)))
    (doseq [env (get-in result [:plan :environments])]
      (is (= work/effects (get-in env [:environment/world :effects])))
      (is (= (:verifier/basis (evaluation/evaluator-ref evaluator))
             (get-in env [:environment/verifier :verifier/basis])))
      (is (= env (environment/validate-environment env))))))

(deftest workflow-provenance-is-frozen-and-bounded-at-admission
  (let [files (assoc fixture/fixture "workflow.edn"
                     (pr-str {:title (apply str (repeat 1000 "x")) :task "Review input" :capture ["/out"]}))
        request (input files)
        result (prepared files request)]
    (is (= {:name "echo" :title (apply str (repeat 256 "x")) :bundle (str (:expected-bundle request))}
           (get-in result [:plan :experiment :experiment/metadata :simmis/workflow])))))

(deftest native-subscriptions-prepare-ordinary-room-benchmark-cells
  (let [files fixture/fixture
        request (assoc (input files) :models ["codex-subscription-sol"])
        prepared (benchmarks/prepare (snapshot files) request)]
    (is (= 1 (:cells prepared)))
    (is (= #{:codex-subscription}
           (set (map #(get-in % [:agent/model-policy :provider])
                     (roster/agents (get-in prepared [:plan :team]))))))
    (is (= 4 (get-in prepared [:cell-limits :max-dispatches])))
    (is (every? #(= :ctx (get-in % [:environment/world :isolation]))
                (get-in prepared [:plan :environments])))))

(deftest native-subscription-benchmark-cells-conserve-dispatches-and-certify-results
  (doseq [dispatches [4 2]]
    (world/with-world [conn w]
      (@#'is.simm.ops.work-test/with-room
       (fn [control]
         (let [host-ctx ec/*execution-context*
               clean (discourse/make-room {:id (keyword (str "native-fixture-" (random-uuid))) :store (memory/make)})
               files (-> fixture/fixture
                         (assoc "workflow.edn" "{:title \"Interpreter workflow\" :task \"Write interpreter output to /out/result.md\" :capture [\"/out\"]}"
                                "checker.clj" "(ns checker) (defn check [{:keys [files gold]}] (let [ok (= (:expected gold) (get files \"/out/result.md\"))] {:checks {:output-correct? ok} :reward (if ok 1 0)}))"
                                "cases/a/gold.edn" "{:expected \"interpreter output\" :private \"PRIVATE-CHECKER-GOLD\"}"
                                "cases/b/gold.edn" "{:expected \"interpreter output\" :private \"PRIVATE-CHECKER-GOLD\"}"))
               request (-> (input files) (assoc :models ["codex-subscription-sol"])
                           (assoc-in [:limits :max-dispatches] dispatches)
                           (assoc-in [:limits :timeout-ms] 30000))
               calls (atom []) retired (atom 0)
               provider (codex/create {:credentials (gateway/static-credentials :fixture {} #{"https://chatgpt.com"})})
               infer (fn [http]
                       (let [body (json/read-value (:body http) json/keyword-keys-object-mapper)
                             second-turn? (some #(= "function_call_output" (:type %)) (:input body))
                             events (concat
                                     (if second-turn?
                                       [{:type "response.output_text.delta" :delta "Result prepared."}]
                                       [{:type "response.output_item.done"
                                         :item {:type "function_call" :call_id "write-result" :name "clojure_eval"
                                                :arguments (json/write-value-as-string
                                                            {:code "(do (babashka.fs/create-dirs \"/out\") (spit \"/out/result.md\" \"interpreter output\"))"})}}])
                                     [{:type "response.completed" :response {:id (str "fixture-" (count @calls)) :model "gpt-5.6-sol"
                                                                             :usage {:input_tokens 100 :output_tokens 20}}}])]
                         (swap! calls conj body)
                         {:status 200 :body (java.io.ByteArrayInputStream.
                                             (.getBytes (apply str (map #(str "data: " (json/write-value-as-string %) "\n\n") events)) "UTF-8"))}))]
           (try
             (workspace/ensure-workspace! control)
             ;; Match real fixture-room provisioning: input prediction must
             ;; see this fixture's virtual workspace, not the shared physical
             ;; fallback which carries a different AGENTS.md guide.
             (binding [ec/*execution-context* (:ctx clean)] (workspace/ensure-workspace! clean))
             (workspace/seed! control {"/private-control.md" "PRIVATE-CONTROL-DATA"})
             (workflows/write! control (ops/party-principal (:owner w)) {:name "echo" :files files})
             (d/transact conn [{:party/id (:owner w) :party/role :admin}])
             (resource/mint! control {:id (random-uuid) :resources (work/resources (:limits request))})
             ;; Root stub covers every asynchronous room worker; no login or
             ;; actual HTTP implementation can be reached in this fixture.
             (with-redefs-fn {#'providers/providers (atom {:codex-subscription provider})
                              #'gateway/*request-fn* infer
                              #'benchmarks/fixture-room! (fn [& _] clean)
                              #'benchmarks/retire-fixture! (fn [& _] (swap! retired inc))}
               (fn []
                 (let [submission (benchmarks/submit! conn host-ctx (ops/party-principal (:owner w)) (:room w) control request)
                       status (jobs/status (:job submission) 60000)
                       attempts (store/-list-attempts (:store control) (store/conversation-id control) {})
                       scorecards (store/-list-scorecards (:store control) (store/conversation-id control) {})
                       scorecard (first scorecards)]
                   (is (= :completed (:status status)) (pr-str (select-keys status [:status :error])))
                   (is (:settled? status))
                   (is (= dispatches (count @calls)) "Each cell is bounded by its conserved share, even when a second model turn is attempted")
                   (is (= 2 (count attempts)))
                   (if (= dispatches 4)
                     (do
                       (is (= 1 (count scorecards)))
                       (is (= 2 (count (:scorecard/entries scorecard))))
                       (is (= 2 (get-in scorecard [:scorecard/summary 0 :passed-count])))
                       (is (every? experiment/verdict? attempts)))
                     (do
                       ;; A gateway quota refusal is an infrastructure fault,
                       ;; not evidence of model quality. Keep the certified
                       ;; Attempts, but do not manufacture a complete board.
                       (is (empty? scorecards))
                       (is (not-any? experiment/verdict? attempts))
                       (is (every? #(= :infrastructure (get-in % [:attempt/receipt :attempt/metrics :failure :kind])) attempts))))
                   (is (every? #(= (if (= dispatches 4) 1.0 0.0) (get-in % [:attempt/receipt :attempt/reward])) attempts))
                   (is (every? #(= {"/out/result.md" "interpreter output"} (get-in % [:attempt/evidence :files])) attempts)
                       "Captured outputs remain inspectable after both successful and interrupted worlds are discarded")
                   (is (every? #(zero? (get-in % [:attempt/receipt :attempt/metrics :spend :microdollars])) attempts))
                   (if (= dispatches 4)
                     (is (every? #(pos? (get-in % [:attempt/receipt :attempt/metrics :spend :notional-microdollars])) attempts)
                         "Completed subscription comparisons retain list-price cost despite zero incremental spend")
                     (do
                       (is (every? #(false? (get-in % [:attempt/receipt :attempt/metrics :spend :priced?])) attempts))
                       (is (every? #(nil? (get-in % [:attempt/receipt :attempt/metrics :spend :notional-microdollars])) attempts)
                           "An interrupted model step must not certify a complete list-price cost")))
                   (is (= {resource/microdollars 10000M} (resource/balance control)))
                   (is (= 1 @retired))
                   (is (empty? (workspace/read-tree control "/out")))
                   (is (empty? (workspace/read-tree clean "/")))
                   (is (not-any? #(re-find #"PRIVATE-CHECKER-GOLD|PRIVATE-CONTROL-DATA" (json/write-value-as-string %)) @calls))
                   (is (= (:job submission) (:job (benchmarks/submit! conn host-ctx (ops/party-principal (:owner w)) (:room w) control request))))
                   (is (= dispatches (count @calls)) "Request replay never creates more model HTTP attempts"))))
             (finally (discourse/close-room! clean)))))))))

(deftest preparation-rejects-revision-case-model-and-budget-ambiguity
  (let [files fixture/fixture
        request (input files)]
    (doseq [bad [(assoc request :expected-bundle (random-uuid))
                 (assoc request :models ["same" "same"])
                 (assoc request :cases ["missing"])
                 (assoc-in request [:limits :settlement] "review")
                 (assoc request :models ["a" "b"] :repetitions 2
                        :limits (assoc (:limits request) :max-dispatches 1))]]
      (is (thrown? clojure.lang.ExceptionInfo (prepared files bad))))))

(deftest unsafe-bundle-policies-are-not-carried-into-benchmark-worlds
  (doseq [extra [":judge {:model \"extra-spend\"}" ":capture [\"/\"]"
                 ":web :live" ":profile \"admin\"" ":allowed-sources [\"example.com\"]"]]
    (let [files (assoc fixture/fixture "workflow.edn"
                       (str "{:title \"Echo\" :task \"Write /out/result.md\" " extra "}"))]
      (is (thrown? clojure.lang.ExceptionInfo (prepared files (input files)))))))

(deftest rendered-task-is-bounded-before-candidate-preparation
  (let [files (assoc fixture/fixture "workflow.edn"
                     (pr-str {:title "Large expansion" :task "{payload}"
                              :params {:payload (apply str (repeat 16001 "x"))}}))]
    (with-redefs [workflow/candidates (fn [& _] (throw (AssertionError. "must refuse before resolving candidates")))]
      (is (thrown? clojure.lang.ExceptionInfo (benchmarks/prepare (snapshot files) (input files)))))))

(deftest checker-execution-requires-both-installation-and-room-authority
  (world/with-world [conn w]
    (doseq [party [(:owner w) (:member w) (:non-member w) (:admin w) nil]]
      (is (thrown? clojure.lang.ExceptionInfo
                   (benchmarks/authorize! conn (ops/party-principal party) (:room w)))))
    (d/transact conn [{:party/id (:owner w) :party/role :admin}])
    (is (= (:owner w) (benchmarks/authorize! conn (ops/party-principal (:owner w)) (:room w))))))

(deftest pure-checker-authority-uses-only-the-supplied-control-snapshot
  (world/with-world [conn w]
    (let [before @conn principal (ops/party-principal (:owner w))]
      (d/transact conn [{:party/id (:owner w) :party/role :admin}])
      (is (thrown? clojure.lang.ExceptionInfo (benchmarks/authorize-in! before principal (:room w))))
      (is (= (:owner w) (benchmarks/authorize-in! @conn principal (:room w))))
      (is (= (:owner w) (benchmarks/authorize! conn principal (:room w)))))))

(deftest normal-benchmark-retry-extraction-preserves-generic-observation-and-conflicts
  (world/with-world [conn w]
    (d/transact conn [{:party/id (:owner w) :party/role :admin}])
    (let [principal (ops/party-principal (:owner w)) request (input fixture/fixture)
          ctx ec/*execution-context*]
      (is (= {:status :absent} (benchmarks/retry-request :benchmark conn (:owner w) (:room w) request)))
      (let [{:keys [request]} (requests/claim! conn {:party (:owner w) :room (:room w)
                                                     :client-id (:request-id request) :kind :benchmark
                                                     :intent {:submission request :provenance :retained}
                                                     :limits (update (:limits request) :settlement keyword)})
            id (:work.request/id request)
            original (get-in (requests/payload conn request) [:input :submission])
            calls (atom 0) forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Retry prepared new work")))]
        (is (= {:status :retry :id id} (benchmarks/retry-request :benchmark conn (:owner w) (:room w) original)))
        (is (= {:status :conflict :id id} (benchmarks/retry-request :cli-benchmark conn (:owner w) (:room w) original)))
        (with-redefs [benchmarks/prepare forbidden workflows/snapshot forbidden]
          (is (= id (:id (benchmarks/submit! conn ctx principal (:room w) nil original))))
          (is (= ::benchmarks/conflict
                 (:type (ex-data (try (benchmarks/submit! conn ctx principal (:room w) nil
                                                          (assoc original :expected-bundle (random-uuid)))
                                      (catch clojure.lang.ExceptionInfo error error))))))
          (is (zero? @calls)))))))

(deftest experiment-adapter-keeps-records-in-control-room-and-refunds-after-cleanup
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [control]
       (let [host-ctx ec/*execution-context*
             clean (discourse/make-room {:id (keyword (str "fixture-" (random-uuid)))
                                         :store (memory/make)})
             files (assoc fixture/fixture "checker.clj"
                          "(ns checker) (defn check [{:keys [files]}] {:checks {:empty-output? (empty? files)} :reward (if (empty? files) 1 0)})")
             files (assoc files "calibration.edn"
                          "{:reference {} :damaged {:unexpected {:files {\"/out/private-damaged-answer\" \"must not reach candidate\"} :loses [:empty-output?]}}}")
             request (input files)
             base (prepared files request)
             [_ echo] (@#'is.simm.ops.work-test/fixture)
             plan (-> (:plan base)
                      (assoc :team (:team echo))
                      ;; Echo is deliberately not an LLM: use the same
                      ;; isolated worlds/wallets but omit LLM-only limits.
                      (update :environments
                              (fn [envs]
                                (mapv (fn [env]
                                        (let [ref (:environment/verifier env)]
                                          (environment/make-environment
                                           {:id (:environment/id env) :task (:environment/task env)
                                            :verifier {:id (:verifier/id ref) :version (:verifier/version ref)
                                                       :basis (:verifier/basis ref)}
                                            :world (:environment/world env)
                                            :limits (dissoc (:environment/limits env) :max-model-steps :budget-dollars)
                                            :metadata (:environment/metadata env)}))) envs))))
             experiment (runner/experiment-def
                         (assoc plan :id :fixture/benchmark :repetitions 1 :metadata {}))
             prepared (-> base (assoc :plan (assoc plan :experiment experiment))
                          (assoc-in [:basis :team] (:team echo))
                          (assoc-in [:basis :experiment] experiment))
             retired (atom 0)]
         (try
           (workspace/ensure-workspace! control)
           (workspace/seed! control {"/private-gold.md" "must not leak into candidate"})
           (workflows/write! control (ops/party-principal (:owner w)) {:name "echo" :files files})
           (d/transact conn [{:party/id (:owner w) :party/role :admin}])
           (resource/mint! control {:id (random-uuid) :resources (work/resources (:limits request))})
           (with-redefs-fn {#'benchmarks/prepare (fn [& _] prepared)
                            #'benchmarks/fixture-room! (fn [& _] clean)
                            #'benchmarks/retire-fixture! (fn [& _] (swap! retired inc))}
             (fn []
               (let [submitted (benchmarks/submit! conn host-ctx (ops/party-principal (:owner w))
                                                   (:room w) control request)
                     status (jobs/status (:job submitted) 25000)]
                 (is (= :completed (:status status)))
                 (is (:settled? status))
                 (is (= 1 @retired))
                 (is (= (work/resources (:limits request)) (resource/balance control)))
                 (is (false? (:return-pending? (work/status conn host-ctx (ops/party-principal (:owner w))
                                                            (:room w) (:id submitted)))))
                 (is (= {} (workspace/read-tree clean "/")) "fixture never inherited the control room's answer")
                 (let [entry (first (store/-list-attempts (:store control) (store/conversation-id control) {}))
                       attempt (store/-load-attempt (:store control) (store/conversation-id control) (:attempt/id entry))
                       evidence (:attempt/evidence attempt)
                       exported (trajectory/trajectory control attempt)]
                   (doseq [key [:result :trace :files]]
                     (is (contains? evidence key) (str "trusted evidence retained: " key)))
                   (is (= (:resources evidence) (get-in attempt [:attempt/receipt :attempt/resources])))
                   (is (seq (get-in evidence [:trace :runs])))
                   (is (true? (get-in evidence [:calibration :ok?])))
                   (is (= {} (:files evidence)) "calibration answers were never installed in the world")
                   (is (= (str (:attempt/id attempt)) (:attempt exported)))
                   (is (vector? (:messages exported))))
                 (workflows/write! control (ops/party-principal (:owner w))
                                   {:name "echo" :files {"gold.edn" "{:changed true}"}})
                 (is (= (:job submitted)
                        (:job (benchmarks/submit! conn host-ctx (ops/party-principal (:owner w))
                                                  (:room w) control request))))
                 (is (= 1 (count (jobs/jobs control))))
                 (is (thrown? clojure.lang.ExceptionInfo
                              (benchmarks/submit! conn host-ctx (ops/party-principal (:owner w))
                                                  (:room w) control (assoc request :repetitions 2)))))))
           (finally (discourse/close-room! clean))))))))

(deftest physical-capacity-permit-is-exclusive-owned-and-returned-once
  (let [a (benchmarks/adapter nil nil {} nil)
        b (benchmarks/adapter nil nil {} nil)
        slot @#'benchmarks/checker-slot]
    ((:admit! a) nil)
    (is (zero? (.availablePermits slot)))
    (is (thrown? clojure.lang.ExceptionInfo ((:admit! b) nil)))
    ((:settled! b) nil)
    (is (zero? (.availablePermits slot)) "loser cannot release winner's permit")
    ((:settled! a) nil)
    ((:settled! a) nil)
    (is (= 1 (.availablePermits slot)))))

(deftest fixture-recovery-covers-pointerless-and-already-refunded-crash-windows
  (let [row (fn [more] (merge {:work.request/id (random-uuid) :work.request/kind :benchmark} more))
        interrupted (row {:work.request/state :interrupted})
        admitting (row {:work.request/state :admitting})
        completed (row {:work.request/job (random-uuid) :work.request/return-pending? false})
        live (row {:work.request/job (random-uuid) :work.request/return-pending? false})
        pending (row {:work.request/job (random-uuid) :work.request/return-pending? true})
        statuses {(:work.request/job completed) {:status :completed :settled? true}
                  (:work.request/job live) {:status :running :settled? false}
                  (:work.request/job pending) {:status :failed :settled? true}}
        retired (atom [])]
    (with-redefs-fn {#'requests/requests (constantly [interrupted admitting completed live pending])
                     #'jobs/status statuses
                     #'benchmarks/retire-fixture! (fn [_ id] (swap! retired conj id))}
      #(benchmarks/recover! nil nil))
    (is (= [(:work.request/id interrupted) (:work.request/id completed)] @retired))))

(deftest failed-calibration-prevents-candidate-execution
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [control]
       (let [clean (discourse/make-room {:id (keyword (str "fixture-" (random-uuid))) :store (memory/make)})
             files (assoc fixture/fixture
                          "checker.clj" "(ns checker) (defn check [_] {:checks {:correct? false} :reward 0})"
                          "calibration.edn"
                          "{:reference {} :damaged {:wrong {:files {} :loses [:missing-check]}}}")
             request (input files)
             prepared (echo-prepared files request)
             invoked (atom 0)
             retired (atom 0)]
         (try
           (resource/mint! control {:id (random-uuid) :resources (work/resources (:limits request))})
           (with-redefs-fn {#'benchmarks/fixture-room! (fn [& _] clean)
                            #'benchmarks/retire-fixture! (fn [& _] (swap! retired inc))
                            #'program/execute-agent-program (fn [& _] (swap! invoked inc)
                                                              (throw (AssertionError. "candidate must never start")))}
             (fn []
               (let [submitted (work/submit-prepared! conn ec/*execution-context* (ops/party-principal (:owner w))
                                                      (:room w) control request
                                                      (benchmarks/adapter ec/*execution-context* control prepared
                                                                          (ops/party-principal (:owner w))))
                     status (jobs/status (:job submitted) 25000)
                     attempts (store/-list-attempts (:store control) (store/conversation-id control) {})]
                 (is (:settled? status))
                 (is (zero? @invoked))
                 (is (= 1 @retired))
                 (is (= (work/resources (:limits request)) (resource/balance control)))
                 ;; Setup failure precedes certification, not a fabricated
                 ;; zero-reward Attempt. An empty loop previously hid this.
                 (is (empty? attempts))
                 (let [children (->> (run/runs control {:root-run-id (:job submitted) :limit 1000})
                                     (remove #(= (:job submitted) (:run/id %))) vec)]
                   (is (= 1 (count children)))
                   (is (= :failed (:run/status (first children))))
                   (is (= :world-setup-failed (:run/reason (first children))))
                   (is (re-find #"Checker calibration failed" (:run/error (first children)))))
                 (is (empty? (experiment/scorecards control {}))))))
           (finally (discourse/close-room! clean))))))))

(deftest cancelled-calibration-retains-capacity-until-physical-exit
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [control]
       (let [clean (discourse/make-room {:id (keyword (str "fixture-" (random-uuid))) :store (memory/make)})
             entered (promise)
             release (java.util.concurrent.CountDownLatch. 1)
             files (assoc fixture/fixture "calibration.edn"
                          "{:reference {} :damaged {:wrong {:files {} :loses [:missing-check]}}}")
             request (input files)
             prepared (echo-prepared files request)
             host-ctx ec/*execution-context*
             owner (ops/party-principal (:owner w))
             grant (work/resources (:limits request))
             retired (atom 0)]
         (try
           (resource/mint! control {:id (random-uuid) :resources grant})
           (with-redefs-fn {#'benchmarks/fixture-room! (fn [& _] clean)
                            #'benchmarks/retire-fixture! (fn [& _] (swap! retired inc))
                            #'checkers/calibrate (fn [_]
                                                   (deliver entered true)
                                                   (loop []
                                                     (when-not (try (.await release) true
                                                                    (catch InterruptedException _ false))
                                                       (recur)))
                                                   {:ok? true})}
             (fn []
               (let [submitted (work/submit-prepared! conn host-ctx owner (:room w) control request
                                                      (benchmarks/adapter host-ctx control prepared owner))
                     job (:job submitted)]
                 (is (= true (deref entered 10000 ::timeout)))
                 (run/cancel-run! job)
                 (is (thrown? clojure.lang.ExceptionInfo (evaluation/await-cleanups-for! control job 1)))
                 (is (zero? (.availablePermits @#'benchmarks/checker-slot)))
                 (is (zero? @retired))
                 (is (every? zero? (vals (resource/balance control))))
                 (is (:return-pending? (work/status conn host-ctx owner (:room w) (:id submitted))))
                 (.countDown release)
                 (is (:settled? (jobs/status job 25000)))
                 (is (= grant (resource/balance control)))
                 (is (= 1 @retired))
                 (is (= 1 (.availablePermits @#'benchmarks/checker-slot))))))
           (finally (.countDown release) (discourse/close-room! clean))))))))
