(ns is.simm.ops.composite-workflow-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.agent.run :as run]
            [dvergr.catalog.room :as catalog]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.jobs :as jobs]
            [dvergr.model.api.codex-subscription :as codex]
            [dvergr.model.gateway :as gateway]
            [dvergr.model.providers :as providers]
            [dvergr.resource :as resource]
            [dvergr.room.store :as store]
            [dvergr.room.store.memory :as memory]
            [is.simm.authority.world :as world]
            [is.simm.ops.benchmark-fixture :as fixture]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.coordinator-benchmark-test :as responses]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.work :as work]
            [is.simm.ops.work-test]
            [is.simm.ops.workflows :as workflows]
            [jsonista.core :as json]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util.concurrent Semaphore]))

(def composite-code
  (str
   "(do (require '[dvergr.agent :as agent]) "
   "(let [offers (agent/offered)] "
   " (assert (= 1 (count (:evaluators offers)))) "
   " (assert (= :simmis/work-completion (:verifier/id (first (:evaluators offers))))) "
   " (assert (empty? (:world-setups offers))) (assert (empty? (:invocations offers)))) "
   "(assert (not (babashka.fs/exists? \"/workflows/composite/gold.edn\"))) "
   "(assert (not (babashka.fs/exists? \"/private-control.md\"))) "
   "(assert (= \"Public source\" (slurp \"/inputs/note.md\"))) "
   "(let [team (-> (agent/roster) "
   " (agent/make-agent {:id :solver :tools #{} :sandbox #{:simmis/file-worker} "
   " :model-policy {:provider :codex-subscription :model \"codex-subscription-luna-6\"} "
   " :program {:kind :llm :max-model-steps 1 :auto-compact? false}}) "
   " (agent/make-agent {:id :critic :tools #{} :sandbox #{:simmis/file-worker} "
   " :model-policy {:provider :codex-subscription :model \"codex-subscription-luna-6\"} "
   " :program {:kind :llm :max-model-steps 1 :auto-compact? false}})) "
   " solved @(agent/hire! team :solver {:task \"Solve from /inputs/note.md\" :settlement :discard :resources {\"microUSD\" 1000M \"model-dispatch\" 1M}}) "
   " critiqued @(agent/hire! team :critic {:task (pr-str (:run/value solved)) :settlement :discard :resources {\"microUSD\" 1000M \"model-dispatch\" 1M}}) "
   " malicious (agent/make-agent (agent/roster) "
   "  {:id :escape :sandbox #{:simmis/file-worker} :tools #{:host_shell} :program {:kind :echo}}) "
   " denied? (try (agent/hire! malicious :escape {:task :escape :settlement :discard}) false (catch Exception _ true))] "
   " (assert (clojure.string/includes? (pr-str (:run/value critiqued)) \"critic-result\")) (assert denied?) "
   " (babashka.fs/create-dirs \"/out\") (spit \"/out/result.md\" \"Solved and critiqued\")))"))

(def files
  {"workflow.edn" "{:title \"Solver and internal critic\" :profile \"coordinator\" :task \"Solve, critique, and write /out/result.md\" :capture [\"/out\"]}"
   "checker.clj" "(ns composite.checker) (defn check [{:keys [files gold]}] (let [ok (= (:expected gold) (get files \"/out/result.md\"))] {:checks {:final-output? ok} :reward (if ok 1.0 0.0)}))"
   "gold.edn" "{:expected \"Solved and critiqued\" :secret \"PRIVATE-CHECKER-GOLD\"}"
   "fixtures/inputs/note.md" "Public source"})

(defn- exercise-composite [cancel?]
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [control]
       (let [host-ctx ec/*execution-context*
             clean (discourse/make-room {:id (keyword (str "composite-fixture-" (random-uuid)))
                                         :store (memory/make)})
             request {:request-id (random-uuid) :name "composite"
                      :expected-bundle (:id (catalog/bundle "composite" files))
                      :models ["codex-subscription-sol-6.1"] :repetitions 1
                      :limits {:budget-microdollars 10000 :max-dispatches 8 :max-model-steps 2
                               :timeout-ms 30000 :settlement "discard"}}
             calls (atom []) root-calls (atom 0) child-calls (atom 0) retired (atom 0) slot (Semaphore. 1)
             entered (promise) release (promise)
             provider (codex/create {:credentials (gateway/static-credentials :fixture {} #{"https://chatgpt.com"})})]
         (try
           (binding [ec/*execution-context* (:ctx clean)] (workspace/ensure-workspace! clean))
           (workspace/ensure-workspace! control)
           (workspace/seed! control {"/private-control.md" "PRIVATE-CONTROL-DATA"})
           (workflows/write! control (ops/party-principal (:owner w)) {:name "composite" :files files})
           (d/transact conn [{:party/id (:owner w) :party/role :admin}])
           (resource/mint! control {:id (random-uuid) :resources (work/resources (:limits request))})
           (with-redefs-fn
             {#'benchmarks/checker-slot slot #'benchmarks/pending-retirements (atom {})
              #'providers/providers (atom {:codex-subscription provider})
              #'fixture/validate-receipt! (fn [_ _ receipt] receipt)
              #'fixture/create! (fn [& _] {:room clean :persistent-id (random-uuid)})
              #'fixture/retire! (fn [& _] (swap! retired inc))
              #'gateway/*request-fn*
              (fn [http]
                (let [body (json/read-value (:body http) json/keyword-keys-object-mapper)]
                  (swap! calls conj body)
                  (case (:model body)
                    "gpt-6.1-sol"
                    (case (swap! root-calls inc)
                      1 (@#'responses/tool composite-code "compose" (:model body))
                      2 (@#'responses/final "done" (:model body))
                      (throw (ex-info "Unexpected root dispatch" {})))
                    "gpt-6-luna"
                    (let [n (swap! child-calls inc)]
                      (when cancel?
                        (deliver entered true)
                        ;; Simulate a transport that acknowledges cancellation
                        ;; only after physical IO returns. No model call occurs.
                        (loop []
                          (when-not (try @release (catch InterruptedException _ false)) (recur))))
                      (@#'responses/response
                       [{:type "response.output_text.delta" :delta (if (= n 1) "solver-result" "critic-result")}
                        {:type "response.completed" :response {:id (str "child-" n) :model (:model body)
                                                               :usage {:input_tokens 1 :output_tokens 1}}}]))
                    (throw (ex-info "Unexpected model dispatch" {})))))}
             (fn []
               (let [submission (benchmarks/submit! conn host-ctx (ops/party-principal (:owner w))
                                                    (:room w) control request)
                     _ (when cancel?
                         (is (= true (deref entered 10000 :not-entered)))
                         (run/cancel-run! (:job submission))
                         (is (not (:settled? (jobs/status (:job submission)))))
                         (is (zero? @retired))
                         (is (zero? (.availablePermits slot)))
                         (is (every? zero? (vals (resource/balance control))))
                         (deliver release true))
                     status (jobs/status (:job submission) 60000)
                     attempts (store/-list-attempts (:store control) (store/conversation-id control) {})
                     outer (first attempts)
                     runs (run/runs control {:limit 20})
                     solver (first (filter #(= :solver (:run/actor %)) runs))
                     critic (first (filter #(= :critic (:run/actor %)) runs))]
                 (if cancel?
                   (do
                     (is (= :cancelled (:status status)) (pr-str (select-keys status [:status :error])))
                     (is (:settled? status))
                     (is (= 2 (count @calls)))
                     (is (= [1 1] [@root-calls @child-calls]))
                     (is (= :cancelled (:run/status solver)))
                     (is (nil? critic))
                     (is (= :discarded (:run/settlement-status solver)))
                     (is (= 1 @retired))
                     (is (= 1 (.availablePermits slot)))
                     (is (= 6M (get (resource/balance control) resource/model-dispatches)))
                     (is (empty? (workspace/read-tree control "/out"))))
                   (do
                     (is (= :completed (:status status)) (pr-str (select-keys status [:status :error])))
                     (is (:settled? status))
                     (is (= 4 (count @calls)))
                     (is (= 2 @root-calls @child-calls))
                     (is (not-any? #(re-find #"PRIVATE-CHECKER-GOLD|PRIVATE-CONTROL-DATA" (pr-str %)) @calls))
                     (is (clojure.string/includes?
                          (pr-str (last (filter #(= "gpt-6-luna" (:model %)) @calls))) "solver-result"))
                     (is (= 1 (count attempts)))
                     (is (= 1.0 (get-in outer [:attempt/receipt :attempt/reward]))
                         (pr-str (filter #(= "function_call_output" (:type %)) (mapcat :input @calls))))
                     (is (= {"/out/result.md" "Solved and critiqued"} (get-in outer [:attempt/evidence :files])))
                     (is (= (:attempt/run-id outer) (:run/parent solver) (:run/parent critic)))
                     (is (not-any? #(= :escape (:run/actor %)) runs))
                     (is (every? #(= :discarded (:run/settlement-status %)) [solver critic]))
                     (is (= {resource/microdollars 10000M resource/model-dispatches 4M} (resource/balance control)))
                     (is (= 1 @retired)) (is (= 1 (.availablePermits slot)))
                     (is (empty? (workspace/read-tree control "/out")))
                     (is (= (:job submission) (:job (benchmarks/submit! conn host-ctx (ops/party-principal (:owner w))
                                                                        (:room w) control request))))
                     (is (= 4 (count @calls))))))))
           (finally
             (deliver release true)
             (discourse/close-room! clean))))))))

(deftest a-benchmarked-workflow-can-own-solver-and-critic-without-grading-authority
  (exercise-composite false))

(deftest cancelling-a-composite-retains-its-fixture-and-grant-until-child-io-acknowledges
  (exercise-composite true))
