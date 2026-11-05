(ns is.simm.ops.workflow-profiles-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.experiment.runner :as runner]
            [dvergr.agent.roster :as roster]
            [dvergr.agent.workflow :as workflow]
            [dvergr.catalog.room :as catalog]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.coordinator-sandbox :as coordinator]
            [is.simm.ops.work :as work]
            [is.simm.ops.knowledge-execution-origin :as execution-origin]
            [is.simm.ops.workflow-profiles :as profiles]
            [is.simm.ops.workflow-runs :as workflow-runs]))

(def files
  {"workflow.edn" "{:title \"Solve and critique\" :profile \"coordinator\" :task \"Write /out/result.md\" :capture [\"/out\"]}"
   "checker.clj" "(defn check [_] {:checks {:done true} :reward 1.0})"
   "gold.edn" "{:secret \"PRIVATE-GRADING-REFERENCE\"}"
   "fixtures/inputs/note.md" "Public source"})

(def limits {:budget-microdollars 10000 :max-dispatches 4 :max-model-steps 3
             :timeout-ms 2000 :settlement "discard"})

(defn snapshot [files] {:files files :manifest []})

(deftest ordinary-and-measured-composite-work-share-the-same-profile
  (with-redefs [workflow/admit-model! (fn [_ model] {:model model :provider :openai})
                runner/runtime-identity (constantly {:libraries {}})]
    (let [bundle (:id (catalog/bundle "composite" files))
          ordinary (workflow-runs/prepare (snapshot files)
                                          {:name "composite" :expected-bundle bundle
                                           :request-id (random-uuid) :model "fixture-model" :limits limits})
          measured (benchmarks/prepare (snapshot files)
                                       {:name "composite" :expected-bundle bundle :request-id (random-uuid)
                                        :models ["fixture-model"] :repetitions 1 :limits limits})
          a (first (roster/agents (:team ordinary)))
          b (first (roster/agents (get-in measured [:plan :team])))
          child-scope (get-in measured [:plan :experiment-capabilities])
          private-ref (evaluation/evaluator-ref (get-in measured [:plan :capabilities :evaluator]))]
      (doseq [key [:agent/tools :agent/sandbox :agent/prompt :agent/model-policy :agent/program]]
        (is (= (get a key) (get b key)) (str key)))
      (is (= #{coordinator/id} (:agent/sandbox a)))
      (is (= coordinator/delegation-policy (:delegation-policy ordinary)
             (get-in measured [:plan :delegation-policy])))
      (is (= #{(evaluation/evaluator-ref work/completion)} (set (keys (:evaluators child-scope)))))
      (is (not (contains? (:evaluators child-scope) private-ref)))
      (is (= {} (:world-setups child-scope)))
      (is (not (contains? child-scope :invocations)))
      (is (contains? (get-in ordinary [:environment :environment/world :effects :admit]) :lifecycle))
      (is (contains? (get-in measured [:plan :environments 0 :environment/world :effects :admit]) :lifecycle))
      (is (= #{} (get-in measured [:plan :environments 0 :environment/world :effects :allow-hosts])))
      (is (= (:basis (profiles/compile-team (:team ordinary) {:profile "coordinator"}))
             (get-in measured [:plan :experiment :experiment/metadata :simmis/execution-profile])))
      (is (= "coordinator" (get-in ordinary [:basis :execution-profile :declared]))))))

(deftest ordinary-file-workers-are-not-recompiled-or-given-child-authority
  (with-redefs [workflow/admit-model! (fn [_ model] {:model model :provider :openai})]
    (let [base (work/prepare {:request-id (random-uuid) :task "Write a note"
                              :model "fixture-model" :limits limits})]
      (is (identical? base (profiles/prepare base {})))
      (is (identical? base (profiles/prepare base {:profile "developer"})))
      (is (not (contains? base :experiment-capabilities)))
      (is (not (contains? base :delegation-policy)))
      (doseq [bad ["coordinator " "administrator" :coordinator]]
        (is (thrown? clojure.lang.ExceptionInfo (profiles/prepare base {:profile bad})))))))

(deftest recursive-cli-benchmarks-refuse-before-provider-construction
  (let [input {:name "composite" :expected-bundle (:id (catalog/bundle "composite" files))
               :request-id (random-uuid) :models ["claude-code-haiku"] :repetitions 1
               :limits {:max-cli-runs 1 :max-model-steps 3 :timeout-ms 2000 :settlement "discard"}}]
    (with-redefs [workflow/candidates (fn [& _] (throw (AssertionError. "No provider construction")))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"native copied-fixture"
                            (benchmarks/prepare-cli (snapshot files) input))))))

(deftest recursive-checkpoint-workflows-refuse-before-provider-construction
  (with-redefs [execution-origin/validate-execution! (constantly {:workspace :retained-batch-checkpoint})
                work/prepare (fn [& _] (throw (AssertionError. "No provider construction")))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"copied-context execution"
                          (workflow-runs/prepare
                           (snapshot files)
                           {:name "composite" :expected-bundle (:id (catalog/bundle "composite" files))
                            :request-id (random-uuid) :model "fixture-model" :limits limits}
                           nil {:fixed-host-selection true})))))
