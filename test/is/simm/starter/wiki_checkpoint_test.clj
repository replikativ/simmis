(ns is.simm.starter.wiki-checkpoint-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [dvergr.agent.experiment.runner :as runner]
            [dvergr.agent.workflow :as workflow]
            [dvergr.catalog.room :as catalog]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.checkers :as checkers]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.starter.wiki-maintenance :as starter]))

(defn- derived-source []
  (let [root (io/file "examples/workflows/wiki-maintenance-checkpoint-eval-v5")
        paths (concat (keys (starter/source))
                      ["benchmark.edn" "calibration.edn"]
                      (for [case ["mixed-evidence" "capture-uncertainty" "no-change"]
                            file ["fixture-pack.edn" "gold.edn"]]
                        (str "cases/" case "/" file)))]
    (into (sorted-map) (map (fn [path] [path (slurp (io/file root path))])) paths)))

(deftest packaged-v5-benchmark-keeps-its-task-and-calibrates-selected-wiki-cases
  (let [source (derived-source) bundle (catalog/bundle starter/name source)
        live (catalog/bundle starter/name (starter/source))
        policy (inputs/declaration (starter/source))
        input {:request-id (random-uuid) :name starter/name :expected-bundle (:id bundle)
               :models ["codex-subscription-luna-6"] :repetitions 1 :split "development"
               :limits {:budget-microdollars 10000 :max-dispatches 4 :max-model-steps 4
                        :timeout-ms 30000 :settlement "discard"}}]
    (is (= (starter/source) (select-keys source (keys (starter/source)))))
    (is (not= (:id live) (:id bundle)))
    (is (= (:definition live) (:definition bundle)))
    (is (= (:checker live) (:checker bundle)))
    (is (:ok? (checkers/calibrate bundle checkers/calibration-policy policy)))
    (with-redefs [workflow/model-policy (fn [model] {:provider :codex-subscription :model model})
                  runner/runtime-identity (constantly {:libraries {}})]
      (let [prepared (benchmarks/prepare-checkpoint-fixture {:files source} input)]
        (is (= (:id bundle) (get-in prepared [:bundle :id])))
        (is (= ["mixed-evidence" "capture-uncertainty"] (get-in prepared [:plan :experiment :experiment/metadata :simmis/evaluation :cases]))))
      (is (thrown? clojure.lang.ExceptionInfo (benchmarks/prepare {:files source} input)))
      (is (thrown? clojure.lang.ExceptionInfo (benchmarks/prepare-cli {:files source} input))))
    (let [quoted (assoc-in bundle [:cases "mixed-evidence" :gold :quotes]
                           [{:source "/wiki/Company.md" :text "# Bright Harbor Labs"}])]
      (is (false? (get-in (checkers/score quoted policy {:environment/metadata {:case "mixed-evidence"}}
                                           (get-in bundle [:calibration :references :mixed :files]) checkers/policy)
                          [:checks :quotes?]))))))
