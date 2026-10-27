(ns is.simm.starter.wiki-checkpoint-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [dvergr.agent.experiment.runner :as runner]
            [dvergr.agent.workflow :as workflow]
            [dvergr.catalog.room :as catalog]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.checkers :as checkers]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.wiki-scoring-test :as fixture]
            [is.simm.starter.wiki-maintenance :as starter]
            [is.simm.starter.wiki-maintenance-test :as source-fixture]))

(defn- derived-source []
  (let [policy (inputs/declaration (starter/source))
        cases (:cases (fixture/bundle))
        pack (fn [wiki] (pr-str {:kind :dvergr/fixture-pack :version 1
                                :files (assoc (@#'source-fixture/frozen policy {"inbox/note.md" fixture/source})
                                              "/wiki/Company.md" wiki)}))]
    (merge (starter/source)
           {"cases/supported/fixture-pack.edn" (pack fixture/original)
            "cases/supported/gold.edn" (pr-str (get-in cases ["supported" :gold]))
            "cases/no-change/fixture-pack.edn" (pack fixture/changed)
            "cases/no-change/gold.edn" (pr-str (get-in cases ["no-change" :gold]))
            "benchmark.edn" (pr-str {:version 1 :splits {:development ["supported"] :held-out ["no-change"]}})
            "calibration.edn"
            (pr-str {:version 2
                     :references {:update {:case "supported" :files {"/out/wiki/Company.md" fixture/changed}}
                                  :new-page {:case "supported" :files {"/out/wiki/Product.md" fixture/fresh}}
                                  :no-op {:case "no-change" :files {}}}
                     :damaged {:duplicate {:case "supported"
                                           :files {"/out/wiki/Product.md" (str/replace fixture/fresh fixture/other-page fixture/page)}
                                           :loses [:unique-identities?]}}})})))

(deftest packaged-v5-benchmark-keeps-its-task-and-calibrates-selected-wiki-cases
  (let [source (derived-source) bundle (catalog/bundle starter/name source)
        live (catalog/bundle starter/name (starter/source))
        policy (inputs/declaration (starter/source))
        input {:request-id (random-uuid) :name starter/name :expected-bundle (:id bundle)
               :models ["codex-subscription-luna-6"] :repetitions 1 :split "development"
               :limits {:budget-microdollars 10000 :max-dispatches 4 :max-model-steps 4
                        :timeout-ms 30000 :settlement "discard"}}]
    (is (not= (:id live) (:id bundle)))
    (is (= (:definition live) (:definition bundle)))
    (is (= (:checker live) (:checker bundle)))
    (is (:ok? (checkers/calibrate bundle checkers/calibration-policy policy)))
    (with-redefs [workflow/model-policy (fn [model] {:provider :codex-subscription :model model})
                  runner/runtime-identity (constantly {:libraries {}})]
      (let [prepared (benchmarks/prepare-checkpoint-fixture {:files source} input)]
        (is (= (:id bundle) (get-in prepared [:bundle :id])))
        (is (= ["supported"] (get-in prepared [:plan :experiment :experiment/metadata :simmis/evaluation :cases]))))
      (is (thrown? clojure.lang.ExceptionInfo (benchmarks/prepare {:files source} input)))
      (is (thrown? clojure.lang.ExceptionInfo (benchmarks/prepare-cli {:files source} input))))
    (let [quoted (assoc-in bundle [:cases "supported" :gold :quotes]
                           [{:source "/wiki/Company.md" :text "# Company"}])]
      (is (false? (get-in (checkers/score quoted policy {:environment/metadata {:case "supported"}}
                                           {"/out/wiki/Company.md" fixture/changed} checkers/policy)
                          [:checks :quotes?]))))))
