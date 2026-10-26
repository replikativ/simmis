(ns is.simm.ops.knowledge-checkpoint-prompt-test
  "Model-free prompt parity through the actual Simmis checker/setup wrappers."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.experiment :as experiment]
            [dvergr.agent.program :as program]
            [dvergr.agent.roster :as roster]
            [dvergr.chat.agent :as chat-agent]
            [dvergr.model.chat :as model-chat]
            [dvergr.model.providers :as providers]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.bytes :as bytes]
            [geschichte.repo :as repo]
            [is.simm.documents.execution-view :as execution-view]
            [is.simm.ops.checkers :as checkers]
            [is.simm.ops.knowledge-changes :as changes]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.knowledge-workspace :as knowledge-workspace]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.ops.owned-workspace-test :as fixture]
            [is.simm.ops.wiki-scoring-test :as scoring-fixture]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- seed-guidance! [conn marker]
  (repo/write! conn "AGENTS.md" (bytes/utf8 (str "# Guidance\n" marker "-GUIDE\n")))
  (repo/write! conn "skills/checkpoint-parity.md"
               (bytes/utf8 (str "---\nname: checkpoint-parity\ndescription: " marker
                                "-SKILL\nrequires_tools: [clojure_eval]\nvetted: true\n---\nRead the knowledge source carefully.\n")))
  (repo/stage-all! conn)
  (repo/commit! conn {:message marker}))

(defn- observed-setup [base observations]
  (evaluation/make-world-setup
   {:id :simmis-test/checkpoint-prompt :basis {:base (evaluation/world-setup-ref base)}
    :prepare (fn [{world :room :as context}]
               (swap! observations update :preparations inc)
               (let [result ((:prepare base) context)]
                 (binding [ec/*execution-context* (:ctx world)]
                   (let [{:keys [system conn]} (workspace/current-workspace)]
                     (swap! observations assoc :source-role?
                            (= ["in/knowledge" "out/wiki"]
                               (:exclude-prefixes (execution-view/selection-options system conn {}))))))
                 result))}))

(defn- exercise! [{:keys [conn w room owned attached own-descriptor]} select-owned?]
  (seed-guidance! owned "C1-OWNED")
  (seed-guidance! attached "ATTACHED")
  (let [target (:target (owned/resolve! @conn (:owner w) :read (:room w) room))
        basis (changes/basis @owned)
        selection {:version 1 :policy :request-evidence-v1 :basis basis :target target
                   :origin {:version 1 :kind :datahike :commit (:commit basis)
                            :source (select-keys basis [:store :repository])}}
        observations (atom {:preparations 0 :calibrations 0 :prompts [] :input-ids [] :calls 0})
        bundle (assoc (scoring-fixture/bundle) :calibration
                      {:version 2
                       :references {:no-op {:case "no-change" :files {}}}
                       :damaged {:unexpected-change {:case "no-change"
                                                     :files {"/out/wiki/Product.md" scoring-fixture/fresh}
                                                     :loses [:expected-change?]}}})
        base (observed-setup (checkers/world-setup bundle inputs/default-policy) observations)
        setup (knowledge-workspace/with-checkpoint
                (knowledge-workspace/with-publication-target base target) selection)
        evaluator (checkers/evaluator bundle inputs/default-policy)
        verifier (evaluation/evaluator-ref evaluator)
        env (environment/make-environment
             {:id :simmis-test/checkpoint-prompt :task "answer"
              :verifier {:id (:verifier/id verifier) :version (:verifier/version verifier)
                         :basis (:verifier/basis verifier)}
              :limits {:timeout-ms 15000 :cancel-timeout-ms 3000}
              :world {:isolation :ctx :settlement :discard :setup (evaluation/world-setup-ref setup)}
              :metadata {:case "no-change"}})
        team (roster/make-agent
              (roster/make-roster {:id :simmis-test/checkpoint-prompt})
              {:id :candidate :tools #{:clojure_eval} :sandbox #{}
               :model-policy {:provider :test :model "checkpoint-inputs-test"}
               :program {:kind :llm :max-model-steps 2 :budget-dollars 1.0 :auto-compact? false}})
        definition (experiment/make-experiment
                    {:id :simmis-test/checkpoint-prompt
                     :dataset (experiment/make-dataset {:id :simmis-test/checkpoint-prompt :environments [env]})
                     :candidates [(roster/agent team :candidate)]})
        instructions program/llm-instructions run-inputs program/run-inputs
        calibrate checkers/calibrate
        original-selection (workspace/selected-system-id)]
    (when select-owned? (workspace/select-workspace! (:runtime-system own-descriptor)))
    (seed-guidance! owned "C2-OWNED")
    (try
      (with-redefs [checkers/calibrate (fn [& args]
                                         (swap! observations update :calibrations inc)
                                         (apply calibrate args))
                    providers/ensure-initialized! (constantly nil)
                    chat-agent/messages->api-format (fn [messages _ _] messages)
                    model-chat/chat (fn [_ _]
                                      (swap! observations update :calls inc)
                                      {:content "answer" :tool-calls nil
                                       :usage {:input-tokens 10 :output-tokens 5} :stop-reason :end-turn})
                    program/llm-instructions
                    (fn [agent tools]
                      (let [text (instructions agent tools)]
                        (swap! observations update :prompts conj {:text text :tools (set (keys tools))})
                        text))
                    program/run-inputs
                    (fn [& args]
                      (let [value (apply run-inputs args)]
                        (swap! observations update :input-ids conj (program/inputs-id value))
                        value))]
        (let [result @(experiment/run room team definition {verifier evaluator}
                                      {:world-setups {(evaluation/world-setup-ref setup) setup}
                                       :resume? true :complete-only? true})]
          (is (= 1 (:calls @observations)))
          (is (= 1 (:preparations @observations)))
          (is (= 1 (:calibrations @observations)))
          (is (:source-role? @observations))
          (is (= 2 (count (:prompts @observations))))
          (is (apply = (map :tools (:prompts @observations))))
          (is (contains? (set (map name (:tools (last (:prompts @observations))))) "clojure_eval"))
          (is (str/includes? (:text (last (:prompts @observations))) "C1-OWNED-GUIDE"))
          (is (str/includes? (:text (last (:prompts @observations))) "C1-OWNED-SKILL"))
          (is (not-any? #(str/includes? (:text %) "C2-OWNED") (:prompts @observations)))
          (is (str/includes? (bytes/decode-utf8 (repo/read owned "AGENTS.md")) "C2-OWNED-GUIDE"))
          (is (str/includes? (bytes/decode-utf8 (repo/read attached "AGENTS.md")) "ATTACHED-GUIDE"))
          (is (= (if select-owned? (:runtime-system own-descriptor) original-selection)
                 (workspace/selected-system-id)))
          (if select-owned?
            (do
              (is (every? #(str/includes? (:text %) "C1-OWNED-GUIDE") (:prompts @observations)))
              (is (= 1 (count (set (:input-ids @observations)))))
              (let [actual (get-in result [:attempts 0 :attempt/receipt :attempt/metrics :inputs-id])]
                (is (uuid? actual))
                (is (= (first (:input-ids @observations)) actual)))
              (is (not-any? :unpredicted? (:results result)))
              (is (= 1 (count (get-in result [:scorecard :scorecard/entries])))))
            (do
              (is (str/includes? (:text (first (:prompts @observations))) "ATTACHED-GUIDE"))
              (is (= 1 (get-in result [:scorecard :incomplete :unpredicted])))
              (is (every? :unpredicted? (:results result)))))))
      (finally
        (evaluation/await-cleanups! room 5000)
        (workspace/select-workspace! original-selection)))))

(deftest actual-simmis-wrappers-preserve-prediction-parity-and-detect-selection-drift
  (doseq [select-owned? [true false]]
    (@#'fixture/with-fixture true #(exercise! % select-owned?))))
