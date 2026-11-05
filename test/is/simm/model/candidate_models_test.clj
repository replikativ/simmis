(ns is.simm.model.candidate-models-test
  "A reasoning effort is part of the model choice: `<id>@<effort>` must be
   listed, admitted and benchmarked as its own candidate on every surface."
  (:require [clojure.test :refer [deftest is testing]]
            [dvergr.agent.roster :as roster]
            [dvergr.agent.workflow :as workflow]
            [dvergr.model.registry :as registry]
            [is.simm.agents.workflows :as agent-workflows]
            [is.simm.model.candidate-models :as candidate-models]
            [is.simm.model.model-catalog :as model-catalog]
            [is.simm.ops.owned-workflow-host :as policy]
            [is.simm.ops.work :as work]
            [is.simm.ops.work-api :as api]
            [is.simm.runtimes.mcp :as mcp]))

(def ^:private luna "codex-subscription-luna-6")

(defn- listed [id]
  (some #(when (= id (:id %)) %) (:models (candidate-models/options))))

(deftest listing-states-the-efforts-each-model-accepts
  (let [{:keys [models effort-syntax]} (candidate-models/options)
        row (listed luna)]
    (is (some? row) "The Codex subscription model is offered")
    (is (= (vec (:reasoning-efforts (registry/get-model luna))) (:reasoning-efforts row)))
    (is (every? (set (:reasoning-efforts row)) ["low" "high"]))
    (is (re-find #"<id>@<effort>" effort-syntax))
    (testing "derived effort candidates are a syntax, not extra registry rows"
      (registry/get-model (str luna "@high"))
      (is (not-any? #(re-find #"@" (:id %)) models)))
    (testing "only providers whose dispatches are accounted are offered"
      (is (every? #(contains? candidate-models/dispatch-accounted-providers (:provider %)) models))
      (is (identical? candidate-models/dispatch-accounted-providers work/dispatch-accounted-providers)))))

(deftest an-effort-is-admitted-as-its-own-candidate-and-an-unknown-one-is-refused
  (is (= {:status :eligible :policy {:provider :codex-subscription :model (str luna "@low")}}
         (work/model-eligibility (str luna "@low"))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unsupported model reasoning effort"
                        (work/model-eligibility (str luna "@turbo"))))
  (let [{:keys [team ids]} (workflow/candidates {:models [(str luna "@low") (str luna "@high")]
                                                 :profile "developer"})]
    (is (= 2 (count (set ids))) "Two efforts of one model are two benchmark candidates")
    (is (= [(str luna "@low") (str luna "@high")]
           (mapv #(get-in (roster/agent team %) [:agent/model-policy :model]) ids)))))

(deftest every-surface-that-chooses-a-model-can-list-them
  (testing "MCP: a room-scoped read on every credential ceiling, admitted on the owned host"
    (is (= :read (get-in api/specifications [:simmis/models :kind])))
    (is (= :read (get-in api/specifications [:simmis/models :authz :action])))
    (is (= :read (get policy/owned-actions :simmis/models)))
    (doseq [tools [mcp/read-tools mcp/work-tools mcp/worker-control-tools]]
      (is (contains? tools "simmis_models")))
    (is (= (candidate-models/options)
           ((get-in api/specifications [:simmis/models :impl]) {} {:room "any-room"})))
    (is (thrown? clojure.lang.ExceptionInfo
                 ((get-in api/specifications [:simmis/models :impl]) {} {:room "r" :provider "openai"}))))
  (testing "the descriptions that take model ids point at the listing and the effort form"
    (doseq [op [:simmis/benchmark :simmis/workflow-run]]
      (is (re-find #"simmis_models" (get-in api/specifications [op :doc])))
      (is (re-find #"@<effort>" (get-in api/specifications [op :doc])))))
  (testing "agents: workflow/models returns the same listing and is documented"
    (is (= (candidate-models/options) (agent-workflows/models)))
    (is (contains? agent-workflows/docs 'models))))

(deftest an-agent-may-choose-a-curated-model-at-a-declared-effort
  (let [row (model-catalog/choice (str luna "@high"))]
    (is (= (str luna "@high") (:value row) (:resolves row)))
    (is (= :model (:kind row)))
    (is (= (:available? (model-catalog/choice luna)) (:available? row))
        "An effort is as available as its curated base model"))
  (is (nil? (model-catalog/choice (str luna "@turbo"))) "Undeclared effort")
  (is (nil? (model-catalog/choice "gpt-*-luna@high")) "Families take no effort")
  (is (nil? (model-catalog/choice "not-curated@high")) "Only curated base models"))
