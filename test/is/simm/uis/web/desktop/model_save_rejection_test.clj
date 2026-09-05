(ns is.simm.uis.web.desktop.model-save-rejection-test
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.agents.room-agents :as room-agents]
            [is.simm.model.model-catalog :as catalog]
            [is.simm.model.model-selection :as selection]
            [is.simm.model.parties :as parties]
            [is.simm.uis.web.desktop.chat-remote :as chat-remote]
            [is.simm.uis.web.desktop.settings-remote :as settings-remote]))

(def ^:private party-id "10000000-0000-0000-0000-000000000001")
(def ^:private unavailable
  {:value "gpt-*-luna"
   :availability :needs-credential
   :availability-reason nil
   :credential-source "OPENAI_API_KEY"
   :available? false})

(defn- rejection [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e
      (ex-data e))))

(deftest preferred-model-save-recomputes-and-rejects-unavailability
  (let [writes (atom [])]
    (with-redefs [catalog/choice (constantly unavailable)
                  parties/update-preferred-model!
                  (fn [& args] (swap! writes conj args))]
      (let [data (rejection
                  #(settings-remote/save-model-server party-id "gpt-*-luna"))]
        (is (= :model-choice-unavailable (:type data)))
        (is (= :needs-credential (:availability data)))
        (is (= "OPENAI_API_KEY" (:credential-source data)))
        (is (empty? @writes) "the rejected preference is not persisted")))))

(deftest agent-model-save-recomputes-and-rejects-unavailability
  (let [writes (atom [])]
    (with-redefs [catalog/choice (constantly unavailable)
                  parties/update-agent! (fn [& args] (swap! writes conj args))
                  parties/get-party (constantly {})
                  room-agents/describe-model-resolution (constantly {})]
      (testing "model, name, and prompt are rejected atomically"
        (let [data (rejection
                    #(chat-remote/update-agent-config-server
                      party-id "Forged rename" "gpt-*-luna" "Forged prompt"))]
          (is (= :model-choice-unavailable (:type data)))
          (is (= :needs-credential (:availability data)))
          (is (empty? @writes)))))))

(deftest clearing-an-override-accepts-an-inherited-soft-fallback
  (let [writes (atom [])]
    (with-redefs [parties/get-party (constantly {})
                  parties/update-agent! (fn [& args] (swap! writes conj args))
                  room-agents/inheritance-choice
                  (constantly {:value "gpt-5.5-luna"
                               :source :owner-preference})
                  room-agents/describe-model-resolution (constantly {})
                  selection/resolve-selection
                  (constantly {:preferred-model "gpt-5.5-luna"
                               :model "gpt-5.6-luna"
                               :fallback? true
                               :fallback-reason :preferred-version-unavailable
                               :available? true})]
      (is (= :ok
             (:status
              (chat-remote/update-agent-config-server
               party-id "" catalog/inherit-choice-value nil))))
      (is (= 1 (count @writes)))
      (is (= {:party/model nil
              :party/model-family nil
              :party/model-version nil
              :party/provider nil}
             (second (first @writes)))))))

(deftest unknown-values-are-rejected-as-not-implemented
  (with-redefs [catalog/choice (constantly nil)]
    (let [data (rejection
                #(settings-remote/save-model-server party-id
                                                    "other-provider/model"))]
      (is (= :model-choice-unavailable (:type data)))
      (is (= :not-implemented (:availability data)))
      (is (= :not-curated (:availability-reason data))))))

(deftest a-curated-row-is-never-reported-as-uncurated
  (testing "a curated row that needs a credential reports no sub-reason"
    (with-redefs [catalog/choice (constantly unavailable)]
      (let [data (rejection
                  #(settings-remote/save-model-server party-id "gpt-*-luna"))]
        (is (= :needs-credential (:availability data)))
        (is (nil? (:availability-reason data))))))
  (testing "a curated row keeps the reason it does carry"
    (with-redefs [catalog/choice
                  (constantly (assoc unavailable
                                     :availability :not-implemented
                                     :availability-reason :registry-missing))]
      (let [data (rejection
                  #(settings-remote/save-model-server party-id "gpt-*-luna"))]
        (is (= :registry-missing (:availability-reason data)))))))
