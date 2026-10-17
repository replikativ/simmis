(ns is.simm.ops.worker-inference-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.agent.program :as program]
            [dvergr.agent.roster :as roster]
            [dvergr.discourse :as discourse]
            [dvergr.model.api.codex-auth :as auth]
            [dvergr.model.provider :as p]
            [dvergr.model.providers :as providers]
            [dvergr.room.store.memory :as memory]
            [is.simm.ops.worker-inference :as inference]
            [jsonista.core :as json]
            [org.replikativ.spindel.engine.core :as ec]))

(def config {:adapter "codex" :version "fixture-1" :model "codex-subscription-sol"
             :policy "local-trusted-v1"})
(def messages [{:role "user" :content "Read selected files and prepare an output."}])
(def answer {:content "Done" :tool-calls nil :usage {:input-tokens 4 :output-tokens 2}
             :stop-reason :end-turn :model "gpt-5.6-sol" :id "response-1"})
(def request {:version 1 :sequence 1 :messages messages :options {}})
(defn wire [value]
  (json/read-value (json/write-value-as-bytes value) json/keyword-keys-object-mapper))

(deftest actual-native-formatting-without-auth-or-native-http
  (with-redefs [auth/create (fn [& _] (throw (Exception. "Must not read host auth")))]
    (let [called (atom [])
          broker (inference/broker config (fn [ms opts] (swap! called conj [ms opts]) answer))
          provider (inference/forward-provider config #(wire (broker (wire %))))
          formatted (p/format-messages provider [{:message/role :user :message/content "Hello"}] nil)
          chunks (atom [])]
      (is (= [{:role "user" :content "Hello"}] formatted))
      (is (= answer (p/direct-chat provider formatted {:model (:model config) :provider :ignored
                                                       :room-id :never-forwarded :on-text #(swap! chunks conj %)})))
      (is (= [[formatted {:model (:model config)}]] @called))
      (is (= ["Done"] @chunks))
      (is (thrown? clojure.lang.ExceptionInfo (p/build-request provider formatted {}))))))

(deftest tools-remain-data-and-model-is-locally-pinned
  (let [calls (atom [])
        tools [{:name "clojure_eval" :description "Evaluate inside the offline executor"
                :parameters {:type "object" :properties {:code {:type "string"}}}}]
        response (assoc answer :content "I will read the file" :stop-reason :tool-use
                        :tool-calls [{:id "call-1" :name "clojure_eval" :input {:code "(read-file \"a\")"}}])
        broker (inference/broker config (fn [ms opts] (swap! calls conj [ms opts]) response))
        provider (inference/forward-provider config broker)]
    (is (= response (p/direct-chat provider messages {:model (:model config) :system "Pinned task"
                                                      :tools tools :room-id :not-an-mcp-route})))
    (is (= [[messages {:model (:model config) :system "Pinned task" :tools tools}]] @calls))
    (is (= [{:type "function_call_output" :call_id "call-1" :output "file bytes"}]
           (p/format-messages provider [{:message/role :tool-result :message/tool-use-id "call-1"
                                         :message/content "file bytes"}] nil)))))

(deftest replay-malformed-request-and-escalation-close-session-before-inference
  (doseq [bad [(assoc request :url "https://outside.invalid")
               (assoc request :sequence 2)
               (assoc-in request [:options :model] "another-model")
               (assoc-in request [:options :provider] :claude-code)
               (assoc-in request [:options :room-id] :native-mcp)
               (assoc-in request [:options :on-text] identity)
               (assoc-in request [:options :tools]
                         [{:name "x" :parameters {:type "object"} :input_schema {:type "string"}}])
               (assoc request :messages [{:role "user" :content [{:type "input_image" :image_url "file:///private"}]}])
               (assoc request :messages [{:type "message" :role "user"
                                          :content [{:type "input_image" :text "not text"}]}])
               (assoc request :messages [{:role "user" :content "text" :credentials "no"}])]]
    (let [calls (atom 0)
          broker (inference/broker config (fn [& _] (swap! calls inc) answer))]
      (is (thrown? clojure.lang.ExceptionInfo (broker bad)))
      (is (thrown? clojure.lang.ExceptionInfo (broker request)))
      (is (zero? @calls))))
  (let [calls (atom 0)
        broker (inference/broker config (fn [& _] (swap! calls inc) answer))]
    (broker request)
    (is (thrown? clojure.lang.ExceptionInfo (broker request)))
    (is (thrown? clojure.lang.ExceptionInfo (broker (assoc request :sequence 2))))
    (is (= 1 @calls))))

(deftest ambiguous-provider-failure-is-sanitized-and-never-reissued
  (let [calls (atom 0)
        broker (inference/broker config (fn [& _] (swap! calls inc)
                                          (throw (ex-info "private-token" {:secret "hidden"}))))]
    (try (broker request)
         (is false "must throw")
         (catch clojure.lang.ExceptionInfo e
           (is (= {:type :is.simm.ops.worker-inference/refused} (ex-data e)))
           (is (nil? (.getCause e)))
           (is (not (.contains (.getMessage e) "private-token")))))
    (is (thrown? clojure.lang.ExceptionInfo (broker request)))
    (is (= 1 @calls))))

(deftest malformed-or-oversized-responses-close-both-ends
  (doseq [bad [(assoc answer :url "file:///private")
               (assoc answer :stop-reason :unknown)
               (assoc answer :usage {:input-tokens -1})
               (assoc answer :tool-calls [{:id "1" :name "x" :input (fn [])}])
               (assoc answer :content (apply str (repeat 1048577 "x")))]]
    (let [calls (atom 0)
          broker (inference/broker config (fn [& _] (swap! calls inc) bad))
          provider (inference/forward-provider config broker)]
      (is (thrown? clojure.lang.ExceptionInfo (p/direct-chat provider messages {:model (:model config)})))
      (is (thrown? clojure.lang.ExceptionInfo (p/direct-chat provider messages {:model (:model config)})))
      (is (= 1 @calls)))))

(deftest budget-is-independent-of-candidate-options-and-no-calls-after-cancel
  (let [calls (atom 0)
        broker (inference/broker config (fn [& _] (swap! calls inc) answer))]
    (dotimes [i 32] (broker (assoc request :sequence (inc i))))
    (is (thrown? clojure.lang.ExceptionInfo (broker (assoc request :sequence 33))))
    (is (= 32 @calls)))
  (doseq [opts [{:model "different"} {:model (:model config) :cancel? (constantly true)}]]
    (let [calls (atom 0)
          provider (inference/forward-provider config (fn [& _] (swap! calls inc) nil))]
      (is (thrown? clojure.lang.ExceptionInfo (p/direct-chat provider messages opts)))
      (is (thrown? clojure.lang.ExceptionInfo (p/direct-chat provider messages {:model (:model config)})))
      (is (zero? @calls)))))

(deftest reply-correlation-and-session-byte-bounds
  (doseq [reply [{:version 1 :sequence 2 :response answer}
                 {:version 1 :sequence 1 :response answer :extra true}]]
    (let [provider (inference/forward-provider config (constantly reply))]
      (is (thrown? clojure.lang.ExceptionInfo (p/direct-chat provider messages {:model (:model config)})))))
  (with-redefs [inference/policy (assoc inference/policy :max-session-bytes 1)]
    (let [calls (atom 0) broker (inference/broker config (fn [& _] (swap! calls inc) answer))]
      (is (thrown? clojure.lang.ExceptionInfo (broker request)))
      (is (zero? @calls)))))

(deftest repeated-large-values-are-refused-before-json-allocation
  (let [calls (atom 0)
        serialized? (atom false)
        broker (inference/broker config (fn [& _] (swap! calls inc) answer))
        shared (apply str (repeat 100 "x"))
        expanded (assoc-in request [:options :tools]
                           [{:name "x" :parameters {:items (vec (repeat 100 shared))}}])]
    (with-redefs [inference/policy (assoc inference/policy :max-frame-bytes 2000)
                  json/write-value-as-bytes (fn [& _] (reset! serialized? true)
                                              (throw (AssertionError. "Must refuse before serialization")))]
      (is (= :is.simm.ops.worker-inference/refused
             (try (broker expanded) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))
    (is (false? @serialized?))
    (is (zero? @calls))))

(deftest colliding-json-field-names-cannot-change-wire-intent
  (let [calls (atom 0)
        broker (inference/broker config (fn [& _] (swap! calls inc) answer))]
    (is (thrown? clojure.lang.ExceptionInfo
                 (broker (assoc-in request [:options :tools]
                                   [{:name "x" :parameters {:field 1 "field" 2}}]))))
    (is (zero? @calls))))

(deftest existing-dvergr-run-really-executes-a-tool-between-forwarded-model-turns
  (let [room (discourse/make-room {:id (keyword (str "worker-inference-" (random-uuid)))
                                   :store (memory/make)})
        calls (atom [])
        broker (inference/broker
                config
                (fn [ms _]
                  (swap! calls conj ms)
                  (if (= 1 (count @calls))
                    (assoc answer :content "" :stop-reason :tool-use
                           :tool-calls [{:id "arithmetic" :name "clojure_eval"
                                         :input {:code "(+ 20 22)"}}])
                    (assoc answer :content "The result is 42."))))
        provider (inference/forward-provider config #(wire (broker (wire %))))
        team (roster/make-agent (roster/make-roster {:id :worker-inference-test})
                                {:id :worker :prompt "Calculate using the supplied tool."
                                 :tools #{:clojure_eval}
                                 :model-policy {:provider :codex-subscription :model (:model config)}
                                 :program {:kind :llm :max-model-steps 3 :auto-compact? false}})]
    (try
      (with-redefs [providers/ensure-initialized! (constantly nil)
                    providers/providers (atom {:codex-subscription provider})]
        (binding [ec/*execution-context* (:ctx room)]
          (let [handle (program/hire! room team :worker {:task "What is 20 + 22?"})
                result (deref handle 10000 ::timeout)]
            (when (= ::timeout result) (program/cancel! handle) (deref handle 5000 ::timeout))
            (is (= :completed (:run/status result)))
            (is (= "The result is 42." (:run/value result)))
            (is (= 2 (count @calls)))
            (is (some #(and (= "function_call_output" (:type %))
                            (= "arithmetic" (:call_id %))
                            (re-find #"42" (:output %))) (second @calls)))
            (is (= :completed (:run/status (program/observe room handle)))))))
      (finally (discourse/close-room! room)))))
