(ns is.simm.ops.worker-subscription-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.model.api.codex-subscription :as codex]
            [dvergr.model.api.codex-auth :as auth]
            [dvergr.model.gateway :as gateway]
            [dvergr.model.provider :as p]
            [hato.client :as http]
            [is.simm.ops.worker-inference :as inference]
            [is.simm.ops.worker-inference-test :as fixture]
            [is.simm.ops.worker-subscription :as subscription]
            [jsonista.core :as json])
  (:import [java.io ByteArrayInputStream IOException InputStream]))

(defn- provider
  ([] (provider {}))
  ([opts]
   (codex/create (assoc opts :credentials
                        (gateway/static-credentials :fixture {"Authorization" "Bearer fixture-private"}
                                                    #{"https://chatgpt.com"})))))
(defn- stream [events]
  (ByteArrayInputStream.
   (.getBytes (apply str (map #(str "data: " (json/write-value-as-string %) "\n\n") events)) "UTF-8")))
(def events [{:type "response.output_text.delta" :delta "Calculated 42."}
             {:type "response.completed"
              :response {:id "response-1" :model "gpt-5.6-sol"
                         :usage {:input_tokens 10 :output_tokens 3}}}])

(deftest real-native-provider-gateway-and-sse-with-no-live-network
  (let [requests (atom [])
        infer (binding [gateway/*request-fn*
                        (fn [request] (swap! requests conj request) {:status 200 :body (stream events)})]
                (subscription/inference fixture/config (provider)))
        broker (inference/broker fixture/config infer)
        forwarding (inference/forward-provider fixture/config #(fixture/wire (broker (fixture/wire %))))
        response (p/direct-chat forwarding fixture/messages {:model (:model fixture/config)})
        request (first @requests)]
    (is (= "Calculated 42." (:content response)))
    (is (= :end-turn (:stop-reason response)))
    (is (= 10 (get-in response [:usage :input-tokens])))
    (is (= 1 (count @requests)))
    (is (= subscription/endpoint (:url request)))
    (is (= "Bearer fixture-private" (get-in request [:headers "Authorization"])))
    (is (= java.net.http.HttpClient$Redirect/NEVER (.followRedirects ^java.net.http.HttpClient (:http-client request))))
    (is (not (.contains (pr-str response) "fixture-private")))))

(deftest native-http-refusals-and-network-failures-never-retry
  (doseq [status [302 401 429 500 :io]]
    (let [calls (atom 0)
          infer (binding [gateway/*request-fn*
                          (fn [_] (swap! calls inc)
                            (if (= :io status) (throw (IOException. "private diagnostic"))
                                {:status status :body (stream [])}))]
                  (subscription/inference fixture/config (provider)))
          broker (inference/broker fixture/config infer)]
      (try (broker fixture/request) (is false "must refuse")
           (catch clojure.lang.ExceptionInfo e
             (is (nil? (.getCause e)))
             (is (not (.contains (.getMessage e) "private diagnostic")))))
      (is (= 1 @calls))
      (is (thrown? clojure.lang.ExceptionInfo (broker fixture/request)))
      (is (= 1 @calls)))))

(deftest incomplete-broken-or-malformed-stream-is-not-a-successful-model-turn
  (doseq [body [(stream [{:type "response.output_text.delta" :delta "Incomplete"}])
                (stream [{:type "response.failed" :response {:error {:message "private failure"}}}])
                (stream (conj events {:type "response.output_text.delta" :delta "after completion"}))
                (ByteArrayInputStream. (.getBytes "data: not-json\n\n" "UTF-8"))
                (proxy [InputStream] [] (read [] (throw (IOException. "broken stream"))))]]
    (let [calls (atom 0)
          infer (binding [gateway/*request-fn* (fn [_] (swap! calls inc) {:status 200 :body body})]
                  (subscription/inference fixture/config (provider)))]
      (is (thrown? clojure.lang.ExceptionInfo
                   (infer fixture/messages {:model (:model fixture/config)})))
      (is (= 1 @calls)))))

(deftest exact-origin-path-model-and-input-options-cannot-change
  (let [calls (atom 0)
        infer (binding [gateway/*request-fn* (fn [_] (swap! calls inc) {:status 200 :body (stream events)})]
                (subscription/inference fixture/config (provider {:base-url "https://chatgpt.com/wrong-path"})))]
    (is (thrown? clojure.lang.ExceptionInfo (infer fixture/messages {:model (:model fixture/config)})))
    (is (zero? @calls)))
  (let [calls (atom 0)
        infer (binding [gateway/*request-fn* (fn [_] (swap! calls inc) {:status 200 :body (stream events)})]
                (subscription/inference fixture/config (provider)))]
    (doseq [opts [{:model "different"}
                  {:model (:model fixture/config) :url "https://elsewhere.invalid"}
                  {:model (:model fixture/config) :room-id :native-tools}]]
      (is (thrown? clojure.lang.ExceptionInfo (infer fixture/messages opts))))
    (is (zero? @calls))))

(deftest bounded-stream-rejects-response-before-unbounded-line-parsing
  (let [calls (atom 0)
        infer (binding [gateway/*request-fn*
                        (fn [_] (swap! calls inc)
                          {:status 200 :body (stream [{:type "response.output_text.delta"
                                                       :delta (apply str (repeat 10000 "x"))}])})]
                (subscription/inference fixture/config (provider)))]
    (with-redefs [inference/policy (assoc inference/policy :max-frame-bytes 4096)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (infer fixture/messages {:model (:model fixture/config)}))))
    (is (= 1 @calls))))

(deftest production-credentials-refuse-refresh-before-any-model-dispatch
  (let [original auth/create
        calls (atom 0)
        oauth (atom 0)
        loaded (atom 0)
        fixture-auth {:auth_mode "chatgpt" :last_refresh "1970-01-01T00:00:00Z"
                      :tokens {:access_token "fixture-expired" :refresh_token "fixture-refresh"
                               :account_id "fixture-account"}}
        native (with-redefs [auth/create (fn [opts]
                                           (original (assoc opts :load-auth
                                                            #(do (swap! loaded inc) fixture-auth))))]
                 (subscription/provider))
        infer (binding [gateway/*request-fn* (fn [_] (swap! calls inc)
                                               (throw (AssertionError. "No HTTP is allowed")))]
                (subscription/inference fixture/config native))]
    (with-redefs [http/request (fn [& _] (swap! oauth inc) (throw (ex-info "No OAuth HTTP is allowed" {})))]
      (is (thrown? clojure.lang.ExceptionInfo
                   (infer fixture/messages {:model (:model fixture/config)}))))
    (is (= 1 @loaded))
    (is (zero? @oauth))
    (is (zero? @calls))))
