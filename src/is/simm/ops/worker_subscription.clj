(ns is.simm.ops.worker-subscription
  "Trusted, single-dispatch Codex-subscription inference for a local broker.
   No agent loop or tools execute here. The owning runner must supply physical
   containment and a total deadline; the stream's idle timer is not that bound."
  (:require [dvergr.model.chat :as chat]
            [dvergr.model.api.codex-auth :as auth]
            [dvergr.model.api.codex-subscription :as codex]
            [dvergr.model.gateway :as gateway]
            [dvergr.model.provider :as p]
            [dvergr.model.registry :as registry]
            [hato.client :as http]
            [is.simm.ops.worker-files :as files]
            [is.simm.ops.worker-inference :as inference]
            [malli.core :as m])
  (:import [java.io FilterInputStream InputStream]
           [java.nio.charset StandardCharsets]))

(def endpoint "https://chatgpt.com/backend-api/codex/responses")

(defn- refuse! []
  (throw (ex-info "Local subscription inference refused or interrupted"
                  {:type ::refused})))

(defn provider
  "Create the broker's native provider using the host's existing file login.
   Never refresh or rewrite that login during work. Expiring credentials fail
   closed; renew them through the normal trusted login flow outside this Run."
  []
  (codex/create {:credentials (auth/create {:refresh-request (fn [_] (refuse!))
                                            :save-auth (fn [_] (refuse!))})}))

(defn- bounded-stream [^InputStream stream]
  (let [used (volatile! 0)
        charge! (fn [n]
                  (when (and (pos? n)
                             (> (vswap! used + n) (:max-frame-bytes inference/policy)))
                    (refuse!)))]
    (proxy [FilterInputStream] [stream]
      (read
        ([] (let [n (.read stream)] (when (not= -1 n) (charge! 1)) n))
        ([bytes] (let [n (.read stream ^bytes bytes)] (charge! n) n))
        ([bytes offset length]
         (let [n (.read stream ^bytes bytes (int offset) (int length))] (charge! n) n))))))

(defn inference
  "Build a broker inference function using an EXISTING trusted native provider.

   The provider/credentials and HTTP implementation are selected by local code,
   not the executor. Each invocation permits at most one guarded model POST,
   disables redirects and refuses every model HTTP error without retries.
   Use this namespace's provider constructor to also refuse token-refresh I/O;
   an arbitrary injected Credentials implementation has its own auth behavior.
   Stream errors propagate without restarting a partially consumed response."
  [config provider]
  (when-not (and (m/validate files/config-schema config) (= "codex" (:adapter config))
                 (= :codex-subscription (p/provider-id provider))
                 (= :openai-responses (p/api-type provider))) (refuse!))
  (let [model-def (registry/get-model! (:model config))
        request! gateway/*request-fn*
        client (http/build-http-client {:redirect-policy :never :connect-timeout 5000})]
    (when-not (= :codex-subscription (:provider model-def)) (refuse!))
    (fn [messages opts]
      (when-not (and (= (:model config) (:model opts))
                     (every? #{:model :system :tools} (keys opts))) (refuse!))
      (let [dispatched? (atom false)
            guarded-request
            (fn [request]
              (when-not (and (= endpoint (:url request)) (= :post (:method request))
                             (string? (:body request))
                             (<= (alength (.getBytes ^String (:body request) StandardCharsets/UTF_8))
                                 (:max-frame-bytes inference/policy))
                             (compare-and-set! dispatched? false true)) (refuse!))
              (try
                (let [response (request! (assoc request :http-client client))
                      body (:body response)]
                  (if (and (= 200 (:status response)) (instance? InputStream body))
                    (assoc response :body (bounded-stream body))
                    (do (when (instance? InputStream body) (.close ^InputStream body)) (refuse!))))
                (catch Throwable _ (refuse!))))]
        (try
          (binding [gateway/*request-fn* guarded-request]
            (let [{:keys [events close!]} (chat/stream-chat provider model-def messages opts)]
              (try
                (let [{:keys [state terminal?]}
                      (reduce
                       (fn [{:keys [state terminal? count]} event]
                         (when (or terminal? (>= count 8192) (:parse-error event)
                                   (= "error" (:type event))) (refuse!))
                         {:state (p/accumulate-event provider state (:type event) event model-def)
                          :terminal? (contains? #{"response.completed" "response.incomplete"} (:type event))
                          :count (inc count)})
                       {:state (p/create-accumulator provider model-def) :terminal? false :count 0}
                       events)]
                  (when-not terminal? (refuse!))
                  (p/extract-response provider state))
                (finally (close!)))))
          (catch Throwable _ (refuse!)))))))
