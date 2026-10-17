(ns is.simm.ops.worker-inference
  "Host-only model-inference boundary for a credential-free Dvergr executor.

   This is not a transport or another agent loop. The embedding runner owns
   private pipes, physical containment and cancellation. A broker receives a
   locally selected inference function, never a credential or URL from a task.
   Instances are single-session external I/O capabilities, not forkable state."
  (:require [dvergr.model.api.codex-subscription :as codex]
            [dvergr.model.provider :as p]
            [is.simm.ops.worker-files :as files]
            [jsonista.core :as json]
            [malli.core :as m]))

(def policy {:version 1 :max-calls 32 :max-frame-bytes 1048576
             :max-session-bytes 16777216 :max-depth 32 :max-nodes 50000})

(defn- refuse! []
  ;; Never include task content, a provider exception or credentials in errors.
  (throw (ex-info "Local worker inference boundary refused the exchange"
                  {:type ::refused})))

(defn- closed! [value fields]
  (when-not (and (map? value) (= (set fields) (set (keys value)))) (refuse!)))

(defn- text! [value]
  (when-not (and (string? value) (<= (count value) (:max-frame-bytes policy))) (refuse!)))

(defn- data! [value]
  (let [remaining (volatile! (:max-nodes policy))
        bytes (volatile! (:max-frame-bytes policy))]
    (letfn [(charge! [size]
              (when (neg? (vswap! bytes - size)) (refuse!)))
            (visit [v depth]
              (when (or (> depth (:max-depth policy)) (neg? (vswap! remaining dec))) (refuse!))
              (cond
                (or (nil? v) (boolean? v)) (charge! 5)
                ;; A JSON UTF-16 code unit needs at most six output bytes
                ;; (\\uXXXX). Charge before serialization, including repeated
                ;; references, so a tiny shared string cannot expand unboundedly.
                (string? v) (do (text! v) (charge! (+ 2 (* 6 (count v)))))
                (keyword? v) (charge! (+ 8 (* 6 (+ (count (name v)) (count (namespace v))))))
                (integer? v) (do (when-not (<= Long/MIN_VALUE v Long/MAX_VALUE) (refuse!)) (charge! 32))
                (float? v) (do (when-not (Double/isFinite (double v)) (refuse!)) (charge! 32))
                (vector? v) (do (charge! (+ 2 (count v)))
                                (doseq [item v] (visit item (inc depth))))
                (map? v) (do (charge! (+ 2 (* 2 (count v))))
                             (reduce (fn [seen [k item]]
                                       (when-not (or (string? k) (keyword? k)) (refuse!))
                                       (visit k (inc depth))
                                       (let [wire-key (if (keyword? k) (subs (str k) 1) k)]
                                         (when (contains? seen wire-key) (refuse!))
                                         (visit item (inc depth))
                                         (conj seen wire-key))) #{} v))
                :else (refuse!)))]
      (visit value 0)))
  value)

(defn- encoded-size! [value]
  (data! value)
  (let [size (alength (json/write-value-as-bytes value))]
    (when (> size (:max-frame-bytes policy)) (refuse!))
    size))

(defn- message! [message]
  (if-let [type (:type message)]
    (case type
      "function_call" (do (closed! message [:type :call_id :name :arguments])
                          (doseq [k [:call_id :name :arguments]] (text! (get message k))))
      "function_call_output" (do (closed! message [:type :call_id :output])
                                 (doseq [k [:call_id :output]] (text! (get message k))))
      "message" (do (closed! message [:type :role :content])
                    (when-not (contains? #{"system" "developer" "user" "assistant"} (:role message)) (refuse!))
                    (when-not (vector? (:content message)) (refuse!))
                    (doseq [part (:content message)]
                      (closed! part [:type :text])
                      (when-not (contains? #{"input_text" "output_text"} (:type part)) (refuse!))
                      (text! (:text part))))
      (refuse!))
    (do (closed! message [:role :content])
        (when-not (contains? #{"system" "developer" "user" "assistant"} (:role message)) (refuse!))
        (text! (:content message)))))

(defn- request! [request]
  (encoded-size! request)
  (closed! request [:version :sequence :messages :options])
  (when-not (and (= 1 (:version request)) (integer? (:sequence request))
                 (<= 1 (:sequence request) (:max-calls policy))
                 (vector? (:messages request)) (<= 1 (count (:messages request)) 512)) (refuse!))
  (doseq [message (:messages request)] (message! message))
  (let [opts (:options request)]
    (when-not (and (map? opts) (every? #{:system :tools} (keys opts))) (refuse!))
    (when (contains? opts :system) (text! (:system opts)))
    (when (contains? opts :tools)
      (when-not (and (vector? (:tools opts)) (<= (count (:tools opts)) 64)) (refuse!))
      (doseq [tool (:tools opts)]
        (when-not (and (map? tool) (contains? tool :name)
                       (every? #{:name :description :parameters :input_schema} (keys tool))) (refuse!))
        (text! (:name tool))
        (when (contains? tool :description) (text! (:description tool)))
        (when (contains? tool :parameters)
          (when-not (map? (:parameters tool)) (refuse!)))
        ;; Dvergr's real tool-definitions includes both API spellings. They
        ;; must identify the same schema, never conflicting interpretations.
        (when (contains? tool :input_schema)
          (when-not (and (map? (:input_schema tool))
                         (= (:input_schema tool) (:parameters tool))) (refuse!))))))
  request)

(defn- response! [response]
  (encoded-size! response)
  (when-not (and (map? response) (contains? response :content)
                 (every? #{:content :reasoning :tool-calls :usage :stop-reason :model :id} (keys response))) (refuse!))
  (text! (:content response))
  (doseq [k [:reasoning :model :id]]
    (when-let [v (get response k)] (text! v)))
  (when-not (contains? #{:end-turn :tool-use :max-tokens "end-turn" "tool-use" "max-tokens"} (:stop-reason response)) (refuse!))
  (when-let [calls (:tool-calls response)]
    (when-not (and (vector? calls) (<= (count calls) 64)) (refuse!))
    (doseq [call calls]
      (closed! call [:id :name :input])
      (text! (:id call))
      (text! (:name call))
      (when-not (map? (:input call)) (refuse!))))
  (when-let [usage (:usage response)]
    (when-not (and (map? usage)
                   (every? #{:input-tokens :output-tokens :cache-read-tokens :cache-write-tokens
                             :reasoning-output-tokens} (keys usage))
                   (every? #(and (integer? %) (<= 0 % Long/MAX_VALUE)) (vals usage))) (refuse!)))
  response)

(defn- config! [config]
  (when-not (and (m/validate files/config-schema config) (= "codex" (:adapter config))) (refuse!))
  config)

(defn broker
  "Return a serial, non-replaying inference exchange function.

   infer! is trusted local code taking [messages options]; it MUST NOT execute
   tools. Model is pinned here. The runner must enforce a total deadline and
   physically stop this capability with the executor. An ambiguous inference
   permanently closes this session; no automatic paid retry is authorized."
  [config infer!]
  (config! config)
  (when-not (ifn? infer!) (refuse!))
  (let [state (atom {:sequence 0 :bytes 0 :closed? false})]
    (fn [request]
      (locking state
        (try
          (when (:closed? @state) (refuse!))
          (request! request)
          (let [size (encoded-size! request)
                {:keys [sequence bytes]} @state]
            (when-not (and (= (inc sequence) (:sequence request))
                           (<= (+ bytes size) (:max-session-bytes policy))) (refuse!))
            ;; Reserve BEFORE inference. Neither a lost reply nor a malformed
            ;; response permits the same sequence to invoke the provider again.
            (reset! state {:sequence (inc sequence) :bytes (+ bytes size) :closed? true})
            (let [response (response! (infer! (:messages request)
                                              (assoc (:options request) :model (:model config))))
                  total (+ bytes size (encoded-size! response))]
              (when (> total (:max-session-bytes policy)) (refuse!))
              (reset! state {:sequence (inc sequence) :bytes total :closed? false})
              {:version 1 :sequence (inc sequence) :response response}))
          (catch Throwable _
            (swap! state assoc :closed? true)
            (refuse!)))))))

(defrecord ForwardProvider [config exchange! formatter state]
  p/LLMProvider
  (provider-id [_] :simmis-worker-inference)
  (api-type [_] :simmis-worker-inference)
  (build-request [_ _ _] (refuse!))
  (create-accumulator [_ _] (refuse!))
  (accumulate-event [_ _ _ _ _] (refuse!))
  (extract-response [_ _] (refuse!))
  p/MessageFormatter
  (format-messages [_ messages model] (p/format-messages formatter messages model))
  p/ToolFormatter
  (format-tools [_ tools] (p/format-tools formatter tools))
  p/DirectChat
  (direct-chat [_ messages opts]
    (locking state
      (try
        (when (or (:closed? @state) (not= (:model config) (:model opts))) (refuse!))
        ;; Callbacks remain LOCAL. Room ids, provider, credentials, native tools
        ;; and arbitrary model options cannot cross the inference boundary.
        (when (and (:cancel? opts) ((:cancel? opts))) (refuse!))
        (let [sequence (inc (:sequence @state))
              request (request! {:version 1 :sequence sequence :messages messages
                                 :options (select-keys opts [:system :tools])})]
          (reset! state {:sequence sequence :closed? true})
          (let [reply (exchange! request)]
            (closed! reply [:version :sequence :response])
            (when-not (and (= 1 (:version reply)) (= sequence (:sequence reply))) (refuse!))
            (let [response (response! (:response reply))
                  response (update response :stop-reason keyword)]
              (when (and (:cancel? opts) ((:cancel? opts))) (refuse!))
              (reset! state {:sequence sequence :closed? false})
              (when-let [on-text (:on-text opts)] (on-text (:content response)))
              response)))
        (catch Throwable _
          (swap! state assoc :closed? true)
          (refuse!))))))

(defn forward-provider
  "Credentialless provider for Dvergr's existing model/tool loop.

   No auth file is read: the native provider is used ONLY for pure message/tool
   formatting with an inert credential sentinel. Its HTTP entry is inaccessible
   through this wrapper. This DirectChat adapter is not dispatch-ledger priced."
  [config exchange!]
  (config! config)
  (when-not (ifn? exchange!) (refuse!))
  (->ForwardProvider config exchange! (codex/create {:credentials ::unavailable})
                     (atom {:sequence 0 :closed? false})))
