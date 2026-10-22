(ns is.simm.ops.scheduled-wiki-allowance-probe
  "Opt-in native dispatcher exhaustion diagnostic; no real inference."
  (:require [is.simm.ops.scheduled-wiki-restart-probe :as restart]
            [datahike.api :as d]
            [dvergr.model.api.codex-subscription :as codex]
            [dvergr.model.gateway :as gateway]
            [dvergr.model.providers :as providers]
            [dvergr.resource :as resource]
            [dvergr.runtime.clock :as clock]
            [is.simm.model.system-db :as system-db]
            [is.simm.model.workflow-schedules :as model]
            [is.simm.ops.workflow-runs :as runs]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflow-schedules :as schedules]
            [is.simm.runtimes.context :as context]))

(defn- check! [condition label]
  (when-not condition (throw (ex-info "Wiki allowance diagnostic failed" {:check label}))))

(defn- quiet-pass! [passes due]
  ;; Correctly paused/held activations create no new occurrence to listen for.
  ;; Join the original dispatcher pass instead; never infer it from a delay.
  (let [receipt (promise)]
    (swap! passes assoc due receipt)
    (try
      (context/with-server-context (clock/set-now! due))
      (let [[status error] (deref receipt 120000 [:timeout])]
        (check! (not= :timeout status) :dispatcher-pass-return)
        (when (= :error status) (throw error)))
      (finally (swap! passes dissoc due)))))

(defn- request-count [conn room]
  (d/q '[:find (count ?r) . :in $ ?room :where [?r :work.request/room ?room]] @conn room))

(defn- exercise! [conn calls forbidden? entered passes balance-reads]
  ;; Reuse the successful native fixture unchanged: one provision of THREE
  ;; dispatches, first settled no-op + unchanged skip, canonical starter files.
  (let [initial (#'restart/stage-a! conn calls)
        {:keys [party room activation]} initial
        rich (#'restart/binding-for! party room)]
    (doseq [text ["Synthetic retained evidence K2." "Synthetic retained evidence K3."]]
      (#'restart/write-evidence! rich text)
      (#'restart/run-once! conn party room activation))
    (check! (= 3 @calls) :three-admitted-calls)
    (let [wallet (resource/balance (:room rich))
          requests (request-count conn room)
          cursor (:schedule.activation/last-processed-input (model/activation conn activation))]
      (check! (= 0M (get wallet resource/model-dispatches 0M)) :exhausted-dispatch-wallet)
      (check! (= 3 requests) :three-canonical-requests)
      (#'restart/write-evidence! rich "Synthetic retained evidence K4 cannot acquire allowance.")
      (reset! forbidden? true)
      (quiet-pass! passes (#'restart/next-due conn activation))
      (check! (some #(and (= (:id (:room rich)) (:room %))
                          (= wallet (:resources %))) @balance-reads)
              :dispatcher-observed-exact-exhausted-wallet)
      (let [held (model/activation conn activation)
            occurrence (model/occurrence conn (:schedule.activation/last-occurrence held))]
        (check! (false? (:schedule.activation/enabled? held)) :visibly-held)
        (check! (keyword? (:schedule.activation/reason held)) :retained-hold-reason)
        (check! (nil? (:schedule.occurrence/request occurrence)) :no-unfunded-request-link)
        (check! (= cursor (:schedule.activation/last-processed-input held)) :unprocessed-input-not-consumed)
        (check! (= requests (request-count conn room)) :no-new-canonical-request)
        (check! (= wallet (resource/balance (:room rich))) :no-automatic-mint)
        (check! (empty? @entered) :no-provider-preparation-after-exhaustion)
        (check! (= 3 @calls) :no-unfunded-provider-call)
        (quiet-pass! passes (java.util.Date. (+ 60000 (.getTime ^java.util.Date (#'restart/next-due conn activation)))))
        (check! (= held (model/activation conn activation)) :held-not-retried)
        (check! (= requests (request-count conn room)) :no-retry-request)
        (check! (= wallet (resource/balance (:room rich))) :held-wallet-unchanged)
        (check! (and (empty? @entered) (= 3 @calls)) :no-held-preparation-or-call)
        {:version 1 :room room :activation activation :requests requests :scripted-calls @calls
         :wallet wallet :hold-reason (:schedule.activation/reason held)
         :budget-reads @balance-reads
         :occurrence-state (:schedule.occurrence/state occurrence)}))))

(defn -main [checkpoint]
  (let [target (#'restart/private-artifact! checkpoint)
        _ (check! (not (.exists target)) :no-overwrite)
        calls (atom 0) forbidden? (atom false) entered (atom {}) passes (atom {})
        balance-reads (atom []) dispatcher-thread (atom nil)
        provider (codex/create {:credentials (gateway/static-credentials :fixture {} #{"https://chatgpt.com"})})]
    ;; Permanent process-owned I/O fixtures also remain installed if cleanup
    ;; fails. This private diagnostic must never be run inside a live REPL.
    (reset! providers/providers {:codex-subscription provider})
    (alter-var-root #'gateway/*request-fn*
                    (constantly (fn [& _] (swap! calls inc) (#'restart/response))))
    (doseq [[v label] [[#'runs/prepare :prepare] [#'resource/mint! :mint]
                      [#'work/submit-prepared! :submit]]]
      (alter-var-root v
                      (fn [original]
                        (fn [& args]
                          (when @forbidden?
                            (swap! entered update label (fnil inc 0))
                            (throw (AssertionError. "Exhausted activation acquired fresh authority or work")))
                          (apply original args)))))
    (alter-var-root #'resource/balance
                    (fn [original]
                      (fn [& args]
                        (let [value (apply original args)]
                          (when (and @forbidden? (= 1 (count args))
                                     (identical? (Thread/currentThread) @dispatcher-thread))
                            (swap! balance-reads conj {:room (:id (first args)) :resources value}))
                          value))))
    (alter-var-root #'schedules/tick-owned!
                    (fn [original]
                      (fn [host now]
                        (let [receipt (get @passes now)]
                          (reset! dispatcher-thread (Thread/currentThread))
                          (try
                            (let [value (original host now)]
                              (when receipt (deliver receipt [:ok])) value)
                            (catch Throwable error
                              (when receipt (deliver receipt [:error error]))
                              (throw error))
                            (finally (reset! dispatcher-thread nil)))))))
    (binding [restart/*pass-receipts* passes]
      (try
        (let [result (try
                       (exercise! (#'restart/boot!) calls forbidden? entered passes balance-reads)
                       (finally (#'restart/stop! (system-db/get-conn))))]
          (spit target (pr-str result))
          (println :allowance-diagnostic-completed (str target)))
        (finally (shutdown-agents))))))
