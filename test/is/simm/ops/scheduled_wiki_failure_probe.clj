(ns is.simm.ops.scheduled-wiki-failure-probe
  "Opt-in failed candidate diagnostic. Job completion is not candidate success."
  (:require [is.simm.ops.scheduled-wiki-restart-probe :as restart]
            [is.simm.ops.scheduled-wiki-allowance-probe :as allowance]
            [clojure.string :as str]
            [dvergr.agent.attempt :as attempt]
            [dvergr.agent.episode :as episode]
            [dvergr.agent.run :as run]
            [dvergr.jobs :as jobs]
            [dvergr.model.api.codex-subscription :as codex]
            [dvergr.model.gateway :as gateway]
            [dvergr.model.providers :as providers]
            [dvergr.resource :as resource]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.system-db :as system-db]
            [is.simm.model.workflow-schedules :as model]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflow-runs :as runs]
            [is.simm.ops.workflow-schedules :as schedules]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- check! [condition label]
  (when-not condition (throw (ex-info "Wiki failure diagnostic failed" {:check label}))))

(defn- failed-response []
  {:status 200
   :body (java.io.ByteArrayInputStream.
           (.getBytes "data: {\"type\":\"response.failed\",\"response\":{\"error\":{\"message\":\"Synthetic failure after native model admission\"}}}\n\n" "UTF-8"))})

(defn- exercise! [conn calls mode forbidden? entered passes]
  (let [initial (#'restart/stage-a! conn calls)
        {:keys [party room activation]} initial
        baseline (:first initial)
        rich (#'restart/binding-for! party room)
        native-room (:room rich)]
    (#'restart/write-evidence! rich "Synthetic K2 must not become processed after model failure.")
    (reset! mode :failed)
    (let [occurrence (#'restart/tick! conn activation (#'restart/next-due conn activation)
                                    #(some->> (:schedule.occurrence/request %) (requests/get-request conn) :work.request/job))
          request-id (:schedule.occurrence/request occurrence)
          job (:work.request/job (requests/get-request conn request-id))]
      (when-let [done (get-in @(var-get #'jobs/live) [job :done])]
        (check! (true? (deref done 120000 false)) :original-job-return))
      (let [status (jobs/status job)
            request (requests/get-request conn request-id)
            expected-environment (get-in (requests/payload conn request) [:input :environment])
            certified (binding [ec/*execution-context* (:ctx native-room)]
                        (let [rows (run/runs native-room {:root-run-id job :limit 1000})
                              children (filterv #(= job (:run/parent %)) rows)]
                          (check! (< (count rows) 1000) :untruncated-run-query)
                          (check! (= 1 (count children)) :one-direct-candidate)
                          (let [candidate (first children)
                                value (attempt/validate-attempt (episode/attempt native-room (:run/id candidate)))]
                            (check! (= :failed (:run/status candidate)) :failed-candidate-run)
                            (check! (and (string? (:run/error candidate)) (not (str/blank? (:run/error candidate)))) :candidate-error)
                            (check! (= (:run/id candidate) (:attempt/run-id value)) :exact-candidate-attempt)
                            (check! (and (map? expected-environment)
                                         (= expected-environment (:attempt/environment value))) :retained-request-environment)
                            (check! (= :failed (get-in value [:attempt/receipt :attempt/status])) :failed-certified-receipt)
                            (check! (false? (get-in value [:attempt/receipt :attempt/checks :completed?])) :unsuccessful-completion-check)
                            value)))]
        (check! (and (:settled? status) (#{:completed :failed :cancelled} (:status status))) :settled-orchestration)
        (check! (false? (:work.request/return-pending? request)) :canonical-physical-return)
        (check! (= 2 @calls) :one-success-one-failure)
        (let [wallet (resource/balance native-room)]
          (reset! forbidden? true)
          (#'allowance/quiet-pass! passes (#'restart/next-due conn activation))
          (let [held (model/activation conn activation)
                oid (:schedule.occurrence/id occurrence)
                assert-unprocessed! (fn []
                                      (check! (empty? @entered) :no-fresh-acquisition)
                                      (check! (= 2 @calls) :no-provider-retry)
                                      (check! (= 2 (#'allowance/request-count conn room)) :two-original-requests)
                                      (check! (= wallet (resource/balance native-room)) :no-fresh-spend-or-mint)
                                      (check! (nil? (model/processing-proof conn oid)) :failed-input-not-processed)
                                      (check! (nil? (dispositions/disposition (requests/get-request conn request-id))) :failed-output-not-filed)
                                      (check! (= (:input baseline) (:schedule.activation/last-processed-input (model/activation conn activation))) :original-cursor)
                                      (check! (= (:proof baseline) (model/processing-proof conn (:occurrence baseline))) :original-proof)
                                      (check! (= (:disposition baseline) (dispositions/disposition (requests/get-request conn (:request baseline)))) :original-filing))]
            (check! (false? (:schedule.activation/enabled? held)) :failed-activation-held)
            (check! (keyword? (:schedule.activation/reason held)) :visible-hold-reason)
            (#'restart/retained-live-input! conn occurrence)
            (assert-unprocessed!)
            (#'allowance/quiet-pass! passes (java.util.Date. (+ 60000 (.getTime ^java.util.Date (#'restart/next-due conn activation)))))
            (assert-unprocessed!)
            {:version 1 :room room :activation activation :job job :root-job-status (:status status)
             :candidate (:attempt/run-id certified) :attempt-content (:attempt/content-id certified)
             :candidate-status :failed :requests 2 :scripted-calls @calls :wallet wallet
             :hold-reason (:schedule.activation/reason held)
             :active-occurrence (:schedule.activation/active-occurrence held)}))))))

(defn -main [checkpoint]
  (let [target (#'restart/private-artifact! checkpoint)
        _ (check! (not (.exists target)) :no-overwrite)
        calls (atom 0) mode (atom :success) forbidden? (atom false) entered (atom {}) passes (atom {})
        provider (codex/create {:credentials (gateway/static-credentials :fixture {} #{"https://chatgpt.com"})})]
    (reset! providers/providers {:codex-subscription provider})
    (alter-var-root #'gateway/*request-fn*
                    (constantly (fn [& _] (swap! calls inc)
                                  (if (= :failed @mode) (failed-response) (#'restart/response)))))
    (doseq [[v label] [[#'inputs/capture :capture] [#'runs/prepare :prepare]
                      [#'work/submit-prepared! :submit] [#'resource/mint! :mint]]]
      (alter-var-root v (fn [original]
                         (fn [& args]
                           (when @forbidden?
                             (swap! entered update label (fnil inc 0))
                             (throw (AssertionError. "Failed activation acquired fresh work")))
                           (apply original args)))))
    (alter-var-root #'schedules/tick-owned!
                    (fn [original]
                      (fn [host now]
                        (let [receipt (get @passes now)]
                          (try
                            (let [value (original host now)]
                              (when receipt (deliver receipt [:ok])) value)
                            (catch Throwable error
                              (when receipt (deliver receipt [:error error]))
                              (throw error)))))))
    (binding [restart/*pass-receipts* passes]
      (try
        (let [result (try (exercise! (#'restart/boot!) calls mode forbidden? entered passes)
                          (finally (#'restart/stop! (system-db/get-conn))))]
          (spit target (pr-str result))
          (println :failure-diagnostic-completed (str target)))
        (finally (shutdown-agents))))))
