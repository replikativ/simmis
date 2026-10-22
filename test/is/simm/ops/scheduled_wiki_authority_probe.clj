(ns is.simm.ops.scheduled-wiki-authority-probe
  "Opt-in native pause/revocation diagnostic; no real inference."
  (:require [is.simm.ops.scheduled-wiki-restart-probe :as restart]
            [is.simm.ops.scheduled-wiki-allowance-probe :as allowance]
            [datahike.api :as d]
            [dvergr.model.api.codex-subscription :as codex]
            [dvergr.model.gateway :as gateway]
            [dvergr.model.providers :as providers]
            [dvergr.resource :as resource]
            [is.simm.model.access :as access]
            [is.simm.model.system-db :as system-db]
            [is.simm.model.workflow-schedules :as model]
            [is.simm.ops.dvergr :as principals]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflow-runs :as runs]
            [is.simm.ops.workflow-schedules :as schedules]
            [is.simm.runtimes.application-ops :as application]))

(defn- check! [condition label]
  (when-not condition (throw (ex-info "Wiki authority diagnostic failed" {:check label}))))

(defn- exercise! [conn calls forbidden? entered passes]
  (let [{:keys [party room activation]} (#'restart/stage-a! conn calls)
        rich (#'restart/binding-for! party room)
        replacement-owner (random-uuid)
        principal (principals/party-principal party)
        wallet (resource/balance (:room rich))
        before (model/activation conn activation)
        due (#'restart/next-due conn activation)
        later (java.util.Date. (+ 60000 (.getTime ^java.util.Date due)))
        control! #(schedules/set-enabled-owned! (application/host!) principal (str room) activation %)
        assert-no-work! (fn []
                          (check! (empty? @entered) :no-hydration-or-acquisition)
                          (check! (= 1 @calls) :no-fresh-model-call)
                          (check! (= 1 (#'allowance/request-count conn room)) :one-original-request)
                          (check! (= wallet (resource/balance (:room rich))) :wallet-unchanged)
                          (let [row (model/activation conn activation)]
                            (check! (= (:schedule.activation/last-occurrence before)
                                       (:schedule.activation/last-occurrence row)) :no-new-occurrence)
                            (check! (= (:schedule.activation/last-processed-input before)
                                       (:schedule.activation/last-processed-input row)) :cursor-unchanged)))]
    (check! (and (access/can? @conn party :read {:room room})
                 (access/can? @conn party :merge {:room room})) :initial-read-and-merge)
    (d/transact conn [{:party/id replacement-owner :party/type :human
                       :party/display-name "Replacement diagnostic owner"}])
    (check! (false? (:schedule.activation/enabled? (control! false))) :paused-through-handler)
    (#'restart/write-evidence! rich "Synthetic K2 stays pending while paused or unauthorized.")
    (reset! forbidden? true)
    (#'allowance/quiet-pass! passes due)
    (assert-no-work!)
    ;; This reauthorizes through the real handler. With this explicitly driven
    ;; clock there is no new pass until after the following revocation.
    (check! (true? (:schedule.activation/enabled? (control! true))) :resume-before-revocation)
    ;; Transfer the actual relation to another valid human, rather than making
    ;; the Room ownerless and accidentally testing malformed metadata.
    (d/transact conn [[:db/add [:room/id room] :room/owner [:party/id replacement-owner]]
                      [:db/add [:room/id room] :room/parties [:party/id replacement-owner]]])
    (check! (access/can? @conn party :read {:room room}) :read-retained)
    (check! (not (access/can? @conn party :merge {:room room})) :merge-revoked)
    (check! (access/can? @conn replacement-owner :merge {:room room}) :valid-replacement-owner)
    (#'allowance/quiet-pass! passes later)
    (assert-no-work!)
    (let [held (model/activation conn activation)]
      (check! (false? (:schedule.activation/enabled? held)) :unauthorized-activation-held)
      (check! (keyword? (:schedule.activation/reason held)) :visible-hold-reason)
      (check! (some? (try (control! true) nil (catch Exception error error))) :unauthorized-resume-refused)
      (check! (= held (model/activation conn activation)) :refused-resume-did-not-write)
      (#'allowance/quiet-pass! passes (java.util.Date. (+ 60000 (.getTime ^java.util.Date later))))
      (assert-no-work!)
      {:version 1 :room room :activation activation :scripted-calls @calls :requests 1
       :wallet wallet :read-retained true :merge-revoked true
       :replacement-owner replacement-owner
       :hold-reason (:schedule.activation/reason held)})))

(defn -main [checkpoint]
  (let [target (#'restart/private-artifact! checkpoint)
        _ (check! (not (.exists target)) :no-overwrite)
        calls (atom 0) forbidden? (atom false) entered (atom {}) passes (atom {})
        provider (codex/create {:credentials (gateway/static-credentials :fixture {} #{"https://chatgpt.com"})})]
    (reset! providers/providers {:codex-subscription provider})
    (alter-var-root #'gateway/*request-fn*
                    (constantly (fn [& _] (swap! calls inc) (#'restart/response))))
    (doseq [[v label] [[#'reads/resolve-host-room! :room] [#'reads/resolve-host-read! :repository]
                      [#'inputs/capture :capture] [#'runs/prepare :prepare]
                      [#'work/submit-prepared! :submit] [#'resource/mint! :mint]]]
      (alter-var-root v
                      (fn [original]
                        (fn [& args]
                          (when @forbidden?
                            (swap! entered update label (fnil inc 0))
                            (throw (AssertionError. "Paused or revoked activation acquired fresh work")))
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
        (let [result (try (exercise! (#'restart/boot!) calls forbidden? entered passes)
                          (finally (#'restart/stop! (system-db/get-conn))))]
          (spit target (pr-str result))
          (println :authority-diagnostic-completed (str target)))
        (finally (shutdown-agents))))))
