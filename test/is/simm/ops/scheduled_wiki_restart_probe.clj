(ns is.simm.ops.scheduled-wiki-restart-probe
  "OPT-IN two-process durable schedule diagnostic. Not a provider benchmark or
   a default test: component startup/hydration is real, the clock is explicitly
   driven, and only the model transport is scripted. No live homes are accepted."
  (:require [is.simm.runtimes.context :as context]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [datahike.api :as d]
            [dvergr.agent.attempt :as attempt]
            [dvergr.agent.episode :as episode]
            [dvergr.catalog.room :as catalog]
            [dvergr.discourse :as discourse]
            [dvergr.jobs :as jobs]
            [dvergr.model.api.codex-subscription :as codex]
            [dvergr.model.gateway :as gateway]
            [dvergr.model.providers :as providers]
            [dvergr.resource :as resource]
            [dvergr.runtime.clock :as clock]
            [dvergr.rooms :as rooms]
            [dvergr.system.rooms :as native]
            [geschichte.repo :as repo]
            [is.simm.agents.room-agents :as agents]
            [is.simm.documents.profile :as profile]
            [is.simm.documents.starter-bundle :as documents]
            [is.simm.model.access :as access]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.knowledge-rooms :as knowledge-rooms]
            [is.simm.model.system-db :as system-db]
            [is.simm.model.workflow-schedules :as model]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.dvergr :as principals]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.knowledge-output-service :as output]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflow-authoring :as authoring]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflow-runs :as runs]
            [is.simm.ops.workflow-schedules :as schedules]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.workflow-schedules :as runtime]
            [is.simm.starter.wiki-maintenance :as starter]
            [jsonista.core :as json]
            [org.replikativ.spindel.engine.core :as ec]
            [taoensso.telemere :as log]))

(defn- check! [condition label]
  (when-not condition (throw (ex-info "Durable wiki schedule diagnostic failed" {:check label}))))

(defn- private-artifact! [path]
  (let [home (System/getenv "DVERGR_HOME")
        root (some-> home io/file .getCanonicalFile .getParentFile)
        file (some-> path io/file .getCanonicalFile)]
    (check! (and (string? home)
                 (re-matches #"/tmp/simmis-wiki-restart\.[A-Za-z0-9]+/dvergr" home)
                 (= home (.getCanonicalPath (io/file home)))
                 file (.startsWith (.toPath file) (.toPath root))) :private-home-required)
    file))

(defn- boot! []
  (context/bind-server-context!)
  (system-db/init!)
  (native/set-grant-permits! access/permission-satisfies?)
  (context/with-server-context
    (native/hydrate-rooms! {:repository-profiles (profile/policies (documents/read-packaged!))})
    (rooms/hydrate-registry! context/server-context)
    (work/recover! (system-db/get-conn) context/server-context agents/live-room)
    (benchmarks/recover! (system-db/get-conn) context/server-context))
  (application/start!)
  (runtime/start-owned! (application/host!))
  (check! (runtime/running?) :dispatcher-ready)
  (system-db/get-conn))

(defn- stop! [conn]
  ;; Same native HOST/Room shutdown boundaries as the application; never
  ;; swallow a failed physical join and then claim a durable restart passed.
  (application/stop!)
  (clock/stop!)
  (context/with-server-context
    (let [plan (discourse/close-registered-rooms! context/server-context)]
      (native/release-room-stores!)
      (when conn (d/release conn true))
      (discourse/close-shutdown-contexts! plan))))

(defn- await-control! [conn observe effect]
  (let [ready (promise) key (random-uuid)
        sample! (fn [_]
                  (try (when-let [value (observe)] (deliver ready [:ok value]))
                       (catch Throwable error (deliver ready [:error error]))))]
    (d/listen conn key sample!)
    (try
      (effect)
      (sample! nil)
      (let [[status value] (deref ready 120000 [:timeout])]
        (check! (not= :timeout status) :control-event-deadline)
        (if (= :error status) (throw value) value))
      (finally (d/unlisten conn key)))))

(def ^:dynamic *pass-receipts* nil)

(defn- tick! [conn activation due predicate]
  (let [receipt (promise)]
    (swap! *pass-receipts* assoc due receipt)
    (try
      (let [occurrence (await-control!
                        conn
                        #(let [row (model/activation conn activation)
                               occurrence (model/occurrence conn (:schedule.activation/last-occurrence row))]
                           (when (and (= due (:schedule.occurrence/intended-at occurrence))
                                      (predicate occurrence)) occurrence))
                        #(context/with-server-context (clock/set-now! due)))
            [status error] (deref receipt 120000 [:timeout])]
        (check! (not= :timeout status) :physical-dispatcher-pass-exit)
        (when (= :error status) (throw error))
        occurrence)
      (finally (swap! *pass-receipts* dissoc due)))))

(defn- response []
  {:status 200
   :body (java.io.ByteArrayInputStream.
           (.getBytes
             (apply str
                    (map #(str "data: " (json/write-value-as-string %) "\n\n")
                         [{:type "response.output_text.delta" :delta "No supported wiki changes."}
                          {:type "response.completed"
                           :response {:id "restart-noop" :model "gpt-5.6-sol"
                                      :usage {:input_tokens 10 :output_tokens 10}}}])) "UTF-8"))})

(defn- write-evidence! [rich text]
  (binding [ec/*execution-context* (:ctx (:room rich))]
    (repo/write! (:conn rich) "inbox/restart-evidence.md" (.getBytes ^String text "UTF-8"))))

(defn- binding-for! [party room-id]
  (application/with-operation!
    #(reads/resolve-host-read! % (principals/party-principal party) (str room-id))))

(defn- next-due [conn activation]
  (:schedule.activation/next-fire (model/activation conn activation)))

(defn- retained-live-input! [conn occurrence]
  (let [value (model/retained-owned-input conn occurrence)]
    (check! (and (map? value) (= 2 (:version value))
                 (= (:occurrence-id value) (:schedule.occurrence/id occurrence))
                 (= (:input-id value) (:schedule.occurrence/input-id occurrence))
                 (= (:target-revision value) (:schedule.occurrence/revision occurrence))
                 (every? map? ((juxt :captured :publication-basis :knowledge-origin) value)))
            :retained-live-input)
    value))

(defn- run-once!
  ([conn party room-id activation]
   (run-once! conn party room-id activation (next-due conn activation)))
  ([conn party room-id activation due]
   (let [occurrence (tick! conn activation due
                          #(some->> (:schedule.occurrence/request %) (requests/get-request conn) :work.request/job))
        request-id (:schedule.occurrence/request occurrence)
        job (:work.request/job (requests/get-request conn request-id))
        ;; status's public wait is deliberately client-capped at 25 seconds.
        ;; This private process owner joins the ORIGINAL Job completion receipt,
        ;; including its settlement callback, rather than polling or extending
        ;; a UI timeout. A Job already absent from live is already settled.
        _ (when-let [done (get-in @(var-get #'jobs/live) [job :done])]
            (check! (true? (deref done 120000 false)) :physical-job-exit))
        status (jobs/status job)]
    (check! (and (= :completed (:status status)) (:settled? status)) :settled-job)
    (check! (= {:status :no-op :request (str request-id)} (output/collect! party room-id request-id)) :filed-noop)
    ;; This actual dispatcher pass observes physical settlement and advances
    ;; its durable processed-input cursor; the new occurrence skips unchanged.
    (let [skipped (tick! conn activation (next-due conn activation)
                         #(= :unchanged (:schedule.occurrence/reason %)))
          proof (model/processing-proof conn (:schedule.occurrence/id occurrence))]
      (check! (some? proof) :processing-proof)
      {:request request-id :job job :occurrence (:schedule.occurrence/id occurrence)
       :input (:schedule.occurrence/input-id occurrence) :proof proof
       :retained-input (retained-live-input! conn occurrence)
       :disposition (dispositions/disposition (requests/get-request conn request-id))
       :skipped (:schedule.occurrence/id skipped)}))))

(defn- stage-a! [conn calls]
  (check! (empty? (model/activations conn)) :fresh-home)
  (let [party (random-uuid)]
    (d/transact conn [{:party/id party :party/type :human :party/display-name "Restart diagnostic"
                       :party/role :admin :actor/id (keyword "party" (str party))
                       :actor/kind :human :actor/name "Restart diagnostic" :actor/status :online
                       :actor/created-at (java.util.Date.)}])
    (let [room-id (:room/id (knowledge-rooms/create! party "Disposable restart diagnostic" []))
          rich (binding-for! party room-id)
          room (:room rich)
          ;; This opt-in diagnostic retains the historical whole-selection
          ;; cursor contract. Explicitly author that version into this NEW
          ;; disposable room using the ordinary revision-guarded writer.
          files (starter/source 3)
          initial (sources/details! (application/host!) (principals/party-principal party)
                                    (str room-id) {:name starter/name})
          _ (authoring/install! (application/host!) (principals/party-principal party)
                                (str room-id) {:name starter/name :files (assoc files "input-batching.edn" nil)
                                               :expected-revision (get-in initial [:data :revision])})
          source (sources/details! (application/host!) (principals/party-principal party)
                                   (str room-id) {:name starter/name})
          _ (check! (= files (get-in source [:data :files])) :historical-source-installed)
          execution {:name starter/name :params {} :model "codex-subscription-sol"
                     :expected-bundle (:id (catalog/bundle starter/name files))
                     :expected-owned-source (parse-uuid (get-in source [:data :revision]))
                     :limits {:budget-microdollars 10000 :max-dispatches 1 :max-model-steps 1
                              :timeout-ms 30000 :settlement "discard"}}]
      (check! (= :file (get-in rich [:target :backend])) :file-backed-repository)
      (write-evidence! rich "Synthetic retained evidence K1.")
      (resource/install-unit! room {:symbol resource/model-dispatches
                                   :name "Model dispatches" :precision 0})
      (resource/mint! room {:id (random-uuid) :resources {resource/microdollars 30000M resource/model-dispatches 3M}})
      (let [row (schedules/create-owned! (application/host!) (principals/party-principal party)
                                         (str room-id)
                                         {:request-id (random-uuid) :kind "workflow"
                                          :cadence {:interval-ms 60000 :tz "UTC"} :execution execution})
            id (:schedule.activation/id row)
            result (run-once! conn party room-id id)]
        (check! (= files (get-in (model/managed-target conn row) [:snapshot :files])) :exact-starter)
        (check! (= 1 @calls) :one-scripted-call)
        (check! (= 2M (get (resource/balance room) resource/model-dispatches)) :finite-admissions)
        {:version 1 :pid (.pid (java.lang.ProcessHandle/current)) :party party :room room-id :activation id
         :target (:target rich) :revision (:schedule.activation/revision row)
         :wallet (resource/balance room) :first result}))))

(defn- stage-b! [conn prior calls forbidden? entered]
  (check! (= 1 (:version prior)) :checkpoint-version)
  (check! (not= (:pid prior) (.pid (java.lang.ProcessHandle/current))) :different-process)
  (let [{:keys [party room activation first wallet target revision]} prior
        rich (binding-for! party room)
        native-room (:room rich)
        request (requests/get-request conn (:request first))]
    (check! (= target (:target rich)) :same-physical-repository)
    (check! (= wallet (resource/balance native-room)) :restored-wallet)
    (check! (= revision (:schedule.activation/revision (model/activation conn activation))) :restored-revision)
    (check! (= (:disposition first) (dispositions/disposition request)) :restored-disposition)
    (check! (= (:job first) (:work.request/job request)) :restored-request-job)
    (let [certified (attempt/validate-attempt
                      (episode/attempt native-room (get-in first [:disposition :candidate])))]
      (check! (= (:attempt/content-id certified) (get-in first [:disposition :attempt-content])) :restored-certified-attempt))
    (check! (= (:retained-input first)
               (retained-live-input! conn (model/occurrence conn (:occurrence first)))) :restored-retained-input)
    (check! (= (:proof first) (model/processing-proof conn (:occurrence first))) :restored-proof)
    (check! (= (:input first) (:schedule.activation/last-processed-input (model/activation conn activation))) :restored-cursor)
    (check! (and (= :completed (:status (jobs/status (:job first))))
                 (:settled? (jobs/status (:job first)))) :restored-job)
    (tick! conn activation (next-due conn activation) #(= :unchanged (:schedule.occurrence/reason %)))
    (check! (empty? @entered) :no-startup-or-unchanged-acquisition)
    (check! (zero? @calls) :no-replayed-inference)
    (check! (= wallet (resource/balance native-room)) :no-replayed-spend)
    (reset! forbidden? false)
    (write-evidence! rich "Synthetic retained evidence K2.")
    (let [second (run-once! conn party room activation)]
      (check! (= 1 @calls) :one-new-call)
      (check! (not= (:request first) (:request second)) :new-request)
      (check! (not= (:input first) (:input second)) :new-evidence-identity)
      (check! (= 1M (get (resource/balance native-room) resource/model-dispatches)) :remaining-finite-admission)
      (check! (= (:disposition first) (dispositions/disposition (requests/get-request conn (:request first)))) :original-receipt-preserved)
      {:version 1 :pid (.pid (java.lang.ProcessHandle/current)) :prior-pid (:pid prior)
       :first first :second second :wallet (resource/balance native-room)})))

(defn -main [phase checkpoint]
  (let [file (private-artifact! checkpoint)
        _ (check! (#{"prepare" "reopen"} phase) :known-phase)
        prior (when (= "reopen" phase) (edn/read-string (slurp file)))
        _ (when (= "prepare" phase) (check! (not (.exists file)) :no-overwrite))
        target (if prior (io/file (.getParentFile file) "reopened.edn") file)
        _ (check! (not (.exists target)) :no-repeated-phase)
        calls (atom 0)
        passes (atom {})
        forbidden? (atom (boolean prior))
        entered (atom {})
        provider (codex/create {:credentials (gateway/static-credentials :fixture {} #{"https://chatgpt.com"})})]
    ;; This opt-in main owns a WHOLE private JVM. Keep its external transport
    ;; permanently scripted until process exit, including failed cleanup; an
    ;; uncertain owner must not outlive a temporary with-redefs fixture scope.
    (reset! providers/providers {:codex-subscription provider})
    (alter-var-root #'gateway/*request-fn* (constantly (fn [& _] (swap! calls inc) (response))))
    ;; Instrument before boot, including the startup-seeded pass. These are
    ;; private-process measurements, not an application authority or executor.
    (doseq [[v label] [[#'inputs/capture :capture] [#'runs/prepare :prepare]
                      [#'work/submit-prepared! :submit]]]
      (alter-var-root v (fn [original]
                         (fn [& args]
                           (when @forbidden?
                             (swap! entered update label (fnil inc 0))
                             (throw (AssertionError. "Unchanged restart acquired fresh work")))
                           (apply original args)))))
    (alter-var-root #'schedules/tick-owned!
                    (fn [original]
                      (fn [host now]
                        ;; Capture on ENTRY: an older same-time pass must not
                        ;; acknowledge a receipt registered after it entered.
                        (let [receipt (get @passes now)]
                          (try
                            (let [result (original host now)]
                              (when receipt (deliver receipt [:ok]))
                              result)
                            (catch Throwable error
                              (when receipt (deliver receipt [:error error]))
                              (throw error)))))))
    (binding [*pass-receipts* passes]
      (try
        (let [result (try (let [conn (boot!)]
                            (if prior (stage-b! conn prior calls forbidden? entered) (stage-a! conn calls)))
                          (finally (stop! (system-db/get-conn))))]
          ;; Written only AFTER normal physical shutdown completed. It contains
          ;; portable audit data, never connections, contexts or authority.
          (spit target (pr-str result))
          (log/log! {:level :info :id ::phase-completed
                     :data {:phase phase :scripted-calls @calls :checkpoint (str target)}}))
        (finally (shutdown-agents))))))
