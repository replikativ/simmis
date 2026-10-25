(ns is.simm.ops.owned-schedule-occurrences-test
  "Inactive owned composition proofs. Claim-only cases make no Job claim;
   the final test uses the actual kernel, provider adapter, SCI and filing."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.agent.episode :as episode]
            [dvergr.agent.run :as run]
            [dvergr.catalog.room :as catalog]
            [dvergr.discourse :as discourse]
            [dvergr.jobs :as jobs]
            [dvergr.model.api.codex-subscription :as codex]
            [dvergr.model.gateway :as gateway]
            [dvergr.model.providers :as providers]
            [dvergr.resource :as resource]
            [dvergr.runtime.clock :as clock]
            [dvergr.runtime.ctx :as rctx]
            [dvergr.runtime.preparation :as preparation]
            [dvergr.room.store :as store]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.agents.room-agents :as rooms]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.work-requests :as requests]
            [is.simm.model.system-db :as system-db]
            [is.simm.model.workflow-schedules :as model]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.benchmarks-test :as benchmark-fixture]
            [is.simm.ops.knowledge-inputs :as knowledge]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-schedule-fixture :as fixture]
            [is.simm.ops.owned-schedule-admission-test :as activation-fixture]
            [is.simm.ops.owned-workflow-admission-test :as native]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflow-runs :as workflow-runs]
            [is.simm.ops.workflow-runs-test :as saved]
            [is.simm.ops.workflow-schedules :as schedules]
            [is.simm.ops.workflows :as workflows]
            [is.simm.runtimes.workflow-schedules :as runtime]
            [jsonista.core :as json]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.signal :as sig]))

(defn- principal [w] (ops/party-principal (:owner w)))
(defn- seed! [conn source]
  (doseq [[path text] source]
    (repo/write! conn (str "workflows/echo/" path) (.getBytes ^String text "UTF-8"))))
(defn- text! [conn path text] (repo/write! conn path (.getBytes ^String text "UTF-8")))
(defn- state [conn]
  (let [db @conn] {:commit (d/commit-id db) :config (:config db) :tree (query/worktree db)}))
(defn- create! [{:keys [w host]} source]
  (let [execution (-> (saved/input source) (dissoc :request-id)
                      (assoc-in [:limits :settlement] "discard")
                      (assoc :expected-owned-source
                             (parse-uuid (get-in (sources/details! host (principal w) (str (:room w)) {:name "echo"})
                                                 [:data :revision]))))]
    (schedules/create-owned! host (principal w) (str (:room w))
                             {:request-id (random-uuid) :kind "workflow"
                              :cadence {:interval-ms 60000 :tz "UTC"} :execution execution})))
(defn- with-occurrence [order source f]
  (@#'fixture/with-fixture
   order
   (fn [{:keys [conn w owned attached room] :as data}]
     (d/transact conn model/schema)
     (d/transact conn [{:party/id (:owner w) :party/role :admin}])
     (seed! owned source)
     (seed! attached (assoc source "helpers/data.txt" "OTHER helper"))
     (is (= (set (keys source)) (set (map :path (workflows/manifest-in @owned "echo")))))
     (is (= (set (keys source)) (set (map :path (workflows/manifest-in @attached "echo")))))
     (text! owned "inbox/evidence.md" "Owned trigger K1")
     (text! owned "wiki/context.md" "Owned context K1")
     (text! attached "inbox/evidence.md" "OTHER trigger")
     (text! attached "wiki/context.md" "OTHER context")
     ;; Isolated fixture allowance only. No account/provider admission or paid
     ;; request: the actual provider adapter below uses a fixed in-memory HTTP.
     (resource/mint! room {:id (random-uuid) :resources (work/resources (:limits (saved/input source)))})
     (let [row (create! data source)]
       (f (assoc data :row row :due (:schedule.activation/next-fire row)))))))
(defn- current-occurrence [conn row]
  (let [a (model/activation conn (:schedule.activation/id row))]
    (model/occurrence conn (:schedule.activation/last-occurrence a))))
(defn- claim-only [seen]
  (fn [conn _ p room-id room input prepared]
    (swap! seen conj {:input input :prepared prepared :room room})
    (requests/claim! conn {:party (work/authorize! conn p room-id) :room room-id
                           :client-id (:request-id input) :kind (:request-kind prepared)
                           :intent (:basis prepared) :limits (update (:limits input) :settlement keyword)})))

(deftest fixtures-match-real-definition-and-closed-local-schemas
  (doseq [source [saved/source saved/knowledge-source]]
    (is (m/validate catalog/Definition (edn/read-string (get source "workflow.edn")))))
  (is (m/validate (var-get #'schedules/owned-workflow-schema)
                  (-> (saved/input) (dissoc :request-id) (assoc :expected-owned-source (random-uuid))))))

(deftest offline-historical-code-needs-no-current-default-or-source
  (doseq [order [true false]]
    (with-occurrence order saved/source
      (fn [{:keys [conn w host owned attached row due]}]
        (text! owned "workflows/echo/helpers/data.txt" "Parent B")
        (d/transact conn [[:db/retract [:room/id (:room w)] :room/default-repository [:system/id (:repo-sys w)]]])
        (let [seen (atom []) calls (atom 0) before (state owned) other (state attached)
              selected (workspace/selected-system-id)
              forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Offline occurrence read current source/default")))]
          (with-redefs [reads/resolve-host-read! forbidden knowledge/selection forbidden knowledge/capture forbidden
                        workflows/snapshot forbidden workflows/snapshot-in forbidden
                        work/submit-prepared! (claim-only seen)]
            (schedules/tick-owned! host due))
          (is (= 1 (count @seen)))
          (is (zero? @calls))
          (is (= "pinned helper" (get-in (first @seen) [:prepared :basis :source "helpers/data.txt"])))
          (is (= (:source-origin (model/managed-target conn row)) (get-in (first @seen) [:prepared :basis :source-origin])))
          (is (= :host (get-in (first @seen) [:prepared :submission-channel])))
          (is (nil? (:schedule.occurrence/input-ref (current-occurrence conn row))))
          (is (= selected (workspace/selected-system-id)))
          (is (= before (state owned))) (is (= other (state attached))))))))

(deftest offline-benchmark-uses-actual-prepare-and-adapter-without-current-default
  (doseq [order [true false]]
    (@#'fixture/with-fixture
     order
     (fn [{:keys [conn w host owned attached room]}]
       (let [source (var-get #'activation-fixture/source)]
         (is (= 5 (count source)))
         (is (m/validate catalog/Definition (edn/read-string (get source "workflow.edn"))))
         (d/transact conn model/schema)
         (d/transact conn [{:party/id (:owner w) :party/role :admin}])
         (seed! owned source) (seed! attached (assoc source "helpers/code.clj" "OTHER helper"))
         (is (= (set (keys source)) (set (map :path (workflows/manifest-in @owned "echo")))))
         (let [execution (-> (benchmark-fixture/input source) (dissoc :request-id)
                             (assoc :models ["codex-subscription-sol"])
                             (assoc :expected-owned-source
                                    (parse-uuid (get-in (sources/details! host (principal w) (str (:room w)) {:name "echo"})
                                                        [:data :revision]))))
               _ (is (m/validate (var-get #'schedules/owned-benchmark-schema) execution))
               _ (resource/mint! room {:id (random-uuid) :resources (work/resources (:limits execution))})
               row (schedules/create-owned! host (principal w) (str (:room w))
                                            {:request-id (random-uuid) :kind "benchmark" :execution execution
                                             :cadence {:interval-ms 60000 :tz "UTC"}})
               target (model/managed-target conn row)
               prepare benchmarks/prepare adapter benchmarks/adapter
               prepared (atom 0) adapted (atom 0) forbidden-calls (atom 0) seen (atom [])
               forbidden (fn [& _] (swap! forbidden-calls inc) (throw (AssertionError. "Offline benchmark acquired source/LIVE inputs")))
               before (state owned) other (state attached) selected (workspace/selected-system-id)]
           (d/transact conn [[:db/retract [:room/id (:room w)] :room/default-repository [:system/id (:repo-sys w)]]])
           (with-redefs [reads/resolve-host-read! forbidden knowledge/selection forbidden knowledge/capture forbidden
                         workflows/snapshot forbidden workflows/snapshot-in forbidden
                         benchmarks/prepare (fn [& args] (swap! prepared inc) (apply prepare args))
                         benchmarks/adapter (fn [& args] (swap! adapted inc) (apply adapter args))
                         work/submit-prepared! (claim-only seen)]
             (schedules/tick-owned! host (:schedule.activation/next-fire row)))
           (is (= 1 @prepared @adapted (count @seen)))
           (is (zero? @forbidden-calls))
           (let [observed (first @seen) o (current-occurrence conn row)
                 request (requests/get-request conn (:schedule.occurrence/request o))
                 basis (:basis (:prepared observed))]
             (is (= :benchmark (:request-kind (:prepared observed)) (:work.request/kind request)))
             (is (= :host (:admission-channel basis)))
             (is (= :host (:submission-channel (:prepared observed))))
             (is (= (:source-origin target) (:source-origin basis)))
             (is (= (assoc execution :request-id (:schedule.occurrence/id o)) (:submission basis) (:input observed)))
             (is (= basis (:input (requests/payload conn request))))
             (is (fn? (:start! (:prepared observed))))
             (is (fn? (:job-context! (:prepared observed))))
             (is (nil? (:work.request/job request)) "Claim-only: no experiment/Job/Attempt was executed")
             (is (nil? (:schedule.occurrence/input-ref o))))
           (is (= before (state owned))) (is (= other (state attached)))
           (is (= selected (workspace/selected-system-id)))))))))

(deftest exact-canonical-request-precedes-hydration-models-source-and-funds
  (with-occurrence true saved/source
    (fn [{:keys [conn w host row due]}]
      (model/claim-due! conn row {:interval-ms 60000} due)
      (let [o (current-occurrence conn row)
            target (model/managed-target conn row)
            input (assoc (:execution target) :request-id (:schedule.occurrence/id o))
            {:keys [request]} (requests/claim! conn {:party (:owner w) :room (:room w)
                                                     :client-id (:schedule.occurrence/id o) :kind :workflow
                                                     :intent {:submission input}
                                                     :limits (update (:limits input) :settlement keyword)})
            calls (atom 0) forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Exact retry touched fresh admission")))]
        (d/transact conn [[:db/retract [:room/id (:room w)] :room/default-repository [:system/id (:repo-sys w)]]])
        (with-redefs [reads/resolve-host-room! forbidden reads/resolve-host-read! forbidden
                      rooms/live-room forbidden schedules/model-policy forbidden schedules/host-policy forbidden
                      resource/balance forbidden workflow-runs/prepare forbidden work/submit-prepared! forbidden]
          (schedules/tick-owned! host due))
        (let [actual (model/occurrence conn (:schedule.occurrence/id o))]
          (is (= :interrupted (:schedule.occurrence/state actual))))
        (is (= (:work.request/id request) (:schedule.occurrence/request (model/occurrence conn (:schedule.occurrence/id o)))))
        (is (zero? @calls))))))

(deftest recovery-holds-legacy-and-corrupt-rows-and-continues-to-valid-canonical-work
  (with-occurrence true saved/source
    (fn [{:keys [conn w host row due]}]
      (let [target (model/managed-target conn row)
            bad (mapv (fn [target]
                        (model/create! conn {:id (random-uuid) :room (:room w) :by (:owner w)
                                             :target target :now due :next-fire due}))
                      [(assoc target :version 1) {:version 3 :unknown true}])
            rows (conj bad row)
            occurrences (mapv (fn [activation]
                                (model/claim-due! conn activation {:interval-ms 60000} due)
                                (let [o (current-occurrence conn activation)]
                                  (is (uuid? (model/admit! conn (:schedule.occurrence/id o))))
                                  (current-occurrence conn activation))) rows)
            good (last occurrences)
            input (assoc (:execution target) :request-id (:schedule.occurrence/id good))
            {:keys [request]} (requests/claim! conn {:party (:owner w) :room (:room w)
                                                     :client-id (:schedule.occurrence/id good) :kind :workflow
                                                     :intent {:submission input} :limits (update (:limits input) :settlement keyword)})
            calls (atom 0) forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Recovery touched fresh execution")))]
        ;; A canonical submitted pointer is a metadata observation here. No
        ;; synthetic Job/Run is executed and completion is not invoked.
        (d/transact conn [[:db/add [:work.request/id (:work.request/id request)] :work.request/state :submitted]])
        (with-redefs [reads/resolve-host-room! forbidden reads/resolve-host-read! forbidden
                      rooms/live-room forbidden schedules/model-policy forbidden schedules/host-policy forbidden
                      resource/balance forbidden workflow-runs/prepare forbidden work/submit-prepared! forbidden
                      model/admitting-occurrences (fn [_] occurrences)]
          (schedules/recover-owned! host))
        (doseq [o (butlast occurrences)]
          (is (= :interrupted (:schedule.occurrence/state (model/occurrence conn (:schedule.occurrence/id o))))))
        (doseq [activation bad]
          (is (= :owned-dispatch-unavailable (:schedule.activation/reason (model/activation conn (:schedule.activation/id activation))))))
        (let [actual (model/occurrence conn (:schedule.occurrence/id good))]
          (is (= :linked (:schedule.occurrence/state actual)))
          (is (= (:work.request/id request) (:schedule.occurrence/request actual))))
        (is (zero? @calls))))))

(deftest legacy-startup-holds-pending-admitting-and-linked-requests-without-replay
  (doseq [phase [:pending :admitting :linked-unsettled :linked-settled]]
    (with-occurrence true saved/source
      (fn [{:keys [conn w host row due owned attached room]}]
        (model/set-enabled! conn (:schedule.activation/id row) false)
        (let [target (-> (model/managed-target conn row)
                         (assoc :version 1)
                         (dissoc :room :by :creation-channel :source-origin)
                         (update :execution dissoc :expected-owned-source))
              legacy (model/create! conn {:id (random-uuid) :room (:room w) :by (:owner w)
                                          :target target :now due :next-fire due})
              id (:schedule.activation/id legacy)
              _ (model/claim-due! conn legacy (:cadence target) due)
              o (current-occurrence conn legacy)
              oid (:schedule.occurrence/id o)
              request (when-not (= :pending phase)
                        (model/admit! conn oid)
                        (:request (requests/claim! conn
                                                   {:party (:owner w) :room (:room w) :client-id oid :kind :workflow
                                                    :intent {:submission (assoc (:execution target) :request-id oid)}
                                                    :limits (update (get-in target [:execution :limits]) :settlement keyword)})))
              _ (when (#{:linked-unsettled :linked-settled} phase)
                  ;; Metadata-only legacy request fixture, not a synthetic Run
                  ;; execution. The dispatcher must never inspect its Job.
                  (d/transact conn [{:db/id (:db/id request) :work.request/state :submitted
                                     :work.request/job (random-uuid)
                                     :work.request/return-pending? (= :linked-unsettled phase)}])
                  (model/finish-admission! conn oid :linked (:work.request/id request) nil))
              before (model/activation conn id)
              before-request (when request (requests/get-request conn (:work.request/id request)))
              before-owned (state owned) before-other (state attached)
              balance (resource/balance room) calls (atom 0)
              forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Legacy hold touched execution or completion")))]
          (with-redefs [reads/resolve-host-room! forbidden reads/resolve-host-read! forbidden
                        rooms/live-room forbidden knowledge/selection forbidden knowledge/capture forbidden
                        workflow-runs/prepare forbidden work/submit-prepared! forbidden jobs/status forbidden]
            (schedules/recover-owned! host)
            (schedules/tick-owned! host due))
          (let [after (model/activation conn id)]
            (is (= :owned-dispatch-unavailable (:schedule.activation/reason after)))
            (is (false? (:schedule.activation/enabled? after)))
            (is (= (dissoc (assoc before :schedule.activation/enabled? false) :schedule.activation/reason)
                   (dissoc after :schedule.activation/reason))))
          (is (= target (model/managed-target conn (model/activation conn id))))
          ;; An unsupported target cannot reconcile its existing canonical
          ;; request. Keep admission held rather than claiming it never submitted.
          (is (= (if (= :admitting phase) :admitting
                     (if (= :pending phase) :pending :linked))
                 (:schedule.occurrence/state (model/occurrence conn oid))))
          (when request
            (is (= before-request (requests/get-request conn (:work.request/id request)))))
          (is (= balance (resource/balance room)))
          (is (= before-owned (state owned)))
          (is (= before-other (state attached)))
          (is (zero? @calls)))))))

(deftest live-capture-is-durable-before-prepare-and-source-and-knowledge-epochs-differ
  (doseq [order [true false]]
    (with-occurrence order saved/knowledge-source
      (fn [{:keys [conn host owned row due]}]
        (text! owned "workflows/echo/helpers/data.txt" "Parent code B")
        (let [seen (atom []) prepare workflow-runs/prepare prepared (atom 0)]
          (with-redefs [work/submit-prepared! (claim-only seen)
                        workflow-runs/prepare
                        (fn [& args]
                          (swap! prepared inc)
                          (let [o (current-occurrence conn row) retained (model/retained-owned-input conn o)]
                            (is (uuid? (:schedule.occurrence/input-ref o)))
                            (is (= "Owned context K1" (get-in retained [:captured :inputs "/in/knowledge/sources/wiki/context.md"]))))
                          (apply prepare args))]
            (schedules/tick-owned! host due))
          (is (= 1 @prepared (count @seen)))
          (is (= "pinned helper" (get-in (first @seen) [:prepared :basis :source "helpers/data.txt"])))
          (is (= "Owned context K1" (get-in (first @seen) [:prepared :basis :inputs "/in/knowledge/sources/wiki/context.md"])))
          (is (= (get-in (model/managed-target conn row) [:source-origin :revision])
                 (get-in (first @seen) [:prepared :basis :source-origin :revision])))
          (is (uuid? (get-in (first @seen) [:prepared :basis :knowledge-origin :selection-id]))))))))

(deftest missing-original-retain-ack-never-reaches-prepare-or-restarts-from-root
  (with-occurrence true saved/knowledge-source
    (fn [{:keys [conn host row due]}]
      (let [retain model/retain-owned-input! calls (atom 0) retains (atom 0)]
        (with-redefs [model/retain-owned-input! (fn [& args] (swap! retains inc) (apply retain args) (throw (ex-info "Lost original ACK" {})))
                      workflow-runs/prepare (fn [& _] (swap! calls inc) (throw (AssertionError. "Prepared without original ACK")))
                      work/submit-prepared! (fn [& _] (swap! calls inc) (throw (AssertionError. "Claimed without original ACK")))]
          (schedules/tick-owned! host due)
          (let [o (current-occurrence conn row)]
            (is (= :interrupted (:schedule.occurrence/state o)))
            (is (uuid? (:schedule.occurrence/input-ref o)))
            (is (some? (model/retained-owned-input conn o))))
          (schedules/recover-owned! host)
          (schedules/tick-owned! host due))
        (is (= 1 @retains))
        (is (zero? @calls))))))

(deftest unchanged-selection-skips-before-capture-or-funds
  (with-occurrence true saved/knowledge-source
    (fn [{:keys [conn host row due]}]
      (let [target (model/managed-target conn row)
            binding (reads/resolve-host-read! host (ops/party-principal (:by target)) (str (:room target)))
            policy (knowledge/declaration (get-in target [:snapshot :files]))
            selected (knowledge/selection (:db binding) policy)
            input-id (hasch/uuid [:simmis/scheduled-input-v1 (:schedule.activation/revision row) (:trigger-id selected)])
            calls (atom 0) forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Unchanged selection did fresh work")))]
        (d/transact conn [[:db/add [:schedule.activation/id (:schedule.activation/id row)] :schedule.activation/last-processed-input input-id]])
        (with-redefs [knowledge/capture forbidden knowledge/publication-basis forbidden resource/balance forbidden
                      workflow-runs/prepare forbidden work/submit-prepared! forbidden]
          (schedules/tick-owned! host due))
        (is (= :skipped (:schedule.occurrence/state (current-occurrence conn row))))
        (is (= :unchanged (:schedule.occurrence/reason (current-occurrence conn row))))
        (is (zero? @calls))))))

(deftest invalid-owner-and-revoked-creator-refuse-before-hydration
  (with-occurrence true saved/knowledge-source
    (fn [{:keys [conn w host due row]}]
      (let [calls (atom 0) forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Unauthorized occurrence hydration")))]
        (with-redefs [reads/resolve-host-room! forbidden reads/resolve-host-read! forbidden
                      workflow-runs/prepare forbidden work/submit-prepared! forbidden]
          (is (some? (try (schedules/tick-owned! (assoc host ::reads/owner nil) due) nil (catch Exception e e))))
          (d/transact conn [[:db/retract [:room/id (:room w)] :room/owner [:party/id (:owner w)]]])
          (schedules/tick-owned! host due))
        (is (empty? (model/occurrences conn (:schedule.activation/id row))))
        (is (zero? @calls))))))

(deftest owner-expiry-after-admit-prevents-even-catch-reconciliation-writes
  (with-occurrence true saved/source
    (fn [{:keys [conn host due]}]
      (let [current (atom conn) admit model/admit! writes (atom 0)
            forbidden (fn [& _] (swap! writes inc) (throw (AssertionError. "Expired owner wrote old control DB")))]
        (with-redefs [system-db/get-conn (fn [] @current)
                      model/admit! (fn [& args] (let [claim (apply admit args)] (reset! current nil) claim))
                      model/finish-admission! forbidden model/block! forbidden work/submit-prepared! forbidden]
          (is (some? (try (schedules/tick-owned! host due) nil (catch Exception e e)))))
        (is (zero? @writes))))))

(deftest interruption-before-traversal-is-fatal-with-no-storage-work
  (let [calls (atom 0) interrupted? (atom nil) error (atom nil)]
    (try
      (.interrupt (Thread/currentThread))
      (with-redefs [reads/current-host-control-db! (fn [& _] (swap! calls inc) nil)
                    model/activations (fn [& _] (swap! calls inc) [])]
        (try (schedules/tick-owned! {} (java.util.Date.))
             (catch Exception e (reset! error e))))
      ;; Capture BEFORE clojure.test reporting, whose STM counters consume IRQ.
      (reset! interrupted? (.isInterrupted (Thread/currentThread)))
      (finally (Thread/interrupted)))
    (is (instance? InterruptedException @error))
    (is (true? @interrupted?))
    (is (zero? @calls))))

(deftest malformed-admit-and-selection-outcomes-cannot-reach-prepare
  (doseq [bad [false (random-uuid)]]
    (with-occurrence true saved/source
      (fn [{:keys [host due]}]
        (let [calls (atom 0) forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Unacknowledged claim prepared")))]
          (with-redefs [model/admit! (fn [& _] bad) reads/resolve-host-room! forbidden
                        workflow-runs/prepare forbidden work/submit-prepared! forbidden]
            (schedules/tick-owned! host due))
          (is (zero? @calls))))))
  (with-occurrence true saved/knowledge-source
    (fn [{:keys [host due]}]
      (let [select model/select-input! calls (atom 0)
            forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Unknown selection captured")))]
        (with-redefs [model/select-input! (fn [& args] (apply select args) :unknown)
                      knowledge/capture forbidden workflow-runs/prepare forbidden work/submit-prepared! forbidden]
          (schedules/tick-owned! host due))
        (is (zero? @calls))))))

(deftest foreign-completion-pointer-is-not-announced-or-hydrated
  (with-occurrence true saved/source
    (fn [{:keys [conn w host row due]}]
      (model/claim-due! conn row {:interval-ms 60000} due)
      (let [o (current-occurrence conn row)
            foreign (requests/request-id (:owner w) (:room w) (random-uuid))
            calls (atom 0) forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Foreign completion used Room")))]
        (d/transact conn [{:work.request/id foreign :work.request/by (:owner w) :work.request/room (:room w)
                           :work.request/client-id (random-uuid) :work.request/kind :workflow
                           :work.request/state :submitted :work.request/return-pending? false}])
        (model/finish-admission! conn (:schedule.occurrence/id o) :linked foreign nil)
        (with-redefs [reads/resolve-host-room! forbidden jobs/status forbidden discourse/post! forbidden]
          (schedules/tick-owned! host due))
        (is (zero? @calls))
        (is (not (:schedule.occurrence/announced? (model/occurrence conn (:schedule.occurrence/id o)))))))))

(defn- response [n]
  (let [tool? (odd? n)
        events (concat
                (when tool? [{:type "response.output_item.done"
                              :item {:type "function_call" :call_id (str "schedule-owned-" n) :name "clojure_eval"
                                     :arguments (json/write-value-as-string
                                                 {:code "(do (babashka.fs/create-dirs \"/out/wiki\") (spit \"/out/wiki/owned-proof.md\" (str (slurp \"/workflows/echo/helpers/data.txt\") \"|\" (slurp \"/in/knowledge/sources/wiki/context.md\"))))"})}}])
                (when-not tool? [{:type "response.output_text.delta" :delta "Scheduled owned proof."}])
                [{:type "response.completed" :response {:id (str "schedule-fixture-" n) :model "gpt-5.6-sol"
                                                        :usage {:input_tokens 10 :output_tokens 10}}}])]
    {:status 200 :body (java.io.ByteArrayInputStream.
                        (.getBytes (apply str (map #(str "data: " (json/write-value-as-string %) "\n\n") events)) "UTF-8"))}))

(defn- join-fixture-jobs! [room observed]
  (let [failure (volatile! nil) interrupted? (volatile! (Thread/interrupted))]
    (try
      (doseq [[id done] @observed]
        (try
          ;; This helper holds through the original Job/group gates even when
          ;; reporting a cleanup diagnostic. Do not skip the other Jobs.
          (@#'native/join-fixture-job! room id done)
          (catch Throwable error (when-not @failure (vreset! failure error)))
          (finally (when (Thread/interrupted) (vreset! interrupted? true)))))
      (when @failure (throw @failure))
      (finally (when @interrupted? (.interrupt (Thread/currentThread)))))))

(defn- stop-fixture-runtime! [room observed]
  (let [original @(var-get #'runtime/service)
        executor (:executor original)
        failure (volatile! nil) interrupted? (volatile! (Thread/interrupted))
        remember! (fn [error] (when-not @failure (vreset! failure error)))]
    (try
      (try (runtime/stop!) (catch Throwable error (remember! error)))
      (when (Thread/interrupted) (vreset! interrupted? true))
      (when executor
        (try (.shutdownNow ^java.util.concurrent.ExecutorService executor)
             (catch Throwable error (remember! error)))
        ;; A timeout is a failed diagnostic, never physical return. Keep the
        ;; exact I/O handles and provider roots until the worker actually exits.
        (loop []
          (let [terminated? (try (.awaitTermination ^java.util.concurrent.ExecutorService executor
                                                    30 java.util.concurrent.TimeUnit/SECONDS)
                                 (catch InterruptedException error
                                   (vreset! interrupted? true) (remember! error) false))]
            (when-not terminated?
              (remember! (ex-info "Fixture scheduler has not physically stopped" {}))
              (recur))))
        ;; Never release or stop a replacement owner's resource slot.
        (when (identical? executor (:executor @(var-get #'runtime/service)))
          (try (runtime/stop!) (catch Throwable error (remember! error)))))
      (finally
        ;; Terminating the scheduler closes the set of observed native Jobs.
        ;; Always join all of them before restoring any provider/function root.
        (try (join-fixture-jobs! room observed)
             (catch Throwable error (remember! error))
             (finally (when (Thread/interrupted) (vreset! interrupted? true))
                      (when @interrupted? (.interrupt (Thread/currentThread)))))))
    (when @failure (throw @failure))))

(deftest actual-owned-clock-worker-dispatches-and-files-through-the-canonical-kernel
  (with-occurrence false saved/knowledge-source
    (fn [{:keys [conn host room row due]}]
      (let [root (rctx/root-ctx (:execution-ctx host))
            signal (binding [ec/*execution-context* root] (sig/signal (java.util.Date. 0)))
            first-pass (promise) completion-pass (promise) passes (atom 0)
            observed (atom {}) calls (atom 0) start jobs/start! tick schedules/tick-owned!
            provider (codex/create {:credentials (gateway/static-credentials :fixture {} #{"https://chatgpt.com"})})]
        ;; Only the private I/O slot, clock and HTTP are isolated. Original
        ;; owner/EACL, actual recovery/tick, Job/SCI/certification/filing run.
        (with-redefs-fn {#'runtime/service (atom nil)}
          #(with-redefs [clock/clock-signal (constantly signal) clock/minute-now (constantly due)
                         gateway/*request-fn* (fn [& _] (response (swap! calls inc)))
                         providers/providers (atom {:codex-subscription provider})
                         schedules/tick-owned! (fn [h now]
                                                 (let [result (tick h now)]
                                                   (case (swap! passes inc)
                                                     1 (deliver first-pass true)
                                                     2 (deliver completion-pass true)
                                                     nil)
                                                   result))
                         jobs/start! (fn [actual-room options actual-work & callbacks]
                                       (is (empty? (get @(:meta actual-room) preparation/leases-key))
                                           "Source/balance leases must end before the native Job/fork")
                                       (apply start actual-room options
                                              (fn [id]
                                                (when (identical? actual-room room)
                                                  (swap! observed assoc id (get-in @(var-get #'jobs/live) [id :done])))
                                                (actual-work id)) callbacks))]
             (try
               (is (= :started (runtime/start-owned! host)))
               (is (true? (deref first-pass 40000 ::timeout)))
               (let [o (current-occurrence conn row)
                     rid (:schedule.occurrence/request o)
                     job (:work.request/job (requests/get-request conn rid))]
                 (is (uuid? job))
                 (is (= :completed (:status (jobs/status job 40000))))
                 (@#'native/join-fixture-job! room job (get @observed job))
                 (binding [ec/*execution-context* root]
                   (reset! signal (java.util.Date. (inc (.getTime ^java.util.Date due)))))
                 (is (true? (deref completion-pass 40000 ::timeout)))
                 (is (some? (dispositions/disposition (requests/get-request conn rid))))
                 (is (some? (model/processing-proof conn (:schedule.occurrence/id o))))
                 (is (nil? (:schedule.activation/active-occurrence
                            (model/activation conn (:schedule.activation/id row)))))
                 (is (= (:input-id (model/retained-owned-input conn (model/occurrence conn (:schedule.occurrence/id o))))
                        (:schedule.activation/last-processed-input (model/activation conn (:schedule.activation/id row))))))
               (is (= 2 @calls))
               (is (= 1 (count @observed)))
               (finally
                 ;; Real stop first, THEN force a diagnostic. Cleanup must
                 ;; still call the original native joins and rethrow that
                 ;; diagnostic, not convert it into a successful shutdown.
                 (let [stop runtime/stop! join @#'native/join-fixture-job!
                       diagnostic (ex-info "Fixture post-stop diagnostic" {}) joined (atom #{})]
                   (with-redefs-fn {#'runtime/stop! (fn [] (stop) (throw diagnostic))
                                    #'native/join-fixture-job! (fn [r id done]
                                                                 (swap! joined conj id)
                                                                 (join r id done))}
                     (fn [] (is (identical? diagnostic
                                            (try (stop-fixture-runtime! room observed) nil
                                                 (catch Throwable error error))))))
                   (is (= (set (keys @observed)) @joined)))))))))))

(deftest real-scheduled-jobs-use-code-A-with-K1-then-K2-and-file-once-per-occurrence
  (doseq [order [true false]]
    (with-occurrence order saved/knowledge-source
      (fn [{:keys [conn host owned attached room row due]}]
        (text! owned "workflows/echo/helpers/data.txt" "Parent B must remain B")
        (let [calls (atom 0) observed (atom {}) start jobs/start!
              provider (codex/create {:credentials (gateway/static-credentials :fixture {} #{"https://chatgpt.com"})})
              selected (workspace/selected-system-id) other (state attached)]
          (with-redefs [gateway/*request-fn* (fn [& _] (response (swap! calls inc)))
                        providers/providers (atom {:codex-subscription provider})
                        jobs/start! (fn [actual-room options actual-work & callbacks]
                                      (is (empty? (get @(:meta actual-room) preparation/leases-key))
                                          "No source lease may cross native fork admission")
                                      (apply start actual-room options
                                             (fn [id]
                                               (when (identical? actual-room room)
                                                 (swap! observed assoc id (get-in @(var-get #'jobs/live) [id :done])))
                                               (actual-work id)) callbacks))]
            (try
              (doseq [[epoch now] [[1 due] [2 (java.util.Date. (+ 60000 (.getTime ^java.util.Date due)))]]]
                (when (= epoch 2)
                  ;; Each admission reserves its full upper bound. The first
                  ;; execution consumed some allowance; explicitly fund the
                  ;; second instead of relying on a full reservation refund.
                  (resource/mint! room {:id (random-uuid)
                                        :resources (work/resources (:limits (saved/input saved/knowledge-source)))})
                  (text! owned "inbox/evidence.md" "Owned trigger K2")
                  (text! owned "wiki/context.md" "Owned context K2"))
                (let [before (state owned)]
                  (schedules/tick-owned! host now)
                  (let [o (current-occurrence conn row)
                        rid (:schedule.occurrence/request o)
                        request (when rid (requests/get-request conn rid))
                        job (:work.request/job request)
                        status (when job (jobs/status job 40000))]
                    (is (uuid? job))
                    (is (contains? @observed job))
                    (is (= :completed (:status status)) (pr-str (select-keys status [:status :error])))
                    (when job (@#'native/join-fixture-job! room job (get @observed job)))
                    (is (false? (:work.request/return-pending? (requests/get-request conn rid))))
                    ;; The actual scheduler, not a test cursor/filer shortcut.
                    (schedules/tick-owned! host now)
                    (let [after (model/activation conn (:schedule.activation/id row))
                          actual (model/occurrence conn (:schedule.occurrence/id o))
                          receipt (dispositions/disposition (requests/get-request conn rid))
                          proof (model/processing-proof conn (:schedule.occurrence/id o))
                          cursor-fact (fn [] (some (fn [datom]
                                                     (when (= (:db/id after) (:e datom))
                                                       {:value (:v datom) :tx (:tx datom)}))
                                                   (d/datoms @conn :aevt :schedule.activation/last-processed-input)))
                          fact (cursor-fact)
                          attempts (store/-list-attempts (:store room) (store/conversation-id room) {})
                          certified (binding [ec/*execution-context* (:ctx room)]
                                      (first (filter #(= job (:run/parent %)) (run/runs room {:root-run-id job :limit 10}))))
                          value (when certified (episode/attempt room (:run/id certified)))]
                      (is (uuid? (:value fact))) (is (some? (:tx fact)))
                      (is (some? receipt)) (is (some? proof))
                      (is (= (:input-id (model/retained-owned-input conn actual)) (:schedule.activation/last-processed-input after)))
                      (is (nil? (:schedule.activation/active-occurrence after)))
                      (is (true? (:schedule.occurrence/announced? actual)))
                      (is (= (str "pinned helper|Owned context K" epoch)
                             (get-in value [:attempt/evidence :files "/out/wiki/owned-proof.md"])))
                      (is (= epoch (count attempts)))
                      ;; Simulate only a crash between durable filing/announcement
                      ;; and pointer release. Jobs are already physically joined;
                      ;; this proves dependency-unavailability, not actual TTL/GC.
                      (let [lookups (atom 0)
                            unavailable (fn [& _] (swap! lookups inc) (throw (AssertionError. "Durable filing reread Job/Attempt")))
                            cards (count (discourse/messages room))]
                        (d/transact conn [[:db/add [:schedule.activation/id (:schedule.activation/id row)]
                                           :schedule.activation/active-occurrence (:schedule.occurrence/id o)]])
                        (with-redefs [jobs/status unavailable episode/attempt unavailable]
                          (schedules/tick-owned! host now))
                        (is (zero? @lookups))
                        (is (nil? (:schedule.activation/active-occurrence (model/activation conn (:schedule.activation/id row)))))
                        (is (= proof (model/processing-proof conn (:schedule.occurrence/id o))))
                        (is (= fact (cursor-fact)))
                        (is (= cards (count (discourse/messages room)))))
                      (let [cursor (:schedule.activation/last-processed-input after)
                            cards (count (discourse/messages room))]
                        (dotimes [_ 2] (schedules/tick-owned! host now))
                        (is (= cursor (:schedule.activation/last-processed-input (model/activation conn (:schedule.activation/id row)))))
                        (is (= fact (cursor-fact)))
                        (is (= proof (model/processing-proof conn (:schedule.occurrence/id o))))
                        (is (= cards (count (discourse/messages room))))))
                    (is (= before (state owned)))
                    (is (= other (state attached)))
                    (is (= selected (workspace/selected-system-id))))))
              (is (= 4 @calls))
              (is (= 2 (count @observed)))
              (finally
                ;; All original Job/group gates join while transport/provider
                ;; roots are still held, including uncertain admission returns.
                (join-fixture-jobs! room observed)))))))))
