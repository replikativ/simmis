(ns is.simm.ops.workflow-schedules-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [dvergr.agent.attempt :as attempt]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.roster :as roster]
            [dvergr.agent.run :as run]
            [dvergr.catalog.room :as catalog]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.jobs :as jobs]
            [dvergr.resource :as resource]
            [dvergr.room.store.datahike :as room-store]
            [dvergr.room.store.memory :as memory-store]
            [dvergr.room.store :as store]
            [dvergr.model.registry :as models]
            [dvergr.substrate.geschichte :as geschichte]
            [geschichte.content :as content]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.ops.knowledge-inputs :as knowledge-inputs]
            [is.simm.ops.knowledge-admission :as knowledge-admission]
            [is.simm.ops.workflow-runs-test :as workflow-fixture]
            [is.simm.model.work-requests :as requests]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.workflow-schedules :as model]
            [is.simm.ops.dvergr :as principals]
            [is.simm.ops.work :as work]
            [is.simm.ops.knowledge-publish :as knowledge-publish]
            [is.simm.ops.workflow-runs :as workflow-runs]
            [is.simm.ops.workflow-schedules :as schedules]
            [is.simm.ops.workflows :as workflows]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]))

(def fixture-policy {:version 1 :fixture true})

(deftest external-cadences-normalize-to-the-actual-cron-runtime-contract
  (is (= {:every-ms 60000 :tz "UTC"} (#'schedules/cadence! {:interval-ms 60000 :tz "UTC"})))
  (is (= {:every :week :on :monday :at "09:00" :tz "Europe/Berlin"}
         (#'schedules/cadence! {:every "week" :on "monday" :at "09:00" :tz "Europe/Berlin"})))
  (doseq [cadence [{:interval-ms 60001 :tz "UTC"}
                   {:interval-ms 60000 :tz "Not/AZone"}
                   {:every "day" :at "25:00" :tz "UTC"}
                   {:interval-ms 60000 :every "day" :at "09:00" :tz "UTC"}]]
    (is (= :is.simm.ops.workflow-schedules/invalid
           (try (#'schedules/cadence! cadence) nil
                (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))
(def source {"workflow.edn" "{:title \"Scheduled echo\" :task \"Write /out/brief.md\" :capture [\"/out\"]}"
             "fixtures/docs/input.md" "Pinned market evidence"
             "checker.clj" "(ns fixture.checker) (defn check [_] {:checks {:ok true} :reward 1.0})"})

(defn- with-fixture [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false}
        ctx (context/create-execution-context)
        owner (random-uuid) room-id (random-uuid)]
    (binding [ec/*execution-context* ctx]
      (d/create-database cfg)
      (let [conn (d/connect cfg)]
        (try
          (d/transact conn (concat model/schema requests/schema dispositions/schema))
          (let [target {:version 1 :kind :workflow :cadence {:interval-ms 60000 :tz "UTC"}
                        :snapshot {:files source :manifest []} :host-policy fixture-policy
                        :execution {:name "echo" :expected-bundle (:id (catalog/bundle "echo" source))
                                    :model "codex-subscription-luna-6"
                                    :limits {:budget-microdollars 1000 :max-dispatches 2
                                             :max-model-steps 2 :timeout-ms 30000 :settlement "review"}}}
                row (model/create! conn {:id (random-uuid) :room room-id :by owner :target target
                                         :now (java.util.Date. 0) :next-fire (java.util.Date. 60000)})]
            (with-redefs [schedules/host-policy (constantly fixture-policy)
                          schedules/model-policy (constantly nil)
                          schedules/publish-completion! (fn [& _] nil)
                          work/authorize! (fn [& _] owner)
                          resource/balance (fn [_] (work/resources (get-in target [:execution :limits])))
                          jobs/status (constantly nil)]
              (f {:conn conn :ctx ctx :owner owner :room-id room-id :room {:ctx ctx}
                  :row row :target target})))
          (finally
            (d/release conn)
            (context/close-context! ctx)))))))

(defn- pending! [{:keys [conn row target]}]
  (model/claim-due! conn row (:cadence target) (java.util.Date. 60000))
  (first (model/occurrences conn (:schedule.activation/id row))))

(defn- record-request! [{:keys [conn owner room-id target environment]} occurrence submitted?]
  (let [o (model/occurrence conn (:schedule.occurrence/id occurrence))
        retained (when (:schedule.occurrence/input-ref o) (model/retained-input conn o))
        basis (:publication-basis retained)
        {:keys [request]} (requests/claim!
                           conn {:party owner :room room-id :client-id (:schedule.occurrence/id occurrence)
                                 :kind :workflow :intent (cond-> {:submission (:execution target)}
                                                           environment (assoc :environment environment)
                                                           basis (assoc :knowledge/publication-basis basis
                                                                        :workflow {:input-selection
                                                                                   {:execution-id (get-in retained [:captured :execution-id])}}))
                                 :limits (update (get-in target [:execution :limits]) :settlement keyword)})]
    (if submitted?
      (requests/record-job! conn (:work.request/id request) (:work.request/claim request) (random-uuid))
      request)))

(defn- fixture-file!
  "Orchestration double ONLY. Real host certification/Proposal storage has its
   own integration tests. These schedules use deliberately synthetic Attempts;
   do not mistake a typed fixture receipt for host filing authority."
  [conn _principal _room-id room rid]
  (let [request (requests/get-request conn rid)]
    (or (dispositions/disposition request)
        (do
          (when-not (#'schedules/input-processed? conn request room)
            (throw (ex-info "Synthetic candidate did not pass" {:type ::uncertified})))
          (let [value (dispositions/make
                       {:request rid :intent (:work.request/intent-id request)
                        :basis (:work.request/publication-basis-id request)
                        :job (:work.request/job request) :candidate (random-uuid)
                        :attempt-content (random-uuid) :output (random-uuid) :kind :no-op})]
            (d/transact conn [[:db.fn/call dispositions/tx-data
                               (dispositions/request-binding request) value]])
            value)))))

(defn- tick! [{:keys [conn ctx room]} millis]
  (schedules/tick! conn ctx (constantly room) (java.util.Date. millis)))

(defn- with-input-fixture [f]
  (with-fixture
    (fn [{:keys [conn ctx owner room-id row target] :as fixture}]
      (model/set-enabled! conn (:schedule.activation/id row) false)
      (let [room (discourse/make-room {:id (keyword (str "scheduled-inputs-" (random-uuid)))
                                       :ctx ctx :store (memory-store/make)})
            source (assoc source "input-selection.edn"
                          (pr-str {:kind :knowledge :policy knowledge-inputs/default-policy}))
            target (-> target (assoc :snapshot {:files source :manifest []})
                       (assoc-in [:execution :expected-bundle] (:id (catalog/bundle "echo" source))))
            row (model/create! conn {:id (random-uuid) :room room-id :by owner :target target
                                     :now (java.util.Date. 0) :next-fire (java.util.Date. 60000)})]
        (try
          (workspace/ensure-workspace! room)
          (with-redefs [knowledge-admission/default-target workflow-fixture/fixture-default-target
                        knowledge-publish/file-completed-request! fixture-file!]
            (f (assoc fixture :room room :target target :row row
                      :environment (environment/make-environment
                                    {:id :scheduled-fixture :task "Process evidence"
                                     :verifier {:id :completed :version 1}}))))
          (finally (discourse/close-room! room)))))))

(defn- write-source! [{:keys [room]} path text]
  (binding [ec/*execution-context* (:ctx room)]
    (repo/write! (:conn (geschichte/current-workspace)) path (.getBytes ^String text "UTF-8"))))

(defn- certify-candidate! [{:keys [conn room environment owner]} occurrence status passed?]
  (let [request (requests/get-request conn (:schedule.occurrence/request occurrence))
        job-id (:work.request/job request)
        agent (roster/agent (roster/make-agent (roster/make-roster {:id :fixture})
                                               {:id :candidate :program {:kind :echo}}) :candidate)
        metrics {:agent-def-hash (hasch/uuid agent) :agent-version (:agent/version agent)
                 :program-kind :echo :interpreter-version 1}]
    (binding [ec/*execution-context* (:ctx room)]
      (run/start! room (:principal/actor (principals/party-principal owner))
                  (random-uuid) nil {:id job-id :kind :workflow})
      (let [candidate (run/start! room :fixture (random-uuid) nil
                                  {:kind :attempt :parent job-id
                                   :provenance (into {} (map (fn [[k v]] [(keyword "run" (name k)) v])) metrics)})
            id (:run/id candidate)
            receipt (environment/make-attempt-receipt
                     environment {:run-id id :provider :fixture :model "fixture"
                                  :status status :started-at 0 :elapsed-ms 1 :metrics metrics
                                  :checks {:completed? passed?} :reward (if passed? 1.0 0.0)})]
        (run/finish! id status)
        (store/-store-attempt! (:store room) (store/conversation-id room)
                               (attempt/make-attempt environment agent receipt {} :review))
        (run/finish! job-id :completed)))))

(defn- with-completed-input [f]
  (with-input-fixture
    (fn [{:keys [conn row] :as fixture}]
      (write-source! fixture "inbox/source.md" "Synthetic evidence for schedule storage tests")
      (let [calls (atom 0)]
        (with-redefs [workflow-runs/submit!
                      (fn [_ _ _ _ _ input _ _]
                        (swap! calls inc)
                        (record-request! fixture {:schedule.occurrence/id (:request-id input)} true))]
          (tick! fixture 60000)
          (let [o (first (model/occurrences conn (:schedule.activation/id row)))]
            (certify-candidate! fixture o :completed true)
            (requests/mark-returned! conn (:schedule.occurrence/request o))
            (f (assoc fixture :occurrence o :calls calls
                      :request (requests/get-request conn (:schedule.occurrence/request o))))))))))

(defn- assert-held! [{:keys [conn row occurrence calls]}]
  (let [a (model/activation conn (:schedule.activation/id row))]
    (is (= (:schedule.occurrence/id occurrence) (:schedule.activation/active-occurrence a)))
    (is (nil? (:schedule.activation/last-processed-input a)))
    (is (= 1 @calls))))

(deftest missing-live-bindings-never-fall-back-to-generic-attempt-processing
  (doseq [missing [:request :retained]]
    (with-completed-input
      (fn [{:keys [conn request] :as fixture}]
        (when (= missing :request)
          (d/transact conn [[:db/retract [:work.request/id (:work.request/id request)]
                             :work.request/publication-basis-id (:work.request/publication-basis-id request)]]))
        (let [read-retained model/retained-input]
          (with-redefs-fn
            {#'model/retained-input (fn [conn o]
                                      (cond-> (read-retained conn o)
                                        (= missing :retained) (dissoc :publication-basis)))
             #'knowledge-publish/file-completed-request!
             (fn [& _] (throw (AssertionError. "Unbound knowledge reached filing")))
             #'schedules/input-processed?
             (fn [& _] (throw (AssertionError. "Unbound knowledge downgraded to fixture")))
             #'jobs/status (constantly {:status :completed :settled? true})}
            #(tick! fixture 120000)))
        (assert-held! fixture)))))

(deftest filed-and-announced-recovery-needs-neither-job-nor-attempt
  (with-completed-input
    (fn [{:keys [conn row occurrence calls] :as fixture}]
      (let [first? (atom true)]
        (with-redefs [jobs/status (constantly {:status :completed :settled? true})
                      schedules/publish-completion!
                      (fn [conn _ o _ _ _] (model/mark-announced! conn (:schedule.occurrence/id o)))
                      knowledge-publish/file-completed-request!
                      (fn [& args]
                        (let [value (apply fixture-file! args)]
                          ;; The durable filing exists, but chat publication's
                          ;; response failed before the host helper returned.
                          (if (compare-and-set! first? true false)
                            (throw (ex-info "Synthetic chat publication failure" {}))
                            value)))]
          (tick! fixture 120000)
          (assert-held! fixture)
          (model/set-enabled! conn (:schedule.activation/id row) false)
          (with-redefs [jobs/status (fn [& _] (throw (AssertionError. "Historical retry read Job")))
                        run/run (fn [& _] (throw (AssertionError. "Historical retry read Run")))
                        schedules/input-processed?
                        (fn [& _] (throw (AssertionError. "Historical retry recertified Attempt")))]
            (tick! fixture 180000))
          (let [a (model/activation conn (:schedule.activation/id row))]
            (is (= (:schedule.occurrence/input-id occurrence) (:schedule.activation/last-processed-input a)))
            (is (nil? (:schedule.activation/active-occurrence a)))
            (is (false? (:schedule.activation/enabled? a)))
            (is (= 1 @calls))))))))

(deftest a-different-self-hashed-receipt-cannot-replace-the-host-filing-result
  (with-completed-input
    (fn [{:keys [conn] :as fixture}]
      (with-redefs [jobs/status (constantly {:status :completed :settled? true})
                    knowledge-publish/file-completed-request!
                    (fn [& args]
                      (let [value (apply fixture-file! args)
                            changed (dispositions/make
                                     (assoc (dissoc value :id :version) :output (random-uuid)))]
                        ;; Coherently rehashed storage corruption, not just an
                        ;; invalid digest: it still wasn't the host's result.
                        (d/transact conn [(into {:db/id [:work.request/id (:request value)]}
                                                (map (fn [[k v]] [(keyword "knowledge.disposition" (name k)) v]))
                                                changed)])
                        value))]
        (tick! fixture 120000))
      (assert-held! fixture))))

(deftest cursor-write-failure-retries-the-filing-not-the-execution
  (with-completed-input
    (fn [{:keys [conn row occurrence calls] :as fixture}]
      (let [mark model/mark-input-processed! first? (atom true)]
        (with-redefs [jobs/status (constantly {:status :completed :settled? true})
                      schedules/publish-completion!
                      (fn [conn _ o _ _ _] (model/mark-announced! conn (:schedule.occurrence/id o)))
                      model/mark-input-processed!
                      (fn [& args]
                        (if (compare-and-set! first? true false)
                          (throw (ex-info "Synthetic cursor write failure" {}))
                          (apply mark args)))]
          (tick! fixture 120000)
          (assert-held! fixture)
          (is (some? (model/processing-proof conn (:schedule.occurrence/id occurrence))))
          (model/set-enabled! conn (:schedule.activation/id row) false)
          (with-redefs [jobs/status (fn [& _] (throw (AssertionError. "Cursor retry read Job")))]
            (tick! fixture 180000))
          (let [a (model/activation conn (:schedule.activation/id row))]
            (is (= (:schedule.occurrence/input-id occurrence) (:schedule.activation/last-processed-input a)))
            (is (nil? (:schedule.activation/active-occurrence a)))
            (is (= 1 @calls))))))))

(deftest an-unannounced-filing-without-a-completion-record-holds
  (with-completed-input
    (fn [{:keys [conn room-id room request] :as fixture}]
      (fixture-file! conn nil room-id room (:work.request/id request))
      (with-redefs [jobs/status (constantly nil) run/run (constantly nil)
                    knowledge-publish/file-completed-request!
                    (fn [& _] (throw (AssertionError. "Missing completion timestamp was invented")))]
        (tick! fixture 120000))
      (assert-held! fixture))))

(deftest a-pending-physical-return-never-enters-filing
  (with-completed-input
    (fn [{:keys [conn request] :as fixture}]
      (d/transact conn [[:db/add [:work.request/id (:work.request/id request)] :work.request/return-pending? true]])
      (with-redefs [jobs/status (fn [& _] (throw (AssertionError. "Pending return entered completion")))
                    knowledge-publish/file-completed-request!
                    (fn [& _] (throw (AssertionError. "Pending return entered filing")))]
        (tick! fixture 120000))
      (assert-held! fixture))))

(deftest explicitly-retained-fixture-inputs-are-not-live-publication
  (with-completed-input
    (fn [{:keys [conn target occurrence request]}]
      (let [retained (model/retained-input conn occurrence)
            fixture-target (assoc-in target [:execution :inputs] (get-in retained [:captured :inputs]))]
        (with-redefs [model/retained-input (constantly (assoc retained :publication-basis nil))]
          (is (not (#'schedules/publication-required?
                    conn fixture-target occurrence (dissoc request :work.request/publication-basis-id)))))))))

(deftest cache-loss-recovers-from-the-owned-durable-root-only
  (doseq [change [nil [:run/actor :other] [:run/room :other] [:run/kind :task]
                  [:run/parent (random-uuid)] [:run/ended-at nil]]]
    (with-completed-input
      (fn [{:keys [conn row request calls] :as fixture}]
        (let [read-run run/run]
          (with-redefs [jobs/status (constantly nil)
                        run/run (fn [room id]
                                  (cond-> (read-run room id)
                                    (and change (= id (:work.request/job request)))
                                    (assoc (first change) (second change))))]
            (tick! fixture 120000)))
        (if change
          (assert-held! fixture)
          (let [a (model/activation conn (:schedule.activation/id row))]
            (is (uuid? (:schedule.activation/last-processed-input a)))
            (is (nil? (:schedule.activation/active-occurrence a)))
            (is (= 1 @calls))))))))

(deftest empty-selection-skips-before-body-allowance-provider-and-job-work
  (with-input-fixture
    (fn [{:keys [conn row] :as fixture}]
      (write-source! fixture "wiki/context.md" "Knowledge context without new evidence")
      (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Skip read a body")))
                    resource/balance (fn [& _] (throw (AssertionError. "Skip checked allowance")))
                    work/prepare (fn [& _] (throw (AssertionError. "Skip prepared a provider")))
                    workflow-runs/submit! (fn [& _] (throw (AssertionError. "Skip submitted a job")))]
        (tick! fixture 60000)
        (let [o (first (model/occurrences conn (:schedule.activation/id row)))]
          (is (= :skipped (:schedule.occurrence/state o)))
          (is (= :no-inputs (:schedule.occurrence/reason o)))
          (is (nil? (:schedule.occurrence/input-ref o)))
          (is (nil? (:schedule.activation/active-occurrence (model/activation conn (:schedule.activation/id row)))))
          (is (empty? (requests/requests conn))))))))

(deftest completed-evidence-skips-with-no-money-even-after-supporting-knowledge-edits
  (with-input-fixture
    (fn [{:keys [conn row] :as fixture}]
      (write-source! fixture "inbox/source.md" "Original evidence")
      (let [calls (atom 0)]
        (with-redefs [workflow-runs/submit!
                      (fn [_ _ _ _ _ input _ resolution]
                        (swap! calls inc)
                        (is (= "Original evidence" (get-in resolution [:captured :inputs "/in/knowledge/sources/inbox/source.md"])))
                        (is (uuid? (get-in resolution [:publication-basis :id])))
                        (record-request! fixture {:schedule.occurrence/id (:request-id input)} true))]
          (tick! fixture 60000)
          (let [o (first (model/occurrences conn (:schedule.activation/id row)))
                retained (model/retained-input conn o)]
            (is (= :linked (:schedule.occurrence/state o)))
            (is (uuid? (:schedule.occurrence/input-revision o)))
            (is (= (:schedule.activation/revision row) (:target-revision retained)))
            (certify-candidate! fixture o :completed true)
            (requests/mark-returned! conn (:schedule.occurrence/request o)))
          (write-source! fixture "wiki/context.md" "Adopted supporting knowledge must not trigger a loop")
          (with-redefs [jobs/status (constantly {:status :completed :settled? true})
                        resource/balance (fn [& _] (throw (AssertionError. "Unchanged evidence checked allowance")))
                        content/read-by-id (fn [& _] (throw (AssertionError. "Unchanged evidence loaded source bytes")))]
            (tick! fixture 120000))
          (let [a (model/activation conn (:schedule.activation/id row))
                [latest prior] (model/occurrences conn (:schedule.activation/id row))]
            (is (= 1 @calls))
            (is (= :unchanged (:schedule.occurrence/reason latest)))
            (is (= :skipped (:schedule.occurrence/state latest)))
            (is (= (:schedule.occurrence/input-id prior) (:schedule.activation/last-processed-input a)))
            (is (:schedule.activation/enabled? a))
            (is (= 1 (count (requests/requests conn))))))))))

(deftest failed-execution-holds-without-advancing-processing-and-explicit-resume-retries
  (with-input-fixture
    (fn [{:keys [conn ctx owner room-id row] :as fixture}]
      (write-source! fixture "inbox/source.md" "Evidence")
      (let [calls (atom 0)]
        (with-redefs [workflow-runs/submit!
                      (fn [_ _ _ _ _ input _ _]
                        (swap! calls inc)
                        (record-request! fixture {:schedule.occurrence/id (:request-id input)} true))]
          (tick! fixture 60000)
          (let [o (first (model/occurrences conn (:schedule.activation/id row)))]
            (requests/mark-returned! conn (:schedule.occurrence/request o)))
          (with-redefs [jobs/status (constantly {:status :failed :settled? true})]
            (tick! fixture 120000)
            (tick! fixture 180000))
          (let [a (model/activation conn (:schedule.activation/id row))]
            (is (= 1 @calls))
            (is (false? (:schedule.activation/enabled? a)))
            (is (= :execution-failed (:schedule.activation/reason a)))
            (is (nil? (:schedule.activation/last-processed-input a))))
          (schedules/set-enabled! conn ctx (principals/party-principal owner) room-id (:schedule.activation/id row) true)
          (tick! fixture 240000)
          (is (= 2 @calls)))))))

(deftest completed-job-with-failed-uncertified-or-wrong-environment-attempt-never-advances-input
  (doseq [[status passed? wrong-environment?]
          [[:failed false false] [:completed false false] [nil nil false] [:completed true true]]]
    (with-input-fixture
      (fn [{:keys [conn row] :as fixture}]
        (write-source! fixture "inbox/source.md" "Evidence must remain retryable")
        (with-redefs [workflow-runs/submit!
                      (fn [_ _ _ _ _ input _ _]
                        (record-request! fixture {:schedule.occurrence/id (:request-id input)} true))]
          (tick! fixture 60000)
          (let [o (first (model/occurrences conn (:schedule.activation/id row)))]
            (when status
              (certify-candidate!
               (cond-> fixture
                 wrong-environment?
                 (assoc :environment (environment/make-environment
                                      {:id :different-task :task "Unrelated work"
                                       :verifier {:id :completed :version 1}})))
               o status passed?))
            (requests/mark-returned! conn (:schedule.occurrence/request o)))
          (with-redefs [jobs/status (constantly {:status :completed :settled? true})]
            (tick! fixture 120000))
          (let [a (model/activation conn (:schedule.activation/id row))]
            ;; A completed Job whose output cannot be certified remains held
            ;; for filing/reconciliation; it is never automatically rerun.
            (is (:schedule.activation/enabled? a))
            (is (uuid? (:schedule.activation/active-occurrence a)))
            (is (nil? (:schedule.activation/last-processed-input a)))))))))

(deftest an-activation-without-the-default-target-policy-refuses-before-capture
  (let [current (schedules/host-policy)
        old (update current :input-selection dissoc :publication-target)]
    (is (= :owned-default-v1 (get-in current [:input-selection :publication-target])))
    (is (= :durable-filing-v1 (get-in current [:input-selection :processing])))
    (with-fixture
      (fn [{:keys [conn ctx row target] :as fixture}]
        (model/set-enabled! conn (:schedule.activation/id row) false)
        (let [target (assoc target :host-policy old)
              activation (model/create! conn {:id (random-uuid) :room (:room-id fixture) :by (:owner fixture)
                                              :target target :now (java.util.Date. 0)
                                              :next-fire (java.util.Date. 60000)})]
          (with-redefs [schedules/host-policy (constantly current)
                        workflow-runs/with-admission-selection (fn [& _] (throw (AssertionError. "Obsolete activation captured inputs")))
                        workflow-runs/submit! (fn [& _] (throw (AssertionError. "Obsolete activation submitted a Run")))]
            (tick! fixture 60000))
          (let [after (model/activation conn (:schedule.activation/id activation))]
            (is (false? (:schedule.activation/enabled? after)))
            (is (= :is.simm.ops.workflow-schedules/policy-changed (:schedule.activation/reason after)))
            (is (empty? (requests/requests conn)))))))))

(deftest deployment-pins-live-input-policy-without-collecting-or-preparing-a-provider
  (with-input-fixture
    (fn [{:keys [conn ctx owner room-id room target]}]
      (with-redefs [models/get-model (fn [id] {:id id})
                    workflows/snapshot (fn [& _] (:snapshot target))
                    workflow-runs/with-admission-selection (fn [& _] (throw (AssertionError. "Deployment captured live evidence")))
                    work/prepare (fn [& _] (throw (AssertionError. "Deployment prepared a provider")))]
        (let [row (schedules/create! conn ctx (principals/party-principal owner) room-id room
                                     {:request-id (random-uuid) :kind "workflow"
                                      :cadence {:interval-ms 60000 :tz "UTC"} :execution (:execution target)})]
          (is (uuid? (:schedule.activation/id row)))
          (is (= (:snapshot target) (:snapshot (model/target conn row)))))))))

(deftest completion-publication-retries-the-same-envelope-after-a-lost-acknowledgement
  ;; Save the real function before the ordinary fixture stubs room delivery.
  (let [publish! schedules/publish-completion!]
    (with-fixture
      (fn [{:keys [conn row room] :as fixture}]
        (let [o (pending! fixture) messages (atom []) mark! model/mark-announced!
              failed? (atom false) request {:work.request/job (random-uuid)}]
          (with-redefs [discourse/post! (fn [_ message] (swap! messages conj message))
                        model/mark-announced! (fn [conn oid]
                                                (if (compare-and-set! failed? false true)
                                                  (throw (ex-info "Lost publication acknowledgement" {}))
                                                  (mark! conn oid)))]
            (is (thrown? clojure.lang.ExceptionInfo (publish! conn row o request {:status :completed :finished-at #inst "2026-10-19T08:00:00Z"} room)))
            (publish! conn row o request {:status :completed :finished-at #inst "2026-10-19T08:00:00Z"} room)
            (is (= 2 (count @messages)))
            (is (= (first @messages) (second @messages)))
            (is (= (.getTime #inst "2026-10-19T08:00:00Z") (:ts (first @messages))))
            (is (= :_room-log (:to (first @messages))))
            (is (= (:work.request/job request) (get-in (first @messages) [:metadata :run-id])))
            (is (true? (:schedule.occurrence/announced? (model/occurrence conn (:schedule.occurrence/id o)))))))))))

(deftest recovery-links-committed-request-without-resubmission
  (with-fixture
    (fn [{:keys [conn ctx row] :as fixture}]
      (let [o (pending! fixture) _ (model/admit! conn (:schedule.occurrence/id o))
            request (record-request! fixture o true)]
        (with-redefs [workflow-runs/submit! (fn [& _] (throw (AssertionError. "Recovery must not submit")))]
          (schedules/recover! conn ctx)
          (schedules/recover! conn ctx)
          (let [after (model/occurrence conn (:schedule.occurrence/id o))]
            (is (= :linked (:schedule.occurrence/state after)))
            (is (= (:work.request/id request) (:schedule.occurrence/request after)))
            (is (:schedule.activation/enabled? (model/activation conn (:schedule.activation/id row))))))))))

(deftest a-real-datahike-room-deduplicates-completion-publication-after-a-lost-ack
  (let [publish! schedules/publish-completion!]
    (with-fixture
      (fn [{:keys [conn ctx row] :as fixture}]
        (let [o (pending! fixture)
              room (discourse/make-room {:id (keyword (str "scheduled-result-" (random-uuid)))
                                         :ctx ctx :store (room-store/make conn)})
              mark! model/mark-announced! failed? (atom false)
              request {:work.request/job (random-uuid)}]
          (try
            (with-redefs [model/mark-announced!
                          (fn [conn oid]
                            (if (compare-and-set! failed? false true)
                              (throw (ex-info "Process lost the system-DB acknowledgement" {}))
                              (mark! conn oid)))]
              (is (thrown? clojure.lang.ExceptionInfo (publish! conn row o request {:status :completed :finished-at #inst "2026-10-19T08:00:00Z"} room)))
              (is (= 1 (count (discourse/messages room))))
              (publish! conn row o request {:status :completed :finished-at #inst "2026-10-19T08:00:00Z"} room)
              (is (= 1 (count (discourse/messages room))))
              (is (= (:work.request/job request) (get-in (first (discourse/messages room)) [:metadata :run-id])))
              (is (true? (:schedule.occurrence/announced? (model/occurrence conn (:schedule.occurrence/id o))))))
            (finally (discourse/close-room! room))))))))

(deftest recovery-holds-missing-and-pointerless-admissions
  (doseq [has-request? [false true]]
    (with-fixture
      (fn [{:keys [conn ctx owner room-id row] :as fixture}]
        (let [o (pending! fixture)]
          (model/admit! conn (:schedule.occurrence/id o))
          (when has-request? (record-request! fixture o false))
          (schedules/recover! conn ctx)
          (is (= :interrupted (:schedule.occurrence/state (model/occurrence conn (:schedule.occurrence/id o)))))
          (is (false? (:schedule.activation/enabled? (model/activation conn (:schedule.activation/id row)))))
          (is (thrown? clojure.lang.ExceptionInfo
                       (schedules/set-enabled! conn ctx (principals/party-principal owner) room-id
                                               (:schedule.activation/id row) true))))))))

(deftest overlap-remains-until-job-and-resource-cleanup-both-finish
  (with-fixture
    (fn [{:keys [conn row] :as fixture}]
      (let [o (pending! fixture) request (record-request! fixture o true)
            status (atom {:status :completed :settled? false})]
        (model/admit! conn (:schedule.occurrence/id o))
        (model/finish-admission! conn (:schedule.occurrence/id o) :linked (:work.request/id request) nil)
        (with-redefs [jobs/status (fn [_] @status)
                      workflow-runs/submit! (fn [& _] (throw (AssertionError. "Overlapping admission")))]
          (tick! fixture 120000)
          (is (= :skipped (:schedule.occurrence/state (first (model/occurrences conn (:schedule.activation/id row))))))
          (reset! status {:status :completed :settled? true})
          (tick! fixture 180000)
          (is (= (:schedule.occurrence/id o)
                 (:schedule.activation/active-occurrence (model/activation conn (:schedule.activation/id row)))))
          (requests/mark-returned! conn (:work.request/id request))
          ;; No new due slot: just release the completed occurrence.
          (tick! fixture 180000)
          (is (nil? (:schedule.activation/active-occurrence (model/activation conn (:schedule.activation/id row))))))))))

(deftest revoked-authority-blocks-before-submission
  (with-fixture
    (fn [{:keys [conn row] :as fixture}]
      (with-redefs [work/authorize! (fn [& _] (throw (ex-info "Revoked" {:type ::revoked})))
                    workflow-runs/submit! (fn [& _] (throw (AssertionError. "Revoked schedule submitted")))]
        (tick! fixture 60000)
        (let [after (model/activation conn (:schedule.activation/id row))
              o (first (model/occurrences conn (:schedule.activation/id row)))]
          (is (false? (:schedule.activation/enabled? after)))
          (is (= ::revoked (:schedule.activation/reason after)))
          (is (= :blocked (:schedule.occurrence/state o)))
          (is (empty? (requests/requests conn))))))))

(deftest exhausted-shared-allowance-does-not-admit-or-mint
  (with-fixture
    (fn [{:keys [conn row] :as fixture}]
      (with-redefs [resource/balance (constantly {})
                    resource/mint! (fn [& _] (throw (AssertionError. "Schedule minted allowance")))
                    workflow-runs/submit! (fn [& _] (throw (AssertionError. "Unfunded schedule submitted")))]
        (tick! fixture 60000)
        (let [after (model/activation conn (:schedule.activation/id row))]
          (is (false? (:schedule.activation/enabled? after)))
          (is (= :is.simm.ops.workflow-schedules/budget-exhausted (:schedule.activation/reason after)))
          (is (nil? (:schedule.activation/active-occurrence after)))
          (is (empty? (requests/requests conn))))))))

(deftest retained-source-submission-survives-current-source-edits
  (with-fixture
    (fn [{:keys [conn owner row target] :as fixture}]
      (let [prepared-snapshots (atom [])]
        (with-redefs [workflows/snapshot (fn [& _] (throw (AssertionError. "Read mutable authoring source")))
                      workflow-runs/prepare (fn [snapshot input]
                                              (swap! prepared-snapshots conj snapshot)
                                              {:basis {:submission input}})
                      work/submit-prepared!
                      (fn [c _ _ room-id _ input prepared]
                        (let [{:keys [request]} (requests/claim! c {:party owner :room room-id
                                                                    :client-id (:request-id input) :kind :workflow
                                                                    :intent (:basis prepared)
                                                                    :limits (update (:limits input) :settlement keyword)})]
                          (requests/record-job! c (:work.request/id request) (:work.request/claim request) (random-uuid))))]
          (tick! fixture 60000)
          (is (= [(:snapshot target)] @prepared-snapshots))
          (let [o (first (model/occurrences conn (:schedule.activation/id row)))]
            (is (= :linked (:schedule.occurrence/state o)))
            (is (= 1 (count (requests/requests conn)))))
          (tick! fixture 60000)
          (is (= 1 (count @prepared-snapshots))))))))

(deftest ambiguous-post-submission-response-links-the-exact-committed-request
  (with-fixture
    (fn [{:keys [conn row] :as fixture}]
      (let [calls (atom 0)]
        (with-redefs [workflow-runs/submit!
                      (fn [_ _ _ _ _ input _]
                        (swap! calls inc)
                        (record-request! fixture {:schedule.occurrence/id (:request-id input)} true)
                        (throw (ex-info "Lost successful response" {})))]
          (tick! fixture 60000)
          (tick! fixture 60000)
          (let [o (first (model/occurrences conn (:schedule.activation/id row)))]
            (is (= :linked (:schedule.occurrence/state o)))
            (is (uuid? (:schedule.occurrence/request o)))
            (is (= 1 @calls))
            (is (= 1 (count (requests/requests conn))))))))))
