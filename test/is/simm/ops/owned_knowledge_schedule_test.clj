(ns is.simm.ops.owned-knowledge-schedule-test
  "Incremental discovery and fixed canonical claims through owned dispatcher.
   Most cases isolate preparation/journaling seams. The echo drain exercises
   actual native Attempts and filing. No provider or live dispatcher starts."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.resource :as resource]
            [dvergr.catalog.room :as catalog]
            [dvergr.model.registry :as models]
            [dvergr.agent.roster :as roster]
            [dvergr.jobs :as jobs]
            [dvergr.model.api.codex-subscription :as codex]
            [dvergr.model.gateway :as gateway]
            [dvergr.model.providers :as providers]
            [dvergr.system.rooms :as native-rooms]
            [dvergr.runtime.preparation :as preparation]
            [geschichte.repo :as repo]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.knowledge-processing :as processing]
            [is.simm.model.system-db :as sdb]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.knowledge-schedule-batches :as scheduled]
            [is.simm.model.workflow-schedules :as model]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.dvergr :as principals]
            [is.simm.ops.knowledge-batch-proof :as proof]
            [is.simm.ops.knowledge-filing :as filing]
            [is.simm.ops.proposals :as proposals]
            [is.simm.ops.shipped-wiki-schedule-test :as scripted]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [is.simm.ops.knowledge-batch-proof-test :as prep-fixture]
            [is.simm.ops.owned-knowledge-batches-test :as fixture]
            [is.simm.ops.owned-workflow-admission :as manual]
            [is.simm.ops.owned-knowledge-discovery :as discovery]
            [is.simm.ops.workflow-schedules :as schedules]
            [is.simm.ops.work :as work]
            [is.simm.ops.knowledge-batch-inputs :as batch-inputs]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.workflows :as workflows]
            [is.simm.starter.wiki-maintenance :as starter]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- with-schedule
  ([f] (with-schedule nil f))
  ([source f]
  (@#'fixture/with-fixture
   (fn [{:keys [conn w host room owned batch-input] :as data}]
     (when source
       ;; Fixture provisioning only: replace the entire synthetic subtree so
       ;; the deployment receives exactly the packaged source, including policy.
       (doseq [path (keys (:files (workflows/snapshot-in @owned "wiki")))]
         (repo/remove! owned (str "workflows/wiki/" path)))
       (doseq [[path text] source]
         (repo/write! owned (str "workflows/wiki/" path) (.getBytes ^String text "UTF-8")))
       (repo/stage-all! owned)
       (repo/commit! owned {:message "Packaged starter schedule fixture"}))
     (d/transact conn (concat requests/schema dispositions/schema processing/schema))
     (d/transact conn [{:party/id (:owner w) :party/role :admin}])
     (let [principal (principals/party-principal (:owner w)) handle (str (:room w))
           source-binding (when source (reads/resolve-host-read! host principal handle))
           batch-input (if source
                         {:expected-bundle (:id (catalog/bundle "wiki" source))
                          :expected-source (sources/source-id source-binding "wiki" (workflows/manifest-in (:db source-binding) "wiki"))
                          :epoch (:epoch (batch-inputs/declaration source))}
                         batch-input)
           execution {:name "wiki" :expected-bundle (:expected-bundle batch-input)
                      :expected-owned-source (:expected-source batch-input)
                      :model "codex-subscription-sol" :limits (assoc prep-fixture/limits :settlement "discard")}
           create! #(schedules/create-owned! host principal handle
                                             {:request-id (random-uuid) :kind "workflow"
                                              :cadence {:interval-ms 60000 :tz "UTC"} :execution execution})
           row (create!)]
       (binding [ec/*execution-context* (:ctx room)]
         (resource/mint! room {:id (random-uuid) :resources (work/resources prep-fixture/limits)}))
       (f (assoc data :row row :due (:schedule.activation/next-fire row) :create! create!
                      :principal principal :handle handle :execution execution)))))))

(defn- occurrence [conn row]
  (let [a (model/activation conn (:schedule.activation/id row))]
    (model/occurrence conn (:schedule.activation/last-occurrence a))))

(defn- journal-only [conn ctx principal room-id room input prepared]
  ;; Actual fixed claim composition, followed by synthetic Job metadata. This
  ;; isolates dispatcher ownership; native Job handoff has separate real tests.
  (let [result (@#'work/claim-batch-prepared! conn ctx principal room-id room input
                (assoc prepared :knowledge/schedule-binding
                       (proof/schedule-input! conn (work/authorize! conn principal room-id) room-id (:basis prepared))))
        row (:request result) id (:work.request/id row)]
    (when (:claimed? result)
      (requests/record-job! conn id (:work.request/claim row) (random-uuid))
      (requests/mark-returned! conn id))
    {:id id}))

(defn- preparing [calls]
  (fn [input]
    (swap! calls inc)
    (@#'prep-fixture/model-free-preparation input)))

(deftest dispatcher-retains-a-bounded-prefix-with-typed-binding-and-no-legacy-cursor
  (with-schedule
    (fn [{:keys [conn room host row due]}]
      (let [calls (atom 0)]
        (with-redefs [work/prepare (preparing calls) work/submit-prepared! journal-only]
          (schedules/tick-owned! host due))
        (let [o (occurrence conn row) request (requests/get-request conn (:schedule.occurrence/request o))
              p (proof/request! conn request) a (model/activation conn (:schedule.activation/id row))]
          (is (= :linked (:schedule.occurrence/state o)))
          (is (= 1 @calls))
          (is (= 1 (count (get-in p [:selection :items]))))
          (is (= 1 (get-in p [:selection :remaining])))
          (is (= (:schedule.occurrence/knowledge-batch o) (get-in p [:origin :batch])))
          (is (= (:schedule.activation/id row) (get-in p [:schedule :value :activation])))
          (is (not-any? #(contains? o %) [:schedule.occurrence/input-id :schedule.occurrence/input-ref
                                         :schedule.occurrence/input-revision]))
          (is (nil? (:schedule.activation/last-processed-input a)))
          (is (true? (:schedule.activation/enabled? a)))
          (is (empty? (get @(:meta room) preparation/leases-key))))))))

(deftest manual-and-scheduled-invocations-share-one-active-stream
  (doseq [manual-first? [true false]]
    (with-schedule
      (fn [{:keys [conn host row due execution principal handle]}]
        (let [calls (atom 0) input (assoc execution :request-id (random-uuid))]
          (with-redefs [work/prepare (preparing calls) work/submit-prepared! journal-only]
            (when manual-first? (manual/submit! host principal handle input))
            (schedules/tick-owned! host due)
            (when-not manual-first?
              (is (= :held (:status (manual/submit! host principal handle input))))))
          (is (= 1 @calls))
          (is (= 1 (count (requests/requests conn))))
          (is (= 1 (count (d/datoms @conn :aevt :knowledge.stream/id))))
          (let [o (occurrence conn row) a (model/activation conn (:schedule.activation/id row))]
            (is (= (if manual-first? :skipped :linked) (:schedule.occurrence/state o)))
            (when manual-first?
              (is (= :stream-busy (:schedule.occurrence/reason o)))
              (is (nil? (:schedule.activation/active-occurrence a))))
            (is (true? (:schedule.activation/enabled? a)))
            (is (nil? (:schedule.activation/last-processed-input a)))))))))

(deftest two-activations-do-not-create-two-backlogs-or-competing-requests
  (with-schedule
    (fn [{:keys [conn host row due create!]}]
      (let [other (create!) now (max-key #(.getTime ^java.util.Date %) due (:schedule.activation/next-fire other))
            calls (atom 0)]
        (with-redefs [work/prepare (preparing calls) work/submit-prepared! journal-only]
          (schedules/tick-owned! host now))
        (let [os (map #(occurrence conn %) [row other])]
          (is (= #{:linked :skipped} (set (map :schedule.occurrence/state os))))
          (is (= [:stream-busy] (keep :schedule.occurrence/reason os)))
          (is (every? :schedule.activation/enabled? (map #(model/activation conn (:schedule.activation/id %)) [row other]))))
        (is (= 1 @calls))
        (is (= 1 (count (requests/requests conn))))))))

(deftest changed-named-source-is-held-before-discovery-or-provider-preparation
  (with-schedule
    (fn [{:keys [conn host row due owned]}]
      (repo/write! owned "workflows/wiki/README.md" (.getBytes "Changed deployment" "UTF-8"))
      (with-redefs [work/prepare (fn [& _] (throw (AssertionError. "Changed source may not prepare provider")))]
        (schedules/tick-owned! host due))
      (is (empty? (requests/requests conn)))
      (is (empty? (d/datoms @conn :aevt :knowledge.batch/id)))
      (is (false? (:schedule.activation/enabled? (model/activation conn (:schedule.activation/id row))))))))

(deftest lost-binding-ack-remains-held-without-a-replacement-admission
  (with-schedule
    (fn [{:keys [conn host row due]}]
      (let [retain! scheduled/retain! calls (atom 0)]
        (with-redefs [scheduled/retain! (fn [& args] (apply retain! args) (throw (ex-info "Lost retention ACK" {})))
                      work/prepare (preparing calls)]
          (schedules/tick-owned! host due))
        (let [o (occurrence conn row)]
          (is (= :interrupted (:schedule.occurrence/state o)))
          (is (uuid? (:schedule.occurrence/batch-ref o)))
          (is (some? (scheduled/retained conn o))))
        (with-redefs [discovery/prepare-batch! (fn [& _] (throw (AssertionError. "Uncertain binding may not rediscover")))]
          (schedules/recover-owned! host)
          (schedules/tick-owned! host due))
        (is (zero? @calls))
        (is (empty? (requests/requests conn)))))))


(deftest real-native-echo-drains-two-bounded-batches-then-skips-empty-without-a-legacy-cursor
  (with-schedule
    (fn [{:keys [conn host room row due]}]
      (let [team (roster/make-agent (roster/make-roster {:id :scheduled-batch/team})
                                    {:id :candidate :program {:kind :echo}})
            calls (atom 0)
            later (fn [n] (java.util.Date. (+ (.getTime ^java.util.Date due) (* n 60000))))]
        ;; Source provisioning is the common disposable registry/runtime fixture.
        ;; Execution, native Attempts, filing, processing, and resource return are
        ;; real; the echo program performs no inference or external effects.
        (with-redefs [work/prepare
                      (fn [input]
                        (swap! calls inc)
                        (is (empty? (get @(:meta room) preparation/leases-key)))
                        (-> (@#'prep-fixture/model-free-preparation input)
                            (assoc :team team :agent :candidate)
                            (assoc-in [:basis :candidate] (roster/agent team :candidate))))
                      sdb/get-conn (constantly conn)]
          (doseq [n [0 1]]
            (schedules/tick-owned! host (later n))
            (let [o (occurrence conn row) request (requests/get-request conn (:schedule.occurrence/request o))
                  status (jobs/status (:work.request/job request) 25000)
                  batch (:schedule.occurrence/knowledge-batch o)]
              (is (= :linked (:schedule.occurrence/state o)))
              (is (= :completed (:status status)))
              (is (:settled? status))
              (schedules/tick-owned! host (later n))
              (let [after (model/activation conn (:schedule.activation/id row))
                    filed (requests/get-request conn (:work.request/id request))]
                (is (nil? (:schedule.activation/active-occurrence after)))
                (is (true? (:schedule.activation/enabled? after)))
                (is (nil? (:schedule.activation/last-processed-input after)))
                (is (= :no-op (:kind (dispositions/disposition filed))))
                (is (= :closed (:knowledge.batch/state (batches/batch @conn batch))))
                (let [summary (first (schedules/list-owned! host
                                      (principals/party-principal (:schedule.activation/by row))
                                      (str (:schedule.activation/room row))))
                      observed (first (filter #(= (str (:work.request/id request)) (:request %)) (:occurrences summary)))]
                  (is (= {:status :observed :id (str batch) :phase :closed} (:batch observed))))
                (is (= 1 (count (get-in (proof/request! conn filed) [:selection :items])))))))
          (schedules/tick-owned! host (later 2))
          (let [o (occurrence conn row) a (model/activation conn (:schedule.activation/id row))
                stream-id (:v (first (d/datoms @conn :aevt :knowledge.stream/id)))]
            (is (= :skipped (:schedule.occurrence/state o)))
            (is (= :no-inputs (:schedule.occurrence/reason o)))
            (is (nil? (:schedule.activation/active-occurrence a)))
            (is (true? (:schedule.activation/enabled? a)))
            (is (nil? (:schedule.activation/last-processed-input a)))
            (is (empty? (streams/pending-items @conn stream-id))))
          (is (= 2 @calls))
          (is (= 2 (count (requests/requests conn)))))))))

(deftest source-changed-during-provider-preparation-cannot-reach-native-claim
  (with-schedule
    (fn [{:keys [conn host row due owned]}]
      (let [calls (atom 0)]
        (with-redefs [work/prepare (fn [input]
                                    (swap! calls inc)
                                    (repo/write! owned "workflows/wiki/README.md" (.getBytes "Changed during prepare" "UTF-8"))
                                    (@#'prep-fixture/model-free-preparation input))
                      work/submit-prepared! (fn [& _] (throw (AssertionError. "Changed deployment must not submit")))]
          (schedules/tick-owned! host due))
        (is (= 1 @calls))
        (is (empty? (requests/requests conn)))
        (let [o (occurrence conn row)]
          (is (= :interrupted (:schedule.occurrence/state o)))
          (is (some? (scheduled/retained conn o)))
          (is (= :ready (:knowledge.batch/state (batches/batch @conn (:schedule.occurrence/knowledge-batch o))))))))))

(deftest lost-busy-skip-ack-cannot-rewrite-the-terminal-skip-or-disable-the-schedule
  (with-schedule
    (fn [{:keys [conn host row due principal handle execution]}]
      (let [skip! model/skip-batch! calls (atom 0)]
        (with-redefs [work/prepare (preparing calls) work/submit-prepared! journal-only]
          (manual/submit! host principal handle (assoc execution :request-id (random-uuid)))
          (with-redefs [model/skip-batch! (fn [& args] (apply skip! args) (throw (ex-info "Lost skip ACK" {})))]
            (schedules/tick-owned! host due)))
        (let [o (occurrence conn row) a (model/activation conn (:schedule.activation/id row))]
          (is (= :skipped (:schedule.occurrence/state o)))
          (is (= :stream-busy (:schedule.occurrence/reason o)))
          (is (true? (:schedule.activation/enabled? a)))
          (is (nil? (:schedule.activation/active-occurrence a))))
        (is (= 1 @calls))
        (is (= 1 (count (requests/requests conn))))))))

(deftest missing-batch-binding-cannot-fall-through-to-ordinary-completion
  (doseq [damage [:one :all :request-marker]]
    (with-schedule
      (fn [{:keys [conn host row due]}]
        (with-redefs [work/prepare (preparing (atom 0)) work/submit-prepared! journal-only]
          (schedules/tick-owned! host due))
        (let [o (occurrence conn row) oid (:schedule.occurrence/id o) rid (:schedule.occurrence/request o)
              attrs (case damage :one [:schedule.occurrence/batch-ref]
                           :all [:schedule.occurrence/knowledge-batch :schedule.occurrence/batch-ref :schedule.occurrence/batch-revision]
                           :request-marker [])]
          (d/transact conn
            (if (= :request-marker damage)
              [[:db/retract [:work.request/id rid] :work.request/knowledge-batch (:schedule.occurrence/knowledge-batch o)]]
              (mapv (fn [attr] [:db/retract [:schedule.occurrence/id oid] attr (get o attr)]) attrs)))
          (with-redefs [work/prepare (fn [& _] (throw (AssertionError. "Completion may not prepare provider")))
                        filing/candidate-outcome (fn [& _] (throw (AssertionError. "Missing binding may not read Attempt")))]
            (schedules/tick-owned! host due))
          (is (= oid (:schedule.activation/active-occurrence (model/activation conn (:schedule.activation/id row)))))
          (is (nil? (:schedule.activation/last-processed-input (model/activation conn (:schedule.activation/id row)))))
          (is (nil? (dispositions/disposition (requests/get-request conn rid)))))))))


(deftest known-stale-policy-or-insufficient-allowance-does-not-reserve-the-shared-backlog
  (doseq [damage [:model-policy :allowance]]
    (with-schedule
      (fn [{:keys [conn host row due]}]
        (let [get-model models/get-model balance resource/balance discovery-calls (atom 0) prep-calls (atom 0)]
          (with-redefs [models/get-model (fn [id] (cond-> (get-model id) (= :model-policy damage) (assoc :model "changed-after-deployment")))
                        resource/balance (if (= :allowance damage) (constantly {}) balance)
                        discovery/prepare-batch! (fn [& _] (swap! discovery-calls inc) (throw (AssertionError. "Unadmitted schedule may not discover")))
                        work/prepare (fn [& _] (swap! prep-calls inc) (throw (AssertionError. "Unadmitted schedule may not prepare")))]
            (schedules/tick-owned! host due))
          (is (zero? @discovery-calls))
          (is (zero? @prep-calls))
          (is (empty? (d/datoms @conn :aevt :knowledge.batch/id)))
          (is (empty? (d/datoms @conn :aevt :knowledge.stream/id)))
          (is (empty? (requests/requests conn)))
          (let [o (occurrence conn row)]
            (is (nil? (:schedule.occurrence/knowledge-batch o)))
            (is (nil? (:schedule.occurrence/batch-ref o)))))))))


(deftest actual-proposal-releases-the-occurrence-but-review-holds-the-stream-until-dismissal
  (let [native-prepare work/prepare]
    (with-schedule
      (fn [{:keys [conn host room w app-owner row due execution]}]
        (let [calls (atom 0)
              provider (codex/create {:credentials (gateway/static-credentials :fixture {} #{"https://chatgpt.com"})})
              later (fn [n] (java.util.Date. (+ (.getTime ^java.util.Date due) (* n 60000))))]
          ;; The real provider adapter, SCI tool, native evaluator/Attempt and
          ;; retained Proposal run against a completely scripted HTTP transport.
          (with-redefs [work/prepare native-prepare
                        sdb/get-conn (constantly conn)
                        gateway/*request-fn*
                        (fn [& _]
                          (if (= 1 (swap! calls inc))
                            (@#'scripted/write-response "A proposed wiki update from the selected evidence.")
                            (@#'scripted/final-response "Prepared the supported work.")))
                        providers/providers (atom {:codex-subscription provider})
                        native-rooms/room-ctx-for #(when (= (:room w) %) (:ctx room))]
            (schedules/tick-owned! host due)
            (let [o (occurrence conn row) request (requests/get-request conn (:schedule.occurrence/request o))
                  status (jobs/status (:work.request/job request) 40000)]
              (is (= :completed (:status status)))
              (is (:settled? status))
              (schedules/tick-owned! host due)
              (let [a (model/activation conn (:schedule.activation/id row))
                    filed (dispositions/disposition (requests/get-request conn (:work.request/id request)))
                    batch-id (:schedule.occurrence/knowledge-batch o)
                    batch (batches/batch @conn batch-id)]
                (is (= :proposed (:kind filed)))
                (is (= :review (:knowledge.batch/state batch)))
                (is (nil? (:schedule.activation/active-occurrence a)))
                (is (nil? (:schedule.activation/last-processed-input a)))
                (is (= 2 @calls))
                ;; Separate finite fixture allowance so the busy check tests a
                ;; review hold, rather than known insufficient dispatch funds.
                (binding [ec/*execution-context* (:ctx room)]
                  (resource/mint! room {:id (random-uuid) :resources (work/resources (:limits execution))}))
                (schedules/tick-owned! host (later 1))
                (is (= :stream-busy (:schedule.occurrence/reason (occurrence conn row))))
                (is (true? (:schedule.activation/enabled? (model/activation conn (:schedule.activation/id row)))))
                (is (= 2 @calls))
                (is (= {:status :dismissed}
                       (lifetime/with-operation! app-owner
                         #(proposals/decide-in! conn (:proposal filed) (:owner w) {:verb :dismiss-proposal}
                                                {:note "Skip this synthetic proposal"}))))
                (is (= :closed (:knowledge.batch/state (batches/batch @conn batch-id))))
                (is (= :dismissed (:knowledge.batch/outcome (batches/batch @conn batch-id))))
                (schedules/tick-owned! host (later 2))
                (let [next-o (occurrence conn row)
                      next-request (requests/get-request conn (:schedule.occurrence/request next-o))
                      next-status (jobs/status (:work.request/job next-request) 40000)]
                  (is (= :linked (:schedule.occurrence/state next-o)))
                  (is (not= batch-id (:schedule.occurrence/knowledge-batch next-o)))
                  (is (= :completed (:status next-status)))
                  (is (:settled? next-status))
                  (schedules/tick-owned! host (later 2))
                  (is (= :no-op (:kind (dispositions/disposition (requests/get-request conn (:work.request/id next-request))))))
                  (is (nil? (:schedule.activation/last-processed-input (model/activation conn (:schedule.activation/id row)))))
                  (is (= 3 @calls)))))))))))


(deftest concurrent-manual-and-scheduled-discovery-share-the-inflight-batch
  (doseq [manual-first? [true false]]
    (with-schedule
      (fn [{:keys [conn host row due execution principal handle]}]
        (let [entered (promise) release (CountDownLatch. 1) worker (atom nil) calls (atom 0)
              input (assoc execution :request-id (random-uuid))]
          (try
            (with-redefs [work/prepare
                          (fn [input]
                            (swap! calls inc)
                            (deliver entered true)
                            (when-not (.await release 15 TimeUnit/SECONDS)
                              (throw (ex-info "Fixture release timeout" {})))
                            (@#'prep-fixture/model-free-preparation input))
                          work/submit-prepared! journal-only]
              (reset! worker
                      (future
                        (if manual-first?
                          (manual/submit! host principal handle input)
                          (schedules/tick-owned! host due))
                        :completed))
              (is (= true (deref entered 15000 :timeout)))
              ;; The first caller holds its App operation during provider
              ;; preparation, with no source lease. The other can inspect the
              ;; same ready hold, without a second preparation or batch claim.
              (if manual-first?
                (schedules/tick-owned! host due)
                (is (= :held (:status (manual/submit! host principal handle input)))))
              (.countDown release)
              (is (= :completed (deref @worker 15000 :timeout))))
            (is (= 1 @calls))
            (is (= 1 (count (requests/requests conn))))
            (is (= 1 (count (d/datoms @conn :aevt :knowledge.stream/id))))
            (is (= (if manual-first? :skipped :linked) (:schedule.occurrence/state (occurrence conn row))))
            (finally
              (.countDown release)
              (when @worker (deref @worker 15000 nil)))))))))


(deftest packaged-v4-and-v5-sources-drain-through-native-batch-scheduling
  (doseq [version [4 5]]
   (let [source (starter/source version)]
    (with-schedule source
      (fn [{:keys [conn host room owned row due]}]
        (let [team (roster/make-agent (roster/make-roster {:id :packaged-starter/team})
                                      {:id :candidate :tools #{} :sandbox #{}
                                       :program (if (= 5 version)
                                                  {:kind :sci :source "(ns scheduled.wiki) (defn main [_] (assert (= \"C1 scheduled wiki\" (slurp \"/wiki/checkpoint.md\"))) {:observed :c1})"
                                                   :entrypoint 'scheduled.wiki/main :args [] :timeout-ms 10000}
                                                  {:kind :echo})})
              calls (atom 0)]
          (is (= source (get-in (model/managed-target conn row) [:snapshot :files])))
          (when (= 5 version)
            (repo/write! owned "wiki/checkpoint.md" (.getBytes "C1 scheduled wiki" "UTF-8")))
          (with-redefs [work/prepare
                        (fn [input]
                          (swap! calls inc)
                          (when (= 5 version)
                            (repo/write! owned "wiki/checkpoint.md" (.getBytes "C2 current wiki" "UTF-8")))
                          (-> (@#'prep-fixture/model-free-preparation input)
                              (assoc :team team :agent :candidate)
                              (assoc-in [:basis :candidate] (roster/agent team :candidate))))
                        sdb/get-conn (constantly conn)]
            (schedules/tick-owned! host due)
            (let [o (occurrence conn row)
                  request (requests/get-request conn (:schedule.occurrence/request o))
                  status (jobs/status (:work.request/job request) 25000)
                  proof (proof/request! conn request)]
              (is (= :linked (:schedule.occurrence/state o)))
              (is (= :completed (:status status)))
              (is (:settled? status))
              (is (= 2 (count (get-in proof [:selection :items]))))
              (is (= (:policy (batch-inputs/declaration source)) (get-in proof [:selection :batch-policy])))
              (schedules/tick-owned! host due)
              (is (= :closed (:knowledge.batch/state (batches/batch @conn (:schedule.occurrence/knowledge-batch o)))))
              (is (= :no-op (:kind (dispositions/disposition (requests/get-request conn (:work.request/id request)))))))
            (schedules/tick-owned! host (java.util.Date. (+ (.getTime ^java.util.Date due) 60000)))
            (let [o (occurrence conn row)]
              (is (= :skipped (:schedule.occurrence/state o)))
              (is (= :no-inputs (:schedule.occurrence/reason o))))
            (is (= 1 @calls))
            (is (= 1 (count (requests/requests conn)))))))))))
