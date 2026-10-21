(ns is.simm.ops.owned-benchmark-admission-test
  "Layer A retains real canonical intent without a Job. Layer B uses the real
   kernel/experiment/checker with fixed transports and isolated fixture Rooms;
   it proves neither deployed provisioning nor an actual CLI process/login."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.agent.experiment :as experiment]
            [dvergr.agent.experiment.runner :as runner]
            [dvergr.catalog.workspace :as catalog-workspace]
            [dvergr.discourse :as discourse]
            [dvergr.jobs :as jobs]
            [dvergr.model.api.claude-code :as claude-code]
            [dvergr.model.api.codex-subscription :as codex]
            [dvergr.model.gateway :as gateway]
            [dvergr.model.providers :as providers]
            [dvergr.resource :as resource]
            [dvergr.room.store :as store]
            [dvergr.room.store.memory :as memory]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.content :as content]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [is.simm.model.system-db :as system-db]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.benchmarks-test :as benchmark-fixture]
            [is.simm.ops.cli-owner :as cli-owner]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.knowledge-inputs :as knowledge]
            [is.simm.ops.owned-benchmark-admission :as owned]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflow-admission-test :as joined]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflows :as workflows]
            [is.simm.runtimes.context :as context]
            [jsonista.core :as json]
            [org.replikativ.spindel.engine.core :as ec]))

(def ^:private files
  {"workflow.edn" "{:title \"Owned offline workflow\" :task \"Read /docs/input.md and write its text to /out/result.md\" :capture [\"/out\"]}"
   "checker.clj" "(ns checker) (defn check [{:keys [files gold]}] (let [ok (= (:expected gold) (get files \"/out/result.md\"))] {:checks {:owned-output? ok} :reward (if ok 1 0)}))"
   "gold.edn" "{:expected \"Owned document\" :private \"PRIVATE-CHECKER-GOLD\"}"
   "fixtures/docs/input.md" "Owned document"})
(def ^:private policy {:version 1 :mode :text :cli-version "fixture"})
(def ^:private cli-owner {:version 1 :unit "fixture.service" :boot "fixture" :invocation "fixture"})
(defn- principal [w] (ops/party-principal (:owner w)))
(defn- submit! [kind & args]
  (apply (case kind :benchmark owned/submit-native! :cli-benchmark owned/submit-cli!) args))
(defn- seed! [conn source]
  (doseq [[path text] source]
    (repo/write! conn (str "workflows/echo/" path) (.getBytes ^String text "UTF-8"))))
(defn- promote! [conn w]
  (d/transact conn [{:party/id (:owner w) :party/role :admin}]))
(defn- state [conn]
  (let [db @conn] {:commit (d/commit-id db) :config (:config db) :tree (query/worktree db)}))
(defn- failure [f] (try (f) nil (catch clojure.lang.ExceptionInfo error error)))
(defn- input [kind host w source]
  (let [base (assoc (benchmark-fixture/input source) :models ["codex-subscription-sol"])]
    (assoc (if (= kind :cli-benchmark)
             (assoc base :models ["claude-code-haiku"]
                    :limits {:max-cli-runs 1 :max-model-steps 3 :timeout-ms 30000 :settlement "discard"})
             (assoc-in base [:limits :timeout-ms] 30000))
           :expected-owned-source (parse-uuid (get-in (sources/details! host (principal w) (str (:room w)) {:name "echo"})
                                                      [:data :revision])))))
(defn- claim-only [seen]
  (fn [conn ctx p room-id room request prepared]
    (reset! seen {:prepared prepared :room room :input request})
    (let [{:keys [request]} (requests/claim! conn {:party (work/authorize! conn p room-id)
                                                   :room room-id :client-id (:request-id request)
                                                   :kind (:request-kind prepared) :intent (:basis prepared)
                                                   :limits (update (:limits request) :settlement keyword)})]
      (work/status conn ctx p room-id (:work.request/id request)))))
(defmacro ^:private with-policies [& body]
  `(with-redefs [cli-owner/ownership! (constantly cli-owner)
                 claude-code/execution-policy (constantly policy)
                 runner/runtime-identity (constantly {:libraries {}})]
     ~@body))

(deftest both-fixed-kinds-pin-one-owned-db-and-retain-original-intent-not-selected-source
  (doseq [kind [:benchmark :cli-benchmark] order [true false]]
    (@#'fixture/with-fixture order
                             (fn [{:keys [conn w host owned attached]}]
                               (promote! conn w)
                               (let [source (assoc files "cases/a/gold.edn" (get files "gold.edn"))]
                                 (seed! owned source)
                                 (seed! attached (assoc source "fixtures/docs/input.md" "OTHER document"))
                                 (let [request (assoc (input kind host w source) :cases ["a"])
                                       p (cond-> (principal w) (not order) (assoc :principal/mcp? true))
                                       resolve reads/resolve-host-read! snapshot workflows/snapshot-in
                                       captured (atom nil) seen (atom nil) calls (atom {:resolve 0 :snapshot 0})
                                       other (state attached) selected (workspace/selected-system-id)]
                                   (with-policies
                                     (with-redefs [reads/resolve-host-read! (fn [& args]
                                                                              (swap! calls update :resolve inc)
                                                                              (let [binding (apply resolve args)] (reset! captured binding) binding))
                                                   workflows/snapshot-in (fn [db name]
                                                                           (swap! calls update :snapshot inc)
                                                                           (let [result (snapshot db name)]
                                                                             (repo/write! owned "workflows/echo/fixtures/docs/input.md" (.getBytes "Later document" "UTF-8"))
                                                                             result))
                                                   workflows/snapshot (fn [& _] (throw (AssertionError. "No ambient source")))
                                                   work/submit-prepared! (claim-only seen)]
                                       (let [result (submit! kind host p (str (:room w)) request)
                                             row (requests/get-request conn (:id result))
                                             basis (get-in (requests/payload conn row) [:input])]
                                         (is (= {:resolve 1 :snapshot 1} @calls))
                                         (is (= kind (:work.request/kind row) (get-in @seen [:prepared :request-kind])))
                                         (is (nil? (:job result)) "Layer A starts no Job")
                                         (is (= request (:submission basis)))
                                         (is (= (if (:principal/mcp? p) :mcp :host) (:admission-channel basis)))
                                         (is (= (:expected-owned-source request) (get-in basis [:source-origin :revision])))
                                         (is (= (:target @captured) (get-in basis [:source-origin :target])))
                                         (is (= (:target-stamp @captured) (get-in basis [:source-origin :target-stamp])))
                                         (is (= (workflows/manifest-in (:db @captured) "echo") (get-in basis [:source-origin :manifest])))
                                         (is (= request (get-in @seen [:prepared :basis :submission])))
                                         (is (= "Owned document" (get-in @seen [:prepared :bundle :fixtures "/docs/input.md"])))
                                         (is (= (:limits request) (:limits basis)))
                                         (is (= ["a"] (mapv #(get-in % [:environment/metadata :case]) (get-in @seen [:prepared :plan :environments]))))
                                         (is (= 1 (get-in @seen [:prepared :cells])))
                                         (is (= (= kind :benchmark) (get-in @seen [:prepared :plan :resume?])))
                                         (when (= kind :cli-benchmark)
                                           (is (= {"cli-run-admission" 1M} (get-in @seen [:prepared :resources])))))))
                                   (is (= other (state attached)))
                                   (is (= selected (workspace/selected-system-id)))))))))

(deftest exact-retries-are-metadata-only-and-kinds-or-owned-expectations-cannot-collide
  (doseq [kind [:benchmark :cli-benchmark]]
    (@#'fixture/with-fixture true
                             (fn [{:keys [conn w host owned hydrations]}]
                               (promote! conn w) (seed! owned files)
                               (let [request (input kind host w files)
                                     result (with-policies (with-redefs [work/submit-prepared! (claim-only (atom nil))]
                                                             (submit! kind host (principal w) (str (:room w)) request)))
                                     row (requests/get-request conn (:id result))
                                     calls (atom 0) forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Known retry did fresh work")))]
                                 (d/transact conn [[:db/retract [:room/id (:room w)] :room/default-repository]])
                                 (repo/write! owned "workflows/echo/new.md" (.getBytes "Changed" "UTF-8"))
                                 (reset! hydrations [])
                                 (with-redefs [reads/resolve-host-read! forbidden workflows/manifest-in forbidden workflows/snapshot-in forbidden
                                               benchmarks/prepare forbidden benchmarks/prepare-cli forbidden
                                               cli-owner/ownership! forbidden claude-code/execution-policy forbidden
                                               benchmarks/adapter forbidden work/submit-prepared! forbidden]
                                   (is (= result (submit! kind host (principal w) (str (:room w)) request)))
                                   (is (= ::owned/conflict (:type (ex-data (failure #(submit! kind host (principal w) (str (:room w))
                                                                                              (assoc request :expected-owned-source (random-uuid))))))))
                                   (is (= ::owned/invalid (:type (ex-data (failure #(submit! kind host (principal w) (str (:room w))
                                                                                             (dissoc request :expected-owned-source)))))))
           ;; A normal other-kind row conflicts even when its payload is exact.
                                   (is (= :conflict (:status (benchmarks/retry-request (if (= kind :benchmark) :cli-benchmark :benchmark)
                                                                                       conn (:owner w) (:room w) request))))
                                   (let [other-kind (if (= kind :benchmark) :cli-benchmark :benchmark)
                                         other-input (if (= other-kind :cli-benchmark)
                                                       (assoc request :models ["claude-code-haiku"]
                                                              :limits {:max-cli-runs 1 :max-model-steps 3 :timeout-ms 30000 :settlement "discard"})
                                                       (assoc request :models ["codex-subscription-sol"]
                                                              :limits (:limits (benchmark-fixture/input files))))]
                                     (is (= ::owned/conflict
                                            (:type (ex-data (failure #(submit! other-kind host (principal w) (str (:room w)) other-input)))))))
                                   (is (zero? @calls)) (is (empty? @hydrations)))
                                 (is (= (:work.request/intent-id row) (:work.request/intent-id (requests/get-request conn (:id result)))))
                                 (is (= 1 (count (requests/requests conn)))))))))

(deftest local-dtos-and-admin-plus-room-authority-refuse-before-hydration
  (@#'fixture/with-fixture false
                           (fn [{:keys [conn w host owned hydrations]}]
                             (seed! owned files)
                             (let [request (input :benchmark host w files)]
                               (reset! hydrations [])
                               (doseq [p [(principal w) (ops/party-principal (:admin w)) (ops/party-principal (:member w))
                                          (assoc (principal w) :principal/kind :worker)]]
                                 (is (= ::owned/unavailable (:type (ex-data (failure #(owned/submit-native! host p (str (:room w)) request)))))))
                               (promote! conn w)
                               (doseq [bad [(dissoc request :expected-owned-source) (assoc request :name "echo\n")
                                            (assoc request :room (str (:room w))) (assoc request :expected-owned-source "not-a-uuid")]]
                                 (is (= ::owned/invalid (:type (ex-data (failure #(owned/submit-native! host (principal w) (str (:room w)) bad)))))))
                               (is (= ::owned/invalid (:type (ex-data (failure #(owned/submit-cli! host (principal w) (str (:room w)) request))))))
                               (is (empty? @hydrations))))))

(deftest same-bytes-default-replacement-conflicts-before-body-or-cli-policy-probes
  (doseq [kind [:benchmark :cli-benchmark]]
    (@#'fixture/with-fixture true
                             (fn [{:keys [conn w host owned attached attached-descriptor]}]
                               (promote! conn w) (seed! owned files) (seed! attached files)
                               (let [request (input kind host w files) calls (atom 0)
                                     forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Stale admission advanced")))]
                                 (d/transact conn [{:room/id (:room w) :room/default-repository [:system/id (:registry-system attached-descriptor)]}])
                                 (with-redefs [content/read-by-id forbidden workflows/snapshot-in forbidden
                                               benchmarks/prepare forbidden benchmarks/prepare-cli forbidden cli-owner/ownership! forbidden
                                               claude-code/execution-policy forbidden work/submit-prepared! forbidden]
                                   (is (= ::owned/conflict (:type (ex-data (failure #(submit! kind host (principal w) (str (:room w)) request))))))
                                   (is (zero? @calls))))))))

(deftest current-owner-context-admin-merge-and-default-gates-surround-preparation
  (doseq [[change at prepares] [[:control 2 0] [:context 3 1] [:admin 2 0] [:merge 3 1] [:default 2 0]]]
    (@#'fixture/with-fixture true
                             (fn [{:keys [conn w host owned attached-descriptor]}]
                               (promote! conn w) (seed! owned files)
                               (let [request (input :benchmark host w files) preflight reads/preflight-host-read!
                                     prepare benchmarks/prepare gates (atom 0) prepared (atom 0) submitted (atom 0)]
                                 (with-policies
                                   (with-redefs [reads/preflight-host-read!
                                                 (fn [& args]
                                                   (if (= at (swap! gates inc))
                                                     (case change
                                                       :control (with-redefs [system-db/get-conn (constantly (Object.))] (apply preflight args))
                                                       :context (with-redefs [context/server-context (Object.)] (apply preflight args))
                                                       :admin (do (d/transact conn [[:db/retract [:party/id (:owner w)] :party/role :admin]]) (apply preflight args))
                                                       :merge (do (d/transact conn [[:db/retract [:room/id (:room w)] :room/owner [:party/id (:owner w)]]])
                                                                  (apply preflight args))
                                                       :default (do (d/transact conn [{:room/id (:room w) :room/default-repository
                                                                                       [:system/id (:registry-system attached-descriptor)]}])
                                                                    (apply preflight args)))
                                                     (apply preflight args)))
                                                 benchmarks/prepare (fn [& args] (swap! prepared inc) (apply prepare args))
                                                 work/submit-prepared! (fn [& _] (swap! submitted inc) (throw (AssertionError. "Revoked claim")))]
                                     (is (= ::owned/unavailable (:type (ex-data (failure #(owned/submit-native! host (principal w) (str (:room w)) request))))))))
                                 (is (= prepares @prepared))
                                 (is (zero? @submitted)))))))

(deftest frozen-case-manifests-never-trigger-live-knowledge-or-publication
  (@#'fixture/with-fixture false
                           (fn [{:keys [conn w host owned]}]
                             (promote! conn w)
                             (let [policy-text "{:kind :knowledge :policy {:version 1 :trigger-roots [\"inbox\"] :context-roots [\"wiki\"] :exclude-prefixes []}}"
                                   base (assoc files "input-selection.edn" policy-text)]
                               (repo/write! owned "inbox/evidence.md" (.getBytes "Frozen evidence" "UTF-8"))
                               (let [captured (knowledge/capture @owned (knowledge/declaration base))
                                     source (into base (for [[path text] (:inputs captured)] [(str "fixtures" path) text]))]
                                 (seed! owned source)
                                 (let [request (input :benchmark host w source) seen (atom nil) calls (atom 0)
                                       forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Benchmark acquired live knowledge")))]
                                   (repo/write! owned "inbox/evidence.md" (.getBytes "Later evidence" "UTF-8"))
                                   (with-policies
                                     (with-redefs [knowledge/selection forbidden knowledge/capture forbidden knowledge/publication-basis forbidden
                                                   work/submit-prepared! (claim-only seen)]
                                       (is (uuid? (:id (owned/submit-native! host (principal w) (str (:room w)) request))))
                                       (is (zero? @calls))
                                       (is (= "Frozen evidence" (get-in @seen [:prepared :bundle :fixtures "/in/knowledge/sources/inbox/evidence.md"])))
                                       (is (nil? (get-in @seen [:prepared :basis :knowledge/publication-basis])))))))))))

(deftest malformed-retry-and-forged-helper-tags-are-unavailable-with-preserved-interruption
  (@#'fixture/with-fixture true
                           (fn [{:keys [conn w host owned]}]
                             (promote! conn w) (seed! owned files)
                             (let [request (input :benchmark host w files) calls (atom 0)
                                   forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Malformed retry advanced")))]
                               (doseq [result [nil :retry [] {:status :retry :id (random-uuid)} {:status :absent :id (random-uuid)}]]
                                 (with-redefs [benchmarks/retry-request (constantly result) reads/resolve-host-read! forbidden]
                                   (is (= ::owned/unavailable (:type (ex-data (failure #(owned/submit-native! host (principal w) (str (:room w)) request))))))))
                               (is (zero? @calls))
                               (doseq [helper [#'requests/request-id #'benchmarks/retry-request #'benchmarks/prepare]]
                                 (with-redefs-fn {helper (fn [& _] (throw (ex-info "SECRET" {:type ::owned/conflict :status 409})))}
                                   #(let [error (failure (fn [] (owned/submit-native! host (principal w) (str (:room w)) request)))]
                                      (is (= ::owned/unavailable (:type (ex-data error))))
                                      (is (not (re-find #"SECRET" (str error)))))))
                               (let [result (with-policies (with-redefs [work/submit-prepared! (claim-only (atom nil))]
                                                             (owned/submit-native! host (principal w) (str (:room w)) request)))]
                                 (try
                                   (with-redefs [benchmarks/retry-request (fn [& _] (throw (ex-info "wrapped" {} (InterruptedException.))))]
                                     (let [error (failure #(owned/submit-native! host (principal w) (str (:room w)) request))
                                           interrupted (.isInterrupted (Thread/currentThread))]
                                       (is (= ::owned/unavailable (:type (ex-data error))))
                                       (is interrupted)))
                                   (finally (Thread/interrupted)))
                                 (is (uuid? (:id result))))))))

(deftest malformed-normal-source-identities-are-not-trusted-revision-conflicts
  (@#'fixture/with-fixture true
                           (fn [{:keys [conn w host owned]}]
                             (promote! conn w) (seed! owned files)
                             (let [request (input :benchmark host w files) bodies (atom 0)
                                   forbidden (fn [& _] (swap! bodies inc) (throw (AssertionError. "Malformed identity decoded source")))]
                               (doseq [value [nil "not-a-UUID"]]
                                 (with-redefs [sources/source-id (constantly value) workflows/snapshot-in forbidden
                                               benchmarks/prepare forbidden work/submit-prepared! forbidden]
                                   (is (= ::owned/unavailable
                                          (:type (ex-data (failure #(owned/submit-native! host (principal w) (str (:room w)) request))))))))
                               (is (zero? @bodies))))))

(defn- model-answer [n]
  (if (= n 1)
    {:content "" :stop-reason :tool-use :usage {:input-tokens 10 :output-tokens 10}
     :tool-calls [{:id "owned-benchmark-write" :name "clojure_eval"
                   :input {:code "(do (babashka.fs/create-dirs \"/out\") (spit \"/out/result.md\" (slurp \"/docs/input.md\")))"}}]}
    {:content "Done" :stop-reason :end-turn :usage {:input-tokens 10 :output-tokens 10}}))

(defn- native-response [n]
  (let [events (concat
                (if (= n 1)
                  [{:type "response.output_item.done"
                    :item {:type "function_call" :call_id "owned-benchmark-write" :name "clojure_eval"
                           :arguments (json/write-value-as-string (get-in (model-answer n) [:tool-calls 0 :input]))}}]
                  [{:type "response.output_text.delta" :delta "Done"}])
                [{:type "response.completed"
                  :response {:id (str "owned-benchmark-fixture-" n) :model "gpt-5.6-sol"
                             :usage {:input_tokens 10 :output_tokens 10}}}])]
    {:status 200 :body (java.io.ByteArrayInputStream.
                        (.getBytes (apply str (map #(str "data: " (json/write-value-as-string %) "\n\n") events)) "UTF-8"))}))

(deftest real-native-and-cli-components-use-owned-offline-fixtures-and-retire-physical-capacity
  (doseq [kind [:benchmark :cli-benchmark] order [true false]]
    (@#'fixture/with-fixture order
                             (fn [{:keys [conn w host room owned attached]}]
                               (promote! conn w) (seed! owned files)
                               (seed! attached (assoc files "fixtures/docs/input.md" "OTHER document"))
                               (repo/write! owned "private-control.md" (.getBytes "PRIVATE-CONTROL-DATA" "UTF-8"))
                               (let [request (input kind host w files) p (cond-> (principal w) (not order) (assoc :principal/mcp? true))
                                     clean (discourse/make-room {:id (keyword (str "owned-benchmark-component-" (random-uuid))) :store (memory/make)})
                                     configs (atom []) clean-conn (atom nil) closed? (atom false) retired (atom 0) slot (java.util.concurrent.Semaphore. 1)
                                     job (atom nil) done (atom nil) start jobs/start! calls (atom [])
                                     parent-before (state owned) other-before (state attached) selected (workspace/selected-system-id)
                                     provider (case kind :benchmark (codex/create {:credentials (gateway/static-credentials :fixture {} #{"https://chatgpt.com"})})
                                                    :cli-benchmark (claude-code/create {}))
                                     retire! (fn []
                                               (locking closed?
                                                 (when-not @closed?
                                                   (discourse/close-room! clean)
                                                   (when-let [c @clean-conn] (d/release c))
                                                   (reset! closed? true)
                                                   (swap! retired inc))))]
                                 (binding [ec/*execution-context* (:ctx clean)]
                                   (catalog-workspace/ensure-workspace! clean)
                                   (reset! clean-conn (:conn (workspace/current-workspace)))
                                   (reset! configs [(:config @@clean-conn)]))
                                 (if (= kind :cli-benchmark)
                                   (do (resource/install-unit! room {:symbol "cli-run-admission" :name "CLI Run admissions" :precision 0})
                                       (resource/mint! room {:id (random-uuid) :resources {"cli-run-admission" 1M}}))
                                   (resource/mint! room {:id (random-uuid) :resources (work/resources (:limits request))}))
                                 (with-redefs-fn
                                   {#'benchmarks/checker-slot slot
                                    #'benchmarks/pending-retirements (atom {})
                                    #'benchmarks/fixture-room! (fn [& _] clean)
                                    #'benchmarks/retire-fixture! (fn [& _] (retire!))
                                    #'cli-owner/ownership! (constantly cli-owner)
                                    #'claude-code/execution-policy (constantly policy)
                                    #'runner/runtime-identity (constantly {:libraries {}})
                                    #'providers/providers (atom {(if (= kind :benchmark) :codex-subscription :claude-code) provider})
            ;; Both kinds retain real model-chat/provider/accounting paths.
            ;; Only HTTP or the CLI subprocess transport is replaced.
                                    #'gateway/*request-fn* (fn [http]
                                                             (when-not (= kind :benchmark)
                                                               (throw (AssertionError. "CLI fixture attempted HTTP")))
                                                             (swap! calls conj {:request (json/read-value (:body http) json/keyword-keys-object-mapper)})
                                                             (native-response (count @calls)))
                                    #'claude-code/run-claude-streaming
                                    (fn [messages opts]
                                      (when-not (= kind :cli-benchmark)
                                        (throw (AssertionError. "Native fixture attempted CLI")))
                                      ;; The real CLI parser returns nil usage when its result
                                      ;; omits usage. This fixture tests THAT unknown-usage
                                      ;; case, not a claim that every CLI result is unpriced.
                                      (swap! calls conj {:messages messages :opts opts :usage nil})
                                      (assoc (model-answer (count @calls)) :usage nil))
                                    #'jobs/start! (fn [actual-room options actual-work & callbacks]
                                                    (apply start actual-room options
                                                           (fn [id]
                                                             (when (and (identical? room actual-room) (compare-and-set! job nil id))
                                                               (reset! done (get-in @(var-get #'jobs/live) [id :done])))
                                                             (actual-work id)) callbacks))}
                                   (fn []
                                     (try
                                       (let [submission (submit! kind host p (str (:room w)) request)
                                             status (jobs/status @job 40000)
                                             attempts (store/-list-attempts (:store room) (store/conversation-id room) {})
                                             scorecards (store/-list-scorecards (:store room) (store/conversation-id room) {})
                                             row (requests/get-request conn (:id submission))
                                             basis (get-in (requests/payload conn row) [:input])]
                                         (is (uuid? @job)) (is (some? @done))
                                         (is (= @job (:job submission)))
                                         (is (= :completed (:status status)) (pr-str (select-keys status [:status :error])))
                                         (is (:settled? status))
                                         (is (false? (:return-pending? (work/status conn (:execution-ctx host) p (:room w) (:id submission)))))
                                         (is (= 2 (count @calls)))
                                         (is (= 1 (count attempts) (count scorecards)))
                                         (is (every? experiment/verdict? attempts))
                                         (is (= 1 (get-in (first scorecards) [:scorecard/summary 0 :passed-count])))
                                         (is (= {"/out/result.md" "Owned document"} (get-in (first attempts) [:attempt/evidence :files])))
                                         (is (= request (:submission basis)))
                                         (is (= (if (:principal/mcp? p) :mcp :host) (:admission-channel basis)))
                                         (is (= kind (:work.request/kind row)))
                                         (is (= 1 @retired)) (is @closed?)
                                         (is (= 1 (.availablePermits slot)))
                                         (is (not (re-find #"PRIVATE-CHECKER-GOLD|PRIVATE-CONTROL-DATA|OTHER document" (pr-str @calls))))
                                         (when (= kind :cli-benchmark)
                                           (is (every? #(= :text (get-in % [:opts :execution-mode])) @calls))
                                           (is (= [nil nil] (mapv :usage @calls)))
                                           (is (= :cli-run-admission (get-in (first scorecards) [:scorecard/experiment :experiment/metadata :simmis/execution :accounting])))
                                           (is (empty? (get-in (first attempts) [:attempt/receipt :attempt/metrics :spend :tokens])))
                                           (is (false? (get-in (first attempts) [:attempt/receipt :attempt/metrics :spend :priced?]))))
                                         (when (= kind :benchmark)
                                           ;; Existing subscription registry semantics: zero incremental
                                           ;; USD, but TWO actual governed HTTP dispatch admissions. This
                                           ;; is not evidence of a real external call/subscription payment.
                                           (is (= {resource/microdollars (bigdec (get-in request [:limits :budget-microdollars]))
                                                   resource/model-dispatches (bigdec (- (get-in request [:limits :max-dispatches]) 2))}
                                                  (resource/balance room)))
                                           (is (zero? (get-in (first attempts) [:attempt/receipt :attempt/metrics :spend :microdollars]))))
                                         (let [fresh (atom 0) forbidden (fn [& _] (swap! fresh inc) (throw (AssertionError. "Real benchmark retry reran")))]
                                           (with-redefs [reads/resolve-host-read! forbidden workflows/snapshot-in forbidden
                                                         benchmarks/prepare forbidden benchmarks/prepare-cli forbidden
                                                         cli-owner/ownership! forbidden claude-code/execution-policy forbidden]
                                             (is (= @job (:job (submit! kind host p (str (:room w)) request))))
                                             (is (zero? @fresh))))
                                         (is (= parent-before (state owned))) (is (= other-before (state attached)))
                                         (is (= selected (workspace/selected-system-id))))
                                       (finally
                 ;; Reuse the reviewed original Job + exact evaluation-gate
                 ;; join, INSIDE all transport/provider/capacity fixture roots.
                                         (try (when @job (@#'joined/join-fixture-job! room @job @done))
                                              (finally (retire!)))))))
         ;; No native work remains after the held-root finally returned.
                                 (doseq [cfg @configs]
                                   (assert (and (= :memory (get-in cfg [:store :backend])) (uuid? (get-in cfg [:store :id]))))
                                   (d/delete-database cfg)))))))
