(ns is.simm.ops.knowledge-filing-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.blob :as blob]
            [dvergr.agent.attempt :as attempt]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.episode :as episode]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.roster :as roster]
            [dvergr.agent.run :as run]
            [dvergr.catalog.room :as catalog]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.jobs :as jobs]
            [dvergr.model.registry :as models]
            [dvergr.resource :as resource]
            [dvergr.room.store :as store]
            [dvergr.room.registry :as registry]
            [dvergr.room.store.datahike :as room-store]
            [dvergr.system.db :as system-db]
            [dvergr.substrate.geschichte :as geschichte]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.documents.markdown :as markdown]
            [is.simm.authority.world :as authority-world]
            [is.simm.model.system-db :as sdb]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.workflow-schedules :as schedule-model]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.knowledge-admission :as admission]
            [is.simm.ops.knowledge-filing :as filing]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.knowledge-outputs :as outputs]
            [is.simm.ops.knowledge-workspace :as knowledge-workspace]
            [is.simm.ops.knowledge-publish :as publish]
            [is.simm.ops.file-output-storage :as output-storage]
            [is.simm.ops.proposals :as proposals]
            [is.simm.ops.workflow-schedules :as scheduler]
            [is.simm.ops.work :as work]
            [is.simm.ops.work-test]
            [is.simm.ops.workflow-runs :as workflows]
            [kontor.governance :as governance]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg]
            [yggdrasil.protocols :as protocols]))

(def source
  {"workflow.edn" "{:title \"Maintain knowledge\" :task \"Update the wiki from the frozen evidence\" :capture [\"/out\"]}"
   "checker.clj" "(throw (ex-info \"Authored checker must not execute\" {}))"
   "fixtures/seed.txt" "Pinned fixture"
   "input-selection.edn" "{:kind :knowledge :policy {:version 1 :trigger-roots [\"inbox\" \"transcripts\"] :context-roots [\"wiki\"] :exclude-prefixes []}}"})

(defn- memory-conn []
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
             :keep-history? false :max-string-length 0 :max-bytes-length 0
             :max-tuple-string-length 0}]
    (d/create-database cfg)
    (d/connect cfg)))

(defn- terminal-run [id room actor kind]
  {:run/id id :run/room room :run/actor actor :run/kind kind
   :run/trigger (random-uuid) :run/status :completed
   :run/created-at (java.util.Date. 1000) :run/started-at (java.util.Date. 1000)
   :run/updated-at (java.util.Date. 1025) :run/ended-at (java.util.Date. 1025)})

(defn- with-fixture
  ([f] (with-fixture {} f))
  ([options f]
   (let [conn (memory-conn) workspace (memory-conn)
         fixture-configs (mapv #(-> % deref :config) [workspace conn])]
     (try
       (d/transact conn system-db/schema)
       (d/transact conn sdb/schema)
       (repo/init! workspace {:name "Canonical knowledge source"})
       (repo/write! workspace "inbox/evidence.md" (markdown/source-bytes (get options :evidence-text "Source evidence")))
       (repo/write! workspace "wiki/existing.md" (markdown/source-bytes "Historical before\r\n"))
       (let [party (random-uuid) logical-room (random-uuid)
             principal {:principal/id (str "party/" party) :principal/actor (keyword "party" (str party))}
             runtime-id (keyword (str "knowledge-filing-" (random-uuid)))
             identity {:registry-system (random-uuid) :runtime-system (str "repo-" (random-uuid))}
             captured (inputs/capture @workspace inputs/default-policy)
             basis (inputs/publication-basis @workspace identity)
             bundle (:id (catalog/bundle "memory" source))
             submission {:request-id (random-uuid) :name "memory" :expected-bundle bundle
                         :model "fixture-no-provider"
                         :limits {:budget-microdollars 10000 :max-dispatches 4 :max-model-steps 4
                                  :timeout-ms 30000 :settlement "discard"}}
             prepared (workflows/prepare-source {:files source}
                                                (select-keys submission workflows/source-fields) captured)
             evaluator (cond-> (outputs/with-capture work/completion)
                         (not (:unbound? options)) (outputs/with-publication-target (:target basis)))
             setup (cond-> (:world-setup prepared)
                     (not (:unbound? options)) (knowledge-workspace/with-publication-target (:target basis)))
             definition (environment/make-environment
                         {:id (get options :environment-id :simmis/saved-workflow) :task (:task prepared)
                          :verifier (let [ref (evaluation/evaluator-ref evaluator)]
                                      {:id (:verifier/id ref) :version (:verifier/version ref)
                                       :basis (:verifier/basis ref)})
                          :world {:isolation :ctx :settlement :discard
                                  :setup (evaluation/world-setup-ref setup)}
                          :metadata {:workflow (:workflow prepared)}})
             agent (-> (roster/make-roster)
                       (roster/make-agent {:id :candidate :program {:kind :echo}})
                       (roster/agent :candidate))
             input ((get options :input-transform clojure.core/identity)
                    {:candidate agent :environment definition :source source
                     :inputs (:inputs prepared) :workflow (:workflow prepared) :submission submission
                     :knowledge/publication-basis basis})
             input (if (:retain-source-origin? options)
                     (let [stamp (random-uuid)
                           manifest (mapv (fn [[path text]]
                                            (let [bytes (markdown/source-bytes text)]
                                              {:path path :content (blob/blob-id bytes)
                                               :size (alength bytes) :mode repo/default-mode}))
                                          (sort-by key source))
                           revision (hasch/uuid [:simmis/owned-workflow-source-v1 logical-room stamp "memory" manifest])]
                       (-> input
                           (assoc-in [:submission :expected-owned-source] revision)
                           (assoc :source-origin {:revision revision :target-stamp stamp
                                                  :target (:target basis) :manifest manifest})))
                     input)
             _ (d/transact conn [{:party/id party}
                                 {:room/id logical-room :room/owner [:party/id party]
                                  :room/parties [[:party/id party]]}])
             {:keys [request]} (requests/claim!
                                conn {:party party :room logical-room :client-id (:request-id submission)
                                      :kind :workflow :intent input
                                      :limits (assoc (:limits submission) :settlement :discard)})
             id (:work.request/id request) job (random-uuid) candidate (random-uuid)
             _ (requests/record-job! conn id (:work.request/claim request) job)
             _ (requests/mark-returned! conn id)
             st (room-store/make conn)
             room {:id runtime-id :store st :ctx ec/*execution-context*}
             _ (store/-store-room! st runtime-id {:slug (name runtime-id)})
             _ (store/-store-run! st runtime-id (terminal-run job runtime-id (:principal/actor principal) :workflow))
             status (get options :candidate-status :completed)
             candidate-run (merge (terminal-run candidate runtime-id (:agent/id agent) :agent-task)
                                  {:run/parent job :run/agent-version (:agent/version agent)
                                   :run/status status
                                   :run/program-kind :echo :run/interpreter-version 5
                                   :run/agent-def-hash (hasch/uuid agent)})
             _ (store/-store-run! st runtime-id candidate-run)
             descendant (when (:extra-descendant? options)
                          (assoc (terminal-run (random-uuid) runtime-id :hired-child :agent-task)
                                 :run/parent candidate))
             _ (when descendant (store/-store-run! st runtime-id descendant))
             output-files (get options :output-files {"wiki/existing.md" "Proposed after\r\n"})
             _ (doseq [[path text] output-files]
                 (repo/write! workspace (str "out/" path) (markdown/source-bytes text)))
             output (outputs/capture-db @workspace)
             trace {:runs (cond-> [(select-keys candidate-run [:run/id :run/parent :run/actor :run/status
                                                               :run/agent-def-hash])]
                            descendant (conj (select-keys descendant [:run/id :run/parent :run/actor :run/status])))}
             evidence ((get options :evidence-transform clojure.core/identity)
                       {:result "done" :trace trace :knowledge-output output
                        :knowledge-workspace (knowledge-workspace/expected-proof (:target basis))})
             receipt (environment/make-attempt-receipt
                      (get options :attempt-environment (:environment input))
                      {:run-id candidate :provider :dvergr :model "echo" :status (get options :receipt-status status)
                       :started-at 1000 :elapsed-ms 25
                       :metrics (merge {:program-kind :echo :model-resolution :not-applicable
                                        :agent-version 1 :agent-def-hash (hasch/uuid agent)
                                        :interpreter-version 5 :requester-party party
                                        :request-id id :execution-kind :saved-workflow
                                        :verifier-trust (evaluation/evaluator-tier evaluator)}
                                       (:metrics options))
                       :checks (get options :checks {:completed? (= :completed status) :knowledge-workspace? true})
                       :reward (get options :reward (if (= :completed status) 1.0 0.0))
                       :result (:result evidence) :trace (:trace evidence)})
             certified (attempt/make-attempt (get options :attempt-environment (:environment input))
                                             agent receipt evidence :discard)]
         (when (get options :persist? true) (attempt/persist! room certified))
         ;; The resolver alone is isolated: authorization, retained payloads,
         ;; durable Runs and Attempt storage below are real Datahike paths.
         (with-redefs [admission/default-target (fn [rid r]
                                                  (when-not (and (= rid logical-room) (identical? r room))
                                                    (throw (ex-info "Not the canonical room" {})))
                                                  {:conn workspace :identity identity})]
           (f {:conn conn :workspace workspace :room room :room-id logical-room :party party
               :principal principal :request id :job job :candidate candidate
               :certified certified :output output :basis basis :identity identity})))
       (finally
         (when-not (every? #(and (= :memory (get-in % [:store :backend]))
                                 (uuid? (get-in % [:store :id]))) fixture-configs)
           (throw (ex-info "Refusing cleanup outside the exact named-memory fixture" {})))
         (governance/ungovern! conn)
         (d/release workspace)
         (d/release conn)
         ;; Release intentionally preserves named-memory stores for reconnect.
         ;; This synchronous fixture owns both UUID stores, not just their
         ;; connections; delete them after writers have physically stopped.
         (doseq [cfg fixture-configs] (d/delete-database cfg)))))))

(defn- prepare [{:keys [conn principal room-id room request]}]
  (filing/prepare conn principal room-id room request))

(defn- candidate-outcome [{:keys [conn principal room-id room request]}]
  (filing/candidate-outcome conn principal room-id room request))

(deftest failed-candidate-is-certified-for-hold-but-never-filing
  (with-fixture {:candidate-status :failed :evidence-transform #(dissoc % :knowledge-output)}
    (fn [{:keys [conn request candidate certified] :as fixture}]
      (let [before @conn result (candidate-outcome fixture)]
        (is (= {:outcome :failed :candidate candidate :attempt-content (:attempt/content-id certified)
                 :request-binding (dispositions/request-binding (requests/get-request conn request))}
                result))
        (is (= before @conn))
        (is (thrown? clojure.lang.ExceptionInfo (prepare fixture)))
        (is (nil? (dispositions/disposition (requests/get-request conn request))))))))

(deftest failed-outcome-requires-the-same-complete-provenance-as-success
  (with-fixture {:candidate-status :failed :persist? false}
    (fn [fixture] (is (thrown? clojure.lang.ExceptionInfo (candidate-outcome fixture)))))
  ;; Writer governance already rejects some inconsistent combinations. A
  ;; broken-store read must still fail this independent host boundary; do not
  ;; deliberately dead-letter invalid writes just to construct those reads.
  (doseq [options [{:receipt-status :completed :checks {:completed? true :knowledge-workspace? true}}
                   {:checks {:completed? true :knowledge-workspace? true}}
                   {:checks {:completed? false :knowledge-workspace? false}}
                   {:checks {:completed? false :knowledge-workspace? true :unknown? true}}
                   {:reward 1.0}
                   {:metrics {:requester-party (random-uuid)}}
                   {:metrics {:request-id (random-uuid)}}
                   {:metrics {:verifier-trust :untrusted}}
                   {:evidence-transform #(dissoc % :knowledge-workspace)}
                   {:evidence-transform #(assoc-in % [:trace :runs 0 :run/status] :completed)}
                   {:evidence-transform #(update-in % [:trace :runs] (fn [rows] (conj rows (first rows))))}]]
    (with-fixture (merge {:candidate-status :failed :persist? false} options)
      (fn [{:keys [certified] :as fixture}]
        (with-redefs [episode/attempt (constantly certified)]
          (is (thrown? clojure.lang.ExceptionInfo (candidate-outcome fixture)) (str options)))))))

(deftest inconsistent-failed-durable-lineage-is-not-a-retry-permit
  (with-fixture {:candidate-status :failed}
    (fn [{:keys [conn request candidate] :as fixture}]
      (let [original run/run]
        (doseq [change [#(assoc % :run/status :completed) #(assoc % :run/parent (random-uuid))
                        #(assoc % :run/actor :other) #(assoc % :run/agent-def-hash (random-uuid))
                        #(dissoc % :run/ended-at)]]
          (with-redefs [run/run (fn [room id] (cond-> (original room id) (= id candidate) change))]
            (is (thrown? clojure.lang.ExceptionInfo (candidate-outcome fixture)))))
        (d/transact conn [[:db/add [:work.request/id request] :work.request/return-pending? true]])
        (is (thrown? clojure.lang.ExceptionInfo (candidate-outcome fixture)))))))

(deftest real-durable-attempt-plans-historical-bytes-without-live-capture-or-provider
  (with-fixture
    (fn [{:keys [conn workspace request certified basis] :as fixture}]
      (repo/write! workspace "wiki/existing.md" (markdown/source-bytes "A later human edit"))
      (let [before @conn
            result (with-redefs [work/prepare (fn [& _] (throw (AssertionError. "Provider preparation")))
                                 inputs/capture (fn [& _] (throw (AssertionError. "Live recapture")))
                                 evaluation/evaluate (fn [& _] (throw (AssertionError. "World execution")))]
                     (prepare fixture))]
        (is (= before @conn))
        (is (= result (prepare fixture)))
        (is (= :proposed (get-in result [:disposition :kind])))
        (is (= (:attempt/content-id certified) (get-in result [:disposition :attempt-content])))
        (is (= (:id basis) (get-in result [:disposition :basis])))
        (is (= request (get-in result [:request-binding :work.request/id])))
        (is (= "Historical before\r\n"
               (markdown/decode (outputs/bytes! (get-in result [:plan :bodies "wiki/existing.md" :before])))))
        (is (= "Proposed after\r\n"
               (markdown/decode (outputs/bytes! (get-in result [:plan :bodies "wiki/existing.md" :after])))))))))

(deftest explicit-empty-and-unchanged-captures-produce-no-op-without-a-proposal
  (doseq [files [{} {"wiki/existing.md" "Historical before\r\n"}]]
    (with-fixture {:output-files files}
      (fn [{:keys [output] :as fixture}]
        (let [result (prepare fixture)]
          (is (= :no-op (get-in result [:disposition :kind])))
          (is (= (:id output) (get-in result [:disposition :output])))
          (is (nil? (:plan result)))
          (is (not (contains? (:disposition result) :proposal))))))))

(deftest recursive-descendants-and-their-evidence-do-not-make-a-second-direct-candidate
  (with-fixture {:extra-descendant? true}
    (fn [fixture]
      (is (= :proposed (get-in (prepare fixture) [:disposition :kind]))))))

(deftest denied-principals-cannot-read-retained-request-artifacts
  (with-fixture
    (fn [fixture]
      (with-redefs [requests/payload (fn [& _] (throw (AssertionError. "Read before authorization")))]
        (is (thrown? clojure.lang.ExceptionInfo
                     (prepare (assoc fixture :principal {:principal/id (str "party/" (random-uuid))}))))))))

(deftest explicit-default-identity-is-required-and-current-target-is-not-retroactive-evidence
  (with-fixture
    (fn [fixture]
      (doseq [[field value] [[:registry-system (random-uuid)] [:runtime-system "other-runtime"]
                             [:repository (random-uuid)] [:store (random-uuid)]
                             [:backend :other] [:branch :other] [:ref "refs/heads/other"]]]
        (let [current (filing/current-target (:room-id fixture) (:room fixture))]
          (with-redefs [filing/current-target (fn [& _] (assoc current field value))]
            (is (thrown? clojure.lang.ExceptionInfo (prepare fixture))))))))
  (with-fixture
    {:input-transform
     (fn [input]
       (let [basis (-> (:knowledge/publication-basis input)
                       (dissoc :id)
                       (update :target dissoc :registry-system :runtime-system))]
         (assoc input :knowledge/publication-basis
                (assoc basis :id (hasch/uuid [:simmis/knowledge-publication-basis-v1 basis])))))}
    (fn [fixture] (is (thrown? clojure.lang.ExceptionInfo (prepare fixture))))))

(deftest pending-physical-return-and-wrong-durable-job-refuse
  (with-fixture
    (fn [{:keys [conn request job] :as fixture}]
      (d/transact conn [[:db/add [:work.request/id request] :work.request/return-pending? true]])
      (is (thrown? clojure.lang.ExceptionInfo (prepare fixture)))
      (d/transact conn [[:db/add [:work.request/id request] :work.request/return-pending? false]])
      (let [read-run run/run]
        (doseq [change [#(assoc % :run/status :failed) #(assoc % :run/kind :experiment)
                        #(assoc % :run/parent (random-uuid)) #(assoc % :run/actor :dvergr)
                        #(assoc % :run/room :other) #(dissoc % :run/ended-at)]]
          (with-redefs [run/run (fn [room id] (cond-> (read-run room id) (= job id) change))]
            (is (thrown? clojure.lang.ExceptionInfo (prepare fixture)))))))))

(deftest direct-candidate-scan-is-bounded-and-hired-descendants-cannot-substitute
  (with-fixture
    (fn [{:keys [room job candidate] :as fixture}]
      (let [original-runs run/runs
            direct (run/run room candidate)]
        (doseq [rows [[] [(assoc direct :run/parent (random-uuid))]
                      [direct (assoc direct :run/id (random-uuid))]
                      (repeat (inc filing/max-runs) direct)]]
          (with-redefs [run/runs (fn [_ opts]
                                   (is (= {:root-run-id job :limit (inc filing/max-runs)} opts))
                                   rows)]
            (is (thrown? clojure.lang.ExceptionInfo (prepare fixture)))))
        (is (seq (original-runs room {:root-run-id job :limit 1001})))))))

(deftest canonical-certification-still-requires-requester-metrics-and-positive-checks
  (doseq [options [{:checks {:other? true}} {:checks {:completed? false}}
                   {:checks {:completed? true :other? false}}
                   {:metrics {:requester-party (random-uuid)}}
                   {:metrics {:request-id (random-uuid)}}
                   {:metrics {:execution-kind :workflow-candidate}}
                   {:metrics {:verifier-trust :untrusted}}]]
    (with-fixture options
      (fn [fixture] (is (thrown? clojure.lang.ExceptionInfo (prepare fixture)))))))

(deftest an-empty-canonical-check-map-from-a-broken-store-is-not-vacuous-success
  ;; The real Datahike Attempt governance already refuses an empty check map
  ;; when persisting. Simulate a broken store read to exercise this additional
  ;; host boundary without disabling that real writer's governance.
  (with-fixture
    (fn [{:keys [certified] :as fixture}]
      (let [definition (:attempt/environment certified)
            receipt-options (into {}
                                  (map (fn [[k v]] [(keyword (name k)) v]))
                                  (select-keys (:attempt/receipt certified)
                                               [:attempt/run-id :attempt/provider :attempt/model :attempt/status
                                                :attempt/started-at :attempt/elapsed-ms :attempt/metrics
                                                :attempt/checks :attempt/reward :attempt/result :attempt/trace
                                                :attempt/resources]))
            receipt (environment/make-attempt-receipt definition (assoc receipt-options :checks {}))
            broken (attempt/make-attempt definition (:attempt/agent certified) receipt
                                         (:attempt/evidence certified) (:attempt/settlement-intent certified))]
        (is (= broken (attempt/validate-attempt broken)))
        (with-redefs [episode/attempt (constantly broken)]
          (is (thrown? clojure.lang.ExceptionInfo (prepare fixture))))))))

(deftest missing-or-malformed-certified-output-is-not-an-empty-no-op
  (doseq [transform [#(dissoc % :knowledge-output)
                     #(assoc-in % [:knowledge-output :id] (random-uuid))]]
    (with-fixture {:evidence-transform transform}
      (fn [fixture] (is (thrown? clojure.lang.ExceptionInfo (prepare fixture)))))))

(deftest repeated-direct-candidate-in-certified-trace-is-ambiguous
  (with-fixture {:evidence-transform #(update-in % [:trace :runs] (fn [rows] (conj rows (first rows))))}
    (fn [fixture] (is (thrown? clojure.lang.ExceptionInfo (prepare fixture))))))

(deftest frozen-source-and-input-selection-bindings-cannot-drift
  (doseq [transform [#(assoc-in % [:workflow :input-selection :execution-id] (random-uuid))
                     #(assoc-in % [:source "workflow.edn"] "{:task \"Different workflow\"}")
                     #(assoc-in % [:inputs "/in/knowledge/sources/inbox/evidence.md"] "Different input")
                     #(assoc-in % [:candidate :agent/name] "Different frozen AgentDef")]]
    (with-fixture {:input-transform transform}
      (fn [fixture] (is (thrown? clojure.lang.ExceptionInfo (prepare fixture)))))))

(deftest frozen-input-repository-must-match-the-publication-repository
  (with-fixture
    {:input-transform
     (fn [input]
       (let [basis (-> (:knowledge/publication-basis input) (dissoc :id)
                       (assoc-in [:target :repository] (random-uuid)))]
         (assoc input :knowledge/publication-basis
                (assoc basis :id (hasch/uuid [:simmis/knowledge-publication-basis-v1 basis])))))}
    (fn [{:keys [conn request] :as fixture}]
      ;; Isolate this invariant from the separate current-physical-target
      ;; check: both historical/current targets agree, but source evidence
      ;; still came from a different repository.
      (let [target (:target (requests/publication-basis conn request))]
        (with-redefs [filing/current-target (fn [& _] target)]
          (is (= :input-repository-mismatch
                 (try (prepare fixture) nil
                      (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))))))

(deftest explicit-registry-snapshot-is-forwarded-for-current-target-resolution
  (with-fixture
    (fn [{:keys [conn workspace identity room room-id]}]
      (let [registry-db @conn seen (atom nil)
            descriptor {:conn workspace :identity identity}
            expected (filing/current-target room-id room)]
        (with-redefs [admission/default-target
                      (fn [db rid r]
                        (reset! seen {:same-db? (identical? registry-db db) :room-id rid :same-room? (identical? room r)})
                        descriptor)]
          (is (= expected (filing/current-target registry-db room-id room)))
          (is (= {:same-db? true :room-id room-id :same-room? true} @seen)))))))

(def native-written-path "wiki/native.md")
(def native-written-text "# Retained knowledge\n\nWritten by bounded SCI.\n")

(defn- native-workspace-probe*
  "Explicit no-model diagnostic, not a scheduled job or a default test. Runs
   the actual Job/fork/setup/Echo/certification driver with two repositories.
   The memory default descriptor is isolated; this is NOT a file-registry
   ownership test. All system DB and Room registry changes are scoped/restored.
   The base fixture mints dispatch units only. Its scheduled continuation
   separately documents its isolated synthetic ledger allowance."
  ([owned-first? program-mode]
   (native-workspace-probe* owned-first? program-mode nil))
  ([owned-first? program-mode fixture-flow]
   (when-not (contains? #{:echo :sci-write} program-mode)
     (throw (ex-info "Unknown fixed native diagnostic mode" {})))
   (authority-world/with-world [conn w]
     (@#'is.simm.ops.work-test/with-room
      (fn [room]
        (d/transact conn [{:room/id (:room w) :room/slug (store/room-id->slug (:id room))}])
        (d/transact conn
                    (mapv (fn [system]
                            {:system/id (:system/id system)
                             :system/scope (str "/fixture/knowledge-native/" (:system/id system))})
                          (filter #(contains? #{:kb :repo} (:system/type %)) (system-db/all-systems))))
        (let [first-conn (workspace/open-memory-workspace! room)
              second-conn (workspace/open-memory-workspace! room)
              owned (if owned-first? first-conn second-conn)
              attached (if owned-first? second-conn first-conn)
              systems (vals (ygg/registered-systems))
              system-id (fn [connection]
                          (some (fn [system]
                                  (when (and (= :geschichte (protocols/system-type system))
                                             (identical? connection (geschichte/gy-connection system)))
                                    (str (protocols/system-id system)))) systems))
              identity {:registry-system (random-uuid) :runtime-system (system-id owned)}
              _ (repo/write! owned "inbox/evidence.md" (markdown/source-bytes "Owned input evidence"))
              _ (repo/write! attached "inbox/evidence.md" (markdown/source-bytes "Other repository evidence"))
              _ (repo/stage-all! owned)
              _ (repo/stage-all! attached)
              _ (repo/commit! owned {:message "Owned evidence for native selection probe"})
              _ (repo/commit! attached {:message "Attached evidence for native selection probe"})
              captured (inputs/capture @owned inputs/default-policy)
              basis (inputs/publication-basis @owned identity)
              attached-before (inputs/publication-basis @attached)
              proposal-count-before (count (d/q '[:find ?p :where [?p :proposal/id _]] @conn))
              submission {:request-id (random-uuid) :name "memory"
                          :expected-bundle (:id (catalog/bundle "memory" source))
                          :model "fixture-no-provider"
                          :limits {:budget-microdollars 1 :max-dispatches 1 :max-model-steps 1
                                   :timeout-ms 30000 :settlement "discard"}}
              source-prepared (workflows/prepare-source {:files source}
                                                        (select-keys submission workflows/source-fields) captured)
              team (roster/make-agent (roster/make-roster {:id :knowledge-native-probe})
                                      {:id :candidate :tools #{} :sandbox #{}
                                       :program (if (= :sci-write program-mode)
                                                  {:kind :sci :timeout-ms 30000 :args []
                                                   :entrypoint 'knowledge.native/main
                                                   :source (str "(ns knowledge.native)"
                                                                "(require '[babashka.fs :as fs])"
                                                                "(defn main [_] (fs/create-dirs \"/out/wiki\")"
                                                                "(spit " (pr-str (str "/out/" native-written-path))
                                                                " " (pr-str native-written-text) ")"
                                                                "\"Prepared retained wiki change.\")")}
                                                  {:kind :echo})})
              agent (roster/agent team :candidate)
              evaluator (outputs/with-capture work/completion)
              verifier (evaluation/evaluator-ref evaluator)
              definition (environment/make-environment
                          {:id :simmis/saved-workflow :task (:task source-prepared)
                           :verifier {:id (:verifier/id verifier) :version (:verifier/version verifier)
                                      :basis (:verifier/basis verifier)}
                           :limits {:timeout-ms 30000 :cancel-timeout-ms 5000}
                           :world {:isolation :ctx :settlement :discard :effects work/effects
                                   :setup (evaluation/world-setup-ref (:world-setup source-prepared))}
                           :metadata {:workflow (:workflow source-prepared)}})
              observed (promise)
              setup (update (:world-setup source-prepared) :prepare
                            (fn [prepare-world!]
                              (fn [{world :room :as context}]
                                (binding [ec/*execution-context* (:ctx world)]
                                  (let [{:keys [system conn]} (geschichte/current-workspace)]
                                    (deliver observed {:runtime-system (some-> system protocols/system-id str)
                                                       :repository (:geschichte.repo/id (query/repository @conn))
                                                       :store (get-in @conn [:config :store :id])
                                                       :backend (get-in @conn [:config :store :backend])
                                                       :branch (get-in @conn [:config :branch])})))
                                (prepare-world! context))))
              input {:candidate agent :environment definition :source source
                     :inputs (:inputs source-prepared) :workflow (:workflow source-prepared)
                     :submission submission :knowledge/publication-basis basis}
              prepared (workflows/bind-publication
                        {:team team :agent :candidate :environment definition :evaluator evaluator
                         :world-setup setup :request-kind :workflow :execution-kind :saved-workflow
                         :delegation-policy {:tools #{} :sandboxes #{#{}} :providers #{}
                                             :program-kinds #{:echo :sci} :max-attempts 1 :max-parallelism 1}
                         :resources {resource/model-dispatches 1M} :basis input}
                        {:publication-basis basis :captured captured})
              principal {:principal/id (str "party/" (:owner w))
                         :principal/actor (keyword "party" (str (:owner w)))}]
         ;; Synthetic dispatch allowance only; no paid model exists in team.
          (resource/mint! room {:id (random-uuid) :resources {resource/model-dispatches 1M}})
          (let [resolve-target (fn [rid r]
                                 (when-not (and (= rid (:room w)) (identical? r room))
                                   (throw (ex-info "Noncanonical memory probe room" {})))
                                 {:conn owned :identity identity})]
           ;; Only the test host default resolver is isolated, in both writer
           ;; snapshot arities. Authority, canonical connection, publication,
           ;; durable card projection and all storage/transactions are real.
            (with-redefs [work/prepare (fn [& _] (throw (AssertionError. "Provider preparation in native probe")))
                          admission/default-target (fn
                                                     ([rid r] (resolve-target rid r))
                                                     ([_db rid r] (resolve-target rid r)))]
              (if fixture-flow
               ;; A test-only fixture continuation, never an agent execution
               ;; lane or persisted callback. The scheduled probe below uses
               ;; the same real Room, repositories, principal and fixed SCI.
                (fixture-flow {:conn conn :room room :room-id (:room w) :principal principal
                               :prepared prepared :submission submission :owned owned :attached attached
                               :identity identity :basis basis :attached-before attached-before
                               :proposal-count-before proposal-count-before :observed observed
                               :captured captured :definition definition :agent agent})
                (let [admitted (work/submit-prepared! conn ec/*execution-context* principal (:room w) room submission prepared)
                      status (jobs/status (:job admitted) 25000)
                      request (requests/get-request conn (:id admitted))
                      result (when (and (= :completed (:status status))
                                        (false? (:work.request/return-pending? request)))
                               (filing/prepare conn principal (:room w) room (:id admitted)))
                      _ (when (= :sci-write program-mode)
                       ;; Refuse before publication if the real lookup would
                       ;; need to hydrate any filesystem-backed application
                       ;; room. This isolated diagnostic owns only this Room.
                          (let [slug (:room/slug (d/entity @conn [:room/id (:room w)]))]
                            (when-not (and slug (identical? room (registry/lookup (store/slug->room-id slug))))
                              (throw (ex-info "Native diagnostic Room is not the registered publication target" {})))))
                      filed (when (and result (= :sci-write program-mode))
                              (publish/file-completed-request! conn principal (:room w) room (:id admitted)))
                      replayed (when filed
                                 (with-redefs [filing/prepare (fn [& _] (throw (AssertionError. "Historical retry recertified")))]
                                   (publish/file-completed-request! conn principal (:room w) room (:id admitted))))
                      proposal (when filed (proposals/get-proposal (:proposal filed)))]
                  {:owned-first? owned-first? :job-status (:status status) :job-error (:error status)
                   :program-mode program-mode
                   :returned? (false? (:work.request/return-pending? request))
                   :expected {:runtime-system (system-id owned)
                              :repository (:geschichte.repo/id (query/repository @owned))
                              :store (get-in @owned [:config :store :id])
                              :backend (get-in @owned [:config :store :backend])}
                   :source-branch (get-in @owned [:config :branch])
                   :attached {:runtime-system (system-id attached)
                              :repository (:geschichte.repo/id (query/repository @attached))}
                   :observed (deref observed 0 :not-prepared)
                   :workspace-proof (some-> (get-in result [:disposition :candidate])
                                            (#(episode/attempt room %)) :attempt/evidence :knowledge-workspace)
                   :disposition (:kind (:disposition result))
                   :filed filed :replayed replayed
                   :joint-receipt (when filed (dispositions/disposition (requests/get-request conn (:id admitted))))
                   :new-proposals (- (count (d/q '[:find ?p :where [?p :proposal/id _]] @conn)) proposal-count-before)
                   :message-status (:proposal/message-status proposal)
                   :cards (when filed (count (filter #(= (:proposal/message-id proposal) (:id %))
                                                     (discourse/messages room))))
                   :retained-pair (when filed (output-storage/read-file conn (:manifest filed) native-written-path))
                   :parents-unchanged? (and (= basis (inputs/publication-basis @owned identity))
                                            (= attached-before (inputs/publication-basis @attached)))}))))))))))

(defn native-workspace-probe
  "Opt-in Echo driver diagnostic; no model/provider, no Proposal or room card."
  [owned-first?]
  (native-workspace-probe* owned-first? :echo))

(defn native-written-workspace-probe
  "Opt-in bounded SCI -> retained Proposal -> joint receipt -> one real room
   card -> historical retry diagnostic. No provider/money/new execution lane.
   Test-only memory default resolution is NOT a provisioned-home auth test.
   This writes no canonical wiki files and performs no adoption."
  [owned-first?]
  (native-workspace-probe* owned-first? :sci-write))

(defn- native-tree [conn]
  (let [db @conn]
    {:head (:geschichte.commit/id (repo/head-commit conn))
     :files (into (sorted-map)
                  (map (fn [row] [(:v row) (d/pull db '[*] (:e row))]))
                  (d/datoms db :aevt :geschichte.work/path))
     :directories (set (map :v (d/datoms db :aevt :geschichte.work-dir/path)))}))

(defn- fixed-scheduled-native [state calls]
  (fn [{:keys [task model limits]}]
    (when-not (= "codex-subscription-luna-6" model)
      (throw (ex-info "Diagnostic factory accepts only the pinned registry selection" {})))
    (swap! calls inc)
    (let [grant (work/resources limits)
          evaluator (update work/completion :capture
                            (fn [capture]
                              (fn [{world :world/room :as context}]
                                (binding [ec/*execution-context* (:ctx world)]
                                  (let [{:keys [system conn]} (geschichte/current-workspace)]
                                    (deliver (:observed state)
                                             {:runtime-system (str (protocols/system-id system))
                                              :repository (:geschichte.repo/id (query/repository @conn))
                                              :store (get-in @conn [:config :store :id])
                                              :backend (get-in @conn [:config :store :backend])
                                              :branch (get-in @conn [:config :branch])})))
                                (capture context))))
          ref (evaluation/evaluator-ref evaluator)
          env (environment/make-environment
               {:id :simmis/bounded-work :task task
                :verifier {:id (:verifier/id ref) :version (:verifier/version ref) :basis (:verifier/basis ref)}
                :world {:isolation :ctx :settlement (keyword (:settlement limits))
                        :effects work/effects :resources grant}
                :limits {:timeout-ms (:timeout-ms limits) :cancel-timeout-ms 5000}})]
      {:team (get-in state [:prepared :team]) :agent (get-in state [:prepared :agent])
       :environment env :evaluator evaluator :resources grant
       :delegation-policy (get-in state [:prepared :delegation-policy])
       :basis {:candidate (:agent state) :environment env :diagnostic :fixed-native-sci}})))

(defn native-scheduled-workspace-probe
  "Opt-in real create!/claim/tick -> native SCI Job -> fixed filing -> cursor
   diagnostic. The ONLY candidate seam is a fixed native work/prepare factory;
   explicit installed-model/host policy, authority, allowance checks, capture,
   publication, cursor and room-card code are real. One synthetic microdollar
   and dispatch exist only in this isolated memory ledger: no provider account,
   provider admission, inference, configured balance or default activation.
   Both repositories seed identical code: this does NOT prove owned-repository
   source selection (the ambient source-snapshot gap remains). Only execution
   and output target selection are tested. Repeated ticks use the same logical
   due time, never a wall-clock sleep; this is not a crash-replay diagnostic."
  [owned-first?]
  (native-workspace-probe*
   owned-first? :sci-write
   (fn [{:keys [conn room room-id principal submission prepared owned attached identity basis
                proposal-count-before observed] :as state}]
     ;; Both attachments carry identical authored code. Registry order may
     ;; choose the source view, but must never choose the execution/output repo.
     (doseq [repository [owned attached]]
       (doseq [[path text] source]
         (repo/write! repository (str "workflows/memory/" path) (markdown/source-bytes text)))
       (repo/stage-all! repository)
       (repo/commit! repository {:message "Seed scheduled native workflow"}))
     (let [parents-before [(native-tree owned) (native-tree attached)]
           ctx ec/*execution-context*
           calls (atom 0)
           execution (-> submission (dissoc :request-id) (assoc :model "codex-subscription-luna-6"))
           create-input {:request-id (random-uuid) :kind "workflow"
                         :cadence {:interval-ms 60000 :tz "UTC"} :execution execution}
           unknown-model (str "not-installed-" (random-uuid))
           invalid-rejected? (try
                               (scheduler/create! conn ctx principal room-id room
                                                  (assoc-in create-input [:execution :model] unknown-model))
                               false
                               (catch clojure.lang.ExceptionInfo e
                                 (= :is.simm.ops.workflow-schedules/invalid (:type (ex-data e)))))
           _ (when-not invalid-rejected? (throw (ex-info "Real schedule model gate did not reject an unknown id" {})))
           _ (when-not (= "codex-subscription-luna-6" (:id (models/get-model "codex-subscription-luna-6")))
               (throw (ex-info "Explicit diagnostic model id is not installed" {})))
           ;; Authorized fixture-only accounting units, not real monetary funding.
           _ (resource/mint! room {:id (random-uuid) :resources {resource/microdollars 1M}})
           allowance-before (resource/balance room)
           row (scheduler/create! conn ctx principal room-id room create-input)
           retried-create (scheduler/create! conn ctx principal room-id room create-input)
           _ (when-not (= (:schedule.activation/id row) (:schedule.activation/id retried-create))
               (throw (ex-info "Schedule creation retry changed identity" {})))
           due (:schedule.activation/next-fire row)
           resolve-room (fn [id] (when (= room-id id) room))
           _ (with-redefs [work/prepare (fixed-scheduled-native state calls)]
               (scheduler/tick! conn ctx resolve-room due))
           active (schedule-model/activation conn (:schedule.activation/id row))
           occurrence (schedule-model/occurrence conn (:schedule.activation/active-occurrence active))
           rid (:schedule.occurrence/request occurrence)
           request (when rid (requests/get-request conn rid))
           job (:work.request/job request)
           status (when job (jobs/status job 25000))
           request (when rid (requests/get-request conn rid))
           slug (:room/slug (d/entity @conn [:room/id room-id]))
           _ (when-not (and slug (identical? room (registry/lookup (store/slug->room-id slug))))
               (throw (ex-info "Scheduled diagnostic refuses application-room hydration" {})))
           ;; No manual filing/cursor shortcut: this tick MUST perform both.
           _ (scheduler/tick! conn ctx resolve-room due)
           after (schedule-model/activation conn (:schedule.activation/id row))
           oid (:schedule.occurrence/id occurrence)
           proof (schedule-model/processing-proof conn oid)
           receipt (when rid (dispositions/disposition (requests/get-request conn rid)))
           cursor-fact (fn []
                         (some (fn [datom]
                                 (when (= (:db/id after) (:e datom))
                                   {:value (:v datom) :tx (:tx datom)}))
                               (d/datoms @conn :aevt :schedule.activation/last-processed-input)))
           cursor-before-retry (cursor-fact)
           ;; Existing fixture scope already forbids work/prepare outside the
           ;; single fixed-factory tick, catching unintended redispatch.
           _ (dotimes [_ 2] (scheduler/tick! conn ctx resolve-room due))
           final (schedule-model/activation conn (:schedule.activation/id row))
           final-occurrence (schedule-model/occurrence conn oid)
           final-request (when rid (requests/get-request conn rid))
           final-receipt (when rid (dispositions/disposition final-request))
           proposal (when receipt (proposals/get-proposal (:proposal receipt)))
           certified (when receipt (episode/attempt room (:candidate receipt)))
           target (schedule-model/target conn final)]
       {:owned-first? owned-first? :job-status (:status status) :job-error (:error status)
        :source-policy-scope :identical-fixture-code-not-owned-source-proof
        :activation-reason (:schedule.activation/reason final)
        :activation-enabled? (:schedule.activation/enabled? final)
        :returned? (false? (:work.request/return-pending? final-request))
        :invalid-model-rejected? invalid-rejected? :factory-calls @calls
        :model-policy-current? (= (:model-policy target) (scheduler/model-policy :workflow execution))
        :host-policy-current? (= (:host-policy target) (scheduler/host-policy))
        :occurrences (count (schedule-model/occurrences conn (:schedule.activation/id row)))
        :occurrence final-occurrence :request rid :client-id (:work.request/client-id final-request)
        :expected-request (when oid (requests/request-id (:schedule.activation/by row) room-id oid))
        :request-count (count (d/q '[:find ?e :in $ ?room :where [?e :work.request/room ?room]] @conn room-id))
        :workflow-jobs (count (filter #(= :workflow (:run/kind %)) (run/runs room)))
        :proof proof :proof-after-retry (schedule-model/processing-proof conn oid)
        :receipt receipt :receipt-after-retry final-receipt
        :cursor (:schedule.activation/last-processed-input after)
        :cursor-after-retry (:schedule.activation/last-processed-input final)
        :cursor-fact-before-retry cursor-before-retry :cursor-fact-after-retry (cursor-fact)
        :active-after-retry (:schedule.activation/active-occurrence final)
        :expected-target (:target basis) :observed (deref observed 0 :not-captured)
        :workspace-proof (get-in certified [:attempt/evidence :knowledge-workspace])
        :new-proposals (- (count (d/q '[:find ?p :where [?p :proposal/id _]] @conn)) proposal-count-before)
        :message-status (:proposal/message-status proposal)
        :cards (count (filter #(= (:proposal/message-id proposal) (:id %)) (discourse/messages room)))
        :completion-cards (count (filter #(and (= :workflow/completed (get-in % [:metadata :kind]))
                                               (= job (get-in % [:metadata :run-id])))
                                         (discourse/messages room)))
        :retained-pair (when receipt (output-storage/read-file conn (:manifest receipt) native-written-path))
        :allowance-before allowance-before :allowance-after (resource/balance room)
        :parents-unchanged? (= parents-before [(native-tree owned) (native-tree attached)])}))))

(deftest only-the-exact-frozen-environment-and-durable-attempt-can-certify
  (with-fixture
    {:attempt-environment (environment/make-environment
                           {:id :another/environment :task "Different task"
                            :verifier {:id :another/verifier}})}
    (fn [fixture] (is (thrown? clojure.lang.ExceptionInfo (prepare fixture)))))
  (with-fixture {:persist? false}
    (fn [fixture] (is (thrown? clojure.lang.ExceptionInfo (prepare fixture))))))

(deftest no-configured-store-means-no-certified-filing
  (with-fixture
    (fn [fixture]
      (let [room (:room fixture)]
        (with-redefs [admission/default-target (fn [& _] {:conn (:workspace fixture) :identity (:identity fixture)})]
          (is (thrown? clojure.lang.ExceptionInfo (prepare (assoc fixture :room (dissoc room :store))))))))))

(deftest historical-unbound-attempts-remain-readable-but-cannot-gain-filing-authority
  (with-fixture {:unbound? true}
    (fn [{:keys [room candidate] :as fixture}]
      (is (some? (episode/attempt room candidate)))
      (is (thrown? clojure.lang.ExceptionInfo (prepare fixture))))))

(deftest trusted-workspace-evidence-and-explicit-check-are-both-required
  (doseq [options [{:evidence-transform #(dissoc % :knowledge-workspace)}
                   {:evidence-transform #(assoc-in % [:knowledge-workspace :actual :store] (random-uuid))}
                   {:checks {:completed? true}}
                   {:checks {:completed? true :knowledge-workspace? false}}]]
    (with-fixture options
      (fn [fixture] (is (thrown? clojure.lang.ExceptionInfo (prepare fixture)))))))

(deftest local-worker-filing-remains-explicitly-outside-native-certification
  (with-fixture {:environment-id :simmis/local-worker}
    (fn [fixture]
      (is (= :unsupported-workflow
             (try (prepare fixture) nil
                  (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))))

(deftest copied-context-requests-cannot-acquire-checkpoint-filing-by-annotation
  (doseq [selection [nil {} {:version 1 :policy :request-evidence-v1}]]
    (with-fixture {:input-transform #(assoc % :knowledge/checkpoint selection)}
      (fn [fixture]
        (is (= :checkpoint-batch-mismatch
               (try (prepare fixture) nil
                    (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))))
