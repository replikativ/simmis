(ns is.simm.ops.owned-worker-admission-test
  "Layer A uses real source/enrollment/claim facts without Jobs. Layer B uses
   the real kernel/protocol with an in-process authenticated worker transport,
   not an OS process, provider, containment or deployed provisioning proof."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.catalog.room :as catalog]
            [dvergr.catalog.workspace :as catalog-workspace]
            [dvergr.discourse :as discourse]
            [dvergr.jobs :as jobs]
            [dvergr.resource :as resource]
            [dvergr.room.store :as store]
            [dvergr.room.store.memory :as memory]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [is.simm.model.work-requests :as requests]
            [is.simm.model.workers :as workers]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.benchmark-fixture :as native-fixture]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.knowledge-inputs :as knowledge]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-worker-admission :as owned]
            [is.simm.ops.owned-workflow-admission-test :as joined]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.work :as work]
            [is.simm.ops.worker-benchmarks :as local-benchmarks]
            [is.simm.ops.worker-credentials :as credentials]
            [is.simm.ops.worker-files-test :as file-fixture]
            [is.simm.ops.worker-service :as service]
            [is.simm.ops.worker-work :as local]
            [is.simm.ops.workflow-runs :as workflow-runs]
            [is.simm.ops.workflows :as workflows]
            [jsonista.core :as json]
            [org.replikativ.spindel.engine.core :as ec]))

(def ^:private source
  {"workflow.edn" "{:title \"Owned worker workflow\" :task \"Read /docs/input.md and write its text to /out/result.md\" :capture [\"/out\"]}"
   "checker.clj" "(ns checker) (defn check [{:keys [files gold]}] (let [ok (= (:expected gold) (get files \"/out/result.md\"))] {:checks {:answer? ok} :reward (if ok 1 0)}))"
   "gold.edn" "{:expected \"Owned document\" :private \"PRIVATE-CHECKER-GOLD\"}"
   "fixtures/docs/input.md" "Owned document"
   "helpers/code.clj" "(def owned true)"})

(defn- seed! [conn files]
  (doseq [[path text] files]
    (repo/write! conn (str "workflows/echo/" path) (.getBytes ^String text "UTF-8"))))
(defn- state [conn]
  (let [db @conn] {:commit (d/commit-id db) :config (:config db) :worktree (query/worktree db)}))
(defn- principal [w] (ops/party-principal (:owner w)))
(defn- promote! [conn w] (d/transact conn [{:party/id (:owner w) :party/role :admin}]))
(defn- failure [f] (try (f) nil (catch clojure.lang.ExceptionInfo error error)))
(defn- submit! [kind & args]
  (apply (case kind :local-worker-workflow owned/submit-workflow! :local-worker-benchmark owned/submit-benchmark!) args))
(defn- input [kind host w worker]
  (cond-> {:request-id (random-uuid) :worker worker :name "echo"
           :expected-bundle (:id (catalog/bundle "echo" source))
           :expected-owned-source (parse-uuid (get-in (sources/details! host (principal w) (str (:room w)) {:name "echo"}) [:data :revision]))
           :limits {:timeout-ms 30000 :settlement "discard"}}
    (= kind :local-worker-benchmark) (assoc :repetitions 1)))
(defn- claim-only [seen]
  (fn [conn ctx p room-id _ admitted prepared]
    (reset! seen {:prepared prepared :kernel admitted})
    (requests/claim! conn {:party (work/authorize! conn p room-id) :room room-id
                           :client-id (:request-id admitted) :kind (:request-kind prepared)
                           :intent (:basis prepared) :limits (update (:limits admitted) :settlement keyword)})
    (work/status conn ctx p room-id (requests/request-id (parse-uuid (subs (:principal/id p) 6)) room-id (:request-id admitted)))))

(defn- with-worker [order f]
  (@#'fixture/with-fixture
   order (fn [{:keys [conn w owned attached host] :as fixture}]
           (promote! conn w)
           (seed! owned source)
           (repo/write! owned "workflows/echo/helpers/code.clj"
                        (.getBytes ^String (get source "helpers/code.clj") "UTF-8") {:mode 33261})
           (seed! attached (assoc source "helpers/code.clj" "OTHER helper" "fixtures/docs/input.md" "OTHER document"))
           (let [id (random-uuid) enrolled (service/enroll! conn (principal w) (:room w) "owned-worker-fixture" id file-fixture/config)]
             (f (assoc fixture :worker id :enrolled enrolled :request #(input % host w id)))))))

(deftest both-fixed-kinds-freeze-owned-source-and-original-intent-in-both-orders
  (doseq [kind [:local-worker-workflow :local-worker-benchmark] order [true false]]
    (with-worker order
      (fn [{:keys [w host owned attached request]}]
        (let [input (request kind) seen (atom nil) before (state owned) other (state attached)
              selected (workspace/selected-system-id)]
          (with-redefs [work/submit-prepared! (claim-only seen)]
            (let [result (submit! kind host (principal w) (str (:room w)) input)
                  prepared (:prepared @seen) basis (:basis prepared)]
              (is (= kind (:request-kind prepared) (:execution-kind prepared)))
              (is (= input (:submission basis)))
              (is (= :host (:admission-channel basis) (:submission-channel prepared)))
              (is (= 1 (get-in @seen [:kernel :limits :max-worker-admissions])))
              (is (nil? (get-in basis [:submission :limits :max-worker-admissions])))
              (is (= (:expected-owned-source input) (get-in basis [:source-origin :revision])))
              (is (uuid? (get-in basis [:source-origin :target-stamp])))
              (is (nil? (:job result)))
              (if (= kind :local-worker-workflow)
                (do (is (= (conj (set (map #(str "workflows/echo/" %) (keys source))) "docs/input.md")
                           (set (keys (get-in basis [:frozen :files])))))
                    (is (= 33261 (get-in basis [:frozen :files "workflows/echo/helpers/code.clj" :mode]))))
                (let [frozen (get-in (first (vals (:frozen basis))) [:frozen :files])]
                  (is (= #{"docs/input.md"} (set (keys frozen))))
                  (is (not (re-find #"PRIVATE-CHECKER-GOLD|OTHER document" (pr-str frozen))))))))
          (is (= before (state owned))) (is (= other (state attached)))
          (is (= selected (workspace/selected-system-id))))))))

(deftest exact-retry-precedes-source-enrollment-config-and-adapter-even-after-revocation
  (doseq [kind [:local-worker-workflow :local-worker-benchmark]]
    (with-worker true
      (fn [{:keys [conn w host worker request]}]
        (let [input (request kind) p (principal w) seen (atom nil)]
          (with-redefs [work/submit-prepared! (claim-only seen)]
            (let [initial (submit! kind host p (str (:room w)) input)]
              (workers/revoke! conn worker)
              (let [calls (atom 0) forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Retry performed fresh work")))]
                (with-redefs [reads/resolve-host-read! forbidden workflows/manifest-in forbidden workflows/snapshot-in forbidden
                              local/enrollment! forbidden workers/payload forbidden local/prepare-workflow-in forbidden
                              local-benchmarks/prepare forbidden local-benchmarks/adapter forbidden]
                  (is (= (:id initial) (:id (submit! kind host p (str (:room w)) input))))
                  (is (= ::owned/conflict (:type (ex-data (failure #(submit! kind host p (str (:room w)) (assoc input :worker (random-uuid))))))))
                  (is (zero? @calls)))))))))))

(deftest same-client-opposite-kind-is-a-conflict-not-an-enrollment-or-source-read
  (with-worker true
    (fn [{:keys [w host request]}]
      (let [input (request :local-worker-workflow) seen (atom nil) calls (atom 0)]
        (with-redefs [work/submit-prepared! (claim-only seen)]
          (owned/submit-workflow! host (principal w) (str (:room w)) input)
          (with-redefs [reads/resolve-host-read! (fn [& _] (swap! calls inc))]
            (is (= ::owned/conflict
                   (:type (ex-data (failure #(owned/submit-benchmark! host (principal w) (str (:room w))
                                                                      (assoc input :repetitions 1)))))))
            (is (zero? @calls))))))))

(deftest workflow-live-selection-and-publication-use-the-same-owned-db
  (with-worker true
    (fn [{:keys [w host owned attached request]}]
      (let [files (assoc source "input-selection.edn"
                         "{:kind :knowledge :policy {:version 1 :trigger-roots [\"inbox\"] :context-roots [\"wiki\"] :exclude-prefixes []}}")
            seen (atom nil)]
        (seed! owned files)
        (repo/write! owned "inbox/evidence.md" (.getBytes "Owned trigger" "UTF-8"))
        (repo/write! owned "wiki/context.md" (.getBytes "Owned context" "UTF-8"))
        (repo/write! attached "inbox/evidence.md" (.getBytes "OTHER trigger" "UTF-8"))
        (repo/write! attached "wiki/context.md" (.getBytes "OTHER context" "UTF-8"))
        (let [input (assoc (request :local-worker-workflow) :expected-bundle (:id (catalog/bundle "echo" files)))]
          (with-redefs [work/submit-prepared! (claim-only seen)]
            (owned/submit-workflow! host (principal w) (str (:room w)) input))
          (let [basis (get-in @seen [:prepared :basis]) frozen (get-in basis [:frozen :files])]
            (is (= (file-fixture/encoded "Owned trigger") (get frozen "in/knowledge/sources/inbox/evidence.md")))
            (is (= (file-fixture/encoded "Owned context") (get frozen "in/knowledge/sources/wiki/context.md")))
            (is (contains? (get-in basis [:knowledge/publication-basis :files]) "wiki/context.md"))
            (is (not (re-find #"OTHER trigger|OTHER context" (pr-str frozen))))))))))

(deftest source-conflict-and-malformed-source-results-are-prebody-pre-enrollment
  (with-worker true
    (fn [{:keys [w host request]}]
      (let [input (request :local-worker-workflow) calls (atom 0)
            forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Prebody gate failed")))]
        (with-redefs [workflows/snapshot-in forbidden local/enrollment! forbidden workers/payload forbidden work/submit-prepared! forbidden]
          (is (= ::owned/conflict (:type (ex-data (failure #(owned/submit-workflow! host (principal w) (str (:room w))
                                                                                    (assoc input :expected-owned-source (random-uuid))))))))
          (doseq [bad [nil "not-a-UUID"]]
            (with-redefs [sources/source-id (constantly bad)]
              (is (= ::owned/unavailable (:type (ex-data (failure #(owned/submit-workflow! host (principal w) (str (:room w)) input))))))))
          (is (zero? @calls)))))))

(deftest malformed-dto-and-worker-identity-never-become-human-admission
  (with-worker true
    (fn [{:keys [w host request enrolled hydrations]}]
      (let [input (request :local-worker-workflow) p (principal w)]
        (reset! hydrations [])
        (doseq [bad [(dissoc input :expected-owned-source) (assoc input :name "echo\n") (assoc input :by (:owner w))]]
          (is (= ::owned/invalid (:type (ex-data (failure #(owned/submit-workflow! host p (str (:room w)) bad)))))))
        (let [worker (credentials/authenticate (:control-conn (::reads/owner host)) {:bearer (:credential enrolled)})]
          (is (= ::owned/unavailable (:type (ex-data (failure #(owned/submit-workflow! host worker (str (:room w)) input)))))))
        (is (empty? @hydrations))))))

(deftest final-enrollment-and-merge-revocation-refuse-canonical-claim
  (doseq [revoke [:worker :merge]]
    (with-worker true
      (fn [{:keys [conn w host worker request]}]
        (let [input (request :local-worker-workflow) prepare local/prepare-workflow-in calls (atom 0)]
          (with-redefs [local/prepare-workflow-in
                        (fn [& args]
                          (let [prepared (apply prepare args)]
                            (case revoke
                              :worker (workers/revoke! conn worker)
                              :merge (d/transact conn [{:room/id (:room w) :room/owner [:party/id (:member w)]
                                                        :room/parties [[:party/id (:owner w)]]}]))
                            prepared))
                        work/submit-prepared! (fn [& _] (swap! calls inc))]
            (is (= ::owned/unavailable (:type (ex-data (failure #(owned/submit-workflow! host (principal w) (str (:room w)) input))))))
            (is (zero? @calls))))))))

(deftest current-enrollment-comparison-is-complete-and-uses-only-supplied-snapshot
  (with-worker true
    (fn [{:keys [conn worker]}]
      (let [pinned (workers/enrollment conn worker) before @conn]
        (is (true? (local/current-enrollment-in? before pinned)))
        (doseq [field [:simmis.worker/id :simmis.worker/by :simmis.worker/room :simmis.worker/generation :simmis.worker/config-id]]
          (is (false? (local/current-enrollment-in? before (dissoc pinned field)))))
        (workers/revoke! conn worker)
        (with-redefs [workers/enrollment (fn [& _] (throw (AssertionError. "Pure comparison reread connection")))]
          (is (true? (local/current-enrollment-in? before pinned)))
          (is (false? (local/current-enrollment-in? @conn pinned))))
        (with-redefs [credentials/permitted? (constantly :truthy)]
          (is (false? (local/current-enrollment-in? before pinned))))))))

(deftest offline-benchmark-does-not-acquire-workflow-live-knowledge
  (with-worker true
    (fn [{:keys [w host request]}]
      (let [input (request :local-worker-benchmark) seen (atom nil) calls (atom 0)]
        (with-redefs [workflow-runs/resolve-admission-in (fn [& _] (swap! calls inc) (throw (AssertionError. "Benchmark scanned LIVE")))
                      work/submit-prepared! (claim-only seen)]
          (is (:id (owned/submit-benchmark! host (principal w) (str (:room w)) input)))
          (is (zero? @calls)))))))

(deftest undeclared-workflow-inputs-do-not-scan-live-knowledge
  (with-worker true
    (fn [{:keys [w host request]}]
      (let [input (request :local-worker-workflow) seen (atom nil) calls (atom 0)
            forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Undeclared inputs scanned LIVE")))]
        (with-redefs [knowledge/selection forbidden knowledge/capture forbidden work/submit-prepared! (claim-only seen)]
          (is (:id (owned/submit-workflow! host (principal w) (str (:room w)) input)))
          (is (zero? @calls)))))))

(deftest frozen-declared-workflow-inputs-do-not-reread-live-knowledge
  (with-worker true
    (fn [{:keys [w host owned request]}]
      (let [files (assoc source "input-selection.edn"
                         "{:kind :knowledge :policy {:version 1 :trigger-roots [\"inbox\"] :context-roots [\"wiki\"] :exclude-prefixes []}}")]
        (seed! owned files)
        (repo/write! owned "inbox/frozen.md" (.getBytes "Frozen trigger" "UTF-8"))
        (let [captured (knowledge/capture @owned (knowledge/declaration files))
              input (assoc (request :local-worker-workflow) :expected-bundle (:id (catalog/bundle "echo" files)) :inputs (:inputs captured))
              seen (atom nil) calls (atom 0)
              forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Frozen inputs scanned LIVE")))]
          (repo/write! owned "inbox/frozen.md" (.getBytes "Later trigger" "UTF-8"))
          (with-redefs [knowledge/selection forbidden knowledge/capture forbidden knowledge/publication-basis forbidden
                        work/submit-prepared! (claim-only seen)]
            (is (:id (owned/submit-workflow! host (principal w) (str (:room w)) input)))
            (is (= (file-fixture/encoded "Frozen trigger")
                   (get-in @seen [:prepared :basis :frozen :files "in/knowledge/sources/inbox/frozen.md"])))
            (is (nil? (get-in @seen [:prepared :basis :knowledge/publication-basis])))
            (is (zero? @calls))))))))

(deftest equal-code-default-replacement-refuses-before-body-or-worker-acquisition
  (with-worker true
    (fn [{:keys [conn w host owned attached attached-descriptor request]}]
      (let [input (request :local-worker-workflow) calls (atom 0)
            forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Stale target performed fresh work")))]
        (seed! attached source)
        (is (= (:files (workflows/snapshot-in @owned "echo")) (:files (workflows/snapshot-in @attached "echo"))))
        (d/transact conn [{:room/id (:room w) :room/default-repository [:system/id (:registry-system attached-descriptor)]}])
        (with-redefs [workflows/snapshot-in forbidden local/enrollment! forbidden workers/payload forbidden work/submit-prepared! forbidden]
          (is (= ::owned/conflict (:type (ex-data (failure #(owned/submit-workflow! host (principal w) (str (:room w)) input))))))
          (is (zero? @calls)))))))

(deftest default-replacement-after-capture-refuses-before-enrollment-or-preparation
  (with-worker true
    (fn [{:keys [conn w host attached-descriptor request]}]
      (let [input (request :local-worker-workflow) snapshot workflows/snapshot-in calls (atom 0)
            forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Changed captured target acquired worker")))]
        (with-redefs [workflows/snapshot-in
                      (fn [& args]
                        (let [result (apply snapshot args)]
                          (d/transact conn [{:room/id (:room w) :room/default-repository [:system/id (:registry-system attached-descriptor)]}])
                          result))
                      local/enrollment! forbidden local/prepare-workflow-in forbidden work/submit-prepared! forbidden]
          (is (= ::owned/unavailable (:type (ex-data (failure #(owned/submit-workflow! host (principal w) (str (:room w)) input))))))
          (is (zero? @calls)))))))

(deftest benchmark-requires-operator-and-room-authority-not-worker-identity
  (with-worker true
    (fn [{:keys [conn w host request hydrations]}]
      (let [input (request :local-worker-benchmark)]
        (d/transact conn [[:db/retract [:party/id (:owner w)] :party/role :admin]])
        (reset! hydrations [])
        (doseq [p [(principal w) (ops/party-principal (:admin w)) (ops/party-principal (:member w))]]
          (is (= ::owned/unavailable (:type (ex-data (failure #(owned/submit-benchmark! host p (str (:room w)) input)))))))
        (is (empty? @hydrations))))))

(deftest helper-failure-data-cannot-forge-a-normal-conflict-and-interruption-is-preserved
  (with-worker true
    (fn [{:keys [w host request]}]
      (let [input (request :local-worker-workflow) calls (atom 0)]
        (doseq [retry [(constantly nil) (constantly {:status :retry :id (random-uuid)})
                       (fn [& _] (throw (ex-info "SECRET" {:type ::owned/conflict :status 409})))]]
          (with-redefs [local/retry-request retry reads/resolve-host-read! (fn [& _] (swap! calls inc))]
            (let [error (failure #(owned/submit-workflow! host (principal w) (str (:room w)) input))]
              (is (= ::owned/unavailable (:type (ex-data error))))
              (is (not (re-find #"SECRET" (pr-str error)))))))
        (is (zero? @calls))
        (try
          (with-redefs [local/retry-request (fn [& _] (throw (ex-info "SECRET" {} (InterruptedException. "cancel"))))]
            (let [error (failure #(owned/submit-workflow! host (principal w) (str (:room w)) input))
                  interrupted (.isInterrupted (Thread/currentThread))]
              (is interrupted) (is (= ::owned/unavailable (:type (ex-data error))))))
          (finally (Thread/interrupted)))))))

(deftest malformed-normal-admission-amount-cannot-escape-or-reach-kernel
  (with-worker true
    (fn [{:keys [w host request]}]
      (let [input (request :local-worker-benchmark) prepare local-benchmarks/prepare calls (atom 0)]
        (with-redefs [local-benchmarks/prepare (fn [& args] (assoc-in (apply prepare args) [:resources local/admission-unit] 4/3))
                      work/submit-prepared! (fn [& _] (swap! calls inc))]
          (is (= ::owned/unavailable (:type (ex-data (failure #(owned/submit-benchmark! host (principal w) (str (:room w)) input))))))
          (is (zero? @calls)))))))

(defn- lease! [conn enrolled id]
  (let [proof (workers/new-secret)
        worker (credentials/authenticate conn {:bearer (:credential enrolled) :headers {"simmis-lease-proof" proof}})
        claimed (service/claim! conn worker {:run (str id) :nonce (str (random-uuid))})]
    (credentials/authenticate conn {:bearer (credentials/lease-token id (:lease claimed) proof)})))

(deftest actual-kernel-worker-protocol-imports-owned-output-and-retires-each-physical-owner
  (doseq [kind [:local-worker-workflow :local-worker-benchmark] order [true false]]
    (with-worker order
      (fn [{:keys [conn w host room owned attached enrolled request]}]
        (let [input (request kind) p (principal w) before (state owned) other (state attached)
              selected (workspace/selected-system-id) job (atom nil) done (atom nil) start jobs/start!
              enqueue workers/enqueue! inputs (atom []) leases (atom {})
              clean (discourse/make-room {:id (keyword (str "owned-worker-checker-" (random-uuid))) :store (memory/make)})
              clean-conn (atom nil) configs (atom []) closed? (atom false) retired (atom 0)
              slot (java.util.concurrent.Semaphore. 1)
              retire! (fn [] (locking closed? (when-not @closed?
                                                (discourse/close-room! clean)
                                                (when @clean-conn (d/release @clean-conn))
                                                (reset! closed? true) (swap! retired inc))))]
          (binding [ec/*execution-context* (:ctx clean)]
            (catalog-workspace/ensure-workspace! clean)
            (reset! clean-conn (:conn (workspace/current-workspace)))
            (reset! configs [(:config @@clean-conn)]))
          (resource/install-unit! room (first local/units))
          (resource/mint! room {:id (random-uuid) :resources {local/admission-unit 2M}})
          (with-redefs-fn
            {#'benchmarks/checker-slot slot #'benchmarks/pending-retirements (atom {})
             #'native-fixture/validate-receipt! (fn [_ _ receipt] receipt)
             #'native-fixture/create! (fn [& _] {:room clean :persistent-id (random-uuid)})
             #'native-fixture/retire! (fn [& _] (retire!))
             #'jobs/start! (fn [actual-room opts work & callbacks]
                             (apply start actual-room opts
                                    (fn [id]
                                      (when (and (identical? room actual-room) (compare-and-set! job nil id))
                                        (reset! done (get-in @(var-get #'jobs/live) [id :done])))
                                      (work id)) callbacks))
             ;; Real durable enqueue, authenticated lease, bounded input and
             ;; completion/STOP APIs; no subprocess/provider or fake Run producer.
             #'workers/enqueue! (fn [c assignment]
                                  (let [result (enqueue c assignment)
                                        id (:id assignment) lease (lease! c enrolled id)]
                                    (swap! leases assoc id lease)
                                    (let [chunk (service/input c lease {:run (str id)})
                                          frozen (json/read-value (.decode (java.util.Base64/getDecoder) ^String (:base64 chunk)) json/keyword-keys-object-mapper)
                                          input-path (case kind :local-worker-workflow (keyword "workflows/echo/fixtures/docs/input.md")
                                                           :local-worker-benchmark :docs/input.md)
                                          descriptor (get-in frozen [:files input-path])]
                                      (swap! inputs conj frozen)
                                      (is (= (file-fixture/encoded "Owned document") descriptor))
                                     ;; Echo ACTUAL authenticated input bytes, not a fixed
                                     ;; output independent of which repository was selected.
                                      (service/complete! c lease {:run (str id) :output {:files {"out/result.md" descriptor}
                                                                                         :summary "in-process fixture" :exit-code 0}}))
                                    (service/stop! c lease {:run (str id)})
                                    result))}
            (fn []
              (try
                (let [submitted (submit! kind host p (str (:room w)) input)
                      status (jobs/status @job 25000)
                      attempts (store/-list-attempts (:store room) (store/conversation-id room) {})]
                  (is (= @job (:job submitted))) (is (some? @done))
                  (is (= :completed (:status status)) (pr-str (select-keys status [:status :error])))
                  (is (:settled? status))
                  (is (false? (:return-pending? (work/status conn (:execution-ctx host) p (:room w) (:id submitted)))))
                  (is (= 1 (count @inputs) (count attempts)))
                  (if (= kind :local-worker-benchmark)
                    (do (is (= "Owned document" (get-in (first attempts) [:attempt/evidence :files "/out/result.md"])))
                        (is (= (file-fixture/encoded "Owned document")
                               (get-in (first attempts) [:attempt/evidence :worker-files "/out/result.md"]))))
                    (is (= (file-fixture/encoded "Owned document")
                           (get-in (first attempts) [:attempt/evidence :files "/out/result.md"]))))
                  (is (false? (get-in (first attempts) [:attempt/receipt :attempt/metrics :spend :priced?])))
                  (is (= 1M (get (resource/balance room) local/admission-unit)))
                  (if (= kind :local-worker-benchmark)
                    (do (is (= #{:docs/input.md} (set (keys (:files (first @inputs))))))
                        (is (not (re-find #"PRIVATE-CHECKER-GOLD|OTHER document" (pr-str @inputs))))
                        (is (= 1 @retired)) (is (= 1 (.availablePermits slot))))
                    (is (contains? (:files (first @inputs)) (keyword "workflows/echo/helpers/code.clj"))))
                  (let [fresh (atom 0) forbidden (fn [& _] (swap! fresh inc) (throw (AssertionError. "Job retry reran worker")))]
                    (workers/revoke! conn (:worker enrolled))
                    (with-redefs [local/enrollment! forbidden reads/resolve-host-read! forbidden]
                      (is (= @job (:job (submit! kind host p (str (:room w)) input))))
                      (is (zero? @fresh))))
                  (is (= before (state owned))) (is (= other (state attached)))
                  (is (= selected (workspace/selected-system-id))))
                (finally
                  ;; Fixed transport roots remain held through actual native Job,
                  ;; evaluation groups, all acknowledged worker STOPs and fixture
                  ;; closure/native-conn release. No timeout substitutes for ACK.
                  (try
                    (doseq [[id lease] @leases] (service/stop! conn lease {:run (str id)}))
                    (finally
                      (try (when @job (@#'joined/join-fixture-job! room @job @done))
                           (finally (retire!)))))))))
          (doseq [cfg @configs]
            (assert (and (= :memory (get-in cfg [:store :backend])) (uuid? (get-in cfg [:store :id]))))
            (d/delete-database cfg)))))))
