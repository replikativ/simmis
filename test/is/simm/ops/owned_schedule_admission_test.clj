(ns is.simm.ops.owned-schedule-admission-test
  "Activation-only control/storage proof. No Job, dispatcher owner, clock,
   occurrence execution, provider call or transport activation is installed."
  (:require [is.simm.support.control :as control]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [dvergr.catalog.room :as catalog]
            [dvergr.model.registry :as models]
            [dvergr.resource :as resource]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.model.work-requests :as requests]
            [is.simm.model.workflow-schedules :as model]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.knowledge-inputs :as knowledge]
            [is.simm.ops.owned-schedule-fixture :as fixture]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.workflow-runs :as workflow-runs]
            [is.simm.ops.workflow-schedules :as schedules]
            [is.simm.ops.workflows :as workflows]
            [malli.core :as m]))

(def ^:private source
  {"workflow.edn" "{:title \"Owned scheduled workflow\" :task \"Read /docs/input.md and write /out/result.md\" :capture [\"/out\"]}"
   "checker.clj" "(ns checker) (defn check [_] {:checks {:ok? true} :reward 1.0})"
   "gold.edn" "{:expected \"Owned evidence\"}"
   "fixtures/docs/input.md" "Owned evidence"
   "helpers/code.clj" "(def owned true)"})
(def ^:private limits {:budget-microdollars 10000 :max-dispatches 4 :max-model-steps 3
                       :timeout-ms 30000 :settlement "discard"})
(defn- principal [w] (ops/party-principal (:owner w)))
(defn- failure [f] (try (f) nil (catch clojure.lang.ExceptionInfo error error)))
(defn- seed! [conn files]
  (doseq [[path text] files] (repo/write! conn (str "workflows/echo/" path) (.getBytes ^String text "UTF-8"))))
(defn- state [conn]
  (let [db @conn] {:commit (d/commit-id db) :config (:config db) :tree (query/worktree db)}))
(defn- input [kind host w files]
  {:request-id (random-uuid) :kind (name kind) :cadence {:interval-ms 60000 :tz "UTC"}
   :execution (cond-> {:name "echo" :expected-bundle (:id (catalog/bundle "echo" files))
                       :expected-owned-source (parse-uuid (get-in (sources/details! host (principal w) (str (:room w)) {:name "echo"}) [:data :revision]))
                       :limits limits}
                (= :workflow kind) (assoc :model "codex-subscription-sol")
                (= :benchmark kind) (assoc :models ["codex-subscription-sol"] :repetitions 1))})
(defn- control-input [kind]
  {:request-id (random-uuid) :kind (name kind) :cadence {:interval-ms 60000 :tz "UTC"}
   :execution (cond-> {:name "echo" :expected-bundle (:id (catalog/bundle "echo" source))
                       :expected-owned-source (random-uuid) :limits limits}
                (= kind :workflow) (assoc :model "codex-subscription-sol")
                (= kind :benchmark) (assoc :models ["codex-subscription-sol"] :repetitions 1))})

(defn- with-fixture [order f]
  (@#'fixture/with-fixture
   order (fn [{:keys [conn w owned attached host] :as fixture}]
           (d/transact conn model/schema)
           (d/transact conn [{:party/id (:owner w) :party/role :admin}])
           (seed! owned source)
           (repo/write! owned "workflows/echo/helpers/code.clj"
                        (.getBytes ^String (get source "helpers/code.clj") "UTF-8") {:mode 33261})
           (seed! attached (assoc source "helpers/code.clj" "OTHER helper"))
           (f (assoc fixture :input #(input % host w source))))))

;; Both registration orders are exercised by the shared owned-workspace,
;; file-projection and transport contracts. Each kind still runs here with the
;; foreign attachment registered first and selected throughout.
(deftest fixture-and-local-dtos-have-the-actual-required-definition-and-closed-shapes
  (is (m/validate catalog/Definition (edn/read-string (get source "workflow.edn"))))
  (is (empty? (catalog/check-files source)))
  (let [base {:name "echo" :expected-bundle (random-uuid) :expected-owned-source (random-uuid) :limits limits}]
    (is (m/validate (var-get #'schedules/owned-workflow-schema) (assoc base :model "codex-subscription-sol")))
    (is (m/validate (var-get #'schedules/owned-benchmark-schema) (assoc base :models ["codex-subscription-sol"] :repetitions 1)))
    (is (not (m/validate (var-get #'schedules/owned-workflow-schema) (assoc base :model "codex-subscription-sol" :request-id (random-uuid))))))
  (is (= (* 32 1024 1024) model/max-managed-target-bytes)))

(deftest both-fixed-kinds-retain-one-owned-source-and-no-execution-with-adverse-attachment-order
  (doseq [kind [:workflow :benchmark] :let [order false]]
    (with-fixture order
      (fn [{:keys [conn w host owned attached input]}]
        (let [request (input kind) before (state owned) other (state attached) selected (workspace/selected-system-id)
              calls (atom 0) forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Activation executed work")))]
          (with-redefs [knowledge/selection forbidden knowledge/capture forbidden resource/balance forbidden
                        workflow-runs/prepare forbidden]
            (let [row (schedules/create-owned! host (principal w) (str (:room w)) request)
                  target (model/managed-target conn row)]
              (is (= 2 (:version target)))
              (is (= kind (:kind target)))
              (is (= (:room w) (:room target) (:schedule.activation/room row)))
              (is (= (:owner w) (:by target) (:schedule.activation/by row)))
              (is (= (:execution request) (:execution target)))
              (is (= source (get-in target [:snapshot :files])))
              (is (= (:expected-owned-source (:execution request)) (get-in target [:source-origin :revision])))
              (is (uuid? (get-in target [:source-origin :target-stamp])))
              (is (= 33261 (:mode (first (filter #(= "helpers/code.clj" (:path %)) (get-in target [:snapshot :manifest]))))))
              (is (= :host (:creation-channel target)))
              (is (= (hasch/uuid [:simmis/schedule-activation-v1 (:owner w) (:room w) (:request-id request)]) (:schedule.activation/id row)))
              (is (empty? (model/occurrences conn (:schedule.activation/id row))))
              (is (empty? (d/datoms @conn :aevt :work.request/id)))
              (is (= 1 (count (model/activations conn))))
              (is (zero? @calls))))
          (is (= before (state owned))) (is (= other (state attached)))
          (is (= selected (workspace/selected-system-id))))))))

(deftest exact-activation-retry-precedes-hydration-source-model-policy-and-funds
  (with-fixture true
    (fn [{:keys [conn w host owned input]}]
      (let [request (input :workflow) p (principal w)
            row (schedules/create-owned! host p (str (:room w)) request)]
        (repo/remove! owned "workflows/echo/workflow.edn")
        (d/transact conn [[:db/retract [:room/id (:room w)] :room/default-repository [:system/id (:repo-sys w)]]])
        (let [calls (atom 0) forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Exact retry acquired fresh work")))]
          (with-redefs [reads/resolve-host-read! forbidden workflows/manifest-in forbidden workflows/snapshot-in forbidden
                        models/get-model forbidden schedules/model-policy forbidden schedules/host-policy forbidden
                        benchmarks/prepare forbidden resource/balance forbidden]
            (is (= row (schedules/create-owned! host p (str (:room w)) request)))
            (is (= ::schedules/conflict (:type (ex-data (failure #(schedules/create-owned! host p (str (:room w))
                                                                                           (assoc-in request [:execution :expected-owned-source] (random-uuid))))))))
            (is (zero? @calls))))))))

(deftest expected-source-guard-and-malformed-normal-identity-are-prebody
  (with-fixture true
    (fn [{:keys [w host input]}]
      (let [request (input :workflow) p (principal w) calls (atom 0)
            forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Stale metadata decoded source")))]
        (with-redefs [workflows/snapshot-in forbidden models/get-model forbidden benchmarks/prepare forbidden model/create! forbidden]
          (is (= ::schedules/conflict (:type (ex-data (failure #(schedules/create-owned! host p (str (:room w))
                                                                                         (assoc-in request [:execution :expected-owned-source] (random-uuid))))))))
          (doseq [bad [nil "not-a-UUID"]]
            (with-redefs [sources/source-id (constantly bad)]
              (is (= ::schedules/owned-unavailable (:type (ex-data (failure #(schedules/create-owned! host p (str (:room w)) request))))))))
          (is (zero? @calls)))))))

(deftest malformed-normal-activation-lookup-is-not-fresh-absence
  (control/with-control
   model/schema
   (fn [{:keys [w host handle]}]
     (let [request (control-input :workflow) calls (atom 0) lookups (atom 0)
           forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Malformed lookup reached fresh work")))]
       (doseq [row [false [] "not-a-row" {} {:schedule.activation/id (random-uuid)}]]
         (reset! lookups 0)
         (with-redefs [model/activation (fn [& _] (swap! lookups inc) row) reads/resolve-host-read! forbidden
                       workflows/manifest-in forbidden workflows/snapshot-in forbidden model/create! forbidden]
           (is (= ::schedules/owned-unavailable
                  (:type (ex-data (failure #(schedules/create-owned! host (principal w) handle request))))))
           (is (= 1 @lookups))))
       (is (zero? @calls))))))

(deftest invalid-human-dto-and-operator-only-never-hydrate
  (control/with-control
   model/schema
   (fn [{:keys [conn w host handle]}]
     (let [request (control-input :workflow) p (principal w) calls (atom 0)
           forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Denied activation acquired source")))]
       (d/transact conn [{:party/id (:owner w) :party/role :admin}])
       (with-redefs [reads/resolve-host-read! forbidden workflows/manifest-in forbidden model/create! forbidden]
         (doseq [bad [(update request :execution dissoc :expected-owned-source)
                      (assoc-in request [:execution :request-id] (random-uuid))
                      (assoc-in request [:execution :name] "echo\n") (assoc request :owner (:owner w))]]
           (is (= ::schedules/invalid (:type (ex-data (failure #(schedules/create-owned! host p handle bad)))))))
         (is (= ::schedules/owned-unavailable
                (:type (ex-data (failure #(schedules/create-owned! host (assoc p :principal/kind :worker) handle request))))))
         (let [benchmark (control-input :benchmark)]
           (d/transact conn [[:db/retract [:party/id (:owner w)] :party/role :admin]])
           (doseq [actor [(:owner w) (:admin w)]]
             (is (= ::schedules/owned-unavailable
                    (:type (ex-data (failure #(schedules/create-owned! host (ops/party-principal actor) handle benchmark)))))))
           (is (zero? @calls))))))))

(deftest current-default-and-action-fences-precede-preparation-and-publication
  (doseq [change [:default :merge] phase [:snapshot :policy]]
    (with-fixture true
      (fn [{:keys [conn w host input attached-descriptor]}]
        (let [request (input :workflow) snapshot workflows/snapshot-in policy schedules/model-policy calls (atom 0)
              change! (fn [] (case change
                               :default (d/transact conn [{:room/id (:room w) :room/default-repository [:system/id (:registry-system attached-descriptor)]}])
                               :merge (d/transact conn [{:room/id (:room w) :room/owner [:party/id (:member w)]
                                                         :room/parties [[:party/id (:owner w)]]}])))]
          (with-redefs [workflows/snapshot-in
                        (fn [& args]
                          (let [result (apply snapshot args)]
                            (when (= :snapshot phase) (change!))
                            result))
                        schedules/model-policy (fn [& args] (let [result (apply policy args)]
                                                              (when (= :policy phase) (change!))
                                                              result))
                        model/create! (fn [& _] (swap! calls inc))]
            (is (= ::schedules/owned-unavailable (:type (ex-data (failure #(schedules/create-owned! host (principal w) (str (:room w)) request))))))
            (is (zero? @calls))))))))

(deftest helper-tags-and-causal-interruption-never-become-a-normal-conflict
  (with-fixture true
    (fn [{:keys [w host input]}]
      (let [request (input :workflow) p (principal w)]
        (with-redefs [sources/source-id (fn [& _] (throw (ex-info "SECRET" {:type ::schedules/conflict :status 409})))]
          (let [error (failure #(schedules/create-owned! host p (str (:room w)) request))]
            (is (= ::schedules/owned-unavailable (:type (ex-data error))))
            (is (not (re-find #"SECRET" (pr-str error))))))
        (schedules/create-owned! host p (str (:room w)) request)
        (try
          (with-redefs [model/managed-target (fn [& _] (throw (ex-info "SECRET" {} (InterruptedException. "cancel"))))]
            (let [error (failure #(schedules/create-owned! host p (str (:room w)) request))
                  interrupted (.isInterrupted (Thread/currentThread))]
              (is interrupted) (is (= ::schedules/owned-unavailable (:type (ex-data error))))))
          (finally (Thread/interrupted)))))))

(deftest retained-validator-binds-room-kind-guard-manifest-and-actual-bytes
  (with-fixture true
    (fn [{:keys [conn w host input]}]
      (let [row (schedules/create-owned! host (principal w) (str (:room w)) (input :workflow))
            target (model/managed-target conn row)]
        (doseq [bad [(assoc target :room (random-uuid))
                     (assoc target :by (random-uuid))
                     (assoc target :kind :benchmark)
                     (assoc-in target [:execution :expected-owned-source] (random-uuid))
                     (assoc-in target [:source-origin :revision] (random-uuid))
                     (assoc-in target [:snapshot :files "helpers/code.clj"] "forged bytes")
                     (assoc-in target [:snapshot :manifest 0 :mode] 0)
                     (assoc target :authority (fn [] true))]]
          (is (= ::schedules/owned-unavailable
                 (:type (ex-data (failure #(@#'schedules/owned-storage! (fn [] (@#'schedules/owned-target! row bad)))))))))))))

(deftest malformed-path-encoding-cannot-change-an-otherwise-consistent-retained-identity
  (with-fixture true
    (fn [{:keys [conn w host input]}]
      (let [row (schedules/create-owned! host (principal w) (str (:room w)) (input :workflow))
            target (model/managed-target conn row)
            original "helpers/code.clj" malformed (str "helpers/" (char 0xD800) ".clj")
            files (-> (get-in target [:snapshot :files]) (dissoc original)
                      (assoc malformed (get-in target [:snapshot :files original])))
            manifest (->> (get-in target [:snapshot :manifest])
                          (mapv #(if (= original (:path %)) (assoc % :path malformed) %))
                          (sort-by :path) vec)
            revision (sources/source-id {:room-id (:room w) :target-stamp (get-in target [:source-origin :target-stamp])} "echo" manifest)
            bad (-> target (assoc :snapshot {:files files :manifest manifest})
                    (assoc-in [:source-origin :manifest] manifest)
                    (assoc-in [:source-origin :revision] revision)
                    (assoc-in [:execution :expected-owned-source] revision)
                    (assoc-in [:execution :expected-bundle] (:id (catalog/bundle "echo" files))))]
        ;; All independent guard/manifest/bundle identities have been updated;
        ;; failure must still precede treating replacement-encoded keys as data.
        (is (m/validate (var-get #'schedules/owned-target-schema) bad))
        (is (= ::schedules/owned-unavailable
               (:type (ex-data (failure #(@#'schedules/owned-storage! (fn [] (@#'schedules/owned-target! row bad))))))))))))

(deftest bounded-managed-reader-rejects-actual-oversize-before-edn-and-preserves-v1
  (with-fixture true
    (fn [{:keys [conn w]}]
      (let [target {:version 1 :payload (apply str (repeat 512 "x"))}
            row (model/create! conn {:id (random-uuid) :room (:room w) :by (:owner w) :target target
                                     :now (java.util.Date.) :next-fire (java.util.Date.)})
            reads (atom 0)]
        (is (= target (model/target conn row) (model/managed-target conn row)))
        (is (> (:size (artifact/prepare-value target)) 128))
        (with-redefs [model/max-managed-target-bytes 128
                      edn/read (fn [& _] (swap! reads inc) (throw (AssertionError. "Oversize was decoded")))]
          (is (some? (failure #(model/managed-target conn row))))
          (is (zero? @reads)))))))

(deftest actual-encoded-creation-cap-is-enforced-before-the-publisher
  (with-fixture true
    (fn [{:keys [w host input]}]
      (let [request (input :workflow) prepare artifact/prepare-value limits (atom []) publications (atom 0)]
        ;; This is a small actual-artifact boundary test, not a32MiB stress test.
        ;; The production limit is asserted separately; the encoder is REAL.
        (with-redefs [model/max-managed-target-bytes 128
                      artifact/prepare-value (fn [value maximum] (swap! limits conj maximum) (prepare value maximum))
                      model/create! (fn [& _] (swap! publications inc))]
          (is (= ::schedules/owned-unavailable (:type (ex-data (failure #(schedules/create-owned! host (principal w) (str (:room w)) request))))))
          (is (= [128] @limits))
          (is (zero? @publications)))))))

(deftest legacy-tick-refuses-v2-before-even-active-completion-room-or-policy-lookup
  (with-fixture true
    (fn [{:keys [conn w host room input]}]
      (let [row (schedules/create-owned! host (principal w) (str (:room w)) (input :workflow))
            id (:schedule.activation/id row) target (model/managed-target conn row)
            _ (model/claim-due! conn row (:cadence target) (:schedule.activation/next-fire row))
            oid (:schedule.activation/active-occurrence (model/activation conn id))
            calls (atom 0) forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Legacy touched v2 execution")))]
        (is (uuid? oid))
        ;; Synthetic settled control request makes release-settled!'s hydration
        ;; path live if the version guard is accidentally moved below it.
        (let [rid (requests/request-id (:owner w) (:room w) oid)]
          (d/transact conn [{:work.request/id rid :work.request/return-pending? false}])
          (model/admit! conn oid)
          (model/finish-admission! conn oid :linked rid nil))
        (with-redefs [models/get-model forbidden schedules/model-policy forbidden reads/resolve-host-read! forbidden]
          (schedules/tick! conn (:ctx room) forbidden (:schedule.activation/next-fire row)))
        (let [after (model/activation conn id)]
          (is (= ::schedules/owned-dispatch-unavailable (:schedule.activation/reason after)))
          (is (= oid (:schedule.activation/active-occurrence after)))
          (is (= :linked (:schedule.occurrence/state (model/occurrence conn oid))))
          (is (zero? @calls)))))))

(deftest legacy-tick-refuses-unknown-target-version-before-completion-or-room-lookup
  (with-fixture true
    (fn [{:keys [conn w room]}]
      (let [row (model/create! conn {:id (random-uuid) :room (:room w) :by (:owner w)
                                     :target {:version 3} :now (java.util.Date.) :next-fire (java.util.Date.)})
            calls (atom 0) forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Unknown version entered legacy work")))]
        (with-redefs-fn {#'schedules/release-settled! forbidden #'models/get-model forbidden
                         #'workflow-runs/prepare forbidden #'benchmarks/prepare forbidden}
          #(schedules/tick! conn (:ctx room) forbidden (java.util.Date.)))
        (is (= ::schedules/target-version-unavailable
               (:schedule.activation/reason (model/activation conn (:schedule.activation/id row)))))
        (is (zero? @calls))))))

(deftest serialized-interleaved-source-generations-have-one-canonical-winner
  (with-fixture true
    (fn [{:keys [conn w host owned]}]
      (let [a (input :workflow host w source) p (principal w) create model/create! once? (atom true) winner (atom nil)
            changed (assoc source "helpers/code.clj" "(def newer true)")]
        (with-redefs [model/create!
                      (fn [& args]
                        (when (compare-and-set! once? true false)
                          (seed! owned changed)
                          (let [b (assoc (input :workflow host w changed) :request-id (:request-id a))]
                            (reset! winner {:input b :row (schedules/create-owned! host p (str (:room w)) b)})))
                        (apply create args))]
          (is (= ::schedules/owned-unavailable (:type (ex-data (failure #(schedules/create-owned! host p (str (:room w)) a)))))))
        (let [{:keys [row input]} @winner target (model/managed-target conn row)]
          (is (= 1 (count (model/activations conn))))
          (is (= changed (:files (:snapshot target))))
          (is (= (:execution input) (:execution target)))
          (is (= row (schedules/create-owned! host p (str (:room w)) input)))
          (is (= ::schedules/conflict (:type (ex-data (failure #(schedules/create-owned! host p (str (:room w)) a)))))))))))

(deftest paused-owned-creation-retains-source-without-queueing-and-retries-observe-current-state
  (doseq [[kind order] [[:workflow true] [:benchmark false]]]
    (with-fixture order
      (fn [{:keys [conn w host input]}]
        (let [request (assoc (input kind) :enabled? false) p (principal w)
              row (schedules/create-owned! host p (str (:room w)) request)
              id (:schedule.activation/id row) target (model/managed-target conn row)]
          (is (false? (:schedule.activation/enabled? row)))
          (is (false? (model/initial-enabled? row)))
          (is (not (contains? target :enabled?)) "creation state does not change the retained execution target")
          (model/claim-due! conn row (:cadence target) (java.util.Date. (+ 86400000 (.getTime ^java.util.Date (:schedule.activation/next-fire row)))))
          (is (empty? (model/occurrences conn id)))
          (is (empty? (requests/requests conn)))
          (model/set-enabled! conn id true)
          (let [forbidden (fn [& _] (throw (AssertionError. "Exact paused-creation retry acquired fresh work")))]
            (with-redefs [reads/resolve-host-read! forbidden workflows/manifest-in forbidden workflows/snapshot-in forbidden
                          models/get-model forbidden schedules/model-policy forbidden schedules/host-policy forbidden
                          benchmarks/prepare forbidden resource/balance forbidden]
              (is (true? (:schedule.activation/enabled? (schedules/create-owned! host p (str (:room w)) request))))
              (is (= ::schedules/conflict
                     (:type (ex-data (failure #(schedules/create-owned! host p (str (:room w)) (assoc request :enabled? true)))))))))
          (model/set-enabled! conn id false)
          (is (false? (:schedule.activation/enabled? (schedules/create-owned! host p (str (:room w)) request)))))))))
