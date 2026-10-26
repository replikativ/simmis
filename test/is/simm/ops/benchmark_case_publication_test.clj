(ns is.simm.ops.benchmark-case-publication-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.versioning :as versioning]
            [dvergr.artifact :as artifact]
            [geschichte.bytes :as bytes]
            [geschichte.repo :as repo]
            [is.simm.model.benchmark-resolutions :as journal]
            [is.simm.ops.benchmark-case-evidence :as evidence]
            [is.simm.ops.benchmark-case-manifest :as manifest]
            [is.simm.ops.benchmark-resolution :as resolution]
            [is.simm.ops.benchmark-resolution-test :as fixture]
            [is.simm.ops.benchmark-source :as source]
            [is.simm.ops.checkpoint-roots :as physical]
            [is.simm.ops.knowledge-changes :as changes]))

(defn- selection [db target]
  (let [basis (changes/basis db)]
    {:version 1 :policy :request-evidence-v1 :basis basis
     :origin {:version 1 :kind :datahike :commit (:commit basis)
              :source (select-keys basis [:store :repository])}
     :target (merge target (select-keys basis [:store :repository :backend :branch :ref]))}))

(defn- native-fixture [f]
  (let [config {:store {:backend :memory :id (random-uuid)}
                :schema-flexibility :write :keep-history? false :commit-graph? true}]
    (d/create-database config)
    (let [conn (d/connect config)]
      (try
        (repo/init! conn {:name "Private case fixture"})
        (repo/write! conn "AGENTS.md" (bytes/utf8 "Trusted instructions"))
        (repo/stage-all! conn) (repo/commit! conn {:message "C0"})
        (let [db @conn target {:registry-system (random-uuid) :runtime-system "private-fixture"}
              c0 {:selection (selection db target) :manifest (manifest/inventory db)}]
          (f conn db c0 target))
        (finally (d/release conn) (d/delete-database config))))))

(defn- build! [conn files extra? f]
  (let [branch (keyword (str "case-" (random-uuid)))]
    (versioning/branch! conn (d/commit-id @conn) branch)
    (let [child (d/connect (assoc (:config @conn) :branch branch))]
      (try
        (doseq [[path text] files] (repo/write! child (subs path 1) (bytes/utf8 text)))
        (when extra? (repo/write! child "hidden-other-case.md" (bytes/utf8 "Must not be visible")))
        (repo/stage-all! child) (repo/commit! child {:message "Selected case"})
        ;; Match the ACTUAL stored configuration, including unchanged-case
        ;; semantics; never synthesize a checkpoint from the live branch name.
        (let [db @child] (f (d/commit-as-db (:store db) (d/commit-id db))))
        (finally (d/release child) (versioning/delete-branch! conn branch))))))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo error (:reason (ex-data error)))))

(defn- with-case
  ([extra? f] (with-case extra? f nil))
  ([extra? f after-cleanup]
   (#'fixture/authorized-fixture
    (fn [control w request principal]
      (native-fixture
       (fn [conn db c0 target]
         (let [preparation (resolution/publish-preparation! control principal (:room w) request)
               retained (:source (resolution/reconstruct control request))
               files (source/case-files retained (get-in preparation [:plan :cases 0 :case]))]
           (let [built (build! conn files extra?
                               (fn [case-db]
                                 (let [context {:control control :w w :request request :principal principal
                                                :preparation preparation :db db :c0 c0 :fixture-conn conn
                                                :case-db case-db :selected (selection case-db target)}]
                                   {:context context :result (f context)})))]
            ;; build! has physically closed its child connection and removed
            ;; its branch before any READY publication in after-cleanup.
             (if after-cleanup (after-cleanup (:context built) (:result built)) (:result built))))))))))

(defn- publish [{:keys [control w request principal preparation c0 selected]}]
  (resolution/publish-case! control principal (:room w) request preparation 0 c0 selected))

(defn- acknowledge [{:keys [control w request principal preparation c0 db]}]
  (resolution/acknowledge-case! control principal (:room w) request preparation 0 c0 db))

(deftest case-is-retained-only-after-exact-physical-pin-and-whole-manifest-readback
  (with-case false
    (fn [{:keys [control preparation case-db selected] :as context}]
      (let [published (publish context) row (:case published)]
        (is (= :pinning (:benchmark.case/state row)))
        (is (= :pin-unavailable (reason #(acknowledge context))))
        (physical/create! case-db (:basis selected) (:benchmark.case/root row)
                          (evidence/root-owner (:body published)))
        (let [retained (acknowledge context)]
          (is (= :retained (get-in retained [:case :benchmark.case/state])))
          (is (= (dissoc published :case) (dissoc retained :case)))
          (is (= retained (acknowledge context)))
          (is (= :preparing (:benchmark.resolution/state
                             (journal/resolution @control (get-in preparation [:resolution :benchmark.resolution/request])))))
          (is (= :not-creation-winner (reason #(publish context)))))))))

(deftest valid-root-cannot-hide-an-unselected-file
  (with-case true
    (fn [{:keys [case-db selected control preparation] :as context}]
      (let [published (publish context) row (:case published)]
        (physical/create! case-db (:basis selected) (:benchmark.case/root row)
                          (evidence/root-owner (:body published)))
        (is (= :case-manifest-changed (reason #(acknowledge context))))
        (is (= :pinning (:benchmark.case/state (first (journal/cases @control
                                                                     (get-in preparation [:resolution :benchmark.resolution/id]))))))))))

(deftest denied-case-reader-cannot-load-retained-artifacts
  (with-case false
    (fn [{:keys [control w request preparation c0] :as context}]
      (publish context)
      (with-redefs [artifact/get-bounded-value (fn [& _] (throw (AssertionError. "Denied artifact read")))]
        (is (thrown? clojure.lang.ExceptionInfo
                     (resolution/read-case! control {:principal/id nil} (:room w) request preparation 0 c0)))))))

(deftest claimed-template-must-match-the-original-physical-snapshot
  (with-case false
    (fn [{:keys [case-db selected] :as context}]
      (let [context (update-in context [:c0 :manifest :files] dissoc "/AGENTS.md")
            published (publish context) row (:case published)]
        (physical/create! case-db (:basis selected) (:benchmark.case/root row)
                          (evidence/root-owner (:body published)))
        (is (= :template-manifest-changed (reason #(acknowledge context))))))))

(deftest revocation-before-publication-writer-keeps-the-case-planned
  (with-case false
    (fn [{:keys [control w preparation] :as context}]
      (let [publish! artifact/publish-value!]
        (with-redefs [artifact/publish-value!
                      (fn [store value commit!]
                        (publish! store value
                                  (fn [ref]
                                    (#'fixture/revoke! control w)
                                    (commit! ref))))]
          (is (thrown? clojure.lang.ExceptionInfo (publish context)))))
      (is (= :planned (:benchmark.case/state
                       (first (journal/cases @control (get-in preparation [:resolution :benchmark.resolution/id])))))))))

(deftest revocation-during-case-readback-prevents-returning-evidence
  (with-case false
    (fn [{:keys [control w request principal preparation c0] :as context}]
      (let [published (publish context) get! artifact/get-bounded-value]
        (with-redefs [artifact/get-bounded-value
                      (fn [store ref maximum]
                        (let [value (get! store ref maximum)]
                          (when (= ref (:ref published)) (#'fixture/revoke! control w))
                          value))]
          (is (thrown? clojure.lang.ExceptionInfo
                       (resolution/read-case! control principal (:room w) request preparation 0 c0))))))))

(deftest final-physical-readback-is-followed-by-a-current-authority-and-inventory-fence
  (doseq [change [:revoke :case]]
    (with-case false
      (fn [{:keys [control w case-db selected] :as context}]
        (let [published (publish context) row (:case published)
              verify! physical/verify! reads (atom 0)]
          (physical/create! case-db (:basis selected) (:benchmark.case/root row)
                            (evidence/root-owner (:body published)))
          (let [failure (with-redefs [physical/verify!
                                      (fn [& args]
                                        (let [verified (apply verify! args)]
                                          (when (= 2 (swap! reads inc))
                                            (if (= :revoke change)
                                              (#'fixture/revoke! control w)
                                              (d/transact control [[:db/add (:db/id row)
                                                                    :benchmark.case/claim (random-uuid)]])))
                                          verified))]
                          (try (acknowledge context) nil
                               (catch clojure.lang.ExceptionInfo error error)))]
            (is (= 2 @reads))
            (is (some? failure))
            (when (= :case change) (is (= :inventory-changed (:reason (ex-data failure)))))))))))

(deftest committed-publication-and-retention-with-lost-acks-are-inspected-not-repeated
  (with-case false
    (fn [{:keys [case-db selected] :as context}]
      (let [publish! artifact/publish-value! calls (atom 0)
            published (with-redefs [artifact/publish-value!
                                    (fn [store value commit!]
                                      (swap! calls inc)
                                      (publish! store value commit!)
                                      (throw (ex-info "publication ACK lost" {})))]
                        (publish context))
            row (:case published)]
        (is (= 1 @calls))
        (physical/create! case-db (:basis selected) (:benchmark.case/root row)
                          (evidence/root-owner (:body published)))
        (let [transact! d/transact writes (atom 0)
              retained (with-redefs [d/transact
                                     (fn [& args]
                                       (swap! writes inc)
                                       (apply transact! args)
                                       (throw (ex-info "retained ACK lost" {})))]
                         (acknowledge context))]
          (is (= 1 @writes))
          (is (= :retained (get-in retained [:case :benchmark.case/state]))))))))

(defn- retain! [{:keys [case-db selected] :as context}]
  (let [published (publish context) row (:case published)]
    (physical/create! case-db (:basis selected) (:benchmark.case/root row)
                      (evidence/root-owner (:body published)))
    (acknowledge context)))

(defn- ready! [{:keys [control w request principal preparation c0 db]}]
  (resolution/publish-ready! control principal (:room w) request preparation c0 db))

(deftest ready-binds-the-canonical-experiment-and-retained-evidence-after-builder-cleanup
  (with-case false retain!
    (fn [{:keys [control request preparation fixture-conn] :as context} retained]
      (is (= #{(get-in @fixture-conn [:config :branch])} (d/branches fixture-conn)))
      (let [ready (ready! context)
            row (journal/resolution @control (:work.request/id request))
            body (:body ready)
            original (get-in preparation [:prepared :plan :experiment])
            actual (get-in ready [:plan :experiment])]
        (is (= :ready (:benchmark.resolution/state row)))
        (is (= (:id ready) (:benchmark.resolution/resolved-id row)))
        (is (= (:ref ready) (:benchmark.resolution/resolved row)))
        (is (= actual (:experiment body)))
        (is (= (select-keys original [:experiment/id :experiment/version :experiment/repetitions :experiment/candidates])
               (select-keys actual [:experiment/id :experiment/version :experiment/repetitions :experiment/candidates])))
        (is (= (:id retained) (get-in body [:cases 0 :evidence-id])))
        (is (= (:ref retained) (get-in body [:cases 0 :evidence-ref])))
        (is (= (manifest/visible-id (get-in retained [:body :manifest]))
               (get-in body [:cases 0 :visible-manifest-id])))
        (is (not= (get-in retained [:body :case :source-manifest-id])
                  (get-in body [:cases 0 :visible-manifest-id])))
        (is (= (get-in body [:cases 0 :setup])
               (get-in actual [:experiment/dataset :dataset/environments 0 :environment/world :setup])))
        (is (some? (get-in ready [:plan :capabilities :world-setups (get-in body [:cases 0 :setup])]))))
      (with-redefs [artifact/publish-value! (fn [& _] (throw (AssertionError. "READY retry must not republish")))]
        (is (= :case-family-changed (reason #(ready! context))))))))

(deftest ready-refuses-unretained-cases
  (with-case false
    (fn [context]
      (is (= :cases-not-retained (reason #(ready! context)))))))

(deftest ready-regeneration-checks-full-program-and-record-correspondence
  (with-case false retain!
    (fn [{:keys [control request preparation c0]} retained]
      (let [compiled (resolution/reconstruct control request)
            build #(evidence/resolve-execution request preparation %1 c0 %2)
            forged-body (update-in (:body retained) [:manifest :files] dissoc "/AGENTS.md")
            forged-id (evidence/content-id forged-body)
            forged (-> retained (assoc :body forged-body :id forged-id)
                       (assoc-in [:case :benchmark.case/selection-id] forged-id))]
        (is (= :execution-binding
               (reason #(build (assoc-in compiled [:prepared :plan :experiment :experiment/repetitions] 2)
                               [retained]))))
        (is (= :execution-binding (reason #(build compiled [retained retained]))))
        (is (= :retained-case-binding
               (reason #(build compiled [(assoc retained :ref (random-uuid))]))))
        (is (= :retained-case-binding
               (reason #(build compiled [(assoc-in retained [:case :benchmark.case/state] :pinning)]))))
        (is (= :case-integrity (reason #(build compiled [forged]))))))))

(deftest ready-lost-ack-is-inspected-without-republication
  (with-case false retain!
    (fn [context _]
      (let [publish! artifact/publish-value! calls (atom 0)
            ready (with-redefs [artifact/publish-value!
                                (fn [store value commit!]
                                  (swap! calls inc) (publish! store value commit!)
                                  (throw (ex-info "READY ACK lost" {})))]
                    (ready! context))]
        (is (= 1 @calls))
        (is (= (:id ready) (evidence/execution-id (:body ready))))))))

(deftest ready-final-artifact-read-is-followed-by-authorization
  (with-case false retain!
    (fn [{:keys [control w] :as context} _]
      (let [get! artifact/get-bounded-value]
        (with-redefs [artifact/get-bounded-value
                      (fn [store ref maximum]
                        (let [result (get! store ref maximum)]
                          (when (contains? (:value result) :experiment) (#'fixture/revoke! control w))
                          result))]
          (is (thrown? clojure.lang.ExceptionInfo (ready! context))))))))
