(ns is.simm.ops.owned-workflows-test
  "Inactive service over real canonical authority and two native repositories.
   No transport/startup/authoring or execution certification claim."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.catalog.room :as catalog]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.content :as content]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.agents.room-agents :as rooms]
            [is.simm.ops.benchmark-dataset :as dataset]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflows :as owned]
            [is.simm.ops.workflows :as workflows]
            [is.simm.ops.workflows-test :as workflow-fixture]))

(defn- seed! [conn name files]
  (doseq [[path text] files]
    (repo/write! conn (str "workflows/" name "/" path) (.getBytes ^String text "UTF-8"))))

(defn- failure [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

(defn- durable [conn]
  (let [db @conn] {:commit (d/commit-id db) :config (:config db) :tree (query/worktree db)}))

(deftest one-owned-capture-serves-normal-humans-without-selected-rereads
  (doseq [first? [true false]]
    (@#'fixture/with-fixture
     first?
     (fn [{:keys [conn w host owned attached hydrations]}]
       (let [files (assoc workflow-fixture/fixture "large.md" (apply str (repeat 2000 "🦋世")))
             p (ops/party-principal (:member w)) handle (str (:room w))
             slug (:room/slug (d/entity @conn [:room/id (:room w)]))]
         (seed! owned "echo" files)
         (seed! attached "echo" (assoc files "gold.edn" "{:expected :OTHER}"))
         (let [other (durable attached) selection (workspace/selected-system-id)
               original reads/resolve-host-read! captures (atom [])
               binding (reads/resolve-host-read! host p handle)
               base (workflows/details-in (:db binding) "echo")
               forbidden (fn [& _] (throw (AssertionError. "No selected/callback source acquisition")))]
           (reset! hydrations [])
           (with-redefs [reads/resolve-host-read! (fn [h principal original-handle]
                                                    (is (identical? host h))
                                                    (let [result (original h principal original-handle)]
                                                      (swap! captures conj [original-handle result]) result))
                         workspace/current-workspace forbidden workflows/details forbidden
                         workflows/source-page forbidden workflows/list-page forbidden]
             (doseq [original-handle [handle slug]]
               (let [full (owned/details! host p original-handle {:name "echo"})
                     page (owned/source! host p original-handle {:name "echo"})
                     listed (owned/list! host p original-handle {})
                     revision (get-in full [:data :revision])
                     chunk (owned/source! host p original-handle {:name "echo" :path "large.md" :offset 0
                                                                  :expected-revision (str/upper-case revision)})]
                 (is (= files (get-in full [:data :files])))
                 (is (= revision (get-in page [:data :revision]) (get-in chunk [:data :revision])))
                 (is (not= (:revision base) revision))
                 (is (= (get-in base [:inspection :bundle]) (get-in full [:data :inspection :bundle])))
                 (is (= ["echo"] (get-in listed [:data :names])))
                 (is (<= (alength (.decode (java.util.Base64/getDecoder) ^String (get-in chunk [:data :base64]))) 8192))
                 (is (= #{:data :read-context} (set (keys full))))
                 (is (= #{:control-db :party :room-id} (set (keys (:read-context full)))))
                 (is (= (:member w) (get-in full [:read-context :party])))
                 (is (= (:room w) (get-in full [:read-context :room-id])))
                 (is (identical? (:control-db (second (nth @captures (- (count @captures) 4))))
                                 (get-in full [:read-context :control-db]))))))
           (is (= 8 (count @captures) (count @hydrations)))
           (is (= selection (workspace/selected-system-id)))
           (is (= other (durable attached)))))))))

(deftest revision-preconditions-precede-bodies-and-stale-list-offset-projection
  (@#'fixture/with-fixture
   false
   (fn [{:keys [conn w host owned attached attached-descriptor]}]
     (let [p (ops/party-principal (:member w)) handle (str (:room w))]
       (seed! owned "echo" workflow-fixture/fixture)
       (seed! attached "echo" workflow-fixture/fixture)
       (let [before (owned/details! host p handle {:name "echo"})
             names (owned/list! host p handle {})
             generic (workflows/details-in @owned "echo")]
         (d/transact conn [{:room/id (:room w)
                            :room/default-repository [:system/id (:registry-system attached-descriptor)]}])
         (let [after (owned/details! host p handle {:name "echo"})]
           (is (not= (get-in before [:data :revision]) (get-in after [:data :revision])))
           (is (= (:revision generic) (:revision (workflows/details-in @attached "echo"))))
           (is (= (get-in before [:data :inspection :bundle]) (get-in after [:data :inspection :bundle])))
           (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Stale revision must precede body IO")))]
             (doseq [read [owned/details! owned/source!]]
               (is (= ::owned/conflict
                      (:type (ex-data (failure #(read host p handle {:name "echo"
                                                                     :expected-revision (get-in before [:data :revision])}))))))))
           (is (= ::owned/conflict
                  (:type (ex-data (failure #(owned/list! host p handle {:offset 100
                                                                        :expected-revision (get-in names [:data :revision])}))))))))))))

(deftest invalid-dtos-and-current-authority-refusals-never-hydrate
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w host hydrations]}]
     (let [p (ops/party-principal (:member w)) handle (str (:room w))]
       (doseq [request [{:name "echo" :owner :forged} {:name "echo\n"} {:name "echo" :expected-revision (apply str (repeat 36 "z"))}
                        {:name "echo" :path "x"} {:name "echo" :file-offset 1}]]
         (is (= ::owned/invalid (:type (ex-data (failure #(owned/source! host p handle request)))))))
       (doseq [principal [(ops/party-principal (:non-member w)) (assoc p :principal/kind :worker)
                          (assoc p :principal/kind :agent)]]
         (is (= ::owned/unavailable (:type (ex-data (failure #(owned/details! host principal handle {:name "echo"})))))))
       (doseq [owner [nil {} {:control-conn :forged :execution-ctx :forged}]]
         (is (= ::owned/unavailable
                (:type (ex-data (failure #(owned/details! (assoc host ::reads/owner owner) p handle {:name "echo"})))))))
       (is (empty? @hydrations))))))

(deftest missing-and-malformed-drafts-have-target-bound-identity-with-fixed-diagnostics
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w host owned]}]
     (let [p (ops/party-principal (:member w)) handle (str (:room w))]
       (seed! owned "draft" {"workflow.edn" "{:unclosed"})
       (doseq [name ["empty" "draft"]]
         (let [full (:data (owned/details! host p handle {:name name}))
               page (:data (owned/source! host p handle {:name name :expected-revision (:revision full)}))]
           (is (uuid? (parse-uuid (:revision full))))
           (is (= (:revision full) (:revision page)))
           (is (false? (get-in full [:inspection :ok])))
           (is (= ["Workflow source is incomplete or invalid; inspect its files"]
                  (get-in full [:inspection :problems]) (get-in page [:inspection :problems])))))))))

(deftest helper-errors-and-caught-inspection-problems-cannot-leak-or-forge-conflict
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w host owned]}]
     (seed! owned "echo" workflow-fixture/fixture)
     (let [p (ops/party-principal (:member w)) handle (str (:room w))
           forged (fn [& _] (throw (ex-info "SECRET/native/config" {:type ::owned/conflict :secret "SECRET"})))]
       (doseq [stage [:binding :manifest :body]]
         (with-redefs [reads/resolve-host-read! (if (= stage :binding) forged reads/resolve-host-read!)
                       workflows/manifest-in (if (= stage :manifest) forged workflows/manifest-in)
                       content/read-by-id (if (= stage :body) forged content/read-by-id)]
           (doseq [read [owned/details! owned/source!]]
             (let [error (failure #(read host p handle {:name "echo"}))]
               (is (= ::owned/unavailable (:type (ex-data error))))
               (is (not (str/includes? (pr-str [(ex-message error) (ex-data error)]) "SECRET")))))))
       (doseq [stage [:bundle :dataset]]
         (with-redefs [catalog/bundle (if (= stage :bundle) forged catalog/bundle)
                       dataset/manifest (if (= stage :dataset) forged dataset/manifest)]
           (doseq [read [owned/details! owned/source!]]
             (let [result (read host p handle {:name "echo"})]
               (is (false? (get-in result [:data :inspection :ok])))
               (is (not (str/includes? (pr-str (:data result)) "SECRET")))))))))))

(deftest source-identities-ignore-unrelated-head-but-track-content-mode-name-and-list
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w host owned]}]
     (let [p (ops/party-principal (:member w)) handle (str (:room w))]
       (seed! owned "echo" workflow-fixture/fixture)
       (let [before (:data (owned/details! host p handle {:name "echo"}))
             names (:data (owned/list! host p handle {}))]
         (repo/write! owned "outside.md" (.getBytes "unrelated" "UTF-8"))
         (repo/stage! owned ["outside.md"])
         (repo/commit! owned {:message "Unrelated HEAD edit"})
         (is (= before (:data (owned/details! host p handle {:name "echo"}))))
         (repo/write! owned "workflows/echo/gold.edn" (.getBytes "{:expected :changed}" "UTF-8"))
         (let [changed (:data (owned/details! host p handle {:name "echo"}))]
           (is (not= (:revision before) (:revision changed)))
           (is (= names (:data (owned/list! host p handle {}))))
           (d/transact owned [[:db/add [:geschichte.work/path "workflows/echo/gold.edn"] :geschichte.work/mode 33261]])
           (let [mode (:data (owned/details! host p handle {:name "echo"}))]
             (is (not= (:revision changed) (:revision mode)))
             (is (= (get-in changed [:inspection :bundle]) (get-in mode [:inspection :bundle])))))
         (seed! owned "second" workflow-fixture/fixture)
         (is (not= (:revision names) (get-in (owned/list! host p handle {}) [:data :revision])))
         (is (not= (get-in (owned/details! host p handle {:name "empty-one"}) [:data :revision])
                   (get-in (owned/details! host p handle {:name "empty-two"}) [:data :revision]))))))))

(deftest wrapped-causal-interruption-is-preserved-without-another-body-read
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w host]}]
     (let [p (ops/party-principal (:member w)) handle (str (:room w))]
       (try
         (with-redefs [workflows/manifest-in (fn [& _] (throw (ex-info "SECRET" {} (InterruptedException. "cancelled"))))
                       content/read-by-id (fn [& _] (throw (AssertionError. "No work after interruption")))]
           (let [error (failure #(owned/details! host p handle {:name "echo"}))
                 interrupted? (.isInterrupted (Thread/currentThread))]
             (is (= ::owned/unavailable (:type (ex-data error))))
             (is interrupted?)))
         (finally (Thread/interrupted)))))))

(deftest post-hydration-revocation-and-original-slug-remapping-refuse-before-source
  (doseq [mode [:revoke :mapping]]
    (@#'fixture/with-fixture
     true
     (fn [{:keys [conn w room host]}]
       (let [p (ops/party-principal (:member w))
             original (:room/slug (d/entity @conn [:room/id (:room w)]))
             count! (atom 0) source-calls (atom 0)]
         (with-redefs [rooms/live-room
                       (fn [id]
                         (is (= (:room w) id))
                         (swap! count! inc)
                         (case mode
                           :revoke (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
                           :mapping (d/transact conn [[:db/add [:room/id (:room w)] :room/slug "replacement-slug"]]))
                         room)
                       workflows/manifest-in (fn [& _] (swap! source-calls inc)
                                               (throw (AssertionError. "No source after authority race")))]
           (is (= ::owned/unavailable
                  (:type (ex-data (failure #(owned/details! host p original {:name "echo"}))))))
           (is (= 1 @count!))
           (is (zero? @source-calls))))))))

(deftest caught-inspection-interruption-remains-unavailable-not-normal-diagnostics
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w host owned]}]
     (seed! owned "echo" workflow-fixture/fixture)
     (let [p (ops/party-principal (:member w)) handle (str (:room w))
           bundle catalog/bundle snapshot workflows/snapshot-in read-body content/read-by-id]
       (doseq [stage [:bundle :dataset] read [owned/details! owned/source!]]
         (let [calls (atom {:snapshot 0 :bundle 0 :dataset 0 :body 0 :page 0})
               fail! (fn [] (throw (ex-info "SECRET" {} (InterruptedException. "cancelled"))))]
           (try
             (with-redefs [catalog/bundle (fn [& args] (swap! calls update :bundle inc)
                                            (if (= stage :bundle) (fail!) (apply bundle args)))
                           dataset/manifest (fn [& _] (swap! calls update :dataset inc) (fail!))
                           workflows/snapshot-in (fn [& args] (swap! calls update :snapshot inc) (apply snapshot args))
                           workflows/source-page-of (fn [& _] (swap! calls update :page inc)
                                                      (throw (AssertionError. "No page after interruption")))
                           content/read-by-id (fn [reader id & options]
                                                (when (empty? options) (swap! calls update :body inc))
                                                (apply read-body reader id options))]
               (let [error (failure #(read host p handle {:name "echo"}))
                     interrupted? (.isInterrupted (Thread/currentThread))]
                 (is (= ::owned/unavailable (:type (ex-data error))))
                 (is interrupted?))
               (is (= {:snapshot 1 :bundle 1 :dataset (if (= stage :bundle) 0 1)
                       :body (count workflow-fixture/fixture) :page 0} @calls)))
             (finally (Thread/interrupted)))))))))

(deftest unexpected-identity-hash-errors-are-not-exposed-as-conflicts
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w host]}]
     (let [p (ops/party-principal (:member w)) handle (str (:room w))
           hash hasch/uuid domains (atom [])
           forged (fn [& _] (throw (ex-info "SECRET/hash" {:type ::owned/conflict})))]
       (with-redefs [hasch/uuid (fn [value & options]
                                  (if (and (vector? value)
                                           (#{:simmis/owned-workflow-source-v1 :simmis/owned-workflow-list-v1} (first value)))
                                    (do (swap! domains conj (first value)) (forged))
                                    (apply hash value options)))]
         (doseq [read [#(owned/details! host p handle {:name "empty"})
                       #(owned/list! host p handle {})]]
           (let [error (failure read)]
             (is (= ::owned/unavailable (:type (ex-data error))))
             (is (not (str/includes? (pr-str [(ex-message error) (ex-data error)]) "SECRET")))))
         (is (= [:simmis/owned-workflow-source-v1 :simmis/owned-workflow-list-v1] @domains)))))))

(deftest normal-manifest-page-bounds-refuse-before-body-decoding
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w host owned]}]
     (seed! owned "echo" workflow-fixture/fixture)
     (let [p (ops/party-principal (:member w)) handle (str (:room w))
           revision (get-in (owned/details! host p handle {:name "echo"}) [:data :revision])
           names (get-in (owned/list! host p handle {}) [:data :revision])]
       (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Bad page bounds before bodies")))]
         (doseq [request [{:path "missing.md" :offset 0} {:path "gold.edn" :offset 1000} {:file-offset 100}]]
           (is (= ::owned/invalid
                  (:type (ex-data (failure #(owned/source! host p handle
                                                           (assoc request :name "echo" :expected-revision revision))))))))
         (is (= ::owned/invalid
                (:type (ex-data (failure #(owned/list! host p handle {:offset 100 :expected-revision names})))))))))))
