(ns is.simm.ops.owned-workflow-transport-test
  "Inactive real op/MCP composition after authentication. No listener, JWT,
   browser activation, authoring or execution claim."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.mcp.server :as server]
            [dvergr.ops :as dops]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.content :as content]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.ops.owned-mcp-transport-test :as wire]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflows :as owned]
            [is.simm.ops.work-api :as api]
            [is.simm.ops.workflows :as workflows]
            [is.simm.ops.workflows-test :as source]
            [is.simm.runtimes.mcp :as mcp]))

(defn- with-api [f]
  (let [previous (dops/operations)]
    (try (api/install!) (f)
         (finally
           (doseq [op (keys api/specifications)]
             (if-let [spec (get previous op)] (dops/register-op! op spec) (dops/unregister-op! op)))
           (server/sync-op-tools!)))))

(defn- principal [w tools]
  (assoc (ops/party-principal (:member w)) :principal/mcp? true
         :principal/tools tools :principal/scopes (if (= tools mcp/read-tools) #{:dvergr/read-only} #{})))

(defn- seed! [conn files]
  (doseq [[path text] files] (repo/write! conn (str "workflows/echo/" path) (.getBytes ^String text "UTF-8"))))

(defn- failure [f] (try (f) nil (catch clojure.lang.ExceptionInfo error error)))

(deftest all-protocols-use-the-same-owned-source-without-selected-rereads
  (doseq [first? [true false]]
    (@#'fixture/with-fixture
     first?
     (fn [{:keys [conn w host owned attached]}]
       (seed! owned source/fixture)
       (seed! attached (assoc source/fixture "gold.edn" "{:expected :OTHER}"))
       (with-api
         (fn []
           (let [p (principal w mcp/work-tools) handle (str (:room w))
                 slug (:room/slug (d/entity @conn [:room/id (:room w)]))
                 full (:data (owned/details! host p handle {:name "echo"}))
                 original reads/resolve-host-read! captures (atom 0)
                 selected (workspace/selected-system-id)
                 other {:commit (d/commit-id @attached) :config (:config @attached) :worktree (query/worktree @attached)}
                 forbidden (fn [& _] (throw (AssertionError. "No selected or second Room lookup")))]
             (with-redefs [server/current-daemon (constantly host)
                           api/room! forbidden workflows/details forbidden workflows/source-page forbidden workflows/list-page forbidden
                           reads/resolve-host-read! (fn [& args] (swap! captures inc) (apply original args))]
               (doseq [version ["2024-11-05" "2025-03-26" "2025-06-18" "2025-11-25" "2026-07-28"]
                       room [handle slug]]
                 (let [send (@#'wire/session version p)
                       names (@#'wire/data (@#'wire/tool send "simmis_workflow-list" {:room room}))
                       page (@#'wire/data (@#'wire/tool send "simmis_workflow-source" {:room room :name "echo"}))
                       chunk (@#'wire/data (@#'wire/tool send "simmis_workflow-source"
                                                         {:room room :name "echo" :path "gold.edn" :offset 0
                                                          :expected-revision (str/upper-case (:revision page))}))]
                   (is (= ["echo"] (:names names)))
                   (is (= (:revision full) (:revision page) (:revision chunk)))
                   (is (= (get-in full [:inspection :bundle]) (get-in page [:inspection :bundle])))
                   (is (= (get source/fixture "gold.edn")
                          (String. ^bytes (.decode (java.util.Base64/getDecoder) ^String (:base64 chunk)) "UTF-8")))
                   (is (not (contains? page :read-context))))))
             (is (= 30 @captures))
             (is (= selected (workspace/selected-system-id)))
             (is (= other {:commit (d/commit-id @attached) :config (:config @attached) :worktree (query/worktree @attached)})))))))))

(deftest plain-read-ceilings-and-worker-agent-denials-are-not-widened
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w host hydrations]}]
     (with-api
       (fn []
         (let [handle (str (:room w)) auth (get-in host [:config :auth :authorize])]
           (with-redefs [server/current-daemon (constantly host)]
             (doseq [version ["2025-11-25" "2026-07-28"]]
               (let [send (@#'wire/session version (principal w mcp/read-tools))
                     names (set (map :name (get-in (send "tools/list" {}) [:result :tools])))]
                 (is (not (contains? names "simmis_workflow-list")))
                 (is (not (contains? names "simmis_workflow-source")))
                 (doseq [[tool args] [["simmis_workflow-list" {:room handle}]
                                      ["simmis_workflow-source" {:room handle :name "echo"}]]]
                   (let [reply (@#'wire/tool send tool args)]
                     (is (or (:error reply) (get-in reply [:result :isError]))))))))
           (is (empty? @hydrations))
           (doseq [tools [mcp/work-tools mcp/worker-control-tools]]
             (let [p (principal w tools)]
               (is (nil? (dops/authorize! host p :simmis/workflow-list {:room handle})))
               (is (nil? (dops/authorize! host p :simmis/workflow-source {:room handle :name "echo"})))))
           (doseq [kind [:worker :agent]]
             (let [p (assoc (principal w mcp/work-tools) :principal/kind kind)]
               (with-redefs [ops/authorize (fn [& _] {:allow? true})]
                 (doseq [op [:simmis/workflow-list :simmis/workflow-source]]
                   (is (false? (:allow? (auth p {:op op :action :read :resource [:room handle]}))))
                   (is (false? (:allow? (auth p {:op op :action :read :resource [:room handle]
                                                 :args (cond-> {:room handle} (= op :simmis/workflow-source) (assoc :name "echo"))})))))
                 (is (:allow? (auth p {:op :room/read :action :read :resource [:room handle]
                                       :args {:room handle :path "wiki/file.md"}}))))))
           (is (empty? @hydrations))))))))

(deftest legacy-owner-absence-and-original-resource-coherence-remain-distinct
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w room host owned attached hydrations]}]
     (seed! owned source/fixture)
     (seed! attached (assoc source/fixture "gold.edn" "{:expected :OTHER}"))
     (with-api
       (fn []
         (let [p (principal w mcp/work-tools) handle (str (:room w))
               legacy (ops/host (:ctx room))
               selected (workflows/source-page room {:name "echo"})
               auth (get-in host [:config :auth :authorize])]
           (is (= selected (dops/invoke legacy p :simmis/workflow-source {:room handle :name "echo"})))
           (is (not= (:revision selected) (get-in (owned/source! host p handle {:name "echo"}) [:data :revision])))
           (doseq [owner [nil {} {:control-conn :forged :execution-ctx :forged}]]
             (let [error (failure #(dops/invoke (assoc host ::reads/owner owner) p :simmis/workflow-source
                                                {:room handle :name "echo"}))]
               (is (= 503 (dops/error-status error)))))
           (d/transact conn [{:room/id (:other-room w) :room/slug (str "other-" (:other-room w))
                              :room/parties [[:party/id (:member w)]]}])
           (let [other (str (:other-room w)) db @conn]
             (is (= (:room w) (:room-id (reads/authorize-read! db p handle))))
             (is (= (:other-room w) (:room-id (reads/authorize-read! db p other))))
             (reset! hydrations [])
             (doseq [op [:simmis/workflow-list :simmis/workflow-source]]
               (is (false? (:allow? (auth p {:op op :action :read :resource [:room other]
                                             :args (cond-> {:room handle} (= op :simmis/workflow-source) (assoc :name "echo"))})))))
             (is (empty? @hydrations)))))))))

(deftest wire-errors-are-fixed-and-stale-targets-conflict-before-bodies
  (@#'fixture/with-fixture
   false
   (fn [{:keys [conn w host owned attached attached-descriptor]}]
     (seed! owned source/fixture) (seed! attached source/fixture)
     (with-api
       (fn []
         (let [p (principal w mcp/work-tools) handle (str (:room w))]
           (with-redefs [server/current-daemon (constantly host)]
             (let [send (@#'wire/session "2026-07-28" p)
                   page (@#'wire/data (@#'wire/tool send "simmis_workflow-source" {:room handle :name "echo"}))]
               (d/transact conn [{:room/id (:room w) :room/default-repository
                                  [:system/id (:registry-system attached-descriptor)]}])
               (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "No stale source body")))]
                 (is (= 409 (dops/error-status
                             (failure #(dops/invoke host p :simmis/workflow-source
                                                    {:room handle :name "echo" :path "gold.edn" :offset 0 :expected-revision (:revision page)})))))
                 (let [reply (@#'wire/tool send "simmis_workflow-source"
                                           {:room handle :name "echo" :path "gold.edn" :offset 0 :expected-revision (:revision page)})]
                   (is (get-in reply [:result :isError]))
                   (is (str/includes? (pr-str reply) "Workflow source changed; reload metadata"))))
               (with-redefs [content/read-by-id (fn [& _] (throw (ex-info "SECRET/body" {:type ::owned/conflict :dvergr.ops/status 409})))]
                 (is (= 503 (dops/error-status
                             (failure #(dops/invoke host p :simmis/workflow-source {:room handle :name "echo"})))))
                 (let [reply (@#'wire/tool send "simmis_workflow-source" {:room handle :name "echo"})]
                   (is (get-in reply [:result :isError]))
                   (is (str/includes? (pr-str reply) "Owned workflow read unavailable"))
                   (is (not (str/includes? (pr-str reply) "SECRET")))))))))))))
