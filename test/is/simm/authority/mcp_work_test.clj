(ns is.simm.authority.mcp-work-test
  "The real MCP binding and ReBAC gate over deterministic governed Runs."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [dvergr.jobs :as jobs]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.agent.experiment.runner :as runner]
            [dvergr.agent.workflow :as workflow]
            [dvergr.mcp.json-rpc :as rpc]
            [dvergr.mcp.http :as http]
            [dvergr.mcp.server :as server]
            [dvergr.mcp.surface :as surface]
            [dvergr.resource :as resource]
            [dvergr.room.store :as store]
            [is.simm.agents.room-agents :as agents]
            [is.simm.authority.world :as world]
            [is.simm.model.room-databases :as room-dbs]
            [is.simm.ops.chat :as chat]
            [is.simm.ops.benchmarks-test :as benchmark-fixture]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.results :as results]
            [is.simm.ops.work :as work]
            [is.simm.ops.work-api :as api]
            [is.simm.ops.workflow-runs :as workflow-runs]
            [is.simm.ops.workflow-runs-test :as workflow-run-fixture]
            [is.simm.ops.workflows :as workflows]
            [is.simm.ops.work-test]
            [is.simm.ops.workflows-test :as workflow-fixture]
            [is.simm.runtimes.auth-config :as auth]
            [is.simm.runtimes.mcp :as mcp]
            [jsonista.core :as json]
            [org.httpkit.client :as client]))

(defn- session [party mode]
  (let [principal ((mcp/authenticator) {:bearer (:token (mcp/mint-token! party mode))})
        ctx (server/session-context (fn [_]) (surface/selection {:profile :admin}) principal)]
    (rpc/handle-message ctx {:jsonrpc "2.0" :id 1 :method "initialize"
                             :params {:protocolVersion "2025-06-18" :capabilities {}
                                      :clientInfo {:name "not-authenticated-agent-identity" :version "1"}}})
    (rpc/handle-message ctx {:jsonrpc "2.0" :method "notifications/initialized"})
    ctx))

(defn- call [ctx tool args]
  (rpc/handle-message ctx {:jsonrpc "2.0" :id 2 :method "tools/call"
                           :params {:name tool :arguments args}}))

(defn- refused? [response]
  (boolean (or (:error response) (get-in response [:result :isError]))))

(defn- value [response]
  (is (not (refused? response)))
  (json/read-value (get-in response [:result :content 0 :text]) json/keyword-keys-object-mapper))

(defn- http-message [port token sid id method params]
  (let [response @(client/post (str "http://127.0.0.1:" port "/mcp")
                               {:timeout 10000
                                :headers (cond-> {"Authorization" (str "Bearer " token)
                                                  "Content-Type" "application/json"
                                                  "MCP-Protocol-Version" "2025-06-18"}
                                           sid (assoc "Mcp-Session-Id" sid))
                                :body (json/write-value-as-string
                                       (cond-> {:jsonrpc "2.0" :method method :params params}
                                         id (assoc :id id)))})]
    (is (nil? (:error response)))
    {:status (:status response) :sid (get-in response [:headers :mcp-session-id])
     :rpc (when (seq (:body response)) (json/read-value (:body response) json/keyword-keys-object-mapper))}))

(deftest actual-http-workflow-authoring-preserves-paths-and-live-room-authority
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [room]
       (let [slug (store/room-id->slug (:id room)) path "fixtures/资料/输入.md"
             input {:room slug :name "echo" :files (assoc workflow-fixture/fixture path "Unicode input")}]
         (workspace/ensure-workspace! room)
         (d/transact conn [{:room/id (:room w) :room/slug slug}])
         (server/set-host! (ops/host (:ctx room)))
         (try
           (api/install!)
           (with-redefs [auth/get-auth-config (constantly {:jwt {:secret "mcp-http-workflow-test"}})]
             (let [transport (http/start! {:port 0 :bind "127.0.0.1" :profile :admin :toolsets #{:host}
                                           :authenticate (mcp/authenticator)})
                   port (:port transport) token (:token (mcp/mint-token! (:owner w) :work))]
               (try
                 (let [opened (http-message port token nil 1 "initialize"
                                            {:protocolVersion "2025-06-18" :capabilities {}
                                             :clientInfo {:name "workflow-http-fixture" :version "1"}})
                       sid (:sid opened)]
                   (is sid)
                   (http-message port token sid nil "notifications/initialized" {})
                   (let [written (value (:rpc (http-message port token sid 2 "tools/call"
                                                            {:name "simmis_workflow-write" :arguments input})))]
                     (is (get-in written [:write :commit]))
                     (is (true? (get-in written [:workflow :ok])))
                     (is (false? (get-in written [:workflow :checker-executed?]))))
                   (is (= "Unicode input" (get (:files (workflows/snapshot room "echo")) path)))
                   (value (:rpc (http-message port token sid 3 "tools/call"
                                              {:name "simmis_workflow-write" :arguments (assoc input :files {path nil})})))
                   (is (not (contains? (:files (workflows/snapshot room "echo")) path)))
                   (let [admitted (atom [])
                         args (-> (workflow-run-fixture/input workflow-fixture/fixture)
                                  (dissoc :params)
                                  (assoc :room slug :request-id (str (random-uuid))
                                         :expected-bundle (:bundle (workflows/inspect room "echo"))
                                         :inputs {"/docs/资料.md" "actual JSON HTTP input"}))]
                     (with-redefs [work/submit-prepared! (fn [_ _ _ _ _ normalized prepared]
                                                         (swap! admitted conj [normalized prepared])
                                                         {:state "submitted"})]
                       (let [submitted (value (:rpc (http-message port token sid 5 "tools/call"
                                                                 {:name "simmis_workflow-run" :arguments args})))]
                         (is (= "submitted" (:state submitted)))))
                     (is (= {"/docs/资料.md" "actual JSON HTTP input"} (:inputs (ffirst @admitted))))
                     (is (= :workflow (:request-kind (second (first @admitted))))))
                   (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:owner w)]]
                                     [:db/add [:room/id (:room w)] :room/owner [:party/id (:non-member w)]]])
                   (is (refused? (:rpc (http-message port token sid 4 "tools/call"
                                                     {:name "simmis_workflow-write" :arguments input}))))
                   (is (not (contains? (:files (workflows/snapshot room "echo")) path))))
                 (finally ((:stop transport))))))
           (finally (api/uninstall!) (server/set-host! nil))))))))

(deftest workspace-bundles-recheck-read-authority-and-credential-ceilings
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [room]
       (let [slug (store/room-id->slug (:id room))
             args {:room slug}]
         (workspace/ensure-workspace! room)
         (workspace/seed! room {"/readme.md" "snapshot input"})
         (d/transact conn [{:room/id (:room w) :room/slug slug}])
         (server/set-host! (ops/host (:ctx room)))
         (try
           (api/install!)
           (with-redefs [auth/get-auth-config (constantly {:jwt {:secret "mcp-bundle-test"}})]
             (let [member (session (:member w) :read)
                   stranger (session (:non-member w) :read)
                   admin (session (:admin w) :read)
                   descriptor (value (call member "simmis_workspace-bundle" args))
                   chunk-args (assoc args :expected-snapshot (:snapshot descriptor) :offset 0 :length 16)
                   old-principal (update (:principal member) :principal/tools disj
                                         "simmis_workspace-bundle" "simmis_workspace-bundle-chunk")
                   old (server/session-context (fn [_]) (surface/selection {:profile :admin}) old-principal)]
               (is (false? (:history descriptor)))
               (is (= (:snapshot descriptor) (:snapshot (value (call member "simmis_workspace-bundle-chunk" chunk-args)))))
               (doseq [ctx [stranger admin old]
                       [tool input] [["simmis_workspace-bundle" args] ["simmis_workspace-bundle-chunk" chunk-args]]]
                 (is (refused? (call ctx tool input))))
               (doseq [bad [(assoc chunk-args :eval true) (assoc chunk-args :length 16385)
                            (assoc chunk-args :offset -1) (assoc chunk-args :expected-snapshot "invalid")]]
                 (is (refused? (call member "simmis_workspace-bundle-chunk" bad))))
               (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
               (is (refused? (call member "simmis_workspace-bundle-chunk" chunk-args)))))
           (finally (api/uninstall!) (server/set-host! nil))))))))

(deftest workflow-authoring-is-scoped-to-current-room-write-permission
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [room]
       (let [slug (store/room-id->slug (:id room))
             args {:room slug :name "echo" :files workflow-fixture/fixture}]
         (workspace/ensure-workspace! room)
         (d/transact conn [{:room/id (:room w) :room/slug slug}])
         (server/set-host! (ops/host (:ctx room)))
         (try
           (api/install!)
           (with-redefs [auth/get-auth-config (constantly {:jwt {:secret "mcp-workflow-test"}})]
             (let [member (session (:member w) :work)
                   owner (session (:owner w) :work)
                   admin (session (:admin w) :work)
                   stranger (session (:non-member w) :work)
                   reader (session (:owner w) :read)]
               (doseq [ctx [stranger admin reader]
                       [tool input] [["simmis_workflow-write" args]
                                     ["simmis_workflow-check" (dissoc args :files)]]]
                 (is (refused? (call ctx tool input))))
               (let [written (value (call member "simmis_workflow-write" args))]
                 (is (some? (get-in written [:write :commit])))
                 (is (true? (get-in written [:workflow :ok]))))
               (is (true? (:ok (value (call member "simmis_workflow-check" (dissoc args :files))))))
               (is (false? (:checker-executed? (value (call owner "simmis_workflow-check" (dissoc args :files))))))
               (doseq [extra [{:sender (str (:owner w))} {:principal "admin"} {:answer "run"} {:eval true}]]
                 (is (refused? (call member "simmis_workflow-check" (merge (dissoc args :files) extra)))))
               (is (refused? (call member "simmis_workflow-write"
                                   (assoc args :files {"../../AGENTS.md" "overwrite"}))))
               (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties
                                  [:party/id (:member w)]]])
               (doseq [[tool input] [["simmis_workflow-write" args]
                                     ["simmis_workflow-check" (dissoc args :files)]]]
                 (is (refused? (call member tool input))))))
           (finally (api/uninstall!) (server/set-host! nil))))))))

(deftest authored-checker-execution-is-not-implied-by-room-ownership-or-membership
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [room]
       (let [slug (store/room-id->slug (:id room))
             files (assoc workflow-fixture/fixture "cases/tiny/gold.edn" "{:expected 1}")
             admitted (atom [])]
         (workspace/ensure-workspace! room)
         (workflows/write! room {:principal/id "host-fixture"} {:name "echo" :files files})
         (d/transact conn [{:room/id (:room w) :room/slug slug}])
         (server/set-host! (ops/host (:ctx room)))
         (try
           (api/install!)
           (with-redefs [auth/get-auth-config (constantly {:jwt {:secret "mcp-benchmark-test"}})
                         workflow/candidates benchmark-fixture/candidates
                         runner/runtime-identity (constantly {:libraries {}})
                         ;; Keep real schemas, auth, immutable snapshot and
                         ;; plan construction; stop exactly before scheduling.
                         work/submit-prepared! (fn [_ _ principal _ _ input prepared]
                                                 (swap! admitted conj [principal input prepared])
                                                 {:state "submitted"})]
             (let [owner (session (:owner w) :work)
                   args (assoc (benchmark-fixture/input files)
                               :room slug :request-id (str (random-uuid)) :cases ["tiny"]
                               :expected-bundle (:bundle (workflows/inspect room "echo")))]
               (doseq [ctx [owner (session (:member w) :work) (session (:non-member w) :work)
                            (session (:admin w) :work) (session (:owner w) :read)]]
                 (is (refused? (call ctx "simmis_benchmark" args))))
               (is (empty? @admitted))
               (d/transact conn [{:party/id (:owner w) :party/role :admin}])
               (is (= "submitted" (:state (value (call owner "simmis_benchmark" args)))))
               (is (= 1 (count @admitted)))
               (is (= (str "party/" (:owner w)) (:principal/id (ffirst @admitted))))
               (is (uuid? (:expected-bundle (second (first @admitted)))))
               (doseq [extra [{:sender (str (:owner w))} {:eval true} {:parallelism 99}]]
                 (is (refused? (call owner "simmis_benchmark" (merge args extra)))))
               (is (refused? (call owner "simmis_benchmark" (assoc args :expected-bundle (str (random-uuid))))))
               (is (= 1 (count @admitted)))))
           (finally (api/uninstall!) (server/set-host! nil))))))))

(deftest saved-workflow-execution-has-an-exact-tool-ceiling-and-current-merge-authority
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [room]
       (let [slug (store/room-id->slug (:id room)) admitted (atom [])]
         (workspace/ensure-workspace! room)
         (workflows/write! room {:principal/id "fixture"} {:name "echo" :files workflow-run-fixture/source})
         (d/transact conn [{:room/id (:room w) :room/slug slug}])
         (server/set-host! (ops/host (:ctx room)))
         (try
           (api/install!)
           (with-redefs [auth/get-auth-config (constantly {:jwt {:secret "mcp-workflow-run-test"}})
                         work/submit-prepared! (fn [_ _ principal _ _ input prepared]
                                                 (swap! admitted conj {:principal principal :input input :prepared prepared})
                                                 {:state "submitted"})]
             (let [owner (session (:owner w) :work)
                   old (with-redefs [mcp/work-tools (disj mcp/work-tools "simmis_workflow-run")]
                         (session (:owner w) :work))
                   input (-> (workflow-run-fixture/input)
                             (assoc :room slug :request-id (str (random-uuid))
                                    :expected-bundle (:bundle (workflows/inspect room "echo"))
                                    :params {:subject "literal {subject}"}
                                    :inputs {(keyword "/docs/输入.md") "explicit UTF-8 input"}))]
               (doseq [ctx [old (session (:owner w) :read) (session (:member w) :work)
                            (session (:non-member w) :work) (session (:admin w) :work)]]
                 (is (refused? (call ctx "simmis_workflow-run" input))))
               (is (empty? @admitted))
               (is (= "submitted" (:state (value (call owner "simmis_workflow-run" input)))))
               (is (= 1 (count @admitted)))
               (is (= (str "party/" (:owner w)) (get-in @admitted [0 :principal :principal/id])))
               (is (= {"/docs/输入.md" "explicit UTF-8 input"} (get-in @admitted [0 :input :inputs])))
               (is (= {"subject" "literal {subject}"} (get-in @admitted [0 :input :params])))
               (is (= :workflow (get-in @admitted [0 :prepared :request-kind])))
               (doseq [extra [{:sender (str (:owner w))} {:eval true} {:tools ["shell"]}
                              {:limits (assoc (:limits input) :settlement "merge")}]]
                 (is (refused? (call owner "simmis_workflow-run" (merge input extra)))))
               (is (refused? (call owner "simmis_workflow-run" (assoc input :expected-bundle (str (random-uuid))))))
               (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:owner w)]]
                                 [:db/add [:room/id (:room w)] :room/owner [:party/id (:non-member w)]]])
               (is (refused? (call owner "simmis_workflow-run" input)))
               (is (= 1 (count @admitted)))))
           (finally (api/uninstall!) (server/set-host! nil))))))))

(deftest explicit-work-capabilities-use-canonical-jobs-and-room-authority
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [room]
       (let [[input prepared] (@#'is.simm.ops.work-test/fixture)
             slug (store/room-id->slug (:id room))
             args (assoc input :room slug :request-id (str (:request-id input)))
             allowance {:room slug :request-id (str (random-uuid))
                        :budget-microdollars 10000 :max-dispatches 2}]
         (d/transact conn [{:room/id (:room w) :room/slug slug}])
         (server/set-host! (ops/host (:ctx room)))
         (try
           (results/install!) (api/install!)
           (with-redefs [auth/get-auth-config (constantly {:jwt {:secret "mcp-work-test"}})
                         work/prepare (constantly prepared)]
             (let [owner (session (:owner w) :work)
                   reader (session (:owner w) :read)
                   member (session (:member w) :work)
                   stranger (session (:non-member w) :work)
                   admin (session (:admin w) :work)]
               (testing "neither a read credential nor room membership is generic code authority"
                 (doseq [ctx [reader member stranger admin]
                         [tool input] [["simmis_task" args] ["simmis_work-allowance" allowance]]]
                   (is (refused? (call ctx tool input)))))
               (testing "closed arguments are enforced before any execution"
                 (doseq [bad [(assoc args :sender (str (:member w)))
                              (assoc-in args [:limits :settlement] "merge")
                              (assoc-in args [:limits :budget-microdollars] -1)
                              (assoc args :request-id "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")]]
                   (is (refused? (call owner "simmis_task" bad)))))
               (let [fund (value (call owner "simmis_work-allowance" allowance))]
                 (is (= "provisioned" (:state fund)))
                 (is (= fund (value (call owner "simmis_work-allowance" allowance)))))
               (let [submitted (value (call owner "simmis_task" args))
                     job (parse-uuid (:job submitted))]
                 (is (= "completed" (name (:status (jobs/status job 25000)))))
                 (is (= (:job submitted) (:job (value (call owner "simmis_task" args)))))
                 (is (= 1 (count (jobs/jobs room))))
                 (is (= (work/resources (:limits input)) (resource/balance room)))
                 (let [status-args {:room slug :id (:id submitted)}
                       status (value (call owner "simmis_work-status" status-args))]
                   (is (not (contains? status :claim)))
                   (is (not (contains? status :payload-ref)))
                   (is (false? (:return-pending? status)))
                   (is (= 1 (count (value (call owner "attempt_list" {:room slug})))))
                   (d/transact conn [{:room/id (:room w) :room/owner [:party/id (:member w)]}
                                     [:db/retract [:room/id (:room w)] :room/parties
                                      [:party/id (:owner w)]]])
                   (is (refused? (call owner "simmis_work-status" status-args)))))))
           (finally (api/uninstall!) (results/uninstall!) (server/set-host! nil))))))))

(deftest chat-source-and-author-come-from-the-host-not-tool-input
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [room]
       (let [slug (store/room-id->slug (:id room))
             seen (atom [])
             id (random-uuid)
             parent (random-uuid)]
         (d/transact conn [{:room/id (:room w) :room/slug slug}])
         (server/set-host! (ops/host (:ctx room)))
         (try
           (api/install!)
           (with-redefs [auth/get-auth-config (constantly {:jwt {:secret "mcp-chat-test"}})
                         room-dbs/connect-room-database (constantly conn)
                         chat/ensure-room-chatroom! (fn [& _])
                         chat/ensure-party-projection! (fn [& _])
                         agents/post-user-message! (fn [& args] (swap! seen conj args) {:status :ok})]
             (let [ctx (session (:member w) :work)
                   args {:room slug :id (str id) :content "fixture" :in-reply-to (str parent)}]
               (is (not (refused? (call ctx "simmis_post" args))))
               (is (= [[(:room w) "fixture" (:member w) conn id parent :mcp]] @seen))
               (is (refused? (call ctx "simmis_post" (assoc args :source "web"))))
               (is (refused? (call ctx "simmis_post" (assoc args :sender (str (:owner w))))))
               (is (= 1 (count @seen)))))
           (finally (api/uninstall!) (server/set-host! nil))))))))
