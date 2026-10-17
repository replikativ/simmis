(ns is.simm.authority.mcp-work-test
  "The real MCP binding and ReBAC gate over deterministic governed Runs."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [dvergr.jobs :as jobs]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.mcp.json-rpc :as rpc]
            [dvergr.mcp.server :as server]
            [dvergr.mcp.surface :as surface]
            [dvergr.resource :as resource]
            [dvergr.room.store :as store]
            [is.simm.agents.room-agents :as agents]
            [is.simm.authority.world :as world]
            [is.simm.model.room-databases :as room-dbs]
            [is.simm.ops.chat :as chat]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.results :as results]
            [is.simm.ops.work :as work]
            [is.simm.ops.work-api :as api]
            [is.simm.ops.work-test]
            [is.simm.ops.workflows-test :as workflow-fixture]
            [is.simm.runtimes.auth-config :as auth]
            [is.simm.runtimes.mcp :as mcp]
            [jsonista.core :as json]))

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
