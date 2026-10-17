(ns is.simm.authority.mcp-workers-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.mcp.json-rpc :as rpc]
            [dvergr.mcp.http :as http]
            [dvergr.mcp.server :as server]
            [dvergr.mcp.surface :as surface]
            [is.simm.authority.world :as world]
            [is.simm.model.workers :as workers]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.worker-api :as api]
            [is.simm.ops.worker-credentials :as credentials]
            [is.simm.ops.worker-files :as files]
            [is.simm.ops.worker-files-test :as file-fixture]
            [is.simm.ops.worker-service :as service]
            [is.simm.ops.workflows-test :as workspace-fixture]
            [jsonista.core :as json]
            [org.httpkit.client :as client])
  (:import [java.util Base64 Date]))

(defn- session [principal]
  (let [ctx (server/session-context (fn [_]) (surface/selection {:profile :admin :toolsets #{:host}}) principal)]
    (rpc/handle-message ctx {:jsonrpc "2.0" :id 1 :method "initialize"
                             :params {:protocolVersion "2025-06-18" :capabilities {}
                                      :clientInfo {:name "worker-fixture" :version "1"}}})
    (rpc/handle-message ctx {:jsonrpc "2.0" :method "notifications/initialized"})
    ctx))

(defn- call [ctx tool args]
  (rpc/handle-message ctx {:jsonrpc "2.0" :id 2 :method "tools/call" :params {:name tool :arguments args}}))
(defn- refused? [response] (boolean (or (:error response) (get-in response [:result :isError]))))
(defn- value [response]
  (is (not (refused? response)) (pr-str response))
  (json/read-value (get-in response [:result :content 0 :text]) json/keyword-keys-object-mapper))

(defn- with-assignment [conn w f]
  (@#'workspace-fixture/with-workspace
   (fn [room]
     (workspace/seed! room {"/docs/input.md" "selected original" "/private.txt" "not selected"})
     (let [owner (ops/party-principal (:owner w)) id (random-uuid)
           enrolled (service/enroll! conn owner (:room w) "fixture-host-secret" id file-fixture/config)
           frozen (files/freeze room "Edit selected input and write out/result.md" file-fixture/config
                                ["docs/input.md"] file-fixture/limits)
           run (random-uuid) proof (workers/new-secret)
           proposed (credentials/authenticate conn {:bearer (:credential enrolled) :headers {"simmis-lease-proof" proof}})]
       (workers/enqueue! conn {:id run :worker id :generation 1 :room (:room w) :request (random-uuid)
                               :job (random-uuid) :input frozen :permitted? credentials/permitted?})
       (server/set-host! (ops/host (:ctx room)))
       (try (api/install!) (f {:room room :owner owner :enrolled enrolled :run run :proof proof
                               :frozen frozen :proposed proposed :enrollment-session (session proposed)})
            (finally (api/uninstall!) (server/set-host! nil)))))))

(deftest worker-mcp-freezes-input-sanitizes-receipts-and-cannot-use-human-operations
  (world/with-world [conn w]
    (with-assignment conn w
      (fn [{:keys [owner enrolled run proof frozen enrollment-session] :as fixture}]
        (let [args {:run (str run)} nonce (str (random-uuid))
              pending (value (call enrollment-session "simmis_worker-pending" {}))
              claim (value (call enrollment-session "simmis_worker-claim" (assoc args :nonce nonce)))
              token (credentials/lease-token run (:lease claim) proof)
              principal (credentials/authenticate conn {:bearer token})
              lease-session (session principal)]
          (doseq [[ctx ceiling] [[enrollment-session credentials/enrollment-tools]
                                 [lease-session credentials/lease-tools]]]
            (is (= ceiling
                   (set (map :name (get-in (rpc/handle-message ctx
                                                               {:jsonrpc "2.0" :id 3 :method "tools/list" :params {}})
                                           [:result :tools]))))))
          (is (= [(str run)] (:runs pending)))
          (is (= "claimed" (:state claim)))
          (is (= claim (value (call enrollment-session "simmis_worker-claim" (assoc args :nonce nonce)))))
          (doseq [response [pending claim]]
            (is (not (str/includes? (pr-str response) "hash")))
            (is (not (str/includes? (pr-str response) "ref")))
            (is (not (str/includes? (pr-str response) proof)))
            (is (not (str/includes? (pr-str response) (:credential enrolled)))))
          (let [chunk (value (call lease-session "simmis_worker-input" args))
                bytes (.decode (Base64/getDecoder) ^String (:base64 chunk))
                input (json/read-value bytes json/keyword-keys-object-mapper)]
            (is (= (str (:input-id frozen)) (:input-id chunk)))
            (is (= (:sha256 chunk) (files/sha256 bytes)))
            (is (= #{:docs/input.md} (set (keys (:files input)))))
            (is (not (str/includes? (String. bytes "UTF-8") "not selected"))))
          (doseq [tool ["room_read" "room_list" "simmis_task" "clojure_eval" "simmis_worker-pending"]]
            (is (refused? (call lease-session tool args))))
          (is (refused? (call (session owner) "simmis_worker-input" args)))
          (doseq [extra [{:room "foreign"} {:eval true} {:now 1} {:length 16385}]]
            (is (refused? (call lease-session "simmis_worker-input" (merge args extra)))))
          (is (refused? (call lease-session "simmis_worker-input" {:run (str (random-uuid))})))
          (let [output {:files {"docs/input.md" (file-fixture/encoded "changed")
                                "out/result.md" (file-fixture/encoded "answer")}
                        :summary "done" :exit-code 0}
                completed (value (call lease-session "simmis_worker-complete" (assoc args :output output)))]
            (is (= "completed" (:state completed)))
            (is (:stop-ack? completed))
            (is (= completed (value (call lease-session "simmis_worker-complete" (assoc args :output output)))))
            (is (refused? (call lease-session "simmis_worker-complete" (assoc args :output (assoc output :summary "different")))))
            (is (= {} (workspace/read-tree (:room fixture) "/out"))
                "transport completion does not bypass the Run's server-side import gate")))))))

(deftest revoked-room-access-cancels-an-open-session-but-retains-minimal-stop-ack
  (world/with-world [conn w]
    (with-assignment conn w
      (fn [{:keys [run proof enrollment-session]}]
        (let [args {:run (str run)}
              claim (value (call enrollment-session "simmis_worker-claim" (assoc args :nonce (str (random-uuid)))))
              principal (credentials/authenticate conn {:bearer (credentials/lease-token run (:lease claim) proof)})
              lease-session (session principal)]
          (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:owner w)]]
                            [:db/add [:room/id (:room w)] :room/owner [:party/id (:non-member w)]]])
          (is (refused? (call lease-session "simmis_worker-input" args)))
          (is (refused? (call lease-session "simmis_worker-complete"
                              (assoc args :output {:files {} :summary "no" :exit-code 0}))))
          (let [beat (value (call lease-session "simmis_worker-heartbeat" args))]
            (is (:cancel-requested? beat))
            (is (false? (:stop-ack? beat))))
          (is (= {:run (str run) :stop-ack? true}
                 (value (call lease-session "simmis_worker-stop" args)))))))))

(deftest room-revocation-during-output-publication-is-checked-in-the-writer
  (world/with-world [conn w]
    (with-assignment conn w
      (fn [{:keys [run proof enrollment-session]}]
        (let [args {:run (str run)}
              claim (value (call enrollment-session "simmis_worker-claim" (assoc args :nonce (str (random-uuid)))))
              lease-session (session (credentials/authenticate conn {:bearer (credentials/lease-token run (:lease claim) proof)}))
              publish artifact/publish-value!]
          (with-redefs [artifact/publish-value!
                        (fn [store output callback]
                          (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:owner w)]]
                                            [:db/add [:room/id (:room w)] :room/owner [:party/id (:non-member w)]]])
                          (publish store output callback))]
            (is (refused? (call lease-session "simmis_worker-complete"
                                (assoc args :output {:files {"out/result" (file-fixture/encoded "answer")}
                                                     :summary "done" :exit-code 0})))))
          (is (nil? (:simmis.worker.assignment/output-id (workers/assignment conn run))))
          (is (nil? (:simmis.worker.assignment/stop-ack-at (workers/assignment conn run)))))))))

(defn- http-message [port token proof sid id method params]
  (let [response @(client/post (str "http://127.0.0.1:" port "/mcp")
                               {:timeout 10000 :headers (cond-> {"Authorization" (str "Bearer " token)
                                                                 "Content-Type" "application/json"
                                                                 "MCP-Protocol-Version" "2025-06-18"}
                                                          proof (assoc "Simmis-Lease-Proof" proof)
                                                          sid (assoc "Mcp-Session-Id" sid))
                                :body (json/write-value-as-string
                                       (cond-> {:jsonrpc "2.0" :method method :params params}
                                         id (assoc :id id)))})]
    (is (nil? (:error response)))
    {:status (:status response)
     :sid (or (get-in response [:headers :mcp-session-id]) (get-in response [:headers "mcp-session-id"]))
     :rpc (when (seq (:body response)) (json/read-value (:body response) json/keyword-keys-object-mapper))}))

(deftest actual-http-worker-session-cannot-upgrade-its-proof-or-outlive-revocation
  (world/with-world [conn w]
    (with-assignment conn w
      (fn [{:keys [enrolled run proof]}]
        (let [transport (http/start! {:port 0 :bind "127.0.0.1" :profile :admin :toolsets #{:host}
                                      :authenticate #(credentials/authenticate conn %)})
              port (:port transport)
              initialize {:protocolVersion "2025-06-18" :capabilities {}
                          :clientInfo {:name "worker-http-fixture" :version "1"}}
              token (:credential enrolled)
              args {:run (str run) :nonce (str (random-uuid))}]
          (try
            (let [old (:sid (http-message port token nil nil 1 "initialize" initialize))]
              (is old)
              (http-message port token nil old nil "notifications/initialized" {})
              (is (refused? (:rpc (http-message port token proof old 2 "tools/call"
                                                {:name "simmis_worker-claim" :arguments args})))))
            (let [sid (:sid (http-message port token proof nil 1 "initialize" initialize))]
              (http-message port token proof sid nil "notifications/initialized" {})
              (let [claimed (value (:rpc (http-message port token proof sid 2 "tools/call"
                                                       {:name "simmis_worker-claim" :arguments args})))
                    lease-token (credentials/lease-token run (:lease claimed) proof)
                    lease-sid (:sid (http-message port lease-token nil nil 1 "initialize" initialize))]
                (http-message port lease-token nil lease-sid nil "notifications/initialized" {})
                (is (:base64 (value (:rpc (http-message port lease-token nil lease-sid 2 "tools/call"
                                                        {:name "simmis_worker-input" :arguments {:run (str run)}})))))
                (workers/revoke! conn (:worker enrolled))
                (is (refused? (:rpc (http-message port lease-token nil lease-sid 3 "tools/call"
                                                  {:name "simmis_worker-input" :arguments {:run (str run)}}))))
                (is (:cancel-requested? (value (:rpc (http-message port lease-token nil lease-sid 4 "tools/call"
                                                                   {:name "simmis_worker-heartbeat" :arguments {:run (str run)}})))))
                (is (= {:run (str run) :stop-ack? true}
                       (value (:rpc (http-message port lease-token nil lease-sid 5 "tools/call"
                                                  {:name "simmis_worker-stop" :arguments {:run (str run)}})))))))
            (finally ((:stop transport)))))))))

(deftest actual-http-accepts-the-full-bounded-output-with-a-small-receipt
  (world/with-world [conn w]
    (with-assignment conn w
      (fn [{:keys [enrolled run proof]}]
        (let [transport (http/start! {:port 0 :bind "127.0.0.1" :profile :admin :toolsets #{:host}
                                      :authenticate #(credentials/authenticate conn %)})
              port (:port transport)
              initialize {:protocolVersion "2025-06-18" :capabilities {}
                          :clientInfo {:name "worker-large-output-fixture" :version "1"}}
              token (:credential enrolled)
              args {:run (str run)}]
          (try
            (let [sid (:sid (http-message port token proof nil 1 "initialize" initialize))]
              (http-message port token proof sid nil "notifications/initialized" {})
              (let [claimed (value (:rpc (http-message port token proof sid 2 "tools/call"
                                                       {:name "simmis_worker-claim"
                                                        :arguments (assoc args :nonce (str (random-uuid)))})))
                    lease-token (credentials/lease-token run (:lease claimed) proof)
                    lease-sid (:sid (http-message port lease-token nil nil 1 "initialize" initialize))
                    file (file-fixture/encoded (apply str (repeat 1048576 "x")))
                    output {:files {"out/a" file "out/b" file} :summary "two MiB" :exit-code 0}]
                (http-message port lease-token nil lease-sid nil "notifications/initialized" {})
                (let [{:keys [status rpc]} (http-message port lease-token nil lease-sid 2 "tools/call"
                                                         {:name "simmis_worker-complete"
                                                          :arguments (assoc args :output output)})
                      receipt (value rpc)]
                  (is (= 200 status))
                  (is (= "completed" (:state receipt)))
                  (is (:stop-ack? receipt))
                  (is (< (count (json/write-value-as-string rpc)) 2048)))
                (is (= output (workers/payload conn (workers/assignment conn run) :simmis.worker.assignment/output-ref)))))
            (finally ((:stop transport)))))))))

(deftest room-revocation-between-claim-precheck-and-writer-refuses-ownership
  (world/with-world [conn w]
    (with-assignment conn w
      (fn [{:keys [run enrollment-session]}]
        (let [transact d/transact]
          (with-redefs [d/transact
                        (fn [connection tx]
                          (when (= @#'workers/claim-tx (get-in tx [0 1]))
                            (transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:owner w)]]
                                            [:db/add [:room/id (:room w)] :room/owner [:party/id (:non-member w)]]]))
                          (transact connection tx))]
            (is (refused? (call enrollment-session "simmis_worker-claim"
                                {:run (str run) :nonce (str (random-uuid))}))))
          (is (nil? (:simmis.worker.assignment/lease (workers/assignment conn run)))))))))
