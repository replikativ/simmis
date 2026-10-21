(ns is.simm.authority.mcp-results-test
  "The real MCP protocol, room authorizer and existing result/file operations.
   No model calls, no second result store, no bypass via resource templates."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as dh]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.jobs :as jobs]
            [dvergr.mcp.json-rpc :as rpc]
            [dvergr.mcp.server :as server]
            [dvergr.mcp.surface :as surface]
            [dvergr.room.store :as store]
            [dvergr.room.registry :as registry]
            [dvergr.room.store.memory :as memory]
            [dvergr.rooms.files :as files]
            [is.simm.authority.world :as world]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.experiments-test]
            [is.simm.ops.results :as results]
            [is.simm.ops.work-api :as work-api]
            [is.simm.runtimes.auth-config :as auth]
            [is.simm.runtimes.mcp :as mcp]
            [kabel.auth.jwt :as jwt]
            [jsonista.core :as json]
            [org.replikativ.spindel.core :as sp]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]))

(def ^:private with-experiment-room
  @#'is.simm.ops.experiments-test/with-experiment-room)

(defn- session [party-id & [ceiling]]
  (let [now (quot (System/currentTimeMillis) 1000)
        token (if ceiling
                (jwt/sign-hs256 (#'mcp/signing-key)
                                (cond-> {:iss mcp/audience :aud mcp/audience :sub (str party-id)
                                         :iat now :exp (+ now 3600)}
                                  (not= :legacy ceiling) (assoc :capability-version 1 :tools (vec ceiling))))
                (:token (mcp/mint-token! party-id)))
        p ((mcp/authenticator) {:bearer token})
        context (server/session-context (fn [_])
                                        (surface/selection {:profile :readonly :toolsets #{:host}}) p)]
    (rpc/handle-message context
                        {:jsonrpc "2.0" :id 1 :method "initialize"
                         :params {:protocolVersion "2025-06-18" :capabilities {}
                                  :clientInfo {:name "simmis-results-test" :version "1"}}})
    (rpc/handle-message context {:jsonrpc "2.0" :method "notifications/initialized"})
    context))

(defn- call [context tool args]
  (rpc/handle-message context
                      {:jsonrpc "2.0" :id 2 :method "tools/call" :params {:name tool :arguments args}}))

(defn- result [response]
  (is (not (:error response)))
  (is (not (get-in response [:result :isError])))
  (json/read-value (get-in response [:result :content 0 :text])
                   json/keyword-keys-object-mapper))

(defn- refused? [response]
  (boolean (or (:error response) (get-in response [:result :isError]))))

(deftest room-results-and-files-share-the-existing-authority-boundary
  (world/with-world [conn w]
    (with-experiment-room
      (fn [room]
        (let [slug (store/room-id->slug (:id room))]
          (dh/transact conn [{:room/id (:room w) :room/slug slug}])
          (workspace/ensure-workspace! room)
          (workspace/seed! room {"/out/result.md" "current workspace, not historical evidence"})
          (server/set-host! (ops/host (:ctx room)))
          (try
            (results/install!)
            (work-api/install!)
            (with-redefs [auth/get-auth-config (constantly {:jwt {:secret "mcp-results-test-key"}})]
              (let [member (session (:member w))
                    stranger (session (:non-member w))
                    admin (session (:admin w))
                    args {:room slug}
                    attempts (result (call member "attempt_list" args))
                    attempt (first attempts)
                    detail-args (assoc args :id (:id attempt))
                    before (result (call member "attempt_detail" detail-args))]
                (is (= 16 (count attempts)))
                (is (contains? before :evidence))
                (is (= "captured attempt output"
                       (get-in before [:evidence :files (keyword "/out/result.md")])))
                (is (= (:id attempt) (:id before)))
                (testing "canonical room UUIDs resolve at both authorization gates"
                  (let [uuid-args (assoc detail-args :room (str (:room w)))]
                    (doseq [[tool input] [["simmis_attempt-files" uuid-args]
                                          ["simmis_attempt-file" (assoc uuid-args :path "/out/result.md")]]]
                      (is (= (result (call member tool (assoc input :room slug)))
                             (result (call member tool input))))
                      (doseq [context [stranger admin]]
                        (is (refused? (call context tool input)))))))
                (testing "unknown UUIDs never fall back to UUID-shaped slugs"
                  (let [unknown (str (random-uuid))]
                    (dh/transact conn [{:room/id (:room w) :room/slug unknown}])
                    (try
                      (doseq [handle [unknown (str (random-uuid))]
                              [tool input] [["simmis_attempt-files" detail-args]
                                            ["simmis_attempt-file" (assoc detail-args :path "/out/result.md")]]]
                        (is (refused? (call member tool (assoc input :room handle)))))
                      (finally (dh/transact conn [{:room/id (:room w) :room/slug slug}])))))
                (testing "discovery exactly matches each credential's ceiling"
                  (doseq [[context ceiling] [[member mcp/read-tools]
                                             [(session (:member w) :legacy) mcp/legacy-read-tools]
                                             [(session (:member w) #{"room_list"}) #{"room_list"}]]]
                    (is (= ceiling
                           (set (map :name (get-in (rpc/handle-message context
                                                                       {:jsonrpc "2.0" :id 5 :method "tools/list" :params {}})
                                                   [:result :tools])))))))
                (testing "recorded Attempt evidence does not follow mutable workspace changes"
                  (files/write! room {"/out/result.md" {:text "changed afterwards"}} {})
                  (is (= before (result (call member "attempt_detail" detail-args))))
                  (let [manifest (result (call member "simmis_attempt-files" detail-args))
                        chunk (result (call member "simmis_attempt-file"
                                            (assoc detail-args :path "/out/result.md")))]
                    (is (= ["/out/result.md"] (mapv :path (:files manifest))))
                    (is (= "captured attempt output"
                           (String. (.decode (java.util.Base64/getDecoder) ^String (:base64 chunk))
                                    java.nio.charset.StandardCharsets/UTF_8)))
                    (is (= (:content-id before) (get-in chunk [:attempt :content-id])))))
                (testing "workspace files and resources are independently useful"
                  (is (= "changed afterwards"
                         (:text (result (call member "room_read"
                                              (assoc args :path "/out/result.md"))))))
                  (is (seq (:entries (result (call member "room_files" args)))))
                  (let [resource {:jsonrpc "2.0" :id 3 :method "resources/read"
                                  :params {:uri (str "room://" slug "/files/out/result.md")}}]
                    (is (not (:error (rpc/handle-message member resource))))
                    (is (:error (rpc/handle-message stranger resource)))
                    (doseq [restricted [(session (:member w) :legacy)
                                        (session (:member w) #{"room_list"})]]
                      (is (:error (rpc/handle-message restricted resource))))))
                (testing "existing scorecards and progress remain the canonical result model"
                  (is (= 1 (count (result (call member "scorecard_list" args)))))
                  (is (map? (result (call member "experiment_progress" args))))
                  (is (map? (result (call member "experiment_report" args)))))
                (testing "non-members, including installation admins, cannot read this room"
                  (doseq [context [stranger admin]
                          [tool input] [["room_files" args]
                                        ["room_read" (assoc args :path "/out/result.md")]
                                        ["attempt_list" args] ["attempt_detail" detail-args]
                                        ["simmis_attempt-files" detail-args]
                                        ["simmis_attempt-file" (assoc detail-args :path "/out/result.md")]
                                        ["scorecard_list" args] ["experiment_report" args]]]
                    (is (refused? (call context tool input)))))
                (testing "advertised limits are enforced, not only documented"
                  (doseq [[tool input] [["room_read" (assoc args :path "/out/result.md" :length 1000000000)]
                                        ["room_files" (assoc args :offset -1)]
                                        ["attempt_list" (assoc args :limit 1000000000)]]]
                    (is (refused? (call member tool input)))))
                (testing "host writes and unrestricted trajectory export remain unavailable"
                  (doseq [tool ["room_write" "room_post" "attempt_export" "catalog_benchmark" "clojure_eval"]]
                    (is (refused? (call member tool args)))))
                (testing "revoking membership affects an already-authenticated MCP session"
                  (dh/transact conn [[:db/retract [:room/id (:room w)] :room/parties
                                      [:party/id (:member w)]]])
                  (is (refused? (call member "attempt_detail" detail-args)))
                  (is (:error (rpc/handle-message member
                                                  {:jsonrpc "2.0" :id 4 :method "resources/read"
                                                   :params {:uri (str "room://" slug "/files/out/result.md")}}))))))
            (finally (work-api/uninstall!) (results/uninstall!) (server/set-host! nil))))))))

(deftest uuid-shaped-slugs-cannot-impersonate-canonical-room-identities
  (world/with-world [conn w]
    (let [unknown (str (random-uuid))
          make-room discourse/make-room]
      (with-redefs [discourse/make-room
                    (fn [opts]
                      (make-room (cond-> opts
                                   (= :simmis-experiments-test (:id opts))
                                   (assoc :id (keyword unknown)))))]
        (with-experiment-room
          (fn [room]
            (is (= unknown (store/room-id->slug (:id room))))
            (dh/transact conn [{:room/id (:room w) :room/slug unknown}])
            (server/set-host! (ops/host (:ctx room)))
            (try
              (results/install!)
              (with-redefs [auth/get-auth-config (constantly {:jwt {:secret "mcp-results-collision-key"}})]
                (let [member (session (:member w))
                      attempt (first (result (call member "attempt_list" {:room unknown})))
                      args {:room (str (:room w)) :id (:id attempt)}]
                  (doseq [[tool input] [["simmis_attempt-files" args]
                                        ["simmis_attempt-file" (assoc args :path "/out/result.md")]]]
                    (is (map? (result (call member tool input))))
                    (is (refused? (call member tool (assoc input :room unknown)))))))
              (finally (results/uninstall!) (server/set-host! nil)))))))))

(deftest job-discovery-and-known-job-ids-are-room-authorized
  (world/with-world [conn w]
    (with-experiment-room
      (fn [room]
        (let [private (discourse/make-room {:id :private-results-job-room
                                            :ctx (context/fork-context (:ctx room))
                                            :store (memory/make)})
              slug (store/room-id->slug (:id room))]
          (try
            (dh/transact conn [{:room/id (:room w) :room/slug slug}
                               {:room/id (:other-room w) :room/slug "private-results-job-room"}])
            (binding [ec/*execution-context* (:ctx room)] (registry/register! private))
            (let [own-job (jobs/start! room {:kind :workflow} (fn [_] (sp/spin :done)))
                  private-job (jobs/start! private {:kind :workflow} (fn [_] (sp/spin :done)))]
              (server/set-host! (ops/host (:ctx room)))
              (with-redefs [auth/get-auth-config (constantly {:jwt {:secret "mcp-jobs-test-key"}})]
                (let [member (session (:member w))
                      visible (result (call member "job_list" {}))]
                  (is (some #(= (str (:id own-job)) (:id %)) visible))
                  (is (not-any? #(= (str (:id private-job)) (:id %)) visible))
                  (is (= (str (:id own-job)) (:id (result (call member "job_status"
                                                                {:job (str (:id own-job))})))))
                  (is (refused? (call member "job_status" {:job (str (:id private-job))}))))))
            (finally
              (server/set-host! nil)
              (binding [ec/*execution-context* (:ctx room)] (registry/unregister! (:id private)))
              (discourse/close-room! private))))))))
