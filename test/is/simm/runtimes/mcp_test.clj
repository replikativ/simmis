(ns is.simm.runtimes.mcp-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [dvergr.mcp.http :as http]
            [dvergr.mcp.json-rpc :as rpc]
            [dvergr.mcp.server :as server]
            [dvergr.mcp.surface :as surface]
            [dvergr.ops :as dops]
            [is.simm.ops.results :as results]
            [is.simm.ops.work-api :as work-api]
            [is.simm.ops.worker-api :as worker-api]
            [is.simm.ops.worker-control :as worker-control]
            [is.simm.model.parties :as parties]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.worker-credentials :as worker-credentials]
            [is.simm.runtimes.auth-config :as auth]
            [is.simm.runtimes.application-ops :as application-ops]
            [is.simm.runtimes.instance :as instance]
            [is.simm.runtimes.http-auth :as http-auth]
            [is.simm.runtimes.mcp :as mcp]
            [is.simm.runtimes.web :as web]
            [jsonista.core :as json]
            [reitit.ring :as ring]
            [kabel.auth.jwt :as jwt]))

(def person #uuid "33333333-3333-3333-3333-333333333333")

(defn installed-ops []
  (mapcat (comp keys deref) [#'results/specifications #'work-api/specifications
                             #'worker-api/specifications #'worker-control/specifications]))

(use-fixtures :each
  (fn [f]
    (with-redefs [auth/get-auth-config (constantly {:jwt {:secret "test-mcp-signing-key"}})
                  parties/get-party (fn [id] (when (= person id) {:party/id id :party/type :human}))]
      ;; Lifecycle tests own a listener cell. Running them in a development
      ;; REPL must neither reuse nor stop that process's real MCP listener.
      (with-redefs-fn {#'mcp/listener (atom nil)
                       #'application-ops/installed? (atom false)}
        (fn []
          (let [previous (dops/operations)
                previous-host @@#'server/embedded-host]
            (try
              (application-ops/start!)
              (f)
              (finally
                (application-ops/stop!)
                (doseq [op (installed-ops)]
                  (if-let [spec (get previous op)]
                    (dops/register-op! op spec)
                    (dops/unregister-op! op)))
                (server/sync-op-tools!)
                (server/set-host! previous-host)))))))))

(deftest only-mcp-credentials-for-an-existing-human-authenticate
  (let [authenticate (mcp/authenticator)
        token (:token (mcp/mint-token! person))
        principal (authenticate {:bearer token})]
    (is (= (str "party/" person) (:principal/id principal)))
    (is (= :human (:principal/kind principal)))
    (is (= mcp/read-tools (:principal/tools principal)))
    (is (nil? (authenticate {:bearer "wrong"})))
    (is (nil? (authenticate {})))
    (is (nil? (authenticate {:local-token token})))
    (with-redefs [parties/get-party (constantly nil)]
      (is (nil? (authenticate {:bearer token}))))
    (with-redefs [parties/get-party (constantly {:party/type :agent})]
      (is (nil? (authenticate {:bearer token}))))
    (is (thrown? clojure.lang.ExceptionInfo (mcp/mint-token! (random-uuid))))))

(deftest worker-control-is-a-new-explicit-ceiling-not-a-work-token-upgrade
  (let [authenticate (mcp/authenticator)
        control (authenticate {:bearer (:token (mcp/mint-token! person :worker-control))})
        read (authenticate {:bearer (:token (mcp/mint-token! person :read))})
        work (authenticate {:bearer (:token (mcp/mint-token! person :work))})]
    (is (= mcp/worker-control-tools (:principal/tools control)))
    (is (contains? (:principal/tools control) "simmis_local-worker-enroll"))
    (is (contains? (:principal/tools control) "simmis_local-worker-workflow"))
    (is (contains? (:principal/tools control) "simmis_local-worker-benchmark"))
    (is (contains? (:principal/tools control) "simmis_local-worker-list"))
    (doseq [principal [read work]]
      (is (not (contains? (:principal/tools principal) "simmis_local-worker-workflow")))
      (is (not (contains? (:principal/tools principal) "simmis_local-worker-benchmark")))
      (is (not (contains? (:principal/tools principal) "simmis_local-worker-list")))
      (is (not (contains? (:principal/tools principal) "simmis_local-worker-enroll"))))
    (doseq [name ["simmis_task" "simmis_workflow-write" "clojure_eval" "simmis_worker-claim"]]
      (is (not (contains? (:principal/tools control) name))))
    (let [now (quot (System/currentTimeMillis) 1000)
          base {:iss mcp/audience :aud mcp/audience :sub (str person) :iat now :exp (+ now 3600)}]
      (doseq [claims [{:capability-version 2 :mode "work" :tools ["simmis_local-worker-enroll"]}
                      {:capability-version 3 :mode "work" :tools ["simmis_local-worker-enroll"]}
                      {:capability-version 3 :mode "worker-control" :tools ["simmis_task"]}]]
        (is (nil? (authenticate {:bearer (jwt/sign-hs256 (#'mcp/signing-key) (merge base claims))})))))))

(deftest adding-workflow-controls-never-widens-an-existing-signed-token
  (let [new-tools mcp/worker-control-tools
        old-tools (disj new-tools "simmis_local-worker-workflow" "simmis_workflow-list"
                        "simmis_workflow-source" "simmis_workflow-check" "simmis_local-worker-benchmark"
                        "simmis_local-worker-list")
        token (with-redefs [mcp/worker-control-tools old-tools]
                (:token (mcp/mint-token! person :worker-control)))
        principal ((mcp/authenticator) {:bearer token})]
    (is (= old-tools (:principal/tools principal)))
    (is (not (contains? (:principal/tools principal) "simmis_local-worker-workflow")))
    (is (not (contains? (:principal/tools principal) "simmis_local-worker-benchmark")))
    (doseq [[principal expected]
            [[principal false]
             [((mcp/authenticator) {:bearer (:token (mcp/mint-token! person :worker-control))}) true]]]
      (let [ctx (server/session-context (fn [_]) (surface/selection {:profile :admin}) principal)]
        (rpc/handle-message ctx {:jsonrpc "2.0" :id 1 :method "initialize"
                                 :params {:protocolVersion "2025-06-18" :capabilities {}
                                          :clientInfo {:name "old-ceiling-regression" :version "1"}}})
        (rpc/handle-message ctx {:jsonrpc "2.0" :method "notifications/initialized"})
        (doseq [name ["simmis_local-worker-workflow" "simmis_local-worker-benchmark" "simmis_local-worker-list"]]
          (is (= expected
                 (contains? (set (map :name (get-in (rpc/handle-message ctx
                                                                        {:jsonrpc "2.0" :id 2 :method "tools/list"})
                                                    [:result :tools]))) name)))
          (when-not expected
            (let [response (rpc/handle-message ctx {:jsonrpc "2.0" :id 3 :method "tools/call"
                                                    :params {:name name :arguments {}}})]
              (is (or (:error response) (get-in response [:result :isError]))))))))))

(deftest worker-prefix-authentication-never-falls-through-to-human-authority
  (let [human-calls (atom 0) worker-calls (atom [])]
    (with-redefs-fn {#'mcp/human-authenticator (fn [] (fn [_] (swap! human-calls inc) ::human))
                     #'system-db/get-conn (constantly ::connection)
                     #'worker-credentials/authenticate
                     (fn [conn request] (swap! worker-calls conj [conn request]) nil)}
      (fn []
        (let [authenticate (mcp/authenticator)]
          (doseq [bearer ["simmis-worker.invalid" "simmis-lease.revoked.or.expired"]]
            (is (nil? (authenticate {:bearer bearer}))))
          (is (zero? @human-calls))
          (is (= 2 (count @worker-calls)))
          (with-redefs [worker-credentials/authenticate (fn [& _] (throw (Exception. "unavailable")))]
            (is (nil? (authenticate {:bearer "simmis-lease.unavailable"}))))
          (is (zero? @human-calls))
          (is (= ::human (authenticate {:bearer "ordinary-human-format"})))
          (is (= 1 @human-calls)))))))

(deftest mcp-and-web-tokens-cannot-be-replayed-across-surfaces
  (let [key (#'mcp/signing-key)
        now (quot (System/currentTimeMillis) 1000)
        claims {:iss mcp/audience :aud mcp/audience :sub (str person) :iat now
                :exp (+ now 3600)}
        authenticate (mcp/authenticator)
        call #(authenticate {:bearer %})]
    (doseq [c [(assoc claims :iss "simmis")
               (assoc claims :aud "simmis-api")
               (dissoc claims :aud)
               (dissoc claims :exp)
               (dissoc claims :iat)
               (assoc claims :exp (+ now 7200))
               (assoc claims :aud [mcp/audience "simmis-api"])
               (assoc claims :exp (- now 60))
               (assoc claims :sub "not-a-uuid")]]
      (is (nil? (call (jwt/sign-hs256 key c)))))
    (is (nil? (call (jwt/sign-hs256 "test-mcp-signing-key" claims)))
        "even matching claims signed with the web key are refused")
    (let [validate-web (jwt/build-bearer-validator
                        {:alg :HS256 :secret "test-mcp-signing-key"
                         :required-claims {:iss "simmis" :aud "simmis-api"}})]
      (is (nil? (validate-web {:headers {"authorization" (str "Bearer " (:token (mcp/mint-token! person)))}}))))))

(deftest a-client-cannot-widen-the-read-only-surface
  (let [principal ((mcp/authenticator) {:bearer (:token (mcp/mint-token! person))})
        ctx (server/session-context (fn [_]) (surface/selection {:profile :readonly}) principal)]
    (rpc/handle-message ctx {:jsonrpc "2.0" :id 1 :method "initialize"
                             :params {:protocolVersion "2025-06-18" :capabilities {}
                                      :clientInfo {:name "test" :version "1"}
                                      :_meta {:dvergr/profile "admin"}}})
    (rpc/handle-message ctx {:jsonrpc "2.0" :method "notifications/initialized"})
    (let [listed (rpc/handle-message ctx {:jsonrpc "2.0" :id 2 :method "tools/list" :params {}})]
      (is (= mcp/read-tools (set (map :name (get-in listed [:result :tools]))))))
    (let [response (rpc/handle-message ctx {:jsonrpc "2.0" :id 3 :method "tools/call"
                                            :params {:name "clojure_eval"
                                                     :arguments {:room "anything" :code "(+ 1 2)"}}})]
      (is (or (:error response) (get-in response [:result :isError]))))))

(deftest signed-capabilities-do-not-upgrade-legacy-or-restricted-credentials
  (let [now (quot (System/currentTimeMillis) 1000)
        base {:iss mcp/audience :aud mcp/audience :sub (str person)
              :iat now :exp (+ now 3600)}
        authenticate (mcp/authenticator)
        call #(authenticate {:bearer (jwt/sign-hs256 (#'mcp/signing-key) %)})]
    (is (= mcp/legacy-read-tools (:principal/tools (call base))))
    (is (= #{"room_read"} (:principal/tools
                           (call (assoc base :capability-version 1 :tools ["room_read"]))))
        "a credential stays restricted even if the server adds more tools")
    (doseq [caps [{:capability-version 2 :tools ["room_read"]}
                  {:capability-version 1 :tools ["room_post"]}
                  {:capability-version 1 :tools []}
                  {:capability-version 1 :tools "room_read"}
                  {:tools ["room_read"]} {:capability-version 1}]]
      (is (nil? (call (merge base caps)))))
    (let [principal (call base)
          definitions (server/resource-definitions principal)]
      (is (not-any? #(= "room_file" (:name %)) (:templates definitions))
          "legacy tokens gain neither new tools nor file resources"))))

(deftest work-is-an-explicit-signed-ceiling-not-an-admin-profile-escalation
  (let [authenticate (mcp/authenticator)
        read (authenticate {:bearer (:token (mcp/mint-token! person))})
        work (authenticate {:bearer (:token (mcp/mint-token! person :work))})]
    (is (= #{:dvergr/read-only} (:principal/scopes read)))
    (is (= #{} (:principal/scopes work)))
    (is (= mcp/work-tools (:principal/tools work)))
    (is (not (contains? (:principal/tools read) "simmis_task")))
    (is (not (contains? (:principal/tools read) "simmis_coordinator")))
    (doseq [[p expected] [[read mcp/read-tools] [work mcp/work-tools]]]
      (let [ctx (server/session-context (fn [_]) (surface/selection {:profile :admin}) p)]
        (rpc/handle-message ctx {:jsonrpc "2.0" :id 1 :method "initialize"
                                 :params {:protocolVersion "2025-06-18" :capabilities {}
                                          :clientInfo {:name "spoofed-admin" :version "1"}
                                          :_meta {:dvergr/profile "admin"}}})
        (rpc/handle-message ctx {:jsonrpc "2.0" :method "notifications/initialized"})
        (is (= expected (set (map :name (get-in (rpc/handle-message ctx
                                                                    {:jsonrpc "2.0" :id 2 :method "tools/list"})
                                                [:result :tools])))))
        (doseq [tool ["room_write" "room_post" "clojure_eval" "catalog_benchmark" "attempt_export"]]
          (let [r (rpc/handle-message ctx {:jsonrpc "2.0" :id 3 :method "tools/call"
                                           :params {:name tool :arguments {}}})]
            (is (or (:error r) (get-in r [:result :isError])))))))
    (is (thrown? clojure.lang.ExceptionInfo (mcp/mint-token! person :admin)))))

(deftest new-controls-do-not-upgrade-an-existing-work-token
  (let [old-tools (disj mcp/work-tools "simmis_cli-benchmark" "simmis_cli-allowance" "simmis_coordinator")
        token (with-redefs [mcp/work-tools old-tools] (:token (mcp/mint-token! person :work)))
        old ((mcp/authenticator) {:bearer token})
        fresh ((mcp/authenticator) {:bearer (:token (mcp/mint-token! person :work))})]
    (is (= old-tools (:principal/tools old)))
    (doseq [tool ["simmis_cli-benchmark" "simmis_cli-allowance" "simmis_coordinator"]]
      (is (not (contains? (:principal/tools old) tool)))
      (is (contains? (:principal/tools fresh) tool)))))

(deftest listener-is-opt-in-private-idempotent-and-transport-only
  (let [events (atom [])
        host @@#'server/embedded-host
        operations (dops/operations)]
    (with-redefs [mcp/configured-port (constantly nil)
                  server/set-host! #(swap! events conj [:host (some? %)])
                  http/start! (fn [opts]
                                (swap! events conj [:start (select-keys opts [:port :bind :profile])])
                                {:port (:port opts) :token "must-not-be-retained"
                                 :stop #(swap! events conj [:stop])})]
      (try
        (mcp/start!)
        (is (empty? @events))
        (with-redefs [mcp/configured-port (constantly 17889)]
          (mcp/start!)
          (mcp/start!))
        (is (= [[:start {:port 17889 :bind "127.0.0.1" :profile :admin}]] @events))
        (is (nil? (:token @@#'mcp/listener)))
        (mcp/stop!)
        (mcp/stop!)
        (is (= operations (dops/operations)))
        (is (identical? host @@#'server/embedded-host))
        (is (= [:stop] (last @events)))
        (finally (mcp/stop!))))))

(deftest failed-listener-start-preserves-application-operations-and-can-retry
  (let [hosts (atom [])
        host @@#'server/embedded-host
        operations (dops/operations)]
    (with-redefs [mcp/configured-port (constantly 17889)
                  server/set-host! #(swap! hosts conj %)
                  http/start! (fn [_] (throw (ex-info "occupied" {})))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"occupied" (mcp/start!)))
      (is (empty? @hosts))
      (is (nil? @@#'mcp/listener))
      (is (identical? host @@#'server/embedded-host))
      (is (= operations (dops/operations)))
      (with-redefs [http/start! (fn [_] {:port 17889 :stop (fn [])})]
        (try (mcp/start!)
             (is (= 17889 (:port @@#'mcp/listener)))
             (finally (mcp/stop!)))))))

(deftest application-operations-exist-without-mcp-and-stop-only-with-application
  (with-redefs [mcp/configured-port (constantly nil)]
    (mcp/start!)
    (mcp/stop!)
    (doseq [op [:simmis/workflow-list :simmis/workflow-source :simmis/workflow-write
                :simmis/attempt-files :simmis/attempt-file]]
      (is (contains? (dops/operations) op)))
    (let [host @@#'server/embedded-host]
      (application-ops/start!)
      (is (identical? host @@#'server/embedded-host) "application start is idempotent"))
    (application-ops/stop!)
    (application-ops/stop!)
    (is (nil? @@#'server/embedded-host))
    (is (not-any? #(contains? (dops/operations) %)
                  (installed-ops)))))

(deftest failed-application-registration-rolls-back-and-can-retry
  (application-ops/stop!)
  (with-redefs [work-api/install! (fn [] (throw (ex-info "registration failed" {})))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"registration failed"
                          (application-ops/start!))))
  (is (nil? @@#'server/embedded-host))
  (is (not-any? #(contains? (dops/operations) %)
                (installed-ops)))
  (application-ops/start!)
  (is (contains? (dops/operations) :simmis/workflow-list)))

(deftest web-login-exchanges-only-its-own-human-identity
  (let [other #uuid "44444444-4444-4444-4444-444444444444"
        handler (ring/ring-handler
                 (ring/router [["/auth" {:auth :public} (mcp/token-route)]]
                              {:validate http-auth/validate-auth-declared!
                               :data {:middleware [http-auth/auth-middleware]}}))
        request {:request-method :post :uri "/auth/mcp-token"
                 :body {:party-id other :sub (str other) :profile "admin"}}]
    (with-redefs [mcp/configured-port (constantly 47889)]
      (with-redefs [http-auth/party-id (constantly nil)]
        (is (= 401 (:status (handler request)))))
      (with-redefs [http-auth/party-id (constantly person)]
        (is (= 400 (:status (handler request))) "caller identity/profile fields are not accepted")
        (let [response (handler (assoc request :body {}))
              principal ((mcp/authenticator) {:bearer (get-in response [:body :token])})]
          (is (= 200 (:status response)))
          (is (= "no-store" (get-in response [:headers "Cache-Control"])))
          (is (= (str "party/" person) (:principal/id principal)))
          (is (= mcp/read-tools (:principal/tools principal))))
        (with-redefs [mcp/configured-port (constantly nil)]
          (is (= 404 (:status (handler request)))))
        (with-redefs [parties/get-party (constantly {:party/type :agent})]
          (is (= 403 (:status (handler request)))))
        (with-redefs [parties/get-party (constantly nil)]
          (is (= 403 (:status (handler request)))))))))

(deftest credential-exchange-uses-the-real-web-validator
  (let [other #uuid "44444444-4444-4444-4444-444444444444"
        key "web-exchange-test"
        now (quot (System/currentTimeMillis) 1000)
        validator (jwt/build-bearer-validator
                   {:alg :HS256 :secret key :leeway-seconds 0
                    :required-claims {:iss "simmis" :aud "simmis-api"}})
        claims {:iss "simmis" :aud "simmis-api" :sub (str other)
                :iat now :exp (+ now 60)}
        handler (ring/ring-handler
                 (ring/router [["/auth" {:auth :public} (mcp/token-route)]]
                              {:data {:middleware [http-auth/auth-middleware]}}))
        request (fn [token]
                  (handler {:request-method :post :uri "/auth/mcp-token"
                            :headers {"authorization" (str "Bearer " token)}
                            :body {}}))]
    (with-redefs [http-auth/validator (delay validator)
                  mcp/configured-port (constantly 47889)
                  parties/get-party (fn [id] (when (#{person other} id)
                                               {:party/id id :party/type :human}))]
      (let [response (request (jwt/sign-hs256 key claims))
            principal ((mcp/authenticator) {:bearer (get-in response [:body :token])})]
        (is (= 200 (:status response)))
        (is (= (str "party/" other) (:principal/id principal))))
      (doseq [token ["wrong" (:token (mcp/mint-token! person))
                     (jwt/sign-hs256 key (assoc claims :exp (- now 60)))
                     (jwt/sign-hs256 key (assoc claims :sub "invalid"))]]
        (is (= 401 (:status (request token))))))))

(deftest web-composite-handler-mounts-and-encodes-the-exchange
  (with-redefs [auth/auth-route-data (constantly [])
                mcp/configured-port (constantly 47889)
                http-auth/party-id (constantly person)]
    (let [handler (web/wrap-composite-handler (fn [_] {:status 599}))
          response (handler {:request-method :post :uri "/auth/mcp-token" :headers {}})
          body (json/read-value (:body response) json/keyword-keys-object-mapper)
          principal ((mcp/authenticator) {:bearer (:token body)})]
      (is (= 200 (:status response)))
      (is (= "no-store" (get-in response [:headers "Cache-Control"])))
      (is (= "Bearer" (:token-type body)))
      (is (= (str "party/" person) (:principal/id principal)))
      (with-redefs [http-auth/party-id (constantly nil)]
        (is (= 401 (:status (handler {:request-method :post :uri "/auth/mcp-token"
                                      :headers {}}))))))))
