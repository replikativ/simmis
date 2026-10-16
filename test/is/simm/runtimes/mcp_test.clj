(ns is.simm.runtimes.mcp-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [dvergr.mcp.http :as http]
            [dvergr.mcp.json-rpc :as rpc]
            [dvergr.mcp.server :as server]
            [dvergr.mcp.surface :as surface]
            [is.simm.model.parties :as parties]
            [is.simm.runtimes.auth-config :as auth]
            [is.simm.runtimes.instance :as instance]
            [is.simm.runtimes.mcp :as mcp]
            [kabel.auth.jwt :as jwt]))

(def person #uuid "33333333-3333-3333-3333-333333333333")

(use-fixtures :each
  (fn [f]
    (with-redefs [auth/get-auth-config (constantly {:jwt {:secret "test-mcp-signing-key"}})
                  parties/get-party (fn [id] (when (= person id) {:party/id id :party/type :human}))]
      (f))))

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

(deftest listener-is-opt-in-private-idempotent-and-cleans-up
  (let [events (atom [])]
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
        (is (= [[:host true] [:start {:port 17889 :bind "127.0.0.1" :profile :readonly}]] @events))
        (is (nil? (:token @@#'mcp/listener)))
        (mcp/stop!)
        (mcp/stop!)
        (is (= [[:stop] [:host false]] (take-last 2 @events)))
        (finally (mcp/stop!))))))

(deftest failed-start-clears-the-host
  (let [hosts (atom [])]
    (with-redefs [mcp/configured-port (constantly 17889)
                  server/set-host! #(swap! hosts conj %)
                  http/start! (fn [_] (throw (ex-info "occupied" {})))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"occupied" (mcp/start!)))
      (is (some? (first @hosts)))
      (is (nil? (last @hosts))))))
