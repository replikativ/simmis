(ns is.simm.authority.tokens-test
  "Principals come from credentials, so the credential check is part of the
   authority surface: an access token must name the surface it is for (plan
   §2.5 — `aud=<origin>/ws` for the web app, `aud=<origin>/mcp` for MCP) and be
   refused anywhere else.

   Validated with the very validator the websocket handshake and the HTTP
   bearer path build (`auth-config/jwt-config` → kabel's
   `build-bearer-validator`), signed with the real key; nothing is stubbed."
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.authority.known-hole :refer [known-hole]]
            [is.simm.runtimes.auth-config :as auth-cfg]
            [kabel.auth.jwt :as jwt]))

(def ^:private a-party #uuid "cafe0000-0000-0000-0000-00000000a0d1")

(defn- now [] (quot (System/currentTimeMillis) 1000))

(defn- token [claims]
  (jwt/sign-hs256 @#'auth-cfg/jwt-secret
                  (merge {:iss "simmis" :sub (str a-party) :iat (now) :exp (+ (now) 3600)}
                         claims)))

(defn- ws-principal
  "What the websocket/HTTP access-token validator makes of `t`: the claims, or
   nil when refused."
  [t]
  ((jwt/build-bearer-validator (#'auth-cfg/jwt-config))
   {:headers {"authorization" (str "Bearer " t)}}))

(deftest an-access-token-is-for-one-audience
  (testing "legitimate: a token for this surface's audience is accepted"
    (is (= (str a-party) (str (:sub (ws-principal (token {:aud "simmis-api"})))))))
  (testing "a token signed with another key is refused, whatever it claims"
    (is (nil? (ws-principal (jwt/sign-hs256 (.getBytes "not-the-key-not-the-key-not-the-key")
                                            {:iss "simmis" :aud "simmis-api" :sub (str a-party)
                                             :iat (now) :exp (+ (now) 3600)})))))
  (testing "an expired token is refused"
    (is (nil? (ws-principal (token {:aud "simmis-api" :iat (- (now) 7200) :exp (- (now) 3600)})))))
  (known-hole
   "§1/§2.5 no audience check"
   "the access-token validator checks no audience: a token minted for another surface (here aud=<origin>/mcp) opens the websocket and every RPC"
   (some? (ws-principal (token {:aud "https://simm.is/mcp"}))))
  (known-hole
   "§1/§2.5 no audience check (absent aud)"
   "a token with no audience at all is accepted"
   (some? (ws-principal (token {})))))
