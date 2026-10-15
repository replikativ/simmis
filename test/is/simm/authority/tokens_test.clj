(ns is.simm.authority.tokens-test
  "Principals come from credentials, so the credential check is part of the
   authority surface: an access token must name the surface it is for (plan
   §2.5 — `aud=<origin>/ws` for the web app, `aud=<origin>/mcp` for MCP) and be
   refused anywhere else.

   Validated with the very validator the websocket handshake and the HTTP
   bearer path build (`auth-config/jwt-config` → kabel's
   `build-bearer-validator`), signed with the real key; nothing is stubbed."
  (:require [clojure.test :refer [deftest is testing]]
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
    (is (= (str a-party) (str (:sub (ws-principal (token {:aud auth-cfg/web-audience})))))))
  (testing "the issuer mints exactly the audience the validator requires, so logins keep working"
    (let [{:keys [audience required-claims]} (#'auth-cfg/jwt-config)]
      (is (= auth-cfg/web-audience audience (:aud required-claims)))))
  (testing "a token signed with another key is refused, whatever it claims"
    (is (nil? (ws-principal (jwt/sign-hs256 (.getBytes "not-the-key-not-the-key-not-the-key")
                                            {:iss "simmis" :aud auth-cfg/web-audience :sub (str a-party)
                                             :iat (now) :exp (+ (now) 3600)})))))
  (testing "an expired token is refused"
    (is (nil? (ws-principal (token {:aud auth-cfg/web-audience :iat (- (now) 7200) :exp (- (now) 3600)})))))
  (testing "§1/§2.5 closed: a token minted for another surface (MCP) is refused"
    (is (nil? (ws-principal (token {:aud "https://simm.is/mcp"})))))
  (testing "§1/§2.5 closed: a token naming no audience is refused"
    (is (nil? (ws-principal (token {})))))
  (testing "an audience list is not the audience"
    (is (nil? (ws-principal (token {:aud [auth-cfg/web-audience "https://simm.is/mcp"]}))))))

(deftest blob-and-access-tokens-do-not-cross
  (let [blob (:token (#'auth-cfg/mint-blob-token a-party))]
    (testing "legitimate: the blob-read token reads blobs"
      (is (= (str a-party) (str (:sub (auth-cfg/blob-read-claims blob))))))
    (testing "a blob-read token does not open the socket"
      (is (nil? (ws-principal blob))))
    (testing "an access token is not a blob-read token"
      (is (nil? (auth-cfg/blob-read-claims (token {:aud auth-cfg/web-audience})))))))
