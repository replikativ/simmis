(ns is.simm.runtimes.http-policy-test
  "The HTTP plane's deny-by-default gate.

   The RPC plane refuses an unpoliced fn; the HTTP plane had no equivalent and
   grew route by route with an ad-hoc `require-auth` inside each handler. Two
   routes got through that way — `GET /blobs/:id` and `/apps/<slug>/`, both
   serving files to anyone who could reach the port.

   The guarantee is now at BOOT: `validate-auth-declared!` is reitit's
   `:validate` hook, so a route with no `:auth` throws when the router is
   CONSTRUCTED and the server never starts. That is stronger than a test —
   a test can be skipped, a boot failure cannot — so what is asserted here is
   the MECHANISM: that the validator rejects what it should, accepts what it
   should, and that the middleware enforces each declared form."
  (:require [clojure.test :refer [deftest is testing]]
            [reitit.ring :as ring]
            [is.simm.model.access :as access]
            [is.simm.runtimes.auth-config :as auth-cfg]
            [is.simm.runtimes.http-auth :as hauth]
            [kabel.auth.jwt :as jwt]))

(defn- build
  "Construct a router the way `web/make-handler` does. Returns :built, or the
   offending route list from the validator's ex-data."
  [routes]
  (try (ring/router routes {:validate hauth/validate-auth-declared!
                            :data {:middleware [hauth/auth-middleware]}})
       :built
       (catch Exception e (or (:routes (ex-data e)) (.getMessage e)))))

(def ^:private ok (fn [_] {:status 200 :body "ok"}))

;; =============================================================================
;; Boot-time validation
;; =============================================================================

(deftest a-route-without-auth-stops-the-boot
  (testing "the omission that produced both live findings"
    (is (= ["GET /oops"] (build [["/oops" {:get {:handler ok}}]]))))

  (testing "and it names the offender rather than just failing"
    ;; A vector where most routes are fine and one is not is the realistic
    ;; case — a new route added beside existing ones.
    (is (= ["POST /b"]
           (build [["/a" {:get {:handler ok :auth :public}}]
                   ["/b" {:post {:handler ok}}]]))))

  (testing "a value outside the vocabulary is not a declaration"
    ;; `:auth :yes` reads like a decision and is not one; accepting it would
    ;; make the gate's `:else` branch reachable in production.
    (is (= ["GET /bad"] (build [["/bad" {:get {:handler ok :auth :sure}}]])))))

(deftest every-valid-form-builds
  (testing ":public"
    (is (= :built (build [["/x" {:get {:handler ok :auth :public}}]]))))
  (testing ":authenticated"
    (is (= :built (build [["/x" {:get {:handler ok :auth :authenticated}}]]))))
  (testing "a resource form"
    (is (= :built (build [["/x" {:get {:handler ok
                                       :auth {:action :read
                                              :resource (fn [_] {:room 1})}}}]]))))
  (testing "a resource form MISSING its resolver is not valid"
    (is (vector? (build [["/x" {:get {:handler ok :auth {:action :read}}}]])))))

(deftest a-parent-may-declare-for-its-subtree
  (testing "how /auth/* is declared once for routes a library supplies"
    (is (= :built (build [["/auth" {:auth :public}
                           ["/login"   {:post {:handler ok}}]
                           ["/refresh" {:post {:handler ok}}]]])))))

;; =============================================================================
;; Enforcement
;; =============================================================================

(defn- app
  "A one-route app under the gate, with identity and `can?` stubbed so the
   decision is observable without a system DB or a real token."
  [auth & {:keys [party can] :or {party nil can false}}]
  (let [handler (ring/ring-handler
                  (ring/router [["/r" {:get {:handler ok :auth auth}}]]
                               {:data {:middleware [hauth/auth-middleware]}}))]
    (with-redefs [hauth/party-id (fn [_] party)
                  access/can? (fn [& _] can)]
      (:status (handler {:request-method :get :uri "/r"})))))

(deftest public-needs-nothing
  (is (= 200 (app :public))))

(deftest authenticated-needs-a-principal
  (testing "no principal"
    (is (= 401 (app :authenticated))))
  (testing "a principal, and nothing more is asked"
    (is (= 200 (app :authenticated :party (random-uuid) :can false))
        "`:authenticated` must NOT consult can? — the handler scopes itself")))

(deftest a-resource-form-asks-can?
  (let [a {:action :read :resource (fn [_] {:room (random-uuid)})}]
    (testing "unauthenticated is refused before can? is reached"
      (is (= 401 (app a :can true))))
    (testing "authenticated but not permitted"
      (is (= 403 (app a :party (random-uuid) :can false))))
    (testing "authenticated and permitted"
      (is (= 200 (app a :party (random-uuid) :can true))))))

(deftest a-throwing-resolver-denies
  (testing "deny-by-default has to survive a BROKEN check, not just a false one"
    ;; Same reasoning as `authorize-remote`: a resolver that throws otherwise
    ;; escapes as a 500 that reads like a bug, and the request is served or
    ;; not depending on where the exception lands.
    (let [a {:action :read :resource (fn [_] (throw (ex-info "boom" {})))}]
      (is (= 403 (app a :party (random-uuid) :can true))
          "can? stubbed TRUE — the refusal must come from the resolver failing"))))

(deftest an-undeclared-route-is-refused-at-request-time-too
  (testing "belt to the boot check's braces"
    ;; Reaching this means the validator was bypassed (a router built without
    ;; :validate). Serving the route would be the wrong answer.
    (is (= 500 (app nil :party (random-uuid) :can true)))))

;; =============================================================================
;; The blob cookie
;; =============================================================================
;;
;; `<img src>` and `<a href>` are issued by the BROWSER, so they cannot carry
;; an Authorization header — a cookie is the only credential they can present.
;; Cookies are not port-scoped, and every port of a host is the same site, so
;; any server on another port of this host receives it. Hence: the cookie holds
;; a blob-READ capability, signed with its own key, and nothing but the blob
;; read path accepts it. Real tokens throughout — what is under test is which
;; credential each path accepts, so stubbing the validator would test nothing.

(def ^:private a-party #uuid "cafe0000-0000-0000-0000-000000000001")

(defn- now [] (quot (System/currentTimeMillis) 1000))

(defn- access-token
  "An access token exactly as `/auth/login` signs one."
  [party]
  (jwt/sign-hs256 @#'auth-cfg/jwt-secret
                  {:iss "simmis" :aud "simmis-api" :sub (str party)
                   :iat (now) :exp (+ (now) 3600)}))

(defn- blob-token [party] (:token (#'auth-cfg/mint-blob-token party)))

(defn- with-real-validator
  "The access-token validator built from the same `:jwt` map the websocket
   plane validates with (`web/auth-middleware-fn`), minus the auth store the
   full config needs."
  [f]
  (with-redefs [hauth/validator
                (delay (jwt/build-bearer-validator (#'auth-cfg/jwt-config)))]
    (f)))

(defn- req [method headers]
  {:request-method method :headers headers :uri "/blobs/x"})

(defn- cookie [token] {"cookie" (str auth-cfg/blob-cookie-name "=" token)})
(defn- bearer [token] {"authorization" (str "Bearer " token)})

(defn- blob-reader [method headers]
  (with-real-validator #(hauth/blob-reader-party-id (req method headers))))

(defn- api-party [method headers]
  (with-real-validator #(hauth/party-id (req method headers))))

(deftest the-blob-token-authenticates-blob-reads
  (testing "the <img> case: no header, cookie only"
    (is (= a-party (blob-reader :get (cookie (blob-token a-party)))))
    (is (= a-party (blob-reader :head (cookie (blob-token a-party))))))
  (testing "alongside other cookies, which is how a browser sends them"
    (is (= a-party (blob-reader :get {"cookie" (str "other=1; " auth-cfg/blob-cookie-name
                                                    "=" (blob-token a-party)
                                                    "; another=2")}))))
  (testing "the header bearer still reads blobs, without any cookie"
    (is (= a-party (blob-reader :get (bearer (access-token a-party)))))))

(deftest the-blob-token-is-worth-nothing-anywhere-else
  (let [bt (blob-token a-party)]
    (testing "as a Bearer header on any route (HTTP API, RPCs)"
      (is (nil? (api-party :get (bearer bt))))
      (is (nil? (api-party :post (bearer bt)))))
    (testing "as a Bearer header even on the blob route — only the cookie carries it"
      (is (nil? (blob-reader :get (bearer bt)))))
    (testing "in the cookie, for anything that is not a blob read"
      (is (nil? (api-party :get (cookie bt)))
          "`party-id` — every :authenticated route — must not read the cookie"))
    (testing "against the validator the websocket handshake uses"
      ;; `web/auth-middleware-fn` hands `(:jwt (get-auth-config))` to kabel,
      ;; which builds exactly this validator. A blob token must not open a socket.
      (is (nil? ((jwt/build-bearer-validator (#'auth-cfg/jwt-config))
                 {:headers (bearer bt)}))))
    (testing "and the access token still is worth what it was"
      (is (= a-party (api-party :post (bearer (access-token a-party))))))))

(deftest the-access-token-is-no-longer-accepted-from-the-cookie
  (testing "the old cookie contents — a full access token — are refused"
    (is (nil? (blob-reader :get (cookie (access-token a-party)))))
    (is (nil? (api-party :get (cookie (access-token a-party)))))))

(deftest a-blob-token-is-forged-by-nothing-but-its-own-key
  (testing "blob claims signed with the ACCESS-token key are refused"
    (is (nil? (auth-cfg/blob-read-claims
                (jwt/sign-hs256 @#'auth-cfg/jwt-secret
                                {:iss auth-cfg/blob-token-issuer
                                 :aud auth-cfg/blob-token-audience
                                 :sub (str a-party) :exp (+ (now) 600)})))))
  (testing "the blob key, but the access token's issuer and audience"
    (is (nil? (auth-cfg/blob-read-claims
                (jwt/sign-hs256 @#'auth-cfg/blob-read-key
                                {:iss "simmis" :aud "simmis-api"
                                 :sub (str a-party) :exp (+ (now) 600)})))))
  (testing "the blob key and issuer, but another audience"
    (is (nil? (auth-cfg/blob-read-claims
                (jwt/sign-hs256 @#'auth-cfg/blob-read-key
                                {:iss auth-cfg/blob-token-issuer :aud "simmis-api"
                                 :sub (str a-party) :exp (+ (now) 600)})))))
  (testing "a well-formed cookie holding garbage"
    (is (nil? (blob-reader :get (cookie "forged")))))
  (testing "a cookie whose name merely ends in ours"
    ;; The regex anchors on a boundary; `evil_simmis_blob=…` must not match.
    (is (nil? (blob-reader :get {"cookie" (str "evil_" auth-cfg/blob-cookie-name
                                               "=" (blob-token a-party))}))))
  (testing "no cookie at all"
    (is (nil? (blob-reader :get {})))))

(deftest an-expired-blob-token-is-refused
  (let [signed (fn [exp]
                 (jwt/sign-hs256 @#'auth-cfg/blob-read-key
                                 {:iss auth-cfg/blob-token-issuer
                                  :aud auth-cfg/blob-token-audience
                                  :sub (str a-party) :iat (- (now) 7200) :exp exp}))]
    (testing "a minute past expiry — beyond the validator's few seconds of leeway"
      (is (nil? (blob-reader :get (cookie (signed (- (now) 60)))))))
    (testing "the same token, unexpired, is accepted"
      (is (= a-party (blob-reader :get (cookie (signed (+ (now) 60)))))))
    (testing "and a minted one expires within the hour"
      (let [{:keys [exp iat]} (jwt/claims (blob-token a-party))]
        (is (<= (- exp iat) 3600))))))

(deftest the-cookie-cannot-authorize-a-write
  ;; `Path=/blobs/` was meant to keep it off the upload POST at `/blobs`, and
  ;; by RFC 6265 it does. MEASURED: curl sends it anyway. Whichever client is
  ;; right, the scope of a credential is not the client's to decide.
  (testing "the same cookie that reads must not write"
    (doseq [m [:post :put :delete :patch]]
      (is (nil? (blob-reader m (cookie (blob-token a-party))))
          (str m " must not be authenticated by the cookie")))))

;; --- the route form -----------------------------------------------------------

(defn- blob-app
  "`/blobs/:id` declared as web.clj declares it, beside an ordinary
   :authenticated route, under the real gate and the real validators."
  [method uri headers]
  (let [handler (ring/ring-handler
                  (ring/router [["/blobs/:id" {:auth :blob-read
                                               :get {:handler ok}
                                               :post {:handler ok}}]
                                ["/api" {:auth :authenticated :get {:handler ok}}]]
                               {:validate hauth/validate-auth-declared!
                                :data {:middleware [hauth/auth-middleware]}}))]
    (with-real-validator
      #(:status (handler {:request-method method :uri uri :headers headers})))))

(deftest blob-read-is-a-declared-route-form
  (let [bt (blob-token a-party)]
    (testing "a valid blob cookie reads a blob"
      (is (= 200 (blob-app :get "/blobs/abc" (cookie bt)))))
    (testing "the same cookie on an :authenticated route is no credential"
      (is (= 401 (blob-app :get "/api" (cookie bt)))))
    (testing "nor on a write to the blob route itself"
      (is (= 401 (blob-app :post "/blobs/abc" (cookie bt)))))
    (testing "no credential, no blob"
      (is (= 401 (blob-app :get "/blobs/abc" {}))))
    (testing "an access token in the cookie, no blob"
      (is (= 401 (blob-app :get "/blobs/abc" (cookie (access-token a-party))))))))

;; --- issuance -----------------------------------------------------------------

(defn- cookie-value [set-cookie]
  (second (re-find (re-pattern (str "^" auth-cfg/blob-cookie-name "=([^;]*)"))
                   set-cookie)))

(deftest token-issuing-responses-set-a-blob-token-not-the-access-token
  (let [at (access-token a-party)
        login (fn [_] {:status 200 :body {:access_token at :expires_in 604800}})
        wrapped (#'auth-cfg/with-blob-cookie login)
        sc (get-in (wrapped {:scheme :http}) [:headers "Set-Cookie"])
        v (cookie-value sc)]
    (testing "the cookie does not carry the access token"
      (is (some? v))
      (is (not= at v)))
    (testing "it carries a blob-read token for the same party"
      (is (= (str a-party) (:sub (auth-cfg/blob-read-claims v)))))
    (testing "Max-Age is the blob token's lifetime, not the access token's"
      (is (re-find (re-pattern (str "Max-Age=" auth-cfg/blob-token-lifetime "$")) sc)))
    (testing "the attributes are unchanged"
      (is (re-find #"; Path=/blobs/; HttpOnly; SameSite=Lax" sc))
      (is (not (re-find #"Secure" sc)))
      (is (re-find #"; Secure"
                   (get-in (wrapped {:scheme :https}) [:headers "Set-Cookie"]))))
    (testing "a failed login sets no cookie"
      (is (nil? (get-in ((#'auth-cfg/with-blob-cookie
                          (fn [_] {:status 401 :body {:error "invalid-credentials"}}))
                         {})
                        [:headers "Set-Cookie"]))))))

(deftest the-renewal-response-sets-a-fresh-blob-token
  (let [resp (auth-cfg/blob-cookie-response {:scheme :http} a-party)
        v (cookie-value (get-in resp [:headers "Set-Cookie"]))]
    (is (= 200 (:status resp)))
    (is (= (str a-party) (:sub (auth-cfg/blob-read-claims v))))
    (is (nil? (api-party :get (bearer v))))))

(defn- renewal
  "POST /auth/blob-cookie through the route data web.clj mounts, under the
   real gate and the real validators."
  [headers]
  (let [handler (ring/ring-handler
                  (ring/router [["/auth" {:auth :public} (hauth/blob-cookie-route)]]
                               {:validate hauth/validate-auth-declared!
                                :data {:middleware [hauth/auth-middleware]}}))]
    (with-real-validator
      #(handler {:request-method :post :uri "/auth/blob-cookie" :headers headers}))))

(deftest the-renewal-route-takes-the-header-and-nothing-else
  (testing "an access token in the header renews the cookie"
    (let [resp (renewal (bearer (access-token a-party)))
          v (cookie-value (get-in resp [:headers "Set-Cookie"]))]
      (is (= 200 (:status resp)))
      (is (= (str a-party) (:sub (auth-cfg/blob-read-claims v))))))
  (testing "the cookie alone cannot extend itself"
    (let [resp (renewal (cookie (blob-token a-party)))]
      (is (= 401 (:status resp)))
      (is (nil? (get-in resp [:headers "Set-Cookie"])))))
  (testing "a blob token presented as a bearer mints nothing"
    (is (= 401 (:status (renewal (bearer (blob-token a-party)))))))
  (testing "a principal whose sub is not a party uuid gets a 401, not a token for \"\""
    ;; The gate already refuses this (`party-id` parses the sub); the handler
    ;; refuses it again in case a gate in front of it ever does not.
    (let [h (get-in (hauth/blob-cookie-route) [1 :post :handler])]
      (with-redefs [hauth/validator (delay (fn [_] {:sub "user_2abc"}))]
        (is (= 401 (:status (h {:request-method :post :headers {}}))))))))
