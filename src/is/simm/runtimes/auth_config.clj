(ns is.simm.runtimes.auth-config
  "Authentication configuration for Simmis.

   Uses kabel-auth's DatahikeAuthStore backed by the system database.
   Self-issued tokens are JWT HS256 (secret from SIMMIS_JWT_SECRET env var).
   Validation runs through kabel-auth's trusted-issuer registry, so external
   OIDC providers (WorkOS/Clerk/Auth0, config.local.edn `:oidc-issuers`) are
   accepted alongside self-issued tokens. Party role is included in JWT claims
   for client-side authorization."
  (:require [kabel.auth.store.datahike :refer [datahike-auth-store]]
            [kabel.auth.store.protocol :as store]
            [clojure.string :as str]
            [kabel.auth.password :as pwd]
            [kabel.auth.routes :as routes]
            [kabel.auth.jwks :as jwks]
            [kabel.auth.config :as cfg]
            [kabel.auth.jwt]
            [is.simm.model.system-db :as system-db]
            [dvergr.substrate.config :as dcfg]
            [taoensso.telemere :as log]))

;; =============================================================================
;; Auth Store (lazy — initialized after system DB)
;; =============================================================================

(defonce auth-store (atom nil))

(defn get-auth-store
  "Get the auth store, initializing it from system DB if needed."
  []
  (or @auth-store
      (let [conn (system-db/get-conn)]
        (when-not conn
          (throw (ex-info "System DB not initialized. Call system-db/init! first."
                          {:type :initialization-error})))
        (let [s (datahike-auth-store conn)]
          (reset! auth-store s)
          (log/log! {:level :info
                     :id ::auth-store-initialized
                     :msg "DatahikeAuthStore initialized from system DB"})
          s))))

;; =============================================================================
;; JWT Configuration
;; =============================================================================

(def ^:private jwt-secret
  "JWT signing secret. From env var SIMMIS_JWT_SECRET, else a dev secret
   persisted at .dvergr/jwt-secret (gitignored) — persisting it keeps
   sessions valid across JVM restarts; a random per-boot secret silently
   invalidates every client token on restart (connections still work but
   carry no principal, so principal-derived sends fail).

   PRIVATE on purpose: dvergr's sandbox mirrors a host namespace by walking
   `ns-publics`, so a credential must never be a public var. That is defence in
   depth BEHIND the sandbox mirror allowlist, not a substitute for it."
  (or (System/getenv "SIMMIS_JWT_SECRET")
      (let [f (java.io.File. ".dvergr/jwt-secret")]
        (if (.exists f)
          (str/trim (slurp f))
          (let [s (str (java.util.UUID/randomUUID))]
            (.mkdirs (.getParentFile (.getAbsoluteFile f)))
            (spit f s)
            (log/log! {:level :info
                       :id ::dev-jwt-secret-created
                       :msg "Generated persistent dev JWT secret at .dvergr/jwt-secret"})
            s)))))

(def ^:private dev-auth?
  "Is `POST /auth/dev` — the unauthenticated token minter — enabled?

   OPT-IN, via `SIMMIS_DEV_AUTH=true`. It used to be derived as
   `(nil? (System/getenv \"SIMMIS_JWT_SECRET\"))`, which made the INSECURE
   state the DEFAULT state: kabel-auth's dev handler takes any email, CREATES
   the account if it does not exist, marks it `:user/email-verified true`, and
   returns real tokens. Every fresh clone — the state a new user is in for
   their whole first session — shipped a complete authentication bypass.

   The two concerns were never the same question. \"Do I have a configured
   signing secret?\" is answered by `jwt-secret`, which already falls back to a
   persisted dev secret so local development works without any env var at all.
   \"May anyone mint a token for any address?\" has to be asked, and answered,
   on its own.

   Nothing in simmis calls `/auth/dev`: the client logs in through
   `/auth/login` against the `:alpha-users` accounts in config.local.edn. This
   exists for throwaway experiments, and it announces itself loudly when on."
  (= "true" (System/getenv "SIMMIS_DEV_AUTH")))

(def ^:private self-registration?
  "Is `POST /auth/register` — open account creation — enabled?

   OPT-IN, via `SIMMIS_ALLOW_SELF_REGISTRATION=true`, and OFF by default
   because simmis's stated policy is that there is no self-registration:
   accounts come from `:alpha-users` in config.local.edn, and the README says
   so in as many words.

   The route came in with kabel-auth's `auth-routes`, which simmis mounted
   whole. MEASURED 2026-08-20 against a running server: an anonymous
   `POST /auth/register` returned 201 with a user id, `/auth/login` then
   returned a real access token, and that token was accepted by
   `POST /blobs` — anonymous to authenticated in two requests, on a server
   whose documentation promised the door was not there. An account is also
   the whole gate for the ~24 RPCs policed as `:authenticated`.

   Whether accounts may be created is simmis's question, not the auth
   library's, which is why the answer lives here rather than in a config key
   passed down to it."
  (= "true" (System/getenv "SIMMIS_ALLOW_SELF_REGISTRATION")))

;; =============================================================================
;; Jitsi meeting tokens (Track 4 — doc/archive/chat-and-media-timeline-design.md §4)
;; =============================================================================

(def jitsi-url
  "Base URL of the Jitsi deployment the :video tab embeds. Local dev runs
   docker-jitsi-meet on 9100 (~/Development/jitsi-local); production is
   https://meet.simm.is."
  (or (System/getenv "SIMMIS_JITSI_URL") "http://localhost:9100"))

(def ^:private jitsi-jwt-secret
  "Shared secret between simmis (token issuer) and the Jitsi prosody
   (JWT_APP_SECRET). DELIBERATELY distinct from `jwt-secret` — the Jitsi
   containers must never hold the simmis auth signing secret. Same
   persistence pattern: env var, else a dev secret at
   .dvergr/jitsi-jwt-secret (gitignored, matches the local jitsi .env)."
  (or (System/getenv "SIMMIS_JITSI_JWT_SECRET")
      (let [f (java.io.File. ".dvergr/jitsi-jwt-secret")]
        (if (.exists f)
          (str/trim (slurp f))
          (let [s (str (java.util.UUID/randomUUID))]
            (.mkdirs (.getParentFile (.getAbsoluteFile f)))
            (spit f s)
            (log/log! {:level :info
                       :id ::dev-jitsi-jwt-secret-created
                       :msg "Generated dev jitsi JWT secret at .dvergr/jitsi-jwt-secret — copy into the jitsi .env JWT_APP_SECRET"})
            s)))))

(def jitsi-app-id
  "JWT_APP_ID on the Jitsi side; iss+aud of every meeting token."
  "simmis_meet")

(defn mint-jitsi-token
  "Meeting JWT for `party` to join the meeting of room `room-name`
   (= the simmis room id, so the room claim binds the meeting to the
   room). Identity travels in context.user — jigasi transcripts and
   jibri recordings attribute to the simmis party."
  [party room-name]
  (let [now (quot (System/currentTimeMillis) 1000)]
    (kabel.auth.jwt/sign-hs256
      jitsi-jwt-secret
      {:iss jitsi-app-id
       :aud jitsi-app-id
       :sub "*"
       :room room-name
       :nbf (- now 10)
       :exp (+ now (* 4 3600))
       :context {:user {:id    (str (:party/id party))
                        :name  (or (:party/display-name party)
                                   (:party/email party))
                        :email (:party/email party)
                        :moderator true}}})))

(defn external-issuers
  "OIDC identity providers (WorkOS, Clerk, Auth0, …) from config.local.edn
   `:oidc-issuers` ([{:iss \"https://…\" :jwks-url \"https://…/jwks.json\"} …],
   or a pinned `:public-key` PEM instead of a jwks-url). Each becomes an RS256
   entry in the validation registry, so this simmis instance accepts that IdP's
   tokens ALONGSIDE its own — the corporate-SSO seam. Absent key ⇒ self-issued
   only (no behavior change)."
  []
  (into {}
        (for [{:keys [iss jwks-url public-key alg]} (:oidc-issuers (dcfg/config))
              :when iss]
          [iss (cond-> {:alg (or alg :RS256)}
                 jwks-url   (assoc :jwks-url jwks-url)
                 public-key (assoc :public-key public-key))])))

(defn- make-auth-config
  "Build auth configuration. Must be called after system DB is initialized."
  []
  (cfg/merge-config
    {:store (get-auth-store)
     :dev-mode dev-auth?
     :extra-claims-fn (fn [party]
                        (when-let [role (:party/role party)]
                          {:role (name role)}))
     :jwt {:secret jwt-secret
           :alg :HS256
           :issuer "simmis"
           :audience "simmis-api"
           ;; VALIDATION registry (build-bearer-validator uses :issuers
           ;; exclusively when present; the token's `iss` selects the entry).
           ;; Our own self-issued HS256 lives here as "simmis"; configured
           ;; external IdPs join alongside it, so self-issued and WorkOS/Clerk
           ;; tokens coexist on one socket. key-resolver fetches rotating JWKS
           ;; keys for :jwks-url issuers. Top-level :secret above still drives
           ;; the /auth/login ISSUING path (routes sign HS256); this is purely
           ;; the verify side, and existing HS256 tokens validate unchanged.
           :issuers (merge {"simmis" {:alg :HS256 :secret jwt-secret}}
                           (external-issuers))
           :key-resolver (jwks/make-key-resolver)
           ;; Dev/alpha: long-lived access tokens. The ws reconnect path
           ;; does NOT run the refresh flow, so after expiry every
           ;; reconnect silently becomes anonymous (reads work, principal-
           ;; requiring remotes fail with :authentication-required).
           ;; Proper fix (roadmap): client handles :kabel/auth-error by
           ;; refreshing via /auth/refresh and re-authing the socket.
           :access-token-expiry 604800
           :refresh-token-expiry 2592000}
     :password {:enabled true :min-length 8}}))

(defonce auth-config-atom (atom nil))

(defn get-auth-config []
  (or @auth-config-atom
      (do
        ;; BEFORE building the config, not after: `make-auth-config` needs the
        ;; system DB and throws without it, and a warning about an open token
        ;; minter must not be the thing that gets skipped on a partial boot.
        ;; :warn, not :info — an operator who exported SIMMIS_DEV_AUTH for one
        ;; experiment and left it exported has an open minter, and the only way
        ;; they find out is if something says so on every boot.
        (when dev-auth?
          (log/log! {:level :warn :id ::dev-auth-enabled
                     :msg (str "SIMMIS_DEV_AUTH=true — POST /auth/dev will mint "
                               "tokens for ANY email address and create the "
                               "account if it does not exist. Never expose this "
                               "server to a network while it is set.")}))
        (when self-registration?
          (log/log! {:level :warn :id ::self-registration-enabled
                     :msg (str "SIMMIS_ALLOW_SELF_REGISTRATION=true — POST "
                               "/auth/register will create an account for "
                               "anyone who can reach this server, and an "
                               "account is what the :authenticated RPCs "
                               "check for.")}))
        (let [c (make-auth-config)]
          (reset! auth-config-atom c)
          c))))

;; =============================================================================
;; Alpha Party Seeding
;; =============================================================================

(defn alpha-parties
  "Alpha tester accounts from config.local.edn `:alpha-users`
   ([{:email .. :name .. :handle .. :password .. :role ..} ...]).
   No accounts are seeded when the key is absent — passwords never
   live in the repo. Existing accounts are unaffected (seeding only
   creates missing ones)."
  []
  (:alpha-users (dcfg/config)))

(def ^:private placeholder-credentials
  "Values `config.example.edn` ships. An account must never be created from
   them.

   MEASURED on a cold first run 2026-08-20: copying the example and starting
   the app before finishing the edits seeded a REAL admin — `you@example.com`
   with the published password `change-me` — and because seeding only creates
   MISSING accounts, editing the config afterwards left it in place, invisible.
   A published credential with `:role :admin` is not a placeholder, it is a
   back door with documentation."
  {:emails #{"you@example.com"} :passwords #{"change-me"}})

(defn- placeholder? [{:keys [email password]}]
  (or (contains? (:emails placeholder-credentials) email)
      (contains? (:passwords placeholder-credentials) password)))

(defn seed-alpha-parties!
  "Register alpha test parties if they don't already exist."
  []
  (let [s (get-auth-store)]
    (doseq [{:keys [email name handle password role] :as entry} (alpha-parties)]
      (when (placeholder? entry)
        (log/log! {:level :warn :id ::alpha-party-placeholder
                   :msg (str "Refusing to create an account from the config.example.edn "
                             "placeholder (" email "). Edit :alpha-users in "
                             "config.local.edn — set a real email and password — "
                             "and restart.")}))
      (when-not (or (placeholder? entry) (store/find-user-by-email s email))
        (let [party (store/create-user! s
                                        {:party/email email
                                         :party/display-name name
                                         :party/handle handle
                                         :party/password-hash (pwd/hash-password password)
                                         :party/role role
                                         :party/auth-providers #{:password}
                                         :party/email-verified true})]
          (log/log! {:level :info
                     :id ::alpha-party-seeded
                     :msg (str "Seeded alpha party: " email " (role: " (clojure.core/name role) ")")
                     :data {:party-id (:party/id party)}})))))
  ;; Ensure each party has a personal AI room and default KB. The try is PER
  ;; party: one bad alpha user must not stop the rest from being seeded.
  (let [s (get-auth-store)]
    (doseq [{:keys [email name]} (alpha-parties)]
      (try
        (when-let [party (store/find-user-by-email s email)]
          (require 'is.simm.model.rooms)
          (require 'is.simm.model.knowledge-bases)
          (let [room ((resolve 'is.simm.model.rooms/ensure-personal-ai-room!)
                      (:party/id party) (or name email))
                kb ((resolve 'is.simm.model.knowledge-bases/ensure-default-kb!)
                    (:party/id party) (or name email))]
            ;; The personal AI room gets the owner's default KB attached —
            ;; otherwise [[page]] links from its chat have nothing to
            ;; resolve against and knowledge tools see no product KB.
            (when (and (:room/id room) (:kb/id kb))
              ((resolve 'is.simm.model.knowledge-bases/attach-kb-to-room!)
               (:room/id room) (:kb/id kb)))))
        (catch Exception e
          (log/log! {:level :warn
                     :id ::room-seed-error
                     :msg (str "Could not seed personal AI room for " email ": " (.getMessage e))
                     :data {:email email}}))))))

;; =============================================================================
;; Route Configuration
;; =============================================================================

(def blob-cookie-name
  "Name of the cookie that authenticates blob READS.

   Read by `http-auth/cookie-token`. Defined here, on the issuing side, because
   `http-auth` already requires this namespace and the reverse would be a
   cycle."
  "simmis_blob")

(defn- https? [req]
  (or (= :https (:scheme req))
      (= "https" (get-in req [:headers "x-forwarded-proto"]))))

(defn- blob-cookie
  "`Set-Cookie` for the blob read cookie.

   Every attribute is doing work:

     Path=/blobs/   Sent ONLY for `/blobs/<id>` reads. `/blobs` (the upload
                    POST) does not match — a cookie path is a prefix and
                    `/blobs` is not a prefix of `/blobs/` — so uploads keep
                    using the Authorization header and this credential cannot
                    authorize a write.
     HttpOnly       Script cannot read it, so an XSS in a page cannot exfiltrate
                    it the way it can read localStorage.
     SameSite=Lax   The important one. A cross-site img tag pointing at a
                    /blobs/ URL does NOT carry it, so a page the user visits cannot
                    read blobs out of their instance — including one on
                    localhost. This is what makes the permissive CORS header
                    harmless for blobs rather than an amplifier.
     Secure         Only when the request arrived over https; a dev server on
                    http://localhost would otherwise set a cookie the browser
                    refuses to store.
     Max-Age        Matched to the token's own lifetime, so the cookie cannot
                    outlive the credential inside it."
  [req token expires-in]
  (str blob-cookie-name "=" token
       "; Path=/blobs/; HttpOnly; SameSite=Lax"
       (when (https? req) "; Secure")
       "; Max-Age=" (or expires-in 3600)))

(defn- token-of [body]
  (when (map? body) (or (:access_token body) (get body "access_token"))))

(defn- expires-of [body]
  (when (map? body) (or (:expires_in body) (get body "expires_in"))))

(defn- with-blob-cookie
  "Wrap a token-ISSUING handler so its response also sets the blob cookie.

   The browser makes blob requests on our behalf (`<img>`, `<a>`) and cannot
   attach a header to them, so the credential has to be something it stores and
   sends itself. Issued at the same moment, from the same token, and expiring
   with it."
  [handler]
  (fn [req]
    (let [resp (handler req)]
      (if-let [t (and (= 200 (:status resp)) (token-of (:body resp)))]
        (assoc-in resp [:headers "Set-Cookie"]
                  (blob-cookie req t (expires-of (:body resp))))
        resp))))

(defn- clearing-blob-cookie
  "Wrap logout so the cookie goes with the session."
  [handler]
  (fn [req]
    (let [resp (handler req)]
      (assoc-in resp [:headers "Set-Cookie"]
                (str blob-cookie-name "=; Path=/blobs/; HttpOnly; SameSite=Lax"
                     (when (https? req) "; Secure")
                     "; Max-Age=0")))))

(defn- wrap-route
  "Apply `f` to the handler of `method` on a reitit route vector, if present."
  [route method f]
  (if (get-in route [1 method :handler])
    (update-in route [1 method :handler] f)
    route))

(defn auth-route-data
  "Reitit route data for /auth/* endpoints.
   Must be called after system DB is initialized.

   `/register` is REMOVED unless `self-registration?` — see that var. kabel-auth
   offers the route unconditionally; mounting it is simmis's decision, and the
   answer is no by default. Filtered here rather than gated inside the handler
   so the endpoint does not exist at all: nothing to probe, nothing to get
   wrong later in a branch."
  []
  (let [rs (routes/auth-routes (get-auth-config))
        rs (if self-registration?
             rs
             (filterv #(not= "/register" (first %)) rs))]
    (mapv (fn [route]
            (case (first route)
              ("/login" "/refresh") (wrap-route route :post with-blob-cookie)
              "/logout"             (wrap-route route :post clearing-blob-cookie)
              route))
          rs)))
