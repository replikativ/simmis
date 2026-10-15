(ns is.simm.runtimes.http-auth
  "Authentication and size limits for the HTTP media routes.

   These routes shipped with NO auth at all — the comment said \"same auth
   posture as /blobs until eacl lands\", and /blobs's posture was: none. Proven
   against the live box: an anonymous

       curl -X POST https://dev.simm.is/blobs --data-binary @anything

   returned a blob id and wrote to our disk. Anyone on the internet could fill
   it. The room-scoped routes were worse than merely open: a stranger holding a
   room uuid could post screen frames, and each frame costs a VLM call — an
   unauthenticated endpoint that spends money.

   The machinery to prevent this ALREADY EXISTED and was simply never wired up:
   kabel's `build-bearer-validator` returns (fn [ring-req] → principal-or-nil)
   for exactly this. So the fix is not new security infrastructure — it is using
   the security infrastructure we have, with the same secret and the same
   trusted-issuer registry as the websocket plane. One identity story, two
   transports.

   Membership, not just authentication: a logged-in user is not thereby entitled
   to write into someone else's room. Room-scoped routes check `:room/parties`.

   And a size cap BEFORE the bytes are read. `.readAllBytes` on a request body
   is an unbounded heap allocation controlled by the caller: a 2GB POST is a 2GB
   array. Content-Length is a claim, not a fact, so we cap the READ as well —
   a lying header buys nothing."
  (:require [clojure.string :as str]
            [is.simm.model.rooms :as rooms]
            [is.simm.runtimes.auth-config :as auth-cfg]
            [kabel.auth.jwt :as jwt]
            [taoensso.telemere :as log])
  (:import [java.io InputStream ByteArrayOutputStream]))

;; 25MB. Screen frames are ~300KB, recording chunks a few MB, an xlsx rarely
;; more. Large-file intake is a DIFFERENT design (streamed, backpressured, quota'd)
;; and must not arrive by accident through a route meant for JPEGs.
(def ^:const max-body-bytes (* 25 1024 1024))

(defonce ^:private validator
  (delay
    ;; Derive from the SAME :jwt map the websocket plane verifies with — do not
    ;; hand-roll a second one. The first version of this namespace did, and got
    ;; it wrong twice over: it passed only `external-issuers` as the registry,
    ;; which OMITS our own "simmis" issuer, and build-bearer-validator uses the
    ;; registry EXCLUSIVELY when present — so every self-issued token, i.e.
    ;; every real user, was rejected as an untrusted issuer. One config, one
    ;; identity story; an external IdP token that opens the socket opens these
    ;; routes, and one that does not, does not.
    (jwt/build-bearer-validator (:jwt (auth-cfg/get-auth-config)))))

(defn- bearer-principal [req]
  (try
    (@validator req)
    (catch Throwable t
      (log/log! {:level :debug :id ::bearer-invalid :data {:error (ex-message t)}})
      nil)))

(defn principal
  "The authenticated principal of a ring request, or nil.

   The Authorization header, validated against the access-token issuers —
   nothing else. In particular NOT the blob cookie: that holds a blob-read
   capability, which only `blob-reader-party-id` accepts. It used to be
   accepted here, by handing the cookie to this same validator, back when the
   cookie held the access token itself — which made a cookie any same-host
   port could receive worth a full bearer credential on every route."
  [req]
  (bearer-principal req))

(defn party-id
  "The authenticated party uuid, or nil. The JWT's :sub."
  [req]
  (when-let [p (principal req)]
    (let [sub (or (:sub p) (get p "sub"))]
      (try (parse-uuid (str sub)) (catch Throwable _ nil)))))

;; ---------------------------------------------------------------------------
;; The blob read path
;; ---------------------------------------------------------------------------

(defn- cookie-token
  "The blob cookie's token, or nil.

   Its whole reason to exist: `<img src>` and `<a href>` are made by the
   BROWSER, not by our code, so they cannot carry an Authorization header. A
   cookie is the only credential those requests can present. See
   `auth-config/blob-cookie` for how it is issued and why it is scoped the way
   it is."
  [req]
  (some-> (get-in req [:headers "cookie"])
          (->> (re-find (re-pattern (str "(?:^|;\\s*)"
                                         auth-cfg/blob-cookie-name
                                         "=([^;]+)"))))
          second))

(def ^:private cookie-methods
  "The cookie authenticates READS only.

   `Path=/blobs/` was supposed to keep it away from the upload POST at
   `/blobs`, and by RFC 6265 it does — `/blobs/` is not a prefix of `/blobs`.
   MEASURED: curl sends it anyway. Which client is right matters less than the
   principle it broke — the scope of a credential is not something to delegate
   to the code holding it. A write must present the header; this is checked
   here, where it cannot be talked out of."
  #{:get :head})

(defn blob-reader-party-id
  "The party a BLOB READ may be attributed to, or nil.

   The header bearer (an access token) first, exactly as `party-id`; then, for
   GET/HEAD only, the blob cookie — validated by `auth-config/blob-read-claims`,
   the blob-read key and issuer, and never by the access-token validator. The
   two credentials do not cross: an access token in the cookie is refused, and
   a blob token in the header is refused. Only routes declaring
   `:auth :blob-read` reach this."
  [req]
  (or (party-id req)
      (when (contains? cookie-methods (:request-method req))
        (when-let [c (some-> (cookie-token req) auth-cfg/blob-read-claims)]
          (try (parse-uuid (str (:sub c))) (catch Throwable _ nil))))))

(defn read-body
  "Read at most `max-body-bytes` from the request body.

   Returns bytes, or nil when the body exceeds the cap — the caller answers 413.
   We do NOT trust Content-Length: it is a claim by the same party we are
   defending against. Reading with a hard ceiling makes the claim irrelevant."
  ^bytes [req]
  (let [^InputStream in (:body req)
        buf (byte-array 65536)
        out (ByteArrayOutputStream.)]
    (loop [total 0]
      (let [n (.read in buf)]
        (cond
          (neg? n) (.toByteArray out)
          (> (+ total n) max-body-bytes) nil       ; over the cap — stop reading
          :else (do (.write out buf 0 n)
                    (recur (+ total n))))))))

;; ---------------------------------------------------------------------------
;; Guards. Each returns an error RESPONSE, or nil when the request may proceed.
;; ---------------------------------------------------------------------------

(defn- deny [status msg]
  {:status status
   :headers {"Content-Type" "application/json"}
   :body (str "{\"error\":\"" msg "\"}")})

(defn require-auth
  "nil when the request carries a valid bearer token, else a 401 response."
  [req]
  (when-not (party-id req)
    (log/log! {:level :warn :id ::unauthenticated
               :data {:uri (:uri req) :method (:request-method req)}})
    (deny 401 "authentication required")))

(defn require-blob-reader
  "nil when the request may read a blob (`blob-reader-party-id`), else 401."
  [req]
  (when-not (blob-reader-party-id req)
    (log/log! {:level :warn :id ::unauthenticated-blob-read
               :data {:uri (:uri req) :method (:request-method req)}})
    (deny 401 "authentication required")))

(defn blob-cookie-route
  "`/auth/blob-cookie` — renews the blob-read cookie for a tab that outlives
   it. Mounted by `web/make-handler` under `/auth`, and built here so the tests
   exercise the very route data the server serves.

   `:authenticated`, i.e. the Authorization header and nothing else: a POST
   never accepts the cookie (and `party-id` never reads it), so a cookie cannot
   extend itself and a blob token cannot mint another. The nil check is not
   redundant with the gate — a principal whose `sub` is not a party uuid (an
   external issuer's) must get a 401, not a capability for party \"\"."
  []
  ["/blob-cookie"
   {:post {:auth :authenticated
           :handler (fn [req]
                      (if-let [pid (party-id req)]
                        (auth-cfg/blob-cookie-response req pid)
                        (deny 401 "authentication required")))}}])

(defn require-room-member
  "nil when the authenticated party is a member of `room-id-str`, else 401/403/404.

   Being logged in is not authorisation. Room uuids are unguessable, but that is
   obscurity, not access control — and it fails the moment a uuid appears in a
   shared link, a log, or a screenshot."
  [req room-id-str]
  (or (require-auth req)
      (let [pid (party-id req)
            room-uuid (try (parse-uuid (str room-id-str)) (catch Throwable _ nil))
            room (when room-uuid (rooms/get-room room-uuid))]
        (cond
          (nil? room) (deny 404 "unknown room")

          (not (contains? (set (:room/parties room)) pid))
          (do (log/log! {:level :warn :id ::not-a-room-member
                         :data {:room room-uuid :party pid :uri (:uri req)}})
              (deny 403 "not a member of this room"))

          :else nil))))

(defn too-large
  "The 413 response, for when `read-body` returns nil."
  [req]
  (log/log! {:level :warn :id ::body-too-large
             :data {:uri (:uri req) :cap-mb (quot max-body-bytes 1048576)}})
  (deny 413 (str "body exceeds " (quot max-body-bytes 1048576) "MB")))

;; =============================================================================
;; The route policy gate
;; =============================================================================
;;
;; The RPC plane has `access/rpc-policy` + `authorize-remote`: deny-by-default,
;; and a fn with no policy row is refused rather than exposed. The HTTP plane
;; had no equivalent. It grew route by route with an ad-hoc `(or (require-auth
;; req) …)` inside each handler, so a route was reachable the moment it was
;; added and nothing noticed an omission. Two got through — `GET /blobs/:id`
;; and `/apps/<slug>/`, both serving files to anyone who could reach the port.
;;
;; The fix is not a third guard to remember. It is to make the declaration
;; MANDATORY and to check it at BOOT: `validate-auth-declared!` runs as
;; reitit's `:validate` hook, which fires when the router is CONSTRUCTED, so a
;; route with no `:auth` key stops the server from starting. There is no state
;; in which an undeclared route is serving traffic.

(def auth-vocabulary
  "What a route's `:auth` may say.

     :public         — no check. The SPA shell, its assets, and the login
                       endpoints, which cannot require a token to issue one.
     :authenticated  — a valid bearer token, nothing more. Correct when the
                       HANDLER scopes to `party-id`, which is how the media
                       routes work.
     :blob-read      — `:authenticated`, OR a valid blob-read cookie on a
                       GET/HEAD (`blob-reader-party-id`). The ONLY form that
                       accepts the cookie; declare it on blob reads and
                       nowhere else.
     {:action a
      :resource f}   — a token, then `access/can? principal a (f req)`. The
                       same vocabulary `rpc-policy` uses, so both planes state
                       authorization in one language and share one predicate.

   Deliberately small. A route that needs a different RULE should not invent
   a new form here; it should express itself as a `:resource` function.
   `:blob-read` is not a different rule — it is `:authenticated` with one more
   accepted CREDENTIAL, and which credentials a route accepts has to be stated
   by the route, not inferred from the request."
  #{:public :authenticated :blob-read})

(defn- declared-auth
  "The `:auth` for this match — method-level first, then route-level, so a
   parent can declare once for a subtree (`[\"/auth\" {:auth :public} …]`) and a
   single method can still differ from its siblings."
  [req]
  (let [d (get-in req [:reitit.core/match :data])]
    (or (get-in d [(:request-method req) :auth])
        (:auth d))))

(defn- check-resource
  "Authenticate, then ask `can?`. Kept out of the middleware body so the
   `require-auth` short-circuit reads the same as everywhere else."
  [req {:keys [action resource]}]
  (or (require-auth req)
      (let [principal {:sub (party-id req)}
            res (try {:value (resource req)}
                     (catch Throwable e {:error e}))]
        (if-let [e (:error res)]
          ;; Deny-by-default has to survive a BROKEN resolver, not just a
          ;; false one — the same reasoning as `authorize-remote`.
          (do (log/log! {:level :error :id ::resource-resolver-failed
                         :msg "route resource resolver threw — denying"
                         :data {:uri (:uri req) :action action
                                :error (.getMessage e)}})
              (deny 403 "not authorized"))
          (when-not ((requiring-resolve 'is.simm.model.access/can?)
                     principal action (:value res))
            (log/log! {:level :warn :id ::route-denied
                       :data {:uri (:uri req) :action action
                              :party (party-id req)}})
            (deny 403 "not authorized"))))))

(def auth-middleware
  "reitit middleware enforcing the route's declared `:auth`.

   Installed once via the router's `:data {:middleware [...]}`, so it applies
   to every route including ones nested under library-provided route data.
   An undeclared route is refused with 500 rather than served: by the time a
   request arrives, `validate-auth-declared!` should already have prevented
   the server from booting, so reaching this branch means the validator was
   bypassed and the safe answer is to fail loudly."
  {:name ::route-auth
   :wrap (fn [handler]
           (fn [req]
             (let [a (declared-auth req)]
               (cond
                 (= :public a) (handler req)
                 (= :authenticated a) (or (require-auth req) (handler req))
                 (= :blob-read a) (or (require-blob-reader req) (handler req))
                 (map? a) (or (check-resource req a) (handler req))
                 :else
                 (do (log/log! {:level :error :id ::undeclared-route
                                :msg "route has no :auth — refusing"
                                :data {:uri (:uri req) :auth (pr-str a)}})
                     (deny 500 "route has no authorization policy"))))))})

(defn validate-auth-declared!
  "reitit `:validate` hook — THROWS AT ROUTER CONSTRUCTION when any endpoint
   is missing a valid `:auth`.

   This is the whole point of the exercise. A test can be forgotten to run; a
   boot failure cannot be. Adding an HTTP route without deciding its
   authorization now makes the server refuse to start, and names the route."
  [routes _opts]
  (let [bad (for [[path data] routes
                  :let [methods (keep #(when (map? (get data %)) %)
                                      [:get :post :put :delete :patch :head :options])]
                  :when (seq methods)
                  m methods
                  :let [a (or (get-in data [m :auth]) (:auth data))]
                  :when (not (or (contains? auth-vocabulary a)
                                 (and (map? a) (:action a) (fn? (:resource a)))))]
              (str (str/upper-case (name m)) " " path))]
    (when (seq bad)
      (throw (ex-info (str "HTTP routes with no :auth declaration — refusing to "
                           "start. Every route must say who may reach it; see "
                           "`http-auth/auth-vocabulary`. Offending: "
                           (pr-str (vec bad)))
                      {:error :http/undeclared-routes :routes (vec bad)})))))
