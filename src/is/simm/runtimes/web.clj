(ns is.simm.runtimes.web
  (:require [is.simm.runtimes.http-auth :as hauth]
            [superv.async :refer [S go-super <?? <?]]
            [kabel.peer :as peer]
            [kabel.http-kit :as http-kit]
            [is.simm.distributed-scope :refer [remote-middleware invoke-on-peer]]
            ;; Authentication middleware
            [kabel.auth.websocket :as ws-auth]
            ;; konserve-sync for store replication
            [konserve-sync.core :as sync]
            ;; Datahike kabel handlers for remote transactions
            [datahike.kabel.handlers :as dh-handlers]
            [datahike.kabel.cbor-handlers :as dh-cbor]
            [taoensso.telemere :as log]
            ;; Server execution context
            [is.simm.runtimes.context :as ctx]
            ;; System DB (must init before auth)
            [is.simm.model.system-db :as system-db]
            [is.simm.model.access :as access]
            [is.simm.model.message-notify-broadcast :as mnb]
            [is.simm.model.run-broadcast :as run-broadcast]
            [is.simm.model.mail-accounts :as mail-accounts]
            ;; Auth configuration
            [is.simm.runtimes.auth-config :as auth-cfg]
            ;; HTTP routing
            [reitit.ring :as ring]
            [ring.util.response :as response]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [datahike.api :as d-api]
            ;; distributed
            [is.simm.model.db :as db]
            [is.simm.model.schema]
            [is.simm.model.crud]
            ;; Block editor remote handlers (still needed for custom operations)
            [is.simm.uis.web.desktop.block-remote]
            ;; Ping test for spin-remote validation
            ;; Settings remote handlers
            [is.simm.uis.web.desktop.settings-remote]
            ;; Briefkasten mail knowledge-source handlers
            [is.simm.uis.web.desktop.mail-remote]
            ;; Admin remote handlers
            [is.simm.uis.web.desktop.admin-remote]
            ;; Chat remote handlers
            [is.simm.uis.web.desktop.chat-remote]
            ;; Sandbox state inspection and eval-in-room
            [is.simm.uis.web.desktop.sandbox-remote]
            ;; KB branching remote handlers (slice α + β)
            [is.simm.uis.web.desktop.branching-remote]
            [is.simm.uis.web.desktop.proposals-remote]
            ;; Tasks aggregate + Accounting position. These are required for
            ;; their SIDE EFFECT: `defn-spin-remote` registers the handler at
            ;; namespace load, so a remote whose namespace no server code
            ;; requires is simply "not found" at invoke time — the client's
            ;; require does not reach the JVM.
            [is.simm.uis.web.desktop.tasks-remote]
            [is.simm.uis.web.desktop.accounting-remote]
            [is.simm.uis.web.desktop.feed-remote]
            [org.replikativ.spindel.distributed.core :as dist])
  (:import [java.io File]
           [java.io BufferedReader InputStreamReader]))

;; =============================================================================
;; JSON Body Parsing Middleware
;; =============================================================================

(defn- read-body-string
  "Read the request body as a string."
  [body]
  (cond
    (string? body) body
    (instance? java.io.InputStream body)
    (with-open [rdr (BufferedReader. (InputStreamReader. body "UTF-8"))]
      (slurp rdr))
    :else nil))

(defn- parse-json-body
  "Parse JSON body into a Clojure map."
  [body-string]
  (when (and body-string (not (clojure.string/blank? body-string)))
    (try
      (let [parsed (json/read-str body-string :key-fn keyword)]
        parsed)
      (catch Exception _
        nil))))

(defn- serialize-json-body
  "Serialize response body map to JSON string."
  [body]
  (cond
    (map? body) (json/write-str body)
    (vector? body) (json/write-str body)
    :else body))

(defn- apps-room-resource
  "The room an `/apps/<slug>/…` URL addresses, as a `can?` resource.

   nil when the slug names no room — `can?` denies an unresolvable resource,
   so an unknown slug is refused without revealing whether it exists."
  [req]
  (let [rest-uri (subs (:uri req) (count "/apps/"))
        slug (java.net.URLDecoder/decode
               (or (first (str/split rest-uri #"/")) "") "UTF-8")]
    (when-let [room ((requiring-resolve 'dvergr.system.db/room-by-slug) slug)]
      {:room (:room/id room)})))

(defn- apps-handler
  "dvergr owns the serving (worktree lookup, containment, content types); this
   only supplies the 404 for a path it declines, which the default handler
   used to swallow into an empty 200."
  [req]
  (or ((requiring-resolve 'dvergr.web.apps/handle) req nil)
      {:status 404 :headers {"Content-Type" "text/plain"} :body "not found"}))

(defn- websocket-upgrade?
  "Check if the request is a WebSocket upgrade."
  [request]
  (let [upgrade (get-in request [:headers "upgrade"] "")]
    (clojure.string/includes? (clojure.string/lower-case upgrade) "websocket")))

(defn query-params
  "Decode the request's query string.

   NOTE: nothing in this handler stack runs wrap-params, so `:query-params` is
   ALWAYS nil. Reading it silently yielded seq=0 for EVERY recording chunk —
   each chunk appended over the last instead of after it, and the segment came
   out as a few hundred bytes of the final chunk. Decode it here rather than
   assume middleware that is not installed."
  [req]
  (let [qs (:query-string req)]
    (if (clojure.string/blank? qs)
      {}
      (into {} (for [pair (clojure.string/split qs #"&")
                     :let [[k v] (clojure.string/split pair #"=" 2)]
                     :when (seq k)]
                 [(java.net.URLDecoder/decode k "UTF-8")
                  (when v (java.net.URLDecoder/decode v "UTF-8"))])))))

(defn wrap-cors
  "Ring middleware that adds CORS headers for cross-origin requests (dev mode).
   Passes WebSocket upgrade requests through unmodified."
  [handler]
  (fn [request]
    (if (websocket-upgrade? request)
      ;; WebSocket upgrade - pass through directly
      (handler request)
      (let [origin (get-in request [:headers "origin"])]
        (if (= (:request-method request) :options)
          ;; Preflight request
          {:status 204
           :headers {"Access-Control-Allow-Origin" (or origin "*")
                     "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS"
                     "Access-Control-Allow-Headers" "Content-Type, Authorization"
                     "Access-Control-Max-Age" "86400"}}
          ;; Normal request
          (when-let [response (handler request)]
            (update response :headers merge
                    {"Access-Control-Allow-Origin" (or origin "*")
                     "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS"
                     "Access-Control-Allow-Headers" "Content-Type, Authorization"})))))))

(defn wrap-json
  "Ring middleware that parses JSON request bodies and serializes JSON response bodies."
  [handler]
  (fn [request]
    (let [;; Parse request body if Content-Type is JSON
          content-type (get-in request [:headers "content-type"] "")
          request (if (clojure.string/includes? content-type "json")
                    (let [body-str (read-body-string (:body request))
                          parsed (parse-json-body body-str)]
                      (assoc request :body parsed))
                    request)
          ;; Call handler
          response (handler request)]
      ;; Serialize response body if it's a map/vector
      (if (and response (or (map? (:body response))
                            (vector? (:body response))))
        (-> response
            (update :body serialize-json-body)
            (assoc-in [:headers "Content-Type"] "application/json"))
        response))))

;; kabel

(def url (or (System/getenv "SIMMIS_WS_URL") "ws://localhost:47295"))

;; this is useful to track messages, so each peer should have a unique id
(def server-id #uuid "05a06e85-e7ca-4213-9fe5-04ae511e50a0")

;; The server execution context is created in is.simm.runtimes.context
;; and is available as ctx/server-context. It's automatically registered
;; with dist/register-context! as :default.

;; CBOR middleware with Datahike handlers (includes DB and TxReport handlers)
(defn datahike-cbor-middleware
  "CBOR serialization middleware configured with Datahike type handlers."
  [peer-config]
  (dh-cbor/datahike-cbor-middleware peer-config))

;; Authentication middleware - validate auth FROM remote clients using JWT
;; Delayed to allow system DB initialization before config access
(defn auth-middleware-fn []
  (ws-auth/auth-middleware
    {:validate {:jwt (:jwt (auth-cfg/get-auth-config))
                :on-auth (fn [principal]
                           (log/log! {:level :info
                                      :id ::auth-success
                                      :msg "Client authenticated"
                                      :data {:email (:email principal)}})
                           ;; Project the just-authenticated party into the synced
                           ;; (The S/Person address-book projection that used to run
                           ;; here is gone: the client gets its roster from
                           ;; `load-rooms!`'s `:contacts`, which is server-filtered
                           ;; to reachable parties. See uis.desktop.people.)
                           nil)}}))

;; Server state - atom for restartability
(defonce ^:private server-state (atom {:server nil :invocation-loop nil :started? false}))

;; =============================================================================
;; Composite HTTP+WS Handler
;; =============================================================================

(defn wrap-composite-handler
  "Wrap kabel's WS handler with a reitit router that handles HTTP routes.
   WebSocket upgrades go to kabel, HTTP requests go through reitit."
  [ws-handler]
  (let [;; ring's file-response does NOT set a content type — that is what
        ;; wrap-content-type exists for. Serving the bundle without one is
        ;; not cosmetic: a reverse proxy matches gzip_types on Content-Type,
        ;; so an untyped response ships UNCOMPRESSED. dev.simm.is shipped a
        ;; multi-MB app.js raw over the wire and looked like a blank page.
        content-type-for (fn [path]
                           (cond
                             (str/ends-with? path ".html") "text/html"
                             (str/ends-with? path ".js") "text/javascript"
                             (str/ends-with? path ".css") "text/css"
                             (str/ends-with? path ".svg") "image/svg+xml"
                             (str/ends-with? path ".png") "image/png"
                             (str/ends-with? path ".ico") "image/x-icon"
                             (str/ends-with? path ".map") "application/json"
                             :else "application/octet-stream"))
        ;; Static file handler for production. A `public/` DIRECTORY next
        ;; to the process wins (deploy-dir override, matches dev), falling
        ;; back to `public/*` CLASSPATH resources — the uberjar ships the
        ;; release CLJS build inside itself, so a bare `java -jar
        ;; simmis.jar` serves the app with no unpacked files.
        static-handler (fn [request]
                         (let [uri (:uri request)
                               path (if (= uri "/") "/index.html" uri)
                               file (File. (str "public" path))]
                           (cond
                             (.isFile file)
                             (-> (response/file-response (.getPath file))
                                 (response/content-type (content-type-for path)))

                             ;; classpath fallback (jar-embedded public/)
                             (not (str/includes? path ".."))
                             (when-let [res (io/resource (str "public" path))]
                               (-> (response/response (io/input-stream res))
                                   (response/content-type (content-type-for path)))))))
        ;; SPA fallback: a browser NAVIGATION to a path the app owns rather than
        ;; the filesystem (`/proposal/<id>`, `/page/<uuid>`, …) must boot the app
        ;; so the client router can resolve it.
        ;;
        ;; Without this an unknown path did not even 404: the default handler
        ;; fell through to kabel, so `GET /proposal/abc` answered **200 with a
        ;; fressian websocket handshake body** (`is.simm.distributed-scope
        ;; register-scope`) labelled `Content-Type: text/html` — binary protocol
        ;; data rendered as a page. Measured 2026-07-31 against the running dev
        ;; server. That is a bug for any mistyped URL, independent of routing.
        ;;
        ;; TWO GATES, and both matter:
        ;;   :get      — a POST/PUT to an unknown path is an API mistake and must
        ;;               not be answered with a page.
        ;;   Accept: text/html — only a top-level navigation says this. Scripts,
        ;;               images, `fetch` and XHR do not, so a missing asset still
        ;;               falls through instead of being masked by index.html.
        ;;               That is the "stale build serves a silently wrong bundle"
        ;;               failure CLAUDE.md warns about, and this is what avoids it.
        ;;
        ;; Ordering: reitit's own routes (/auth, /blobs, /blobs/:id) match before
        ;; the default handler, and `static-handler` runs before this, so a real
        ;; file always wins. This only sees what both declined.
        ;;
        ;; Dev needs no separate arrangement: shadow-cljs proxies EVERYTHING here
        ;; (`:proxy-url` with no `:proxy-predicate` makes it discard its own
        ;; handler — dev_http.clj:95-105), so :8080 inherits this fallback.
        index-response (fn []
                         (let [f (File. "public/index.html")]
                           (or (when (.isFile f)
                                 (-> (response/file-response (.getPath f))
                                     (response/content-type "text/html")))
                               (when-let [res (io/resource "public/index.html")]
                                 (-> (response/response (io/input-stream res))
                                     (response/content-type "text/html"))))))
        spa-fallback (fn [request]
                       (when (and (= :get (:request-method request))
                                  (str/includes? (str (get-in request [:headers "accept"]))
                                                 "text/html"))
                         (index-response)))
        ;; Reitit router for HTTP API routes
        router (ring/router
                 [(into ["/auth" {:auth :public}] (auth-cfg/auth-route-data))
                  ;; Content-addressed blobs (voice-note audio, media
                  ;; stills). Content-addressing makes ids unguessable
                  ;; (SHA-256); real per-blob authz arrives with eacl.
                  ["/blobs"
                   ;; Upload: raw body + Content-Type → CAS. Returns
                   ;; {:id :size}; the caller then transacts a tree node
                   ;; referencing the hash (drives/link-blob! via RPC).
                   {:auth :authenticated
                    :post (fn [req]
                            ;; WAS UNAUTHENTICATED. An anonymous POST wrote to our
                            ;; disk — proven against the live box — and .readAllBytes
                            ;; made the heap allocation the CALLER's choice.
                            (or (hauth/require-auth req)
                                (if-let [bytes (hauth/read-body req)]
                                  (let [mime (get-in req [:headers "content-type"]
                                                     "application/octet-stream")
                                        blob ((requiring-resolve 'is.simm.model.blobs/store!)
                                              bytes mime)]
                                    {:status 200
                                     :headers {"Content-Type" "application/json"}
                                     :body (str "{\"id\":\"" (:blob/id blob)
                                                "\",\"size\":" (:blob/size blob) "}")})
                                  (hauth/too-large req))))}]
                  ;; Live screen-share frames (Track 4c), scoped by USER
                  ;; (doc/archive/screen-capture-scoping.md): the frame belongs to the
                  ;; authenticated sharer's OWN stream — no room in the path.
                  ;; Which rooms see it is decided by grants, not by where it was
                  ;; posted. Any authenticated party may feed their own stream.
                  ["/screen-frames"
                   {:auth :authenticated
                    :post (fn [req]
                            (or (hauth/require-auth req)
                                (if-let [bytes (hauth/read-body req)]
                                  (let [mime (get-in req [:headers "content-type"] "image/jpeg")]
                                    ((requiring-resolve 'is.simm.runtimes.screen-intake/handle-frame!)
                                     (hauth/party-id req) bytes mime))
                                  (hauth/too-large req))))}]

                  ;; Web-page intake (doc/archive/web-intake-design.md): the browser
                  ;; extension POSTs a captured DOM as JSON, authenticated with the
                  ;; user's JWT so it lands in that user's OWN page archive. Any
                  ;; authenticated party may feed their own archive; TLS in transit;
                  ;; DOM content is never logged.
                  ["/pages"
                   {:auth :authenticated
                    :post (fn [req]
                            (or (hauth/require-auth req)
                                (let [b (:body req)          ; wrap-json parsed the JSON
                                      g (fn [k] (or (get b k) (get b (name k))))]
                                  ((requiring-resolve 'is.simm.runtimes.web-intake/handle-page!)
                                   (hauth/party-id req)
                                   {:url (g :url) :title (g :title) :html (g :html)
                                    :text (g :text) :meta (g :meta)}))))}]

                  ;; NOTE: screen-share GRANTS (open/close/heartbeat) are NOT here.
                  ;; They are control-plane messages, not blob uploads, so they go
                  ;; over distributed-scope as defn-spin-remote RPCs
                  ;; (chat_remote.cljc: open-screen-grant! / close-screen-grant! /
                  ;; screen-grant-heartbeat!) — auth + party binding for free, no
                  ;; hand-written HTTP glue. Only the BINARY media (frames + video
                  ;; chunks) needs an HTTP route, and only because of blob size.

                  ;; Continuous recording (Track 4e) — the OWNER's archive tier,
                  ;; keyed by the authenticated sharer (no room in the path). A
                  ;; recording of your desktop is yours; rooms see slices via
                  ;; grants. Authentication is required (it writes to our disk),
                  ;; but not room membership — you record your own screen.
                  ["/screen-recordings/start"
                   ;; the json middleware above already parsed :body into a map
                   {:auth :authenticated
                    :post (fn [req]
                            (or (hauth/require-auth req)
                                (let [body (:body req)
                                      g (fn [k] (or (get body k) (get body (name k))))]
                                  ((requiring-resolve 'is.simm.runtimes.screen-recording/start-session!)
                                   (hauth/party-id req)
                                   {:session-id (g :session)
                                    :mime (g :mime)
                                    :width (g :width)
                                    :height (g :height)
                                    :fps (g :fps)}))))}]
                  ["/screen-recordings/chunk"
                   {:auth :authenticated
                    :post (fn [req]
                            (or (hauth/require-auth req)
                                (if-let [bytes (hauth/read-body req)]
                                  (let [q (query-params req)]
                                    ((requiring-resolve 'is.simm.runtimes.screen-recording/append-chunk!)
                                     (hauth/party-id req)
                                     bytes
                                     {:session-id (get q "session")
                                      :segment (parse-long (or (get q "segment") "0"))
                                      :seq (parse-long (or (get q "seq") "0"))
                                      :mime (get q "mime")}))
                                  (hauth/too-large req))))}]
                  ["/screen-recordings/finalize"
                   {:auth :authenticated
                    :post (fn [req]
                            (or (hauth/require-auth req)
                                (let [q (query-params req)]
                                  ((requiring-resolve 'is.simm.runtimes.screen-recording/finalize-segment!)
                                   (hauth/party-id req)
                                   {:session-id (get q "session")
                                    :segment (parse-long (or (get q "segment") "0"))
                                    :offset-ms (parse-long (or (get q "offset") "0"))}))))}]
                  ["/screen-recordings/end"
                   {:auth :authenticated
                    :post (fn [req]
                            (or (hauth/require-auth req)
                                ((requiring-resolve 'is.simm.runtimes.screen-recording/end-session!)
                                 (hauth/party-id req)
                                 {:session-id (get (query-params req) "session")})))}]
                  ;; Room apps. These were served from the DEFAULT handler,
                  ;; which no policy could see — `dvergr.web.apps/handle` has no
                  ;; auth check of its own and is keyed by a human-readable
                  ;; SLUG, so anyone who could guess `after-fix-team-7bad6b86`
                  ;; could read that room's app files. Routed here so the gate
                  ;; applies; an unknown slug resolves to no resource and `can?`
                  ;; denies, which also stops the endpoint confirming which
                  ;; slugs exist.
                  ["/apps/*path"
                   {:auth {:action :read :resource apps-room-resource}
                    :get  {:handler apps-handler}
                    :head {:handler apps-handler}}]
                  ["/blobs/:id"
                   ;; Reads were open until the blob cookie landed: the UI
                   ;; reaches blobs through `<img src>` and `<a href>`, which
                   ;; the BROWSER issues and which therefore cannot carry a
                   ;; bearer header. `auth-config/blob-cookie` gives those
                   ;; requests a credential they can present —
                   ;; `Path=/blobs/`, `HttpOnly`, `SameSite=Lax` — and
                   ;; `http-auth/principal` accepts it by handing it to the same
                   ;; bearer validator.
                   ;;
                   ;; Still `:authenticated` rather than a resource check: a
                   ;; blob is content-addressed and deliberately shared, so the
                   ;; same bytes in two rooms are ONE blob and nothing records
                   ;; which rooms referenced it. Per-blob authorization needs
                   ;; that index; see README "Known gaps".
                   {:auth :authenticated
                    :get (fn [req]
                           (let [id (get-in req [:path-params :id])
                                 bytes ((requiring-resolve 'is.simm.model.blobs/get-bytes) id)]
                             (if bytes
                               {:status 200
                                :headers {"Content-Type" "application/octet-stream"
                                          "Cache-Control" "public, max-age=31536000, immutable"}
                                :body (java.io.ByteArrayInputStream. bytes)}
                               {:status 404 :body "not found"})))}]]
                 ;; The two halves of the policy. `:validate` runs when the
                 ;; router is BUILT — a route with no `:auth` throws here, so
                 ;; the server refuses to start rather than serving it. The
                 ;; middleware then enforces what each route declared. Adding a
                 ;; route without deciding its authorization is no longer
                 ;; something you can do quietly.
                 {:validate hauth/validate-auth-declared!
                  :data {:middleware [hauth/auth-middleware]}})
        ring-handler (ring/ring-handler
                       router
                       ;; Default handler: WS upgrades go to kabel; room apps
                       ;; (/apps/<slug>/ — worktree app/ dirs, dvergr.web.apps)
                       ;; before static files; otherwise static, then kabel.
                       ;; What is left after the router: the SPA shell and its
                       ;; assets, both public by construction — `static-handler`
                       ;; serves `public/` and nothing else, `spa-fallback`
                       ;; serves index.html. Room apps used to be here too,
                       ;; where no policy could reach them; they are a routed
                       ;; endpoint now.
                       ;;
                       ;; Ending in 404 rather than `ws-handler`: a request that
                       ;; matched nothing used to fall through to kabel and come
                       ;; back as an empty 200, so "no such route" and "handler
                       ;; returned nothing" were the same answer. A websocket
                       ;; arrives as an upgrade and is dispatched above.
                       (fn [request]
                         (if (websocket-upgrade? request)
                           (ws-handler request)
                           (or (static-handler request)
                               (spa-fallback request)
                               {:status 404
                                :headers {"Content-Type" "text/plain"}
                                :body "not found"}))))]
    ;; Wrap with CORS and JSON parsing/serialization
    (-> ring-handler
        wrap-json
        wrap-cors)))

;; Shared scope UUID for sync - must match client's simmis-scope.
;; This identifies the logical store across different network environments.
(def simmis-scope #uuid "a1b2c3d4-e5f6-7890-abcd-ef1234567890")

(def ^:private control-topics
  "Global app NOTIFICATION pubsub topics — invalidation signals, not stores.
   Any authenticated party may subscribe; they carry no store data, only
   'something changed' hints that trigger a client-side refresh. Kept as an
   explicit allowlist (not a blanket 'allow all keywords') so a future
   keyword topic stays deny-by-default until vetted. Canonical vars:
   `is.simm.model.branching-broadcast/topic` (:branching/event) and
   `is.simm.model.user-rooms-broadcast/topic` (:user-rooms/dirty)."
  #{:branching/event :user-rooms/dirty})

(defn- notify-topic-party
  "The party a per-user notify topic belongs to, or nil.

   Delegates to `message-notify-broadcast/notify-topic?`, which PARSES the name
   as a uuid and answers nil when it is not one. This was an inline copy that
   returned the raw string unvalidated, kept that way to avoid a require — but
   there is no cycle (that namespace does not require this one, and this one
   already requires it dynamically at boot), and the copy was the weaker half:
   it admitted `:notify/<any-string>` as a well-formed topic and left the
   comparison below as the only thing standing between an arbitrary keyword and
   a private stream. With a non-uuid `:sub` — which an external OIDC issuer can
   produce — that comparison is a plain string match."
  [topic]
  (some-> (mnb/notify-topic? topic) str))

(defn- run-topic-room [topic]
  (run-broadcast/run-topic-room topic))

(defn data-plane-authorized?
  "Join-time subscribe gate for kabel.pubsub topics (see :authorize-fn).
   `principal` is the message's :kabel/principal (stamped by the auth
   middleware); `topic` is a store scope (UUID) OR a control-topic (keyword).

   - base app store (`simmis-scope`): any authenticated party
   - control-topics (global notification channels): any authenticated party
   - per-user notify topics (`:notify/<party>`): ONLY that party (private stream)
   - per-room Run topics (`:runs/<room>`): ONLY members of that room
   - KB / room store scopes: `access/can?`, which denies anonymous and any
     party without an owner/shared/membership/grant relation.

   Deny-by-default: without this gate any peer that learned a scope UUID could
   replicate that store wholesale."
  [principal topic]
  (cond
    (= topic simmis-scope)           (some? (:sub principal))
    (contains? control-topics topic) (some? (:sub principal))
    (notify-topic-party topic)       (= (notify-topic-party topic) (str (:sub principal)))
    (run-topic-room topic)           (access/can? principal :read {:room (run-topic-room topic)})
    :else                            (access/can? principal :read topic)))

(defn data-plane-publish-authorized?
  "Gate on INBOUND `:pubsub/publish` — kabel's `:authorize-publish-fn`.

   Refuses everything, and that is the correct policy rather than a
   placeholder. Sync here is one-directional BY CONSTRUCTION:

   - clients send TRANSACTIONS, not stores. datahike's `KabelWriter` sends a
     transaction to the peer that owns the database; the server applies it
     through simmis's own authorization and the resulting nodes flow back over
     sync.
   - the publisher-side write-hook is attached only inside konserve-sync's
     `register-store!`, which runs SERVER-side. Clients call `subscribe-store!`,
     which attaches none. No client has a mechanism that publishes, let alone a
     reason to.

   So every inbound publish is either a bug or an attack, and the server's own
   publishes never reach here — the write-hook calls `pubsub/publish!`
   directly, while this gate sits on INBOUND messages.

   Until kabel 0.3.105 there was no way to say this: one `authorize-fn`
   answered both operations, and ours answers `:read`, so anyone who could read
   a store could write it. Not merely writing datoms — `-apply-publish` does a
   bare `k/assoc`, with no check that a content-addressed key is the hash of
   its value, and konserve-sync's `:filter-fn` runs outbound only. A room
   member could break a store's CAS invariant, or park an arbitrarily large or
   unlawful value in a store the operator is answerable for.

   A whitelist of nothing is a stronger property than a correct `:write`
   predicate would be, and it is the honest one for this topology.

   WHEN P2P LANDS this must change, because a peer IS a legitimate publisher
   and refusal stops being available. The primitives then divide: content
   addressing says a node is internally consistent, signatures say who produced
   it, the grant says whether they were allowed to. Keeping the refusal HERE, as
   policy, rather than in kabel, is what leaves that room.
   See `.internal/data-plane-write-authorization.md`."
  [_principal _topic]
  false)

(defn create-server!
  "Create a new server peer instance with composite HTTP+WS handler."
  []
  (let [hk-handler (http-kit/create-http-kit-handler! S url server-id)
        ;; Wrap the start-fn to use composite handler
        original-start-fn (:start-fn hk-handler)
        composite-start-fn (fn [volatile]
                             ;; Replace the :handler with composite before starting
                             (let [ws-handler (:handler volatile)
                                   composite (wrap-composite-handler ws-handler)]
                               (original-start-fn (assoc volatile :handler composite))))
        hk-handler (assoc hk-handler :start-fn composite-start-fn)]
    (peer/server-peer S hk-handler server-id
                      ;; Compose middlewares (innermost runs first on connection):
                      ;; 1. auth-middleware - handles :kabel/auth and attaches :kabel/principal
                      ;; 2. remote-middleware - distributed-scope function invocation
                      ;; 3. sync/server-middleware - konserve-sync for store replication,
                      ;;    gated per-topic by data-plane-authorized? (deny-by-default).
                      (comp (sync/server-middleware
                             {:authorize-fn data-plane-authorized?
                              :authorize-publish-fn data-plane-publish-authorized?})
                            remote-middleware
                            (auth-middleware-fn))
                      datahike-cbor-middleware)))

(defn get-server
  "Get the current server instance."
  []
  (:server @server-state))

;; simmis-scope + data-plane-authorized? are defined ABOVE create-server!
;; (near the middleware wiring); this comment marks where the def used to live.

;; Store config factory for dynamically created databases
;; Client sends TieredStore config (mem + indexeddb), but server needs file stores
(defn server-store-config-fn
  "Create server-side store config for a given scope-id.
   Uses file backend since client's TieredStore won't work on JVM."
  [scope-id _client-config]
  {:backend :file
   :path (str "data/simmis-kbs/" scope-id)
   :scope scope-id})

;; Note: sync-store-config and setup-datahike-store-sync! removed
;; Store registration is now handled by dh-handlers/register-store-for-remote-access!

(def ^:private deferred-namespaces
  "Namespaces this process reaches only through `requiring-resolve` on a WORKER
   thread — the muschel/geschichte substrate behind the sandbox's file tools and
   the room repos, plus the room-database layer.

   They are loaded here, single-threaded, before anything starts a thread."
  '[muschel.fs
    muschel.fs.mount
    muschel.fs.geschichte
    muschel.builtins.git
    dvergr.substrate.geschichte
    is.simm.model.room-databases])

(defn- warm-up-deferred-namespaces!
  "Load `deferred-namespaces` on the boot thread before any worker starts.

   `clojure.core/ns` conj's onto `*loaded-libs*` when the ns HEADER finishes,
   not when the body does. So while one thread is still evaluating the body of
   `muschel.fs`, a second thread requiring `muschel.fs.mount` sees `muschel.fs`
   already 'loaded', skips it, and compiles against a namespace whose vars do
   not exist yet — `No such var: fs/normalize-segments`. `load-lib`'s error path
   then `remove-ns`es mount, but mount's own `ns` form had already registered
   it, leaving the lib in `*loaded-libs*` with NO namespace. From then on plain
   `require` is a silent no-op and every later load reports the misleading
   \"namespace 'muschel.fs.mount' not found\". The JVM is poisoned until a
   `:reload` or a restart — which is why this only ever bit cold boots, and bit
   them hard: the web server refused to start at all.

   `requiring-resolve` is NOT protection: its lock only excludes other
   `requiring-resolve` calls, never a plain `require` running on the boot
   thread. The race is in `clojure.core`, so no library can fix it for us; the
   remedy is to make the load order explicit, which is what this does.

   Diagnosed 2026-07-28 with a 6-thread reproduction in ../muschel (~1 run in 2)
   that traced the `*loaded-libs*` write to `muschel/fs.cljc:1`."
  []
  (doseq [ns-sym deferred-namespaces]
    (try (require ns-sym)
         (catch Throwable e
           ;; Loud: a namespace that fails HERE fails on a worker later, where
           ;; it surfaces as an unrelated missing var.
           (log/log! {:level :error :id ::warm-up-failed
                      :msg "Deferred namespace failed to preload"
                      :data {:ns ns-sym :error (ex-message e)}}))))
  (log/log! {:level :info :id ::warm-up-complete
             :data {:count (count deferred-namespaces)}}))

(defn start-server!
  "Start the web server. Idempotent - does nothing if already started."
  []
  (when-not (:started? @server-state)
    (warm-up-deferred-namespaces!)
    ;; Ensure server execution context is registered (may have been lost on restart)
    (require '[org.replikativ.spindel.distributed.core :as dist])
    ((resolve 'dist/register-context!) :default ctx/server-context)
    ;; Make the server context the root binding of *execution-context* so
    ;; that arbitrary worker threads (remote-handler go-loops, http-kit
    ;; receive callbacks, …) inherit the trunk context. Forked spin bodies
    ;; still override via `binding` for branch-aware writes.
    (ctx/bind-server-context!)

    ;; Initialize system DB (must be before auth store)
    (system-db/init!)
    (mail-accounts/start-all!)

    ;; Expose the kontor accounting kernel inside the agent clojure_eval sandbox
    ;; (dvergr owns the generic injector hook; the kontor binding lives in simmis
    ;; since simmis depends on kontor and dvergr does not). Idempotent.
    (try
      ((requiring-resolve 'is.simm.model.accounting-sandbox/register!))
      (catch Throwable e
        (log/log! {:level :warn :id ::kontor-sandbox-register-failed
                   :data {:error (str e)}})))

    ;; Hydrate dvergr rooms: per-room execution ctxs + yggdrasil systems are
    ;; in-memory, so after a JVM restart every provisioned room must be
    ;; rebuilt (ctx + systems), then its live discourse Room re-registered —
    ;; otherwise room stores don't resolve and no room is live. Same order
    ;; dvergr's daemon uses at boot.
    (try
      (ctx/with-server-context
        ((requiring-resolve 'dvergr.system.rooms/hydrate-rooms!))
        ((requiring-resolve 'dvergr.rooms/hydrate-registry!) ctx/server-context))
      (log/log! {:level :info :id ::rooms-hydrated
                 :msg "dvergr rooms hydrated (ctxs + live registry)"})
      (catch Exception e
        (log/log! {:level :warn :id ::room-hydration-failed
                   :msg "dvergr room hydration failed"
                   :data {:error (str e)}})))

    ;; Reap orphaned repo workspace branches. AFTER hydration (it resolves each
    ;; repo on its room's ctx) and BEFORE anything can open an overlay — it
    ;; keeps nothing, which is correct exactly here: an overlay is an in-memory
    ;; record, so nothing that survived the restart holds any of these. Work
    ;; that mattered was published to a canonical branch and is untouched.
    ;; Without this a restart leaks every un-discarded workspace permanently —
    ;; their `:geschichte.workspace/<uuid>` names match no other sweeper.
    (try
      ((requiring-resolve 'is.simm.runtimes.branching/reap-orphan-repo-workspaces!))
      (catch Exception e
        (log/log! {:level :warn :id ::workspace-reap-failed
                   :msg "Orphaned repo workspaces not reaped — they will accumulate"
                   :data {:error (str e)}})))

    ;; Seed alpha parties before starting
    (auth-cfg/seed-alpha-parties!)

    (let [server (create-server!)]
      (swap! server-state assoc :server server)

      ;; Set kabel peer for spin-remote distributed functions
      (dist/set-system-peer! server)

      ;; Start invocation loop
      (let [inv-loop (invoke-on-peer server {:authorize-fn access/authorize-remote})]
        (swap! server-state assoc :invocation-loop inv-loop))

      (go-super S
        ;; Register server peer with db for broadcasting
        (db/set-server-peer! server)

        ;; Register global handlers for remote Datahike operations
        (dh-handlers/register-global-handlers! server {:store-config-fn server-store-config-fn})
        (log/log! {:level :info
                   :id ::datahike-global-handlers-registered
                   :msg "Global datahike.kabel handlers registered"})

        ;; Register the existing simmis store for remote access
        (dh-handlers/register-store-for-remote-access! simmis-scope (db/get-conn) server)
        (log/log! {:level :info
                   :id ::datahike-store-registered-for-remote
                   :msg "Datahike store registered for remote access"
                   :data {:scope-id simmis-scope}})

        ;; Install user-rooms invalidation broadcaster (single topic, party-id payload).
        ;; Clients subscribe on login and re-fetch load-rooms! when their party-id
        ;; appears in a published payload.
        (try
          (require 'is.simm.model.user-rooms-broadcast)
          ((resolve 'is.simm.model.user-rooms-broadcast/install-listener!) server)
          (catch Exception e
            (log/log! {:level :warn :id ::user-rooms-broadcast-failed
                       :msg "Failed to install user-rooms broadcaster"
                       :data {:error (str e)}})))

        ;; Register a per-user notify topic for every party, so a client can
        ;; subscribe to its own `:notify/<party-id>` mention stream on login.
        ;; Fan-out happens at message-persist (message-notify-broadcast/notify-message!).
        (try
          (require 'is.simm.model.message-notify-broadcast)
          ((resolve 'is.simm.model.message-notify-broadcast/register-all-parties!) server)
          (catch Exception e
            (log/log! {:level :warn :id ::message-notify-register-failed
                       :msg "Failed to register per-user notify topics"
                       :data {:error (str e)}})))

        ;; Room-private Dvergr Run lifecycle streams. The watcher queues events
        ;; off Dvergr's lifecycle lock and each topic is membership-gated above.
        (try
          (run-broadcast/install! server)
          (catch Exception e
            (log/log! {:level :warn :id ::run-broadcast-failed
                       :msg "Failed to install Run lifecycle broadcaster"
                       :data {:error (str e)}})))

        ;; Register the branching event topic on the server peer. Client
        ;; subscribes to :branching/event on login and updates its sidebar
        ;; tree + page header pill reactively as branches are created /
        ;; merged / discarded / committed.
        (try
          (require 'is.simm.model.branching-broadcast)
          ((resolve 'is.simm.model.branching-broadcast/ensure-topic-registered!) server)
          (catch Exception e
            (log/log! {:level :warn :id ::branching-broadcast-failed
                       :msg "Failed to register branching broadcast topic"
                       :data {:error (str e)}})))

        ;; ...and on the SAME topic, the proposal rows. A ForkSet's branches
        ;; announce themselves; the ForkSet itself did not, so the Proposals
        ;; inbox went stale the moment an agent filed one and stayed stale.
        ;; Must run after the block above — that one caches the peer.
        (try
          (require 'is.simm.model.branching-broadcast 'is.simm.model.system-db)
          ((resolve 'is.simm.model.branching-broadcast/install-proposal-tx-listener!)
           ((resolve 'is.simm.model.system-db/get-conn)))
          (catch Exception e
            (log/log! {:level :warn :id ::proposal-broadcast-failed
                       :msg "Failed to install proposal tx listener"
                       :data {:error (str e)}})))

        ;; Room subsystems: ONE dvergr call arms the per-room hooks
        ;; (scheduler, message-fold — by ns load), starts the reactive
        ;; clock, and installs the drive substrate: blob store on
        ;; simmis's EXISTING path (old blobs keep resolving) + the host
        ;; conn-resolver routing dvergr's tree/mount/upload paths onto
        ;; simmis REGISTRY drives (named, grant-attached, '<Room> Drive'
        ;; auto-provisioned on first need) + the /drive bash mount.
        (try
          ((requiring-resolve 'dvergr.rooms.subsystems/start-room-subsystems!)
           ctx/server-context
           {:blob-store {:backend :file :path "data/simmis-blobs"
                         ;; generic connect-store requires a stable :id;
                         ;; blobs on disk are key-hashed, unaffected by it
                         :id (java.util.UUID/nameUUIDFromBytes
                              (.getBytes "simmis-blobs:data/simmis-blobs" "UTF-8"))
                         :opts {:sync? true}}
            :drive-conn-fn
            (fn [room-uuid]
              (when-let [drive ((requiring-resolve 'is.simm.model.drives/ensure-room-drive!)
                                room-uuid)]
                ((requiring-resolve 'is.simm.model.drives/connect-drive-database)
                 (:drive/db-scope drive))))})
          (catch Exception e
            (log/log! {:level :warn :id ::room-subsystems-failed
                       :data {:error (str e)}})))

        (<? S (peer/start server))
        (swap! server-state assoc :started? true)
        (log/log! {:level :info
                   :id ::server-started
                   :msg (str "Server started on " url)})

        ;; Backfill for rooms hydrated BEFORE start-room-subsystems! armed
        ;; the register-hooks (they only catch FUTURE registrations —
        ;; upstream TODO: fold this backfill into start-room-subsystems!).
        (try
          (ctx/with-server-context
            (doseq [room ((requiring-resolve 'dvergr.room.registry/list-rooms))]
              (try ((requiring-resolve 'dvergr.rooms.scheduler/start-room-scheduler!) room)
                   (catch Throwable _ nil))
              ;; message fulltext (scriptum) on the room store — chat search
              ;; + summarizer raw material. Best-effort, like the KB index.
              ;; Rooms provisioned through dvergr already carry the index (at
              ;; dvergr's <store-path>-ft location); re-declaring it with our
              ;; legacy path is a :db.secondary/config schema UPDATE, which
              ;; datahike rejects and its writer logs as :error on every boot
              ;; — so only declare when the ident is genuinely absent.
              (try (when-let [conn (:conn (:store room))]
                     (when-not (:db.secondary/type
                                (d-api/entity @conn :room/fulltext))
                       ((requiring-resolve 'dvergr.search.secondary/declare-index!)
                        conn :room/fulltext [:message/content]
                        (str ".dvergr/room-ft/" (name (:id room))))))
                   (catch Throwable _ nil))))
          ;; Scheduled CODE tasks need a fully-joined agent ctx (kb/wiki), which
          ;; simmis injects on join — otherwise the fire hits an unresolved kb/.
          ;; Join the auto-respond agents of every scheduled room now.
          ((requiring-resolve 'is.simm.agents.room-agents/prepare-scheduled-agents!))
          (log/log! {:level :info :id ::scheduler-started
                     :msg "Room scheduler + clock heartbeat started"})
          (catch Exception e
            (log/log! {:level :warn :id ::scheduler-start-failed
                       :data {:error (str e)}})))

        ;; Telegram channel (Stage 4) — no-op without a configured token.
        (try
          (require 'is.simm.runtimes.telegram)
          ((resolve 'is.simm.runtimes.telegram/start-telegram!))
          (catch Exception e
            (log/log! {:level :warn :id ::telegram-start-failed
                       :msg "Telegram channel failed to start"
                       :data {:error (str e)}})))

        ;; Bus pump watchdog — loud detection of the source-pump
        ;; waiter-loss wedge until its root cause is fixed.
        (try
          (require 'is.simm.runtimes.pump-watchdog)
          ((resolve 'is.simm.runtimes.pump-watchdog/start!))
          (catch Exception e
            (log/log! {:level :warn :id ::watchdog-start-failed
                       :data {:error (str e)}})))

        ;; Storage GC — datahike leaves superseded index nodes orphaned and only
        ;; an explicit sweep reclaims them. dvergr owns the sweep but drives it
        ;; from its own daemon, which simmis does not boot, so nothing has ever
        ;; reclaimed. Orphan-only: no retention window, every branch head and its
        ;; history is kept. Must stay in THIS process — it is the writer.
        (try
          (require 'is.simm.runtimes.storage-gc)
          ((resolve 'is.simm.runtimes.storage-gc/start!))
          (catch Exception e
            (log/log! {:level :warn :id ::storage-gc-start-failed
                       :data {:error (str e)}})))))))

(defn stop-server!
  "Stop the web server. Idempotent - does nothing if not started."
  []
  (when (:started? @server-state)
    (run-broadcast/uninstall!)
    (mail-accounts/close-all!)
    (try
      (require 'is.simm.runtimes.telegram)
      ((resolve 'is.simm.runtimes.telegram/stop-telegram!))
      (catch Exception _ nil))
    (when-let [server (:server @server-state)]
      (go-super S
        (<? S (peer/stop server))
        (log/log! {:level :info
                   :id ::server-stopped
                   :msg "Server stopped"})))
    (reset! server-state {:server nil :invocation-loop nil :started? false})))

(defn restart-server!
  "Restart the web server."
  []
  (stop-server!)
  (Thread/sleep 500) ;; Give time for cleanup
  (start-server!))

(defn started?
  "Check if server is started."
  []
  (:started? @server-state))

(defn- maybe-start-nrepl!
  "Start an nREPL server when SIMMIS_NREPL_PORT is set — bound to
   127.0.0.1 ONLY. An nREPL is full remote code execution, so the sole
   supported access path is an SSH tunnel to the host
   (`ssh -L <port>:localhost:<port> …`); the SSH key IS the auth layer.
   Deliberately opt-in via env so the freely-distributable uberjar
   never listens by default. This is the interactive inspect/fix loop
   for the dev deployment (dev.simm.is) — the same clj-nrepl-eval
   workflow as local development, against the live process."
  []
  (when-let [port (some-> (System/getenv "SIMMIS_NREPL_PORT") parse-long)]
    (let [start (requiring-resolve 'nrepl.server/start-server)]
      (start :bind "127.0.0.1" :port port)
      (log/log! {:level :info :id ::nrepl-started
                 :msg (str "nREPL on 127.0.0.1:" port " (SSH-tunnel access only)")}))))

;; Entry point for standalone usage
(defn -main [& _args]
  (log/log! {:level :info
             :id ::starting
             :msg "Starting Simmis server"
             :data {:url url
                    :jwt-configured? (some? (System/getenv "SIMMIS_JWT_SECRET"))}})
  (maybe-start-nrepl!)
  (start-server!)
  ;; Park the main thread — the server's own threads must not depend on
  ;; main staying alive by accident, and a returning -main would let the
  ;; JVM exit if everything else is daemonized.
  @(promise))
