(ns is.simm.runtimes.web
  (:require ;; kabel
            [superv.async :refer [S simple-supervisor] :refer-macros [go-try <?]]
            [clojure.core.async :refer [<! promise-chan put! close!] :refer-macros [go]]
            [kabel.peer :as peer]
            [is.simm.distributed-scope :refer [remote-middleware invoke-on-peer connect-distributed-scope]]
            ;; Authentication middleware
            [kabel.auth.websocket :as ws-auth]
            ;; konserve-sync for store replication
            [konserve-sync.core :as sync]
            ;; Datahike's CBOR middleware for kabel serialization
            [datahike.kabel.cbor-handlers :refer [datahike-cbor-middleware]]
            ;; spindel runtime - context from bootstrap preload
            [is.simm.runtimes.bootstrap :as bootstrap]
            ;; Login page
            [is.simm.uis.web.desktop.login :as login]
            ;; Signals for current user
            [is.simm.uis.web.desktop.signals :as sig]
            [org.replikativ.spindel.engine.core :as rtc]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.runtimes.web-url :as web-url]
            [clojure.string]
            [taoensso.telemere :as tel]
            [taoensso.timbre :as timbre]
            [taoensso.trove :as trove]
            [taoensso.trove.console :as trove-console]))

(goog-define ^string development-websocket-port "")

(def url
  "Server WebSocket URL, derived from the page origin.

   - https page (deployment behind nginx/TLS, e.g. dev.simm.is): SAME
     ORIGIN — `wss://<host[:port]>`; nginx terminates TLS on 443 and
     proxies (with Upgrade headers) to the composite server port. A
     hardcoded :47295 here would make the browser dial a port the
     firewall doesn't expose.
   - http page (local dev, SSH tunnel, tailnet): the page comes from
     shadow's 8080 while the kabel server listens on 47295 — keep the
     fixed-port convention, only the host follows the page."
  (let [loc (.-location js/window)]
    (web-url/websocket-url {:protocol (.-protocol loc)
                            :hostname (.-hostname loc)
                            :host (.-host loc)}
                           development-websocket-port)))

;; Auth goes to the PAGE'S OWN ORIGIN, which is what `login/get-auth-base-url`
;; already falls back to — so there is nothing to set here.
;;
;; This used to derive the auth URL from the websocket URL. Over https that is
;; the page origin anyway and the override did nothing; over http it pointed at
;; `http://<host>:47295` while the page came from shadow's 8080, making
;; `/auth/login` a CROSS-ORIGIN request. A cross-origin `fetch` with the default
;; credentials mode does not store a `Set-Cookie` at all, so the blob cookie
;; issued at login was silently dropped in development and every `<img
;; src="/blobs/…">` 401'd — while working in production, which is the worst
;; shape for a bug to have.
;;
;; Shadow proxies non-file requests to the app server (`:dev-http :proxy-url`),
;; so `/auth/*` on :8080 reaches the same handler. Same origin everywhere means
;; no credentialed CORS, no origin allowlist, and dev behaving like production.

(def server-id #uuid "05a06e85-e7ca-4213-9fe5-04ae511e50a0")

;; Authenticated principal - set from login response
(defonce authenticated-principal (atom nil))

;; A client session owns its auth result. Callbacks capture this channel and id
;; lexically, so a late result from a replaced socket cannot release the new
;; session's bootstrap.
(defonce ^:private current-session* (atom nil))

(defn current-session [] @current-session*)

(defn current-session?
  [session]
  (= (:id session) (:id @current-session*)))

(defn stop-session!
  "Abort all supervised work owned by a client session and remove its peer
   registry entry. Client peers connect directly rather than through
   `peer/start`, so `peer/stop` is not their lifecycle boundary."
  [{:keys [client supervisor] :as session}]
  (when (and session (current-session? session))
    (reset! current-session* nil))
  (when supervisor
    (doseq [abort-ch (:aborts supervisor)]
      (put! abort-ch :abort)
      (close! abort-ch)))
  (when client
    (peer/unregister-peer! (:id @client))))

;; Use the shared execution context from bootstrap (preloaded before this ns)
;; This ensures signals can be created at namespace load time
(def execution-context bootstrap/execution-context)

;; Client peer - lazily created with JWT token
(defonce client (atom nil))
(defonce invocation-loop (atom nil))

(defn reauth-and-reload!
  "One-shot: refresh the access token and reboot with it. The guard lives in
   sessionStorage so it survives the reload and can't loop; after one failed
   retry, clear auth and fall back to login. Shared by the WS-auth `:on-error`
   handshake path and the per-RPC auth-denial recovery path (a `:permissive`
   socket that went anonymous when the short-lived token expired)."
  []
  (if (= "1" (.getItem js/sessionStorage "simmis-reauth-tried"))
    (do (.removeItem js/sessionStorage "simmis-reauth-tried")
        (login/clear-auth!)
        (js/location.reload))
    (if-let [rt (login/get-stored-refresh-token)]
      (do (.setItem js/sessionStorage "simmis-reauth-tried" "1")
          (-> (login/refresh-token! rt)
              (.then (fn [_] (js/location.reload)))
              (.catch (fn [_] (login/clear-auth!) (js/location.reload)))))
      (do (login/clear-auth!) (js/location.reload)))))

(defn maybe-reauth-on-rpc-error!
  "Recover the SESSION only when `err` is an AUTHENTICATION failure — the
   signature of a stale-token socket that reconnected anonymously
   (`:authentication-required`). Refreshes the token + reloads, returns true.

   A `:not-authorized` is DIFFERENT: the socket is authenticated but the party
   is genuinely forbidden (e.g. writing to a KB they can only read). That is NOT
   a session problem — reloading/re-logging in won't help and just dumps the user
   at the login screen. Return false so the caller surfaces a normal error.
   (distributed-scope emits the two distinct :type/message values.)"
  [err]
  (let [s (str (some-> err ex-data) " " (some-> err ex-message))]
    (if (re-find #"authentication-required" s)
      (do (js/console.warn "[Auth] session expired — refreshing:" (clj->js err))
          (reauth-and-reload!)
          true)
      false)))

(defn wipe-all-local-data!
  "Delete every `simmis-*` client IndexedDB (the system replica plus all room and
   KB replicas) and reload. The manual escape hatch for when a corrupt or
   version-incompatible local replica makes the app unreloadable — e.g. the
   `-blob-exists?` self-heal wedge. The durable server copy is untouched, so a
   full fresh resync follows the reload."
  []
  (-> (.databases js/indexedDB)
      (.then (fn [dbs]
               (let [names (->> (array-seq dbs)
                                (map #(.-name %))
                                (filter #(and % (.startsWith ^js % "simmis-"))))]
                 (js/console.warn "[Reset] wiping local databases:" (clj->js names))
                 (js/Promise.all
                   (into-array
                     (map (fn [n]
                            (js/Promise.
                              (fn [res _]
                                (let [req (.deleteDatabase js/indexedDB n)]
                                  (set! (.-onsuccess req) (fn [_] (res n)))
                                  (set! (.-onerror req) (fn [_] (res n)))
                                  (set! (.-onblocked req) (fn [_] (res n)))))))
                          names))))))
      (.then (fn [_] (js/location.reload)))
      (.catch (fn [_] (js/location.reload)))))

(defn create-client!
  "Create the kabel client peer with the given JWT token.
   Must be called after successful login."
  [token]
  (when-let [old-session @current-session*]
    ;; Invalidate the generation before stopping it. An already queued callback
    ;; from the old socket can no longer complete the replacement session.
    (reset! current-session* nil)
    (stop-session! old-session))
  (let [session-id (random-uuid)
        session-supervisor (simple-supervisor)
        auth-result (promise-chan)
        session-ref (atom nil)
        current? #(current-session? @session-ref)
        peer (peer/client-peer session-supervisor (random-uuid)
                               (comp remote-middleware
                                     (sync/client-middleware)
                                     (ws-auth/auth-middleware
                                       {:authenticate
                                        {:token token
                                         :on-auth
                                         (fn [principal]
                                           (when (current?)
                                             (.removeItem js/sessionStorage "simmis-reauth-tried")
                                             (reset! authenticated-principal principal)
                                             (put! auth-result {:status :authenticated
                                                                :principal principal})))
                                         :on-error
                                         (fn [err]
                                           (when (current?)
                                             ;; Publish terminal failure before refresh/reload.
                                             (put! auth-result {:status :failed :error err})
                                             (js/console.warn "[Auth] WS auth rejected:" (clj->js err))
                                             (reauth-and-reload!)))}
                                        :permissive true}))
                               datahike-cbor-middleware)
        session {:id session-id
                 :client peer
                 :supervisor session-supervisor
                 :auth-result auth-result}]
    (reset! session-ref session)
    (reset! current-session* session)
    (reset! client peer)
    (reset! invocation-loop (invoke-on-peer peer {:supervisor session-supervisor}))
    session))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn show-app!
  "Show the app container and hide the login container."
  []
  (when-let [login-el (js/document.getElementById "login")]
    (set! (.-style.display login-el) "none"))
  (when-let [app-el (js/document.getElementById "app")]
    (set! (.-style.display app-el) "")))

(defn show-login!
  "Show the login container and hide the app container."
  []
  (when-let [app-el (js/document.getElementById "app")]
    (set! (.-style.display app-el) "none"))
  (when-let [login-el (js/document.getElementById "login")]
    (set! (.-style.display login-el) "")))

(defn start-app!
  "Start the main application after successful auth.
   Called by main.cljs init function."
  [token user]
  (reset! authenticated-principal nil)
  ;; Set current-user signal for UI
  (binding [rtc/*execution-context* runtime]
    (reset! sig/current-user user))
  (let [session (create-client! token)]
    (show-app!)
    session))

(defn ^:export init []
  ;; Telemere level: :info by default so DevTools isn't flooded by
  ;; spindel-engine / konserve / datahike trace lines on every fire —
  ;; that volume dominates Enter-keypress-to-paint latency in profiles.
  ;; Bump back to :trace from the REPL when you need the deep traces:
  ;;   (taoensso.telemere/set-min-level! :trace)
  (tel/set-min-level! :info)
  ;; spindel, datahike, konserve log through `taoensso.trove`, not
  ;; telemere — so telemere's min-level does nothing for them. Set a
  ;; trove console log-fn with `:min-level :info` to silence the
  ;; per-event `:trace` engine logs. Lift this from the REPL via:
  ;;   (taoensso.trove/set-log-fn!
  ;;     (taoensso.trove.console/get-log-fn {:min-level :trace}))
  (trove/set-log-fn! (trove-console/get-log-fn {:min-level :info}))
  ;; Suppress Timbre debug logs to avoid stack overflow in datahike kabel code
  (timbre/set-level! :warn)

  (js/console.log "[Auth] Checking for existing authentication...")

  ;; Check for existing auth token
  (-> (login/check-existing-auth)
      (.then (fn [{:keys [token user]}]
               (js/console.log "[Auth] Found existing token, starting app...")
               (let [session (start-app! token user)]
               ;; Require main.cljs to continue initialization
               ;; This is done lazily to avoid circular deps
                 (when-let [main-init (aget js/window "__simmis_main_init")]
                   (main-init session)))))
      (.catch (fn [_]
               (js/console.log "[Auth] No valid auth, showing login page...")
               (show-login!)
               (login/render-login-page!
                 (js/document.getElementById "login")
                 (fn [result]
                   (let [{:keys [access_token user]} (js->clj result :keywordize-keys true)]
                     (js/console.log "[Auth] Login successful, starting app...")
                     (let [session (start-app! access_token user)]
                       ;; Trigger main init
                       (when-let [main-init (aget js/window "__simmis_main_init")]
                         (main-init session))))))))))
