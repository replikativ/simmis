(ns is.simm.uis.web.desktop.login
  "Landing page and login for Simmis.

   Renders a landing/intro page with login form before the WebSocket
   connection is established. On successful login, stores JWT in
   localStorage and triggers app init."
  (:require [clojure.string :as str]))

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:const TOKEN_KEY "simmis-access-token")
(def ^:const REFRESH_TOKEN_KEY "simmis-refresh-token")
(def ^:const USER_KEY "simmis-user")

;; Auth base URL - derived from the current page origin at runtime.
;; In dev, page is served on 8080 but auth API is on 47295.
;; In production, everything is on the same port.
(defonce auth-base-url (atom nil))

(defn get-auth-base-url
  "Get the auth API base URL. Uses explicitly set URL, or falls back to page origin."
  []
  (or @auth-base-url (.-origin js/location)))

;; =============================================================================
;; Token Management
;; =============================================================================

(defn get-stored-token
  "Get the stored access token from localStorage."
  []
  (.getItem js/localStorage TOKEN_KEY))

(defn auth-headers
  "Bearer header for the HTTP media routes (/blobs, /screen-frames,
   /screen-recordings).

   Those routes used to accept ANY caller — an anonymous POST wrote to our disk,
   and a stranger with a room uuid could trigger paid VLM calls. They are
   authenticated now, so every client call must carry the same token the
   websocket plane already uses. Returns {} when signed out, so the request
   fails with a clean 401 instead of a confusing success."
  []
  (if-let [t (get-stored-token)]
    #js {"Authorization" (str "Bearer " t)}
    #js {}))

(defn auth-header-map
  "Same, as a merge-able JS object for fetch :headers alongside Content-Type."
  [extra]
  (let [h (auth-headers)]
    (doseq [k (js/Object.keys extra)]
      (aset h k (aget extra k)))
    h))

(defn get-stored-refresh-token
  "Get the stored refresh token from localStorage."
  []
  (.getItem js/localStorage REFRESH_TOKEN_KEY))

(defn get-stored-user
  "Get the stored user info from localStorage."
  []
  (when-let [json (.getItem js/localStorage USER_KEY)]
    (try
      (js->clj (js/JSON.parse json) :keywordize-keys true)
      (catch :default _ nil))))

(defn store-auth!
  "Store authentication tokens and user info in localStorage."
  [{:keys [access_token refresh_token user]}]
  (.setItem js/localStorage TOKEN_KEY access_token)
  (when refresh_token
    (.setItem js/localStorage REFRESH_TOKEN_KEY refresh_token))
  (when user
    (.setItem js/localStorage USER_KEY (js/JSON.stringify (clj->js user)))))

;; =============================================================================
;; The blob cookie
;; =============================================================================
;;
;; The blob cookie holds a short-lived blob-READ token, not the access token (a
;; cookie reaches every port of this host, so it must not be worth a bearer
;; credential). It is HttpOnly, so this code never sees it: it only asks the
;; server for a fresh one, authenticating with the header.
;;
;; Max-Age runs on WALL-CLOCK time, timers do not: they pause while the machine
;; sleeps and are throttled in background tabs. So the renewal point is stored
;; as a wall-clock instant, and waking up — the tab becoming visible, the
;; window regaining focus, the network coming back — renews when it has passed.
;; The timer is only the fast path for a tab that stays awake.

(def ^:private blob-renew-at-fraction
  "Renew once this fraction of the cookie's lifetime has elapsed."
  0.8)

(defonce ^:private blob-cookie
  ;; :gen        bumped by every start/stop; a renewal from an older generation
  ;;             must not reschedule (e.g. one in flight across a logout)
  ;; :timer      the pending js/setTimeout id
  ;; :renew-at   wall-clock ms after which a wake-up renews
  ;; :in-flight  the pending renewal promise — at most one at a time
  ;; :refused?   the server said no (401): stop asking until the next start,
  ;;             the same no-loop behaviour as before
  ;; :listeners  [[target event f] …] actually registered, so a re-init (hot
  ;;             reload included — this is a defonce) removes exactly those
  (atom {:gen 0}))

(defn- clear-blob-timer! []
  (when-let [t (:timer @blob-cookie)] (js/clearTimeout t))
  (swap! blob-cookie assoc :timer nil))

(declare renew-blob-cookie!)

(defn- schedule-blob-renewal! [expires-in]
  (clear-blob-timer!)
  (let [delay-ms (* 1000 blob-renew-at-fraction expires-in)]
    (swap! blob-cookie assoc
           :renew-at (+ (js/Date.now) delay-ms)
           :timer (js/setTimeout renew-blob-cookie! delay-ms))))

(defn- renew-blob-cookie!
  "POST /auth/blob-cookie with the access token and reschedule. Resolves to
   true when the server granted a cookie, false otherwise; never rejects, so
   boot can await it without being blocked by a failure."
  []
  (or (:in-flight @blob-cookie)
      (let [gen (:gen @blob-cookie)
            current? #(= gen (:gen @blob-cookie))
            p (if-not (get-stored-token)
                (js/Promise.resolve false)
                (-> (js/fetch (str (get-auth-base-url) "/auth/blob-cookie")
                              #js {:method "POST" :headers (auth-headers)})
                    (.then (fn [resp]
                             (if (.-ok resp)
                               (-> (.json resp)
                                   (.then (fn [body]
                                            (let [ttl (aget body "expires_in")]
                                              (when (and (current?) (number? ttl) (pos? ttl))
                                                (schedule-blob-renewal! ttl))
                                              true))))
                               (do (js/console.warn "[Auth] blob cookie renewal refused:"
                                                    (.-status resp))
                                   (when (current?)
                                     (clear-blob-timer!)
                                     (swap! blob-cookie assoc :refused? true))
                                   false))))
                    ;; Network failure: not a refusal. `renew-at` stays where it
                    ;; was, so the next wake-up (`online` included) tries again.
                    (.catch (fn [e]
                              (js/console.warn "[Auth] blob cookie renewal failed:" e)
                              false))))
            p (.then p (fn [granted?]
                         (when (current?) (swap! blob-cookie dissoc :in-flight))
                         granted?))]
        (swap! blob-cookie assoc :in-flight p)
        p)))

(defn- renew-blob-cookie-if-due! []
  (let [{:keys [renew-at refused? listeners]} @blob-cookie]
    (when (and listeners
               (not refused?)
               (or (nil? renew-at) (>= (js/Date.now) renew-at)))
      (renew-blob-cookie!))))

(defn stop-blob-cookie-renewal!
  "Cancel the schedule and remove the wake-up listeners."
  []
  (clear-blob-timer!)
  (doseq [[target event f] (:listeners @blob-cookie)]
    (.removeEventListener target event f))
  (swap! blob-cookie (fn [s] (-> s
                                 (update :gen inc)
                                 (assoc :listeners nil :renew-at nil :refused? false)
                                 (dissoc :in-flight)))))

(defn start-blob-cookie-renewal!
  "Issue the blob cookie now and keep it fresh for as long as the server grants
   it. Replaces any previous schedule and listeners, so calling it again (a
   re-init, a hot reload) never duplicates them. Returns the first renewal's
   promise — await it before rendering, so the first `<img src=\"/blobs/…\">`
   does not race a cookie that expired while the tab was closed."
  []
  (stop-blob-cookie-renewal!)
  (let [on-visible (fn [_]
                     (when (= "visible" (.-visibilityState js/document))
                       (renew-blob-cookie-if-due!)))
        on-wake (fn [_] (renew-blob-cookie-if-due!))
        listeners [[js/document "visibilitychange" on-visible]
                   [js/window "focus" on-wake]
                   [js/window "online" on-wake]]]
    (doseq [[target event f] listeners]
      (.addEventListener target event f))
    (swap! blob-cookie assoc :listeners listeners)
    (renew-blob-cookie!)))

(defn clear-auth!
  "Clear stored authentication data and end the session on the server.

   The local half is synchronous. The server half — POST /auth/logout, which
   deletes the refresh session and expires the blob cookie — is the only way
   to clear that cookie at all: it is HttpOnly, so script cannot touch it.
   `keepalive` lets the request finish even if the caller navigates away, but
   callers that reload should still wait on the returned promise, which never
   rejects."
  []
  (let [rt (get-stored-refresh-token)]
    (stop-blob-cookie-renewal!)
    (.removeItem js/localStorage TOKEN_KEY)
    (.removeItem js/localStorage REFRESH_TOKEN_KEY)
    (.removeItem js/localStorage USER_KEY)
    (-> (js/fetch (str (get-auth-base-url) "/auth/logout")
                  (clj->js {:method "POST"
                            :keepalive true
                            :headers {"Content-Type" "application/json"}
                            :body (js/JSON.stringify
                                    (clj->js {:refresh_token rt}))}))
        (.then (constantly nil))
        (.catch (fn [e]
                  (js/console.warn "[Auth] server logout failed:" e)
                  nil)))))

;; =============================================================================
;; API
;; =============================================================================

(defn login!
  "Attempt login with email and password. Returns a promise."
  [email password]
  (-> (js/fetch (str (get-auth-base-url) "/auth/login")
                (clj->js {:method "POST"
                          :headers {"Content-Type" "application/json"}
                          :body (js/JSON.stringify
                                  (clj->js {:email email :password password}))}))
      (.then (fn [resp]
               (if (.-ok resp)
                 (.json resp)
                 (-> (.json resp)
                     (.then (fn [body]
                              (throw (js/Error.
                                       (or (aget body "message")
                                           (aget body "error")
                                           "Login failed")))))))))
      (.then (fn [data]
               (let [result (js->clj data :keywordize-keys true)]
                 (store-auth! result)
                 result)))))

(defn refresh-token!
  "Attempt to refresh the access token. Returns a promise."
  [refresh-token]
  (-> (js/fetch (str (get-auth-base-url) "/auth/refresh")
                (clj->js {:method "POST"
                          :headers {"Content-Type" "application/json"}
                          :body (js/JSON.stringify
                                  (clj->js {:refresh_token refresh-token}))}))
      (.then (fn [resp]
               (if (.-ok resp)
                 (.json resp)
                 (throw (js/Error. "Token refresh failed")))))
      (.then (fn [data]
               (let [result (js->clj data :keywordize-keys true)]
                 (.setItem js/localStorage TOKEN_KEY (:access_token result))
                 result)))))

(defn- b64url->str [s]
  (let [b64 (-> s (str/replace "-" "+") (str/replace "_" "/"))
        pad (case (mod (count b64) 4) 2 "==" 3 "=" "")]
    (js/atob (str b64 pad))))

(defn token-expired?
  "True if the JWT access `token` is at/near expiry (default 60s skew) or its
   :exp can't be read. Used to decide whether to refresh BEFORE connecting, so a
   returning user with a still-valid refresh token never lands on the login page
   (or connects anonymously) just because the short-lived access token lapsed."
  ([token] (token-expired? token 60))
  ([token skew-s]
   (let [exp (try
               (-> token (str/split #"\.") (nth 1)
                   b64url->str js/JSON.parse (aget "exp"))
               (catch :default _ nil))]
     (or (nil? exp) (< (- exp skew-s) (/ (js/Date.now) 1000))))))

;; =============================================================================
;; UI Rendering (vanilla DOM — pre-WebSocket)
;; =============================================================================

(defn- create-element
  "Create a DOM element with attributes and children."
  [tag attrs & children]
  (let [el (js/document.createElement tag)]
    (doseq [[k v] attrs]
      (case k
        :class (set! (.-className el) v)
        :type (.setAttribute el "type" v)
        :placeholder (.setAttribute el "placeholder" v)
        :for (.setAttribute el "for" v)
        :id (.setAttribute el "id" v)
        ;; NOTE: this `case` silently DROPS any key it does not list — an
        ;; unhandled attribute is not an error, it just never reaches the DOM.
        :autocomplete (.setAttribute el "autocomplete" v)
        :src (.setAttribute el "src" v)
        :alt (.setAttribute el "alt" v)
        :href (.setAttribute el "href" v)
        :disabled (when v (set! (.-disabled el) true))
        :style (set! (.. el -style -cssText) v)
        nil))
    (doseq [child children]
      (when child
        (if (string? child)
          (.appendChild el (js/document.createTextNode child))
          (.appendChild el child))))
    el))

(defn- render-hero-section
  "Render the hero section with logo, tagline, and description."
  []
  (create-element "section" {:class "landing-hero"}
    (create-element "div" {:class "landing-hero-content"}
      ;; Logo
      (create-element "div" {:class "landing-logo"}
        (create-element "img" {:src "/logo.png" :alt "Simmis"}))
      ;; Tagline
      (create-element "h1" {:class "landing-title"} "Simmis")
      (create-element "p" {:class "landing-tagline"} "Think it through"))))

(defn- render-login-form
  "Render the login form section. Returns [form-element, show-error-fn]."
  [on-success]
  (let [error-el (atom nil)
        email-input (atom nil)
        password-input (atom nil)
        submit-btn (atom nil)

        show-error!
        (fn [msg]
          (when @error-el
            (set! (.-textContent @error-el) msg)
            (set! (.. @error-el -style -display) "block")))

        hide-error!
        (fn []
          (when @error-el
            (set! (.. @error-el -style -display) "none")))

        set-loading!
        (fn [loading?]
          (when @submit-btn
            (set! (.-disabled @submit-btn) loading?)
            (set! (.-textContent @submit-btn)
                  (if loading? "Signing in..." "Sign in"))))

        handle-submit!
        (fn [e]
          (.preventDefault e)
          (let [email (str/trim (.-value @email-input))
                password (.-value @password-input)]
            (if (or (str/blank? email) (str/blank? password))
              (show-error! "Please enter email and password")
              (do
                (hide-error!)
                (set-loading! true)
                (-> (login! email password)
                    (.then (fn [result]
                             (set-loading! false)
                             (on-success result)))
                    (.catch (fn [err]
                              (set-loading! false)
                              (show-error! (.-message err)))))))))

        section
        (create-element "section" {:class "landing-login"}
          (create-element "div" {:class "login-card"}
            (create-element "h2" {:class "login-title"} "Sign in")
            (create-element "p" {:class "login-subtitle"} "to continue to Simmis")
            ;; Error message (hidden initially)
            (let [err (create-element "div" {:class "login-error"
                                             :style "display: none"})]
              (reset! error-el err)
              err)
            ;; Form
            (let [form (create-element "form" {:class "login-form"}
                         ;; Email field
                         (create-element "div" {:class "login-field"}
                           (create-element "label" {:for "login-email"} "Email or username")
                           ;; `type="text"`, not `"email"`. The server matches this
                           ;; against `:party/email` as an OPAQUE STRING — no format
                           ;; check anywhere in the auth path — so accounts may be
                           ;; provisioned with a bare handle. With `type="email"` the
                           ;; BROWSER refused to submit those before `handle-submit!`
                           ;; ever ran, which made such an account unreachable through
                           ;; the UI while looking perfectly valid in the config.
                           (let [input (create-element "input"
                                         {:type "text"
                                          :id "login-email"
                                          :autocomplete "username"
                                          :placeholder "you@example.com"})]
                             (reset! email-input input)
                             input))
                         ;; Password field
                         (create-element "div" {:class "login-field"}
                           (create-element "label" {:for "login-password"} "Password")
                           (let [input (create-element "input"
                                         {:type "password"
                                          :id "login-password"
                                          :placeholder "Enter your password"})]
                             (reset! password-input input)
                             input))
                         ;; Submit button
                         (let [btn (create-element "button"
                                     {:class "login-submit"
                                      :type "submit"}
                                     "Sign in")]
                           (reset! submit-btn btn)
                           btn))]
              (.addEventListener form "submit" handle-submit!)
              form)
            ;; Footer
            (create-element "p" {:class "login-footer"}
              ;; Was "Alpha access only", which told a self-hosting reader
              ;; nothing and read as a closed door. On a first run the useful
              ;; fact is WHERE accounts come from — there is no sign-up.
              "Accounts are configured in config.local.edn")))]
    section))

(defn render-login-page!
  "Render the landing page with login form into the container.
   Calls on-success with the login result when auth succeeds."
  [container on-success]
  (set! (.-innerHTML container) "")
  (let [page (create-element "div" {:class "landing-container"}
               (render-hero-section)
               (render-login-form on-success)
               ;; Footer
               (create-element "footer" {:class "landing-footer"}
                 (create-element "p" {} "simm.is")))]
    (.appendChild container page)))

;; =============================================================================
;; Auto-Login Check
;; =============================================================================

(defn check-existing-auth
  "Check if there's a valid stored token. Returns a promise that resolves
   to {:token token :user user} or rejects if no valid auth."
  []
  (js/Promise.
    (fn [resolve reject]
      (let [token (get-stored-token)
            refresh (get-stored-refresh-token)
            do-refresh (fn []
                         (-> (refresh-token! refresh)
                             (.then (fn [result]
                                      (resolve {:token (:access_token result)
                                                :user (get-stored-user)})))
                             (.catch (fn [_]
                                       (clear-auth!)
                                       (reject (js/Error. "No valid auth"))))))]
        (cond
          ;; ASK THE SERVER FIRST when we can. `token-expired?` decodes `exp`
          ;; locally; it cannot check the signature, so it proves only that the
          ;; token has not lapsed — never that the server still honours it.
          ;;
          ;; After a JWT-secret rotation or a server reseed, a stale token looks
          ;; perfectly valid here. The app then boots past the login screen, the
          ;; websocket presents the dead token, `:permissive true` degrades the
          ;; socket to ANONYMOUS instead of erroring (so the `:on-error` →
          ;; reauth path never fires), the store subscribe is denied, and the
          ;; user is left staring at a blank page with nothing in the console.
          ;; Measured on dev.simm.is: server logged `:auth-failed` then
          ;; `:pubsub/subscription-denied`; the client logged neither.
          ;;
          ;; A refresh round trip is the only validation the client has, so
          ;; spend it: the server decides, and a rejected refresh lands the user
          ;; on the login page instead of a void.
          refresh (do-refresh)
          ;; No refresh token — the local expiry check is the best we can do
          ;; alone. Still better than nothing, and the runtime guard below
          ;; (`authenticated-principal`) catches what it misses.
          (and token (not (token-expired? token)))
          (resolve {:token token :user (get-stored-user)})
          ;; nothing usable → login page
          :else (reject (js/Error. "No stored auth")))))))
