(ns user
  "Development namespace - auto-loaded by Clojure REPL.

   Provides lifecycle management for:
   - nREPL server (port 47888) for CLJ access via clj-nrepl-eval
   - Shadow-cljs watcher with nREPL (port 9631) for CLJS access
   - Simmis web server (ws://localhost:47295)

   Those are the ports of the DEFAULT instance. SIMMIS_PORT_OFFSET moves all
   of them (and shadow's own 9630 and the page's 8080) by one number, so a
   second instance can run beside the first — see `is.simm.runtimes.instance`
   and \"Running a second instance\" in CLAUDE.md.

   Usage:
     clj -A:dev              # Auto-starts everything

   From clj-nrepl-eval:
     clj-nrepl-eval -p 47888 \"(user/restart-web!)\"
     clj-nrepl-eval -p 9631 \"(js/console.log \\\"test\\\")\"

   Lifecycle:
     (start!) (stop!) (restart-web!) (status)

   Everything in `repl` is referred here too, so reading the running system
   needs no requires: (rooms) (parties) (knowledge-bases) (room \"slug\")
   (room-conn \"slug\") (conn) (db) (q '[:find ...]). Call (help) for the list."
  (:require [nrepl.server :as nrepl]
            [cider.nrepl :refer [cider-nrepl-handler]]
            [clojure.java.shell]
            [is.simm.runtimes.instance :as instance]
            [taoensso.telemere :as log]))

;; State management
(defonce ^:private state
  (atom {:nrepl nil
         :shadow-started? false
         :web-started? false}))

;; =============================================================================
;; nREPL Server (port 47888 + offset)
;; =============================================================================

(defn start-nrepl!
  "Start the nREPL server on the instance's port (47888 + SIMMIS_PORT_OFFSET)."
  []
  (when-not (:nrepl @state)
    (let [port (instance/port :nrepl-clj)]
      (try
        (let [server (nrepl/start-server :port port :handler cider-nrepl-handler)]
          (swap! state assoc :nrepl server)
          (log/log! {:level :info :id ::nrepl-started :msg (str "nREPL started on port " port)})
          server)
        (catch java.net.BindException _
          (log/log! {:level :warn :id ::nrepl-port-in-use
                     :msg (str "Port " port " already in use, skipping nREPL start")})
          nil)))))

(defn stop-nrepl!
  "Stop nREPL server."
  []
  (when-let [server (:nrepl @state)]
    (nrepl/stop-server server)
    (swap! state assoc :nrepl nil)
    (log/log! {:level :info :id ::nrepl-stopped :msg "nREPL stopped"})))

;; =============================================================================
;; Shadow-cljs (watcher + nREPL on port 9631 + offset)
;; =============================================================================

(defn shadow-config
  "shadow-cljs.edn with this instance's ports applied.

   The file holds the default instance's ports, and `#shadow/env` can read a
   port but not add an offset to one, so the dev JVM sets them here instead:
   shadow's nREPL, its own server (9630, the devtools websocket), the page's
   :dev-http port, the app server that :dev-http proxies to, and the websocket
   port compiled into the client (`development-websocket-port`). Page and
   websocket must agree, which is why they are derived from one offset. With
   no offset every value equals what the file says.

   Applies to `clj -M:dev` only; `npm run dev` runs shadow from the file and
   is always the default instance. Editing shadow-cljs.edn while an offset
   instance runs makes shadow reload the FILE's ports — restart that instance
   afterwards."
  []
  (let [cfg ((requiring-resolve 'shadow.cljs.devtools.config/load-cljs-edn!))
        {:keys [nrepl-cljs shadow-http http websocket]} (instance/ports)
        ws-define 'is.simm.runtimes.web/development-websocket-port
        dev-http (-> cfg :dev-http vals first
                     (assoc :proxy-url (str "http://localhost:" websocket)))]
    (-> cfg
        (assoc-in [:nrepl :port] nrepl-cljs)
        (assoc-in [:http :port] shadow-http)
        (assoc :dev-http {http dev-http})
        (update :builds update-vals
                (fn [build]
                  (cond-> build
                    (get-in build [:dev :compiler-options :closure-defines ws-define])
                    (assoc-in [:dev :compiler-options :closure-defines ws-define]
                              (str websocket))))))))

(defn start-shadow!
  "Start shadow-cljs server and watch :app build, on this instance's ports."
  []
  (when-not (:shadow-started? @state)
    (require '[shadow.cljs.devtools.server :as shadow-server])
    (require '[shadow.cljs.devtools.api :as shadow])
    (let [cfg (shadow-config)]
      ((resolve 'shadow.cljs.devtools.server/start!) cfg)
      ((resolve 'shadow.cljs.devtools.api/watch) (get-in cfg [:builds :app]))
      (swap! state assoc :shadow-started? true)
      (log/log! {:level :info :id ::shadow-started
                 :msg (str "Shadow-cljs started, watching :app (CLJS nREPL on port "
                           (instance/port :nrepl-cljs) ", page on http://localhost:"
                           (instance/port :http) ")")}))))

(defn stop-shadow!
  "Stop shadow-cljs watcher and server."
  []
  (when (:shadow-started? @state)
    (require '[shadow.cljs.devtools.server :as shadow-server])
    (require '[shadow.cljs.devtools.api :as shadow])
    (try
      ((resolve 'shadow.cljs.devtools.api/stop-worker) :app)
      ((resolve 'shadow.cljs.devtools.server/stop!))
      ;; `stop!` returns before the server is down; shadow provides the
      ;; predicate to wait on, so a caller that restarts does not have to
      ;; guess an interval. This is what the `Thread/sleep 500` in
      ;; `restart-shadow!` and `refresh!` was standing in for.
      ((resolve 'shadow.cljs.devtools.server/wait-for-stop!))
      (catch Exception e
        (log/log! {:level :warn :id ::shadow-stop-error :msg "Error stopping shadow" :data {:error (.getMessage e)}})))
    (swap! state assoc :shadow-started? false)
    (log/log! {:level :info :id ::shadow-stopped :msg "Shadow-cljs stopped"})))

(defn restart-shadow!
  "Restart the shadow-cljs watcher.

   DO NOT CALL THIS OVER nREPL. Observed: it took the whole dev JVM with it —
   nREPL, the websocket server and the http server all went down, not just the
   watcher, leaving nothing to reconnect to. Restart the dev process from a
   shell instead.

   The wait now lives in `stop-shadow!`, which asks shadow when it is actually
   down rather than sleeping for an interval that happened to work."
  []
  (stop-shadow!)
  (start-shadow!))

;; =============================================================================
;; Web Server (ws://localhost:47295 + offset)
;; =============================================================================

(defn start-web!
  "Start the simmis web server."
  []
  (when-not (:web-started? @state)
    (require '[is.simm.runtimes.web :as web])
    ((resolve 'is.simm.runtimes.web/start-server!))
    (swap! state assoc :web-started? true)))

(defn stop-web!
  "Stop the simmis web server."
  []
  (when (:web-started? @state)
    (require '[is.simm.runtimes.web :as web])
    ((resolve 'is.simm.runtimes.web/stop-server!))
    (swap! state assoc :web-started? false)))

(defn restart-web!
  "Restart the simmis web server."
  []
  (require '[is.simm.runtimes.web :as web])
  ((resolve 'is.simm.runtimes.web/restart-server!))
  (swap! state assoc :web-started? true))

;; =============================================================================
;; Main Lifecycle
;; =============================================================================

(defn ensure-css!
  "Build `public/main.css` if it is missing or older than `core.css`.

   It is a GITIGNORED build artifact (Lightning CSS bundles `core.css` into
   it), so a fresh clone does not have one — and nothing in the dev startup
   built it. The app then serves a ZERO-BYTE stylesheet and renders as
   unstyled fragments: it works perfectly and looks broken, which is the worst
   possible first impression and exactly what a new contributor met.

   Also builds when STALE. Building only when absent left a checkout that
   never ran the css watcher (`npm run styles-dev`) serving a stylesheet six
   weeks behind `core.css`: every rule added since was missing, and nothing
   said so — an empty nav section meant to be `display: none` rendered as a
   bare heading. A stale check does not fight the watcher: while it runs, the
   output is never older than its source."
  []
  (let [f (java.io.File. "public/main.css")
        src (java.io.File. "src/is/simm/core.css")
        missing? (not (and (.exists f) (pos? (.length f))))]
    (when (or missing? (< (.lastModified f) (.lastModified src)))
      (log/log! {:level :info :id ::css-building
                 :msg (if missing?
                        "public/main.css missing — building it"
                        "public/main.css older than core.css — rebuilding it")})
      (try
        (let [{:keys [exit err]} (clojure.java.shell/sh "npm" "run" "build:css")]
          (if (zero? exit)
            (log/log! {:level :info :id ::css-built
                       :msg "Built public/main.css"})
            (log/log! {:level :warn :id ::css-build-failed
                       :msg (str "Could not build public/main.css — the UI will render "
                                 "unstyled. Run `npm install && npm run build:css`.")
                       :data {:exit exit :err (some-> err (subs 0 (min 300 (count err))))}})))
        (catch Throwable e
          (log/log! {:level :warn :id ::css-build-failed
                     :msg (str "Could not build public/main.css — the UI will render "
                               "unstyled. Run `npm install && npm run build:css`.")
                     :data {:error (ex-message e)}}))))))

(defn start!
  "Start all development services: nREPL, shadow-cljs, and web server."
  []
  (log/log! {:level :info :id ::dev-starting :msg "Starting development environment..."
             :data (instance/summary)})
  (ensure-css!)
  (start-nrepl!)
  ;; start-shadow! returns once the watcher is up; the web server binds its
  ;; own ports and needs nothing from it. A `future` + sleep here used to
  ;; "sequence" the two — it only managed to SWALLOW boot failures, leaving
  ;; a half-started stack that reported success.
  ;; Load the server graph BEFORE shadow starts, on this thread, and nothing
  ;; else. `shadow watch :app` compiles CLJS on its own thread pool, and
  ;; compiling our .cljc files loads CLJ namespaces for their MACROS — the same
  ;; subtree `start-web!` loads here. Two threads, one namespace graph, and
  ;; `clojure.core/ns` conj's onto `*loaded-libs*` when the ns HEADER finishes
  ;; rather than the body: the second thread sees a namespace already "loaded",
  ;; skips it, and compiles against vars that do not exist yet. `load-lib` then
  ;; `remove-ns`es the victim while its own `ns` form has already registered it,
  ;; leaving a lib in `*loaded-libs*` with NO namespace — after which plain
  ;; `require` is a silent no-op and the JVM stays poisoned until a `:reload`.
  ;; Observed as `No such var: fs/normalize-segments`, then
  ;; `namespace 'muschel.fs.mount' not found`, then the same thing one layer
  ;; deeper in geschichte, on every cold boot.
  ;;
  ;; This is why the ordering matters and a `future` here would bring the bug
  ;; straight back.
  (require '[is.simm.runtimes.web])
  (start-shadow!)
  (try
    (start-web!)
    (catch Throwable e
      (log/log! {:level :error :id ::web-start-failed
                 :msg "Web server failed to start" :data {:error (ex-message e)}})
      (throw e)))
  (log/log! {:level :info :id ::dev-started :msg "Development environment started"}))

(defn stop!
  "Stop all development services."
  []
  (log/log! {:level :info :id ::dev-stopping :msg "Stopping development environment..."})
  (stop-web!)
  (stop-shadow!)
  (stop-nrepl!)
  (log/log! {:level :info :id ::dev-stopped :msg "Development environment stopped"}))

(defn status
  "What is actually running, and is the bundle current.

   Delegates to `repl/status`, which PROBES the ports rather than reporting
   this namespace's own bookkeeping — `state` stays true after a component has
   died — and compares the bundle's mtime against the newest source, because a
   failed build leaves the previous bundle serving and looks identical to
   success from outside."
  []
  ((requiring-resolve 'repl/status)))

;; =============================================================================
;; Namespace Reloading
;; =============================================================================

(defn refresh!
  "Refresh changed namespaces and restart all services.

   KNOWN HAZARD, and it does not announce itself: reloading namespaces that
   define or consume datahike RECORDS gives you two classes with the same name
   from different classloaders. Nothing throws at reload time. What you see
   afterwards is every authenticated RPC answering `:not-authorized`, because
   a record instance created before the reload no longer satisfies the
   protocol checked after it. If that happens, restart the JVM — do not try to
   debug it as an auth problem.

   Prefer restarting the process. This is here for the cases where that is
   genuinely too slow."
  []
  (require '[clojure.tools.namespace.repl :as tn-repl])
  (stop!)
  ((resolve 'clojure.tools.namespace.repl/refresh) :after 'user/start!))

(defn reload-ns!
  "Reload one namespace.

   Usage: (reload-ns! 'is.simm.runtimes.web)

   Safe for a namespace of plain functions. Carries the same classloader
   hazard as `refresh!` for anything touching datahike records — see there.
   Note also that reloading a namespace does NOT re-run code that ran at load
   time elsewhere: reloading a handler namespace re-registers its handlers,
   but a router already built still holds the old ones."
  [ns-sym]
  (require ns-sym :reload))

;; =============================================================================
;; Reading the running system
;; =============================================================================
;;
;; Loaded here so a one-liner needs no requires:
;;   clj-nrepl-eval -p 47888 "(rooms)"
;; `repl` only READS; lifecycle stays in this namespace, where it is harder to
;; call by accident.

(require '[repl])
(doseq [sym '[rooms parties knowledge-bases room room-conn kb-conn conn db q help]]
  (intern 'user sym @(ns-resolve 'repl sym)))

;; =============================================================================
;; Auto-start on load
;; =============================================================================

(defn -main
  "Entry point - starts all services and keeps JVM alive."
  [& _args]
  (start!)
  ;; Keep the main thread alive
  @(promise))

;; Auto-start when namespace is loaded in REPL
(when-not (System/getProperty "user.skip-auto-start")
  (start!))
