(ns is.simm.runtimes.application-ops
  "Application-owned operation registrations shared by browser RPC and MCP.
   This process resource is not forkable room state or an MCP listener."
  (:require [dvergr.mcp.server :as server]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.owned-mcp :as owned-mcp]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.results :as results]
            [is.simm.ops.work-api :as work-api]
            [is.simm.ops.worker-api :as worker-api]
            [is.simm.ops.worker-control :as worker-control]
            [is.simm.runtimes.context :as context]
            [is.simm.runtimes.media-cache :as media-cache]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [is.simm.runtimes.workflow-schedules :as workflow-schedules]
            [org.replikativ.spindel.engine.impl.simple :as simple]))

(defonce ^:private installation (atom nil))
(defonce ^:private coordinator (Object.))

(defn- off-engine! []
  (when (simple/on-drain-thread?)
    (throw (ex-info "Application lifecycle must run off the engine drain" {:type ::engine-thread}))))

(defn host!
  "Non-owning lookup of the exact ready host. Use with-operation! for native
   access; the returned map alone does not retain application lifetime."
  []
  (off-engine!)
  (locking installation
    (let [{:keys [host owner]} @installation]
      (when-not (and owner (lifetime/ready? owner))
        (throw (ex-info "Application operations are not ready" {:type ::held})))
      (reads/current-host-control-db! host)
      host)))

(defn capture-operation
  "Capture only this installation's registration, without native access or a
   lifetime admission. Safe on the engine drain before queuing blocking work.
   The returned HOST-only thunk enters the ORIGINAL owner off-engine; a queued
   invocation can never borrow a replacement installation. Do not export it."
  []
  (let [{:keys [host owner]} (locking installation
                             (let [{:keys [owner] :as current} @installation]
                               (when-not (and owner (lifetime/ready? owner))
                                 (throw (ex-info "Application operations are not ready" {:type ::held})))
                               current))]
    (fn [f]
      (off-engine!)
      (when-not (fn? f)
        (throw (ex-info "Invalid application operation" {:type ::invalid-body})))
      (lifetime/with-operation! owner
        #(do (reads/current-host-control-db! host) (f host))))))

(defn with-operation!
  "Enter the captured installation BEFORE passing its host to synchronous work.
   A shutdown between capture and admission refuses, without using old handles."
  [f]
  (off-engine!)
  (let [{:keys [host owner]} (locking installation
                             (or @installation
                                 (throw (ex-info "Application operations are not ready" {:type ::held}))))]
    (lifetime/with-operation! owner
      #(do (reads/current-host-control-db! host) (f host)))))

(defn- lifecycle-entry! []
  (off-engine!)
  ;; Check BEFORE waiting for a lifecycle coordinator: it may already be held
  ;; by shutdown, which is waiting for this caller's physical operation exit.
  (when-let [owner (:owner @installation)] (lifetime/assert-not-owner! owner)))

(defn- release-all! [[release! & remaining]]
  (when release!
    (try (release!) (finally (release-all! remaining)))))

(defn- quiesce-owned! []
  (let [owner (locking installation
                (when-let [owner (:owner @installation)]
                  (lifetime/fence! owner)
                  owner))]
    ;; Neither physical join holds installation. Operations may finish/reenter
    ;; short lookups; their final acknowledgement needs no coordinator monitor.
    (workflow-schedules/stop!)
    (when owner (lifetime/join! owner 5000))))

(defn quiesce!
  "Fence admission and physically join schedules and synchronous HOST calls.
   Retain the original host/registrations on any unconfirmed physical exit."
  []
  (lifecycle-entry!)
  (locking coordinator (quiesce-owned!)))

(defn stop!
  "Join schedules and HOST operations before releasing host or registrations.
   A failed physical join retains a non-ready installation for retry."
  []
  (lifecycle-entry!)
  (locking coordinator
    (quiesce-owned!)
    (locking installation
      (when @installation
      ;; Unknown cleanup is not release acknowledgement. Keep the non-ready
      ;; resource for an explicit retry if either unregister or host release
      ;; throws. The unregister functions are idempotent.
        (release-all! [worker-control/uninstall! worker-api/uninstall!
                       work-api/uninstall! results/uninstall!
                       #(when-let [resource (:media-cache (:host @installation))]
                          (media-cache/close! resource))])
        (server/set-host! nil)
        (reset! installation nil)))))

(defn start!
  "Install before the web peer accepts requests. A failed installation rolls
   back its registrations; a failed optional transport does not own them."
  []
  (lifecycle-entry!)
  (locking coordinator
    (if @installation
      (host!)
      (do
        ;; Fail before admitting the App if its fixed incoming-sync handler
        ;; cannot load. Delayed namespace loading breaks the HOST/App cycle;
        ;; neither this symbol nor the scheduler callback is source-selected.
        (requiring-resolve 'is.simm.ops.git-polling/tick-owned!)
        ;; The application is the sole lifecycle coordinator. This observation
        ;; is not a reservation against arbitrary direct runtime/start! calls;
        ;; that runtime also enforces its own mode and owner checks.
        (when (workflow-schedules/held?)
          (throw (ex-info "Prior scheduled-work resource must be stopped first" {:type ::held})))
        (let [owner (lifetime/create)
              host (assoc (owned-mcp/host (system-db/get-conn) context/server-context owner)
                          :media-cache (media-cache/create))]
          (reads/current-host-control-db! host)
          (reset! installation {:host host :owner owner})
          (try
            (results/install!)
            (work-api/install!)
            (worker-api/install!)
            (worker-control/install!)
            (server/set-host! host)
            (lifetime/open! owner)
            host
            (catch Throwable error
              (try (stop!)
                   (catch Throwable cleanup-error
                     (.addSuppressed error cleanup-error)))
              (throw error))))))))
