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
            [is.simm.runtimes.workflow-schedules :as workflow-schedules]
            [org.replikativ.spindel.engine.impl.simple :as simple]))

(defonce ^:private installation (atom nil))

(defn- off-engine! []
  (when (simple/on-drain-thread?)
    (throw (ex-info "Application lifecycle must run off the engine drain" {:type ::engine-thread}))))

(defn host!
  "The exact installed host, only while ready and still canonically owned."
  []
  (off-engine!)
  (locking installation
    (let [{:keys [host phase]} @installation]
      (when-not (= :ready phase)
        (throw (ex-info "Application operations are not ready" {:type ::held})))
      (reads/current-host-control-db! host)
      host)))

(defn- release-all! [[release! & remaining]]
  (when release!
    (try (release!) (finally (release-all! remaining)))))

(defn quiesce!
  "Mark the installation non-ready and physically join scheduled admissions.
   Retain host and registrations until the remaining application shuts down."
  []
  (off-engine!)
  (locking installation
    (when @installation
      (swap! installation assoc :phase :stopping))
    ;; Lock order is installation -> scheduler. Its worker uses the captured
    ;; host directly, never host!, so physical join cannot reenter this lock.
    (workflow-schedules/stop!)))

(defn stop!
  "Join scheduled admissions before releasing their host or registrations.
   A failed physical join retains a non-ready installation for retry."
  []
  (off-engine!)
  (locking installation
    (quiesce!)
    (when @installation
      ;; Unknown cleanup is not release acknowledgement. Keep the non-ready
      ;; resource for an explicit retry if either unregister or host release
      ;; throws. The unregister functions are idempotent.
      (release-all! [worker-control/uninstall! worker-api/uninstall!
                     work-api/uninstall! results/uninstall!])
      (server/set-host! nil)
      (reset! installation nil))))

(defn start!
  "Install before the web peer accepts requests. A failed installation rolls
   back its registrations; a failed optional transport does not own them."
  []
  (off-engine!)
  (locking installation
    (if @installation
      (host!)
      (do
        ;; The application is the sole lifecycle coordinator. This observation
        ;; is not a reservation against arbitrary direct runtime/start! calls;
        ;; that runtime also enforces its own mode and owner checks.
        (when (workflow-schedules/held?)
          (throw (ex-info "Prior scheduled-work resource must be stopped first" {:type ::held})))
        (let [host (owned-mcp/host (system-db/get-conn) context/server-context)]
          (reads/current-host-control-db! host)
          (reset! installation {:host host :phase :installing})
          (try
            (results/install!)
            (work-api/install!)
            (worker-api/install!)
            (worker-control/install!)
            (server/set-host! host)
            (swap! installation assoc :phase :ready)
            host
            (catch Throwable error
              (try (stop!)
                   (catch Throwable cleanup-error
                     (.addSuppressed error cleanup-error)))
              (throw error))))))))
