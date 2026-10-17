(ns is.simm.runtimes.application-ops
  "Application-owned operation registrations shared by browser RPC and MCP.
   This process resource is not forkable room state or an MCP listener."
  (:require [dvergr.mcp.server :as server]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.results :as results]
            [is.simm.ops.work-api :as work-api]
            [is.simm.ops.worker-api :as worker-api]
            [is.simm.ops.worker-control :as worker-control]
            [is.simm.runtimes.context :as context]))

(defonce ^:private installed? (atom false))

(defn- release-all! [[release! & remaining]]
  (when release!
    (try (release!) (finally (release-all! remaining)))))

(defn stop!
  "Release application operations at full application shutdown, not MCP stop."
  []
  (locking installed?
    (when @installed?
      (try
        (release-all! [worker-control/uninstall! worker-api/uninstall!
                       work-api/uninstall! results/uninstall!])
        (finally
          (try (server/set-host! nil)
               (finally (reset! installed? false))))))))

(defn start!
  "Install before the web peer accepts requests. A failed installation rolls
   back its registrations; a failed optional transport does not own them."
  []
  (locking installed?
    (when-not @installed?
      (reset! installed? true)
      (try
        (results/install!)
        (work-api/install!)
        (worker-api/install!)
        (worker-control/install!)
        (server/set-host! (ops/host context/server-context))
        (catch Throwable e
          (stop!)
          (throw e))))))
