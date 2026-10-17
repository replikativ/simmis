(ns is.simm.runtimes.application-ops
  "Application-owned operation registrations shared by browser RPC and MCP.
   This process resource is not forkable room state or an MCP listener."
  (:require [dvergr.mcp.server :as server]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.results :as results]
            [is.simm.ops.work-api :as work-api]
            [is.simm.runtimes.context :as context]))

(defonce ^:private installed? (atom false))

(defn stop!
  "Release application operations at full application shutdown, not MCP stop."
  []
  (locking installed?
    (when @installed?
      (try
        (work-api/uninstall!)
        (finally
          (try (results/uninstall!)
               (finally
                 (server/set-host! nil)
                 (reset! installed? false))))))))

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
        (server/set-host! (ops/host context/server-context))
        (catch Throwable e
          (stop!)
          (throw e))))))
