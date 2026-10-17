(ns is.simm.ops.worker-api
  "Narrow MCP bindings for separately authenticated external workers.
   Installing these operations does NOT enable their transport authenticator."
  (:require [dvergr.mcp.server :as server]
            [dvergr.ops :as ops]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.worker-service :as service]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- invoke [host operation args]
  (binding [ec/*execution-context* (:execution-ctx host)]
    (try
      (operation (system-db/get-conn) ops/*principal* args)
      (catch clojure.lang.ExceptionInfo error
        (if-let [status (get {:is.simm.ops.worker-service/invalid 400
                              :is.simm.ops.worker-service/denied 403
                              :is.simm.ops.worker-service/missing 404
                              :is.simm.ops.worker-files/invalid 400
                              :is.simm.ops.worker-files/unavailable 503
                              :is.simm.model.workers/invalid 400
                              :is.simm.model.workers/corrupt 503
                              :is.simm.model.workers/denied 403
                              :is.simm.model.workers/missing 404
                              :is.simm.model.workers/conflict 409
                              :is.simm.model.workers/busy 409} (:type (ex-data error)))]
          (ops/op-error! (ex-message error) (:type (ex-data error)) status)
          (throw error))))))

(def specifications
  (into {}
        (map (fn [[op kind schema operation description]]
               [op {:kind kind :schema schema
                    ;; Worker-only authorization resolves the durable enrollment
                    ;; and exact Run, never a caller-supplied room handle.
                    :authz {:action (if (= :read kind) :read :write) :resource (constantly [:global])}
                    :doc description :impl (fn [host args] (invoke host operation args))}]))
        [[:simmis/worker-pending :read service/pending-schema service/pending
          "List at most ten queued Run UUIDs for this enrollment only. No tasks, files, other workers or room discovery. Persist the selected Run, nonce and lease secret locally BEFORE claim."]
         [:simmis/worker-claim :write service/claim-schema service/claim!
          "Claim one selected Run for this enrolled worker. Persist Run, nonce and proof first: the nonce is also the lease UUID, allowing shutdown after a lost reply/revocation. Lease proof comes from the authentication header, NEVER arguments. One physical slot per enrollment; expiry does not release it. Check a successful claim's cancellation/stop state before executing; an ambiguous reply never authorizes execution."]
         [:simmis/worker-input :read service/input-schema service/input
          "Read <=16384 bytes of this lease's immutable selected-file input JSON as base64. Validate input id, full SHA256, size, offsets and individual file digests before materialization. No current-room or host-file read."]
         [:simmis/worker-heartbeat :write service/run-schema service/heartbeat!
          "Renew this owned lease while enrollment and current room authority permit it. Expiry, revocation or cancellation requests physical shutdown; never extends such a lease or reports it stopped automatically."]
         [:simmis/worker-complete :write service/complete-schema service/complete!
          "Submit bounded regular-file output AFTER the CLI and all descendants stopped. Only selected files and out/ deliverables; server checks size/digests/paths and owns import/verification. Immutable identical retries acknowledge one receipt; a changed result conflicts. Worker cannot certify an Attempt or merge."]
         [:simmis/worker-stop :write service/run-schema service/stop!
          "Acknowledge this lease's physical process-tree shutdown without publishing output. Remains available to the existing lease proof after expiry or revocation; grants no new execution, input, room or result access."]]))

(defn install! []
  (doseq [[op spec] specifications] (ops/register-op! op spec))
  (server/sync-op-tools!))

(defn uninstall! []
  (doseq [op (keys specifications)] (ops/unregister-op! op))
  (server/sync-op-tools!))
