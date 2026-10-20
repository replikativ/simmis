(ns is.simm.ops.dvergr
  "The one way simmis invokes a dvergr op (`dvergr.ops/invoke`): for a simmis
   party, with simmis's authorizer installed.

   dvergr asks its `:authorize` seam before every op it runs for a principal
   other than the process itself (`dvergr.principal`). Its default answers
   allow-all. simmis has no dvergr daemon, so it hands the seam over with
   each call: the daemon map carries `{:config {:auth {:authorize …}}}`, which
   `dvergr.principal/authorizer` prefers to anything process-wide. The
   authorizer is `access/dvergr-authorize` — dvergr's resource descriptors
   mapped onto `can?` (doc/authority.md, \"dvergr ops\").

   Deliberately per call and not process-wide (`principal/configure!`): the
   process-wide seam would also answer every registry-tool call of every agent
   in every room (`dvergr.tools/execute` asks it with the agent's principal),
   where dvergr classes `clojure_eval` as `:admin` on the room — an owner's act
   in simmis — while simmis gives a room's agents their sandbox by design
   (doc/authority.md, \"The sandbox and eval\"). Putting agents' tool calls
   behind `can?` is P2's (plan §2.4), not a side effect of this."
  (:require [datahike.api :as d]
            [dvergr.ops :as dops]
            [dvergr.principal :as principal]
            [malli.core :as m]
            [is.simm.model.access :as access]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.worker-credentials :as worker-credentials]))

(defn room-handle
  "Translate a canonical Simmis UUID through its current system-db row.
   UUID-shaped handles never fall back to a slug with the same spelling."
  [arg]
  (if-let [id (when (string? arg) (parse-uuid arg))]
    (when-let [conn (system-db/get-conn)]
      (d/q '[:find ?slug . :in $ ?id
             :where [?r :room/id ?id] [?r :room/slug ?slug]] @conn id))
    arg))

(defn external-arguments-valid?
  "Pure existing MCP call-shape check. Visibility requests have no :args KEY;
   present nil args are calls, not visibility. This grants no authority."
  [principal {:keys [op args] :as request}]
  (or (not (and (:principal/mcp? principal) (contains? request :args)))
      (and (m/validate (dops/malli-schema op) args)
           (m/validate [:map [:limit {:optional true} [:int {:min 1 :max 1000}]]]
                       args))))

(defn authorize
  "Shared authority with runtime argument validation for external MCP calls.
   Dvergr advertises schemas but does not validate them in invoke. Visibility
   filter requests have no :args and still use the normal resource predicate."
  [principal {:keys [op] :as request}]
  (if-not (external-arguments-valid? principal request)
    {:allow? false :reason "Invalid operation arguments"}
    (cond
      (= :worker (:principal/kind principal))
      (worker-credentials/authorize (system-db/get-conn) principal request)
      (contains? worker-credentials/worker-ops op)
      {:allow? false :reason "Worker transport operations require their separate worker credential"}
      :else (access/dvergr-authorize principal request))))

(defn party-principal
  "The dvergr principal of the simmis party `party-id`: a human that posts as
   the party's participant keyword (`:party/<uuid>`), with no MCP scopes."
  [party-id]
  (let [id (str "party/" party-id)]
    {:principal/id id
     :principal/kind :human
     :principal/actor (principal/actor {:principal/id id})
     :principal/scopes #{}}))

(defn host
  "The daemon map dvergr's ops run against for simmis: the execution context
   `ctx` they run in, and simmis's authorizer."
  [ctx]
  {:execution-ctx ctx
   :config {:auth {:authorize authorize}}})

(defn invoke
  "Run dvergr op `op` with `args` in execution context `ctx` for `principal`
   (`party-principal`, or `principal/local-principal` for the process itself,
   which dvergr never sends to the authorizer). Throws dvergr's 403 op error
   (`:type :dvergr.principal/denied`) when simmis's authorizer refuses."
  [ctx principal op args]
  (dops/invoke (host ctx) principal op args))
