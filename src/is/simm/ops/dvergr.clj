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
  (:require [dvergr.ops :as dops]
            [dvergr.principal :as principal]
            [is.simm.model.access :as access]))

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
   :config {:auth {:authorize access/dvergr-authorize}}})

(defn invoke
  "Run dvergr op `op` with `args` in execution context `ctx` for `principal`
   (`party-principal`, or `principal/local-principal` for the process itself,
   which dvergr never sends to the authorizer). Throws dvergr's 403 op error
   (`:type :dvergr.principal/denied`) when simmis's authorizer refuses."
  [ctx principal op args]
  (dops/invoke (host ctx) principal op args))
