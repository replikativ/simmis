(ns is.simm.ops.owned-reads
  "Fixed transport-neutral HOST canonical human read service. No configurable
   resolver, activation, writer action, selected fallback or retained authority."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [is.simm.agents.room-agents :as rooms]
            [is.simm.model.access :as access]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.runtimes.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.impl.simple :as simple]))

(defn- refuse! []
  (throw (ex-info "Canonical room read refused" {:type ::refused})))

(defn- bounded-text? [value maximum]
  (and (string? value) (<= 1 (count value) maximum)
       (not (str/blank? value)) (not (re-find #"[\p{Cntrl}]" value))))

(defn current-control-db!
  "HOST INTERNAL ownership guard shared with transport delegation. No endpoint
   or second resolver. Requires the actual current canonical connection/server
   context and physical host worker. Returned DB is ephemeral, not a lease."
  [conn host-ctx]
  (when (or (simple/on-drain-thread?)
            (not (and conn host-ctx (identical? conn (system-db/get-conn))
                      (identical? host-ctx context/server-context))))
    (refuse!))
  @conn)

(defn- human! [db principal]
  (let [candidate (when (and (= :human (:principal/kind principal))
                             (string? (:principal/id principal)))
                    (access/actor->party-uuid (:principal/id principal)))
        party (when (and (uuid? candidate) (= (:principal/id principal) (str "party/" candidate)))
                candidate)
        entity (when (uuid? party) (d/entity db [:party/id party]))]
    (when-not (and (uuid? party) (:db/id entity)
                   (= :human (or (:actor/kind entity) (:party/type entity))))
      (refuse!))
    party))

(defn- canonical-room! [db handle]
  (when-not (bounded-text? handle 1024) (refuse!))
  ;; Only canonical wire UUID shapes reserve UUID semantics. UUID/fromString
  ;; also accepts abbreviations; those remain ordinary slugs, as before.
  (let [id (when (re-matches #"(?i)[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}" handle)
             (parse-uuid handle))
        rows (if id
               (d/q '[:find ?e ?id ?slug :in $ ?id
                      :where [?e :room/id ?id] [?e :room/slug ?slug]] db id)
               (d/q '[:find ?e ?id ?slug :in $ ?slug
                      :where [?e :room/id ?id] [?e :room/slug ?slug]] db handle))
        [eid room-id slug] (when (= 1 (count rows)) (first rows))]
    (when-not (and eid (uuid? room-id) (bounded-text? slug 1024)
                   (= #{[eid room-id]}
                      (set (d/q '[:find ?e ?id :in $ ?slug
                                  :where [?e :room/slug ?slug] [?e :room/id ?id]] db slug))))
      (refuse!))
    {:room-id room-id :slug slug}))

(defn authorize-read!
  "Pure fixed read gate over the caller's CURRENT immutable control DB. Caller
   authenticates the human principal; MCP marker is transport policy, not this
   service's authority. Normal browser party principals need no invented marker.
   No hydration/store/body IO; no worker/agent/local-to-owner substitution."
  [control-db authenticated-human-principal original-handle]
  (let [party (human! control-db authenticated-human-principal)
        {:keys [room-id] :as resolved} (canonical-room! control-db original-handle)]
    (when-not (access/can? control-db party :read {:room room-id}) (refuse!))
    (assoc resolved :party party)))

(defn resolve-read!
  "ONE ephemeral HOST rich binding from current canonical identity/EACL before
   hydration and again after. Fixed read only. No callbacks, actor/target/action
   options, selected fallback, cache, global state or parent selection changes.
   :control-db and all live handles MUST stay host-only: never persist/export
   them through RPC/SCI or fork state, or treat them as an authorization lease."
  [canonical-control-conn actual-host-context authenticated-human-principal original-handle]
  (binding [ec/*execution-context* actual-host-context]
    (let [before (current-control-db! canonical-control-conn actual-host-context)
          {:keys [room-id]} (authorize-read! before authenticated-human-principal original-handle)
          room (rooms/live-room room-id)
          after (current-control-db! canonical-control-conn actual-host-context)
          current (authorize-read! after authenticated-human-principal original-handle)]
      (when-not (and room (= room-id (:room-id current))) (refuse!))
      (assoc (owned/resolve! after authenticated-human-principal :read room-id room)
             :control-db after))))

(defn host-owner
  "HOST-only captured owner DATA, not a callback or transferable authority.
   Never serialize, persist, expose to SCI or attach to fork state."
  [canonical-control-conn actual-host-context]
  {:control-conn canonical-control-conn :execution-ctx actual-host-context})

(defn owned-host? [host]
  ;; Presence is intentional: malformed/expired owners MUST NOT fall back.
  (contains? host ::owner))

(defn resolve-host-read!
  "Fixed HOST composition entry. Validate the ORIGINAL host owner, then obtain
   ONE rich canonical binding. No callback lookup, fresh owner adoption or
   selected fallback; the explicit principal is authenticated by the caller."
  [host authenticated-human-principal original-handle]
  (let [{:keys [control-conn execution-ctx] :as owner} (::owner host)]
    (when-not (and (map? owner) (= #{:control-conn :execution-ctx} (set (keys owner)))
                   control-conn execution-ctx
                   (identical? execution-ctx (:execution-ctx host)))
      (refuse!))
    (resolve-read! control-conn execution-ctx authenticated-human-principal original-handle)))
