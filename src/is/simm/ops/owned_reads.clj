(ns is.simm.ops.owned-reads
  "Fixed HOST canonical human workspace gates. Read/write entries are fixed;
   no configurable resolver/action, activation, selected fallback or retained authority."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [dvergr.room.registry :as registry]
            [dvergr.room.store :as room-store]
            [dvergr.system.rooms :as native-rooms]
            [is.simm.agents.room-agents :as rooms]
            [is.simm.documents.space :as space]
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

(defn- assert-control-owner! [conn host-ctx]
  (when (or (simple/on-drain-thread?)
            (not (and conn host-ctx (identical? conn (system-db/get-conn))
                      (identical? host-ctx context/server-context))))
    (refuse!)))

(defn current-control-db!
  "HOST INTERNAL ownership guard shared with transport delegation. No endpoint
   or second resolver. Requires the actual current canonical connection/server
   context and physical host worker. Returned DB is ephemeral, not a lease."
  [conn host-ctx]
  (assert-control-owner! conn host-ctx)
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

(defn authorize-write!
  "Fixed HOST human read+write gate; no hydration or transferable authority."
  [control-db principal original-handle]
  (let [{:keys [party room-id] :as resolved} (authorize-read! control-db principal original-handle)]
    (when-not (access/can? control-db party :write {:room room-id}) (refuse!))
    resolved))

(defn- authorize-install! [db principal handle]
  (let [{:keys [party room-id] :as resolved} (authorize-write! db principal handle)]
    ;; Use existing room governance; membership/write permission alone does not
    ;; authorize installation of the product's managed starter workflow.
    (when-not (and (access/can? db party :admin {:room room-id})
                   (#{:personal-ai :project :group} (:room/type (d/entity db [:room/id room-id]))))
      (refuse!))
    resolved))

(declare ^:private registered-native-room)

(defn- original-install-room!
  [expectation current]
  (let [captured (:binding expectation) room (:room captured)]
    (when-not (and (map? captured) room
                   (= (select-keys current [:party :room-id :slug])
                      (select-keys captured [:party :room-id :slug]))
                   (identical? room (registered-native-room (:room-id current) (:slug current))))
      (refuse!))
    room))

(defn- resolve-action! [conn host-ctx principal handle action & [expectation]]
  (binding [ec/*execution-context* host-ctx]
    (let [authorize (case action :read authorize-read! :write authorize-write! :install authorize-install!)
          before (current-control-db! conn host-ctx)
          original (authorize before principal handle)
          room (if expectation
                 (original-install-room! expectation original)
                 (rooms/live-room (:room-id original)))
          after (current-control-db! conn host-ctx)
          current (authorize after principal handle)]
      (when-not (and room (= (select-keys original [:room-id :party])
                             (select-keys current [:room-id :party]))) (refuse!))
      (when expectation (original-install-room! expectation current))
      (let [binding (assoc (owned/resolve! after principal (if (= :install action) :write action)
                                           (:room-id original) room)
                           :control-db after)]
        (when (and (= :install action) (not (space/provisioned? (:db binding)))) (refuse!))
        binding))))

(defn resolve-read!
  "ONE ephemeral HOST rich binding from current canonical identity/EACL before
   hydration and again after. Fixed read only. No callbacks, actor/target/action
   options, selected fallback, cache, global state or parent selection changes.
   :control-db and all live handles MUST stay host-only: never persist/export
   them through RPC/SCI or fork state, or treat them as an authorization lease."
  [canonical-control-conn actual-host-context authenticated-human-principal original-handle]
  (resolve-action! canonical-control-conn actual-host-context authenticated-human-principal
                   original-handle :read))

(defn host-owner
  "HOST-only captured owner DATA, not a callback or transferable authority.
   Never serialize, persist, expose to SCI or attach to fork state."
  [canonical-control-conn actual-host-context]
  {:control-conn canonical-control-conn :execution-ctx actual-host-context})

(defn owned-host? [host]
  ;; Presence is intentional: malformed/expired owners MUST NOT fall back.
  (contains? host ::owner))

(defn- original-owner! [host]
  (let [{:keys [control-conn execution-ctx] :as owner} (::owner host)]
    (when-not (and (map? owner) (= #{:control-conn :execution-ctx} (set (keys owner)))
                   control-conn execution-ctx
                   (identical? execution-ctx (:execution-ctx host)))
      (refuse!))
    owner))

(defn preflight-host-read!
  "HOST metadata-only original-owner/canonical-human gate. No hydration or
   target lookup. Returned control snapshot is ephemeral, never authority."
  [host principal original-handle]
  (let [{:keys [control-conn execution-ctx]} (original-owner! host)]
    (binding [ec/*execution-context* execution-ctx]
      (let [db (current-control-db! control-conn execution-ctx)]
        (assoc (authorize-read! db principal original-handle) :control-db db)))))

(defn current-host-control-db!
  "Fixed HOST owner/liveness guard before scheduler traversal. No principal,
   hydration, target lookup or retained authority; callers authorize each row."
  [host]
  (let [{:keys [control-conn execution-ctx]} (original-owner! host)]
    (binding [ec/*execution-context* execution-ctx]
      (current-control-db! control-conn execution-ctx))))

(defn- authorize-room! [db principal handle]
  (let [{:keys [room-id party] :as current} (authorize-read! db principal handle)]
    (when-not (access/can? db party :merge {:room room-id}) (refuse!))
    current))

(defn- registered-native-room [room-id slug]
  ;; Use the slug from our authorized immutable control DB, not registered-room
  ;; (which looks up room metadata again through a global connection).
  (let [native-id (room-store/slug->room-id slug)
        room (registry/lookup native-id)]
    (when room
      (when-not (and (= native-id (:id room)) (= slug (:slug room))
                     (:ctx room) (:meta room) (uuid? (:incarnation room))
                     (identical? (:ctx room) (native-rooms/room-ctx-for room-id))
                     (true? (registry/admitted-incarnation? room))) (refuse!))
      room)))

(defn resolve-registered-host-read-room!
  "Read an already admitted native Room under current human read authority.
   A missing Room remains nil: this metadata path never hydrates or installs.
   Present bindings use the ordinary exact-incarnation validation boundary."
  [host principal original-handle]
  (let [{:keys [control-conn execution-ctx] :as owner} (original-owner! host)]
    (binding [ec/*execution-context* execution-ctx]
      (let [db (current-control-db! control-conn execution-ctx)
            current (authorize-read! db principal original-handle)]
        (assoc current :room (registered-native-room (:room-id current) (:slug current))
               :control-db db ::room-owner owner)))))

(defn preflight-host-install!
  "Fixed metadata-only human governance gate before Room hydration."
  [host principal original-handle]
  (authorize-install! (current-host-control-db! host) principal original-handle))

(defn- resolve-room! [host principal original-handle authorize]
  (let [{:keys [control-conn execution-ctx] :as owner} (original-owner! host)]
    (binding [ec/*execution-context* execution-ctx]
      (let [before (current-control-db! control-conn execution-ctx)
            original (authorize before principal original-handle)
            registered (registered-native-room (:room-id original) (:slug original))
            room (rooms/live-room (:room-id original))
            after (current-control-db! control-conn execution-ctx)
            current (authorize after principal original-handle)
            canonical (registered-native-room (:room-id current) (:slug current))]
        (when-not (and room
                       (= original current)
                       (or (nil? registered) (identical? registered room))
                       (identical? room canonical))
          (refuse!))
        (assoc current :room room :control-db after ::room-owner owner)))))

(defn- validate-room-in! [host principal original-handle binding authorize db]
  (let [{:keys [control-conn execution-ctx] :as owner} (original-owner! host)
        captured (::room-owner binding)]
    (when-not (and (map? binding)
                   (= #{:party :room-id :slug :room :control-db ::room-owner} (set (keys binding)))
                   (map? captured) (= #{:control-conn :execution-ctx} (set (keys captured)))
                   (identical? control-conn (:control-conn captured))
                   (identical? execution-ctx (:execution-ctx captured)))
      (refuse!))
    (clojure.core/binding [ec/*execution-context* execution-ctx]
      ;; Recheck ownership without acquiring another control snapshot. Ordinary
      ;; callers return the exact DB they authorized; writers use their own DB.
      (assert-control-owner! control-conn execution-ctx)
      (let [current (authorize db principal original-handle)
            room (registered-native-room (:room-id current) (:slug current))]
        (when-not (and (= current (select-keys binding [:party :room-id :slug]))
                       room (identical? room (:room binding))
                       (identical? (:ctx room) (:ctx (:room binding)))
                       (identical? (:meta room) (:meta (:room binding)))) (refuse!))
        (assoc current :room room :control-db db ::room-owner owner)))))

(defn- validate-room! [host principal original-handle binding authorize]
  (validate-room-in! host principal original-handle binding authorize (current-host-control-db! host)))

(defn validate-host-room-in!
  "Fixed HOST writer-side read+merge and exact original Room observation on
   its own control DB. No hydration, source IO or thread-entry permission.
   The caller separately retains the original App operation and validates its
   actual-thread ownership outside this short serialized decision."
  [host principal original-handle binding db]
  (validate-room-in! host principal original-handle binding authorize-room! db))

(defn validate-host-read-room-in!
  "Read-only original Room observation on one caller-supplied current control
   snapshot. No source acquisition or hydration follows this decision here."
  [host principal original-handle binding db]
  (validate-room-in! host principal original-handle binding authorize-read! db))

(defn resolve-host-read-room!
  "Fixed human read-only Room gate before taking the native operation lease.
   No default repository snapshot, merge requirement or selected fallback."
  [host principal original-handle]
  (resolve-room! host principal original-handle authorize-read!))

(defn validate-host-read-room!
  "Recheck current human read authority and the exact original Room incarnation."
  [host principal original-handle binding]
  (validate-room! host principal original-handle binding authorize-read!))

(defn resolve-host-room!
  "Fixed HOST human read+MERGE Room-only hydration for retained offline work.
   No default/source/files lookup. Returns ephemeral HOST data including the
   exact original owner pair; never persist/export it, put it in SCI/fork state,
   or treat its control DB as a lease. Room ctx need not equal server ctx."
  [host principal original-handle]
  (resolve-room! host principal original-handle authorize-room!))

(defn resolve-host-write-room!
  "Fixed human read+WRITE Room-only hydration before taking its native lease.
   No repository snapshot or merge authority is required."
  [host principal original-handle]
  (resolve-room! host principal original-handle authorize-write!))

(defn validate-host-write-room!
  "Current read+WRITE and exact Room incarnation; never hydrates a replacement."
  [host principal original-handle binding]
  (validate-room! host principal original-handle binding authorize-write!))

(defn validate-host-room!
  "Current fixed read+MERGE and exact native Room-incarnation validation.
   Never hydrates a missing/replacement Room or resolves a source/default."
  [host principal original-handle binding]
  (validate-room! host principal original-handle binding authorize-room!))

(defn resolve-host-install-room!
  "Fixed human read+write+admin Room-only gate for inactive starter setup.
   Canonical repository eligibility must still be checked UNDER the Room lease."
  [host principal original-handle]
  (resolve-room! host principal original-handle authorize-install!))

(defn validate-host-install-room!
  "Recheck install governance and exact original Room without target IO."
  [host principal original-handle binding]
  (validate-room! host principal original-handle binding authorize-install!))

(defn resolve-host-read!
  "Fixed HOST composition entry. Validate the ORIGINAL host owner, then obtain
   ONE rich canonical binding. No callback lookup, fresh owner adoption or
   selected fallback; the explicit principal is authenticated by the caller."
  [host authenticated-human-principal original-handle]
  (let [{:keys [control-conn execution-ctx]} (original-owner! host)]
    (resolve-read! control-conn execution-ctx authenticated-human-principal original-handle)))

(defn resolve-host-write!
  "Fixed inactive HOST human writer binding. Never selected or owner-adopting."
  [host principal original-handle]
  (let [{:keys [control-conn execution-ctx]} (original-owner! host)]
    (resolve-action! control-conn execution-ctx principal original-handle :write)))

(defn validate-host-write!
  "Revalidate current authority and the ORIGINAL target at native effect gates."
  [host principal original-handle binding]
  (let [{:keys [control-conn execution-ctx]} (original-owner! host)
        db (current-control-db! control-conn execution-ctx)
        current (authorize-write! db principal original-handle)]
    (when-not (= (select-keys binding [:room-id :party]) (select-keys current [:room-id :party]))
      (refuse!))
    (owned/validate! db principal :write (:room-id binding) (:room binding) binding)
    true))

(defn resolve-host-install!
  "Fixed HOST room-governance + write gate for an explicitly selected canonical
   workspace. With an install-room binding, reuse ONLY that leased Room and
   validate its canonical mapping before any target IO. No fallback hydration."
  [host principal original-handle]
  (let [{:keys [control-conn execution-ctx]} (original-owner! host)]
    (resolve-action! control-conn execution-ctx principal original-handle :install
                     (when (contains? host ::install-room-binding)
                       {:binding (::install-room-binding host)}))))

(defn validate-host-install!
  "Recheck original host, current room governance and canonical target at every
   native install effect boundary. A retained binding is never a permission."
  [host principal original-handle binding]
  (let [{:keys [control-conn execution-ctx]} (original-owner! host)
        db (current-control-db! control-conn execution-ctx)
        current (authorize-install! db principal original-handle)]
    (when (contains? host ::install-room-binding)
      (when-not (identical? (:room binding)
                           (original-install-room! {:binding (::install-room-binding host)} current))
        (refuse!)))
    (when-not (= (select-keys binding [:room-id :party]) (select-keys current [:room-id :party]))
      (refuse!))
    (let [fresh (owned/validate! db principal :write (:room-id binding) (:room binding) binding)]
      (when-not (space/provisioned? (:db fresh)) (refuse!)))
    true))
