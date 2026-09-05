(ns is.simm.model.rooms
  "Room CRUD against the (shared dvergr) system DB.

   A simmis room IS a dvergr room/project (Stage 1 of
   doc/dvergr-integration-plan.md): provisioning goes through
   `dvergr.system.rooms/provision-room!`, which gives every room its own
   messages store + KB store + cloned workspace repo (dvergr-sandbox stdlib)
   as yggdrasil systems on the room's OWN forked execution context.

   simmis extension attributes live on the same room row:
   - :room/created-by (party UUID)
   - :room/content-db-scope (konserve-sync scope of the room's database —
     since the store collapse this IS dvergr's room store, shared by
     conversation, category-S content and the room's book)
   - :room/budget-dollars, :room/knowledge-bases

   Membership uses dvergr's :room/parties (REF cardinality-many to party
   entities). The external API of this namespace keeps returning party UUID
   sets so callers are unaffected.

   Agent configuration is a property of the agent party itself (see
   is.simm.model.parties), not of the room. To add an agent to a room,
   just put its party in :room/parties."
  (:require [is.simm.model.system-db :as system-db]
            [is.simm.model.room-databases :as room-dbs]
            [is.simm.model.knowledge-bases :as kbs]
            [is.simm.model.parties :as parties]
            [is.simm.agents.templates :as templates]
            [is.simm.runtimes.context :as ctx]
            [dvergr.rooms :as drooms]
            [dvergr.system.rooms :as srooms]
            [dvergr.system.db :as sdb]
            [clojure.string :as str]
            [datahike.api :as d]
            [taoensso.telemere :as log]))

;; =============================================================================
;; Room CRUD
;; =============================================================================

(defn- room-slug
  "Slug for dvergr room provisioning: kebab-cased name plus a short random
   suffix (room names are not unique in simmis, slugs must be)."
  [name]
  (str (-> (or name "room")
           str/lower-case
           (str/replace #"[^a-z0-9]+" "-")
           (str/replace #"(^-+|-+$)" ""))
       "-" (subs (str (random-uuid)) 0 8)))

(defn- parties->uuids
  "Convert a pulled ref-valued :room/parties into the party UUID set the
   simmis API exposes."
  [room]
  (if-let [ps (:room/parties room)]
    (assoc room :room/parties (into #{} (keep :party/id) ps))
    room))

(def ^:private room-pull
  '[:room/id :room/slug :room/name :room/type :room/created-by
    :room/created {:room/parties [:party/id]} :room/content-db-scope
    :room/budget-dollars])

(defn- assoc-room-kbs
  "KB attachment lives in grant rows, not on the room entity — surface it
   on the room map as :room/knowledge-bases so callers keep their shape."
  [room]
  (if-let [kb-ids (seq (kbs/room-kb-ids (:room/id room)))]
    (assoc room :room/knowledge-bases (vec kb-ids))
    room))

(defn create-room!
  "Create a new room. Creator is automatically added to :room/parties.

   Provisions the room as a dvergr project (msgs store + KB + workspace repo
   + own execution ctx), then transacts the simmis extension attributes and
   installs simmis's schema on the room store. Returns the room map."
  [creator-party-id name room-type party-ids]
  (when (system-db/get-conn)
    (let [slug (room-slug name)
          ;; Full dvergr room lifecycle: provisions (msgs store + KB + workspace
          ;; repo + own execution ctx) AND registers the live discourse Room
          ;; with its DatahikeStore. Needs a bound execution ctx.
          _ (ctx/with-server-context
              (drooms/create-room! {:title name :slug slug
                                    :type room-type
                                    :parent-id false
                                    :ctx ctx/server-context}))
          ;; drooms/create-room! doesn't thread an owner — upsert it (by slug).
          ;; Re-pass :name and :type: the upsert DEFAULTS omitted fields
          ;; (name → slug, type → :project) instead of preserving them.
          room-id (sdb/create-room! {:slug slug :name name :type room-type
                                     :owner-id creator-party-id})
          ;; One database per room: the scope IS dvergr's room store, not a
          ;; scope simmis mints for a second store beside it. Asking dvergr for
          ;; the id (rather than re-deriving it) keeps the two sides from
          ;; drifting apart on how a store is named.
          content-scope (ctx/with-server-context
                          (srooms/room-msgs-store-id room-id))
          all-parties (into #{creator-party-id} party-ids)]
      (d/transact (system-db/get-conn)
                  (into [{:room/id room-id
                          :room/created-by creator-party-id
                          :room/content-db-scope content-scope}]
                        (map (fn [pid] {:room/id room-id
                                        :room/parties [:party/id pid]}))
                        all-parties))
      (room-dbs/ensure-room-database! content-scope)
      (log/log! {:level :info
                 :id ::room-created
                 :msg "Room created (dvergr-provisioned)"
                 :data {:room-id room-id :slug slug :name name :type room-type
                        :creator creator-party-id :party-count (count all-parties)
                        :content-db-scope content-scope}})
      (-> (d/pull @(system-db/get-conn) room-pull [:room/id room-id])
          parties->uuids
          assoc-room-kbs))))

(defn get-room
  [room-id]
  (when-let [conn (system-db/get-conn)]
    (when (d/q '[:find ?e . :in $ ?rid :where [?e :room/id ?rid]]
               @conn room-id)
      (-> (d/pull @conn room-pull [:room/id room-id])
          (dissoc :db/id)
          parties->uuids
          assoc-room-kbs))))

(defn get-party-rooms
  "All rooms containing the given party (human or agent)."
  [party-id]
  (when-let [conn (system-db/get-conn)]
    (->> (d/q {:find [[(list 'pull '?e room-pull) '...]]
               :in '[$ ?pid]
               :where '[[?p :party/id ?pid]
                        [?e :room/parties ?p]]}
              @conn party-id)
         (map #(-> % (dissoc :db/id) parties->uuids assoc-room-kbs))
         (sort-by :room/name)
         vec)))

(defn get-room-parties
  "Return full party records for every member of the room."
  [room-id]
  (when-let [room (get-room room-id)]
    (->> (:room/parties room)
         (mapv parties/get-party)
         (filterv some?))))

(defn add-party!
  [room-id party-id]
  (when-let [conn (system-db/get-conn)]
    (when-let [eid (d/q '[:find ?e . :in $ ?rid :where [?e :room/id ?rid]]
                        @conn room-id)]
      (d/transact conn [{:db/id eid :room/parties [:party/id party-id]}])
      (log/log! {:level :info :id ::party-added-to-room
                 :data {:room-id room-id :party-id party-id}}))))

(defn remove-party!
  [room-id party-id]
  (when-let [conn (system-db/get-conn)]
    (when-let [eid (d/q '[:find ?e . :in $ ?rid :where [?e :room/id ?rid]]
                        @conn room-id)]
      (d/transact conn [[:db/retract eid :room/parties [:party/id party-id]]])
      (log/log! {:level :info :id ::party-removed-from-room
                 :data {:room-id room-id :party-id party-id}}))))

(defn get-room-agents
  "Agent parties in this room."
  [room-id]
  (->> (get-room-parties room-id)
       (filterv #(= :agent (:party/type %)))))

(defn get-room-humans
  [room-id]
  (->> (get-room-parties room-id)
       (filterv #(= :human (:party/type %)))))

;; =============================================================================
;; Room Budget
;; =============================================================================

(defn get-room-budget-dollars
  "Budget in dollars. Returns default 10.0 if unset."
  [room-id]
  (when-let [conn (system-db/get-conn)]
    (or (d/q '[:find ?b . :in $ ?rid
               :where [?r :room/id ?rid] [?r :room/budget-dollars ?b]]
             @conn room-id)
        10.0)))

(defn set-room-budget!
  [room-id budget-dollars]
  (when-let [conn (system-db/get-conn)]
    (when-let [eid (d/q '[:find ?e . :in $ ?rid :where [?e :room/id ?rid]]
                        @conn room-id)]
      (d/transact conn [{:db/id eid :room/budget-dollars (double budget-dollars)}]))))

;; =============================================================================
;; Personal AI Room
;; =============================================================================

(defn ensure-personal-ai-room!
  "Ensure the party has a :personal-ai room with a Vár agent.
   Returns the room."
  [owner-party-id owner-display-name]
  (when-let [conn (system-db/get-conn)]
    (let [existing-id (d/q '[:find ?rid .
                             :in $ ?pid
                             :where
                             [?p :party/id ?pid]
                             [?e :room/parties ?p]
                             [?e :room/type :personal-ai]
                             [?e :room/id ?rid]]
                           @conn owner-party-id)
          room (if existing-id
                 (get-room existing-id)
                 ;; "<Name>'s Assistants", matching "<Name>'s Wiki" beside it in
                 ;; the sidebar. Possessive rather than "My …" because the same
                 ;; room is listed in shared and multi-tenant views, where "My"
                 ;; is ambiguous about whose. `owner-display-name` may be absent
                 ;; for a party created without one — fall back rather than
                 ;; producing "'s Assistants".
                 (create-room! owner-party-id
                               (if (str/blank? owner-display-name)
                                 "My Assistants"
                                 (str owner-display-name "'s Assistants"))
                               :personal-ai []))
          tmpl templates/secretary-template
          ;; Check if this party already owns a Vár-type agent in this room.
          agents (->> (get-room-agents (:room/id room))
                      (filter #(= (:party/template %) (:id tmpl))))
          has-var? (seq agents)]
      (when-not has-var?
        (let [agent (parties/create-agent!
                     owner-party-id
                     (assoc (templates/agent-options (:name tmpl) tmpl)
                            :handle (str "vár-" (subs (str owner-party-id) 0 8))
                            :avatar "🤖"))]
          (add-party! (:room/id room) (:party/id agent))))
      room)))
