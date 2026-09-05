(ns is.simm.model.parties
  "Unified party model — humans and agents as first-class identities.

   A party is anyone you can chat with. Identity is shared with dvergr's
   actor model: each party entity carries dvergr's actor core
   (:actor/id, :actor/kind, :actor/name, :actor/created-at,
   :actor/system-prompt, :actor/config — an EDN-string map holding
   :model/:provider/:auto-respond?/:template) PLUS simmis extension
   attrs (:party/id uuid alias, :party/handle, :party/avatar, auth
   fields, :party/owner, :party/contacts).

   The UUID↔keyword bridge is `:actor/id = (keyword \"party\" (str uuid))`
   — see `party-id->actor-id` / `actor-id->party-id`.

   All public fns keep the legacy contract: they take/return maps with
   :party/* keys (:party/type, :party/display-name, :party/created,
   :party/system-prompt, :party/model, :party/provider,
   :party/auto-respond?, :party/template, ...) so callers are unchanged;
   `ent->party` adapts the stored actor+extension shape back to it.

   Auth attributes (:party/email, :party/password-hash, :party/auth-providers)
   are written by kabel-auth.store.datahike and read by this namespace.
   Budgets live on dvergr's ledger/budget schema — see is.simm.model.billing."
  (:require [is.simm.model.model-selection :as model-selection]
            [is.simm.model.system-db :as system-db]
            [is.simm.model.billing :as billing]
            [dvergr.actors :as actors]
            [clojure.edn :as edn]
            [datahike.api :as d]
            [clojure.string :as str]
            [taoensso.telemere :as log]))

;; =============================================================================
;; UUID ↔ actor-id bridge
;; =============================================================================

(defn party-id->actor-id
  "dvergr actor keyword for a party UUID."
  [uuid]
  (keyword "party" (str uuid)))

(defn actor-id->party-id
  "Party UUID for a `:party/<uuid>` actor keyword, or nil if not one."
  [actor-id]
  (when (and (keyword? actor-id) (= "party" (namespace actor-id)))
    (try (java.util.UUID/fromString (name actor-id))
         (catch Exception _ nil))))

;; =============================================================================
;; Entity → legacy party-map adapter
;; =============================================================================

(defn- <-edn-str [s]
  (when (and s (not= s ""))
    (try (edn/read-string s)
         (catch Exception _ nil))))

(defn- apply-config-patch
  "Merge a legacy :party/* config patch into actor config.

   nil means remove the key, not store a nil placeholder. Model switches have
   always sent nil for the mutually exclusive form; making that a real removal
   is also what lets clearing an override return an agent to genuinely empty
   inherited model state."
  [config patch]
  (reduce-kv (fn [m k v]
               (if (nil? v)
                 (dissoc m k)
                 (assoc m k v)))
             (or config {})
             patch))

(def ^:private actor-core-keys
  [:actor/id :actor/kind :actor/name :actor/created-at
   :actor/system-prompt :actor/status :actor/config])

(def ^:private dropped-legacy-attrs
  "Agent attrs no longer declared in the system-db schema. Old stores
   still declare + carry them (read as fallback); fresh stores never had
   them and datahike's pull rejects undeclared idents — so they are only
   included in pull patterns when the store's schema declares them."
  [:party/system-prompt :party/model :party/provider
   :party/auto-respond? :party/template])

(defn- party-pull
  "Pull pattern covering the dvergr actor core, the simmis extension
   attrs, and the legacy :party/* attrs (pre-migration rows + humans
   kabel-auth created since the last boot)."
  [db]
  (let [declared (set (keys (:schema db)))]
    (-> '[:party/id :party/handle :party/avatar :party/last-login
          :party/email :party/email-verified :party/role
          :party/preferred-model
          ;; legacy fallbacks (still declared: kabel-auth write-compat)
          :party/type :party/display-name :party/created
          {:party/owner [:party/id]}
          {:party/contacts [:party/id]}]
        (into actor-core-keys)
        (into (filter declared) dropped-legacy-attrs))))

(defn- ent->party
  "Adapt a pulled actor+extension entity to the legacy {:party/* ...} map.
   The actor core wins over legacy :party/* datoms when both are present."
  [ent]
  (when ent
    (let [config (<-edn-str (:actor/config ent))]
      (cond-> (apply dissoc ent :db/id actor-core-keys)
        (:actor/kind ent)          (assoc :party/type (:actor/kind ent))
        (:actor/name ent)          (assoc :party/display-name (:actor/name ent))
        (:actor/created-at ent)    (assoc :party/created (:actor/created-at ent))
        (:actor/system-prompt ent) (assoc :party/system-prompt (:actor/system-prompt ent))
        (contains? config :model)         (assoc :party/model (:model config))
        ;; Family + version, resolved to a concrete id when a participant joins
        ;; (see is.simm.model.model-selection). An explicit :model is the legacy
        ;; form — kept working, but it is the thing that froze Vár on glm-5p1.
        (contains? config :model-family)  (assoc :party/model-family (:model-family config))
        (contains? config :model-version) (assoc :party/model-version (:model-version config))
        (contains? config :provider)      (assoc :party/provider (:provider config))
        (contains? config :auto-respond?) (assoc :party/auto-respond? (:auto-respond? config))
        (contains? config :template)      (assoc :party/template (:template config))))))

;; =============================================================================
;; Party lookups
;; =============================================================================

(defn- eid-by-party-id [db party-id]
  (d/q '[:find ?e . :in $ ?pid :where [?e :party/id ?pid]] db party-id))

(defn- pull-party [db eid]
  (ent->party (d/pull db (party-pull db) eid)))

(defn get-party
  "Fetch a party by UUID. Returns map with :party/* keys or nil."
  [party-id]
  (when-let [conn (system-db/get-conn)]
    (when-let [eid (eid-by-party-id @conn party-id)]
      (pull-party @conn eid))))

(defn get-party-by-handle
  [handle]
  (when-let [conn (system-db/get-conn)]
    (when-let [eid (d/q '[:find ?e . :in $ ?h :where [?e :party/handle ?h]]
                        @conn handle)]
      (pull-party @conn eid))))

(defn get-party-by-email
  [email]
  (when-let [conn (system-db/get-conn)]
    (when-let [eid (d/q '[:find ?e . :in $ ?em :where [?e :party/email ?em]]
                        @conn email)]
      (pull-party @conn eid))))

(def ^:private party-summary-pull
  '[:party/id :actor/kind :actor/name :party/type :party/display-name
    :party/handle :party/avatar])

(defn- summary->party [ent]
  (when ent
    (cond-> (dissoc ent :db/id :actor/kind :actor/name)
      (:actor/kind ent) (assoc :party/type (:actor/kind ent))
      (:actor/name ent) (assoc :party/display-name (:actor/name ent)))))

(defn list-parties
  "List parties filtered by :type (or all). Returns vector sorted by display-name."
  ([] (list-parties nil))
  ([party-type]
   (when-let [conn (system-db/get-conn)]
     (->> (d/q '[:find [(pull ?e pattern) ...]
                 :in $ pattern
                 :where [?e :party/id _]]
               @conn party-summary-pull)
          (map summary->party)
          (filter #(or (nil? party-type) (= party-type (:party/type %))))
          (sort-by :party/display-name)
          vec))))

;; =============================================================================
;; Profile updates
;; =============================================================================

(defn update-party!
  "Update mutable attributes of a party. Only :party/* keys are allowed.
   Maps the legacy keys onto the actor core: display-name → :actor/name,
   system-prompt → :actor/system-prompt; model/provider/auto-respond?/
   template are merged into the :actor/config EDN map. handle/avatar/
   preferred-model stay as simmis extension attrs."
  [party-id updates]
  (when-let [conn (system-db/get-conn)]
    (when-let [eid (eid-by-party-id @conn party-id)]
      (let [allowed (select-keys updates [:party/display-name :party/handle
                                          :party/avatar :party/preferred-model
                                          :party/system-prompt :party/model
                                          :party/model-family :party/model-version
                                          :party/provider :party/auto-respond?
                                          :party/template])
            config-patch (cond-> {}
                           (contains? allowed :party/model)
                           (assoc :model (:party/model allowed))
                           (contains? allowed :party/model-family)
                           (assoc :model-family (:party/model-family allowed))
                           (contains? allowed :party/model-version)
                           (assoc :model-version (:party/model-version allowed))
                           (contains? allowed :party/provider)
                           (assoc :provider (:party/provider allowed))
                           (contains? allowed :party/auto-respond?)
                           (assoc :auto-respond? (:party/auto-respond? allowed))
                           (contains? allowed :party/template)
                           (assoc :template (:party/template allowed)))
            existing-config (when (seq config-patch)
                              (<-edn-str (:actor/config (d/pull @conn '[:actor/config] eid))))
            tx (cond-> {:db/id eid
                        :actor/id (party-id->actor-id party-id)}
                 (contains? allowed :party/display-name)
                 (assoc :actor/name (:party/display-name allowed))
                 (contains? allowed :party/system-prompt)
                 (assoc :actor/system-prompt (:party/system-prompt allowed))
                 (contains? allowed :party/handle)
                 (assoc :party/handle (:party/handle allowed))
                 (contains? allowed :party/avatar)
                 (assoc :party/avatar (:party/avatar allowed))
                 (contains? allowed :party/preferred-model)
                 (assoc :party/preferred-model (:party/preferred-model allowed))
                 (seq config-patch)
                 (assoc :actor/config
                        (pr-str (apply-config-patch existing-config config-patch))))]
        (d/transact conn [tx])
        (log/log! {:level :info
                   :id ::party-updated
                   :msg "Party updated"
                   :data {:party-id party-id :fields (keys allowed)}})))))

(defn update-preferred-model!
  "Persist an owner's preference and activate it on live inheriting agents.

   Explicit agent overrides are unaffected. A live inheriting participant is
   switched through dvergr's targeted model directive, preserving its working
   context and inbox. An unavailable resolution leaves that participant on its
   previous model and is reported as a partial post-commit activation failure."
  [party-id model-id]
  (require 'is.simm.agents.room-agents)
  (let [owner-lock-fn
        (requiring-resolve
         'is.simm.agents.room-agents/owner-model-activation-lock)
        activate-fn
        (requiring-resolve
         'is.simm.agents.room-agents/activate-agent-model!)]
    ;; Owner -> agent -> participant is the activation lock order. Serializing
    ;; the commit too is load-bearing: a newer unavailable preference must leave
    ;; the model active immediately before ITS commit, not allow an older writer
    ;; to post after it and manufacture a stale runtime state.
    (locking (owner-lock-fn party-id)
      ;; Commit first inside the owner boundary. Besides preserving the failed-
      ;; write rule, the post-commit snapshot closes the override-clear race: an
      ;; agent that became inheriting while this preference was being saved must
      ;; be part of activation.
      (update-party! party-id {:party/preferred-model model-id})
      (let [inheriting-agent-ids
            (when-let [conn (system-db/get-conn)]
              (let [db @conn]
                (->> (d/q '[:find [?agent ...]
                            :in $ ?owner-id
                            :where
                            [?owner :party/id ?owner-id]
                            [?agent :party/owner ?owner]
                            [?agent :actor/kind :agent]]
                          db party-id)
                     (map #(pull-party db %))
                     ;; `describe-resolution` is pure and defines the same
                     ;; configured/inherited boundary as the join path.
                     (remove (fn [agent]
                               (:configured?
                                (model-selection/describe-resolution
                                 {:model-family (:party/model-family agent)
                                  :model-version (:party/model-version agent)
                                  :model (:party/model agent)}))))
                     (map :party/id)
                     (sort-by str)
                     vec)))
            failures
            (into []
                  (keep (fn [agent-id]
                          (try
                            ;; Pass the committed write as an expectation, not a
                            ;; captured execution spec. The activation boundary
                            ;; re-reads both agent and owner under its lifecycle
                            ;; lock and skips this call if a newer write won.
                            (activate-fn agent-id model-id)
                            nil
                            (catch Exception e
                              ;; Continue through every inheriting agent: one
                              ;; unavailable/broken participant must not prevent
                              ;; activation of all later agents.
                              (log/log! {:level :error
                                         :id ::preferred-model-activation-failed
                                         :msg "An inheriting agent could not activate an owner preference change"
                                         :error e
                                         :data {:owner-id party-id
                                                :agent-id agent-id}})
                              (merge
                               {:agent-id agent-id
                                :error-class (.getName (class e))}
                               (select-keys (ex-data e)
                                            [:type :model :provider
                                             :availability
                                             :availability-reason
                                             :rooms :room-failures]))))))
                  inheriting-agent-ids)]
        (when (seq failures)
          (throw (ex-info "Model preference was saved, but some live agents could not activate it"
                          {:type :preferred-model-activation-partial
                           :preference-committed? true
                           :owner-id party-id
                           :failures failures})))))))

;; =============================================================================
;; Agents (create, update, delete — agents are parties)
;; =============================================================================

(def default-model
  "Concrete fallback id. Re-exported from `model-selection`, which owns it.
   Prefer `default-family` + :auto — a creation-time default is exactly what froze Vár on
   glm-5p1 for eleven days after the code default said 5p2."
  model-selection/default-model)

(def default-family
  "Family form of the product fallback. Kept for callers that deliberately
   request an explicit family override; new agents store no model choice."
  model-selection/default-family)

(defn create-agent!
  "Create a new agent party. Returns the agent party.

   :owner-id   — party UUID that owns and is billed for this agent (required)
   :opts       — {:template :secretary :display-name \"Vár\" :handle \"vár\"
                  :system-prompt ... :model ... :provider :fireworks
                  :auto-respond? true :avatar \"🤖\"}

   Routes the identity core through dvergr.actors/spawn-agent! (actor row
   with :actor/config EDN), then transacts the simmis extension attrs
   (:party/id alias, :party/owner, handle, avatar) onto the same entity."
  [owner-id {:keys [display-name handle system-prompt model model-family model-version
                    provider template auto-respond? avatar]
             :or {auto-respond? true}}]
  (when-let [conn (system-db/get-conn)]
    (let [party-id (random-uuid)
          actor-id (party-id->actor-id party-id)
          owner-eid (eid-by-party-id @conn owner-id)
          _ (when-not owner-eid
              (throw (ex-info "Owner party not found" {:type :party-not-found
                                                       :party-id owner-id})))]
      ;; 1. dvergr actor core (kind/name/created-at/status/config)
      (actors/spawn-agent! conn
                           (cond-> {:id actor-id
                                    :name display-name
                                    ;; NO :provider unless one was asked for.
                                    ;; It used to default to :fireworks, and
                                    ;; that stamp then beat the registry at turn
                                    ;; time, so an agent preferring gpt-5.5 was
                                    ;; still posted to Fireworks. The provider
                                    ;; follows the model now.
                                    :config (cond-> {:auto-respond? auto-respond?}
                                              provider      (assoc :provider provider)
                                              ;; A model key exists only when a
                                              ;; person explicitly asked for an
                                              ;; override. Otherwise resolution
                                              ;; follows owner preference.
                                              model         (assoc :model model)
                                              (and (not model) model-family)
                                              (assoc :model-family model-family
                                                     :model-version (or model-version :auto))
                                              template      (assoc :template template))}
                             system-prompt (assoc :system-prompt system-prompt)))
      ;; 2. simmis extension attrs on the same entity
      (d/transact conn [(cond-> {:actor/id actor-id
                                 :party/id party-id
                                 :party/owner owner-eid}
                          handle (assoc :party/handle handle)
                          avatar (assoc :party/avatar avatar))])
      (log/log! {:level :info
                 :id ::agent-created
                 :msg "Agent created"
                 :data {:party-id party-id :display-name display-name
                        :owner owner-id}})
      ;; The address-book PROJECTION that used to run here is gone —
      ;; `is.simm.model.address-book` no longer exists (see `runtimes/web.clj`'s
      ;; :on-auth note and `uis/.../people.cljs`). The call survived the removal
      ;; and, wrapped in `requiring-resolve` + `catch Throwable`, threw a
      ;; FileNotFoundException on EVERY agent creation and swallowed it — a dead
      ;; call that looked live and cost a load attempt each time. Parties are
      ;; read from the system DB directly now; nothing needs projecting.
      (get-party party-id))))

(defn update-agent!
  "Update mutable fields of an agent party and reset its joined participants.

   This explicit agent-edit path is separate from ordinary turns. The next
   dispatch rejoins the participant and re-resolves its model configuration.
   Owner preference changes reset inheriting agents through their own write
   path; provider-catalog refreshes remain observational and do not mutate a
   live participant."
  [agent-id updates]
  (update-party! agent-id updates)
  ;; Leave current participants and clear their cached contexts. The next join
  ;; captures the edited prompt/model resolution; there is no per-turn switch.
  (require 'is.simm.agents.room-agents)
  (when-let [reset-fn (resolve 'is.simm.agents.room-agents/reset-agent-contexts!)]
    (reset-fn agent-id)))

(defn delete-party!
  "Remove a party entirely. Only call for agents or test parties."
  [party-id]
  (when-let [conn (system-db/get-conn)]
    (when-let [eid (eid-by-party-id @conn party-id)]
      (d/transact conn [[:db/retractEntity eid]]))))

;; =============================================================================
;; Contacts
;; =============================================================================

(defn get-contacts
  "Return parties in the :party/contacts set of party-id."
  [party-id]
  (when-let [conn (system-db/get-conn)]
    (->> (d/q '[:find [(pull ?c pattern) ...]
                :in $ ?pid pattern
                :where
                [?p :party/id ?pid]
                [?p :party/contacts ?c]]
              @conn party-id party-summary-pull)
         (map summary->party)
         (sort-by :party/display-name)
         vec)))

(defn add-contact!
  "Add contact-party-id to party-id's contacts (directed edge)."
  [party-id contact-party-id]
  (when-let [conn (system-db/get-conn)]
    (let [db @conn
          eid (eid-by-party-id db party-id)
          contact-eid (eid-by-party-id db contact-party-id)]
      (when (and eid contact-eid (not= eid contact-eid))
        (d/transact conn [[:db/add eid :party/contacts contact-eid]])
        (log/log! {:level :info :id ::contact-added
                   :data {:party-id party-id :contact-id contact-party-id}})))))

(defn remove-contact!
  [party-id contact-party-id]
  (when-let [conn (system-db/get-conn)]
    (let [db @conn
          eid (eid-by-party-id db party-id)
          contact-eid (eid-by-party-id db contact-party-id)]
      (when (and eid contact-eid)
        (d/transact conn [[:db/retract eid :party/contacts contact-eid]])))))

(defn find-contact-by-handle
  "Resolve a handle (possibly with @host suffix for future federation) to a
   party UUID. Currently only resolves local handles."
  [handle]
  (let [local-handle (first (str/split handle #"@" 2))]
    (:party/id (get-party-by-handle local-handle))))

;; =============================================================================
;; Env vars (per-party)
;; =============================================================================

(def allowed-env-prefixes
  #{"SLACK_" "GITHUB_" "OPENAI_" "ANTHROPIC_" "FIREWORKS_" "GOOGLE_"})

(defn valid-env-key?
  [key]
  (some #(str/starts-with? key %) allowed-env-prefixes))

(defn get-env-vars
  "Return [{:id :key :value}] for the party."
  [party-id]
  (when-let [conn (system-db/get-conn)]
    (->> (d/q '[:find ?id ?key ?value
                :keys id key value
                :in $ ?pid
                :where
                [?e :env-var/party-id ?pid]
                [?e :env-var/id ?id]
                [?e :env-var/key ?key]
                [?e :env-var/value ?value]]
              @conn party-id)
         (sort-by :key)
         vec)))

(defn get-env-map
  "Return env vars as a plain map {key value}."
  [party-id]
  (into {} (map (juxt :key :value) (get-env-vars party-id))))

(defn set-env-var!
  [party-id key value]
  (when-not (valid-env-key? key)
    (throw (ex-info "Env var key not allowed"
                    {:type :validation-error
                     :key key
                     :allowed-prefixes allowed-env-prefixes})))
  (when-let [conn (system-db/get-conn)]
    (let [existing-eid (d/q '[:find ?e . :in $ ?pid ?key
                              :where
                              [?e :env-var/party-id ?pid]
                              [?e :env-var/key ?key]]
                            @conn party-id key)]
      (if existing-eid
        (d/transact conn [{:db/id existing-eid :env-var/value value}])
        (d/transact conn [{:env-var/id (random-uuid)
                           :env-var/party-id party-id
                           :env-var/key key
                           :env-var/value value}]))
      (log/log! {:level :info :id ::env-var-set
                 :data {:party-id party-id :key key}}))))

(defn delete-env-var!
  [party-id key]
  (when-let [conn (system-db/get-conn)]
    (when-let [eid (d/q '[:find ?e . :in $ ?pid ?key
                          :where
                          [?e :env-var/party-id ?pid]
                          [?e :env-var/key ?key]]
                        @conn party-id key)]
      (d/transact conn [[:db/retractEntity eid]]))))

;; =============================================================================
;; Budgets (delegated to dvergr's :budget/* + ledger via billing)
;; =============================================================================

(defn get-budget
  "Return {:total :used :remaining} microdollars, or nil if unset."
  [party-id]
  (billing/get-party-budget party-id))

(defn set-budget!
  [party-id total-microdollars]
  (billing/set-party-budget! party-id total-microdollars))

(defn has-budget?
  [party-id cost-microdollars]
  (billing/has-party-budget? party-id cost-microdollars))

;; =============================================================================
;; UI Preferences
;; =============================================================================

(defn get-ui-prefs
  [party-id]
  (when-let [conn (system-db/get-conn)]
    (if-let [eid (d/q '[:find ?e . :in $ ?pid :where [?e :ui-pref/party-id ?pid]]
                      @conn party-id)]
      (-> (d/pull @conn '[:ui-pref/syntax] eid)
          (dissoc :db/id))
      {})))

(defn set-ui-pref!
  [party-id attr value]
  (when-let [conn (system-db/get-conn)]
    (d/transact conn [{:ui-pref/party-id party-id attr value}])
    (log/log! {:level :info :id ::ui-pref-set
               :data {:party-id party-id :attr attr :value value}})))
