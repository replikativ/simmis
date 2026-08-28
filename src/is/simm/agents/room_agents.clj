(ns is.simm.agents.room-agents
  "Room agent dispatch — thin simmis policy layer over dvergr's canonical
   agent harness (Stage 2 of doc/dvergr-integration-plan.md).

   What dvergr owns (nothing here duplicates it):
   - the live discourse Room (registered by `dvergr.rooms/create-room!`,
     which simmis room provisioning calls; looked up via
     `dvergr.room.registry`), with its DatahikeStore persisting every post
     to the room's own msgs store
   - the per-(room, agent) working chat-ctx (`dvergr.agent.room-context/
     ensure-ctx!` — stable chat-id, bus fold, store seeding)
   - the turn factory + SCI sandbox (`dvergr.agent.turn/new-working-ctx`
     via llm-agent's on-message: guarded datahike, dvergr.room/*kb*,
     fs/git/http/intake namespaces, room workspace repo on the load path)

   What simmis adds:
   - agent identity/config from party rows (model, provider, prompt, budget)
   - the product-KB vocabulary: `wiki/*` + `kb/*` SCI namespaces over the
     KBs attached to the room (grants), plus a system-prompt context block
   - interim dual-write of messages to the room's category-S content DB so
     the existing client sync keeps rendering chat (goes away in Stage 5)

   Spend is NOT recorded here: dvergr's turn path writes the `:ledger/*` row
   and the `:chat/budget-used` rollup into the room's own store."
  (:require [dvergr.model.providers :as providers]
            [dvergr.chat.context :as chat-ctx]
            [dvergr.tools :as tools]
            [dvergr.discourse :as d]
            [dvergr.discourse.llm :as llm]
            [dvergr.agent.room-context :as room-ctx]
            [dvergr.sandbox :as sandbox]
            [dvergr.rooms :as drooms]
            [dvergr.room.registry :as rreg]
            [dvergr.room.store :as rstore]
            [sci.core :as sci]
            [org.replikativ.spindel.engine.core :as rtc]
            [is.simm.runtimes.context :as ctx]
            [is.simm.runtimes.branching :as branching]
            [dvergr.system.db :as sdb]
            [is.simm.model.rooms :as rooms]
            [is.simm.agents.dispatch :as dispatch]
            [is.simm.agents.vocab :as vocab]
            [is.simm.model.parties :as parties]
            [is.simm.model.model-selection :as model-selection]
            [is.simm.model.seed :as seed]
            [is.simm.model.knowledge-bases :as kbs]
            [is.simm.model.room-databases :as room-dbs]
            [is.simm.model.accounting-sandbox :as acct-sandbox]
            [is.simm.model.references :as refs]
            [is.simm.model.message-notify-broadcast :as mnb]
            [is.simm.model.drives :as drives]
            [is.simm.media.sheet :as sheet]
            [is.simm.media.office :as office]
            [is.simm.runtimes.screen-intake :as screen-intake]
            [is.simm.runtimes.web-intake :as web-intake]
            [clojure.zip]
            [is.simm.agents.templates :as templates]
            [is.simm.model.fractional-index :as frac]
            [datahike.api :as d-api]
            [datahike.reference :as dh-ref]
            [muschel.fs :as mfs]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.pprint :as pp]
            [taoensso.telemere :as log]))

;; Default PERSONA — who the agent is when its party names nobody in particular.
;; The tool manual is NOT here; see `persona+tools`.
(def ^:private default-persona
  (:system-prompt templates/secretary-template))

(defn- persona+tools
  "A turn's base prompt: who this agent is, THEN what its sandbox can do.

   These used to be one string chosen by `or`, so setting `:party/system-prompt`
   replaced the whole tool manual with your persona. The agent kept every
   capability and was told about none of them — `wiki/`, `kb/`, widgets and
   `kontor/` all still injected into the sandbox, all undocumented. Vár has had
   a governed double-entry book bound in since the accounting kernel landed and
   never posted to it once, because nothing ever mentioned it to her.

   A persona is a job description; the tool docs describe the room she is
   standing in. Composing them means a custom agent cannot be crippled by being
   given a personality."
  [agent-party]
  (str (or (:party/system-prompt agent-party) default-persona)
       "\n\n"
       templates/tool-docs))

;; =============================================================================
;; Identity: simmis party uuid ↔ dvergr actor keyword
;;
;; The A2 identity unification writes :actor/id as (keyword "party" <uuid>)
;; on every party row — participants use the same convention, so the
;; mapping is a pure function in both directions (no registry).
;; =============================================================================

(defn party->actor-kw
  [party-or-id]
  (let [pid (if (uuid? party-or-id) party-or-id (:party/id party-or-id))]
    (keyword "party" (str pid))))

(defn actor-kw->party-uuid
  "Inverse of party->actor-kw; nil for non-party participant ids.
   Accepts both :party/<uuid> and the bare :<uuid> form (dvergr's room
   store normalizes participant keywords to their name part)."
  [kw]
  (when (keyword? kw)
    (when (or (= "party" (namespace kw)) (nil? (namespace kw)))
      (parse-uuid (name kw)))))

;; #{[room-uuid party-id]} — participants already joined to the live room.
(defonce ^:private joined (atom #{}))

(defn- leave-participant!
  "Make a participant LEAVE the live dvergr room (unsubscribes its bus
   pumps). Forgetting a participant without leaving orphans its inbox
   subscription — a re-join then creates a SECOND responder and every
   message gets answered twice."
  [room-uuid actor-kw]
  (when-let [slug (:room/slug (rooms/get-room room-uuid))]
    (when-let [room (rreg/lookup (rstore/slug->room-id slug))]
      (when (contains? @(:participants room) actor-kw)
        (binding [rtc/*execution-context* (:ctx room)]
          (d/leave room actor-kw))))))

(defn reset-room-context!
  "Drop participation for a room: LEAVE the live discourse room first
   (unsubscribe pumps), then clear caches; next dispatch re-joins and
   dvergr re-seeds the working ctxs from the room store."
  [room-uuid]
  (let [ks (filter (fn [[rid _]] (= rid room-uuid)) @joined)]
    (doseq [[_ pid] ks]
      (leave-participant! room-uuid (party->actor-kw pid))))
  (swap! joined (fn [s] (into #{} (remove (fn [[rid _]] (= rid room-uuid)) s))))
  (when-let [slug (:room/slug (rooms/get-room room-uuid))]
    (room-ctx/drop-room! (rstore/slug->room-id slug))))

(defn reset-agent-contexts!
  "Drop participation for an agent everywhere (e.g. on prompt edit):
   leave each live room, then clear caches."
  [agent-party-id]
  (let [room-uuids (map first (filter (fn [[_ aid]] (= aid agent-party-id)) @joined))]
    (doseq [rid room-uuids]
      (leave-participant! rid (party->actor-kw agent-party-id)))
    (swap! joined (fn [s] (into #{} (remove (fn [[_ aid]] (= aid agent-party-id)) s))))
    (doseq [rid room-uuids]
      (when-let [slug (:room/slug (rooms/get-room rid))]
        (room-ctx/drop-ctx! (rstore/slug->room-id slug)
                            (party->actor-kw agent-party-id))))))

;; =============================================================================
;; Live discourse Room resolution
;; =============================================================================

(defn live-room
  "The live dvergr discourse Room for a simmis room uuid. Rooms created by
   `rooms/create-room!` are registered at provision time; after a JVM
   restart the registry re-hydrates lazily here via drooms/create-room!
   (idempotent on slug)."
  [room-uuid]
  (when-let [{:room/keys [slug name type]} (rooms/get-room room-uuid)]
    (when slug
      (let [room-id (rstore/slug->room-id slug)]
        (or (rreg/lookup room-id)
            (binding [rtc/*execution-context* ctx/server-context]
              (drooms/create-room! {:title (or name slug)
                                    :slug slug
                                    :type (or type :internal)
                                    :parent-id false
                                    :ctx ctx/server-context})
              (rreg/lookup room-id)))))))

;; =============================================================================
;; Interim content-DB persistence (client render path — removed in Stage 5)
;; =============================================================================

(defn ensure-room-party-entity!
  "Ensure the room content DB has a minimal author entity (S.User projection)
   for the party. Idempotent. Public: the telegram room mirror uses it too."
  [room-conn party]
  (let [party-id (:party/id party)]
    (when-not (d-api/q '[:find ?e . :in $ ?uuid :where [?e :entity/uuid ?uuid]]
                       @room-conn party-id)
      (d-api/transact room-conn
        [(cond-> {:entity/uuid party-id
                  :entity/created-at (java.util.Date.)
                  :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-000000000029"]
                  :S.User/display-name (or (:party/display-name party) "Unknown")}
           (:party/handle party) (assoc :S.User/handle (:party/handle party))
           (:party/email party)  (assoc :S.User/email (:party/email party))
           (= :agent (:party/type party)) (assoc :S.User/is-ai true))]))))

(defn persist-message!
  "Write one message row into a room content DB (interim client render path).
   Public: the telegram room mirror uses it too. `reasoning` (optional) is
   the agent's <think> content, rendered collapsed in the UI.

   `sent-at` (optional) overrides the timestamp. Anything replaying a
   conversation that already happened needs this — a demo scenario seeding a
   plausible history, and the telegram mirror, which otherwise stamps every
   backfilled message with the moment it was imported."
  ([room-conn msg-uuid content room-uuid author-uuid]
   (persist-message! room-conn msg-uuid content room-uuid author-uuid nil nil))
  ([room-conn msg-uuid content room-uuid author-uuid reasoning]
   (persist-message! room-conn msg-uuid content room-uuid author-uuid reasoning nil))
  ([room-conn msg-uuid content room-uuid author-uuid reasoning sent-at]
   (let [now (or sent-at (java.util.Date.))]
     (d-api/transact room-conn
       [(cond-> {:entity/uuid msg-uuid
                 :entity/created-at now
                 :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-00000000002b"]
                 :block/content content
                 :S.Message/author [:entity/uuid author-uuid]
                 :S.Message/room [:entity/uuid room-uuid]
                 :S.Message/sent-at now}
          (and (string? reasoning) (seq reasoning))
          (assoc :S.Message/reasoning reasoning))]))))

;; =============================================================================
;; KB lookup (grants, Stage 1b)
;; =============================================================================

(defn- get-room-kb-conns
  "Product KBs the agent can read/write in this room. Prefers KBs attached
   via grants; falls back to the room creator's default KB."
  [room-uuid]
  (let [room-kbs (kbs/get-room-kbs room-uuid)
        named-conns (->> room-kbs
                         (keep (fn [kb]
                                 (when-let [conn (kbs/connect-kb-database (:kb/db-scope kb))]
                                   {:name (:kb/name kb)
                                    :conn conn
                                    :db-scope (:kb/db-scope kb)
                                    ;; katzen binding: Entity ≡ S/Page (doc/kb-unification.md)
                                    :binding kbs/simmis-kb-binding})))
                         vec)]
    (if (seq named-conns)
      named-conns
      (let [room (rooms/get-room room-uuid)
            creator-id (:room/created-by room)
            first-kb (first (when creator-id (kbs/get-party-kbs creator-id)))
            creator-conn (when first-kb
                           (kbs/connect-kb-database (:kb/db-scope first-kb)))]
        (when creator-conn
          [{:name (:kb/name first-kb)
            :conn creator-conn
            :db-scope (:kb/db-scope first-kb)}])))))

;; =============================================================================
;; Simmis SCI vocabulary: wiki/* (read) + kb/* (write) over product KBs
;; =============================================================================

;; Proposal overlays: {:title … :forks {[scope agent] → branch-kw} …}.
;; The agent's kb/* and kontor/* conn-fns consult the overlay so writes during
;; an active proposal land on fork branches instead of trunk (doc §6 — NOT
;; ctx-forking; matches the existing conn-fn indirection).
;;
;; Two overlays exist per agent and exactly one is ACTIVE at a time:
;;
;;   PRIVATE   — per (room, agent), opened by `proposal/start!`. One agent's
;;               work, filed as its own proposal. Unchanged behaviour.
;;   CAMPAIGN  — per room, opened by `proposal/open-campaign!` and joined by
;;               `proposal/join-campaign!`. Every member's forked writes record
;;               into the SAME overlay, so `file!` produces ONE proposal
;;               carrying every contributor's work. Without it four agents in a
;;               room produce four proposals, and a reviewer has no object that
;;               says "this is the joint change".
(defonce ^:private proposal-overlays (atom {}))
(defonce ^:private room-campaigns (atom {}))

(defn- overlay-cell
  "The atom `registry` holds for `k`, minted once. `swap!` may retry, so the
   atom is created inside the update fn's `contains?` guard rather than before
   it — two agents joining a campaign in the same instant must not end up with
   two different cells."
  [registry k]
  (or (get @registry k)
      (get (swap! registry (fn [m] (if (contains? m k) m (assoc m k (atom nil)))))
           k)))

(defn- private-overlay [room-uuid agent-uuid]
  (overlay-cell proposal-overlays [room-uuid agent-uuid]))

(defn- campaign-overlay [room-uuid]
  (overlay-cell room-campaigns room-uuid))

(defn- active-overlay
  "The overlay this agent's forked writes record into: the room campaign once
   it has joined one, otherwise its private overlay. Membership lives IN the
   campaign map, so filing (which resets that map to nil) drops every member at
   once — there is no second place holding a stale join."
  [room-uuid agent-uuid]
  (let [c (campaign-overlay room-uuid)]
    (if (contains? (:members @c) agent-uuid)
      c
      (private-overlay room-uuid agent-uuid))))

(defn- overlay-branch!
  "Branch for `db-scope` under the ACTIVE proposal, minted lazily on
   first access (unique slug — branch names must not be reused within a
   JVM session: stale scriptum branch locks).

   KEYED BY [scope agent], NOT BY SCOPE. Inside a campaign two agents writing
   the same scope therefore get a branch each, filed as two forks, rather than
   sharing one branch. The decision and its cost are argued on
   `proposal/open-campaign!` — in one line: a fork is the unit a reviewer can
   accept or refuse (`ops.proposals/accept-fork!`), and datoms from two agents
   fused onto one branch can no longer be told apart, let alone refused apart.

   `system-type` is recorded per fork so `file!` can say what KIND of artifact
   each branch holds — the semantic diff and the review card both dispatch on
   it, and a book fork rendered as a KB diff shows nothing but datom counts."
  [overlay db-scope agent-uuid system-type]
  ;; A CLOSED token means: you were contributing to a proposal, and it has been
  ;; filed. Falling through to trunk here is what made the guarantee fail OPEN —
  ;; an agent still working when someone else called `file!` had its next writes
  ;; land live, silently, believing they were governed. Refuse instead, and say
  ;; how to recover. Reads never reach this fn (see `existing-fork`), so a
  ;; closed agent can still look at everything; only writing is refused.
  (when-let [c (and overlay (:closed @overlay) @overlay)]
    (throw (ex-info (str "the proposal \"" (:title c) "\" you were contributing to was filed"
                         (when (:proposal c) (str " as " (:proposal c)))
                         " — your writes are no longer governed. Start a follow-up with"
                         " (proposal/open-campaign! \"…\") or (proposal/start! \"…\"), or"
                         " (proposal/release!) to write directly to trunk on purpose.")
                    {:type :proposal-closed :title (:title c) :proposal (:proposal c)})))
  (when (and overlay @overlay)
    (let [k [db-scope agent-uuid]]
      (or (get-in @overlay [:forks k])
          ;; KB yggdrasil systems are registered on the SERVER ctx; the sandbox
          ;; runs on the room ctx where get-kb-system sees nothing — bind it.
          (ctx/with-server-context
            (let [t (:title @overlay)
                  slug (str (or (some-> t
                                        (clojure.string/replace #"[^a-zA-Z0-9]+" "-")
                                        (as-> x (subs x 0 (min 16 (count x)))))
                                "proposal")
                            ;; the agent in the branch name, so a campaign's
                            ;; sibling branches on ONE scope are distinguishable
                            ;; in `list-kb-branches` and in the logs
                            "-" (subs (str agent-uuid) 0 4)
                            "-" (subs (str (random-uuid)) 0 4))
                  ;; `branching`'s neutral API picks the backend from
                  ;; `system-type`. Routing a code fork through the KB verbs
                  ;; would look up a datahike system that does not exist for this
                  ;; scope and answer nil — and a nil branch here means the write
                  ;; lands on trunk, ungoverned, which is the exact failure this
                  ;; overlay exists to prevent.
                  {branch :branch} (branching/fork-branch! db-scope system-type slug)]
              (when branch
                ;; Record the trunk head AT FORK TIME. This is the merge base, and
                ;; capturing it here makes it exact and O(1): the alternative is
                ;; `common-ancestor`, which walks the whole commit graph (measured
                ;; 361 ms on a 325-commit KB, and linear in commit count) and
                ;; cannot answer at all once retention has pruned the base.
                ;; Without a base the diff runs against trunk HEAD, so trunk's own
                ;; concurrent advance shows up as the branch having REMOVED things.
                (let [base (branching/head-id db-scope system-type
                                              (branching/trunk-of db-scope system-type))]
                  (swap! overlay #(-> %
                                      (assoc-in [:forks k] branch)
                                      (assoc-in [:bases k] base)
                                      (assoc-in [:types k] system-type)))
                  (log/log! {:level :info :id ::proposal-fork
                             :data {:scope (str db-scope) :branch branch :base base
                                    :agent (str agent-uuid) :system-type system-type}}))
                branch)))))))

;; ---------------------------------------------------------------------------
;; Code forks — an agent's repo writes, governed by the active proposal.
;;
;; A repository is not a datahike store and cannot be governed the same way. A
;; KB write is redirected by handing the writer a different CONNECTION; a repo
;; write goes through a muschel filesystem that dvergr's tool context captures
;; ONCE PER TURN, and geschichte's one current ref means every agent in a room
;; otherwise shares a HEAD. So the fork here is an execution-context fork —
;; `ygg/fork!`, whose overlay gives this agent a private workspace — and the
;; redirection is a swap of the tool context's `:filesystem`.
;; ---------------------------------------------------------------------------

(defn- room-repo-scope
  "`:system/id` of the room's OWN writable repo, or nil.

   The id rather than the path, because that is what a filed fork can carry:
   `:proposal.fork/scope` is `:db.type/uuid`."
  [room-uuid]
  (->> (sdb/systems-for-room room-uuid)
       (filter #(and (= :repo (get-in % [:system :system/type]))
                     (#{:owner :read-write} (:permission %))))
       first :system :system/id))

(defn- repo-fork!
  "The [scope agent] code fork under `overlay`, minted on first WRITE.

   Two things are minted together and must stay together: the canonical branch
   (so the proposal has something reviewable to name, and a recorded fork point)
   and the context fork (so the agent has somewhere isolated to write). Neither
   is useful alone — a branch with no workspace collects nothing, a workspace
   with no branch has nowhere to publish.

   Returns the fork map, or nil when there is no active proposal or no repo."
  [overlay room-uuid agent-uuid]
  (when-let [scope (room-repo-scope room-uuid)]
    (when (and overlay @overlay)
      (or (get-in @overlay [:ctx-forks [scope agent-uuid]])
          ;; `overlay-branch!` records :forks/:bases/:types, and refuses when the
          ;; proposal has been filed — so a closed agent cannot open a code fork
          ;; either, the same guarantee its KB writes get.
          (when-let [branch (overlay-branch! overlay scope agent-uuid :repo)]
            ;; on the fork's OWN branch — see `open-repo-fork!`. A reopened fork
            ;; must extend what it already published, not start again from trunk.
            (when-let [fork (branching/open-repo-fork! scope branch)]
              (swap! overlay assoc-in [:ctx-forks [scope agent-uuid]] fork)
              (log/log! {:level :info :id ::code-fork-opened
                         :data {:scope (str scope) :agent (str agent-uuid)}})
              fork))))))

(def ^:private repo-write-tools
  "The subset of `repo-tools` that can CHANGE the tree, and so must mint a fork.

   Everything else in `repo-tools` reaches the filesystem read-only and gets an
   ALREADY-OPEN fork or trunk — never a freshly minted one. `repo-fork!` says
   \"minted on first WRITE\"; until this split existed it minted on every call,
   so a single `read_file` or `grep` under an open proposal produced a canonical
   branch identical to trunk. `file!` then had something in `:forks`, so its
   \"no writes happened — nothing to propose\" guard stopped firing and it ran a
   full sandboxed test suite over a branch with no changes in it.

   `shell` and `clojure_eval` are here because they CAN write — an agent can
   `spit` from either — and we cannot know until the code runs. Minting for them
   is the conservative choice: a spurious fork is noise, an ungoverned write is
   the thing this whole mechanism exists to prevent."
  #{"write_file" "edit_file" "clojure_edit" "shell" "clojure_eval"})

(defn- default-repo-workspace
  "The workspace a repo tool would resolve for itself when handed none — the
   same pair `dvergr.tools/make-context` builds via its private
   `resolve-default-workspace`. Needed so the CLOSED case can wrap that tree
   read-only instead of passing it through writable."
  []
  (try
    (when-let [ws ((requiring-resolve 'dvergr.substrate.geschichte/current-workspace))]
      {:workspace ws
       :filesystem ((requiring-resolve 'dvergr.substrate.geschichte/filesystem) ws)})
    (catch Throwable _ nil)))

(defn- read-only-filesystem
  "`fs`, with every WRITE refused and every read passed through.

   The muschel FS protocol splits cleanly along this line, so the wrapper is
   mechanical: `-open-sink`, `-mkdir`, `-delete`, `-rename`, `-touch`, `-chmod`,
   `-symlink` and `-chown` throw; everything else delegates. `-cd!` delegates —
   moving the cwd mutates the handle, not the tree.

   Exists so a CLOSED proposal can refuse writes without refusing to run. See
   `wrap-repo-tool`."
  [fs closed]
  ;; `fs` may be nil: `default-repo-workspace` resolves nothing outside a live
  ;; room context. A nil delegate still has to produce a REFUSING filesystem
  ;; rather than no filesystem, because handing the tool nothing lets it
  ;; resolve its own writable default — the fail-open this exists to close.
  ;; Reads then answer empty, which is truthful: there is no tree.
  (let [refuse (fn [op]
                 (throw (ex-info (str "the proposal \"" (:title closed) "\" you were contributing"
                                      " to was filed"
                                      (when (:proposal closed) (str " as " (:proposal closed)))
                                      " — this file write (" op ") is refused because it would"
                                      " land on trunk ungoverned. Start a follow-up with"
                                      " (proposal/open-campaign! \"…\") or (proposal/start! \"…\"),"
                                      " or (proposal/release!) to write directly to trunk on"
                                      " purpose.")
                                 {:type :proposal-closed :op op
                                  :title (:title closed) :proposal (:proposal closed)})))]
    (reify mfs/FS
      (-resolve [_ path] (when fs (mfs/-resolve fs path)))
      (-cwd [_] (if fs (mfs/-cwd fs) "/"))
      (-cd! [_ path] (when fs (mfs/-cd! fs path)))
      (-exists? [_ path] (boolean (when fs (mfs/-exists? fs path))))
      (-stat [_ path] (when fs (mfs/-stat fs path)))
      (-list-dir [_ path] (if fs (mfs/-list-dir fs path) []))
      (-read-file [_ path] (when fs (mfs/-read-file fs path)))
      (-read-bytes [_ path] (when fs (mfs/-read-bytes fs path)))
      (-open-source [_ path] (when fs (mfs/-open-source fs path)))
      (-sandbox-relativize [_ p] (if fs (mfs/-sandbox-relativize fs p) p))
      (-physical-path [_ p] (if fs (mfs/-physical-path fs p) p))
      (-open-sink [_ _ _] (refuse "write"))
      (-mkdir [_ _] (refuse "mkdir"))
      (-delete [_ _] (refuse "delete"))
      (-rename [_ _ _] (refuse "rename"))
      (-touch [_ _] (refuse "touch"))
      (-chmod [_ _ _] (refuse "chmod"))
      (-symlink [_ _ _] (refuse "symlink"))
      (-chown [_ _ _ _] (refuse "chown")))))

(defn- existing-ctx-fork
  "The agent's already-open code fork under `overlay`, or nil. Never mints —
   the READ half of `repo-fork!`, in the same shape as `existing-fork` is to
   `overlay-branch!`."
  [overlay room-uuid agent-uuid]
  (when-let [scope (room-repo-scope room-uuid)]
    (when (and overlay @overlay)
      (get-in @overlay [:ctx-forks [scope agent-uuid]]))))

(defn- wrap-repo-tool
  "Redirect one file tool onto the agent's code fork when a proposal is active.

   The interception is a swap of `:filesystem` (and `:workspace`) in the tool
   context, NOT a rebinding of the execution context. `dvergr.tools/make-context`
   resolves a workspace only `when-not` those keys are supplied, and every file
   verb reads `:filesystem` straight off that map — so this redirects the
   filesystem and NOTHING else. The room's messages, its KBs and its book all
   resolve elsewhere and keep resolving elsewhere, which a broader context
   binding would quietly have changed.

   With no proposal active the tool is passed through untouched: writing
   directly to trunk is the normal, ungoverned case, exactly as for `kb/*`."
  [tool overlay-fn room-uuid agent-uuid]
  (assoc tool :execute
         (fn [params tctx]
           (let [ov     (overlay-fn)
                 closed (when (and ov @ov (:closed @ov)) @ov)
                 write? (contains? repo-write-tools (:name tool))
                 fs
                 (cond
                   ;; FILED. The tool still RUNS — refusing outright bricked the
                   ;; agent, because `clojure_eval` is in `repo-tools` and the
                   ;; recovery verb `(proposal/release!)` is evaluated THROUGH
                   ;; `clojure_eval`, so the escape hatch sat behind the door it
                   ;; was meant to open.
                   ;;
                   ;; But it must not WRITE. An earlier attempt handed the tool
                   ;; nothing here, reasoning that with no fork there was
                   ;; nothing of the proposal's to protect. That conflated two
                   ;; different no-fork states: never-had-one (true, harmless)
                   ;; and `file!`-discarded-it — and since `file!` drops
                   ;; `:ctx-forks` when it stamps the closed token, the second is
                   ;; the ONLY state reachable after filing. Handing back nothing
                   ;; let the tool resolve its own default workspace, writable,
                   ;; so a `write_file` after filing landed on trunk unrefused —
                   ;; fail-OPEN, in the guard that exists to prevent exactly that.
                   ;;
                   ;; So: wrap whichever tree it would have used, read-only.
                   closed
                   (let [base (or (some-> (existing-ctx-fork ov room-uuid agent-uuid)
                                          branching/repo-fork-workspace)
                                  (default-repo-workspace))]
                     (assoc (or base {})
                            :filesystem (read-only-filesystem (:filesystem base) closed)))

                   ;; Can change the tree ⇒ mint the fork so the change is
                   ;; governed.
                   write?
                   (some-> (repo-fork! ov room-uuid agent-uuid)
                           branching/repo-fork-workspace)

                   ;; Read-only ⇒ an already-open fork if there is one, else
                   ;; trunk. Never mints. See `repo-write-tools`.
                   :else
                   (some-> (existing-ctx-fork ov room-uuid agent-uuid)
                           branching/repo-fork-workspace))]
             ((:execute tool) params (if fs (merge tctx fs) tctx))))))

(defn- wrap-run-tests-tool
  "Run the agent's tests the way the REVIEWER's check runs them: from the files
   on the branch, in a vanilla sandbox.

   Two things were wrong with the session-local behaviour, and they compound.
   `clojure.test` enumerates the vars a SESSION has loaded, so an agent that
   wrote `src/foo_test.clj` and never required it ran zero tests and was told
   everything passed. And what a session has loaded is not a property of the
   branch — it is a property of that turn, so the same branch could report
   differently twice, and neither answer was the one the reviewer would get.

   Now both go through `checks/run-tests-on-fs`: same enumeration, same fresh
   interpreter, one meaning for \"the tests on this branch\". A result the agent
   sees is a result the reviewer will see.

   Falls back to the tool's own behaviour when there is no workspace (a
   room-less context), rather than reporting nothing."
  [tool]
  (assoc tool :execute
         (fn [params {:keys [filesystem] :as tctx}]
           (if-not filesystem
             ((:execute tool) params tctx)
             (try
               (let [{:keys [status tests passed failed errors output]}
                     ((requiring-resolve 'is.simm.ops.checks/run-tests-on-fs) filesystem)]
                 {:type (if (= :fail status) :error :success)
                  :content (str (case status
                                  :pass "Tests pass."
                                  :fail "Tests FAIL."
                                  :error "The tests did not run."
                                  :none "No test namespaces on this branch."
                                  (str status))
                                (when (and tests (pos? tests))
                                  (str " " passed " passed"
                                       (when (pos? (or failed 0)) (str ", " failed " failed"))
                                       (when (pos? (or errors 0)) (str ", " errors " errored"))
                                       ", " tests " namespaces."))
                                (when (seq output) (str "\n\n" output)))
                  :metadata {:tests tests :passed passed
                             :failed failed :errors errors
                             :exit-code (if (= :fail status) 1 0)}})
               (catch Exception e
                 {:type :error
                  :error (str "Could not run the branch's tests: " (.getMessage e))}))))))

(def ^:private repo-tools
  "The tools that reach the workspace filesystem, and so must follow a code fork.

   `clojure_eval` is deliberately included: an agent can `spit` a file from it,
   and leaving that one path ungoverned is precisely the fail-open shape that
   let governed KB writes land on trunk unnoticed."
  #{"read_file" "write_file" "edit_file" "clojure_edit" "clj_kondo"
    "shell" "grep" "glob" "run_tests" "clojure_eval"})

(defn- publish-code-forks!
  "Publish every open code fork onto its canonical branch, then close it.

   Called when a proposal is FILED — the point at which the agent's work stops
   being private and becomes something a reviewer must be able to see. Until
   then the commits live only in the workspace, which is what keeps an
   in-progress fork off the review surface."
  [overlay]
  (doseq [[[scope agent] fork] (:ctx-forks @overlay)]
    (let [branch (get-in @overlay [:forks [scope agent]])]
      (try
        (when branch
          (branching/publish-repo-fork! fork (keyword (name branch))))
        (catch Exception e
          (log/log! {:level :error :id ::code-fork-publish-failed
                     :msg "Code fork not published — its work is only in the workspace"
                     :data {:scope (str scope) :branch (str branch)
                            :error (.getMessage e)}}))
        (finally (branching/close-repo-fork! fork)))))
  (swap! overlay dissoc :ctx-forks))

(defn- discard-code-forks!
  "Close every open code fork WITHOUT publishing — the withdrawal path."
  [overlay]
  (doseq [[_ fork] (:ctx-forks @overlay)]
    (branching/close-repo-fork! fork))
  (swap! overlay dissoc :ctx-forks))

(defn reopen-fork!
  "Put `agent-uuid` back to writing on a fork branch it already filed.

   The revise half of review. `file!` stamps a `:closed` token so the next
   governed write is refused rather than silently trunked; a request for changes
   has to lift exactly that, and put the agent back on the SAME branch — a new
   one would strand the work the reviewer just read and commented on, and the
   proposal row names the old branch.

   Reuses the existing branch by seeding the overlay's `:forks` entry directly
   rather than going through `overlay-branch!`, which mints. `:bases` is
   deliberately NOT re-recorded: the fork point has not moved, and overwriting it
   with today's trunk head would make the next diff hide everything the fork has
   done so far.

   A code fork gets a fresh workspace — the old one was discarded at filing —
   opened lazily on the next write by `repo-fork!`, which finds the branch
   already in `:forks` and forks a workspace for it.

   Returns true when an overlay was reopened. False means the author is not an
   agent this process holds a cell for (a human reviewer, or a restart), which
   is not a failure: the comment is the durable record, and reopening is a
   convenience for a live agent."
  [room-uuid agent-uuid scope branch system-type title]
  (boolean
   (when (and room-uuid agent-uuid scope branch)
     (let [cell (private-overlay room-uuid agent-uuid)
           o @cell
           ;; REFUSE to reopen into an UNRELATED open proposal.
           ;;
           ;; Three states reach here. nil and `:closed` both start clean. The
           ;; third — the agent has since begun its OWN new proposal — used to
           ;; fall through to `(or o {})`, which merged the old proposal's
           ;; branch into the new one's `:forks` and put the OLD title first in
           ;; `(or title (:title o) …)`, silently renaming work in progress.
           ;; The agent's next `file!` then filed that branch a second time,
           ;; into a second proposal.
           ;;
           ;; Same-proposal reopens must still work: a reviewer can request
           ;; changes on several forks of one proposal, and the second call
           ;; finds the overlay the first one created. The title is what tells
           ;; them apart — the caller passes `(:proposal/title p)`.
           unrelated? (and o (not (:closed o)) title (:title o)
                           (not= title (:title o)))]
       (if unrelated?
         (do (log/log! {:level :warn :id ::fork-reopen-refused
                        :data {:room (str room-uuid) :agent (str agent-uuid)
                               :scope (str scope)
                               :requested title :in-progress (:title o)}}
                       "not reopening — the agent has a different proposal open")
             false)
         (do
           (swap! cell
                  (fn [o]
                    (-> (if (:closed o) {} (or o {}))
                    (assoc :title (or title (:title o) "revision"))
                    (dissoc :closed :proposal)
                    (assoc-in [:forks [scope agent-uuid]] branch)
                    (assoc-in [:types [scope agent-uuid]] (or system-type :kb))
                    ;; keep any base already recorded; only seed one if absent
                    (update-in [:bases [scope agent-uuid]]
                               #(or % (branching/head-id
                                       scope (or system-type :kb)
                                       (branching/trunk-of scope (or system-type :kb))))))))
           (log/log! {:level :info :id ::fork-reopened
                      :data {:room (str room-uuid) :agent (str agent-uuid)
                             :scope (str scope) :branch (str branch)}})
           true))))))

(defn- existing-fork
  "The fork branch ALREADY minted for `[db-scope agent-uuid]` under `overlay`,
   or nil. Never mints — this is the READ half of the split `overlay-branch!`
   opens.

   Why the split exists: minting used to happen in the one conn resolver that
   reads and writes shared, so merely CALLING `kontor/balances` or `kb/link`
   forked the database. A live four-agent run filed a proposal whose only two
   forks were branches minted by reads and identical to trunk — the reviewer was
   asked to accept or refuse two changes that did not exist.

   Read-your-own-writes still holds, and needs no minting to do so: a fork
   exists if and only if a write happened, so whenever there is something of the
   agent's own to read back, `existing-fork` finds the branch holding it. Before
   any write, trunk and the not-yet-minted branch have identical content by
   construction — a branch is CoW-forked from trunk head."
  [overlay db-scope agent-uuid]
  (when (and overlay @overlay)
    (get-in @overlay [:forks [db-scope agent-uuid]])))

(defn- add-kb-write-ns!
  "Install the `kb/*` SCI vocabulary — the agent's GUARDED datahike write surface
   over the room's product KBs. EVERY fn takes an EXPLICIT database as its first
   arg: a KB NAME (string, as shown in the room-context block) or its db-scope
   uuid/string. It is resolved against the KBs attached to THIS room, so an
   unknown/ungranted name is refused — the agent can only touch KBs in its room.
   Branch-aware (an active proposal overlay redirects writes onto its fork
   branch). `kb-conns` is the room's KB list ([{:name :conn :db-scope}…]).
   `overlay-fn` is called PER WRITE rather than closed over once: an agent can
   join a campaign mid-turn, and which overlay is active is exactly what that
   changes."
  [sci-ctx kb-conns overlay-fn agent-uuid]
  (let [by-name  (into {} (map (juxt :name identity)) kb-conns)
        by-scope (into {} (map (juxt (comp str :db-scope) identity)) kb-conns)
        target   (fn [db]
                   (or (get by-name db)
                       (get by-scope (str db))
                       (throw (ex-info (str "no KB " (pr-str db) " attached to this room. "
                                            "Available: " (pr-str (mapv :name kb-conns)))
                                       {:type :unknown-kb :db db}))))
        ;; Branch/overlay-aware live conns for a resolved target map, in two
        ;; halves. WRITE mints the fork on first use; READ resolves one only if
        ;; it already exists (see `existing-fork`) and otherwise reads trunk.
        ;; Every verb below picks deliberately: an unconditional writer takes
        ;; `write-conn-of`, a pure reader `read-conn-of`, and a CONDITIONAL
        ;; writer reads first and only takes the write conn on the branch that
        ;; actually transacts — so a no-op call cannot mint a fork either.
        write-conn-for (fn [{:keys [db-scope conn]}]
                         (or (when-let [b (overlay-branch! (overlay-fn) db-scope agent-uuid :kb)]
                               (ctx/with-server-context (branching/get-kb-conn-on-branch db-scope b)))
                             (branching/get-kb-conn db-scope)
                             conn))
        read-conn-for (fn [{:keys [db-scope conn]}]
                        (or (when-let [b (existing-fork (overlay-fn) db-scope agent-uuid)]
                              (ctx/with-server-context (branching/get-kb-conn-on-branch db-scope b)))
                            (branching/get-kb-conn db-scope)
                            conn))
        write-conn-of (fn [db] (write-conn-for (target db)))
        read-conn-of  (fn [db] (read-conn-for (target db)))
        ;; the conditional writers' shape: check on read, transact on write
        conn-of  write-conn-of
        title->uuid (fn [conn title]
                      (d-api/q '[:find ?u . :in $ ?t :where
                                 [?e :S.Page/title ?t] [?e :entity/uuid ?u]] @conn title))
        uuid->title (fn [conn uuid]
                      (d-api/q '[:find ?t . :in $ ?u :where
                                 [?e :entity/uuid ?u] [?e :S.Page/title ?t]] @conn uuid))
        next-order (fn [conn page-uuid]
                     (let [max-order (d-api/q '[:find (max ?order) . :in $ ?p :where
                                                [?page :entity/uuid ?p] [?block :block/parent ?page]
                                                [?block :block/order ?order]]
                                              @conn page-uuid)]
                       (frac/generate-key-between max-order nil)))]
    (sci/add-namespace! sci-ctx 'kb
      (vocab/with-docs 'kb
       {'ensure-page!
       (fn [db title]
         ;; existence check on the READ conn: an idempotent hit must not fork
         (or (title->uuid (read-conn-of db) title)
             (let [conn (write-conn-of db)
                   new-uuid (java.util.UUID/randomUUID) now (java.util.Date.)]
                 (d-api/transact conn [{:entity/uuid new-uuid
                                        :entity/name (str "Page " (subs (str new-uuid) 0 8))
                                        :entity/created-at now :entity/updated-at now
                                        :instance/of-role [:entity/name "S/Page"]
                                        :S.Page/title title :S.Page/archived false}
                                       ;; UI invariant: every page has ≥1 block
                                       {:entity/uuid (java.util.UUID/randomUUID)
                                        :entity/created-at now :entity/updated-at now
                                        :instance/of-role [:entity/name "S/Block"]
                                        :block/parent [:entity/uuid new-uuid]
                                        :block/order "a0" :block/content ""}])
                 new-uuid)))

       'next-order (fn [db page-uuid] (next-order (read-conn-of db) page-uuid))

       'upsert-block!
       (fn [db page-uuid content & [order]]
         (let [conn (conn-of db)
               actual-order (or order (next-order conn page-uuid))
               block-uuid (java.util.UUID/randomUUID) now (java.util.Date.)]
           (d-api/transact conn [{:entity/uuid block-uuid
                                  :entity/created-at now :entity/updated-at now
                                  :instance/of-role [:entity/name "S/Block"]
                                  :block/parent [:entity/uuid page-uuid]
                                  :block/order actual-order :block/content content}])
           ;; [[Title]] in the text become stored SAME-KB refs (backlinks). For
           ;; CROSS-database links, embed a (kb/link …) result instead.
           (kbs/link-block-references! conn block-uuid content)
           block-uuid))

       'archive-page!
       (fn [db title]
         ;; absent page ⇒ no-op, and a no-op must not mint a fork
         (when-let [uuid (title->uuid (read-conn-of db) title)]
           (d-api/transact (write-conn-of db) [{:entity/uuid uuid :S.Page/archived true}])
           uuid))

       'retract-block!
       (fn [db block-uuid]
         (d-api/transact (conn-of db) [[:db/retractEntity [:entity/uuid block-uuid]]])
         nil)

       'install-attr!
       (fn [db ident value-type cardinality]
         ;; already-declared ⇒ no-op, and a no-op must not mint a fork
         (when-not (d-api/q '[:find ?e . :in $ ?id :where [?e :db/ident ?id]]
                            @(read-conn-of db) ident)
           (d-api/transact (write-conn-of db) [{:db/ident ident :db/valueType value-type
                                                :db/cardinality cardinality}]))
         ident)

       'upsert-viz-block!
       (fn [db page-uuid viz-spec-edn & [caption]]
         (let [conn (conn-of db)
               block-uuid (java.util.UUID/randomUUID) now (java.util.Date.)]
           (d-api/transact conn [{:entity/uuid block-uuid
                                  :entity/created-at now :entity/updated-at now
                                  :instance/of-role [:entity/name "S/Block"]
                                  :block/parent [:entity/uuid page-uuid]
                                  :block/order (next-order conn page-uuid)
                                  :block/content (str "<p>" (or caption "") "</p>")
                                  :block/viz-spec (if (string? viz-spec-edn) viz-spec-edn (pr-str viz-spec-edn))}])
           block-uuid))

       'upsert-widget-block!
       (fn [db page-uuid widget-code & [caption]]
         (let [conn (conn-of db)
               block-uuid (java.util.UUID/randomUUID) now (java.util.Date.)]
           (d-api/transact conn [{:entity/uuid block-uuid
                                  :entity/created-at now :entity/updated-at now
                                  :instance/of-role [:entity/name "S/Block"]
                                  :block/parent [:entity/uuid page-uuid]
                                  :block/order (next-order conn page-uuid)
                                  :block/content (str "<p>" (or caption "") "</p>")
                                  :block/widget-code (if (string? widget-code) widget-code (pr-str widget-code))}])
           block-uuid))

       ;; --- typed entities / properties (CRM etc.) --------------------------
       ;; Thin, GUARDED datahike helpers (not bespoke CRM tools): add-type!
       ;; requires an existing type, set-property! a DECLARED attribute — so no
       ;; junk/undeclared data lands.
       'add-type!
       (fn [db entity-uuid type-name]
         ;; refusal is a no-op, so the type check reads before anything forks
         (if-not (d-api/q '[:find ?e . :in $ ?n :where
                            [?e :entity/name ?n] [?e :object/of-category _]]
                          @(read-conn-of db) type-name)
           {:error (str "unknown type '" type-name "' — must be an existing category-S object")}
           (do (d-api/transact (write-conn-of db) [{:entity/uuid entity-uuid
                                                    :instance/of-role [:entity/name type-name]}])
               entity-uuid)))

       'set-property!
       (fn [db entity-uuid attr value]
         ;; both refusals are no-ops, so they are decided on the read conn
         (let [attr-kw (if (keyword? attr) attr (keyword attr))]
           (cond
             (not (d-api/q '[:find ?e . :in $ ?a :where [?e :db/ident ?a]]
                           @(read-conn-of db) attr-kw))
             {:error (str "unknown property " attr-kw " — declare it (install-attr!) or use a"
                          " defined one such as :S.Person/company, :S.Person/email …")}
             (= "db" (namespace attr-kw))
             {:error "refusing to set a :db/* system attribute"}
             :else
             (do (d-api/transact (write-conn-of db) [{:entity/uuid entity-uuid attr-kw value}])
                 entity-uuid))))

       'retract-property!
       (fn [db entity-uuid attr]
         (d-api/transact (conn-of db) [[:db/retract [:entity/uuid entity-uuid]
                                        (if (keyword? attr) attr (keyword attr))]])
         entity-uuid)

       ;; --- reading the data, not just the pages ----------------------------
       ;; Until these existed the vocabulary was write-only for `kb` and
       ;; page-only for `wiki`, so any entity that is not an S/Page was
       ;; unreachable. A live run stalled two agents for 22 and 14 minutes on
       ;; exactly that: 46 :S.Customer entities sat in an attached KB with no
       ;; sanctioned way to see them, and both agents concluded — correctly,
       ;; from where they stood — that the data did not exist. `datahike.api`
       ;; was in the sandbox but inert, because nothing handed out a db.
       ;; All three READ, so none of them forks (see `existing-fork`).
       ;; HANDLES, not wrapped verbs. `datahike.api` is already in the sandbox;
       ;; what was missing was any way to get a database out of a KB NAME, so
       ;; the whole API sat there inert. Handing over the handle means agents
       ;; use the datahike they already know — `d/q`, `d/pull`, `d/datoms`,
       ;; `d/entity` — instead of a bespoke wrapper that has to be learned and
       ;; kept in step with datahike's own surface.
       ;;
       ;; The handle IS the permission. `db` resolves the read path (an existing
       ;; fork, else trunk) and hands back an immutable value; `conn` resolves
       ;; the write path, so transacting on it lands on this agent's proposal
       ;; fork exactly as `kb/upsert-block!` would. Neither can address a KB
       ;; that is not attached to this room — `target` refuses by name first.
       'db
       (fn [db] @(read-conn-of db))

       'conn
       (fn [db] (write-conn-of db))

       'attributes
       ;; The discovery verb. A list of function names would NOT have prevented
       ;; the stall above — the agents needed to learn that :S.Customer entities
       ;; were THERE. Attributes actually carrying data, with how many entities
       ;; carry each, answers "what is in this database" in one call.
       (fn [db]
         (let [d @(read-conn-of db)
               ;; A KB carries the whole katzen/kontor meta-schema alongside its
               ;; actual content, and the meta outnumbers the data ~9:1. Sorting
               ;; the domain types (S.*) to the front keeps the answer to "what
               ;; is in here" at the top of the result instead of 80 rows down.
               ;; Nothing is hidden — an agent that needs the meta still sees it.
               domain? (fn [[a _]] (clojure.string/starts-with? (namespace a) "S."))]
           (->> (d-api/q '[:find ?a (count ?e) :where [?e ?a _]] d)
                (remove #(= "db" (namespace (first %))))
                (sort-by (juxt (complement domain?) (comp namespace first) (comp name first)))
                (mapv (fn [[a n]] {:attribute a :entities n})))))

       ;; --- cross-database links (datahike.reference) -----------------------
       ;; Build a VALIDATED cross-db link to a page in `db`, as `[[dh://…][text]]`.
       ;; Paste the returned string into a CHAT message or a block in ANOTHER
       ;; database. NEVER hand-write a dh:// URI, and NEVER write `[[Title]]`
       ;; across databases (a bare title only resolves within its own KB).
       'link
       (fn [db title & [display]]
         (let [{:keys [db-scope] :as t} (target db)
               uuid (title->uuid (read-conn-for t) title)]
           (if-not uuid
             {:error (str "no page titled " (pr-str title) " in " (pr-str db)
                          " — create it first: (kb/ensure-page! " (pr-str db) " " (pr-str title) ")")}
             (str "[[" (dh-ref/render (dh-ref/reference db-scope [:entity/uuid uuid]))
                  "][" (or display title) "]]"))))

       'link-to
       (fn [db entity-uuid & [display]]
         (let [{:keys [db-scope] :as t} (target db)
               ttl (uuid->title (read-conn-for t) entity-uuid)]
           (if-not ttl
             {:error (str "no page with uuid " entity-uuid " in " (pr-str db))}
             (str "[[" (dh-ref/render (dh-ref/reference db-scope [:entity/uuid entity-uuid]))
                  "][" (or display ttl) "]]"))))}
       vocab/kb-docs))))

(defn- add-wiki-ns!
  "Install the `wiki/*` read vocabulary over the room's product KBs.
   `room-uuid` (optional) additionally enables `wiki/summarize!` — the
   chat→wiki summarizer as a sandbox call, so agents AND room schedules
   can trigger it (`(wiki/summarize!)` in a :schedule/code task)."
  [sci-ctx kb-conns & [room-uuid]]
  ;; `refs/strip-html`, not a fourth local copy. This one decoded three
  ;; entities of six, so `&quot;` and `&nbsp;` reached the agent's prompt
  ;; literally — in the verb an agent uses to READ a page before writing.
  (let [strip-html refs/strip-html]
    (sci/add-namespace! sci-ctx 'wiki
      (vocab/with-docs 'wiki
       {'pages (fn []
                (->> kb-conns
                     (mapv (fn [{:keys [name conn]}]
                             {:kb name
                              :pages (->> (d-api/q '[:find ?title :where [?e :S.Page/title ?title]] @conn)
                                          (map first) sort vec)}))))
       'read-page (fn [title]
                    (some (fn [{:keys [name conn]}]
                            (let [eid (d-api/q '[:find ?e . :in $ ?t :where [?e :S.Page/title ?t]] @conn title)]
                              (when eid
                                (let [blocks (d-api/q '[:find ?c ?o :keys content order :in $ ?p
                                                        :where [?b :block/parent ?p] [?b :block/content ?c] [?b :block/order ?o]]
                                                      @conn eid)]
                                  {:kb name :title title
                                   :blocks (->> (sort-by :order blocks) (mapv (comp strip-html :content)))}))))
                          kb-conns))
       'search (fn [query]
                 ;; Ranked fulltext over titles + block content (scriptum
                 ;; secondary index) when built; title-regex fallback.
                 (let [secondary (try (requiring-resolve 'dvergr.search.secondary/search)
                                      (catch Throwable _ nil))
                       ft (when secondary
                            (->> kb-conns
                                 (mapcat (fn [{:keys [name conn]}]
                                           (let [db @conn]
                                             (for [[eid score] (secondary db kbs/wiki-fulltext-ident query {:limit 12})
                                                   :let [e (d-api/pull db [:S.Page/title :block/content
                                                                           {:block/parent [:S.Page/title]}] eid)
                                                         title (or (:S.Page/title e)
                                                                   (get-in e [:block/parent :S.Page/title]))]
                                                   :when title]
                                               {:kb name :title title :score score
                                                :snippet (when-let [c (:block/content e)]
                                                           (subs (str/replace c #"<[^>]+>" "") 0
                                                                 (min 140 (count (str/replace c #"<[^>]+>" "")))))}))))
                                 (sort-by :score >)
                                 (take 12)
                                 vec))]
                   (if (seq ft)
                     ft
                     (let [pattern (re-pattern (str "(?i)" query))]
                       (->> kb-conns
                            (mapcat (fn [{:keys [name conn]}]
                                      (->> (d-api/q '[:find ?title :where [?e :S.Page/title ?title]] @conn)
                                           (map first)
                                           (filter #(re-find pattern %))
                                           (map (fn [title] {:kb name :title title})))))
                            (sort-by :title) vec)))))

       'backlinks (fn [title]
                    (->> kb-conns
                         (mapcat (fn [{:keys [name conn]}]
                                   (->> (d-api/q '[:find [?src-title ...] :in $ ?t :where
                                                   [?p :S.Page/title ?t]
                                                   [?b :block/references ?p]
                                                   [?b :block/parent ?src]
                                                   [?src :S.Page/title ?src-title]]
                                                 @conn title)
                                        (map (fn [st] {:kb name :title st})))))
                         distinct vec))

       'neighborhood (fn [title]
                       ;; The read-before-write context: the page + its forward
                       ;; links (block-refs rollup — the Σ view of the tree) +
                       ;; backlinks. Load these BEFORE editing (see protocol).
                       (let [out (->> kb-conns
                                      (mapcat (fn [{:keys [name conn]}]
                                                (->> (d-api/q '[:find [?tt ...] :in $ ?t :where
                                                                [?p :S.Page/title ?t]
                                                                [?b :block/parent ?p]
                                                                [?b :block/references ?tp]
                                                                [?tp :S.Page/title ?tt]]
                                                              @conn title)
                                                     (map (fn [tt] {:kb name :title tt})))))
                                      distinct vec)
                             back (->> kb-conns
                                       (mapcat (fn [{:keys [name conn]}]
                                                 (->> (d-api/q '[:find [?src-title ...] :in $ ?t :where
                                                                 [?p :S.Page/title ?t]
                                                                 [?b :block/references ?p]
                                                                 [?b :block/parent ?src]
                                                                 [?src :S.Page/title ?src-title]]
                                                               @conn title)
                                                      (map (fn [st] {:kb name :title st})))))
                                       distinct vec)]
                         {:page title :links-to out :linked-from back}))

       'summarize!
       (fn []
         (if room-uuid
           ((requiring-resolve 'is.simm.agents.summarizer/summarize-room!)
            room-uuid
            (requiring-resolve 'is.simm.agents.room-agents/live-room))
           {:error "summarize! needs a room-bound wiki ns"}))}
       vocab/wiki-docs))))

(defn- overlay-forks
  "An overlay's `{[scope agent] → branch}` map as the fork vector
   `ops.proposals/file-proposal!` takes. `base-commit` is the trunk head
   captured when the branch was minted (see `overlay-branch!`); the schema has
   always declared it, and before it was supplied every diff fell back to a
   commit-graph walk.

   The AGENT half of the key rides along as `:author`. It used to be dropped
   here, which is why a campaign's forks reached the review card anonymous: four
   agents could contribute four forks and a reviewer deciding them one at a time
   had nothing to tell them apart."
  [o]
  (mapv (fn [[k branch]]
          (cond-> {:scope (first k) :branch branch
                   :author (second k)
                   :system-type (get-in o [:types k] :kb)}
            (get-in o [:bases k]) (assoc :base-commit (get-in o [:bases k]))))
        (:forks o)))

(defn- discard-overlay-forks!
  "Delete the branches behind `forks` (an overlay `:forks` subset), surviving a
   failure — one undeletable branch must not strand the withdrawal of the rest.

   `types` is the overlay's `:types` map, and it is what routes each branch to
   the right backend. This used to call `discard-kb-branch!` unconditionally.
   On a `:repo` fork that looks up a KB system which does not exist for the
   scope and returns `:not-registered` — no exception, so the `catch` below
   never even saw it, and the repo branch survived `abandon!` with nothing
   pointing at it. `ops/proposals.clj` already routes through the neutral
   dispatcher and says why: \"the failure mode of forgetting a dispatch is
   silent, so the fewer places that can forget, the better.\" This was the
   place that forgot."
  [forks types]
  (ctx/with-server-context
    (doseq [[k b] forks
            :let [[scope _] k]]
      (try (branching/drop-branch! scope (get types k :kb) b)
           (catch Exception _ nil)))))

(defn- add-proposal-ns!
  "Install the `proposal/*` SCI vocabulary — safe structural changes:
   start! redirects subsequent kb/* and kontor/* writes onto fork branches;
   file! records the ForkSet + AI summary and clears the overlay; abandon!
   discards. (doc/proposals-and-time-travel.md §6)

   Two modes, one filing path — see the overlay comment above
   `proposal-overlays`. `start!` is private to this agent; `open-campaign!` /
   `join-campaign!` put several agents' forks into ONE room-level overlay so
   `file!` produces a single proposal carrying all of them."
  [sci-ctx room-uuid agent-uuid]
  (let [priv    #(private-overlay room-uuid agent-uuid)
        camp    #(campaign-overlay room-uuid)
        active  #(active-overlay room-uuid agent-uuid)
        member? #(contains? (:members @(camp)) agent-uuid)
        mine    (fn [o] (into {} (filter (fn [[[_ a] _]] (= a agent-uuid))) (:forks o)))
        ;; A `:closed` token sits in an agent's OWN cell after the proposal it
        ;; contributed to was filed. It is not an active proposal — it exists so
        ;; the next governed write is REFUSED rather than silently trunked (see
        ;; `overlay-branch!`). Declaring new intent clears it.
        closed?      #(:closed @(priv))
        clear-closed! #(when (closed?) (reset! (priv) nil))
        ;; live proposal state, ignoring a closed token
        open-priv    #(let [o @(priv)] (when-not (:closed o) o))]
    (sci/add-namespace! sci-ctx 'proposal
      (vocab/with-docs 'proposal
       {'start!
        (fn [title]
          (clear-closed!)
          (cond
            ;; refused rather than silently reinterpreted: with a campaign
            ;; joined, `start!`'s writes would record into the SHARED overlay
            ;; and be filed under someone else's title.
            (member?)
            {:error (str "you are contributing to the room campaign \""
                         (:title @(camp)) "\" — (proposal/abandon!) withdraws,"
                         " or (proposal/file!) files the campaign")}

            (open-priv)
            {:error "a proposal is already active — file! or abandon! it first"}

            :else
            (do (reset! (priv) {:title (str title) :forks {}})
                {:started (str title)
                 :note "kb/* and kontor/* writes now land on fork branches; file! when done"})))

        'open-campaign!
        (fn [title]
          (clear-closed!)
          (cond
            (open-priv)
            {:error "you have a private proposal open — file! or abandon! it first"}

            @(camp)
            {:error (str "a campaign is already open in this room: \""
                         (:title @(camp)) "\" — (proposal/join-campaign!) to contribute")}

            :else
            (do (reset! (camp) {:title (str title) :forks {} :campaign? true
                                :members #{agent-uuid}})
                (log/log! {:level :info :id ::campaign-opened
                           :data {:room (str room-uuid) :agent (str agent-uuid)
                                  :title (str title)}})
                {:opened (str title)
                 :note (str "other agents in this room can (proposal/join-campaign!);"
                            " file! files everyone's work as ONE proposal")})))

        'join-campaign!
        ;; Takes no argument — there is only ever one campaign per room, so the
        ;; title is not a selector. An agent that has just READ the title
        ;; naturally passes it, though, and an arity error for a harmless extra
        ;; word is friction with nothing behind it: the room's campaign is the
        ;; one it meant either way. Accept and ignore it, but say so if it names
        ;; a DIFFERENT campaign, because then the agent's model of the room is
        ;; wrong and silently joining the other one would hide that.
        (fn [& [title]]
          (clear-closed!)
          (when (and title @(camp) (not= (str title) (:title @(camp))))
            (log/log! {:level :warn :id ::campaign-title-mismatch
                       :data {:room (str room-uuid) :agent (str agent-uuid)
                              :asked (str title) :open (:title @(camp))}}))
          (cond
            (nil? @(camp))
            {:error "no campaign open in this room — (proposal/open-campaign! \"title\") opens one"}

            (open-priv)
            {:error "you have a private proposal open — file! or abandon! it first"}

            (member?) {:joined (:title @(camp)) :note "you were already a member"}

            :else
            (do (swap! (camp) update :members conj agent-uuid)
                (log/log! {:level :info :id ::campaign-joined
                           :data {:room (str room-uuid) :agent (str agent-uuid)}})
                {:joined (:title @(camp))
                 :note (str "your kb/* and kontor/* writes now fork into the campaign;"
                            " (proposal/abandon!) withdraws just your part")})))

        'active
        (fn []
          (if (closed?)
            ;; NOT nil: "you have no proposal" and "the proposal you were in was
            ;; filed" are different situations and the second one makes your next
            ;; write fail, so it has to be visible here.
            (let [c @(priv)]
              {:closed true :title (:title c) :proposal (:proposal c)
               :note (str "this proposal was filed; governed writes are refused until you"
                          " start a follow-up or (proposal/release!)")})
            (when-let [o @(active)]
              {:title (:title o)
               :campaign? (boolean (:campaign? o))
               :contributors (when (:campaign? o) (mapv str (:members o)))
               :waiting-for (when (:campaign? o)
                              (mapv str (clojure.set/difference (:members o) (:done o))))
             ;; one entry per FORK, not per scope — a campaign can hold two
             ;; branches of one scope, one per agent
               :forks (mapv (fn [[[scope agent] b]]
                              {:scope (str scope) :agent (str agent) :branch (name b)
                               :type (get-in o [:types [scope agent]] :kb)})
                            (:forks o))})))

        'release!
        ;; The deliberate way out of the closed state: I know the proposal was
        ;; filed, and I intend to write to trunk anyway. Explicit, so an
        ;; ungoverned write after a filing is always a choice someone made.
        (fn []
          (if-not (closed?)
            {:error "nothing to release — you have no filed proposal blocking writes"}
            (let [t (:title @(priv))]
              (reset! (priv) nil)
              (log/log! {:level :info :id ::proposal-released
                         :data {:room (str room-uuid) :agent (str agent-uuid) :title t}})
              {:released t :note "your writes go directly to trunk again"})))

        'refresh!
        ;; Update-from-trunk, for the agent's own CODE fork.
        ;;
        ;; The verb a reviewer would otherwise have to ask for by hand, and the
        ;; only thing that makes a conflict resolvable. It matters more here than
        ;; on a human team: several agents working one small repository collide
        ;; far more often, and geschichte's conflict granularity is a whole FILE.
        ;;
        ;; Deliberately the agent's job rather than the merge's. A conflict
        ;; surfaced HERE lands mid-turn, in front of the party that still has the
        ;; file text and the task it was doing; surfaced at Accept it lands in
        ;; front of a reviewer holding neither. So a conflict is RETURNED, not
        ;; thrown — it is a result to work with, not a failure.
        (fn []
          (let [o @(active)]
            (cond
              (nil? o) {:error "no proposal active — (proposal/start! \"title\") opens one"}
              (empty? (:ctx-forks o))
              {:error "no code fork to refresh — write a file first"}
              :else
              (let [results
                    (for [[[scope agent] fork] (:ctx-forks o)
                          :when (= agent agent-uuid)
                          :let [branch (get-in o [:forks [scope agent]])]]
                      (let [ff (branching/refresh-repo-fork! fork)]
                        (if (:ok? ff)
                          (assoc ff :scope (str scope) :how :fast-forward)
                          ;; Not a fast-forward — you and trunk both moved. That
                          ;; used to be the end of it. Now the two are merged
                          ;; line by line, so the only clashes left are ones
                          ;; where you edited the SAME lines trunk did.
                          (let [r (branching/merge-trunk-into-fork! scope branch)]
                            (if (seq (:unresolved r))
                              {:ok? false :scope (str scope) :how :conflict
                               :paths (mapv :path (:unresolved r))}
                              {:ok? true :scope (str scope) :how :merged
                               :files (:resolved r)})))))
                    results (vec results)
                    stuck (remove :ok? results)]
                (if (seq stuck)
                  {:conflicts (vec (mapcat :paths stuck))
                   :note (str "you and trunk changed the SAME lines in these files."
                              " Open them in your workspace, decide what the combined"
                              " version should say, write it, commit, and"
                              " (proposal/refresh!) again. Everything else has already"
                              " been merged for you.")}
                  {:refreshed (count results)
                   :merged-files (reduce + 0 (keep :files results))
                   :note "your fork now sits on top of trunk's current state"})))))

        'abandon!
        (fn []
          (let [ov (active) o @ov]
            (cond
              ;; Nothing to abandon. Returning nil told the agent nothing at all.
              (nil? o)
              {:abandoned false :note "you have no open proposal"}

              ;; ALREADY FILED. The old code fell through to the private branch,
              ;; discarded nil forks, reset the cell and answered
              ;; `{:abandoned <title>}` — telling the agent it had withdrawn work
              ;; that is already on the review surface, and quietly acting as an
              ;; undocumented third exit from the closed state alongside
              ;; `release!` and `open-campaign!`.
              (:closed o)
              {:abandoned false
               :filed (:proposal o)
               :note (str "\"" (:title o) "\" was already filed as " (:proposal o)
                          " — it cannot be abandoned. Ask for changes on the "
                          "proposal, or (proposal/release!) to stop writing "
                          "under it.")}

              :else
              (if (:campaign? o)
                ;; Per-agent branches are what make a per-agent withdrawal
                ;; possible at all: discard only what THIS agent forked and drop
                ;; its membership. Tearing down a shared campaign wholesale would
                ;; destroy work its other members have not filed yet.
                (let [ks (keys (mine o))]
                  (discard-overlay-forks! (mine o) (:types o))
                  ;; and this agent's code fork, which is a live workspace rather
                  ;; than a branch — nothing else reclaims it
                  (doseq [[k fork] (:ctx-forks o)
                          :when (= agent-uuid (second k))]
                    (branching/close-repo-fork! fork))
                  (swap! ov (fn [c]
                              (-> c
                                  (update :forks #(reduce dissoc % ks))
                                  (update :bases #(reduce dissoc % ks))
                                  (update :types #(reduce dissoc % ks))
                                  (update :ctx-forks
                                          #(reduce dissoc % (filter (fn [k] (= agent-uuid (second k)))
                                                                    (keys %))))
                                  (update :members disj agent-uuid))))
                  ;; The last member out closes the campaign. Leaving the cell
                  ;; non-nil with an empty `:members` made `open-campaign!`
                  ;; refuse forever — "a campaign is already open in this room",
                  ;; naming a campaign nobody was in and nothing could file.
                  (if (empty? (:members @ov))
                    (do (discard-code-forks! ov)
                        (reset! ov nil)
                        {:left-campaign (:title o) :discarded (count ks)
                         :note "you were the last member — the campaign is closed"})
                    {:left-campaign (:title o) :discarded (count ks)}))
                (do (discard-overlay-forks! (:forks o) (:types o))
                    (discard-code-forks! ov)
                    (reset! ov nil)
                    {:abandoned (:title o)})))))

        'file!
        (fn [& [rationale]]
          (let [;; the filing body, shared by both modes. Stamps a CLOSED token on
                ;; every contributor's cell so a member still working discovers the
                ;; filing on its next write instead of writing live (see
                ;; `overlay-branch!`).
                perform!
                (fn [o]
                  ;; FIRST, before anything reads a branch: a code fork's commits
                  ;; live only in its private workspace until now. Publishing is
                  ;; what turns them into the branch the diff below reads and the
                  ;; reviewer later merges — run it after the diff and every code
                  ;; fork would render as empty.
                  (publish-code-forks! (if (:campaign? o) (camp) (priv)))
                  (let [o (if (:campaign? o) @(camp) @(priv))
                        forks (overlay-forks o)
                        diffs (ctx/with-server-context
                                (mapv (fn [{:keys [scope branch system-type base-commit]}]
                                        ((requiring-resolve 'is.simm.ops.semantic-diff/semantic-diff)
                                         scope branch system-type :base-commit base-commit))
                                      forks))
                        ;; `:entries` rides along with `:pages` so a book fork's
                        ;; postings reach the summarizer as postings rather than as
                        ;; a datom count it can say nothing about
                        diff-text (pr-str (mapv #(select-keys % [:pages :files :entries :counts]) diffs))
                        rationales (or (not-empty (vals (:rationales o))) (when rationale [rationale]))
                        summary (let [r ((requiring-resolve 'dvergr.tools.llm-call/cheap-llm-call)
                                         (str "Summarize this proposed change in 2-3 plain sentences "
                                              "for a review card. Title: " (:title o)
                                              (when (:campaign? o)
                                                (str "\nJoint proposal by " (count (:members o))
                                                     " agents."))
                                              (when (seq rationales)
                                                (str "\nContributor rationales:\n"
                                                     (clojure.string/join "\n" rationales)))
                                              "\nChange data (EDN):\n" diff-text)
                                         "" {:max-tokens 700})]
                                  (or (not-empty (:text r)) (str "(no summary) " (:title o))))
                        pid ((requiring-resolve 'is.simm.ops.proposals/file-proposal!)
                             {:title (:title o)
                              :summary summary
                              ;; the FILER authors the row; a campaign's real
                              ;; authorship is the fork set, which per-fork accept
                              ;; already addresses one contribution at a time
                              :author agent-uuid
                              :room room-uuid
                              :forks forks})
                        token {:closed true :title (:title o) :proposal (str pid)}]
                    ;; every contributor — not just the filer — learns on its next
                    ;; governed write that this proposal is closed
                    (doseq [m (or (not-empty (:members o)) #{agent-uuid})]
                      (reset! (private-overlay room-uuid m) token))
                    (log/log! {:level :info :id ::proposal-filed
                               :data {:proposal (str pid) :room (str room-uuid)
                                      :campaign? (boolean (:campaign? o))
                                      :forks (count forks)}})
                    (cond->
                     {:proposal (str pid) :summary summary
                      :forks (count forks)
                      :campaign? (boolean (:campaign? o))
                      :contributors (count (distinct (map second (keys (:forks o)))))}
                      ;; Run the checks NOW and tell the agent, rather than
                      ;; leaving the reviewer to discover a red suite. This is
                      ;; the shift-left half of CI: at this instant the agent is
                      ;; still live, still holds the context, and can open a
                      ;; follow-up; an hour later a human reads a failure with
                      ;; none of that. Reported, never blocking — filing a
                      ;; change whose tests fail is sometimes exactly right, and
                      ;; the reviewer sees the same result on the card.
                      true
                      (assoc :checks
                             (vec (keep (fn [{:keys [scope branch system-type]}]
                                          (when (= :repo system-type)
                                            (some-> ((requiring-resolve
                                                      'is.simm.ops.checks/fork-checks)
                                                     scope :repo branch)
                                                    (select-keys [:status :passed :failed :errors])
                                                    (assoc :branch (name branch)))))
                                        forks))))))
                outstanding #(vec (clojure.set/difference (:members %) (:done %)))]
            (cond
              (closed?)
              {:error (str "\"" (:title @(priv)) "\" was already filed as "
                           (:proposal @(priv))
                           " — (proposal/release!) to write to trunk, or start a follow-up")}

              ;; CAMPAIGN — `file!` means "my part is done". The LAST member to
              ;; say so files for everyone. Filing on the first call is what let
              ;; one agent close the campaign out from under three others still
              ;; working, whose next writes then went live.
              (member?)
              (let [c (camp)
                    [old new]
                    (swap-vals! c (fn [m]
                                    (let [m (-> m
                                                (update :done (fnil conj #{}) agent-uuid)
                                                (assoc-in [:rationales agent-uuid] rationale))]
                                      (cond-> m
                                        (and (seq (:forks m))
                                             (empty? (clojure.set/difference (:members m) (:done m))))
                                        (assoc :filing? true)))))]
                (cond
                  ;; exactly one caller sees the false→true transition, so two
                  ;; simultaneous file!s cannot produce two proposals
                  (and (:filing? new) (not (:filing? old)))
                  ;; `perform!` publishes code forks, computes a semantic diff
                  ;; per fork, makes an LLM call, writes the proposal row and
                  ;; runs the fork's test suite. Any of those can throw.
                  ;;
                  ;; Unguarded, a throw left `:filing? true` set FOREVER: every
                  ;; later `file!` answered "another member is filing this
                  ;; campaign now", `open-campaign!` refused because the cell
                  ;; was truthy, and `abandon!` does not clear the flag. No verb
                  ;; resets a campaign cell, so the room's campaign was dead
                  ;; until the JVM restarted.
                  ;;
                  ;; Clear the FLAG, not the cell: the members' forks are in
                  ;; there and they are real branches. Dropping them would
                  ;; strand work that a retry can still file.
                  (try
                    (let [res (perform! new)]
                      (reset! c nil)
                      res)
                    (catch Throwable e
                      (swap! c dissoc :filing?)
                      (log/log! {:level :error :id ::campaign-filing-failed
                                 :error e
                                 :data {:room (str room-uuid) :title (:title new)}}
                                "Campaign filing threw — flag cleared so the room can retry")
                      {:filed false
                       :error (ex-message e)
                       :note (str "filing failed and was rolled back to un-filed. Your forks "
                                  "are still here — call (proposal/file! \"…\") again. If it "
                                  "keeps failing, (proposal/abandon!) discards them.")}))

                  (:filing? new)
                  {:filed false :note "another member is filing this campaign now"}

                  (empty? (:forks new))
                  {:filed false :campaign (:title new)
                   :waiting-for (mapv str (outstanding new))
                   :note "no writes have happened in this campaign yet"}

                  :else
                  {:filed false :campaign (:title new)
                   :waiting-for (mapv str (outstanding new))
                   :note (str "your part is submitted — the campaign files itself once the"
                              " others submit theirs. Tell them in the room.")}))

              :else
              ;; PRIVATE proposal — unchanged semantics, plus the closed token
              (let [ov (priv) o @ov]
                (cond
                  (nil? o)
                  {:error "no active proposal — proposal/start! or proposal/open-campaign! first"}

                  (empty? (:forks o))
                  (do (reset! ov nil) {:error "no writes happened — nothing to propose"})

                  :else (perform! o))))))}
       vocab/proposal-docs))))

(defn- add-book-ns!
  "Re-install `kontor/*` over the room's book with its conn routed through the
   agent's active proposal overlay — the same indirection `kb/*` uses.

   dvergr's generic ns-injector (`accounting-sandbox/add-kontor-ns!`) already
   bound this namespace when the ctx was created, but it is handed only a
   `:room-id`: it knows nothing about which agent is running or whether a
   proposal is open, so every `kontor/entry!` went straight to the room's LIVE
   book. Postings are the one artifact an agent produced that a human could not
   review before it landed. Re-adding the namespace here (after `ensure-ctx!`,
   like every other simmis vocabulary) replaces the injector's binding with one
   that forks.

   `*book*` deliberately stays the TRUNK conn: it is a value, not a call, so it
   cannot re-resolve per use, and a stale branch conn left in it after `file!`
   would be worse than a truthful trunk handle. Every verb and reader — the
   writes AND `balances`, so an agent can read its own proposed postings back —
   goes through the overlay-resolving conn-fn."
  [sci-ctx room-uuid overlay-fn agent-uuid]
  (when-let [scope (room-dbs/get-room-db-scope room-uuid)]
    (when-let [trunk (room-dbs/connect-room-database scope)]
      (sci/add-namespace! sci-ctx 'kontor
        (acct-sandbox/kontor-bindings
         trunk
         ;; WRITE: mints the fork on first posting, and throws if the proposal
         ;; this agent was contributing to has already been filed
         (fn []
           (or (when-let [b (overlay-branch! (overlay-fn) scope agent-uuid :book)]
                 (ctx/with-server-context (branching/get-kb-conn-on-branch scope b)))
               trunk))
         ;; READ: an existing fork if there is one, else trunk. Never mints —
         ;; `kontor/balances` used to fork the book just by being called.
         (fn []
           (or (when-let [b (existing-fork (overlay-fn) scope agent-uuid)]
                 (ctx/with-server-context (branching/get-kb-conn-on-branch scope b)))
               trunk)))))))

;; =============================================================================
;; sheet/* — spreadsheets as a live MODEL (doc/document-intake-design.md §2.5)
;; =============================================================================

;; The shell's drive FS (muschel DriveFS) TRUNCATES reads at 8 MB — a sane cap
;; for `cat`, and silent corruption for a zip container. So /drive paths resolve
;; straight to the CAS through the drive model (same addressing, no cap), and
;; only non-drive paths (the room worktree) go through muschel — where we refuse
;; anything past the cap rather than hand POI a half file.
(def ^:private shell-read-cap-bytes (* 8 1024 1024))

(defn- drive-relative
  "\"/drive/finance/model.xlsx\" → \"finance/model.xlsx\"; nil for any other path."
  [path]
  (let [p (str path)]
    (cond
      (= p "/drive") ""
      (str/starts-with? p "/drive/") (subs p (count "/drive/"))
      :else nil)))

(defn- room-doc-fs
  "The filesystem the document vocabularies (`sheet/*`, `office/*`) run on,
   bound to this room: /drive through the drive model (full-fidelity bytes —
   critical for a zip container the shell FS would truncate), everything else
   through the agent's muschel FS, and writes as NEW content-addressed blobs in
   the drive. Format-agnostic: both sheet and office share this contract."
  [cctx room room-uuid]
  (let [muschel-fs (fn []
                     (let [host ((requiring-resolve 'dvergr.intake.bash/get-or-create-host!) cctx)]
                       (:fs host)))
        drive-node (fn [rel]
                     (let [conn (drives/room-conn room-uuid)]
                       (when conn
                         (when-let [n (drives/resolve-path conn rel)]
                           [conn n]))))]
    {:read-bytes
     (fn [path]
       (binding [rtc/*execution-context* (:ctx room)]
         (if-let [rel (drive-relative path)]
           (when-let [[conn n] (drive-node rel)]
             (drives/read-file conn (:fs.node/id n)))
           (let [fs (muschel-fs)
                 size (:size ((requiring-resolve 'muschel.fs/stat) fs path))]
             (when (and size (> (long size) shell-read-cap-bytes))
               (throw (ex-info (str path " is " size " bytes; the shell filesystem only"
                                    " serves the first " shell-read-cap-bytes
                                    " — put it on the drive (/drive/…) to open it whole")
                               {:error :too-large-for-shell-fs :size size})))
             ((requiring-resolve 'muschel.fs/read-bytes) fs path)))))

     :stat
     (fn [path]
       (binding [rtc/*execution-context* (:ctx room)]
         (if-let [rel (drive-relative path)]
           (when-let [[_ n] (drive-node rel)]
             {:size (or (:fs.node/size n) 0) :type (:fs.node/kind n)})
           ((requiring-resolve 'muschel.fs/stat) (muschel-fs) path))))

     :write-file!
     (fn [path ^bytes bs mime]
       (let [rel (drive-relative path)]
         (when-not rel
           (throw (ex-info (str "agents can only write spreadsheets to the drive:"
                                " use a /drive/… path, got " path)
                           {:error :bad-path :path path})))
         (let [segs (vec (remove str/blank? (str/split rel #"/")))]
           (when (empty? segs)
             (throw (ex-info "destination needs a file name" {:error :bad-path :path path})))
           (binding [rtc/*execution-context* (:ctx room)]
             (drives/store-in-room! room-uuid (butlast segs) (last segs) bs
                                    :mime mime :source :agent)))))}))

(defn- add-sheet-ns!
  "Install `sheet/*` — an .xlsx on the drive becomes a live model the agent
   queries (get / range / deps / explain) and perturbs (set! → recalc →
   only-the-changed-cells). The bytes never enter the sandbox."
  [sci-ctx cctx room room-uuid]
  (sci/add-namespace! sci-ctx 'sheet
                      (sheet/sci-namespace (room-doc-fs cctx room room-uuid) room-uuid)))

(defn- add-office-ns!
  "Install `office/*` — a .docx/.pptx/.odt on the drive opens HOST-side and the
   agent edits its XML parts as DATA (walk/zip), re-serialising only the parts
   it touched. Also exposes `clojure.zip` for positional edits (`clojure.walk`
   is already in the sandbox). The container bytes never enter the sandbox."
  [sci-ctx cctx room room-uuid]
  (sci/add-namespace! sci-ctx 'office
                      (office/sci-namespace (room-doc-fs cctx room room-uuid) room-uuid))
  (sci/add-namespace! sci-ctx 'clojure.zip
                      (sci/copy-ns clojure.zip (sci/create-ns 'clojure.zip nil))))

(defn- add-screen-ns!
  "Install `screen/*` — the room's live screen shares as DATA (sharers / frames /
   search / text), grant-resolved and attributed. Not a tool: agents program
   over the screen the way they do over sheets and KBs."
  [sci-ctx room-uuid]
  (sci/add-namespace! sci-ctx 'screen (screen-intake/sci-namespace room-uuid)))

(defn- add-web-ns!
  "Install `web/*` — the OWNER's captured web pages as DATA — but ONLY in their
   personal room. Browsing is personal: a shared room's agents must not see any
   member's browsing archive. Bound to the room's owner party."
  [sci-ctx room-uuid]
  (let [r (rooms/get-room room-uuid)]
    (when (= :personal-ai (:room/type r))
      (when-let [owner (:room/created-by r)]
        (sci/add-namespace! sci-ctx 'web (web-intake/sci-namespace owner))))))

(defn- build-room-context-block
  "System-prompt addendum: which product KBs are reachable, their page
   titles (snapshot), and the [[Page]] wiki-link convention."
  [kb-conns]
  (let [kb-blocks
        (->> kb-conns
             (map (fn [{:keys [name conn]}]
                    (let [db @conn
                          titles (->> (d-api/q '[:find ?title
                                                 :where [?e :S.Page/title ?title]]
                                               db)
                                      (map first)
                                      (filter string?)
                                      sort)
                          ;; The DOMAIN entities this KB holds, as "S.Customer ×46".
                          ;;
                          ;; Listing page titles alone is what made a full database
                          ;; look empty: an agent told to answer with queries over
                          ;; customer records saw "Customers:" followed by four prose
                          ;; page names, concluded the records were elsewhere, and
                          ;; spent 20 minutes searching the room DB and the shell
                          ;; while 46 :S.Customer entities sat in the KB it had just
                          ;; been shown. Naming the types here is what connects the
                          ;; job ("query customer records") to the database that
                          ;; holds them; `kb/attributes` then gives the full shape.
                          types (->> (d-api/q '[:find ?a (count ?e) :where [?e ?a _]] db)
                                     (keep (fn [[a n]]
                                             (let [ns* (namespace a)]
                                               (when (and ns* (str/starts-with? ns* "S.")
                                                          (not= ns* "S.Page")
                                                          (not= ns* "S.Block"))
                                                 [ns* n]))))
                                     ;; one row per TYPE, sized by its widest attribute
                                     (reduce (fn [m [t n]] (update m t (fnil max 0) n)) {})
                                     (sort-by key))]
                      (str "- " (or name "Knowledge Base") ":\n"
                           "  pages: "
                           (if (seq titles) (str/join ", " titles) "(none)")
                           (when (seq types)
                             (str "\n  data:  "
                                  (->> types
                                       (map (fn [[t n]] (str t " ×" n)))
                                       (str/join ", "))
                                  "  — query with (kb/db \"" name "\")"))))))
             (str/join "\n"))]
    (str "\n\n## What the user calls this\n\n"
         ;; The UI says Team; the code, the schema and this vocabulary say room
         ;; (dvergr owns the concept). Stating the synonym costs one line and
         ;; prevents an agent from answering \"I don't have access to a team\".
         "The interface calls this room a **Team** (an assistant room is called "
         "an **Assistant**). Room and team are the SAME thing — your tools, this "
         "context and every `room` in the API refer to it.\n"
         "\n\n## Room knowledge bases\n\n"
         "Pages reachable from this room:\n"
         kb-blocks
         "\n\n## Wiki-link convention\n\n"
         "When the user writes `[[Page Title]]` in a chat message, that's a wiki link to "
         "a page in one of the KBs above. Treat it as a reference: if the user is asking "
         "about a page, fetch its content before responding.\n\n"
         "The KB-access functions live in SCI namespaces — call them via the `clojure_eval` "
         "tool:\n\n"
         "- `(wiki/read-page \"Page Title\")` — fetch a page's blocks\n"
         "- `(wiki/pages)` — list all pages, grouped by KB\n"
         "- `(wiki/search \"query\")` — RANKED fulltext over titles + content\n"
         "- `(wiki/neighborhood \"Title\")` — a page's outgoing links + backlinks\n"
         "- `(wiki/backlinks \"Title\")` — pages that link here\n"
         "The page list above is ONLY the pages. A KB also holds typed entities "
         "(customers, invoices, contacts …) that are NOT pages and never appear in "
         "`wiki/pages` — an empty-looking KB may be full of them. To see and query "
         "the actual data:\n\n"
         "- `(kb/attributes \"KB Name\")` — every attribute carrying data + how many "
         "entities carry it. **Run this before concluding a database is empty.**\n"
         "- `(kb/db \"KB Name\")` — the database VALUE, for the ordinary datahike API "
         "(`datahike.api` is available as `d`):\n"
         "  `(d/q '[:find ?e ?a :where [?e :S.Customer/account-id ?a]] (kb/db \"KB Name\"))`\n"
         "  `d/pull`, `d/datoms`, `d/entity` all work on it too.\n"
         "- `(kb/conn \"KB Name\")` — the connection, for `d/transact`. Inside a "
         "proposal it resolves to YOUR fork, so a direct transact is governed just "
         "like the `kb/*` verbs.\n\n"
         "Every `kb/*` WRITE takes the target database as its FIRST argument — a KB "
         "name from the list above (e.g. \"" (or (some-> kb-conns first :name) "My KB") "\"). "
         "You may only write to KBs attached to this room.\n"
         "- `(kb/ensure-page! \"KB Name\" \"New Title\")` — create a page, returns its uuid\n"
         "- `(kb/upsert-block! \"KB Name\" page-uuid content)` — add a block to a page\n"
         "- `(kb/archive-page! \"KB Name\" \"Title\")` — archive an emptied/obsolete page\n"
         "- `(kb/add-type! \"KB Name\" page-uuid \"S/Person\")` — tag a page with a type\n"
         "- `(kb/set-property! \"KB Name\" page-uuid :S.Person/company \"Acme\")` — set a typed "
         "property (only declared props; e.g. S/Person has :S.Person/company :email :phone "
         ":linkedin :title :source :status :notes). Build CRM/contact pages from intake this way.\n\n"
         "## Posting diagrams & charts\n\n"
         "You can post visuals straight into chat (they render inline) — reach for them to give "
         "the user an OVERVIEW at a glance instead of walls of text (a chat/document/domain map, "
         "a process, a schema):\n"
         "- a fenced ```mermaid code block renders as a diagram (flowchart, sequence, state, ER, "
         "mindmap). Keep it at overview altitude — a handful of meaningful nodes, never one per "
         "detail. The same ```mermaid block also works inside a wiki page.\n"
         "- a fenced ```vega-lite code block renders as a chart.\n"
         "When you draw a process YOU maintain (e.g. an intake workflow you scheduled), keep its "
         "diagram up to date whenever the steps change — a stale diagram is worse than none.\n"
         "For a background workflow you scheduled, attach its overview diagram so it shows in the "
         "Schedules view: `(workflow/schedules)` lists this room's workflows (id + description), "
         "then `(workflow/set-shape! \"<schedule-id>\" \"flowchart LR\\n  ...\")` stores/updates a "
         "few-node mermaid of its real steps (sources → dedup → write → which KB). Re-call it when "
         "you change the workflow.\n\n"
         "## Links\n\n"
         "Inside a wiki, `[[Title]]` links to a page in the SAME KB — keep using it there.\n"
         "To link ACROSS databases — e.g. mention a wiki page in a CHAT message, or link "
         "between two KBs — do NOT write a bare `[[Title]]` (it can't tell which database). "
         "Build a real cross-db link and paste the returned string:\n"
         "- `(kb/link \"KB Name\" \"Page Title\" [\"optional display text\"])` → returns "
         "`[[dh://…][display]]`, a validated link to that page. Create the page first if needed.\n"
         "- `(kb/link-to \"KB Name\" page-uuid [\"display\"])` — same, by uuid.\n"
         "So to tell the room about a contact you filed: "
         "`(kb/link \"Marketing Intake\" \"Bond (bondapp.io)\")` then include that in your reply.\n\n"
         "## Wiki protocol — read before write\n\n"
         "BEFORE adding or changing knowledge: `(wiki/search …)` for the topic, "
         "then `(wiki/neighborhood \"Title\")` + `(wiki/read-page …)` on the "
         "closest pages, THEN write coherently: extend the right existing page "
         "or create a genuinely new one — never a near-duplicate. Cross-reference "
         "entities/topics/pages as `[[Title]]` in every block you write; links "
         "are stored as refs and power backlinks + navigation.\n\n"
         "To CONSOLIDATE duplicate/overlapping pages: extend the best page with "
         "anything unique from the others, `[[link]]` it from them, then "
         "`(kb/archive-page! …)` the emptied ones. Consolidation is a wiki-only "
         "operation — it never requires touching workspace code.\n\n"
         "For BIG RESTRUCTURINGS (page splits/merges, mass edits): wrap the work "
         "in a proposal — `(proposal/start! \"title\")`, do the kb/* edits (they "
         "land on a review branch, trunk untouched), `(proposal/file! \"why\")`. "
         "A human reviews and accepts/dismisses. `(proposal/abandon!)` to bail. "
         "Accounting writes fork too: `kontor/entry!` inside a proposal posts to "
         "a branch of the room's book, so the entries are PROPOSED and land only "
         "on accept.\n\n"
         "When the ROOM is working on one joint change, do not each file your own "
         "proposal — open a campaign: `(proposal/open-campaign! \"title\")`, the "
         "others `(proposal/join-campaign!)`, everyone does their part, and one "
         "of you calls `(proposal/file! \"why\")`. That produces ONE proposal "
         "carrying a fork per contributor, which the human can accept or refuse "
         "one contribution at a time.")))

;; =============================================================================
;; Eval-entry persistence (agent inspector feed)
;; =============================================================================

(def ^:private max-result-chars 2000)

(defn- display-input
  "Tool input as a reader would want to see it. dvergr namespaces the keys of
   structured tool input (:tool-input.shell/command) — an internal detail that
   only clutters the chip."
  [input]
  (if (map? input)
    (update-keys input (fn [k] (if (keyword? k) (keyword (name k)) k)))
    input))

(defn- persist-eval-entry!
  "Project one tool call into the room timeline as an S.EvalEntry chip.
   `tool` names the tool (clojure_eval, shell, …) so the UI can badge it;
   `code` is the evaluated code, or the tool's input for other tools."
  [room-conn room-uuid agent-uuid tool code result]
  (let [now (java.util.Date.)
        success? (= (:type result) :success)
        result-str (let [s (or (:content result) (str result))]
                     (if (> (count s) max-result-chars)
                       (str (subs s 0 max-result-chars) "\n… (truncated)")
                       s))]
    (try
      (d-api/transact room-conn
        [{:entity/uuid (random-uuid)
          :entity/created-at now
          :S.EvalEntry/room [:entity/uuid room-uuid]
          :S.EvalEntry/agent [:entity/uuid agent-uuid]
          :S.EvalEntry/tool (or tool "clojure_eval")
          :S.EvalEntry/code code
          :S.EvalEntry/result result-str
          :S.EvalEntry/success? success?
          :S.EvalEntry/evaluated-at now}])
      (catch Exception e
        (log/log! {:level :warn :id ::eval-entry-persist-failed
                   :msg "Failed to persist eval entry"
                   :data {:error (.getMessage e)}})))))

;; =============================================================================
;; Room timeline projector — the ONE content-DB writer per room
;; =============================================================================

(defonce ^:private projected-rooms (atom #{}))

(defn ensure-room-projector!
  "Arm the single content-DB writer for a room: one bus listener that
   projects conversational messages into the room content DB (the
   interim client render path — dies with Stage 5 store replication).

   Replaces the three historical writers (web-dispatch direct persist,
   the human persistence participant, the telegram content mirror) —
   multiple writers needed per-path dedup/filter hacks and still
   drifted. Projection rules:
   - tool-call events (:tool-uses present) are SKIPPED — the timeline
     represents tool activity as S.EvalEntry chips written with results
     by the wrapped tools;
   - everything else persists idempotently under the message id
     (multi-recipient sends share one id, so the upsert dedupes);
   - author entities are ensured so the timeline's author join holds.

   Idempotent per room per JVM."
  [room room-uuid room-conn]
  (when-not (contains? @projected-rooms room-uuid)
    (swap! projected-rooms conj room-uuid)
    (d/on-each-message room
      (fn [msg]
        (try
          (let [content (:content msg)
                ;; play-by-play activity rows (turn/post-turn-activity!)
                ;; carry :tool-uses and :reasoning inside :metadata —
                ;; reading only top-level leaked their "🔧 …" summaries
                ;; into chat as plain messages and DROPPED the reasoning
                ;; traces from real replies.
                tool-uses (or (seq (:tool-uses msg))
                              (seq (get-in msg [:metadata :tool-uses])))
                reasoning (or (:reasoning msg)
                              (get-in msg [:metadata :reasoning]))]
            (when (seq tool-uses)
              ;; Uniform tool visibility: project NON-eval tool calls as
              ;; the same collapsed S.EvalEntry chips the wrapped
              ;; clojure_eval writes (which carries its own chips WITH
              ;; results — skip it here to avoid doubles).
              (when-let [author-uuid (let [from (:from msg)]
                                       (cond (uuid? from) from
                                             (keyword? from) (actor-kw->party-uuid from)))]
                (doseq [tu tool-uses
                        :let [tname (or (:tool-use/name tu) (:name tu))
                              input (or (:tool-use/input tu) (:input tu))]
                        :when (and tname (not= tname "clojure_eval"))]
                  (persist-eval-entry! room-conn room-uuid author-uuid
                                       tname
                                       (if (seq input)
                                         (with-out-str (pp/pprint (display-input input)))
                                         "")
                                       {:type :success :content ""}))))
            (when (and (string? content) (seq content)
                       (empty? tool-uses))
              (when-let [author-uuid (let [from (:from msg)]
                                       (cond (uuid? from) from
                                             (keyword? from) (actor-kw->party-uuid from)))]
                (when-let [party (parties/get-party author-uuid)]
                  (ensure-room-party-entity! room-conn party))
                (let [msg-id (or (:id msg) (random-uuid))
                      ;; Resolve bare [[Title]] links against the room's KBs to
                      ;; explicit [[dh://…]] references so the client opens them
                      ;; directly (a chat message isn't in any KB, so a bare title
                      ;; can't say which database it means). Mentions/notify stay on
                      ;; the ORIGINAL text — resolution rewrites only what renders.
                      {resolved :content unresolved :unresolved}
                      (kbs/resolve-room-links room-uuid content)]
                  (persist-message! room-conn msg-id
                                    resolved room-uuid author-uuid
                                    reasoning)
                  (when (seq unresolved)
                    (log/log! {:level :info :id ::unresolved-links
                               :msg "Bare [[links]] left unresolved at persist"
                               :data {:room-uuid room-uuid :author author-uuid
                                      :unresolved unresolved}}))
                  ;; [[Title]] page-mentions + @handle party-mentions as
                  ;; VALUE-level datoms (cross-DB safe) — wiki-page backlinks and
                  ;; @-notifications become a plain query instead of a content scan.
                  (let [page-ments  (seq (kbs/extract-wikilinks content))
                        party-ments (seq (refs/extract-user-mentions content))]
                    (when (or page-ments party-ments)
                      (d-api/transact room-conn
                        [(cond-> {:entity/uuid msg-id}
                           page-ments  (assoc :S.Message/mentions (vec page-ments))
                           party-ments (assoc :S.Message/party-mentions (vec party-ments)))]))
                    ;; Fan out a notification event to the room's human members
                    ;; (badge every message; pop only for mentions).
                    (mnb/notify-message! {:room-uuid room-uuid
                                          :author-uuid author-uuid
                                          :content content}))
                  ;; Attachment ref (e.g. original voice-note audio) —
                  ;; served via /blobs/<id>, rendered as a player.
                  (when-let [{:keys [blob-id mime]} (get-in msg [:metadata :attachment])]
                    (d-api/transact room-conn
                      [{:entity/uuid msg-id
                        :S.Message/attachment-blob (str blob-id)
                        :S.Message/attachment-mime (or mime "application/octet-stream")}]))))))
          (catch Exception e
            (log/log! {:level :warn :id ::projector-persist-failed
                       :data {:room room-uuid :error (.getMessage e)}})))))
    (log/log! {:level :info :id ::room-projector-armed
               :data {:room room-uuid}})))

(defn- wrap-eval-tool
  [eval-tool room-conn room-uuid agent-uuid]
  (assoc eval-tool
    :execute (fn [params tctx]
               (let [result ((:execute eval-tool) params tctx)]
                 (persist-eval-entry! room-conn room-uuid agent-uuid
                                      "clojure_eval" (:code params) result)
                 result))))

(defn- wrap-knowledge-add-tool
  "Retarget knowledge_add at the room's ATTACHED product KBs (grants):
   published knowledge becomes a title-addressable wiki page — visible in
   the wiki, clickable from chat as [[Title]], backlink-indexed. The
   default target is the primary grant; an optional :kb param (KB name)
   selects among multiple attached KBs. Without any grant the tool falls
   through to dvergr's room-internal knowledge graph (standalone
   behavior). The room-internal store remains the agent's EPISODIC
   memory either way — knowledge_add means 'publish to shared
   knowledge'."
  [ka-tool kb-conns]
  (if (empty? kb-conns)
    ka-tool
    (-> ka-tool
        (update :description str
                "\n\nThis room publishes into the attached shared knowledge base(s): "
                (str/join ", " (map :name kb-conns))
                ". Entries become wiki pages — reference them as [[Title]] in your replies "
                "so people can click through. Optional :kb (name) targets a specific base."
                "\n\nPROTOCOL (read before write): run (wiki/search \"…\") and "
                "(wiki/neighborhood \"Title\") in clojure_eval FIRST to load what "
                "already exists; EXTEND the closest existing page rather than creating "
                "a near-duplicate; cross-reference related pages as [[Title]] inside "
                "your :summary/:context text — those links are stored and power "
                "backlinks/navigation. Titles are matched case-insensitively; a close "
                "match reuses the existing page.")
        (update-in [:parameters :properties] assoc
                   :kb {:type "string"
                        :description "Target knowledge base by name (default: primary)"})
        (assoc :execute
               (fn [{:keys [title summary context source url kb] :as _params} _tctx]
                 (let [target (or (when kb (first (filter #(= kb (:name %)) kb-conns)))
                                  (first kb-conns))
                       conn (:conn target)]
                   (if-not conn
                     {:error (str "knowledge base " (:name target) " not reachable")}
                     (let [;; fuzzy dedup: a case/whitespace-insensitive title
                           ;; match reuses the EXISTING page instead of forking
                           ;; a near-duplicate (drift guard).
                           norm (fn [t] (-> (str t) str/trim str/lower-case
                                            (str/replace #"\s+" " ")))
                           existing-titles (map first
                                             (d-api/q '[:find ?t :where [?e :S.Page/title ?t]]
                                                      @conn))
                           match (some (fn [t] (when (= (norm t) (norm title)) t))
                                       existing-titles)
                           title' (or match title)
                           page-uuid (kbs/kb-upsert-knowledge-page!
                                      conn title'
                                      :summary summary :context context
                                      :source source :url url)]
                       {:content (str "Recorded [[" title' "]] in " (:name target)
                                      " (page " page-uuid ")."
                                      (when match
                                        (str " (Matched existing page [[" match
                                             "]] — extended it instead of duplicating.)"))
                                      " Reference it as [[" title' "]] in your reply.")}))))))))

;; =============================================================================
;; Provider resolution + billing
;; =============================================================================

(defn- ensure-providers! []
  (when (empty? (providers/list-providers))
    (providers/init-defaults!)))

(defn- resolve-provider [model provider-hint]
  (or provider-hint
      (cond
        (str/starts-with? (or model "") "accounts/fireworks/") :fireworks
        (str/starts-with? (or model "") "gpt-") :openai
        (str/starts-with? (or model "") "claude-") :anthropic
        :else :fireworks)))

;; A `:run-turn-fn` wrapper used to mirror each turn's usage onto the owner's
;; billing ledger in the system DB. It never recorded anything: it read
;; `(:message/usage last-msg)`, and no such attribute exists anywhere in dvergr
;; — assistant messages do not carry usage, and `run-agent-turn!` returns a bare
;; status keyword, not a map. The `when usage` guard turned that into silence,
;; so the admin dashboard read an always-empty ledger and reported $0.00 while
;; real spend accumulated elsewhere.
;;
;; Spend is recorded by dvergr's own turn path: `chat.context/account-usage!` ->
;; `chat.accounting/record-usage!`, which writes the `:ledger/*` row AND the
;; `:chat/budget-used` rollup into the ROOM's store in one transact. That is the
;; authoritative record (dvergr rebuilds a room's used-budget by summing those
;; rows on rehydrate), so `billing/get-system-stats` now aggregates the room
;; stores instead of maintaining a second, parallel ledger here.

;; =============================================================================
;; Joining participants
;; =============================================================================

(defn- add-workflow-ns!
  "Install the `workflow/*` vocab: an agent declares/updates the OVERVIEW
   diagram (mermaid) for a background workflow (schedule) it maintains in THIS
   room. Stored in the system DB (schedule uuid → mermaid). The Schedules view
   renders it; the KB-named topology template is the fallback (design B). The
   agent should keep it current when the workflow's steps change."
  [sci-ctx room-uuid]
  (let [room-slug (:room/slug (rooms/get-room room-uuid))
        list-scheds #((requiring-resolve 'dvergr.scheduler.core/list-all-schedules))
        room-ids (fn [] (into #{} (comp (filter #(= room-slug (:room %))) (map (comp str :id)))
                              (list-scheds)))]
    (sci/add-namespace! sci-ctx 'workflow
      {'schedules
       (fn []
         ;; This room's workflows (id + description) — find the id to shape.
         (->> (list-scheds)
              (filter #(= room-slug (:room %)))
              (mapv (fn [s] {:id (str (:id s)) :description (:description s)}))))
       'set-shape!
       (fn [schedule-id-str mermaid]
         ;; Attach/replace an overview-altitude mermaid diagram on a workflow.
         ;; Guarded to this room's schedules. Keep it a handful of nodes.
         (let [id (java.util.UUID/fromString (str schedule-id-str))]
           (if (contains? (room-ids) (str id))
             (do (d-api/transact ((requiring-resolve 'is.simm.model.system-db/get-conn))
                   [{:workflow.shape/id id
                     :workflow.shape/mermaid (str mermaid)
                     :workflow.shape/updated (java.util.Date.)}])
                 {:ok true :schedule (str id)})
             (throw (ex-info "Not a workflow scheduled in this room"
                             {:schedule-id (str id)})))))})))

(defn- make-link-reply-hook
  "The dvergr `:on-reply` hook for a room (see run-agent-turn!). Rewrites bare
   `[[Title]]` links in an agent's reply to explicit `[[dh://…]]` references —
   resolved against the room's KBs, so the agent's own next-turn copy and the
   posted reply both carry working links — and returns a system note for any
   title it couldn't pin to a single wiki, teaching the agent to make the link
   explicit with `kb/link` (or to create the page first)."
  [room-uuid]
  (fn [content]
    (let [{:keys [content unresolved]} (kbs/resolve-room-links room-uuid content)]
      {:content content
       :notes
       (mapv (fn [{:keys [title reason candidates]}]
               (if (= reason :ambiguous)
                 (str "Heads up: your wiki link [[" title "]] is ambiguous — a page titled \""
                      title "\" exists in more than one wiki attached to this room ("
                      (str/join ", " candidates) "), so it was left as plain text. Make it "
                      "explicit with (kb/link \"<Wiki>\" \"" title "\") and paste the returned "
                      "[[dh://…]] link into your message.")
                 (str "Heads up: your wiki link [[" title "]] didn't resolve — no page titled \""
                      title "\" exists in this room's wikis, so it was left as plain text. Create "
                      "it first with (kb/ensure-page! \"<Wiki>\" \"" title "\") — then it links "
                      "automatically — or, for a page in another wiki, use (kb/link \"<Wiki>\" \""
                      title "\") and paste the returned [[dh://…]] link.")))
             unresolved)})))

(defn ensure-agent-joined!
  "Join `agent-party` into the live room as a dvergr llm-agent (once per
   [room party]). dvergr's turn factory builds the full sandbox; here we
   pre-create the working ctx so we can add the simmis wiki/kb vocabulary
   and the KB context block to it."
  [room room-uuid agent-party room-conn]
  (let [k [room-uuid (:party/id agent-party)]]
    (when-not (contains? @joined k)
      (let [agent-uuid (:party/id agent-party)
            actor-kw (party->actor-kw agent-party)
            kb-conns (get-room-kb-conns room-uuid)
            budget-dollars (rooms/get-room-budget-dollars room-uuid)
            owner-id (:party/id (:party/owner agent-party))
            fallback-model (when owner-id
                             (:party/preferred-model (parties/get-party owner-id)))
            ;; Resolved HERE, per participant creation — never frozen into the
            ;; agent's stored config. An agent records the family it belongs to
            ;; and whether its version is pinned or :auto; the concrete id is
            ;; computed against the provider's live catalog. This is why Vár ran
            ;; glm-5p1 for eleven days after we "switched" to 5p2: the id had
            ;; been baked in at creation, and no code change could reach it.
            model (or (model-selection/resolve-config
                        {:model-family (:party/model-family agent-party)
                         :model-version (:party/model-version agent-party)
                         :model (:party/model agent-party)})
                      fallback-model
                      parties/default-model)
            provider (resolve-provider model (:party/provider agent-party))
            _ (log/log! {:level :info :id ::model-resolved
                         :data {:agent (:party/display-name agent-party)
                                :family (:party/model-family agent-party)
                                :version (or (:party/model-version agent-party) :pinned-id)
                                :model model}})
            ;; Base prompt via dvergr's ONE assembler (discourse-preamble +
            ;; [SKIP] convention + skills + tool-use-guideline + the sandbox
            ;; self-knowledge pointer + the workspace AGENTS.md/intake-catalog
            ;; guide) — the same path personas/daemon use, so simmis stops
            ;; drifting its own hand-rolled prompt. ctx bound so it resolves
            ;; THIS room's worktree for skills + AGENTS.md.
            base-prompt (persona+tools agent-party)
            assembled-prompt (binding [rtc/*execution-context* (:ctx room)]
                               ((requiring-resolve 'dvergr.agent.prompt/assemble-system-prompt)
                                base-prompt
                                {:tools #{"clojure_eval" "knowledge_search" "knowledge_add"
                                          "read_file" "write_file" "edit_file" "clojure_edit"
                                          "clj_kondo" "shell" "grep" "glob" "run_tests"}}))
            system-prompt (str assembled-prompt
                               ;; Self-identity: schedules/hires address participants by
                               ;; keyword id; without this the agent guesses (:var) and
                               ;; scheduled tasks never reach it.
                               ;;
                               ;; It must be told how to CONSTRUCT the keyword, not just
                               ;; shown it. `:party/<uuid>` cannot be read as a literal —
                               ;; a keyword's name part may not begin with a digit, and
                               ;; clojure.core, clojure.edn and SCI all reject it with
                               ;; "Invalid token" / "Invalid keyword". The value only
                               ;; exists because `(keyword "party" (str uuid))` bypasses
                               ;; the reader. Printing the literal told the agent to type
                               ;; something untypeable; it read the reader error as a
                               ;; type mismatch and reported the scheduler as broken.
                               "\n\nYour participant id in this room is `" actor-kw "`."
                               " It cannot be written as a literal — a keyword name"
                               " cannot start with a digit — so build it with"
                               " `(keyword \"party\" \"" (name actor-kw) "\")`."
                               " Use that as :agent-id when scheduling work for yourself."
                               (when (seq kb-conns)
                                 (build-room-context-block kb-conns))
                               ;; The drive is a MOUNT, not a tool: the shell's
                               ;; filesystem carries it at /drive (muschel MountFS
                               ;; over the datahike tree + CAS blobs).
                               (when (seq (drives/get-room-drives room-uuid))
                                 (str "\n\nThis room has a shared drive mounted at"
                                      " /drive in your shell — use it for documents"
                                      " (ls /drive, cat, echo > /drive/notes.md,"
                                      " mkdir, mv). Files there are visible to the"
                                      " humans in the room and persist."
                                      sheet/prompt-block
                                      office/prompt-block))
                               screen-intake/prompt-block
                               ;; web/* is installed only in the owner's personal
                               ;; room — describe it only there.
                               (when (= :personal-ai (:room/type (rooms/get-room room-uuid)))
                                 web-intake/prompt-block))

            ;; Pre-create the working ctx (same key llm-agent resolves on its
            ;; turn) so the simmis vocabulary is installed before first use.
            cctx (room-ctx/ensure-ctx! room actor-kw
                                       {:system-prompt system-prompt
                                        :budget-dollars budget-dollars})
            _ (when-let [sci-ctx (:sci-ctx cctx)]
                ;; resolved per write, not captured: `join-campaign!` changes
                ;; which overlay is active in the middle of a turn
                (let [overlay-fn #(active-overlay room-uuid agent-uuid)]
                  (add-wiki-ns! sci-ctx kb-conns room-uuid)
                  (add-kb-write-ns! sci-ctx kb-conns overlay-fn agent-uuid)
                  (add-proposal-ns! sci-ctx room-uuid agent-uuid)
                  (add-book-ns! sci-ctx room-uuid overlay-fn agent-uuid)
                  (add-sheet-ns! sci-ctx cctx room room-uuid)
                  (add-office-ns! sci-ctx cctx room room-uuid)
                  (add-screen-ns! sci-ctx room-uuid)
                  (add-workflow-ns! sci-ctx room-uuid)
                  (add-web-ns! sci-ctx room-uuid)))

            ;; Full development toolbelt — file ops, structural editing
            ;; (clojure_edit), located lint errors (clj_kondo), shell. The
            ;; prior 3-tool grant forced agents to do EVERYTHING through
            ;; clojure_eval string ops (hand-built code lines, manual paren
            ;; counting — see Vár's 2026-07-04 consolidation turn). Tool cwd
            ;; resolves to the room worktree via the bound execution ctx.
            ;; PDF-scale text processing regularly overruns the default
            ;; 60s eval timeout — the timeout error feeds back and the
            ;; model retries variations, burning budget on work that
            ;; just needed runway.
            _ (reset! tools/eval-timeout-ms 180000)
            base-tools (select-keys @tools/registry
                                    ["clojure_eval" "knowledge_search" "knowledge_add"
                                     "read_file" "write_file" "edit_file" "clojure_edit"
                                     "clj_kondo" "shell" "grep" "glob" "run_tests"])
            agent-tools (cond-> base-tools
                          (get base-tools "clojure_eval")
                          (assoc "clojure_eval"
                                 (wrap-eval-tool (get base-tools "clojure_eval")
                                                 room-conn room-uuid agent-uuid))
                          (get base-tools "knowledge_add")
                          (assoc "knowledge_add"
                                 (wrap-knowledge-add-tool
                                  (get base-tools "knowledge_add") kb-conns)))
            ;; Code forks. Applied LAST and over the already-wrapped map, so
            ;; `clojure_eval` keeps its transcript persistence and additionally
            ;; writes into the fork. The overlay is resolved PER CALL for the
            ;; same reason the KB vocabulary resolves it per write: an agent can
            ;; join a campaign mid-turn, and which proposal governs its writes is
            ;; exactly what that changes.
            ;; run_tests gets the file-loading wrapper FIRST, so that the repo
            ;; wrapper below hands it the fork's filesystem — the order matters,
            ;; the inner wrapper reads `:filesystem` off the context the outer
            ;; one swaps.
            agent-tools (cond-> agent-tools
                          (get agent-tools "run_tests")
                          (update "run_tests" wrap-run-tests-tool))
            agent-tools (reduce (fn [m tname]
                                  (if-let [t (get m tname)]
                                    (assoc m tname
                                           (wrap-repo-tool
                                            t #(active-overlay room-uuid agent-uuid)
                                            room-uuid agent-uuid))
                                    m))
                                agent-tools repo-tools)]
                          ;; NOTE: live screen shares are NOT a tool — they are
                          ;; the `screen/*` SCI vocabulary (add-screen-ns!), read
                          ;; in clojure_eval like sheet/* and kb/*.
        (binding [rtc/*execution-context* (:ctx room)]
          (d/join room
                  (llm/llm-agent
                    {:id actor-kw
                     :ctx (:ctx room)
                     :spec {:provider provider :model model
                            :system-prompt system-prompt}
                     :tools agent-tools
                     ;; flexible execution under a BOUNDED DOLLAR budget —
                     ;; no turn caps (removed deliberately). Exhaustion
                     ;; pauses at the budget checkpoint (room warning +
                     ;; grace window; raising the room budget resumes).
                     :budget {:dollars budget-dollars}
                     ;; Resolve bare [[Title]] links in replies to [[dh://…]] and
                     ;; nudge the agent (system note) on anything unresolvable.
                     :on-reply (make-link-reply-hook room-uuid)})))
        (swap! joined conj k)
        (log/log! {:level :info :id ::participant-created
                   :msg "LLM participant joined (dvergr harness)"
                   :data {:room-uuid room-uuid
                          :agent-party-id agent-uuid
                          :agent-name (:party/display-name agent-party)}})))))

(defn prepare-scheduled-agents!
  "Join the auto-respond agents of every live room that has an active
   schedule, so scheduled CODE tasks fire in a COMPLETE agent ctx (kb/wiki
   injected), not only after the room's first user message. Called at boot
   after the scheduler starts; best-effort per room. Also incidentally
   fixes 'agents unjoined after restart' for scheduled rooms."
  []
  (binding [rtc/*execution-context* ctx/server-context]
    (doseq [room ((requiring-resolve 'dvergr.room.registry/list-rooms))]
      (try
        (let [store-conn (some-> room :store :conn)
              has-sched? (and store-conn
                              (binding [rtc/*execution-context* (:ctx room)]
                                (seq (d-api/q '[:find [?s ...]
                                                :where [?s :schedule/active? true]]
                                              @store-conn))))]
          (when (and has-sched? (:slug room))
            (when-let [row ((requiring-resolve 'dvergr.system.db/room-by-slug) (:slug room))]
              (let [room-uuid (:room/id row)
                    sys-conn @((requiring-resolve 'is.simm.model.system-db/get-conn))
                    scope (:room/content-db-scope
                           (d-api/pull sys-conn [:room/content-db-scope]
                                       [:room/id room-uuid]))
                    room-conn (when scope
                                ((requiring-resolve
                                  'is.simm.model.room-databases/connect-room-database) scope))]
                (when room-conn
                  (doseq [agent (filter :party/auto-respond? (rooms/get-room-agents room-uuid))]
                    (ensure-room-party-entity! room-conn agent)
                    (ensure-agent-joined! room room-uuid agent room-conn))
                  (log/log! {:level :info :id ::scheduled-agents-prepared
                             :data {:room (:slug room)}}))))))
        (catch Throwable t
          (log/log! {:level :warn :id ::prepare-scheduled-agents-failed
                     :data {:room (:slug room) :error (.getMessage t)}}))))))

;; =============================================================================
;; External inspection surface (chat_remote / sandbox_remote)
;; =============================================================================

(defn room-context-state
  "Any agent's working state for `room-uuid` — {:chat-ctx … :sci-ctx …} —
   resolved through dvergr's per-(room, agent) registry. Replaces the old
   local `contexts` cache walked by chat_remote/sandbox_remote."
  [room-uuid]
  (when-let [slug (:room/slug (rooms/get-room room-uuid))]
    (let [room-id (rstore/slug->room-id slug)]
      (some (fn [[_ aid] ]
              (when-let [cctx (room-ctx/lookup room-id (party->actor-kw aid))]
                {:chat-ctx cctx :sci-ctx (:sci-ctx cctx)}))
            (filter (fn [[rid _]] (= rid room-uuid)) @joined)))))

;; =============================================================================
;; Dispatch
;; =============================================================================

(defn dvergr-room-assignments
  "Return the room's durable, room-local actor assignments."
  [room-slug]
  (sdb/room-assignments room-slug))

(defn assign-room-agent!
  "Persist one room-local actor assignment."
  [room-slug actor-id assignment]
  (sdb/assign-room-actor! room-slug actor-id assignment))

(defn unassign-room-agent!
  "Retract one durable room-local actor assignment."
  [room-slug actor-id]
  (sdb/unassign-room-actor! room-slug actor-id))

(defn- ensure-agent-assignment!
  "Lazily materialize the assignment for a pre-assignment Simmis room member.
   Existing dvergr assignments always win. The old `auto-respond?` bit is used
   only once as a migration default."
  [room-slug agent]
  (let [actor-id (party->actor-kw agent)]
    (or (sdb/assignment-for room-slug actor-id)
        (assign-room-agent!
         room-slug actor-id
         {:role :specialist
          :response-policy (if (:party/auto-respond? agent) :always :manual)}))))

(defn post-user-message!
  "Post a user message into the room's live dvergr discourse Room. Works
   for ALL room kinds — the send path is one:

   1. Persist to the room content DB (interim client render path) — the
      dvergr room store ALSO persists it via the bus.
   2. Resolve @handles to room-local actor ids. Unknown/ambiguous mentions fail
      closed; assignment response policies select recipients.
   3. Post one Message per selected recipient. When a room has assigned agents
      but none should wake, target the reserved `:_room-log` endpoint so the
      durable/projector listeners see it without broadcasting to every joined
      participant. Rooms without agents retain their adapter-facing target.

   Returns {:status :ok :recipients [...]} immediately; replies are async."
  [room-uuid user-message sender-party-id room-conn & [msg-uuid in-reply-to]]
  (ensure-providers!)
  (let [room-parties (rooms/get-room-parties room-uuid)
        all-agents (filterv #(= :agent (:party/type %)) room-parties)
        room-info (rooms/get-room room-uuid)
        room-slug (:room/slug room-info)
        assignments (when room-slug
                      (mapv #(ensure-agent-assignment! room-slug %) all-agents))
        {:keys [mentions audience recipients]}
        (dispatch/plan-message-dispatch room-parties assignments user-message)]
    (binding [rtc/*execution-context* ctx/server-context]
      (let [sender-party (or (parties/get-party sender-party-id)
                             {:party/id (or sender-party-id seed/user-uuid-you)
                              :party/type :human
                              :party/display-name "You"})
            user-uuid (:party/id sender-party)
            room (live-room room-uuid)]
        (when-not room
          (throw (ex-info "No live discourse room (missing :room/slug — pre-Stage-1 room?)"
                          {:room-uuid room-uuid})))
        (ensure-room-party-entity! room-conn sender-party)
        ;; ONE content-DB writer per room: the projector persists every
        ;; conversational bus message (including this send and the
        ;; replies) idempotently by message id. The client renders the
        ;; send optimistically and reconciles on the sync echo.
        (ensure-room-projector! room room-uuid room-conn)
        (doseq [agent recipients]
          (ensure-room-party-entity! room-conn agent)
          (ensure-agent-joined! room room-uuid agent room-conn))
        (let [from-kw (party->actor-kw user-uuid)
              ;; All copies of a multi-recipient send share ONE id
              ;; (client-supplied when present) — the projector's upsert
              ;; dedupes them into a single timeline row.
              send-id (or msg-uuid (random-uuid))
              metadata {:role :user :mentions mentions :audience audience}
              msgs (binding [rtc/*execution-context* (:ctx room)]
                     (->> (if (seq recipients)
                            (mapv #(d/message from-kw (party->actor-kw %) user-message in-reply-to
                                              metadata)
                                  recipients)
                            [(d/message from-kw
                                        (if (seq all-agents)
                                          :_room-log
                                          (d/room-target room))
                                        user-message in-reply-to metadata)])
                          (mapv #(assoc % :id send-id))))]
          (binding [rtc/*execution-context* (:ctx room)]
            (doseq [m msgs]
              (d/post! room m))))
        {:status :ok
         :recipients (mapv :party/id recipients)}))))
