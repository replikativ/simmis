(ns is.simm.model.system-db
  "System Datahike database for identity, rooms, sessions, env vars,
   and budgets/accounting.

   Separate from the wiki/chat DB. Stores operational data that
   persists across server restarts.

   Identity is unified with dvergr's actor model: every party entity
   carries BOTH dvergr's actor core (:actor/id keyword identity,
   :actor/kind, :actor/name, :actor/created-at, :actor/system-prompt,
   :actor/config — installed via dvergr.chat.schema/ensure-full-schema!)
   AND simmis's extension attributes (:party/id uuid alias, :party/handle,
   auth fields, :party/owner, :party/contacts, ...). The :party/id uuid
   is kept as a :db.unique/identity alias ON THE SAME ENTITY so every
   existing uuid-keyed reference (sessions, rooms, room-DB authors,
   browser sync) keeps working. The bridge is
   `:actor/id = (keyword \"party\" (str party-uuid))`.

   Accounting lives on dvergr's :ledger/* + :budget/* schema (see
   is.simm.model.billing); the old :llm-log/* and :party-budget/* attrs
   are no longer declared (dead data in old stores is left in place)."
  (:require [datahike.api :as d]
            [dvergr.chat.schema :as chat-schema]
            [dvergr.system.db :as sdb]
            [taoensso.telemere :as log]))

;; =============================================================================
;; Schema (simmis-only extension attributes)
;; =============================================================================

(def schema
  "Simmis extension schema. The actor identity core (:actor/*), ledger
   (:ledger/*), and budget (:budget/*) come from dvergr — see `init!`."
  [;; --- Party extension (same entity as the dvergr actor row) ---
   {:db/ident :party/id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/doc "Party UUID — alias for :actor/id (keyword \"party\" uuid) on the same entity"}
   {:db/ident :party/handle
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/doc "Short unique handle (e.g. 'christian', 'vár')"}
   {:db/ident :party/avatar
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Avatar URL or emoji"}
   {:db/ident :party/contacts
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/doc "Other parties this party knows (directed)"}

   ;; --- Workflow shapes: agent-authored overview diagram for a room's
   ;;     background workflow (schedule uuid → mermaid). Design B — the agent
   ;;     declares what a workflow DOES; the Schedules view renders it, the
   ;;     KB-named topology template is the fallback. ---
   {:db/ident :workflow.shape/id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/doc "The schedule uuid this shape describes"}
   {:db/ident :workflow.shape/mermaid
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Agent-authored mermaid diagram of the workflow's steps"}
   {:db/ident :workflow.shape/updated
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one
    :db/doc "When the shape was last (re)authored"}

   ;; --- Legacy write-compat (kabel-auth.store.datahike still transacts
   ;;     these on signup; reads in is.simm.model.parties prefer the
   ;;     :actor/* core and fall back to these; `migrate-to-actors!`
   ;;     lifts them onto the actor core at every boot) ---
   {:db/ident :party/type
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc "LEGACY (kabel-auth write-compat): :human or :agent — superseded by :actor/kind"}
   {:db/ident :party/display-name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "LEGACY (kabel-auth write-compat): superseded by :actor/name"}
   {:db/ident :party/created
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one
    :db/doc "LEGACY (kabel-auth write-compat): superseded by :actor/created-at"}

   ;; --- Human-only (auth) ---
   {:db/ident :party/email
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/doc "Email address (humans only, unique)"}
   {:db/ident :party/password-hash
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Bcrypt password hash (humans only)"}
   {:db/ident :party/email-verified
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one
    :db/doc "Whether email is verified"}
   {:db/ident :party/auth-providers
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/many
    :db/doc "Auth providers (:password, :google, ...)"}
   {:db/ident :party/role
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc "Role: :admin or :user (humans only)"}
   {:db/ident :party/last-login
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one
    :db/doc "Last login timestamp"}
   {:db/ident :party/preferred-model
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Preferred LLM model id (humans use this for personal AI)"}

   ;; --- Agent-only ---
   {:db/ident :party/owner
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc "Party that owns (and is billed for) this agent"}

   ;; --- Sessions ---
   {:db/ident :session/id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/doc "Session UUID"}
   {:db/ident :session/party-id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/doc "Party UUID that owns this session"}
   {:db/ident :session/refresh-token-hash
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/doc "SHA-256 hash of refresh token"}
   {:db/ident :session/expires
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one
    :db/doc "Session expiration"}
   {:db/ident :session/user-agent
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Client user-agent"}
   {:db/ident :session/ip
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Client IP"}
   {:db/ident :session/created
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one
    :db/doc "Session created"}

   ;; --- Per-party environment variables ---
   {:db/ident :env-var/id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/doc "Env var UUID"}
   {:db/ident :env-var/party-id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/doc "Owning party UUID"}
   {:db/ident :env-var/key
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Env var key (e.g. SLACK_TOKEN)"}
   {:db/ident :env-var/value
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Env var value"}

   ;; --- Rooms ---
   {:db/ident :room/id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/doc "Room UUID"}
   {:db/ident :room/name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Display name"}
   {:db/ident :room/type
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc "Room type: :personal-ai, :group, :telegram-mirror"}
   {:db/ident :room/created-by
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/doc "Creator party UUID"}
   {:db/ident :room/created
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one
    :db/doc "Room creation timestamp"}
   ;; --- Telegram identity linking (Stage 4) ---
   {:db/ident :party/telegram-id
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/doc "Telegram numeric user id linked to this party"}
   {:db/ident :party/telegram-username
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Telegram @username at link time (informational; ids are canonical)"}

   ;; NOTE: :room/parties is dvergr's attribute (ref, cardinality-many to party
   ;; entities) — declared by dvergr.system.db, not here. simmis adopted the ref
   ;; version when the system DBs were unified (Stage 1, dvergr-integration-plan).
   {:db/ident :room/content-db-scope
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/doc "Konserve-sync scope UUID for this room's per-room Datahike DB"}
   {:db/ident :room/budget-dollars
    :db/valueType :db.type/double
    :db/cardinality :db.cardinality/one
    :db/doc "Per-room LLM budget in dollars (default 10.0)"}

   ;; --- Read cursors (durable unread badges; mentions-notifications design) ---
   ;; One per (party, room): the timestamp up to which the party has read the
   ;; room. Unread = count of :S.Message/sent-at after this. Key
   ;; "<party-uuid>:<room-uuid>" upserts by identity.
   {:db/ident :read-cursor/key
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/doc "\"<party>:<room>\" composite key for a per-(party,room) read cursor."}
   {:db/ident :read-cursor/at
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one
    :db/doc "Timestamp up to which the party has read the room; unread = messages after this."}

   ;; --- Per-room notification level (mentions-notifications design 3C) ---
   ;; One per (party, room). Absent ⇒ the default :mentions (badge everything,
   ;; pop only when @mentioned). :all pops every message; :none never pops
   ;; (badge stays — awareness is not muted).
   {:db/ident :notify-pref/key
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/doc "\"<party>:<room>\" composite key for a per-(party,room) notification level."}
   {:db/ident :notify-pref/level
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc "Notification level: :all | :mentions | :none (default :mentions when absent)."}

   ;; NOTE: room↔KB attachment lives in dvergr's grant rows
   ;; (:grant/room + :grant/system + :grant/permission), not on the room
   ;; entity — see is.simm.model.knowledge-bases (Stage 1b).

   ;; --- Screen-share grants (doc/archive/screen-capture-scoping.md) ---
   ;; A user's screen CAPTURE is owned by the user; a grant is a time-boxed
   ;; window in which one room may see it. The share button toggles a grant,
   ;; not a capture. `until` absent ⇒ the window is open now. The personal-ai
   ;; room's grant is standing (re-opened whenever a session starts).
   {:db/ident :screen-grant/id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :screen-grant/party
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/index true
    :db/doc "The sharer whose stream this window exposes"}
   {:db/ident :screen-grant/room
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/index true
    :db/doc "The audience room granted a view onto the stream"}
   {:db/ident :screen-grant/from
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/doc "Epoch ms the window opened"}
   {:db/ident :screen-grant/until
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/doc "Epoch ms the window closed; ABSENT ⇒ active (bounded by heartbeat)"}
   {:db/ident :screen-grant/beat
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/doc "Epoch ms of the last client heartbeat; a stale beat fails the window safe"}

   ;; --- Knowledge Bases ---
   ;; --- Proposals (ForkSets): the only first-class JOINT multi-system
   ;; state — a named {scope → branch} map + metadata. Lives here because
   ;; fork-sets span stores (doc/proposals-and-time-travel.md §6).
   {:db/ident :proposal/id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :proposal/title
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :proposal/summary
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "AI-written change summary"}
   {:db/ident :proposal/status
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc ":open :accepted :dismissed"}
   {:db/ident :proposal/intent
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc
    "What KIND of proposed future this is: :change (an edit to the present,
     today's only kind) | :budget | :goal | :scenario. ABSENT ⇒ :change, so
     this is purely additive and existing rows keep working.

     Status and intent answer different questions and both are needed to
     place a row in the UI: status says whether it is still live, intent says
     which view it belongs to at all. A budget and a wiki edit are both
     ForkSets over the same substrate, but one is a plan and the other is a
     patch, and only the reader knows which — nothing about the fork itself
     distinguishes them.

     Note the concept is a ForkSet while the rows are named `:proposal/*`.
     Renaming the namespace is a migration; recording intent is not, so the
     mismatch is deliberate and stays until a migration is worth it."}
   {:db/ident :proposal/author
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/doc "Party UUID"}
   {:db/ident :proposal/room
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/doc "Originating room (optional)"}
   {:db/ident :proposal/run
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/doc "Exact Dvergr Run whose retained world this proposal governs"}
   {:db/ident :proposal/adoption
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc "Durable ownership record for an adopted Dvergr world"}
   {:db/ident :proposal/created-at
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one}
   {:db/ident :proposal/resolved-at
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one}
   {:db/ident :proposal/resolved-by
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/doc "Party who accepted or dismissed it"}
   {:db/ident :proposal/resolution-note
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc
    "WHY this was accepted or dismissed, in the reviewer's words. Optional,
     and nothing reads it yet — it exists so the corpus starts accruing. An
     agent reviewer few-shot prompted on what this workspace has accepted vs
     refused, and for what stated reason, needs decisions that were recorded
     as they happened; the status flag alone carries none of the reasoning,
     and it cannot be reconstructed after the fact."}
   {:db/ident :proposal/forks
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/isComponent true
    :db/doc "The ForkSet rows"}
   {:db/ident :proposal.fork/scope
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/doc "System db-scope"}
   {:db/ident :proposal.fork/authority-scope
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/doc "Resource whose :merge grant governs this component; absent means scope"}
   {:db/ident :proposal.fork/branch
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Datahike branch name (keyword name part)"}
   {:db/ident :proposal.fork/base-commit
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Trunk head snapshot-id at fork time"}
   {:db/ident :proposal.fork/system-type
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc ":kb :fs :book :repo :room"}
   {:db/ident :proposal.fork/world-system-id
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Exact Spindel system id governed by this adopted-world component"}
   {:db/ident :proposal.fork/settlement-id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/doc "Affine settlement capability identity; the live handle is never persisted"}
   {:db/ident :proposal.fork/settlement-state
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc ":open :settling :committed :commit-failed :abort-failed"}
   {:db/ident :proposal.fork/settlement-operation
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one}
   {:db/ident :proposal.fork/descriptor
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Portable EDN descriptor; never contains a process-local ForkHandle"}

   ;; Adoption is the durable owner established before Dvergr transfers its
   ;; affine capability. A Proposal is created only after exact partition
   ;; descriptors have committed, so ordinary proposal readers never observe a
   ;; half-constructed ForkSet.
   {:db/ident :world-adoption/id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :world-adoption/run
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :world-adoption/room
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one}
   {:db/ident :world-adoption/proposal
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}
   {:db/ident :world-adoption/status
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc ":prepared :partitioning :open :releasing :released :failed :recovery-required"}
   {:db/ident :world-adoption/descriptor
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :world-adoption/error
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :world-adoption/updated-at
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one}
   ;; --- review conversation -------------------------------------------------
   {:db/ident :proposal/comments
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/isComponent true
    :db/doc
    "The review conversation.

     Before this the only free-form field was `:proposal/resolution-note`, and
     it is written ONCE, at the moment of accept or dismiss — so there was
     nowhere to say \"change this\" without also deciding. A reviewer's only
     moves were to take the change as it stood or refuse it, which for an
     agent-authored change is the wrong shape: the agent can revise, and asking
     it to is cheaper than either alternative."}
   {:db/ident :proposal.comment/id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/doc "Comment id"}
   {:db/ident :proposal.comment/body
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "What was said"}
   {:db/ident :proposal.comment/author
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/doc "The party who said it — human reviewer or agent"}
   {:db/ident :proposal.comment/at
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one
    :db/doc "When"}
   {:db/ident :proposal.comment/fork-branch
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc
    "The fork this comment is about, by branch name, or absent for the proposal
     as a whole.

     A branch name rather than a ref to the fork ENTITY: forks are components
     and a dismissed one keeps its row, but addressing a comment by entity would
     make the comment's meaning depend on that row surviving. The branch name is
     what the client already holds from the diff, and what the author was
     looking at when they wrote it."}
   {:db/ident :proposal.comment/kind
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc
    "`:comment` (default) or `:changes-requested`.

     A request for changes is a comment that also REOPENS the contributor's
     overlay onto its existing branch, so the distinction has to survive in the
     record — otherwise a proposal that went round twice reads afterwards as one
     that was accepted first time."}
   {:db/ident :proposal.fork/author
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/doc
    "The party whose writes this fork holds.

     `:proposal/author` is the FILER, which on a campaign is whoever happened to
     finish last — a live four-agent run filed a proposal whose recorded author
     had contributed no forks at all. Per-fork accept/dismiss asks a reviewer to
     judge one contribution at a time, and they cannot do that without knowing
     whose it is: the card showed two identical rows labelled only BOOK.

     Absent on rows filed before this existed, so nothing is backfilled."}
   {:db/ident :proposal.fork/status
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc
    "This ONE fork's own resolution: :accepted | :dismissed. ABSENT ⇒ still
     open, so nothing is backfilled and every ForkSet filed before this
     attribute existed keeps meaning exactly what it meant.

     Four agents can contribute four forks to one proposal, and a reviewer who
     must refuse one of them (the customer notice that admits liability) cannot
     be made to refuse the other three with it. Per-fork resolution is safe
     precisely because a fork IS an independent branch on its own scope:
     `accept-proposal!` filters which branches it merges, and never asks the
     3-way merge for part of one.

     A dismissed fork is kept as a row, not deleted. The refusal and the note
     that explains it are the record of a decision — the same reason
     `:proposal/resolution-note` exists — so the card renders it struck through
     rather than hiding it. Those two `:proposal/resolution-*` attributes are
     written onto the FORK entity as well: who decided and why means the same
     thing at either level, and a parallel `:proposal.fork/*` pair would have to
     be read by every consumer of the first for nothing in return."}

   {:db/ident :kb/id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/doc "KB UUID"}
   {:db/ident :kb/name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Display name"}
   {:db/ident :kb/owner
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/doc "Owning party UUID"}
   {:db/ident :kb/created
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one
    :db/doc "Creation timestamp"}
   {:db/ident :kb/shared-with
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/many
    :db/doc "Party UUIDs with access"}
   {:db/ident :kb/db-scope
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/doc "Konserve-sync scope UUID for this KB's Datahike DB"}
   {:db/ident :kb/tags
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/many
    :db/doc "Tags for grouping KBs"}
   {:db/ident :kb/system-id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/doc "The dvergr shared-registry :system/id row for this KB (grants target it)"}

   ;; --- Drives (file systems; doc/archive/file-system-design.md) ---
   ;; Same shape as KBs deliberately: registry row here, own datahike DB
   ;; (the fs.node tree) as an yggdrasil system, attached to rooms via
   ;; grants. Drives hold raw documents; KBs hold derived knowledge —
   ;; the auto-indexer (intake) walks a drive and publishes into a KB
   ;; fork with :derived-from qualified refs.
   {:db/ident :drive/id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/doc "Drive UUID"}
   {:db/ident :drive/name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Display name"}
   {:db/ident :drive/owner
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/doc "Owning party UUID"}
   {:db/ident :drive/created
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one
    :db/doc "Creation timestamp"}
   {:db/ident :drive/db-scope
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/doc "Konserve-sync scope UUID for this drive's Datahike DB (the file tree)"}
   {:db/ident :drive/system-id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/doc "The dvergr shared-registry :system/id row for this drive (grants target it)"}

   ;; --- Mail accounts (Briefkasten-backed knowledge sources) ---
   ;; Credentials are encrypted server-side with a key that is never exposed
   ;; through settings RPCs. Message metadata and blobs live in the account's
   ;; own Briefkasten database/CAS, not in this control-plane registry.
   {:db/ident :mail-account/id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/doc "Simmis mail account UUID"}
   {:db/ident :mail-account/name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :mail-account/email
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :mail-account/owner
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one}
   {:db/ident :mail-account/created
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one}
   {:db/ident :mail-account/db-scope
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/doc "Scope of the account's Briefkasten metadata database"}
   {:db/ident :mail-account/system-id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/doc "Shared-registry :system/id (:system/type :mail) used by grants"}
   {:db/ident :mail-account/folders
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/many
    :db/doc "Selected IMAP folders; empty means all folders"}
   {:db/ident :mail-account/status
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one}
   {:db/ident :mail-account/last-sync
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one}
   {:db/ident :mail-account/error
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :mail-account/secret-nonce
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Base64url AES-GCM nonce; server-only"}
   {:db/ident :mail-account/secret-ciphertext
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Base64url encrypted IMAP configuration; server-only"}

   ;; --- UI preferences (one entity per party) ---
   {:db/ident :ui-pref/party-id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/doc "Owning party UUID"}
   {:db/ident :ui-pref/syntax
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc "Preferred code syntax view: :clojure or :superficie"}])

;; =============================================================================
;; Migration: lift legacy :party/* rows onto the dvergr actor core
;; =============================================================================

(defn migrate-to-actors!
  "Idempotent migration: for every entity with a :party/id but no
   :actor/id, write the actor core attrs derived from the legacy party
   attrs (:party/type → :actor/kind, :party/display-name → :actor/name,
   :party/created → :actor/created-at, :party/system-prompt →
   :actor/system-prompt; model/provider/auto-respond?/template are folded
   into the :actor/config EDN string). Legacy datoms are left in place.

   Also catches humans created by kabel-auth since the last boot (it only
   writes :party/* attrs). Old :llm-log/* rows are NOT migrated — they
   are dead data, harmless in old stores."
  [conn]
  (let [eids (d/q '[:find [?e ...]
                    :where [?e :party/id _] (not [?e :actor/id _])]
                  @conn)]
    (when (seq eids)
      (let [;; Only pull attrs the store actually declares — fresh stores
            ;; never had the dropped agent attrs (:party/model etc.) and
            ;; datahike's pull rejects undeclared idents.
            declared (set (keys (:schema @conn)))
            pattern (into [:party/id]
                          (filter declared)
                          [:party/type :party/display-name :party/created
                           :party/system-prompt :party/model :party/provider
                           :party/auto-respond? :party/template])
            txs (mapv
                 (fn [eid]
                   (let [p (d/pull @conn pattern eid)
                         config (cond-> {}
                                  (:party/model p)    (assoc :model (:party/model p))
                                  (:party/provider p) (assoc :provider (:party/provider p))
                                  (contains? p :party/auto-respond?)
                                  (assoc :auto-respond? (:party/auto-respond? p))
                                  (:party/template p) (assoc :template (:party/template p)))]
                     (cond-> {:db/id eid
                              :actor/id (keyword "party" (str (:party/id p)))
                              :actor/kind (or (:party/type p) :human)
                              :actor/status :online
                              :actor/created-at (or (:party/created p) (java.util.Date.))}
                       (:party/display-name p)  (assoc :actor/name (:party/display-name p))
                       (:party/system-prompt p) (assoc :actor/system-prompt (:party/system-prompt p))
                       (seq config)             (assoc :actor/config (pr-str config)))))
                 eids)]
        (d/transact conn txs)
        (log/log! {:level :info
                   :id ::migrated-parties-to-actors
                   :msg "Migrated legacy party rows onto the actor core"
                   :data {:count (count txs)}})))))

;; =============================================================================
;; Schema-collision guard
;; =============================================================================

(defn- assert-schema-compatible!
  "Fail loudly if a simmis extension attribute redefines an already-installed
   ident (dvergr system/actor/chat schema) with different type, cardinality,
   or uniqueness. Datahike silently accepts such a redefinition and the store
   ends up with whichever definition was transacted LAST — see the
   :room/db-scope incident in doc/dvergr-integration-plan.md (Stage 1)."
  [conn extension-schema]
  (let [db @conn
        diffs (into []
                    (keep (fn [{:db/keys [ident valueType cardinality unique]}]
                            (when-let [existing
                                       (d/q '[:find (pull ?e [:db/valueType :db/cardinality :db/unique]) .
                                              :in $ ?ident :where [?e :db/ident ?ident]]
                                            db ident)]
                              (when (:db/valueType existing)
                                (let [mismatch (cond-> {}
                                                 (not= (:db/valueType existing) valueType)
                                                 (assoc :valueType {:installed (:db/valueType existing)
                                                                    :extension valueType})
                                                 (not= (:db/cardinality existing) cardinality)
                                                 (assoc :cardinality {:installed (:db/cardinality existing)
                                                                      :extension cardinality})
                                                 (not= (:db/unique existing) unique)
                                                 (assoc :unique {:installed (:db/unique existing)
                                                                 :extension unique}))]
                                  (when (seq mismatch) [ident mismatch]))))))
                    extension-schema)]
    (when (seq diffs)
      (throw (ex-info "simmis extension schema collides with installed system schema — rename the simmis attribute or adopt the installed definition"
                      {:collisions diffs})))))

;; =============================================================================
;; Connection Management
;; =============================================================================

(defonce conn (atom nil))

(defn get-conn
  "Get the system DB connection. Must call init! first."
  []
  @conn)

(defn init!
  "Initialize the system database. Idempotent - safe to call multiple times.

   The system DB IS dvergr's system DB (project-local `.dvergr/system-db`,
   governed by `dvergr.substrate.paths`). One physical DB means dvergr's
   rooms/systems/grants and simmis parties live on the same entities, so
   `[:party/id ...]` lookup refs resolve across both — the Stage 1
   prerequisite of doc/dvergr-integration-plan.md.

   On every init: obtains dvergr's connection (which installs dvergr's
   system/actor/pricing/task schema), installs dvergr's full chat schema
   (actor/ledger/budget/...) via `ensure-full-schema!`, transacts the simmis
   extension schema (idempotent upserts), then runs `migrate-to-actors!`."
  []
  (when-not @conn
    (let [c (sdb/get-conn)]
      ;; dvergr core: :actor/*, :ledger/*, :budget/*, :chat/*, ...
      (chat-schema/ensure-full-schema! c)
      ;; simmis extensions on the same entities — guarded against silent
      ;; redefinition of installed idents
      (assert-schema-compatible! c schema)
      (d/transact c schema)
      (migrate-to-actors! c)
      (reset! conn c)
      (log/log! {:level :info
                 :id ::system-db-connected
                 :msg "System database connected (shared dvergr system DB)"})
      c)))

(defn get-db
  "Get the current system DB value."
  []
  (when-let [c @conn]
    @c))
