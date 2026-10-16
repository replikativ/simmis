(ns is.simm.model.schema
  "Categorical schema definitions for Simmis.

   This schema implements a category-theoretic approach where:
   - Everything is a Block (pages, nested blocks, schema objects, morphisms, functors)
   - Schema is data (queryable entities, not code)
   - Properties are morphisms with dynamic attribute installation
   - Relations are hypergraphs (objects with role morphisms)
   - UI rendering via functors (S → Comp)

   Core categories:
   - S (Schema): entity types, value types, properties, relations
   - Comp (UI Components): Card, Table, Text, etc.
   - Cat₀ (Categories): meta-category containing S, Comp, etc.

   Chat/message schema is provided by dvergr (dvergr.chat.schema).
   Design rationale: doc/current-categorical-model.md."
  (:require [clojure.string :as str]
            [is.simm.model.morphism :as mor]
            [datahike.api :as d]))

;; ============================================================================
;; Universal Entity Attributes
;; ============================================================================

(def entity-schema
  "Universal attributes for all entities (blocks, schema objects, etc.)"
  [;; Identity
   {:db/ident :entity/uuid
    :db/valueType :db.type/uuid
    :db/unique :db.unique/identity
    :db/cardinality :db.cardinality/one
    :db/doc "Stable UUID for all entities"}

   {:db/ident :entity/name
    :db/valueType :db.type/string
    :db/unique :db.unique/identity
    :db/cardinality :db.cardinality/one
    :db/doc "Human-readable unique name. Used for lookup refs. Namespaced for schema objects (e.g., 'S/Block/title')"}

   ;; Timestamps
   {:db/ident :entity/created-at
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one
    ;; Word for word dvergr's (`dvergr.kb.schema/column-docs`). dvergr installs
    ;; its chat schema again on every fork's branched store; with a different
    ;; doc here, each fork of a room "changed" these two schema datoms back,
    ;; and a review's `[:diff/only-paths …]` check failed every simmis world
    ;; on a database nobody had touched.
    :db/doc "When the entity was first mentioned"}

   {:db/ident :entity/updated-at
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one
    :db/doc "When the entity was last updated"}])

;; ============================================================================
;; Block Attributes
;; ============================================================================

(def block-schema
  "Attributes for blocks (the only entity type - everything is a block)"
  [;; Structure
   {:db/ident :block/parent
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc "Parent block. Nil for top-level pages. Nested blocks have parents."}

   {:db/ident :block/order
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Fractional index string for block ordering. Lexicographically sortable."}

   {:db/ident :block/content
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "HTML content of the block"}

   {:db/ident :block/references
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/doc "Entities (pages or blocks) referenced by this block via [[page]] or ((block-uuid)) syntax"}

   {:db/ident :block/mentions
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/many
    :db/doc "@handle party-mentions in this block (value-level handle strings, cross-DB safe — parties live in the system DB). Resolved to parties at notify time. See doc/archive/mentions-notifications-contacts-design.md."}

   {:db/ident :block/collapsed
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one
    :db/doc "Whether this block is collapsed (children hidden) in the UI"}

   {:db/ident :block/viz-spec
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Vega-Lite specification as EDN string for visualization blocks. When present, block renders as a chart instead of text."}

   {:db/ident :block/widget-code
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Reactive widget code as a Clojure expression (string). Evaluated client-side by a curated SCI sandbox: no JS interop, no DOM access, no filesystem/network. Available vocab: el/* element fns, dh/q dh/pull dh/entity (read), kb/* (writes via authorized remote calls). The expression should return a vnode."}

   ;; Role/Type
   {:db/ident :instance/of-role
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/doc "Schema objects (types) this instance conforms to. Points to objects in category S."}])

;; ============================================================================
;; Category Schema
;; ============================================================================

(def category-schema
  "Attributes for category entities (S, Comp, Cat₀, etc.)"
  [{:db/ident :category/objects
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/doc "Objects in this category (entity types, value types)"}

   {:db/ident :category/morphisms
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/doc "Morphisms in this category (properties, relations, arrows between objects)"}

   {:db/ident :category/id-object
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc "Optional: designated identity object (for monoids/categories with unit)"}])

;; ============================================================================
;; Object Schema
;; ============================================================================

(def object-schema
  "Attributes for object entities (types in categories)"
  [{:db/ident :object/of-category
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc "Which category this object belongs to"}

   {:db/ident :object/primitive?
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one
    :db/doc "True for primitive value types (String, Number, Date, Boolean)"}

   {:db/ident :object/base-type
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc "For derived types (URL, Email), points to base type (String)"}

   {:db/ident :object/enum-values
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/many
    :db/doc "For enum types, allowed string values"}])

;; ============================================================================
;; Morphism Schema
;; ============================================================================

(def morphism-schema
  "Attributes for morphism entities (properties, relations, arrows in categories)"
  [{:db/ident :morphism/src
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc "Source object (domain of morphism)"}

   {:db/ident :morphism/dst
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc "Target object (codomain of morphism)"}

   {:db/ident :morphism/of-category
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc "Which category this morphism belongs to"}

   {:db/ident :morphism/cardinality
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc ":one or :many (for multi-valued properties)"}

   {:db/ident :morphism/optional?
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one
    :db/doc "True if this property is optional (can be nil)"}

   {:db/ident :morphism/property-type
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc "UI rendering hint: :text, :number, :date, :checkbox, :select, :multi-select, :url, :email, :phone, :relation, :rollup, :formula"}

   {:db/ident :morphism/attr-type
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc "For Attr morphisms projected from a katzen schema: the exact katzen attr-type (e.g. :Identity, :Keyword, :Instant). Preserves the distinction the primitive dst object would otherwise collapse (Identity/String/Keyword all map onto S/String)."}

   {:db/ident :morphism/unique
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc "Datahike uniqueness for the projected storage attribute (:db.unique/value or :db.unique/identity). Set for identity/key attrs such as the knowledge-base title."}

   {:db/ident :morphism/computation
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "EDN string describing computation for rollup/formula properties"}

   {:db/ident :morphism/validation
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "EDN string describing validation rules (regex, range, etc.)"}

   {:db/ident :morphism/storage-attr
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc "Explicit datahike storage attribute for this property, overriding the default S.<Object>/<prop> derivation from the morphism name. Set when projecting a katzen schema whose object declares a :storage-ns, so the property REUSES an existing external attribute (e.g. :kontor.posting/amount) instead of minting a parallel :S.* one — the categorical view then reads/writes the domain system's own data with zero query-time indirection."}])

;; ============================================================================
;; Functor Schema
;; ============================================================================

(def page-record-schema
  "Attributes that mark an S/Page as a generated RECORD rather than a page a
   human wrote — today only the chat summaries `is.simm.agents.summarizer`
   files. The chat backref is value-level (room uuid + window instants, never
   eids), so a record page survives being read from another database.

   Ordinary wiki pages carry none of these; `views/nav` filters records out of
   the browse list by `:S.Page/kind`.

   Kept here as the single definition. `store/install!` re-transacts
   `full-schema` unconditionally on every store, which is what gets it onto
   stores that predate it — there is no migration step."
  [{:db/ident :S.Page/kind
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc "Page record kind, e.g. :chat-summary. Absent on ordinary wiki pages."}

   {:db/ident :S.Page/room
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/index true
    :db/doc "Room this record page summarizes (value-level uuid, cross-DB safe)."}

   {:db/ident :S.Page/window-start
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one
    :db/doc "Start of the summarized conversation window."}

   {:db/ident :S.Page/window-end
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one
    :db/doc "End of the summarized conversation window."}])

(def kb-event-schema
  "Attributes for S/KBEvent entities — first-class timeline events recording
   knowledge base changes, interleaved with messages in chat history."
  [{:db/ident :S.KBEvent/room
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc "ChatRoom whose timeline this event belongs to"}

   {:db/ident :S.KBEvent/kb-id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/doc "UUID of the knowledge base that was modified (nil for main shared DB)"}

   {:db/ident :S.KBEvent/type
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc "Kind of change: :page-created, :blocks-added, :block-removed, :page-updated"}

   {:db/ident :S.KBEvent/entity-uuid
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/doc "UUID of the page or block that was affected"}

   {:db/ident :S.KBEvent/title
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Snapshot of the page title at time of event"}

   {:db/ident :S.KBEvent/block-count
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/doc "Number of blocks affected (for :blocks-added events)"}

   {:db/ident :S.KBEvent/author
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc "S/User who caused this event"}

   {:db/ident :S.KBEvent/timestamp
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one
    :db/doc "When this event occurred"}

   {:db/ident :S.KBEvent/in-response-to
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc "The S/Message entity that triggered this event (for AI-generated events)"}])

;; ============================================================================
;; Eval Entry Schema (REPL history in chat timeline)
;; ============================================================================

(def eval-entry-schema
  "Attributes for S.EvalEntry entities — records of clojure_eval tool calls
   by agents, displayed as expandable events in the chat timeline."
  [{:db/ident :S.EvalEntry/room
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc "ChatRoom this eval entry belongs to"}

   {:db/ident :S.EvalEntry/agent
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc "S.User (agent) who ran this evaluation"}

   {:db/ident :S.EvalEntry/tool
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Tool that produced this entry (clojure_eval, shell, screen_look, …). Absent on entries written before tools other than clojure_eval were projected."}

   {:db/ident :S.EvalEntry/code
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "The code evaluated, or the tool's input (pretty-printed EDN) for non-eval tools"}

   {:db/ident :S.EvalEntry/result
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Result string (may be truncated for large outputs)"}

   {:db/ident :S.EvalEntry/success?
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one
    :db/doc "Whether evaluation succeeded"}

   {:db/ident :S.EvalEntry/evaluated-at
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one
    :db/doc "When this evaluation occurred"}])

;; ============================================================================
;; Code Cross-Reference Schema (yggdrasil / repo integration)
;; ============================================================================

(def code-schema
  "Attributes for linking wiki entities to source code repositories.
   Any block or page can optionally reference a specific code location."
  [{:db/ident :code/repo-uuid
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/doc "UUID of the source repository (yggdrasil repo ID) this entity references"}

   {:db/ident :code/path
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "File or directory path within the repository (e.g., 'src/main/MyClass.java')"}

   {:db/ident :code/commit
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Git commit hash or symbolic ref for point-in-time code reference (e.g., 'main', 'abc1234')"}])

;; ============================================================================
;; Full Schema
;; ============================================================================

(def message-record-schema
  "Message attributes that are simmis's, not dvergr's.

   These lived in `store/ensure-late-schema!` as one-off migrations, which meant
   they were installed on stores reached by that function and NOWHERE else. The
   app store is not, and `columns.cljc` falls back to it when a room store has
   not connected — where three of these appear in `:where` position, so the
   WHOLE chat timeline query rejects rather than one chip going missing.

   `store/install!` re-transacts `full-schema` unconditionally on every store,
   so putting them here installs them everywhere, once, and the migration entry
   becomes redundant. That is the general rule: a new attribute goes in
   `full-schema`, never in a migration."
  [{:db/ident :S.Message/mentions
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/many
    :db/doc "[[Title]] wiki-mentions in the message text (value-level, cross-DB safe) — extracted by the room projector."}
   {:db/ident :S.Message/party-mentions
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/many
    :db/doc "@handle party-mentions in the message (value-level handle strings, cross-DB safe). Resolved to parties at notify time."}
   {:db/ident :S.Message/attachment-blob
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Content-addressed blob id (is.simm.model.blobs) attached to the message, e.g. original voice-note audio."}
   {:db/ident :S.Message/attachment-mime
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :S.Message/reasoning
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "The agent's <think> reasoning for this message (collapsed in the UI)."}
   {:db/ident :block/widget-code
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Reactive widget code; evaluated client-side in a curated SCI sandbox."}
   {:db/ident :S.EvalEntry/tool
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Tool that produced this entry (clojure_eval, shell, …); absent on pre-existing eval-only entries."}
   ;; A Lucide icon name on a type object, so a page tagged with that type can
   ;; render the type's icon. Was seeded only by `ensure-person-schema!`, yet
   ;; `nav/query-pages` pulls it on every store.
   {:db/ident :object/icon
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}])

(def provenance-schema
  "Who made a transaction, recorded ON the transaction via `:tx-meta`.

   Datahike stamps `:db/txInstant` automatically and a user-supplied value in
   `:tx-meta` wins over it (`datahike.db.transaction/next-tx-instant`), so a
   write can carry both WHEN it happened and WHO did it without touching the
   entities it writes. That is what turns the Timelines audit view from `this
   page changed` into `Vár changed this page at 09:41`.

   A STRING, not a ref: parties live in the shared system DB and these
   transactions land in per-KB and per-room stores, so a ref would dangle. The
   value is the party uuid as a string, resolved for display against the roster
   the client already holds."
  [{:db/ident :tx/author
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "Party uuid (as a string) that caused this transaction. Set via :tx-meta."}])

(def customer-schema
  "Customer accounts as RECORDS, not pages.

   The reconciliation lane's whole point is that `who was charged and never
   provisioned` is a QUERY over facts, not a search over documents — so these
   are datoms with their own attributes rather than wiki prose. Charged and
   provisioned are separate instants precisely so their absence is expressible:
   a customer with `charged-at` and no `provisioned-at` IS the incident."
  [{:db/ident :S.Customer/account-id
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/doc "Stable external account id."}
   {:db/ident :S.Customer/email
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :S.Customer/plan
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :S.Customer/amount-cents
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/doc "What they were charged, in minor units — integers, never floats, for money."}
   {:db/ident :S.Customer/charged-at
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one}
   {:db/ident :S.Customer/provisioned-at
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one
    :db/doc "ABSENT when provisioning never happened — that absence is the query."}
   {:db/ident :S.Customer/credited-at
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one}])

(def label-schema
  "A person's verdict on a workflow's work or on its reviewer — one entity per
   decision, in the ROOM's store (`is.simm.workflows.review`,
   doc/proposals-and-time-travel.md \"Review escalations and labels\").

   Append-only: nothing in simmis updates or retracts a label, so a later
   reader (agreement rates, a case pack of this room's decisions) sees every
   decision exactly as it was made. Identity is `:entity/uuid`, like every
   entity in a store."
  [{:db/ident :S.Label/subject-kind
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc ":proposal | :run — what the label is about, with `:S.Label/subject-id`"}
   {:db/ident :S.Label/subject-id
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/index true}
   {:db/ident :S.Label/about
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc ":workflow (the work was right or wrong) | :reviewer (the reviewer's approval was)"}
   {:db/ident :S.Label/review
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/index true
    :db/doc "The dvergr Review (`:review/id`) the decision followed"}
   {:db/ident :S.Label/policy
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "The review spec's content id (`:review/policy`) the Review ran under"}
   {:db/ident :S.Label/reviewer-verdict
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc "What the reviewer said (:approve | :escalate); absent when no reviewer was asked"}
   {:db/ident :S.Label/workflow-id
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :S.Label/workflow-version
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :S.Label/verdict
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc ":accept | :reject | :edit. About the reviewer: :accept agrees with it, :reject does not"}
   {:db/ident :S.Label/edit
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "The diff the person applied before accepting (verdict :edit)"}
   {:db/ident :S.Label/by
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one
    :db/doc "The deciding party"}
   {:db/ident :S.Label/reasons
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :S.Label/at
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one}])

(def full-schema
  "Complete categorical schema - all attributes.
   Note: chat/message/participant schema is installed separately via dvergr."
  (vec (concat entity-schema
               block-schema
               category-schema
               object-schema
               morphism-schema
               kb-event-schema
               page-record-schema
               eval-entry-schema
               message-record-schema
               provenance-schema
               customer-schema
               code-schema
               label-schema)))

;; ============================================================================
;; Helper Functions
;; ============================================================================

(defn morphism->attr-ident
  "Convert morphism entity name to Datahike attribute ident.
   Example: 'S/Block/title' → :S.Block/title"
  [morphism-name]
  ;; Kept as the server-side name; the definition lives in
  ;; `is.simm.model.morphism`, which the CLJS client can require.
  (mor/name->attr-ident morphism-name))

(defn attr-ident->morphism
  "Convert Datahike attribute ident to morphism entity name.
   Example: :S.Block/title → 'S/Block/title'"
  [attr-ident]
  (let [attr-ns (namespace attr-ident)
        attr-name (name attr-ident)
        morphism-ns (str/replace attr-ns #"\." "/")]
    (str morphism-ns "/" attr-name)))

(defn object-name->namespace
  "Extract namespace from object name for derived attributes.
   Example: 'S/Block' → 'S.Block'"
  [object-name]
  (str/replace object-name #"/" "."))

(defn codomain->db-type
  "Map object codomain to Datahike :db/valueType.
   Takes an object entity (or entity name) and returns the appropriate DB type."
  [object-ref]
  (let [object-name (if (string? object-ref)
                      object-ref
                      (:entity/name object-ref))]
    (case object-name
      "S/String" :db.type/string
      "S/Number" :db.type/long
      "S/Float" :db.type/double
      "S/Boolean" :db.type/boolean
      "S/Date" :db.type/instant
      "S/UUID" :db.type/uuid
      "S/URL" :db.type/string
      "S/Email" :db.type/string
      "S/Phone" :db.type/string
      ;; Default: references to other entities
      :db.type/ref)))

(def attr-type->db-type
  "The exact katzen attr-type → Datahike `:db/valueType`.

   `codomain->db-type` goes through the PRIMITIVE object a morphism points at
   (`S/String`, `S/Number`, …), and several katzen attr-types collapse onto one
   primitive — `:Identity`, `:String` and `:Keyword` all land on `S/String`.
   That collapse is fine for the categorical view and wrong for storage: it
   turned katzen's `kind : Entity → Keyword` into a `:db.type/string`
   declaration that contradicted `:S.Page/kind`'s real `:db.type/keyword`, and
   datahike rightly refused to alter an attribute with existing datoms. The
   projection aborted mid-`doseq`, so every attr AFTER `kind` (`created-at`,
   `updated-at`) silently never projected at all.

   `katzen-projection` already records the exact attr-type on
   `:morphism/attr-type` precisely so the distinction survives; this is where
   storage finally reads it."
  {:Identity :db.type/string :String :db.type/string :Str :db.type/string
   :URI :db.type/string
   :Keyword :db.type/keyword :Symbol :db.type/symbol
   :Long :db.type/long :Int :db.type/long :Integer :db.type/long
   ;; DELIBERATE DIVERGENCE from katzen, which maps :Number to
   ;; :db.type/double. simmis's S/Number primitive is the one every
   ;; hand-written seed morphism points at for counts and quantities, and
   ;; `codomain->db-type "S/Number"` has always answered :db.type/long. Reals
   ;; have :Float/:Double, and money now has :BigDec, so nothing needs :Number
   ;; to be lossy. Written down because it is the one place the two tables
   ;; disagree, and the test below asserts the rest.
   :Number :db.type/long
   :BigDec :db.type/bigdec :BigInt :db.type/bigint
   :Float :db.type/double :Double :db.type/double
   :Boolean :db.type/boolean :Bool :db.type/boolean
   :Instant :db.type/instant :Date :db.type/instant
   :UUID :db.type/uuid
   ;; A reference whose target katzen cannot name — datahike's schema does not
   ;; record what a :db.type/ref points at. See `accounting/vt->attr-type`.
   :Ref :db.type/ref})

(defn cardinality->db-cardinality
  "Map morphism cardinality to Datahike :db/cardinality."
  [morphism-cardinality]
  (case morphism-cardinality
    :one :db.cardinality/one
    :many :db.cardinality/many
    :db.cardinality/one))

;; ============================================================================
;; Schema Installation Helpers
;; ============================================================================

(defn morphism->attribute-schema
  "Generate Datahike attribute schema from morphism entity.
   This is used during dynamic schema installation when morphisms are created.

   Note: For seed data where morphism/dst is a lookup ref, you must pass
   the full morphism entity with :morphism/dst resolved, or pass dst-name separately."
  ([morphism-entity]
   (let [attr-ident (or (:morphism/storage-attr morphism-entity)
                        (morphism->attr-ident (:entity/name morphism-entity)))
         ;; Extract object name from lookup ref if needed
         dst (if (vector? (:morphism/dst morphism-entity))
               ;; For seed data: lookup ref, need to know the object name
               ;; This is a limitation - we'll pass dst-name explicitly for seed
               (:morphism/dst morphism-entity)
               (:entity/name (:morphism/dst morphism-entity)))
         ;; The exact attr-type wins over the collapsed primitive — see
         ;; `attr-type->db-type`. Absent (hand-authored morphisms) or unknown,
         ;; fall back to the primitive the morphism points at.
         value-type (or (attr-type->db-type (:morphism/attr-type morphism-entity))
                        (codomain->db-type dst))
         cardinality (cardinality->db-cardinality (:morphism/cardinality morphism-entity))]
     {:db/ident attr-ident
      :db/valueType value-type
      :db/cardinality cardinality
      :db/doc (str "Property: " (:entity/name morphism-entity))}))
  ([morphism-entity dst-object-name]
   ;; Version that takes explicit dst object name (for seed data)
   (let [attr-ident (or (:morphism/storage-attr morphism-entity)
                        (morphism->attr-ident (:entity/name morphism-entity)))
         value-type (or (attr-type->db-type (:morphism/attr-type morphism-entity))
                        (codomain->db-type dst-object-name))
         cardinality (cardinality->db-cardinality (:morphism/cardinality morphism-entity))]
     {:db/ident attr-ident
      :db/valueType value-type
      :db/cardinality cardinality
      :db/doc (str "Property: " (:entity/name morphism-entity))})))

(comment
  ;; Example usage:
  (morphism->attribute-schema
   {:entity/name "S/Block/title"
    :morphism/dst "S/String"
    :morphism/cardinality :one})
  ;; => {:db/ident :S.Block/title
  ;;     :db/valueType :db.type/string
  ;;     :db/cardinality :db.cardinality/one
  ;;     :db/doc "Property: S/Block/title"}
  )

;; ============================================================================
;; Database Initialization
;; ============================================================================

(defn install-schema!
  "Install the categorical schema into a Datahike connection"
  [conn]
  (d/transact conn full-schema))

(defn reset-schema!
  "Reset database and install fresh categorical schema"
  [cfg]
  (try
    (d/delete-database cfg)
    (catch Exception _))
  (d/create-database cfg)
  (let [conn (d/connect cfg)]
    (install-schema! conn)
    conn))
