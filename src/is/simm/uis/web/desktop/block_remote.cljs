(ns is.simm.uis.web.desktop.block-remote
  "Client-side remote block operations.

   These functions call the server via distributed-scope's invoke-remote.
   Each returns a core.async channel with the result.

   Each function accepts two optional trailing args:
   - `db-scope` — UUID of the KB to target (nil → shared DB).
   - `branch-kw` — keyword of the branch to target on that KB
                   (nil or `:db` → trunk). When provided, the server-side
                   handler routes the write through
                   `branching/get-kb-conn-on-branch`."
  (:require [is.simm.distributed-scope :refer [invoke-remote]]))

;; =============================================================================
;; Peer IDs (must match runtimes/web.clj)
;; =============================================================================

(def server-id #uuid "05a06e85-e7ca-4213-9fe5-04ae511e50a0")

;; =============================================================================
;; Remote Block CRUD Operations
;; Each returns a core.async channel with the result
;; =============================================================================

(defn create-block-remote!
  "Create a new block on the server."
  [parent-uuid content order & [db-scope branch-kw]]
  (invoke-remote server-id
                 'block-remote/create-block
                 (cond-> {:parent-uuid parent-uuid :content content :order order}
                   db-scope (assoc :db-scope db-scope)
                   branch-kw (assoc :branch-kw branch-kw))))

(defn create-block-after-remote!
  "Create a new block after a specific position using fractional indexing."
  [parent-uuid content insert-after-order & [db-scope branch-kw]]
  (invoke-remote server-id
                 'block-remote/create-block
                 (cond-> {:parent-uuid parent-uuid :content content :insert-after-order insert-after-order}
                   db-scope (assoc :db-scope db-scope)
                   branch-kw (assoc :branch-kw branch-kw))))

(defn create-sibling-block-after-remote!
  "Create a new sibling block after the given block.
   Server figures out parent-id and order from block-id. `new-uuid`, when
   given, is the new block's uuid — the one a prediction already shows."
  [block-uuid content & [db-scope branch-kw new-uuid]]
  (invoke-remote server-id
                 'block-remote/create-sibling-after
                 (cond-> {:block-uuid block-uuid :content content}
                   db-scope (assoc :db-scope db-scope)
                   branch-kw (assoc :branch-kw branch-kw)
                   new-uuid (assoc :new-uuid new-uuid))))

(defn ensure-page-remote!
  "Ensure a page exists with the given UUID. Creates it if it doesn't exist.
   Returns a channel with {:status :exists|:created :uuid page-uuid :max-tx t}.
   `first-block-uuid`, when given, is the uuid of the empty first block a new
   page is created with — the one a prediction already shows."
  [page-uuid title & [db-scope branch-kw first-block-uuid]]
  (invoke-remote server-id
                 'block-remote/ensure-page
                 (cond-> {:page-uuid page-uuid :title title}
                   db-scope (assoc :db-scope db-scope)
                   branch-kw (assoc :branch-kw branch-kw)
                   first-block-uuid (assoc :first-block-uuid first-block-uuid))))

(defn rename-page-remote!
  "Rename a page. Sends the INSTRUCTION, not the resulting transaction.

   The server repoints inbound `:block/references` and rewrites the literal
   `[[Old Title]]` text in every referring block — work whose transaction is
   O(referring blocks × content size), against a constant-size instruction.

   Returns a channel with {:success true :blocks-updated n}
                       or {:conflict true :existing-page-uuid uuid}.
   Pass :overwrite? or :merge? to resolve a conflict."
  [page-uuid new-title & [{:keys [db-scope branch-kw overwrite? merge?]}]]
  (invoke-remote server-id
                 'block-remote/rename-page
                 (cond-> {:page-uuid page-uuid :new-title new-title}
                   db-scope (assoc :db-scope db-scope)
                   branch-kw (assoc :branch-kw branch-kw)
                   overwrite? (assoc :overwrite? true)
                   merge? (assoc :merge? true))))

(defn update-block-content-remote!
  "Update a block's content on the server."
  [block-uuid content & [db-scope branch-kw]]
  (invoke-remote server-id
                 'block-remote/update-block-content
                 (cond-> {:block-uuid block-uuid :content content}
                   db-scope (assoc :db-scope db-scope)
                   branch-kw (assoc :branch-kw branch-kw))))

(defn update-block-order-remote!
  "Update a block's order on the server."
  [block-uuid new-order & [db-scope branch-kw]]
  (invoke-remote server-id
                 'block-remote/update-block-order
                 (cond-> {:block-uuid block-uuid :new-order new-order}
                   db-scope (assoc :db-scope db-scope)
                   branch-kw (assoc :branch-kw branch-kw))))

(defn update-block-collapsed-remote!
  "Update a block's collapsed state on the server."
  [block-uuid collapsed & [db-scope branch-kw]]
  (invoke-remote server-id
                 'block-remote/update-block-collapsed
                 (cond-> {:block-uuid block-uuid :collapsed collapsed}
                   db-scope (assoc :db-scope db-scope)
                   branch-kw (assoc :branch-kw branch-kw))))

(defn toggle-block-collapsed-remote!
  "Toggle a block's collapsed state on the server.
   Server reads current state and flips it."
  [block-uuid & [db-scope branch-kw]]
  (invoke-remote server-id
                 'block-remote/toggle-block-collapsed
                 (cond-> {:block-uuid block-uuid}
                   db-scope (assoc :db-scope db-scope)
                   branch-kw (assoc :branch-kw branch-kw))))

(defn delete-block-remote!
  "Delete a block on the server."
  [block-uuid cascade? & [db-scope branch-kw]]
  (invoke-remote server-id
                 'block-remote/delete-block
                 (cond-> {:block-uuid block-uuid :cascade? cascade?}
                   db-scope (assoc :db-scope db-scope)
                   branch-kw (assoc :branch-kw branch-kw))))

(defn indent-block-remote!
  "Indent a block (make it a child of previous sibling)."
  [block-uuid & [db-scope branch-kw]]
  (invoke-remote server-id
                 'block-remote/indent-block
                 (cond-> {:block-uuid block-uuid}
                   db-scope (assoc :db-scope db-scope)
                   branch-kw (assoc :branch-kw branch-kw))))

(defn outdent-block-remote!
  "Outdent a block (make it a sibling of its parent)."
  [block-uuid & [db-scope branch-kw]]
  (invoke-remote server-id
                 'block-remote/outdent-block
                 (cond-> {:block-uuid block-uuid}
                   db-scope (assoc :db-scope db-scope)
                   branch-kw (assoc :branch-kw branch-kw))))

(defn move-block-up-remote!
  "Move a block up (swap with previous sibling)."
  [block-uuid & [db-scope branch-kw]]
  (invoke-remote server-id
                 'block-remote/move-block-up
                 (cond-> {:block-uuid block-uuid}
                   db-scope (assoc :db-scope db-scope)
                   branch-kw (assoc :branch-kw branch-kw))))

(defn move-block-down-remote!
  "Move a block down (swap with next sibling)."
  [block-uuid & [db-scope branch-kw]]
  (invoke-remote server-id
                 'block-remote/move-block-down
                 (cond-> {:block-uuid block-uuid}
                   db-scope (assoc :db-scope db-scope)
                   branch-kw (assoc :branch-kw branch-kw))))

(defn find-page-by-title-remote!
  "Find a page by its title. Returns the page UUID or nil if not found."
  [title & [db-scope branch-kw]]
  (invoke-remote server-id
                 'block-remote/find-page-by-title
                 (cond-> {:title title}
                   db-scope (assoc :db-scope db-scope)
                   branch-kw (assoc :branch-kw branch-kw))))

;; =============================================================================
;; Page types and properties
;; =============================================================================
;;
;; These carry `db-scope` (and a branch, when one is checked out) for the same
;; reason every other write here does: the server resolves the store. The
;; handlers they used to be resolved the client's DEFAULT conn instead, so a
;; type or property set on a KB page was written into the app store — a
;; different database, where nothing reads it. See `block_remote.clj`.

(defn add-type-remote!
  "Tag a page with a type."
  [page-uuid type-uuid & [db-scope branch-kw]]
  (invoke-remote server-id
                 'block-remote/add-type
                 (cond-> {:page-uuid page-uuid :type-uuid type-uuid}
                   db-scope (assoc :db-scope db-scope)
                   branch-kw (assoc :branch-kw branch-kw))))

(defn remove-type-remote!
  [page-uuid type-uuid & [db-scope branch-kw]]
  (invoke-remote server-id
                 'block-remote/remove-type
                 (cond-> {:page-uuid page-uuid :type-uuid type-uuid}
                   db-scope (assoc :db-scope db-scope)
                   branch-kw (assoc :branch-kw branch-kw))))

(defn add-property-remote!
  "Declare a property on a type page.

   The client sends the SHAPE it wants — a morphism name, a value type, a
   target — and the server derives the attribute keyword. Schema is
   append-only, so what may be declared is a server decision."
  [{:keys [type-page-uuid morphism-name cardinality optional?
           property-type target-type-uuid target-object-name]}
   & [db-scope branch-kw]]
  (invoke-remote server-id
                 'block-remote/add-property
                 (cond-> {:type-page-uuid type-page-uuid
                          :morphism-name morphism-name
                          :cardinality cardinality
                          :optional? optional?
                          :property-type property-type
                          :target-type-uuid target-type-uuid
                          :target-object-name target-object-name}
                   db-scope (assoc :db-scope db-scope)
                   branch-kw (assoc :branch-kw branch-kw))))

(defn remove-property-remote!
  "Retract a property AND every value stored under it. Destructive — confirm
   before calling. Returns `{:values-removed n}`."
  [morphism-uuid morphism-name & [db-scope branch-kw]]
  (invoke-remote server-id
                 'block-remote/remove-property
                 (cond-> {:morphism-uuid morphism-uuid :morphism-name morphism-name}
                   db-scope (assoc :db-scope db-scope)
                   branch-kw (assoc :branch-kw branch-kw))))

(defn save-property-remote!
  "Set or clear one property value on a page."
  [page-uuid morphism-name value & [db-scope branch-kw]]
  (invoke-remote server-id
                 'block-remote/save-property
                 (cond-> {:page-uuid page-uuid :morphism-name morphism-name :value value}
                   db-scope (assoc :db-scope db-scope)
                   branch-kw (assoc :branch-kw branch-kw))))

;; =============================================================================
;; Widget writes
;; =============================================================================
;;
;; A widget's writes (`widget_sandbox.cljs`). Both act on the trunk of KB
;; `db-scope` and answer `{:max-tx head …}`; see `block_remote.clj`.

(defn widget-transact-remote!
  "A widget's DATA transaction (`:write` on the KB). Schema in it is refused."
  [db-scope tx-data]
  (invoke-remote server-id
                 'block-remote/widget-transact
                 {:db-scope db-scope :tx-data tx-data}))

(defn widget-install-attr-remote!
  "Declare `ident` (`:merge` on the KB — its owner). `codomain` is an OBJECT
   name (\"S/String\"); the server derives the value type from it."
  [db-scope ident codomain cardinality]
  (invoke-remote server-id
                 'block-remote/widget-install-attr
                 {:db-scope db-scope :ident ident :codomain codomain
                  :cardinality cardinality}))
