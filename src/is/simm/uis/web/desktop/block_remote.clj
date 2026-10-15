(ns is.simm.uis.web.desktop.block-remote
  "Server-side remote block handlers.

   These handlers are registered with the distributed-scope system
   and invoked when clients call the corresponding remote functions.

   Each handler accepts an optional `:branch-kw` in the arg-map: when
   provided and not `:db` (trunk), the write is routed to that branch
   via `branching/get-kb-conn-on-branch`. Data propagation back to
   clients flows through kabel store sync — no per-tx pubsub needed."
  (:require [clojure.string :as str]
            [is.simm.distributed-scope :as ds]
            [is.simm.model.crud :as crud]
            [is.simm.model.morphism :as mor]
            [is.simm.model.db :as db]
            [is.simm.model.knowledge-bases :as kbs]
            [is.simm.model.fractional-index :as frac]
            [is.simm.model.references :as refs]
            [is.simm.model.schema :as schema]
            [is.simm.runtimes.branching :as branching]
            [datahike.api :as d]
            [superv.async :refer [go-try S]]))

;; =============================================================================
;; Connection Resolution
;; =============================================================================

(defn- resolve-conn
  "Resolve the correct Datahike connection for a write.

   Three layered cases:
   1. `branch-kw` explicit + not `:db` → check out that branch via
      `branching/get-kb-conn-on-branch`. Used by browser writes that
      target a non-trunk branch (KB-only branching path).
   2. `db-scope` only → prefer `branching/get-kb-conn` so writes from
      inside a forked spin-context (e.g. an agent in a `fork-room :ctx`)
      land on the cascaded branch automatically. Falls back to
      `kbs/connect-kb-database` for KBs not yet registered (freshly
      imported stores).
   3. Neither → shared DB."
  ([db-scope] (resolve-conn db-scope nil))
  ([db-scope branch-kw]
   (if db-scope
     (let [uuid (if (string? db-scope)
                  (java.util.UUID/fromString db-scope)
                  db-scope)]
       (cond
         (and branch-kw (not= :db branch-kw))
         (or (branching/get-kb-conn-on-branch uuid branch-kw)
             (throw (ex-info "KB branch not resolvable"
                             {:db-scope (str uuid)
                              :branch-kw branch-kw})))

         :else
         ;; Mirror the branch path: throw a useful error if no KB exists
         ;; for this scope. Previously this fell through to nil, and
         ;; downstream `@conn` blew up as "Cannot invoke Future.get()
         ;; because fut is null" — a confusing stack for the actual
         ;; "this KB has no on-disk DB" condition.
         (or (branching/get-kb-conn uuid)
             (kbs/connect-kb-database uuid)
             (throw (ex-info "KB not resolvable"
                             {:db-scope (str uuid)})))))
     (db/get-conn))))

;; =============================================================================
;; Remote Block Handlers
;; These are executed on the server when invoked via distributed-scope
;; =============================================================================

(defn -create-block-handler
  "Create a block using fractional indexing for ordering.

   Args:
     parent-uuid - UUID of the parent block/page
     content - HTML content string
     order - Optional: explicit fractional index string
     insert-after-order - Optional: fractional index to insert after

   If `insert-after-order` is provided, generates a key between it and the next sibling.
   Otherwise, appends at the end using fractional indexing."
  [{:keys [parent-uuid content order insert-after-order db-scope branch-kw]}]
  (go-try S
    (let [conn (resolve-conn db-scope branch-kw)
          db @conn]
      (if insert-after-order
        ;; Insert after specific position: find next sibling and generate key between
        (let [;; Find all siblings with order > insert-after-order, get the min (next one)
              ;; Note: [:find (min ?order) .] returns a scalar, not a tuple
              next-order (d/q '[:find (min ?order) .
                                :in $ ?parent-uuid ?after-order
                                :where
                                [?parent :entity/uuid ?parent-uuid]
                                [?e :block/parent ?parent]
                                [?e :block/order ?order]
                                [(> ?order ?after-order)]]
                              db parent-uuid insert-after-order)
              ;; Generate fractional index between insert-after-order and next-order
              new-order (frac/generate-key-between insert-after-order next-order)]
          (crud/create-block! conn {:parent parent-uuid
                                    :content content
                                    :order new-order}))

        ;; Simple append or explicit order
        (let [final-order (if order
                           order
                           ;; Find max order among siblings and generate key after it
                           ;; Note: [:find (max ?order) .] returns a scalar, not a tuple
                           (let [max-order (d/q '[:find (max ?order) .
                                                   :in $ ?parent-uuid
                                                   :where
                                                   [?parent :entity/uuid ?parent-uuid]
                                                   [?block :block/parent ?parent]
                                                   [?block :block/order ?order]]
                                                 db parent-uuid)]
                             (frac/generate-key-between max-order nil)))]
          (crud/create-block! conn {:parent parent-uuid :content content :order final-order}))))))

(defn -create-sibling-after-handler
  "Create a sibling block after the given block.
   Looks up parent and order from the database server-side.

   This is used by keyboard handlers that can't access the database client-side.

   Args:
     block-uuid - UUID of the block to create a sibling after
     content - HTML content for the new block"
  [{:keys [block-uuid content db-scope branch-kw]}]
  (go-try S
    (let [conn (resolve-conn db-scope branch-kw)
          db @conn
          ;; Get the current block's parent and order
          block (d/pull db [:block/order {:block/parent [:entity/uuid]}]
                        [:entity/uuid block-uuid])
          parent-uuid (get-in block [:block/parent :entity/uuid])
          current-order (:block/order block)]

      (if (and parent-uuid current-order)
        ;; Find the next sibling (if any)
        (let [next-order (d/q '[:find (min ?order) .
                                :in $ ?parent-uuid ?after-order
                                :where
                                [?parent :entity/uuid ?parent-uuid]
                                [?e :block/parent ?parent]
                                [?e :block/order ?order]
                                [(> ?order ?after-order)]]
                              db parent-uuid current-order)
              ;; Generate fractional index between current and next
              new-order (frac/generate-key-between current-order next-order)]
          (crud/create-block! conn {:parent parent-uuid
                                    :content content
                                    :order new-order}))
        ;; Block not found or missing data
        (throw (ex-info "Block not found or missing parent/order"
                       {:block-uuid block-uuid
                        :block block}))))))

(defn -ensure-page-handler
  "Ensure a page exists with the given UUID. Creates it if it doesn't exist.
   When creating a new page, also creates the first block automatically."
  [{:keys [page-uuid title db-scope branch-kw]}]
  (go-try S
    (let [conn (resolve-conn db-scope branch-kw)
          db @conn
          ;; Use d/q to check existence - d/pull throws if entity doesn't exist
          exists? (seq (d/q '[:find ?e
                              :in $ ?uuid
                              :where [?e :entity/uuid ?uuid]]
                            db page-uuid))]
      (if exists?
        ;; Page exists, return its UUID
        {:status :exists :uuid page-uuid}
        ;; Page doesn't exist, create it with the specific UUID and first block
        (do
          (d/transact conn [{:entity/uuid page-uuid
                             :entity/name (str "Page " (subs (str page-uuid) 0 8))
                             :entity/created-at (java.util.Date.)
                             :entity/updated-at (java.util.Date.)
                             :instance/of-role [:entity/name "S/Page"]
                             :S.Page/title (or title "Untitled Page")}])
          ;; Retroactively add :block/references to any pre-existing
          ;; blocks that contain "[[title]]" — these were typed BEFORE
          ;; the page existed, so resolve-references returned nil for
          ;; them and they landed with no refs. Backlinks for the new
          ;; page light up immediately on the next read.
          (let [backfill (refs/backfill-tx-for-page @conn page-uuid (or title "Untitled Page"))]
            (when (seq backfill)
              (d/transact conn backfill)))
          ;; Create first block for the new page
          (crud/create-block! conn {:parent page-uuid
                                   :content ""
                                   :order (frac/generate-key-between nil nil)})
          {:status :created :uuid page-uuid})))))

(defn -update-block-content-handler [{:keys [block-uuid content db-scope branch-kw]}]
  (go-try S
    (crud/update-block! (resolve-conn db-scope branch-kw) block-uuid {:block/content content})))

(defn -rename-page-handler
  "Rename a page, keeping its inbound links coherent.

   This is an INSTRUCTION, not a transaction. The tx the rename produces is
   O(referring blocks × content size) — every `[[Old Title]]` occurrence has to
   be rewritten — while the instruction is a constant handful of bytes, and the
   server already holds the content the client would otherwise ship back.

   It also carries an invariant a raw client transaction could not enforce:
   inbound `:block/references` must follow the title (repointed on
   overwrite/merge) and the literal link text must be rewritten. See
   `crud/update-page-title!`.

   Returns {:success true :blocks-updated n}
        or {:conflict true :existing-page-uuid uuid} when another page in this
           KB already holds the title and no :overwrite?/:merge? was requested."
  [{:keys [page-uuid new-title db-scope branch-kw overwrite? merge?]}]
  (go-try S
    (crud/update-page-title! (resolve-conn db-scope branch-kw)
                             page-uuid
                             new-title
                             (cond-> {}
                               overwrite? (assoc :overwrite? true)
                               merge? (assoc :merge? true)))))

(defn -update-block-order-handler [{:keys [block-uuid new-order db-scope branch-kw]}]
  (go-try S
    (crud/update-block! (resolve-conn db-scope branch-kw) block-uuid {:block/order new-order})))

(defn -update-block-collapsed-handler [{:keys [block-uuid collapsed db-scope branch-kw]}]
  (go-try S
    (crud/update-block! (resolve-conn db-scope branch-kw) block-uuid {:block/collapsed collapsed})))

(defn -toggle-block-collapsed-handler
  "Toggle the collapsed state of a block.
   Reads current state and flips it."
  [{:keys [block-uuid db-scope branch-kw]}]
  (go-try S
    (let [conn (resolve-conn db-scope branch-kw)
          db @conn
          block (d/pull db [:block/collapsed] [:entity/uuid block-uuid])
          current-collapsed (get block :block/collapsed false)
          new-collapsed (not current-collapsed)]
      (crud/update-block! conn block-uuid {:block/collapsed new-collapsed}))))

(defn -delete-block-handler [{:keys [block-uuid cascade? db-scope branch-kw]}]
  (go-try S
    (crud/delete-block! (resolve-conn db-scope branch-kw) block-uuid {:cascade? cascade?})))

(defn -indent-block-handler [{:keys [block-uuid db-scope branch-kw]}]
  (go-try S
    (crud/indent-block! (resolve-conn db-scope branch-kw) block-uuid)))

(defn -outdent-block-handler [{:keys [block-uuid db-scope branch-kw]}]
  (go-try S
    (crud/outdent-block! (resolve-conn db-scope branch-kw) block-uuid)))

(defn -move-block-up-handler [{:keys [block-uuid db-scope branch-kw]}]
  (go-try S
    (crud/move-block-up! (resolve-conn db-scope branch-kw) block-uuid)))

(defn -move-block-down-handler [{:keys [block-uuid db-scope branch-kw]}]
  (go-try S
    (crud/move-block-down! (resolve-conn db-scope branch-kw) block-uuid)))

;; =============================================================================
;; Page TYPES and PROPERTIES
;; =============================================================================
;;
;; These were client-side transactions until 2026-07-26, and they were the last
;; ones. Two things were wrong with that, and only the first was visible:
;;
;;   1. They resolved the client's DEFAULT conn — the app store — while the page
;;      they were editing lives in a KB store. A type added to a KB page was
;;      written into a different database, where nothing reads it; the UI simply
;;      appeared to do nothing. Reaching `resolve-conn` fixes that AND makes
;;      these branch-aware, which a local-conn fix would not: a fork you are
;;      viewing is a different db value over the same store.
;;
;;   2. `add-property` declares a `:db/ident`. Schema is APPEND-ONLY in
;;      datahike, and a browser could declare attributes in any store its kabel
;;      writer could reach. The server is the only place `can?` can gate that.

(defn -add-type-handler
  "Tag `page-uuid` with the type entity `type-uuid`."
  [{:keys [page-uuid type-uuid db-scope branch-kw]}]
  (go-try S
    (let [conn (resolve-conn db-scope branch-kw)]
      (d/transact conn [{:entity/uuid page-uuid
                         :instance/of-role [:entity/uuid type-uuid]}])
      {:success true})))

(defn -remove-type-handler
  [{:keys [page-uuid type-uuid db-scope branch-kw]}]
  (go-try S
    (let [conn (resolve-conn db-scope branch-kw)]
      (d/transact conn [[:db/retract [:entity/uuid page-uuid]
                         :instance/of-role [:entity/uuid type-uuid]]])
      {:success true})))

(defn -add-property-handler
  "Declare a property on a type page: the datahike attribute AND its morphism.

   `target` is `{:type-uuid …}` for a relation, or a primitive object NAME for
   a value. The client sends the shape it wants; the server decides the
   attribute keyword and the value type, because those are schema."
  [{:keys [type-page-uuid morphism-name cardinality optional?
           property-type target-type-uuid target-object-name db-scope branch-kw]}]
  (go-try S
    (let [conn (resolve-conn db-scope branch-kw)
          ;; The morphism is being CREATED, so there is no storage-attr to
          ;; honour — a name derivation is the right answer here, and the only
          ;; possible one.
          attr (mor/name->attr-ident morphism-name)
          object-name (or target-object-name "S/String")
          target (if target-type-uuid
                   [:entity/uuid target-type-uuid]
                   [:entity/name object-name])
          ;; DERIVED here, never taken from the wire. The client used to send
          ;; `:value-type` and this handler transacted it verbatim into a
          ;; `:db/ident` declaration — and datahike schema is APPEND-ONLY, so a
          ;; browser could permanently fix the type of any attribute it could
          ;; name, in any store its writer reached. It is also the one table
          ;; that has to agree with `codomain->db-type`, which every other
          ;; reader of these attributes goes through; the client's private copy
          ;; disagreed with it about numbers.
          value-type (if target-type-uuid
                       :db.type/ref
                       (schema/codomain->db-type object-name))]
      (when-not (d/q '[:find ?e . :in $ ?n :where [?e :entity/name ?n]]
                     @conn object-name)
        ;; only checked for the value case; a relation target is a page
        (when-not target-type-uuid
          (throw (ex-info "no such value type in this store"
                          {:target object-name
                           :db-scope (str db-scope)}))))
      (d/transact conn
        [{:db/ident attr
          :db/valueType value-type
          :db/cardinality (or cardinality :db.cardinality/one)}
         {:entity/uuid (random-uuid)
          :entity/name morphism-name
          :morphism/src [:entity/uuid type-page-uuid]
          :morphism/dst target
          :morphism/cardinality (if (= :db.cardinality/many cardinality) :many :one)
          :morphism/optional? (boolean optional?)
          :morphism/property-type (or property-type :text)}])
      {:success true :attr (str attr)})))

(defn -remove-property-handler
  "Retract a property morphism AND every value stored under its attribute.

   DESTRUCTIVE, and it was not before: pointed at the app store this found
   nothing, so it looked harmless. Against the store the pages actually live in
   it deletes user data, which is why the client confirms first and why the
   count comes back — the caller can tell how much it removed."
  [{:keys [morphism-uuid morphism-name db-scope branch-kw]}]
  (go-try S
    ;; Delegates to `crud/delete-morphism!`, which knows what a REFLECTED
    ;; morphism is and drops only the categorical view, leaving the owning
    ;; system's data alone. This handler used to derive the attribute from the
    ;; NAME, so on a reflected morphism it queried an attribute that holds
    ;; nothing, retracted no values, deleted the morphism anyway and reported
    ;; `:values-removed 0`. The ledger survived by accident — the code could
    ;; not name its attribute — not by design.
    ;;
    ;; `crud`'s guard was written deliberately on 2026-07-20; this copy landed
    ;; six days later, lifted from the client-side rule by a change fixing an
    ;; unrelated wrong-database bug. The older code was the correct one.
    (let [conn (resolve-conn db-scope branch-kw)
          r (crud/delete-morphism! conn morphism-uuid)]
      (assoc r :values-removed (:affected-entities r 0)))))

(defn -save-property-handler
  "Set or clear one property value on a page.

   REFUSES a reflected property. `:morphism/storage-attr` means the values
   belong to another system — kontor's ledger, or the universal
   `:entity/created-at` — and that system has invariants a wiki text box does
   not honour (kontor's governor gates postings on balance and sealing). The
   categorical view exists so those facts can be READ through category S; it is
   not a write surface onto them.

   Until now this derived the attribute from the NAME and transacted blindly.
   Since every store is `:schema-flexibility :write` and the derived attribute
   is not declared, that threw `:transact/schema` for all 80 reflected
   morphisms on a `store/install!`ed store — including `created-at` and
   `updated-at` on S/Page, i.e. on every wiki page. The user saw \"Could not
   save that property.\" Refusing deliberately says why."
  [{:keys [page-uuid morphism-name value db-scope branch-kw]}]
  (go-try S
    (let [conn (resolve-conn db-scope branch-kw)
          morphism (d/q '[:find (pull ?m [:entity/name :morphism/storage-attr]) .
                          :in $ ?n :where [?m :entity/name ?n]]
                        @conn morphism-name)]
      (if (mor/reflected? morphism)
        {:success false
         :error :reflected-property
         :attr (str (mor/attr-of morphism))
         :message (str "\"" morphism-name "\" reflects data owned by another "
                       "system (" (mor/attr-of morphism) "). It is read here, "
                       "not edited here.")}
        (let [attr (mor/attr-of (or morphism {:entity/name morphism-name}))]
          (d/transact conn
            (if (or (nil? value) (and (string? value) (str/blank? value)))
              [[:db/retract [:entity/uuid page-uuid] attr]]
              [{:entity/uuid page-uuid attr value}]))
          {:success true})))))

(defn -find-page-by-title-handler
  "Find a page by its title. Returns the page UUID or nil if not found.

   Read-only — if the KB scope doesn't resolve to a conn (e.g. a stale
   tab pointing at a deleted/missing KB), return nil rather than
   propagating the resolve-conn throw. The caller treats nil as 'no
   such page' and the [[link]] flow naturally falls through to the
   'create new page' path."
  [{:keys [title db-scope branch-kw]}]
  (go-try S
    (try
      ;; a nil title is UNBOUND to datahike and would match an arbitrary page;
      ;; a blank one names no page either
      (when-not (str/blank? title)
        (let [conn (resolve-conn db-scope branch-kw)
              db @conn]
          (d/q '[:find ?uuid .
                 :in $ ?title
                 :where
                 [?e :S.Page/title ?title]
                 [?e :entity/uuid ?uuid]]
               db title)))
      (catch clojure.lang.ExceptionInfo e
        (when-not (= "KB not resolvable" (.getMessage e))
          (throw e))
        nil))))

;; =============================================================================
;; Auto-registration at namespace load time
;; =============================================================================

(ds/register-remote-fn! 'block-remote/create-block -create-block-handler)
(ds/register-remote-fn! 'block-remote/create-sibling-after -create-sibling-after-handler)
(ds/register-remote-fn! 'block-remote/ensure-page -ensure-page-handler)
(ds/register-remote-fn! 'block-remote/rename-page -rename-page-handler)
(ds/register-remote-fn! 'block-remote/update-block-content -update-block-content-handler)
(ds/register-remote-fn! 'block-remote/update-block-order -update-block-order-handler)
(ds/register-remote-fn! 'block-remote/update-block-collapsed -update-block-collapsed-handler)
(ds/register-remote-fn! 'block-remote/toggle-block-collapsed -toggle-block-collapsed-handler)
(ds/register-remote-fn! 'block-remote/delete-block -delete-block-handler)
(ds/register-remote-fn! 'block-remote/indent-block -indent-block-handler)
(ds/register-remote-fn! 'block-remote/outdent-block -outdent-block-handler)
(ds/register-remote-fn! 'block-remote/move-block-up -move-block-up-handler)
(ds/register-remote-fn! 'block-remote/move-block-down -move-block-down-handler)
(ds/register-remote-fn! 'block-remote/find-page-by-title -find-page-by-title-handler)
(ds/register-remote-fn! 'block-remote/add-type -add-type-handler)
(ds/register-remote-fn! 'block-remote/remove-type -remove-type-handler)
(ds/register-remote-fn! 'block-remote/add-property -add-property-handler)
(ds/register-remote-fn! 'block-remote/remove-property -remove-property-handler)
(ds/register-remote-fn! 'block-remote/save-property -save-property-handler)
