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
            [is.simm.model.page-kind :as page-kind]
            [is.simm.model.db :as db]
            [is.simm.model.knowledge-bases :as kbs]
            [is.simm.model.fractional-index :as frac]
            [is.simm.model.references :as refs]
            [is.simm.model.schema :as schema]
            [is.simm.model.attributes :as attributes]
            [is.simm.runtimes.branching :as branching]
            [is.simm.model.access :as access]
            [datahike.api :as d]
            [superv.async :refer [go-try S]]
            [taoensso.telemere :as log]))

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
;; What a browser needs back from a write
;; =============================================================================
;;
;; The browser shows its own writes at once, as a datahike.optimistic
;; PREDICTION, and this server is what makes them durable. It drops the
;; prediction once its replica has caught up with the write, which it can only
;; tell by a head: `:max-tx` of the store after the write. Every handler the
;; browser predicts for returns one. See `optimistic-write`.

(defn- head
  "The store's head after a write: what the browser's replica must reach
   before it holds that write."
  [conn]
  (:max-tx @conn))

(defn- unused-uuid
  "`u`, a uuid the browser chose for an entity it is about to create — after
   checking it names nothing yet. `:entity/uuid` is a unique identity: a uuid
   that already exists would turn the create into an UPSERT onto that entity
   (a page, someone else's block), not a new block."
  [db u]
  (when-not (uuid? u)
    (throw (ex-info "Not a uuid" {:type :invalid-argument :value (pr-str u)})))
  (when (d/q '[:find ?e . :in $ ?u :where [?e :entity/uuid ?u]] db u)
    (throw (ex-info "That uuid already names an entity"
                    {:type :invalid-argument :uuid (str u)})))
  u)

(defn- existing-block
  "`u`, after checking it names an entity. An UPDATE of a uuid that names
   nothing would create one — `:entity/uuid` is a unique identity, so the
   transaction upserts — and a late save of a block deleted a moment earlier
   would bring back a parentless fragment of it."
  [db u]
  (when-not (d/q '[:find ?e . :in $ ?u :where [?e :entity/uuid ?u]] db u)
    (throw (ex-info "No such block" {:type :not-found :block-uuid (str u)})))
  u)

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
     new-uuid - Optional: the new block's uuid, chosen by a browser that
                already shows it (must name nothing yet)

   If `insert-after-order` is provided, generates a key between it and the next sibling.
   Otherwise, appends at the end using fractional indexing.

   Returns {:block-uuid <the new block> :max-tx <head after the write>}."
  [{:keys [parent-uuid content order insert-after-order new-uuid db-scope branch-kw]}]
  (go-try S
    (let [conn (resolve-conn db-scope branch-kw)
          db @conn
          with-uuid (fn [opts] (cond-> opts
                                 new-uuid (assoc :uuid (unused-uuid db new-uuid))))
          created
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
          (crud/create-block! conn (with-uuid {:parent parent-uuid
                                               :content content
                                               :order new-order})))

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
          (crud/create-block! conn (with-uuid {:parent parent-uuid :content content
                                               :order final-order}))))]
      {:block-uuid created :max-tx (head conn)})))

(defn -create-sibling-after-handler
  "Create a sibling block after the given block.
   Looks up parent and order from the database server-side.

   This is used by keyboard handlers that can't access the database client-side.

   Args:
     block-uuid - UUID of the block to create a sibling after
     content - HTML content for the new block
     new-uuid - optional: the new block's uuid, chosen by a browser that
                already shows it (must name nothing yet)

   Returns {:block-uuid <the new block> :max-tx <head after the write>}."
  [{:keys [block-uuid content new-uuid db-scope branch-kw]}]
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
              new-order (frac/generate-key-between current-order next-order)
              created (crud/create-block! conn (cond-> {:parent parent-uuid
                                                        :content content
                                                        :order new-order}
                                                 new-uuid (assoc :uuid (unused-uuid db new-uuid))))]
          {:block-uuid created :max-tx (head conn)})
        ;; Block not found or missing data
        (throw (ex-info "Block not found or missing parent/order"
                       {:block-uuid block-uuid
                        :block block}))))))

(defn -ensure-page-handler
  "Ensure a page exists with the given UUID. Creates it if it doesn't exist.
   When creating a new page, also creates the first block automatically —
   under `first-block-uuid` when the browser chose one (it already shows that
   block, and focuses it).

   Returns {:status :exists|:created :uuid page-uuid :max-tx head}."
  [{:keys [page-uuid title first-block-uuid db-scope branch-kw]}]
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
        {:status :exists :uuid page-uuid :max-tx (head conn)}
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
          (crud/create-block! conn (cond-> {:parent page-uuid
                                            :content ""
                                            :order (frac/generate-key-between nil nil)}
                                     first-block-uuid
                                     (assoc :uuid (unused-uuid @conn first-block-uuid))))
          {:status :created :uuid page-uuid :max-tx (head conn)})))))

(defn -update-block-content-handler
  "Returns {:block-uuid … :max-tx <head after the write>}."
  [{:keys [block-uuid content db-scope branch-kw]}]
  (go-try S
    (let [conn (resolve-conn db-scope branch-kw)]
      (crud/update-block! conn (existing-block @conn block-uuid) {:block/content content})
      {:block-uuid block-uuid :max-tx (head conn)})))

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

(defn -update-block-collapsed-handler
  "Returns {:block-uuid … :max-tx <head after the write>}."
  [{:keys [block-uuid collapsed db-scope branch-kw]}]
  (go-try S
    (let [conn (resolve-conn db-scope branch-kw)]
      (crud/update-block! conn (existing-block @conn block-uuid) {:block/collapsed collapsed})
      {:block-uuid block-uuid :max-tx (head conn)})))

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

(defn -delete-block-handler
  "Returns {:deleted <blocks removed> :max-tx <head after the write>}."
  [{:keys [block-uuid cascade? db-scope branch-kw]}]
  (go-try S
    (let [conn (resolve-conn db-scope branch-kw)
          n (crud/delete-block! conn block-uuid {:cascade? cascade?})]
      {:deleted n :max-tx (head conn)})))

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
   save that property.\" Refusing deliberately says why.

   REFUSES a value of the wrong type, with the reason. A keyword attribute
   (`:S.Page/kind`) edited as text arrived as a string, and the transaction
   failed with nothing the user could act on. And a page's kind changes only
   between the kinds a person may set (`page-kind/change-refusal`): the
   system's kinds hide pages."
  [{:keys [page-uuid morphism-name value db-scope branch-kw]}]
  (go-try S
    (let [conn (resolve-conn db-scope branch-kw)
          db @conn
          morphism (d/q '[:find (pull ?m [:entity/name :morphism/storage-attr]) .
                          :in $ ?n :where [?m :entity/name ?n]]
                        db morphism-name)
          attr (mor/attr-of (or morphism {:entity/name morphism-name}))
          clear? (or (nil? value) (and (string? value) (str/blank? value)))
          value-type (get-in (d/schema db) [attr :db/valueType])
          refusal (cond
                    (mor/reflected? morphism)
                    {:error :reflected-property
                     :message (str "\"" morphism-name "\" reflects data owned by another "
                                   "system (" attr "). It is read here, not edited here.")}

                    (and (not clear?) value-type
                         (not (page-kind/value-fits? value-type value)))
                    {:error :wrong-value-type
                     :message (str "\"" morphism-name "\" holds a " (name value-type)
                                   "; " (pr-str value) " is not one.")}

                    (= page-kind/attr attr)
                    (when-let [why (page-kind/change-refusal
                                    (d/q '[:find ?k . :in $ ?p
                                           :where [?e :entity/uuid ?p] [?e :S.Page/kind ?k]]
                                         db page-uuid)
                                    (when-not clear? value))]
                      {:error :system-page-kind :message why}))]
      (if refusal
        (assoc refusal :success false :attr (str attr))
        (do
          (d/transact conn
            (if clear?
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
;; Widget writes
;; =============================================================================
;;
;; A wiki widget (`widget_sandbox.cljs`) writes the KB its page lives in. It
;; used to `d/transact!` its replica conn, whose KabelWriter waits forever for
;; a `kabel.remote` route simmis does not serve — nothing was sent, nothing
;; failed, and the write was gone after a reload. These two handlers are the
;; widget's only writes, gated like every other one (`access/rpc-policy`):
;;
;;   widget-transact      DATA, `:write` on the KB (`{:kb-store …}` — never a
;;                        room store). Schema in it is refused.
;;   widget-install-attr  SCHEMA, `:merge` on the KB (its owner). The client
;;                        names the attribute and its CODOMAIN object; the
;;                        value type is derived here (`doc/data-model.md`,
;;                        "Schema types"), never taken from the wire.
;;
;; Both write the trunk: a widget is only handed writes on a trunk view.

(def ^:private widget-ops
  "The transaction operations a widget may send. No tx functions (`:db.fn/call`
   runs code), no purge, no compare-and-swap of schema."
  #{:db/add :db/retract :db/retractEntity :db.fn/retractEntity})

(def ^:private max-widget-ops 500)

(defn- reserved-attr?
  "An attribute a widget's data may not name: datahike's own (`:db/*`,
   `:db.*/*` — schema and installation), the transaction's provenance (`:tx/*`),
   and anything unqualified."
  [a]
  (or (not (keyword? a))
      (let [n (namespace a)]
        (or (nil? n) (= "db" n) (str/starts-with? n "db.") (= "tx" n)))))

(defn- tx-entity? [e]
  (contains? #{"datomic.tx" "datahike.tx" :db/current-tx} e))

(defn- invalid! [msg data]
  (throw (ex-info msg (assoc data :type :validation-error))))

(defn- schema-refused! [msg data]
  (log/log! {:level :warn :id ::widget-schema-refused :msg msg :data data})
  (throw (ex-info (str "not authorized: " msg) (assoc data :type :not-authorized))))

(defn- check-widget-entity-map! [m]
  (doseq [[k v] m]
    (cond
      (= :db/id k) (when (tx-entity? v)
                     (schema-refused! "a widget may not write the transaction entity" {}))
      (reserved-attr? k) (schema-refused! "schema is not data: declare attributes with kb/install-attr!"
                                          {:attr (str k)})
      :else (doseq [x (if (and (coll? v) (not (map? v))) v [v])
                    :when (map? x)]
              (check-widget-entity-map! x)))))

(defn- check-widget-tx-shape!
  "Refuse, before anything runs, what a widget's data transaction may not
   carry. The shape check names the problem; `check-widget-effect!` below is
   the one that cannot be talked around."
  [tx-data]
  (when-not (and (sequential? tx-data) (seq tx-data))
    (invalid! "a transaction is a non-empty vector" {}))
  (when (> (count tx-data) max-widget-ops)
    (invalid! "too many operations in one widget transaction" {:max max-widget-ops}))
  (doseq [op tx-data]
    (cond
      (map? op) (check-widget-entity-map! op)
      (sequential? op)
      (let [[f e a] op]
        (when-not (contains? widget-ops f)
          (invalid! "unsupported transaction operation" {:op (pr-str f)}))
        (when (tx-entity? e)
          (schema-refused! "a widget may not write the transaction entity" {}))
        (when (and (#{:db/add :db/retract} f) (reserved-attr? a))
          (schema-refused! "schema is not data: declare attributes with kb/install-attr!"
                           {:attr (pr-str a)})))
      :else (invalid! "unsupported transaction element" {:op (pr-str op)}))))

(defn- check-widget-effect!
  "Refuse a transaction by what it WOULD do, computed with `d/with` on the
   current db: every datom must be data — an attribute outside datahike's own
   namespaces, on an entity that is not a schema entity — except the
   transaction entity's own `:db/txInstant` and the `:tx/author` this handler
   stamps. This is what makes a `:db/retractEntity` of an attribute, or a
   schema change smuggled through a lookup ref, a refusal rather than a
   permanent change to the KB."
  [db tx-data party]
  (let [report (d/with db {:tx-data tx-data :tx-meta {:tx/author (str party)}})
        tx (:max-tx (:db-after report))]
    (doseq [[e a] (map (juxt :e :a) (:tx-data report))]
      (if (= e tx)
        (when-not (#{:db/txInstant :tx/author} a)
          (schema-refused! "a widget may not write the transaction entity" {:attr (str a)}))
        (when (or (reserved-attr? a) (:db/ident (d/entity db e)))
          (schema-refused! "schema is not data: declare attributes with kb/install-attr!"
                           {:attr (str a)}))))))

(defn -widget-transact-handler
  "A widget's data transaction on the trunk of KB `db-scope`, as the caller.
   Returns `{:max-tx head}` (see `optimistic-write`)."
  [{:keys [db-scope tx-data]}]
  ;; the principal is bound for the invoke only: read it before the go block
  (let [party (access/authenticated-party-id)]
   (go-try S
    (let [conn (resolve-conn db-scope)]
      (check-widget-tx-shape! tx-data)
      (check-widget-effect! @conn tx-data party)
      (d/transact conn {:tx-data (vec tx-data) :tx-meta {:tx/author (str party)}})
      {:max-tx (head conn)}))))

(defn -widget-install-attr-handler
  "Declare attribute `ident` on KB `db-scope`, with the value type its CODOMAIN
   object determines, plus the morphism describing it (`attributes/declaration`,
   shared with an agent's `kb/install-attr!`).

   `codomain` is an object NAME in the store (`\"S/String\"`, `\"S/Number\"`,
   `\"S/Page\"` for a reference). A `:db.type/*` keyword is refused: the client
   never chooses a value type (`doc/data-model.md`) — schema is append-only,
   so a client that could choose one could fix an attribute's type for good.
   Idempotent: an `ident` that exists is answered `:exists` and not touched."
  [{:keys [db-scope ident codomain cardinality]}]
  (let [party (access/authenticated-party-id)]
   (go-try S
    (let [conn (resolve-conn db-scope)
          {:keys [status value-type tx-data]}
          (attributes/declaration @conn {:ident ident :codomain codomain
                                         :cardinality cardinality})]
      (if (= :exists status)
        {:status :exists :ident ident :value-type value-type :max-tx (head conn)}
        (do (d/transact conn {:tx-data tx-data :tx-meta {:tx/author (str party)}})
            {:status :installed :ident ident :value-type value-type :max-tx (head conn)}))))))

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
(ds/register-remote-fn! 'block-remote/widget-transact -widget-transact-handler)
(ds/register-remote-fn! 'block-remote/widget-install-attr -widget-install-attr-handler)
