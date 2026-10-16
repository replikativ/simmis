(ns is.simm.model.crud
  "CRUD operations for the categorical schema.

   Core operations:
   - Block CRUD (everything is a block)
   - Role management (instance/of-role)
   - Morphism creation (with dynamic schema installation)
   - Property value operations (via dynamic attributes)
   - Backlinks and references"
  (:require [datahike.api :as d]
            [is.simm.model.schema :as cat]
            [is.simm.model.morphism :as mor]
            [is.simm.model.references :as refs]
            [is.simm.model.fractional-index :as frac]
            [clojure.set :as set]
            [clojure.walk :as walk]
            [taoensso.telemere :as log])
  (:import [java.util UUID Date]))

;; ============================================================================
;; Utilities
;; ============================================================================

(defn uuid []
  (UUID/randomUUID))

(defn now []
  (Date.))

;; ============================================================================
;; Role Queries
;; ============================================================================

(defn create-block!
  "Create a new block with optional roles and properties.

   Options:
   - :name - entity/name (required for top-level blocks)
   - :roles - vector of role entity-names (e.g., ['S/Block' 'S/Tag'])
   - :parent - UUID of parent block (nil for top-level)
   - :order - fractional index string for ordering (auto-generated if nil)
   - :content - HTML content
   - :properties - map of property values {attr-ident value}
                   e.g., {:S.Block/title 'My Title'}
   - :at - the instant this block is to have HAPPENED at, stamped on both the
           row and the transaction (`:db/txInstant`). Omitted → wall clock, the
           only correct answer for a live write. Supplied by installers that
           back-date a store to the beginning of its narrative time, so a cut
           taken before the store existed in wall-clock terms still finds the
           entities the store is made of — see `store/install!`.
   - :uuid - the block's `:entity/uuid`, when the caller has to know it before
             the block exists (a browser that already shows the block it asked
             for). Omitted → a fresh one. The caller is responsible for it
             naming nothing yet: `:entity/uuid` is a unique identity, so an
             existing uuid would UPSERT onto that entity.

   Returns the UUID of created block."
  [conn {:keys [name roles parent order content properties at]
         requested-uuid :uuid
         :or {roles ["S/Block"]
              content ""}}]
  (let [db @conn
        block-uuid (or requested-uuid (uuid))
        now-inst (or at (now))
        ;; Resolve role names to entity refs
        role-refs (mapv (fn [role-name] [:entity/name role-name]) roles)

        ;; Auto-calculate order if not specified (using fractional indexing)
        actual-order (if (nil? order)
                       (if parent
                         ;; Find max order among siblings and generate next fractional key
                         (let [parent-eid (d/q '[:find ?e .
                                                 :in $ ?uuid
                                                 :where
                                                 [?e :entity/uuid ?uuid]]
                                               db parent)
                               max-order (when parent-eid
                                          (d/q '[:find (max ?order) .
                                                 :in $ ?parent
                                                 :where
                                                 [?child :block/parent ?parent]
                                                 [?child :block/order ?order]]
                                               db parent-eid))]
                           ;; Generate fractional index after max-order (or first if none)
                           (frac/generate-key-between max-order nil))
                         ;; Top-level block, generate first key
                         (frac/generate-key-between nil nil))
                       ;; Order explicitly specified
                       order)

        ;; Extract references from content
        resolved-refs (when (seq content)
                       (refs/resolve-references db content))

        ;; Base block entity
        block-entity (cond-> {:entity/uuid block-uuid
                             :entity/created-at now-inst
                             :entity/updated-at now-inst
                             :instance/of-role role-refs
                             :block/content content
                             :block/order actual-order}
                       name (assoc :entity/name name)
                       parent (assoc :block/parent [:entity/uuid parent])
                       (seq resolved-refs) (assoc :block/references resolved-refs))

        ;; Merge with property values
        full-entity (merge block-entity properties)]

    (d/transact conn (cond-> {:tx-data [full-entity]}
                       at (assoc :tx-meta {:db/txInstant at})))
    (log/log! {:level :info
               :id ::create-block
               :msg "Created block"
               :data {:uuid block-uuid :name name :roles roles :refs-count (count resolved-refs)}})
    block-uuid))

(defn get-block
  "Get a block by UUID with optional pull pattern"
  ([db block-uuid]
   (get-block db block-uuid '[*]))
  ([db block-uuid pull-pattern]
   (d/pull db pull-pattern [:entity/uuid block-uuid])))

(defn list-blocks
  "List all blocks (optionally filtered by parent).

   Pulls parent as a map with :entity/uuid to avoid entity ID references."
  ([db]
   (d/q '[:find [(pull ?b [* {:block/parent [:entity/uuid]}]) ...]
          :where
          [?b :instance/of-role ?role]
          [?role :entity/name "S/Block"]]
        db))
  ([db parent-uuid]
   (d/q '[:find [(pull ?b [* {:block/parent [:entity/uuid]}]) ...]
          :in $ ?parent-uuid
          :where
          [?parent :entity/uuid ?parent-uuid]
          [?b :block/parent ?parent]]
        db parent-uuid)))

(defn update-block!
  "Update a block's properties.

   Updates map can contain any attributes, including:
   - :entity/name
   - :block/content
   - :block/order
   - :block/parent
   - Any morphism-derived attributes like :S.Block/title

   When updating :block/content, automatically extracts and updates :block/references.

   `at` is the instant the update is to have happened at — same contract as
   `create-block!`'s `:at`, and here for the same caller: an installer writing a
   store's own furniture at the beginning of narrative time rather than at the
   wall clock of whoever provisioned it.

   Returns updated block UUID."
  ([conn block-uuid updates] (update-block! conn block-uuid updates nil))
  ([conn block-uuid updates at]
  ;; If content is being updated, extract references
  (if-let [content (:block/content updates)]
    (let [db @conn
          ;; Resolve references from content
          resolved-refs (refs/resolve-references db content)
          resolved-ref-set (set resolved-refs)

          ;; Get existing reference lookup refs
          existing-ref-uuids (d/q '[:find [?uuid ...]
                                   :in $ ?block-uuid
                                   :where
                                   [?block :entity/uuid ?block-uuid]
                                   [?block :block/references ?ref]
                                   [?ref :entity/uuid ?uuid]]
                                 db block-uuid)
          existing-refs (set (mapv (fn [uuid] [:entity/uuid uuid]) existing-ref-uuids))

          ;; Only retract refs that are no longer in the new set
          refs-to-remove (set/difference existing-refs resolved-ref-set)
          retract-txs (map (fn [ref] [:db/retract [:entity/uuid block-uuid] :block/references ref])
                          refs-to-remove)

          ;; Only add refs that weren't already there
          refs-to-add (set/difference resolved-ref-set existing-refs)

          ;; @handle party-mentions — value-level handle strings (parties live in
          ;; the system DB), diffed the same way so removing an @mention retracts it.
          new-mentions (refs/extract-user-mentions content)
          existing-mentions (set (d/q '[:find [?h ...]
                                        :in $ ?block-uuid
                                        :where
                                        [?block :entity/uuid ?block-uuid]
                                        [?block :block/mentions ?h]]
                                      db block-uuid))
          mention-retracts (map (fn [h] [:db/retract [:entity/uuid block-uuid] :block/mentions h])
                                (set/difference existing-mentions new-mentions))
          mentions-to-add (set/difference new-mentions existing-mentions)

          base-tx (merge {:entity/uuid block-uuid
                         :entity/updated-at (or at (now))}
                        (dissoc updates :block/references :block/mentions))

          ;; Build transaction: base + retractions + additions
          all-txs (concat
                   [base-tx]
                   retract-txs
                   mention-retracts
                   (when (seq refs-to-add)
                     [{:entity/uuid block-uuid
                       :block/references (vec refs-to-add)}])
                   (when (seq mentions-to-add)
                     [{:entity/uuid block-uuid
                       :block/mentions (vec mentions-to-add)}]))]
      (d/transact conn (cond-> {:tx-data (filterv some? all-txs)}
                         at (assoc :tx-meta {:db/txInstant at})))
      (log/log! {:level :info
                 :id ::update-block
                 :msg "Updated block with references"
                 :data {:uuid block-uuid
                        :updates (keys updates)
                        :refs-added (count refs-to-add)
                        :refs-removed (count refs-to-remove)}})
      block-uuid)
    ;; Content not being updated - just do normal update
    (let [update-map (assoc updates
                           :entity/uuid block-uuid
                           :entity/updated-at (or at (now)))]
      (d/transact conn (cond-> {:tx-data [update-map]}
                         at (assoc :tx-meta {:db/txInstant at})))
      (log/log! {:level :info
                 :id ::update-block
                 :msg "Updated block"
                 :data {:uuid block-uuid :updates (keys updates)}})
      block-uuid))))

(defn delete-block!
  "Delete a block and optionally its children.

   Options:
   - :cascade? - if true, delete child blocks recursively (default true)

   Returns number of blocks deleted."
  [conn block-uuid {:keys [cascade?] :or {cascade? true}}]
  (let [db @conn
        children (when cascade?
                   (list-blocks db block-uuid))]
    ;; Delete children first (if cascading)
    (when cascade?
      (doseq [child children]
        (delete-block! conn (:entity/uuid child) {:cascade? true})))

    ;; Delete the block itself
    (d/transact conn [[:db/retractEntity [:entity/uuid block-uuid]]])
    (log/log! {:level :info
               :id ::delete-block
               :msg "Deleted block"
               :data {:uuid block-uuid :cascade? cascade?}})
    (inc (count children))))

;; ============================================================================
;; Morphism Operations
;; ============================================================================

(defn create-morphism!
  "Create a morphism and install its corresponding Datahike attribute.

   Options:
   - :name - entity/name (e.g., 'S/Task/status')
   - :src - source object entity-name
   - :dst - dest object entity-name
   - :category - category entity-name (default 'S')
   - :cardinality - :one or :many (default :one)
   - :property-type - keyword for UI rendering hint
   - :optional? - true if can be nil
   - :computation - EDN string for rollup/formula
   - :validation - EDN string for validation rules
   - :attr-type - the exact katzen attr-type (e.g. :Identity, :Keyword) this
     property projects from; preserves fidelity the primitive dst would collapse
   - :unique - datahike uniqueness (:db.unique/value / :db.unique/identity);
     stored on the morphism AND propagated to the generated storage attribute

   Returns the morphism UUID."
  [conn {:keys [name src dst category cardinality property-type optional? computation validation attr-type unique storage-attr]
         :or {category "S"
              cardinality :one}}]
  (let [morphism-uuid (uuid)
        morphism-entity {:entity/uuid morphism-uuid
                        :entity/name name
                        :entity/created-at (now)
                        :morphism/src [:entity/name src]
                        :morphism/dst [:entity/name dst]
                        :morphism/of-category [:entity/name category]
                        :morphism/cardinality cardinality}
        ;; Add optional fields
        morphism-entity (cond-> morphism-entity
                          property-type (assoc :morphism/property-type property-type)
                          optional? (assoc :morphism/optional? optional?)
                          computation (assoc :morphism/computation computation)
                          validation (assoc :morphism/validation validation)
                          attr-type (assoc :morphism/attr-type attr-type)
                          unique (assoc :morphism/unique unique)
                          ;; storage-attr: reuse an EXISTING external attribute
                          ;; (e.g. :kontor.posting/amount) as this property's
                          ;; storage, instead of minting a parallel :S.* one.
                          storage-attr (assoc :morphism/storage-attr storage-attr))

        ;; Generate Datahike attribute schema (carry uniqueness onto storage).
        ;; morphism->attribute-schema honors :morphism/storage-attr as the ident.
        attr-schema (cond-> (cat/morphism->attribute-schema morphism-entity dst)
                      unique (assoc :db/unique unique))
        ;; If the storage attribute already exists (the domain system installed
        ;; it — kontor's kernel schema), NEVER re-declare it: install only the
        ;; categorical metadata so the projection reuses the real attr as-is.
        reuse-existing? (and storage-attr (contains? (d/schema @conn) storage-attr))]

    ;; Transact the morphism entity (+ its attribute schema unless reusing one)
    (d/transact conn (if reuse-existing? [morphism-entity] [morphism-entity attr-schema]))
    (log/log! {:level :info
               :id ::create-morphism
               :msg "Created morphism with dynamic attribute"
               :data {:uuid morphism-uuid :name name :attr-ident (:db/ident attr-schema)}})
    morphism-uuid))

(defn delete-morphism!
  "Delete a morphism (property) from the schema.

   This will:
   1. Delete all options associated with this morphism (for select/multi-select)
   2. Retract all values using this property from instances
   3. Delete the morphism entity itself

   WARNING: This is a destructive operation. All data stored in this property
   will be permanently lost.

   Returns {:success true :deleted-options n :affected-entities n :reflected? bool}.
   `:affected-entities` is how many values were actually retracted — the UI
   confirms a destructive delete and then reports the count, so it has to be
   returned and not merely logged. For a REFLECTED morphism it is 0 by design:
   only the categorical view is dropped."
  [conn morphism-uuid]
  (let [db @conn
        ;; Get morphism details
        ;; A lookup ref for an absent entity THROWS :entity-id/missing rather
        ;; than answering nil, and a stale tab is an ordinary way to get here.
        morphism (when (d/q '[:find ?e . :in $ ?u :where [?e :entity/uuid ?u]]
                            db morphism-uuid)
                   (d/pull db '[:entity/name :morphism/property-type :morphism/storage-attr]
                           [:entity/uuid morphism-uuid]))
        _ (when-not morphism
            (throw (ex-info "no such morphism" {:type :no-such-morphism :uuid morphism-uuid})))
        morphism-name (:entity/name morphism)
        ;; A REFLECTED morphism (:morphism/storage-attr) is a VIEW onto a domain
        ;; system's own attribute (e.g. :kontor.posting/amount). Dropping the view
        ;; removes only the categorical metadata — it must NEVER retract the
        ;; underlying data, which the domain system owns.
        reflected? (some? (:morphism/storage-attr morphism))
        attr-ident (or (:morphism/storage-attr morphism)
                       (cat/morphism->attr-ident morphism-name))

        ;; Find and delete all options for this morphism
        options (when (#{:select :multi-select} (:morphism/property-type morphism))
                 (d/q '[:find [?option ...]
                        :in $ ?morphism-uuid
                        :where
                        [?option :S.Option/for-property ?morphism]
                        [?morphism :entity/uuid ?morphism-uuid]]
                      db morphism-uuid))

        ;; Find all entities that have this property set (skipped for reflected
        ;; views — we never wipe the domain system's data)
        entities-with-property (if reflected?
                                 []
                                 (d/q '[:find [?e ...]
                                        :in $ ?attr
                                        :where
                                        [?e ?attr]]
                                      db attr-ident))

        ;; Build transaction to retract all property values
        retract-values-tx (mapv (fn [eid]
                                 [:db/retract eid attr-ident])
                               entities-with-property)

        ;; Build transaction to delete all options
        delete-options-tx (mapv (fn [option-eid]
                                 [:db/retractEntity option-eid])
                               options)

        ;; Delete the morphism entity
        delete-morphism-tx [[:db/retractEntity [:entity/uuid morphism-uuid]]]]

    ;; Execute all transactions
    (d/transact conn (concat retract-values-tx delete-options-tx delete-morphism-tx))

    (log/log! {:level :info
               :id ::delete-morphism
               :msg "Deleted morphism and associated data"
               :data {:uuid morphism-uuid
                      :name morphism-name
                      :deleted-options (count options)
                      :affected-entities (count entities-with-property)}})

    {:success true
     :deleted-options (count options)
     :affected-entities (count entities-with-property)
     :reflected? reflected?}))

(defn morphism-attr
  "The datahike storage attribute for a property (morphism) `property-name`.
   Prefers an explicit `:morphism/storage-attr` — set by a namespace-preserving
   projection so the property reuses an external attribute (e.g.
   :kontor.posting/amount) — else the default S.<Object>/<prop> derivation. So a
   reflected type is read/written on the domain system's own storage."
  [db property-name]
  ;; The name→attribute half now lives in `is.simm.model.morphism`, shared with
  ;; the CLIENT read path — which had its own storage-blind copy and so
  ;; rendered every reflected property blank.
  (mor/attr-of {:entity/name property-name
                :morphism/storage-attr
                (d/q '[:find ?a . :in $ ?n
                       :where [?m :entity/name ?n] [?m :morphism/storage-attr ?a]]
                     db property-name)}))

(defn set-property-value!
  "Set a property value on a block.

   The property-name should be the morphism entity-name (e.g., 'S/Block/title').
   This function converts it to the appropriate attribute ident (:S.Block/title)
   and sets the value.

   For ref-typed properties, value should be a UUID.

   Returns the block UUID."
  [conn block-uuid property-name value]
  (let [attr-ident (morphism-attr @conn property-name)
        ;; For ref-typed values, convert UUID to lookup ref
        final-value (if (uuid? value)
                      [:entity/uuid value]
                      value)]
    (update-block! conn block-uuid {attr-ident final-value})))

(defn add-role-to-block!
  "Add a role to a block's :instance/of-role.

   role-name is the entity-name like 'S/Page' or 'S/Tag'.
   Returns the block UUID."
  [conn block-uuid role-name]
  (let [db @conn
        block (get-block db block-uuid '[:instance/of-role])
        current-roles (set (map :entity/name (:instance/of-role block)))

        ;; Only add if not already present
        _ (when (contains? current-roles role-name)
            (log/log! {:level :info
                       :id ::role-already-exists
                       :msg "Role already assigned to block"
                       :data {:block-uuid block-uuid :role role-name}}))]
    (when-not (contains? current-roles role-name)
      (d/transact conn [{:entity/uuid block-uuid
                         :instance/of-role [:entity/name role-name]
                         :entity/updated-at (now)}])
      (log/log! {:level :info
                 :id ::role-added
                 :msg "Added role to block"
                 :data {:block-uuid block-uuid :role role-name}}))
    block-uuid))

(defn remove-role-from-block!
  "Remove a role from a block's :instance/of-role.

   role-name is the entity-name like 'S/Page' or 'S/Tag'.
   Returns the block UUID."
  [conn block-uuid role-name]
  (let [db @conn
        block-eid (d/q '[:find ?e .
                        :in $ ?uuid
                        :where
                        [?e :entity/uuid ?uuid]]
                      db block-uuid)
        role-eid (d/q '[:find ?e .
                       :in $ ?name
                       :where
                       [?e :entity/name ?name]]
                     db role-name)]
    (when (and block-eid role-eid)
      (d/transact conn [[:db/retract block-eid :instance/of-role role-eid]
                        {:db/id block-eid
                         :entity/updated-at (now)}])
      (log/log! {:level :info
                 :id ::role-removed
                 :msg "Removed role from block"
                 :data {:block-uuid block-uuid :role role-name}}))
    block-uuid))

(defn create-type!
  "Create a new type (object in category S).

   Options:
   - :name - Simple name like 'Client' or 'Project' (required)
   - :primitive? - Whether this is a primitive value type (default false)

   Returns the type UUID or nil if type already exists."
  [conn {:keys [name primitive?] :or {primitive? false}}]
  (let [db @conn
        entity-name (str "S/" name)

        ;; Check if type already exists
        existing (d/q '[:find ?e .
                       :in $ ?name
                       :where
                       [?e :entity/name ?name]]
                     db entity-name)]

    (if existing
      (do
        (log/log! {:level :info
                   :id ::type-exists
                   :msg "Type already exists"
                   :data {:name entity-name}})
        nil)
      (let [type-uuid (uuid)
            type-entity {:entity/uuid type-uuid
                        :entity/name entity-name
                        :entity/created-at (now)
                        :entity/updated-at (now)
                        :object/of-category [:entity/name "S"]
                        :instance/of-role [:entity/name "S/EntityType"]  ;; Mark as EntityType
                        :object/primitive? primitive?}

            ;; Add this type to category S's objects
            category-tx {:entity/name "S"
                        :category/objects [:entity/uuid type-uuid]}]

        (d/transact conn [type-entity category-tx])
        (log/log! {:level :info
                   :id ::create-type
                   :msg "Created new type"
                   :data {:uuid type-uuid :name entity-name}})
        type-uuid))))

;; ============================================================================
;; Option Management (for Select/Multi-select Properties)
;; ============================================================================

(defn get-backlinks
  "Get all blocks that reference this block via :block/references
   (i.e., blocks that contain [[page]] or ((block-uuid)) syntax).

   Returns a map of {page-uuid -> {:page {...} :blocks [...]}}
   grouped by the page each block belongs to."
  [db block-uuid]
  (let [;; Find all blocks that reference the target via :block/references attribute
        ref-blocks (d/q '[:find [(pull ?ref-block [* {:instance/of-role [:entity/name]}]) ...]
                         :in $ ?target-uuid
                         :where
                         [?target :entity/uuid ?target-uuid]
                         [?ref-block :block/references ?target]]
                       db block-uuid)]
    ;; Group blocks by their parent page
    ;; For each block, find its root parent (the page it belongs to)
    (reduce
     (fn [acc block]
       (let [;; Walk up the parent chain to find the page (block with S/Page role)
             page-uuid (loop [current-uuid (:entity/uuid block)]
                        (let [current-block (get-block db current-uuid '[* {:instance/of-role [:entity/name]} {:block/parent [:entity/uuid]}])
                              parent-uuid (when-let [parent-ref (:block/parent current-block)]
                                           (if (map? parent-ref)
                                             (:entity/uuid parent-ref)
                                             parent-ref))
                              is-page? (some #(= "S/Page" (:entity/name %))
                                            (:instance/of-role current-block))]
                         (cond
                           is-page? current-uuid
                           parent-uuid (recur parent-uuid)
                           :else current-uuid)))
             ;; Fetch the page to get its title
             page (get-block db page-uuid '[* {:instance/of-role [:entity/name]}])
             page-with-title (if (:S.Page/title page)
                              (assoc page :page/title (:S.Page/title page))
                              page)]
         (-> acc
             (update-in [page-uuid :blocks] (fnil conj []) block)
             (assoc-in [page-uuid :page] page-with-title))))
     {}
     ref-blocks)))

;; ============================================================================
;; Search and Query
;; ============================================================================

(defn indent-block!
  "Indent a block by making it a child of its previous sibling.

   Returns true if successful, false if no previous sibling exists."
  [conn block-uuid]
  (let [db @conn
        block (get-block db block-uuid)
        parent-ref (:block/parent block)
        parent-id (:db/id parent-ref)
        position (:block/order block)

        ;; Find previous sibling (same parent, highest order less than current)
        prev-sibling (d/q '[:find (pull ?b [:entity/uuid :block/order]) .
                            :in $ ?parent ?pos ?current-uuid
                            :where
                            [?b :block/parent ?parent]
                            [?b :block/order ?order]
                            [?b :entity/uuid ?uuid]
                            [(not= ?uuid ?current-uuid)]
                            [(< ?order ?pos)]
                            ;; Get the one with max order (closest to current)
                            (not-join [?parent ?order]
                              [?other :block/parent ?parent]
                              [?other :block/order ?other-order]
                              [?other :entity/uuid ?other-uuid]
                              [(not= ?other-uuid ?current-uuid)]
                              [(< ?other-order ?pos)]
                              [(> ?other-order ?order)])]
                          db parent-id position block-uuid)]

    (if prev-sibling
      (do
        ;; Make this block a child of previous sibling
        ;; Find max order among previous sibling's children and generate fractional key after it
        (let [prev-uuid (:entity/uuid prev-sibling)
              prev-eid (d/q '[:find ?e .
                              :in $ ?uuid
                              :where
                              [?e :entity/uuid ?uuid]]
                            db prev-uuid)
              max-order (when prev-eid
                         (d/q '[:find (max ?order) .
                                :in $ ?parent
                                :where
                                [?child :block/parent ?parent]
                                [?child :block/order ?order]]
                              db prev-eid))
              ;; Generate fractional index after max-order (or first if none)
              new-order (frac/generate-key-between max-order nil)]
          (update-block! conn block-uuid {:block/parent [:entity/uuid prev-uuid]
                                          :block/order new-order})
          true))
      false)))

(defn outdent-block!
  "Outdent a block by making it a sibling of its parent.

   Returns true if successful, false if block is already top-level."
  [conn block-uuid]
  (let [db @conn
        block (get-block db block-uuid)
        parent-ref (:block/parent block)]

    (if parent-ref
      (let [parent-id (:db/id parent-ref)
            parent (d/pull db '[:entity/uuid :block/parent :block/order] parent-id)
            grandparent (:block/parent parent)
            parent-position (:block/order parent)

            ;; Find next sibling after parent (to insert between)
            grandparent-id (when grandparent (:db/id grandparent))
            ;; Note: [:find (min ?order) .] returns a scalar, not a tuple
            next-sibling-order (when grandparent-id
                                (d/q '[:find (min ?order) .
                                       :in $ ?parent ?after
                                       :where
                                       [?child :block/parent ?parent]
                                       [?child :block/order ?order]
                                       [(> ?order ?after)]]
                                     db grandparent-id parent-position))

            ;; Generate fractional index between parent and next sibling
            new-position (frac/generate-key-between parent-position next-sibling-order)]

        ;; Update this block to be sibling of parent
        ;; If grandparent is nil, we need to retract :block/parent (can't set to nil)
        (if grandparent
          (update-block! conn block-uuid {:block/parent grandparent
                                          :block/order new-position})
          ;; Retract parent attribute for top-level blocks
          (let [block-eid (d/q '[:find ?e .
                                 :in $ ?uuid
                                 :where
                                 [?e :entity/uuid ?uuid]]
                               db block-uuid)]
            (d/transact conn [[:db/retract block-eid :block/parent parent-id]
                              {:entity/uuid block-uuid
                               :block/order new-position
                               :entity/updated-at (now)}])))
        true)
      false)))

(defn move-block-up!
  "Move a block up by generating a new fractional index between the previous sibling
   and the one before it.

   Returns true if successful, false if already first."
  [conn block-uuid]
  (let [db @conn
        block (get-block db block-uuid '[:entity/uuid :block/parent :block/order])
        parent-ref (:block/parent block)
        parent-id (:db/id parent-ref)
        current-order (:block/order block)

        ;; Find previous sibling (highest order < current)
        prev-sibling (d/q '[:find (pull ?b [:entity/uuid :block/order]) .
                            :in $ ?parent ?current-order ?current-uuid
                            :where
                            [?b :block/parent ?parent]
                            [?b :block/order ?order]
                            [?b :entity/uuid ?uuid]
                            [(not= ?uuid ?current-uuid)]
                            [(< ?order ?current-order)]
                            ;; Get the max among all previous siblings
                            (not-join [?parent ?order]
                              [?other :block/parent ?parent]
                              [?other :block/order ?other-order]
                              [?other :entity/uuid ?other-uuid]
                              [(not= ?other-uuid ?current-uuid)]
                              [(< ?other-order ?current-order)]
                              [(> ?other-order ?order)])]
                          db parent-id current-order block-uuid)]

    (if prev-sibling
      (let [prev-order (:block/order prev-sibling)
            ;; Find the sibling before the previous one (to place between them)
            prev-prev-sibling (d/q '[:find (pull ?b [:block/order]) .
                                     :in $ ?parent ?prev-order ?current-uuid
                                     :where
                                     [?b :block/parent ?parent]
                                     [?b :block/order ?order]
                                     [?b :entity/uuid ?uuid]
                                     [(not= ?uuid ?current-uuid)]
                                     [(< ?order ?prev-order)]
                                     ;; Get the max
                                     (not-join [?parent ?order]
                                       [?other :block/parent ?parent]
                                       [?other :block/order ?other-order]
                                       [?other :entity/uuid ?other-uuid]
                                       [(not= ?other-uuid ?current-uuid)]
                                       [(< ?other-order ?prev-order)]
                                       [(> ?other-order ?order)])]
                                   db parent-id prev-order block-uuid)
            prev-prev-order (when prev-prev-sibling (:block/order prev-prev-sibling))
            ;; Generate new order between prev-prev and prev
            new-order (frac/generate-key-between prev-prev-order prev-order)]
        (update-block! conn block-uuid {:block/order new-order})
        true)
      ;; No previous sibling, already first
      false)))

(defn move-block-down!
  "Move a block down by generating a new fractional index between the next sibling
   and the one after it.

   Returns true if successful, false if already last."
  [conn block-uuid]
  (let [db @conn
        block (get-block db block-uuid '[:entity/uuid :block/parent :block/order])
        parent-ref (:block/parent block)
        parent-id (:db/id parent-ref)
        current-order (:block/order block)

        ;; Find next sibling (lowest order > current)
        next-sibling (d/q '[:find (pull ?b [:entity/uuid :block/order]) .
                            :in $ ?parent ?current-order ?current-uuid
                            :where
                            [?b :block/parent ?parent]
                            [?b :block/order ?order]
                            [?b :entity/uuid ?uuid]
                            [(not= ?uuid ?current-uuid)]
                            [(> ?order ?current-order)]
                            ;; Get the min among all next siblings
                            (not-join [?parent ?order]
                              [?other :block/parent ?parent]
                              [?other :block/order ?other-order]
                              [?other :entity/uuid ?other-uuid]
                              [(not= ?other-uuid ?current-uuid)]
                              [(> ?other-order ?current-order)]
                              [(< ?other-order ?order)])]
                          db parent-id current-order block-uuid)]

    (if next-sibling
      (let [next-order (:block/order next-sibling)
            ;; Find the sibling after the next one (to place between them)
            next-next-sibling (d/q '[:find (pull ?b [:block/order]) .
                                     :in $ ?parent ?next-order ?current-uuid
                                     :where
                                     [?b :block/parent ?parent]
                                     [?b :block/order ?order]
                                     [?b :entity/uuid ?uuid]
                                     [(not= ?uuid ?current-uuid)]
                                     [(> ?order ?next-order)]
                                     ;; Get the min
                                     (not-join [?parent ?order]
                                       [?other :block/parent ?parent]
                                       [?other :block/order ?other-order]
                                       [?other :entity/uuid ?other-uuid]
                                       [(not= ?other-uuid ?current-uuid)]
                                       [(> ?other-order ?next-order)]
                                       [(< ?other-order ?order)])]
                                   db parent-id next-order block-uuid)
            next-next-order (when next-next-sibling (:block/order next-next-sibling))
            ;; Generate new order between next and next-next
            new-order (frac/generate-key-between next-order next-next-order)]
        (update-block! conn block-uuid {:block/order new-order})
        true)
      ;; No next sibling, already last
      false)))

;; ============================================================================
;; Page Operations (Pages are blocks with S/Page role)
;; ============================================================================

(defn create-page!
  "Create a new page (block with S/Page role and no parent).

   Options:
   - :title - Page title (required)
   - :content - Initial content (optional)

   Returns the page UUID."
  [conn {:keys [title content]}]
  (let [page-uuid (create-block! conn
                                 {:roles ["S/Page"]
                                  :content (or content "")
                                  :properties {:S.Page/title title}})
        ;; Broadcast to all clients for incremental page list updates
        ;; Require db namespace dynamically to avoid circular dependency
        broadcast-fn (requiring-resolve 'is.simm.model.db/broadcast-pages-list-changed!)]
    (when broadcast-fn
      (broadcast-fn :created page-uuid {:entity/uuid page-uuid
                                        :S.Page/title title}))
    page-uuid))

(defn- referring-block-uuids
  "UUIDs of blocks whose `:block/references` point at `page-uuid`."
  [db page-uuid]
  (d/q '[:find [?block-uuid ...]
         :in $ ?page-uuid
         :where
         [?target :entity/uuid ?page-uuid]
         [?block :block/references ?target]
         [?block :entity/uuid ?block-uuid]]
       db page-uuid))

(defn rewrite-inbound-link-text!
  "Rewrite the literal `[[old-title]]` text of every block linking to `page-uuid`.

   Renaming a page does not break its inbound links — they are ref datoms — but
   the text a reader sees would still name the old title. Display text in
   `[[Old][Display]]` is preserved.

   Returns the number of blocks whose content changed."
  [conn page-uuid old-title new-title]
  (if-not (and (seq old-title) (not= old-title new-title))
    0
    (reduce
     (fn [n block-uuid]
       (if-let [content (d/q '[:find ?c . :in $ ?u
                               :where [?e :entity/uuid ?u] [?e :block/content ?c]]
                             @conn block-uuid)]
         (let [rewritten (refs/rename-page-references content old-title new-title)]
           (if (not= rewritten content)
             (do (update-block! conn block-uuid {:block/content rewritten})
                 (inc n))
             n))
         n))
     0
     (referring-block-uuids @conn page-uuid))))

(defn repoint-references!
  "Re-aim every `:block/references` datom from `from-uuid` to `to-uuid`.

   `[[Page]]` links are stored as ref datoms to the target ENTITY, not by title.
   So when one page absorbs another's title (overwrite/merge), the inbound links
   must be moved to the survivor before the old entity is deleted — otherwise a
   cascading delete silently takes every backlink with it."
  [conn from-uuid to-uuid]
  (let [referrers (referring-block-uuids @conn from-uuid)
        tx (mapcat (fn [block-uuid]
                     [[:db/retract [:entity/uuid block-uuid] :block/references
                       [:entity/uuid from-uuid]]
                      [:db/add [:entity/uuid block-uuid] :block/references
                       [:entity/uuid to-uuid]]])
                   referrers)]
    (when (seq tx)
      (d/transact conn (vec tx)))
    (count referrers)))

(defn update-page-title!
  "Update a page's title.

   Options:
   - :overwrite? - Delete existing page with same title
   - :merge? - Merge blocks from both pages

   Returns {:success true}, {:conflict true :existing-page-uuid uuid},
   or {:error ...}"
  [conn page-uuid new-title opts]
  (let [db @conn
        old-title (d/q '[:find ?t . :in $ ?uuid
                         :where [?e :entity/uuid ?uuid] [?e :S.Page/title ?t]]
                       db page-uuid)
        ;; Check if another page with this title exists
        existing (d/q '[:find ?uuid .
                        :in $ ?title ?exclude-uuid
                        :where
                        [?role :entity/name "S/Page"]
                        [?e :instance/of-role ?role]
                        [?e :S.Page/title ?title]
                        [?e :entity/uuid ?uuid]
                        [(not= ?uuid ?exclude-uuid)]]
                      db new-title page-uuid)]

    (cond
      ;; No conflict - just update
      (nil? existing)
      (do
        (set-property-value! conn page-uuid "S/Page/title" new-title)
        ;; The ref datoms already point here; only the literal `[[Old Title]]`
        ;; text in referring blocks would go stale.
        {:success true
         :blocks-updated (rewrite-inbound-link-text! conn page-uuid old-title new-title)})

      ;; Conflict and overwrite requested
      (:overwrite? opts)
      (do
        ;; Re-point inbound references BEFORE deleting the page they name.
        ;; `:block/references` is a ref attribute to the target entity, so a
        ;; cascading delete of the old page destroys every `[[Old Page]]` link
        ;; instead of aiming it at the page that now carries the title.
        (repoint-references! conn existing page-uuid)
        (delete-block! conn existing {:cascade? true})
        (set-property-value! conn page-uuid "S/Page/title" new-title)
        {:success true :blocks-updated 0})

      ;; Conflict and merge requested
      (:merge? opts)
      (let [existing-blocks (list-blocks db existing)
            max-order (d/q '[:find (max ?order) .
                            :in $ ?parent-uuid
                            :where
                            [?parent :entity/uuid ?parent-uuid]
                            [?child :block/parent ?parent]
                            [?child :block/order ?order]]
                          db page-uuid)]
        ;; Move all blocks from existing page to current page
        ;; Generate sequential fractional keys after max-order
        (loop [blocks existing-blocks
               prev-order max-order]
          (when-let [block (first blocks)]
            (let [new-order (frac/generate-key-between prev-order nil)]
              (update-block! conn (:entity/uuid block)
                           {:block/parent [:entity/uuid page-uuid]
                            :block/order new-order})
              (recur (rest blocks) new-order))))
        ;; Inbound `[[Source]]` links must follow the merge to the surviving page.
        (repoint-references! conn existing page-uuid)
        ;; Delete the now-empty existing page
        (delete-block! conn existing {:cascade? false})
        ;; Update title
        (set-property-value! conn page-uuid "S/Page/title" new-title)
        {:success true :blocks-updated (count existing-blocks)})

      ;; Conflict without resolution. Report WHICH page conflicts — the caller
      ;; needs it to offer overwrite/merge, and we already resolved it above.
      :else
      {:conflict true :existing-page-uuid existing})))

(comment
  ;; Example usage:
  (require '[is.simm.model.db :as db])

  ;; Create a page
  (def my-page-uuid
    (create-page! (db/get-conn) {:title "My First Page"}))

  ;; Create a block
  (def my-block-uuid
    (create-block! (db/get-conn)
                   {:roles ["S/Block"]
                    :parent my-page-uuid
                    :order 0
                    :content "<p>Content here</p>"}))

  ;; Read the page's blocks
  (list-blocks @(db/get-conn) {:parent my-page-uuid})

  ;; Move block
  (move-block-down! (db/get-conn) my-block-uuid)

  ;; Indent block
  (indent-block! (db/get-conn) my-block-uuid)
  )
