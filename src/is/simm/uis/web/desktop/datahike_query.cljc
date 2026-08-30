(ns is.simm.uis.web.desktop.datahike-query
  "Reactive Datahike queries for spindel integration.

   This module bridges Datahike queries with spindel's reactive system:
   - Queries run on the local-db signal (synced via konserve-sync)
   - Query results are diffed to produce spindel-compatible deltas
   - Intervals are produced for use with ifor-each and other combinators

   Usage in a spin:
     (spin
       (let [db-iv (track local-db)
             blocks-iv (query-with-deltas db-iv blocks-query :block/uuid)]
         (ifor-each :block/uuid blocks-iv
           (fn [block] (render-block block)))))

   The query-with-deltas function:
   1. Extracts the db from the interval
   2. Runs the query
   3. Diffs against cached previous result
   4. Returns new interval with query-level deltas"
  (:require [org.replikativ.spindel.incremental.interval :as iv]
            [clojure.set :as set]
            [clojure.string :as str]
            [is.simm.model.morphism :as mor]
            #?(:cljs [is.simm.uis.web.desktop.db-signal :as db-signal])
            #?(:cljs [datahike.api :as d])))

;; =============================================================================
;; Query Result Diffing
;; =============================================================================

(defn diff-by-key
  "Diff two collections by key function.
   Returns {:added [...] :removed [...] :updated [{:key k :old o :new n} ...]}"
  [old-coll new-coll key-fn]
  (let [old-by-key (into {} (map (juxt key-fn identity)) (or old-coll []))
        new-by-key (into {} (map (juxt key-fn identity)) (or new-coll []))
        old-keys (set (keys old-by-key))
        new-keys (set (keys new-by-key))
        added-keys (set/difference new-keys old-keys)
        removed-keys (set/difference old-keys new-keys)
        common-keys (set/intersection old-keys new-keys)
        updated (for [k common-keys
                      :let [old-item (old-by-key k)
                            new-item (new-by-key k)]
                      :when (not= old-item new-item)]
                  {:key k :old old-item :new new-item})]
    {:added (mapv new-by-key added-keys)
     :removed (mapv old-by-key removed-keys)
     :updated (vec updated)}))

(defn diff->spindel-deltas
  "Convert query diff to spindel delta format for ifor-each.

   Spindel ifor-each expects:
   - {:delta :add :value item}
   - {:delta :remove :old-value item}
   - {:delta :update :value new :old-value old}"
  [diff]
  (concat
   (for [item (:added diff)]
     {:delta :add :value item})
   (for [item (:removed diff)]
     {:delta :remove :old-value item})
   (for [{:keys [old new]} (:updated diff)]
     {:delta :update :old-value old :value new})))

;; =============================================================================
;; Reactive Query State (per-query cache)
;; =============================================================================

;; Cache of previous query results keyed by query-id
;; Structure: {query-id {:result [...] :db-identity ...}}
(defonce ^:private query-cache (atom {}))

(defn- get-cache-entry
  "Get cached query entry for a query-id.
   Returns {:result [...] :db-identity ...} or nil."
  [query-id]
  (get @query-cache query-id))

(defn- set-cached-result!
  "Cache a query result for a query-id."
  [query-id result db-identity]
  (swap! query-cache assoc query-id {:result result :db-identity db-identity}))

(defn- set-cache-entry!
  "Store an arbitrary cache entry map (for callers that carry more than
   :result/:db-identity, e.g. the window of a windowed query)."
  [query-id entry]
  (swap! query-cache assoc query-id entry))

(defn clear-cache!
  "Clear all cached query results. Call this between tests."
  []
  (reset! query-cache {}))

(defn evict-cache-for-entity!
  "Drop every cached query whose key mentions `entity-uuid` (a page-uuid
   or room-uuid). Called on tab close so the full-timeline vector and the
   per-(page,scope,ref) block entries don't accumulate for the session."
  [entity-uuid]
  (swap! query-cache
         (fn [m]
           (into {} (remove (fn [[k _]]
                              (and (vector? k) (some #(= % entity-uuid) k)))
                            m)))))

;; =============================================================================
;; Query Execution with Delta Computation
;; =============================================================================

(defn query-with-deltas
  "Execute a query on the database from an interval, producing deltas.

   Args:
     db-interval - Interval from (track local-db), contains :new = db instance
     query-fn    - Function (db) -> results, executes the Datahike query
     key-fn      - Function to extract unique key from each result item
     query-id    - Keyword identifying this query (for caching)

   Returns: Interval with
     :old    - previous query result (or nil)
     :new    - current query result
     :deltas - spindel deltas [{:delta :add/:remove/:update ...}]

   The returned interval is suitable for ifor-each:
     (ifor-each key-fn (query-with-deltas db-iv query-fn key-fn :my-query)
       (fn [item] ...))"
  [db-interval query-fn key-fn query-id]
  (let [;; Extract db from interval
        db (iv/get-new db-interval)
        db-identity (when db (hash db))  ; Use hash as identity for change detection

        ;; Get cached entry (contains :result and :db-identity)
        cache-entry (get-cache-entry query-id)
        old-result (:result cache-entry)
        old-db-identity (:db-identity cache-entry)

        ;; Check if db actually changed (optimization)
        db-changed? (or (nil? old-db-identity)
                        (not= old-db-identity db-identity))]

    (if (and db db-changed?)
      ;; DB changed - run query and compute diff
      (let [new-result (query-fn db)
            diff (diff-by-key old-result new-result key-fn)
            deltas (vec (diff->spindel-deltas diff))]

        ;; Cache new result
        (set-cached-result! query-id new-result db-identity)

        ;; Return interval with deltas. When the diff was empty AND
        ;; new-result = old-result, `deltas` is `[]` and `:old = :new`,
        ;; which spindel recognises as `no-change?` — downstream
        ;; combinators will short-circuit. Otherwise deltas describes
        ;; the actual diff.
        (iv/->Interval old-result new-result deltas))

      ;; DB unchanged (or nil). We verified the db hash matches, so
      ;; the cached result is still correct. Emit the canonical
      ;; verified-no-change interval: `:old = :new`, `:deltas []`.
      ;; spindel's `no-change?` recognises this and downstream
      ;; consumers skip all work.
      (iv/->Interval old-result old-result []))))

;; =============================================================================
;; Common Block Queries
;; =============================================================================

#?(:cljs
   (defn all-blocks-query
     "Query all blocks with parent reference."
     [db]
     (when db
       (d/q '[:find [(pull ?b [:block/uuid :block/content :block/order
                               {:block/parent [:block/uuid]}]) ...]
              :where [?b :block/uuid]]
            db))))

#?(:cljs
   (defn page-blocks-query
     "Create a query for blocks under a specific page.
      Returns a query function that can be passed to query-with-deltas."
     [page-uuid]
     (fn [db]
       (when db
         (d/q '[:find [(pull ?b [:block/uuid :block/content :block/order
                                 {:block/parent [:block/uuid]}]) ...]
                :in $ ?page-uuid
                :where
                [?page :block/uuid ?page-uuid]
                [?b :block/parent ?page]]
              db page-uuid)))))

;; =============================================================================
;; Tree Flattening for Document Order
;; =============================================================================

(defn build-children-index
  "Build index of parent-uuid -> [children] for tree traversal."
  [blocks]
  (group-by #(get-in % [:block/parent :block/uuid]) blocks))

(defn flatten-to-document-order
  "Flatten blocks to document order via DFS traversal.

   Args:
     blocks - Collection of blocks with :block/parent and :block/order
     root-uuid - UUID of the root (page), children of this are top-level

   Returns: Vector of blocks in document order (depth-first)"
  [blocks root-uuid]
  (let [by-parent (build-children-index blocks)]
    (letfn [(flatten-tree [parent-uuid]
              (let [children (->> (get by-parent parent-uuid [])
                                  (sort-by :block/order))]
                (mapcat (fn [block]
                          (cons block (flatten-tree (:block/uuid block))))
                        children)))]
      (vec (flatten-tree root-uuid)))))

(defn blocks-in-document-order
  "Transform blocks interval to document-order interval.

   Takes the result of query-with-deltas and flattens to document order.
   Note: This recomputes order from scratch - deltas are preserved
   but may need reindexing if order changes significantly.

   For truly incremental tree updates, consider tracking structural
   changes separately from content changes."
  [blocks-interval root-uuid key-fn]
  (let [old-blocks (iv/get-old blocks-interval)
        new-blocks (iv/get-new blocks-interval)

        old-ordered (when old-blocks (flatten-to-document-order old-blocks root-uuid))
        new-ordered (when new-blocks (flatten-to-document-order new-blocks root-uuid))

        ;; Recompute diff on ordered collections
        ;; This handles cases where tree structure changed
        diff (diff-by-key old-ordered new-ordered key-fn)
        deltas (vec (diff->spindel-deltas diff))]

    (iv/->Interval old-ordered new-ordered deltas)))

;; =============================================================================
;; Convenience: Combined Query + Flatten
;; =============================================================================

#?(:cljs
   (defn page-blocks-ordered
     "Query and flatten page blocks to document order.

      Args:
        db-interval - Interval from (track local-db)
        page-uuid   - UUID of the page to query

      Returns: Interval of blocks in document order with deltas"
     [db-interval page-uuid]
     (let [blocks-iv (query-with-deltas
                       db-interval
                       (page-blocks-query page-uuid)
                       :block/uuid
                       [:page-blocks page-uuid])]
       (blocks-in-document-order blocks-iv page-uuid :block/uuid))))

;; =============================================================================
;; Convenience: make-reactive-query for spin integration
;; =============================================================================

#?(:cljs
   (defn make-reactive-query
     "Create a reactive query function for use inside a spin.

      This creates a function that, when called with a db-interval (from track),
      executes the query and returns an interval with deltas suitable for ifor-each.

      Args:
        query-fn - Function (db) -> results, executes the Datahike query
        key-fn   - Function to extract unique key from each result item
        query-id - Unique keyword identifying this query (for caching)

      Returns: Function (db-interval) -> result-interval

      Usage:
        (def pages-query
          (make-reactive-query
            (fn [db] (d/q '[:find ...] db))
            :entity/uuid
            :pages))

        ;; In a spin:
        (spin
          (let [db-iv (track db-signal/local-db)
                pages-iv (pages-query db-iv)]
            (ifor-each :entity/uuid pages-iv
              (fn [page] (render-page page)))))"
     [query-fn key-fn query-id]
     (fn [db-interval]
       (query-with-deltas db-interval query-fn key-fn query-id))))

(def page-role-query
  "The `S/Page` role entity, asked for as a VALUE.

   Portable data rather than a cljs function body so the invariant below can be
   tested on the JVM against a real store — the trap it avoids is a datahike
   one, not a browser one."
  '[:find ?r . :where [?r :entity/name "S/Page"]])

(def pages-by-role-query
  "Pages, with the role passed IN. The lookup-ref form of the same query is the
   bug; see `page-role-eid`."
  '[:find [(pull ?e [:entity/uuid :S.Page/title]) ...]
    :in $ ?role
    :where [?e :instance/of-role ?role]])

#?(:cljs
   (defn page-role-eid
     "The eid of the `S/Page` role entity in `db`, or nil when this database does
      not have one yet.

      A VALUE resolved up front rather than a lookup ref written into a query,
      and the difference is the whole point. A lookup ref that misses is an
      ERROR in datahike — `[?e :instance/of-role [:entity/name \"S/Page\"]]`
      against a database where the seed entity is absent throws `Nothing found
      for entity id [:entity/name \"S/Page\"]` (measured 2026-07-27) — whereas a
      query over a bound eid simply matches nothing.

      A database WITHOUT the role is reachable and ordinary: `d/as-of` at a cut
      older than the store's install. Seeded workspaces install at the beginning
      of narrative time so their own cuts land after this (store/install!), but
      that only covers stores we seed — any store whose history predates a later
      schema addition is in the same position, and a time rail that throws when
      you scrub too far is worse than one that says the wiki was empty then."
     [db]
     (when db
       (d/q page-role-query db))))

#?(:cljs
   (defn all-pages-query
     "Query all pages. Returns a function to use with make-reactive-query or
      query-with-deltas.

      Empty — never an exception — at a cut older than the store's vocabulary;
      see `page-role-eid`."
     [db]
     (if-let [role (page-role-eid db)]
       (d/q pages-by-role-query db role)
       [])))

;; =============================================================================
;; Chat Message Queries (using categorical schema)
;; =============================================================================

(defn- try-parse-uuid [x]
  (when x
    (try
      #?(:clj (java.util.UUID/fromString (str x))
         :cljs (uuid (str x)))
      (catch #?(:clj Throwable :cljs :default) _ nil))))

(defn actor->party-uuid
  "Resolve Simmis's canonical dvergr actor id to its party UUID. Bare UUID
   keywords are accepted for old room-store rows; reserved/system actors are
   deliberately not invented into parties."
  [actor]
  (when (and (keyword? actor)
             (or (= "party" (namespace actor)) (nil? (namespace actor))))
    (try-parse-uuid (name actor))))

(defn merge-message-projections
  "Project one canonical dvergr message into the temporary S.Message-shaped UI
   contract. `legacy` is optional enrichment written by the old projector;
   `users` maps party UUIDs to {:name :is-ai}.

   Canonical identity, timestamp, author, reasoning and causality win. Legacy
   display content wins temporarily because the projector resolves bare wiki
   links to their cross-store dh:// targets. Once link resolution moves to the
   canonical render path this exception can disappear with S.Message itself."
  [canonical legacy users]
  (let [{:keys [id content sent-at from source-user reasoning metadata
                to in-reply-to thread-root-id run-id activities]} canonical
        metadata (or metadata {})
        author-uuid (or (actor->party-uuid from)
                        (try-parse-uuid source-user)
                        (:S.Message/author-uuid legacy))
        user (get users author-uuid)
        attachment (:attachment metadata)
        attachment-blob (or (some-> (:blob-id attachment) str)
                            (:S.Message/attachment-blob legacy))
        attachment-mime (or (:mime attachment)
                            (:S.Message/attachment-mime legacy))]
    (cond-> (merge legacy
                   {:entity/uuid id
                    :block/content (or (:block/content legacy) content)
                    :S.Message/sent-at sent-at
                    :S.Message/author-uuid author-uuid
                    :S.Message/author-name (or (:name user)
                                               (:S.Message/author-name legacy)
                                               (:source-username metadata)
                                               source-user
                                               (some-> from name))
                    :S.Message/is-ai (boolean (or (:is-ai user)
                                                  (:S.Message/is-ai legacy)))
                    :timeline/type :message
                    :timeline/ts sent-at})
      reasoning (assoc :S.Message/reasoning reasoning)
      attachment-blob (assoc :S.Message/attachment-blob attachment-blob
                             :S.Message/attachment-mime
                             (or attachment-mime "application/octet-stream"))
      to (assoc :message/to to)
      in-reply-to (assoc :message/in-reply-to in-reply-to)
      thread-root-id (assoc :message/thread-root-id thread-root-id)
      run-id (assoc :message/run-id run-id)
      (seq activities) (assoc :message/activities activities)
      (seq metadata) (assoc :message/metadata metadata)
      (seq (:audience metadata)) (assoc :message/audience (:audience metadata))
      (seq (:mentions metadata)) (assoc :message/mention-handles (:mentions metadata)))))

(defn canonical-message-entity->message
  "Rebuild dvergr's runtime message envelope from a typed Datahike pull.

   Keeping this conversion independent of Datahike makes the storage boundary
   explicit and testable on both the JVM and the browser."
  [m]
  (let [attachment (cond-> {}
                     (or (:message/attachment-store-ref m)
                         (:message/attachment-blob-id m))
                     (assoc :blob-id
                            (or (:message/attachment-store-ref m)
                                (:message/attachment-blob-id m)))
                     (:message/attachment-node-id m)
                     (assoc :node-id (:message/attachment-node-id m))
                     (:message/attachment-mime m)
                     (assoc :mime (:message/attachment-mime m))
                     (:message/attachment-name m)
                     (assoc :name (:message/attachment-name m))
                     (:message/attachment-size m)
                     (assoc :size (:message/attachment-size m)))
        provenance (cond-> {}
                     (:message/provenance-mode m)
                     (assoc :mode (:message/provenance-mode m))
                     (:message/provenance-source m)
                     (assoc :source (:message/provenance-source m)))
        object (when (and (:message/object-kind m)
                          (:message/object-id m))
                 {:kind (:message/object-kind m)
                  :id (:message/object-id m)})
        metadata (cond-> {:role (:message/role m)}
                   (:message/run-id m)
                   (assoc :run-id (:message/run-id m))
                   (:message/source-user m)
                   (assoc :source-user (:message/source-user m))
                   (:message/source-username m)
                   (assoc :source-username (:message/source-username m))
                   (:message/source-user-id m)
                   (assoc :source-user-id (:message/source-user-id m))
                   (seq (:message/audience m))
                   (assoc :audience (set (:message/audience m)))
                   (seq (:message/mention-handles m))
                   (assoc :mentions (set (:message/mention-handles m)))
                   (:message/metadata-kind m)
                   (assoc :kind (:message/metadata-kind m))
                   (:message/context-from m)
                   (assoc :from (:message/context-from m))
                   (:message/source m)
                   (assoc :source (:message/source m))
                   (:message/schedule-id m)
                   (assoc :schedule-id (:message/schedule-id m))
                   (seq attachment)
                   (assoc :attachment attachment)
                   (seq provenance)
                   (assoc :provenance provenance)
                   object
                   (assoc :object object)
                   (:message/notification-type m)
                   (assoc :notification/type (:message/notification-type m))
                   (:message/notification-agent m)
                   (assoc :notification/agent (:message/notification-agent m))
                   (:message/notification-task m)
                   (assoc :notification/task (:message/notification-task m))
                   (:message/notification-elapsed m)
                   (assoc :notification/elapsed
                          (:message/notification-elapsed m))
                   (seq (:message/activities m))
                   (assoc :activities (:message/activities m)))]
    {:id (:message/id m)
     :content (:message/content m)
     :sent-at (:message/created-at m)
     :role (:message/role m)
     :from (:message/from m)
     :to (:message/to m)
     :in-reply-to (:message/in-reply-to m)
     :thread-root-id (:message/thread-root-id m)
     :run-id (:message/run-id m)
     :activities (vec (:message/activities m))
     :metadata metadata
     :reasoning (:message/reasoning m)
     :source-user (:message/source-user m)}))

#?(:cljs
   (defn- room-message-users [db]
     (let [names (into {} (d/q '[:find ?uuid ?name
                                  :where
                                  [?u :entity/uuid ?uuid]
                                  [?u :S.User/display-name ?name]]
                                db))
           ai-uuids (into #{} (d/q '[:find [?uuid ...]
                                      :where
                                      [?u :S.User/is-ai true]
                                      [?u :entity/uuid ?uuid]]
                                    db))]
       (into {} (map (fn [[uuid name]]
                       [uuid {:name name :is-ai (contains? ai-uuids uuid)}]))
             names))))

#?(:cljs
   (defn- legacy-room-messages [db room-eid users]
     ;; Optional fields are joined in Clojure because get-else in a CLJS
     ;; multi-join can corrupt Datahike results.
     (let [reasonings (into {} (d/q '[:find ?uuid ?reasoning
                                      :in $ ?room
                                      :where
                                      [?m :S.Message/room ?room]
                                      [?m :S.Message/reasoning ?reasoning]
                                      [?m :entity/uuid ?uuid]]
                                    db room-eid))
           attachments (into {} (d/q '[:find ?uuid ?blob
                                       :in $ ?room
                                       :where
                                       [?m :S.Message/room ?room]
                                       [?m :S.Message/attachment-blob ?blob]
                                       [?m :entity/uuid ?uuid]]
                                     db room-eid))
           att-mimes (into {} (d/q '[:find ?uuid ?mime
                                     :in $ ?room
                                     :where
                                     [?m :S.Message/room ?room]
                                     [?m :S.Message/attachment-mime ?mime]
                                     [?m :entity/uuid ?uuid]]
                                   db room-eid))
           ;; Optimistic sends are still S.Message-only until the canonical
           ;; echo arrives. Preserve their causal edge so a reply does not lose
           ;; its context during that reconciliation window.
           replies (into {} (d/q '[:find ?uuid ?parent
                                    :in $ ?room
                                    :where
                                    [?m :S.Message/room ?room]
                                    [?m :message/in-reply-to ?parent]
                                    [?m :entity/uuid ?uuid]]
                                  db room-eid))
           thread-roots (into {} (d/q '[:find ?uuid ?root
                                         :in $ ?room
                                         :where
                                         [?m :S.Message/room ?room]
                                         [?m :message/thread-root-id ?root]
                                         [?m :entity/uuid ?uuid]]
                                       db room-eid))]
       (->> (d/q '[:find ?uuid ?content ?sent-at ?author-uuid ?author-name
                    :in $ ?room
                    :where
                    [?m :S.Message/room ?room]
                    [?m :entity/uuid ?uuid]
                    [?m :block/content ?content]
                    [?m :S.Message/sent-at ?sent-at]
                    [?m :S.Message/author ?author]
                    [?author :entity/uuid ?author-uuid]
                    [?author :S.User/display-name ?author-name]]
                  db room-eid)
            (map (fn [[uuid content sent-at author-uuid author-name]]
                   (cond-> {:entity/uuid uuid
                            :block/content content
                            :S.Message/sent-at sent-at
                            :S.Message/author-uuid author-uuid
                            :S.Message/author-name author-name
                            :S.Message/is-ai (boolean (get-in users [author-uuid :is-ai]))
                            :timeline/type :message
                            :timeline/ts sent-at}
                     (reasonings uuid)
                     (assoc :S.Message/reasoning (reasonings uuid))
                     (replies uuid)
                     (assoc :message/in-reply-to (replies uuid))
                     (thread-roots uuid)
                     (assoc :message/thread-root-id (thread-roots uuid))
                     (attachments uuid)
                     (assoc :S.Message/attachment-blob (attachments uuid)
                            :S.Message/attachment-mime (att-mimes uuid)))))
            vec))))

(def ^:private canonical-message-pull
  '[:message/id :message/content
    :message/created-at :message/role
    :message/from :message/to
    :message/in-reply-to :message/thread-root-id
    :message/run-id
    :message/reasoning
    :message/source-user :message/source-username
    :message/source-user-id
    :message/audience :message/mention-handles
    :message/metadata-kind :message/context-from
    :message/source :message/schedule-id
    :message/attachment-store-ref
    :message/attachment-blob-id
    :message/attachment-node-id
    :message/attachment-mime
    :message/attachment-name
    :message/attachment-size
    :message/provenance-mode
    :message/provenance-source
    :message/object-kind :message/object-id
    :message/notification-type
    :message/notification-agent
    :message/notification-task
    :message/notification-elapsed
    :message/tool-uses])

(def ^:private activity-pull
  {:message/activities
   [:activity/id :activity/run-id :activity/kind :activity/verb
    :activity/status :activity/tool-name :activity/tool-use-id
    :activity/outcome :activity/critical? :activity/at]})

(defn canonical-message-pull-pattern
  "Canonical message pull, extended only when the Room schema knows activity."
  [activity-schema?]
  (cond-> canonical-message-pull
    activity-schema? (conj activity-pull)))

#?(:cljs
   (defn- schema-has? [db ident]
     (boolean
      (d/q '[:find ?e . :in $ ?ident :where [?e :db/ident ?ident]] db ident))))

#?(:cljs
   (defn- canonical-room-messages [db]
     ;; One bounded pull rather than one query per optional attribute. This is
     ;; both cheaper today and the shape the async client can fetch lazily later.
     (let [pull-pattern (canonical-message-pull-pattern
                         (schema-has? db :message/activities))
           message-eids (d/q '[:find [?m ...]
                               :where
                               [?m :message/id _]
                               [?m :message/chat _]
                               [?m :message/content _]
                               [?m :message/created-at _]
                               [?m :message/role _]]
                             db)]
       (->> (d/pull-many db pull-pattern message-eids)
            ;; Rich exact tool-call entities already own tool history in Simmis.
            ;; Keep these activity messages out of chat rather than showing both.
            (remove #(seq (:message/tool-uses %)))
            (map canonical-message-entity->message)
            vec))))

#?(:cljs
   (defn- query-room-message-projections [db room-eid]
     (let [users (room-message-users db)
           legacy (legacy-room-messages db room-eid users)
           legacy-by-id (into {} (map (juxt :entity/uuid identity)) legacy)
           canonical (canonical-room-messages db)
           canonical-ids (into #{} (map :id) canonical)
           canonical-items (map #(merge-message-projections
                                  % (legacy-by-id (:id %)) users)
                                canonical)]
       (->> (concat canonical-items
                    ;; Compatibility-only rows such as historical summaries may
                    ;; not have traversed the dvergr bus. Keep them visible until
                    ;; the projector is retired with an explicit migration.
                    (remove #(contains? canonical-ids (:entity/uuid %)) legacy))
            (sort-by :timeline/ts)
            vec))))

#?(:cljs
   (defn make-room-messages-query
     "Read canonical dvergr messages with temporary S.Message enrichment.
      Results retain the established UI keys while the compatibility projector
      is removed incrementally."
     [room-uuid]
     (fn [db]
       (when db
         (when-let [room-eid (d/q '[:find ?r . :in $ ?uuid :where [?r :entity/uuid ?uuid]] db room-uuid)]
           (query-room-message-projections db room-eid))))))

#?(:cljs
   (defn room-messages-with-deltas
     "Query room messages with delta computation for incremental rendering.

      Args:
        db-interval - Interval from (track local-db)
        room-uuid   - UUID of the chat room

      Returns: Interval of messages with deltas, suitable for ifor-each"
     [db-interval room-uuid]
     (query-with-deltas
       db-interval
       (make-room-messages-query room-uuid)
       :entity/uuid
       [:room-messages room-uuid])))

#?(:cljs
   (defn make-room-timeline-query
     "Create a query for the unified room timeline: messages + KB events + eval entries, sorted by time.

      Each item has:
      - :entity/uuid         - unique key for ifor-each
      - :timeline/type       - :message, :kb-event, or :eval-entry
      - :timeline/ts         - sort key (inst)

      Messages additionally carry :block/content, :S.Message/* fields.
      KB events additionally carry :S.KBEvent/* fields.
      Eval entries additionally carry :S.EvalEntry/* fields."
     [room-uuid]
     (fn [db]
       (when db
         ;; Resolve the Simmis room projection first; it scopes compatibility
         ;; rows while canonical dvergr messages are already isolated by the
         ;; per-room database itself.
         (when-let [room-eid (d/q '[:find ?r . :in $ ?uuid :where [?r :entity/uuid ?uuid]] db room-uuid)]
           (let [messages (query-room-message-projections db room-eid)
                 kb-events (->> (d/q '[:find ?uuid ?type ?title ?block-count ?author-uuid ?author-name ?ts
                                        :in $ ?room
                                        :where
                                        [?e :S.KBEvent/room ?room]
                                        [?e :entity/uuid ?uuid]
                                        [?e :S.KBEvent/type ?type]
                                        [?e :S.KBEvent/title ?title]
                                        [?e :S.KBEvent/author ?author]
                                        [?author :entity/uuid ?author-uuid]
                                        [?author :S.User/display-name ?author-name]
                                        [?e :S.KBEvent/timestamp ?ts]
                                        [(get-else $ ?e :S.KBEvent/block-count 0) ?block-count]]
                                      db room-eid)
                                (map (fn [[uuid type title block-count author-uuid author-name ts]]
                                       {:entity/uuid uuid
                                        :S.KBEvent/type type
                                        :S.KBEvent/title title
                                        :S.KBEvent/block-count block-count
                                        :S.KBEvent/author-uuid author-uuid
                                        :S.KBEvent/author-name author-name
                                        :S.KBEvent/timestamp ts
                                        :timeline/type :kb-event
                                        :timeline/ts ts})))
                 eval-entries (try
                                (->> (d/q '[:find ?uuid ?tool ?code ?result ?success ?agent-uuid ?agent-name ?ts
                                             :in $ ?room
                                             :where
                                             [?e :S.EvalEntry/room ?room]
                                             [?e :entity/uuid ?uuid]
                                             [?e :S.EvalEntry/code ?code]
                                             [?e :S.EvalEntry/result ?result]
                                             [?e :S.EvalEntry/success? ?success]
                                             [?e :S.EvalEntry/evaluated-at ?ts]
                                             [?e :S.EvalEntry/agent ?agent]
                                             [?agent :entity/uuid ?agent-uuid]
                                             [?agent :S.User/display-name ?agent-name]
                                             ;; entries written before the tool field existed
                                             ;; were all clojure_eval
                                             [(get-else $ ?e :S.EvalEntry/tool "clojure_eval") ?tool]]
                                           db room-eid)
                                     (map (fn [[uuid tool code result success agent-uuid agent-name ts]]
                                            {:entity/uuid uuid
                                             :S.EvalEntry/tool tool
                                             :S.EvalEntry/code code
                                             :S.EvalEntry/result result
                                             :S.EvalEntry/success? success
                                             :S.EvalEntry/agent-uuid agent-uuid
                                             :S.EvalEntry/agent-name agent-name
                                             :S.EvalEntry/evaluated-at ts
                                             :timeline/type :eval-entry
                                             :timeline/ts ts})))
                                ;; Eval chips vanishing from the timeline is a
                                ;; silent, plausible-looking outcome — a room
                                ;; simply looks like it ran no code. Log it.
                                (catch :default e
                                  (js/console.error
                                   "[timeline] eval-entry query failed — chips omitted:" e)
                                  []))]
             (->> (concat messages kb-events eval-entries)
                  (sort-by :timeline/ts)
                  vec)))))))

(def ^:private min-run-length
  ;; A lone tool call stays a chip — hiding one call behind a dot would cost
  ;; a click to learn nothing. Runs are where the noise lives.
  2)

(defn group-tool-runs
  "Collapse each maximal stretch of consecutive tool calls into ONE
   `:eval-run` row holding them.

   What the agent DID between two things it SAID is a single event to the
   reader; 27 rows of it are not 27× as informative, they just bury the
   conversation. The view renders a run as a dot strip that opens into the
   chips.

   Grouping happens HERE, not in the view: the room renders through
   `ifor-each` over an interval, so rows must already be rows before the
   diff. Two properties this relies on —
   - the key is the FIRST call's uuid: stable as the run grows, so a run the
     user opened stays open while the agent keeps working;
   - the calls live IN the item (`:items`), so appending one changes the
     item's VALUE — ifor-each memoizes on item equality and would otherwise
     never re-render the strip."
  [items]
  (->> items
       (partition-by #(= :eval-entry (:timeline/type %)))
       (mapcat (fn [chunk]
                 (if (and (= :eval-entry (:timeline/type (first chunk)))
                          (>= (count chunk) min-run-length))
                   [{:timeline/type :eval-run
                     :entity/uuid (:entity/uuid (first chunk))
                     :timeline/ts (:timeline/ts (last chunk))
                     :items (vec chunk)}]
                   chunk)))
       vec))

(defn- thread-parent-preview
  "Small, presentation-ready parent projection. Keeping the preview bounded
   avoids copying an entire rich message into every descendant's interval row."
  [message]
  (when message
    (let [content (-> (or (:block/content message) "")
                      str
                      (str/replace #"\s+" " ")
                      str/trim)
          preview (if (> (count content) 96)
                    (str (subs content 0 95) "…")
                    content)]
      {:id (:entity/uuid message)
       :author-name (:S.Message/author-name message)
       :content preview})))

(defn annotate-message-threads
  "Derive thread presentation data from canonical root/parent fields without
   changing chronological timeline order.

   A persisted `:message/thread-root-id` wins. Parent traversal is the fallback
   for optimistic/legacy rows and supplies local depth/preview context. If no
   durable root exists and an ancestor is outside a bounded/async result, its
   UUID is retained as the provisional root and `:thread/root-known?` is false;
   absence is therefore never presented as a known top-level message. Cycles
   are bounded and marked incomplete.

   Added keys on message rows:
   - `:thread/root-id`, `:thread/depth`, `:thread/root-known?`
   - `:thread/parent` (bounded direct-parent preview when available)
   - `:thread/reply-count` (all known descendants, on their known root row)"
  [items]
  (let [messages (filterv #(= :message (:timeline/type %)) items)
        by-id (into {} (map (juxt :entity/uuid identity)) messages)
        resolve-thread
        (fn [message]
          (let [authoritative-root (:message/thread-root-id message)]
            (loop [current message
                   depth 0
                   seen #{(:entity/uuid message)}]
              (if-let [parent-id (:message/in-reply-to current)]
                (cond
                  (contains? seen parent-id)
                  (let [root-id (or authoritative-root (:entity/uuid message))]
                    {:root-id root-id
                     :depth depth
                     :root-known? (contains? by-id root-id)})

                  (get by-id parent-id)
                  (recur (get by-id parent-id) (inc depth) (conj seen parent-id))

                  :else
                  (let [root-id (or authoritative-root parent-id)]
                    {:root-id root-id
                     :depth (inc depth)
                     :root-known? (contains? by-id root-id)}))
                (let [root-id (or authoritative-root (:entity/uuid current))]
                  {:root-id root-id
                   :depth depth
                   :root-known? (contains? by-id root-id)})))))
        resolutions (into {}
                          (map (fn [message]
                                 [(:entity/uuid message)
                                  (resolve-thread message)]))
                          messages)
        descendant-counts
        (reduce (fn [counts message]
                  (let [id (:entity/uuid message)
                        root-id (:root-id (get resolutions id))]
                    (if (and root-id (not= id root-id))
                      (update counts root-id (fnil inc 0))
                      counts)))
                {}
                messages)]
    (mapv (fn [item]
            (if (= :message (:timeline/type item))
              (let [id (:entity/uuid item)
                    parent-id (:message/in-reply-to item)
                    {:keys [root-id depth root-known?]} (get resolutions id)]
                (cond-> (assoc item
                               :thread/root-id root-id
                               :thread/depth depth
                               :thread/root-known? root-known?
                               :thread/reply-count (get descendant-counts id 0))
                  parent-id (assoc :thread/parent
                                   (thread-parent-preview (get by-id parent-id)))))
              item))
          items)))

(defn select-message-thread
  "Project one focused thread from already annotated room timeline rows.

   The root is returned separately so clients can pin it as context while
   independently windowing the descendants. Tool/activity rows are excluded
   until Dvergr gives them a durable thread/execution correlation; guessing
   from chronology would attach critical work to the wrong topic."
  [items root-id]
  (let [root (some #(when (and (= :message (:timeline/type %))
                               (= root-id (:entity/uuid %)))
                      %)
                   items)
        replies (->> items
                     (filterv #(and (= :message (:timeline/type %))
                                    (= root-id (:thread/root-id %))
                                    (not= root-id (:entity/uuid %)))))]
    {:root root :items replies}))

#?(:cljs
   (defn room-timeline-window-with-deltas
     "Windowed room timeline: the window is applied IN the query layer
      (doc/proposals-and-time-travel.md chat step-2), so:
      - diffs are O(window), not O(room);
      - the cache is per-room AND window-aware (a window change with an
        unchanged db must not serve the stale windowed result);
      - the render layer needs no islice (whose with-cache address is
        global per call-site — two open chat tabs previously diffed
        against each other's cached slice).

      window-spec: {:start N}            — user-scrolled window start
                   :end / nil            — tail view (last default-size)
                   plus optional :anchor-uuid — center start 10 above it
                   (only honored when no user scroll state, see caller)
                   plus optional :thread-root-id — return that root separately
                   and window only its message descendants.
      Returns {:iv Interval :total n :anchor-idx i-or-nil}.

      The FULL sorted timeline is cached per db-hash (one recompute per
      db change — the rseek-datoms cold path can replace this inside
      this fn without touching callers)."
     [db-interval room-uuid window-spec default-size cut-ts]
     (let [db (iv/get-new db-interval)
           db-identity (when db (hash db))
           full-key [:room-timeline-full room-uuid]
           full-entry (get-cache-entry full-key)
           full-all (if (and db (not= (:db-identity full-entry) db-identity))
                      (let [r ((make-room-timeline-query room-uuid) db)]
                        (set-cached-result! full-key r db-identity)
                        r)
                      (:result full-entry))
           ;; GlobalCut for chat = filter by the DOMAIN timestamp
           ;; (:timeline/ts = message :sent-at etc.), NOT d/as-of on the
           ;; db. as-of filters by TRANSACTION time, which on a
           ;; bulk-synced replica clusters away from sent-at and made the
           ;; past look empty. sent-at is the meaningful chat axis.
           cut-ms (when cut-ts (.getTime cut-ts))
           ;; cut on the RAW calls (per-call timestamps), then group — so the
           ;; time cut can land inside a run and truncate it honestly
           annotated (-> (if cut-ms
                           (filterv #(when-let [ts (:timeline/ts %)]
                                       (<= (.getTime ts) cut-ms))
                                    full-all)
                           full-all)
                         annotate-message-threads)
           thread-root-id (:thread-root-id window-spec)
           thread-projection (when thread-root-id
                               (select-message-thread annotated thread-root-id))
           full (-> (if thread-root-id
                      (:items thread-projection)
                      annotated)
                    group-tool-runs)
           total (count full)
           anchor-uuid (:anchor-uuid window-spec)
           anchor-idx (when anchor-uuid
                        (first (keep-indexed
                                (fn [i m] (when (= anchor-uuid (:entity/uuid m)) i))
                                full)))
           start (cond
                   (and (map? window-spec) (:start window-spec))
                   (min (:start window-spec) (max 0 (- total default-size)))
                   anchor-idx (max 0 (- anchor-idx 10))
                   :else (max 0 (- total default-size)))
           windowed (if (seq full) (subvec (vec full) start total) [])
           ;; window participates in the diff cache: key by room only,
           ;; but store the window; a window change forces a re-diff.
           win-key [:room-timeline-win room-uuid thread-root-id]
           win-entry (get-cache-entry win-key)
           unchanged? (and (= (:db-identity win-entry) db-identity)
                           (= (:window win-entry) start)
                           (= (:cut win-entry) cut-ms))
           iv (if unchanged?
                (iv/->Interval (:result win-entry) (:result win-entry) [])
                (let [old (:result win-entry)
                      diff (diff-by-key old windowed :entity/uuid)
                      deltas (vec (diff->spindel-deltas diff))]
                  (set-cache-entry! win-key {:result windowed
                                             :db-identity db-identity
                                             :window start
                                             :cut cut-ms})
                  (iv/->Interval old windowed deltas)))]
       {:iv iv
        :total total
        :anchor-idx anchor-idx
        :thread-root (:root thread-projection)})))

;; =============================================================================
;; Chat Room Queries
;; =============================================================================

#?(:cljs
   (defn all-chat-rooms-query
     "Query all chat rooms. Returns sorted by name."
     [db]
     (when db
       (->> (d/q '[:find ?uuid ?name ?description
                   :keys entity/uuid S.ChatRoom/name S.ChatRoom/description
                   :where
                   [?e :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-00000000002a"]]
                   [?e :entity/uuid ?uuid]
                   [?e :S.ChatRoom/name ?name]
                   [(get-else $ ?e :S.ChatRoom/description "") ?description]]
                 db)
            (sort-by :S.ChatRoom/name)
            vec))))

;; =============================================================================
;; User Queries
;; =============================================================================

#?(:cljs
   (defn all-users-query
     "Query all users. Returns sorted by display name."
     [db]
     (when db
       (->> (d/q '[:find ?uuid ?name ?email ?is-ai
                   :keys entity/uuid S.User/display-name S.User/email S.User/is-ai
                   :where
                   [?e :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-000000000029"]]
                   [?e :entity/uuid ?uuid]
                   [?e :S.User/display-name ?name]
                   [(get-else $ ?e :S.User/email "") ?email]
                   [(get-else $ ?e :S.User/is-ai false) ?is-ai]]
                 db)
            (sort-by :S.User/display-name)
            vec))))

;; =============================================================================
;; Type and Property Queries
;; =============================================================================

#?(:cljs
   (defn page-title-query
     "Current `:S.Page/title` of a page, or nil.

      `:page/title` is not a datom — the server synthesizes it on returned maps
      for backward compatibility. The stored attribute is `:S.Page/title`."
     [db page-uuid]
     (when (and db page-uuid)
       (d/q '[:find ?title .
              :in $ ?page-uuid
              :where
              [?e :entity/uuid ?page-uuid]
              [?e :S.Page/title ?title]]
            db page-uuid))))

#?(:cljs
   (defn page-with-title-query
     "UUID of another page holding `title`, or nil.

      The client carries the whole KB replica, so a rename conflict can be
      detected locally BEFORE any optimistic overlay is applied — an overlay the
      server rejects would otherwise linger until its TTL, since overlays only
      clear when their caught-up predicate turns true. The server re-checks
      authoritatively; this is a pre-flight, not the decision."
     [db title exclude-page-uuid]
     (when (and db (seq title))
       (d/q '[:find ?uuid .
              :in $ ?title ?exclude
              :where
              [?role :entity/name "S/Page"]
              [?e :instance/of-role ?role]
              [?e :S.Page/title ?title]
              [?e :entity/uuid ?uuid]
              [(not= ?uuid ?exclude)]]
            db title exclude-page-uuid))))

#?(:cljs
   (defn page-types-query
     "Query all types assigned to a page via :instance/of-role.
      Returns vector of type entities with :entity/uuid and :entity/name.

      NOTE: Uses tuple find spec [:find ?page ?role] to work around CLJS datahike
      bug where collection binding [(pull ?role ...) ...] returns nil for joined
      variables when combined with :in bindings."
     [db page-uuid]
     (when (and db page-uuid)
       (->> (d/q '[:find ?page ?role
                   :in $ ?page-uuid
                   :where
                   [?page :entity/uuid ?page-uuid]
                   [?page :instance/of-role ?role]
                   [?role :object/of-category _]]
                 db page-uuid)
            (map second)
            (map #(d/pull db [:entity/uuid :entity/name :object/primitive? :object/icon] %))
            (filter :entity/name)
            vec))))

#?(:cljs
   (defn all-types-query
     "Query all available types (objects in category S).
      Returns vector of type entities.

      NOTE: Uses tuple find spec to work around CLJS datahike bug with
      collection binding on joined variables."
     [db]
     (when db
       (->> (d/q '[:find ?cat ?type
                   :where
                   [?cat :entity/name "S"]
                   [?type :object/of-category ?cat]]
                 db)
            (map second)
            (map #(d/pull db [:entity/uuid :entity/name :object/primitive? :object/icon] %))
            (filter :entity/name)
            (remove :object/primitive?)
            (sort-by :entity/name)
            vec))))

#?(:cljs
   (defn type-properties-query
     "Query all properties (morphisms) for a type.
      Returns vector of morphism entities with property metadata.

      NOTE: Uses tuple find spec to work around CLJS datahike bug with
      collection binding on joined variables."
     [db type-name]
     (when (and db type-name)
       (->> (d/q '[:find ?type ?morph
                   :in $ ?type-name
                   :where
                   [?type :entity/name ?type-name]
                   [?morph :morphism/src ?type]]
                 db type-name)
            (map second)
            (map #(d/pull db [:entity/uuid :entity/name :morphism/property-type
                              ;; WHERE the values live. Without it the two
                              ;; derivations below compute an attribute that
                              ;; does not exist for the 64% of morphisms that
                              ;; are reflected — including `created-at` and
                              ;; `updated-at` on S/Page, i.e. every wiki page.
                              :morphism/storage-attr
                              :morphism/cardinality :morphism/optional?
                              {:morphism/dst [:entity/uuid :entity/name]}] %))
            (filter :entity/name)
            (sort-by :entity/name)
            vec))))

#?(:cljs
   (defn page-properties-query
     "Query all properties and their values for a page.
      Collects properties from all types the page has.
      Returns {:properties [...] :values {prop-name value}}."
     [db page-uuid]
     (when (and db page-uuid)
       (let [;; Get all types for this page
             types (page-types-query db page-uuid)
             type-names (map :entity/name types)

             ;; Collect properties from all types
             all-properties (->> type-names
                                 (mapcat #(type-properties-query db %))
                                 distinct
                                 vec)

             ;; Get the page entity to read property values.
             ;; Resolve the eid via d/q first — d/pull with a [:entity/uuid …]
             ;; lookup-ref THROWS :entity-id/missing when the entity isn't in
             ;; this db yet (e.g. a page created via a [[link]] whose creation
             ;; hasn't synced to the client's KB DB). Pulling by a resolved
             ;; eid (or skipping when absent) keeps the query graceful — the
             ;; render shows a loading state and re-runs once sync lands.
             ;; Mirrors the guard pattern in is-type-page?.
             eid (d/q '[:find ?e . :in $ ?uuid
                        :where [?e :entity/uuid ?uuid]]
                      db page-uuid)
             page (when eid (d/pull db '[*] eid))

             ;; Extract property values from the page entity
             ;; Properties are stored as dynamic attributes like :S.Page/title
             values (reduce
                      (fn [acc prop]
                        (let [prop-name (:entity/name prop)
                              ;; ONE derivation, storage-attr aware — see
                              ;; is.simm.model.morphism.
                              attr-kw (mor/attr-of prop)]
                          (if-let [v (get page attr-kw)]
                            (assoc acc prop-name v)
                            acc)))
                      {}
                      all-properties)]
         {:properties all-properties
          :values values}))))

#?(:cljs
   (defn is-type-page?
     "Check if a page is a type (has :object/of-category).
      Returns the type entity with :entity/name if it's a type, nil otherwise."
     [db page-uuid]
     (when (and db page-uuid)
       (when-let [eid (first
                        (d/q '[:find [?page ...]
                               :in $ ?page-uuid
                               :where
                               [?page :entity/uuid ?page-uuid]
                               [?page :object/of-category _]]
                             db page-uuid))]
         (d/pull db [:entity/uuid :entity/name :object/of-category] eid)))))

#?(:cljs
   (defn type-instances-query
     "Query all instances of a type.
      Returns vector of entities that have :instance/of-role pointing to this type.

      NOTE: Uses tuple find spec to work around CLJS datahike bug with
      collection binding on joined variables."
     [db type-name]
     (when (and db type-name)
       (->> (d/q '[:find ?type ?inst
                   :in $ ?type-name
                   :where
                   [?type :entity/name ?type-name]
                   [?inst :instance/of-role ?type]]
                 db type-name)
            (map second)
            (map #(d/pull db [:entity/uuid :entity/name :S.Page/title] %))
            (sort-by #(or (:S.Page/title %) (:entity/name %) ""))
            vec))))

#?(:cljs
   (defn type-instances-with-properties-query
     "Query all instances of a type with their property values.
      Returns {:properties [...] :instances [{:entity/uuid ... :values {...}}]}.
      Properties are morphisms from the type, instances are entities with values.

      NOTE: Uses tuple find spec to work around CLJS datahike bug with
      collection binding on joined variables."
     [db type-name]
     (when (and db type-name)
       (let [;; Get properties (morphisms) for this type
             properties (type-properties-query db type-name)

             ;; Get all instances - pull everything to get property values
             instances (->> (d/q '[:find ?type ?inst
                                   :in $ ?type-name
                                   :where
                                   [?type :entity/name ?type-name]
                                   [?inst :instance/of-role ?type]]
                                 db type-name)
                            (map second)
                            (map #(d/pull db '[*] %))
                            (sort-by #(or (:S.Page/title %) (:entity/name %) ""))
                            vec)

             ;; Extract property values for each instance. ONE derivation,
             ;; storage-attr aware — see is.simm.model.morphism.
             prop-attrs (into {}
                              (map (fn [prop]
                                     (when-let [prop-name (:entity/name prop)]
                                       [prop-name (mor/attr-of prop)])))
                              properties)

             ;; Build instances with extracted property values
             instances-with-values
             (mapv (fn [inst]
                     {:entity/uuid (:entity/uuid inst)
                      :entity/name (:entity/name inst)
                      :S.Page/title (:S.Page/title inst)
                      :values (into {}
                                    (keep (fn [[prop-name attr-kw]]
                                            (when-let [v (get inst attr-kw)]
                                              [prop-name v])))
                                    prop-attrs)})
                   instances)]

         {:properties properties
          :instances instances-with-values}))))

;; =============================================================================
;; Debug Helpers
;; =============================================================================

(defn debug-interval
  "Print interval contents for debugging."
  [label interval]
  #?(:cljs (js/console.log label
                            "old:" (count (iv/get-old interval))
                            "new:" (count (iv/get-new interval))
                            "deltas:" (count (or (iv/get-deltas interval) []))))
  #?(:clj (println label
                   "old:" (count (iv/get-old interval))
                   "new:" (count (iv/get-new interval))
                   "deltas:" (count (or (iv/get-deltas interval) [])))))

(defn debug-cache-state
  "Get current cache state for debugging."
  []
  (into {}
        (map (fn [[k v]] [k {:result-count (count (:result v))
                             :db-identity (:db-identity v)}]))
        @query-cache))
