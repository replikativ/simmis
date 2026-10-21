(ns is.simm.documents.view
  "Experimental read barrier over Geschichte's current worktree.

   A view is an immutable derived DB value, NOT another writable repository,
   registered Yggdrasil system, durable cache or authorization boundary. The
   caller must authorize and retain the captured source DB/content lifetime."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [geschichte.content :as content]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.documents.blocks :as blocks]
            [is.simm.documents.canonical :as canonical]
            [is.simm.documents.files :as files]
            [is.simm.documents.identity :as identity]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.metadata :as metadata]
            [is.simm.documents.properties :as properties]
            [is.simm.documents.repository :as documents]
            [is.simm.documents.space :as space]))

(def ^:private max-files 200)
(def ^:private max-scanned-entries 20000)
(def ^:private max-total-bytes (* 8 1024 1024))
(def ^:private max-total-nodes 100000)

(defn- refuse! [reason]
  (throw (ex-info "Knowledge view cannot be prepared" {:type ::refused :reason reason})))

(defn- prefix! [root]
  ;; Require one exact repository-relative folder, not an arbitrary query.
  (when-not (and (string? root) (<= 0 (count root) 512)
                 (not (re-find #"[\\\p{Cntrl}]" root))
                 (or (= "" root)
                     (every? #(and (not (str/blank? %))
                               (not (#{"." ".." ".git" ".geschichte"} %)))
                             (str/split root #"/" -1))))
    (refuse! :invalid-root))
  (if (= "" root) "" (str root "/")))

(defn- policy-prefixes! [paths]
  (when-not (and (vector? paths) (<= (count paths) 64))
    (refuse! :invalid-selection-policy))
  (->> paths (map prefix!) distinct sort vec))

(defn selection
  "Bounded source-only descriptor. Cache writes and unrelated files do not
   change its identity. Schema revision is supplied by trusted host code."
  [db {:keys [roots exclude-prefixes schema-revision property-bindings]
       :or {roots [""] exclude-prefixes [] property-bindings {}} :as options}]
  ;; The experimental singular spelling must not silently widen to all files.
  (when (contains? options :root) (refuse! :invalid-selection-policy))
  (when (and (contains? options :entity-profile) (not= space/profile (:entity-profile options)))
    (refuse! :unsupported-entity-profile))
  (when (:entity-profile options)
    (when-not (space/provisioned? db) (refuse! :unprovisioned-canonical-space))
    (when (seq property-bindings) (refuse! :mixed-property-profiles)))
  (when (nil? schema-revision) (refuse! :missing-schema-revision))
  (let [prefixes (policy-prefixes! roots)
        excluded (policy-prefixes! exclude-prefixes)
        repository-id (:geschichte.repo/id (query/repository db))
        rows (vec (take (inc max-scanned-entries)
                        (d/datoms db :avet :geschichte.work/path)))]
    (when-not repository-id (refuse! :missing-repository))
    ;; Metadata traversal and selected source bodies have separate limits:
    ;; room sandbox code must not consume the Markdown document allowance.
    (when (> (count rows) max-scanned-entries) (refuse! :too-many-worktree-entries))
    (let [files (into (sorted-map)
                      (keep (fn [datom]
                              (let [row (d/pull db '[:geschichte.work/path
                                                    :geschichte.work/size :geschichte.work/mode
                                                    {:geschichte.work/content
                                                       [:geschichte.content/id]}]
                                                (:e datom))
                                    path (:geschichte.work/path row)]
                                (when (and (some #(str/starts-with? path %) prefixes)
                                           (not-any? #(str/starts-with? path %) excluded)
                                           (re-find #"(?i)\.(md|markdown)$" path))
                                  [path {:content (get-in row [:geschichte.work/content
                                                               :geschichte.content/id])
                                         :size (:geschichte.work/size row)
                                         :mode (:geschichte.work/mode row)}])))) rows)]
      (when (> (count files) max-files) (refuse! :too-many-files))
      (doseq [[path {:keys [content size mode]}] files]
        (files/path! path)
        (when-not (and (uuid? content) (integer? size) (<= 0 size markdown/max-source-bytes)
                       ;; No symlink following, executable Markdown or gitlinks.
                       (= mode repo/default-mode))
          (refuse! :unsupported-file)))
      (when (> (reduce + 0 (map :size (vals files))) max-total-bytes)
        (refuse! :too-many-source-bytes))
      (cond-> {:repository-id repository-id :selection-kind :worktree
       :roots prefixes :exclude-prefixes excluded :projection-version 3
       :files files :codec markdown/syntax-profile :block-codec blocks/profile
       :property-schema (:revision (properties/compile-schema db property-bindings))
       :schema-revision schema-revision}
        (:entity-profile options)
        (assoc :entity-profile space/profile :canonical-codec canonical/profile
               :metadata-codec metadata/profile :typed-property-codec properties/typed-profile)))))

(declare canonical-base! projection-schema!)

(defn current?
  "Whether this view still matches the selected source and trusted schema.
   A true answer is as-of this DB snapshot, not a lock on subsequent writes."
  [db options view]
  (try
    (and (= (:selection view) (selection db options))
         (or (nil? (:entity-profile options))
             (let [own-derived? (identical? db (:db view))]
               (canonical-base! db own-derived?)
               (and (canonical/current? db (:typed-scopes view) (:constraint-dependencies view) own-derived?)
                    (or own-derived? (canonical/source-available? db (:documents view)))))))
    (catch clojure.lang.ExceptionInfo error
      (if (and (:entity-profile options) (= ::refused (:type (ex-data error))))
        false
        (throw error)))))

(defn- canonical-base!
  ([db] (canonical-base! db false))
  ([db own-derived?]
  (let [schema (d/schema db)]
    (doseq [[attr expected]
            {:entity/uuid {:db/valueType :db.type/uuid :db/cardinality :db.cardinality/one
                           :db/unique :db.unique/identity}
             :instance/of-role {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}}]
      (when-not (= expected (select-keys (get schema attr) (keys expected)))
        (refuse! :incompatible-canonical-identity-schema)))
    (when-not (every? #(contains? schema (:db/ident %)) documents/schema)
      (refuse! :missing-canonical-document-schema))
    (projection-schema! db)
    (when-not (= {:db/valueType :db.type/string :db/cardinality :db.cardinality/one
                  :db/unique :db.unique/value}
                 (select-keys (:S.Page/title schema) [:db/valueType :db/cardinality :db/unique]))
      (refuse! :incompatible-canonical-page-title))
    (when (and (not own-derived?)
               (some #(seq (d/datoms db :aevt (:db/ident %))) documents/schema))
      (refuse! :derived-db-is-not-canonical-source)))))

(defn- projection-schema! [db]
  (let [schema (d/schema db)]
    (doseq [expected documents/schema
            :let [actual (get schema (:db/ident expected))]
            :when actual]
      (when-not (and (= expected (select-keys actual (keys expected)))
                     (= (boolean (:db/isComponent expected)) (boolean (:db/isComponent actual)))
                     (= (:db/unique expected) (:db/unique actual)))
        (refuse! :incompatible-projection-schema)))))

(defn- projection-ownership!
  "Only our disposable schema/entities may be replaced in a derived view.
   Mixed authored legacy rows/references require an explicit migration, not
   retractEntity's implicit removal of incoming references."
  [db]
  (let [schema (d/schema db)
        allowed (set (map :db/ident documents/schema))]
    (projection-schema! db)
    (when (or (contains? schema :document/id) (contains? schema :document.node/kind)
              (contains? schema :document.block/id))
      (let [roots (when (contains? schema :document/id)
                    (vec (take (inc max-files) (map :e (d/datoms db :aevt :document/id)))))
            nodes (when (contains? schema :document.node/kind)
                    (vec (take (inc max-total-nodes)
                               (map :e (d/datoms db :aevt :document.node/kind)))))
            block-entities (when (contains? schema :document.block/id)
                             (vec (take (inc max-total-nodes)
                                        (map :e (d/datoms db :aevt :document.block/id)))))
            owned (set (concat roots nodes block-entities))]
        (when (or (> (count roots) max-files) (> (+ (count nodes) (count block-entities)) max-total-nodes))
          (refuse! :too-many-existing-projection-entities))
        (let [reachable (loop [pending (vec roots), seen #{}]
                          (if-let [eid (peek pending)]
                            (if (contains? seen eid)
                              (recur (pop pending) seen)
                              (let [attrs (filterv #(contains? schema %)
                                                   [:document/nodes :document.node/children
                                                    :document/blocks :document.block/children])
                                    row (d/pull db attrs eid)
                                    children (map :db/id (concat (:document/nodes row)
                                                                 (:document.node/children row)
                                                                 (:document/blocks row)
                                                                 (:document.block/children row)))]
                                (when-not (every? owned children) (refuse! :foreign-projection-child))
                                (recur (into (pop pending) children) (conj seen eid))))
                            seen))]
          (when-not (= owned reachable) (refuse! :orphan-projection-entity)))
        (doseq [eid owned, datom (d/datoms db :eavt eid)]
          (when-not (contains? allowed (:a datom))
            (refuse! :authored-projection-entity)))
        (when (seq owned)
          (doseq [[source attr] (d/q '[:find ?e ?a :in $ [?target ...]
                                      :where [?e ?a ?target]] db (vec owned))
                  :when (= :db.type/ref (:db/valueType (get schema attr)))]
            (when-not (and (contains? owned source)
                           (#{:document/nodes :document.node/children
                              :document/blocks :document.block/children} attr))
              (refuse! :authored-reference-to-projection))))))))

(defn prepare
  "Build a historical/as-of view from ONE immutable DB snapshot. Does not
   transact, write source, execute file declarations or install global state.
   Invalid documents return diagnostics without a queryable partial DB."
  [source-db options]
  (let [selected (selection source-db options)
        canonical? (some? (:entity-profile options))
        _ (when canonical? (canonical-base! source-db))
        _ (projection-ownership! source-db)
        property-schema (properties/compile-schema source-db (get options :property-bindings {}))
        docs (:documents
              (reduce (fn [{:keys [documents node-count]} [path {:keys [content size]}]]
                        (let [logical-size (:geschichte.content/size
                                            (d/entity source-db [:geschichte.content/id content]))
                              _ (when-not (= size logical-size) (refuse! :content-size-mismatch))
                              bytes (content/read-by-id (atom source-db) content)]
                          (when-not (and bytes (= size (alength ^bytes bytes)))
                            (refuse! :content-size-mismatch))
                          (let [parsed (markdown/parse (markdown/decode bytes))
                                identity (identity/document (:repository-id selected) path parsed)
                                semantic (when-let [id (:id identity)] (blocks/project id parsed))
                                property-values (properties/decode property-schema
                                                                   (get-in parsed [:frontmatter :properties]))
                                n (+ node-count (count (tree-seq (comp seq :children) :children
                                                                 {:children (:nodes parsed)})))]
                            ;; Stop between files rather than accumulating all ASTs
                            ;; before checking the workspace-wide memory budget.
                            (when (> n max-total-nodes) (refuse! :too-many-syntax-nodes))
                            {:node-count n
                             :documents (conj documents
                                              (-> (merge parsed (dissoc identity :diagnostics))
                                                  (assoc :path path :content content :semantic semantic
                                                         :properties property-values
                                                         :diagnostics (into (or (:diagnostics semantic)
                                                                                (:diagnostics parsed))
                                                                            (concat (:diagnostics identity)
                                                                                    (:diagnostics property-values))))))})))
                      {:documents [] :node-count 0} (:files selected)))
        canonical-result (when canonical? (canonical/prepare source-db docs))
        duplicate-ids (->> docs (keep :id) frequencies (keep (fn [[id n]] (when (> n 1) id))) set)
        semantic-ids (mapcat (fn [doc]
                              (cons {:id (:id doc) :path (:path doc)}
                                    (for [block (mapcat #(tree-seq (comp seq :children) :children %)
                                                        (get-in doc [:semantic :blocks]))]
                                      {:id (:id block) :path (:path doc) :from (:from block)}))) docs)
        duplicate-semantic-ids (->> semantic-ids (keep :id) frequencies
                                    (keep (fn [[id n]] (when (> n 1) id))) set)
        diagnostics (into (vec (mapcat :diagnostics docs))
                          (concat (:diagnostics canonical-result)
                                  (for [doc docs :when (contains? duplicate-ids (:id doc))]
                                    {:kind :duplicate-document-id :path (:path doc)})
                                  (for [{:keys [id path from]} semantic-ids
                                        :when (and (some? from) (contains? duplicate-semantic-ids id))]
                                    {:kind :duplicate-semantic-id :path path :from from})))
        base (cond-> {:id (hasch/uuid selected) :selection selected
                      :documents docs :diagnostics diagnostics}
               canonical? (assoc :id (hasch/uuid [selected (:scopes canonical-result) (:dependencies canonical-result)])
                                 :typed-scopes (:scopes canonical-result)
                                 :constraint-dependencies (:dependencies canonical-result)))]
    (if (seq diagnostics)
      (assoc base :status :invalid)
      (let [installed (set (d/q '[:find [?a ...] :where [_ :db/ident ?a]] source-db))
            missing (remove #(contains? installed (:db/ident %)) documents/schema)
            old (when (contains? installed :document/id)
                  (d/q '[:find [?e ...] :where [?e :document/id]] source-db))
            projection (map (fn [{:keys [id path content revision nodes properties semantic] :as document}]
                              (merge (:values properties)
                               {:document/id id :document/path path
                               :document/source-content [:geschichte.content/id content]
                               :document/revision revision :document/projection-version 3
                               :document/valid? true
                               :document/blocks (mapv blocks/entity-tx (:blocks semantic))
                               :document/nodes (mapv documents/syntax-node-tx (range) nodes)}
                               (when canonical? (canonical/document-tx document))))
                            (if canonical? (:documents canonical-result) docs))
            tx (vec (concat missing (when-not canonical? (map #(vector :db/retractEntity %) old)) projection))]
        (assoc base :status :ready :db (if (seq tx) (d/db-with source-db tx) source-db))))))

(defn- same-source? [before after]
  (or (identical? before after)
      ;; A connection refresh may return a new wrapper for the SAME stored
      ;; commit. This is only a stability test on our captured connection,
      ;; never authority supplied by a caller or a derived-view exemption.
      (let [cid (get-in before [:meta :datahike/commit-id])]
        (and (uuid? cid) (= cid (get-in after [:meta :datahike/commit-id]))
             (= (get-in before [:config :store :id]) (get-in after [:config :store :id]))
             (= (get-in before [:config :branch]) (get-in after [:config :branch]))
             (= (select-keys before [:hash :max-tx :max-eid :op-count])
                (select-keys after [:hash :max-tx :max-eid :op-count]))))))

(defn current-view
  "Prepare and recheck before offering a current view. A racing publication
   yields pending, never a stale DB mislabeled current. Callers still retain
   the returned descriptor for later query/edit/merge authorization checks."
  [conn options]
  (let [source @conn
        view (prepare source options)
        current @conn]
    ;; A stable invalid preparation is useful diagnostics, not perpetual
    ;; pending merely because no resolved-constraint receipt exists for it.
    (if (or (same-source? source current) (current? current options view))
      view
      {:status :pending :reason :source-changed})))
