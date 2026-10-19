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
            [is.simm.documents.identity :as identity]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.properties :as properties]
            [is.simm.documents.repository :as documents]))

(def ^:private max-files 200)
(def ^:private max-total-bytes (* 8 1024 1024))
(def ^:private max-total-nodes 100000)

(defn- refuse! [reason]
  (throw (ex-info "Knowledge view cannot be prepared" {:type ::refused :reason reason})))

(defn- prefix! [root]
  ;; Require one exact repository-relative folder, not an arbitrary query.
  (when-not (and (string? root) (<= 1 (count root) 512)
                 (not (re-find #"[\\\p{Cntrl}]" root))
                 (every? #(and (not (str/blank? %))
                               (not (#{"." ".." ".git" ".geschichte"} %)))
                         (str/split root #"/" -1)))
    (refuse! :invalid-root))
  (str root "/"))

(defn selection
  "Bounded source-only descriptor. Cache writes and unrelated files do not
   change its identity. Schema revision is supplied by trusted host code."
  [db {:keys [root schema-revision property-bindings] :or {root "wiki" property-bindings {}}}]
  (when (nil? schema-revision) (refuse! :missing-schema-revision))
  (let [prefix (prefix! root)
        repository-id (:geschichte.repo/id (query/repository db))
        rows (vec (take (inc max-files)
                        (d/index-range db {:attrid :geschichte.work/path
                                           :start prefix
                                           :end (str root "0")})))]
    (when-not repository-id (refuse! :missing-repository))
    ;; Bound all entries in this folder, not just Markdown: never traverse an
    ;; unbounded prefix and then filter it down to a small visible result.
    (when (> (count rows) max-files) (refuse! :too-many-files))
    (let [files (into (sorted-map)
                      (keep (fn [datom]
                              (let [row (d/pull db '[* {:geschichte.work/content
                                                       [:geschichte.content/id]}]
                                                (:e datom))
                                    path (:geschichte.work/path row)]
                                (when (and (str/starts-with? path prefix)
                                           (re-find #"(?i)\.(md|markdown)$" path))
                                  [path {:content (get-in row [:geschichte.work/content
                                                               :geschichte.content/id])
                                         :size (:geschichte.work/size row)
                                         :mode (:geschichte.work/mode row)}])))) rows)]
      (doseq [[_ {:keys [content size mode]}] files]
        (when-not (and (uuid? content) (integer? size) (<= 0 size markdown/max-source-bytes)
                       ;; No symlink following, executable Markdown or gitlinks.
                       (= mode repo/default-mode))
          (refuse! :unsupported-file)))
      (when (> (reduce + 0 (map :size (vals files))) max-total-bytes)
        (refuse! :too-many-source-bytes))
      {:repository-id repository-id :selection-kind :worktree :root root :projection-version 2
       :files files :codec markdown/syntax-profile :block-codec blocks/profile
       :property-schema (:revision (properties/compile-schema db property-bindings))
       :schema-revision schema-revision})))

(defn current?
  "Whether this view still matches the selected source and trusted schema.
   A true answer is as-of this DB snapshot, not a lock on subsequent writes."
  [db options view]
  (= (:selection view) (selection db options)))

(defn- projection-ownership!
  "Only our disposable schema/entities may be replaced in a derived view.
   Mixed authored legacy rows/references require an explicit migration, not
   retractEntity's implicit removal of incoming references."
  [db]
  (let [schema (d/schema db)
        allowed (set (map :db/ident documents/schema))]
    (doseq [expected documents/schema
            :let [actual (get schema (:db/ident expected))]
            :when actual]
      (when-not (and (= expected (select-keys actual (keys expected)))
                     (= (boolean (:db/isComponent expected)) (boolean (:db/isComponent actual)))
                     (= (:db/unique expected) (:db/unique actual)))
        (refuse! :incompatible-projection-schema)))
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
        duplicate-ids (->> docs (keep :id) frequencies (keep (fn [[id n]] (when (> n 1) id))) set)
        semantic-ids (mapcat (fn [doc]
                              (cons {:id (:id doc) :path (:path doc)}
                                    (for [block (mapcat #(tree-seq (comp seq :children) :children %)
                                                        (get-in doc [:semantic :blocks]))]
                                      {:id (:id block) :path (:path doc) :from (:from block)}))) docs)
        duplicate-semantic-ids (->> semantic-ids (keep :id) frequencies
                                    (keep (fn [[id n]] (when (> n 1) id))) set)
        diagnostics (into (vec (mapcat :diagnostics docs))
                          (concat (for [doc docs :when (contains? duplicate-ids (:id doc))]
                                    {:kind :duplicate-document-id :path (:path doc)})
                                  (for [{:keys [id path from]} semantic-ids
                                        :when (and (some? from) (contains? duplicate-semantic-ids id))]
                                    {:kind :duplicate-semantic-id :path path :from from})))
        base {:id (hasch/uuid selected) :selection selected
              :documents docs :diagnostics diagnostics}]
    (if (seq diagnostics)
      (assoc base :status :invalid)
      (let [installed (set (d/q '[:find [?a ...] :where [_ :db/ident ?a]] source-db))
            missing (remove #(contains? installed (:db/ident %)) documents/schema)
            old (when (contains? installed :document/id)
                  (d/q '[:find [?e ...] :where [?e :document/id]] source-db))
            projection (map (fn [{:keys [id path content revision nodes properties semantic]}]
                              (merge (:values properties)
                               {:document/id id :document/path path
                               :document/source-content [:geschichte.content/id content]
                               :document/revision revision :document/projection-version 3
                               :document/valid? true
                               :document/blocks (mapv blocks/entity-tx (:blocks semantic))
                               :document/nodes (mapv documents/syntax-node-tx (range) nodes)})) docs)
            tx (vec (concat missing (map #(vector :db/retractEntity %) old) projection))]
        (assoc base :status :ready :db (if (seq tx) (d/db-with source-db tx) source-db))))))

(defn current-view
  "Prepare and recheck before offering a current view. A racing publication
   yields pending, never a stale DB mislabeled current. Callers still retain
   the returned descriptor for later query/edit/merge authorization checks."
  [conn options]
  (let [view (prepare @conn options)]
    (if (current? @conn options view)
      view
      {:status :pending :reason :source-changed})))
