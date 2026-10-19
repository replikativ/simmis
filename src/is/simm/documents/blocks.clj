(ns is.simm.documents.blocks
  "Reversible semantic grouping over an exact Markdown syntax view.

   Experimental, inert and source-preserving. Explicit block comments delimit
   Simmis groups independently of Markdown lists/headings. Unmarked syntax is
   provisional, not an assertion of stable identity or legacy HTML migration."
  (:require [clojure.string :as str]
            [hasch.core :as hasch]
            [is.simm.documents.markdown :as markdown]))

(def profile {:id :simmis/markdown-blocks :version 1})
(def ^:private max-group-depth 100)

(defn- canonical-uuid [s]
  (when (string? s)
    (try (let [id (java.util.UUID/fromString s)] (when (= s (str id)) id))
         (catch IllegalArgumentException _ nil))))

(defn- marker [source node]
  (when (#{:HtmlCommentBlock :HtmlInlineComment} (:kind node))
    (let [text (str/trim (subs source (:from node) (:to node)))]
      (cond
        (= text "<!-- /simmis:block -->") {:kind :close}
        (str/starts-with? text "<!-- simmis:block")
        (if-let [[_ s] (re-matches #"<!-- simmis:block ([^ ]+) -->" text)]
          (if-let [id (canonical-uuid s)] {:kind :open :id id} {:kind :invalid})
          {:kind :invalid})
        (str/starts-with? text "<!-- /simmis:block") {:kind :invalid}))))

(defn- plain-block [document-id revision node]
  {:id (hasch/uuid [:provisional-block document-id revision (:from node) (:to node)])
   :identity-status :provisional :from (:from node) :to (:to node)
   :regions [{:from (:from node) :to (:to node)}] :syntax [node] :children []})

(defn- append-block [stack block]
  (update-in stack [(dec (count stack)) :children]
             (fn [children] (conj children (assoc block :ordinal (count children))))))

(defn- group-regions [frame end]
  ;; Only the group's OWN content, never the separately identified children.
  ;; Whitespace is retained; unsupported regions stay source-editable.
  (loop [position (:content-from frame), children (:children frame), regions []]
    (if-let [child (first children)]
      (recur (:to child) (next children)
             (cond-> regions (< position (:from child))
               (conj {:from position :to (:from child)})))
      (cond-> regions (< position end) (conj {:from position :to end})))))

(defn project
  "Derive groups without writing or executing source. Comments must occupy
   top-level Markdown comment nodes; markers inside code/HTML examples are
   inert. Invalid/duplicate/unclosed groups produce diagnostics, not repairs."
  [document-id {:keys [source revision nodes] :as document}]
  (when-not (uuid? document-id)
    (throw (ex-info "Block projection requires document identity" {:type ::invalid})))
  (let [nested-comments (->> nodes (mapcat :children)
                             (mapcat #(tree-seq (comp seq :children) :children %))
                             (filter #(marker source %)))
        initial {:stack [{:children []}] :diagnostics
                 (into (vec (:diagnostics document))
                       (map #(hash-map :kind :nested-block-marker :from (:from %) :to (:to %)))
                       nested-comments)}
        {:keys [stack diagnostics]}
        (reduce
         (fn [{:keys [stack] :as state} node]
           (let [{:keys [kind id]} (marker source node)]
             (case kind
               :open (do
                       (when (> (count stack) max-group-depth)
                         (throw (ex-info "Semantic block nesting limit exceeded" {:type ::invalid})))
                       (update state :stack conj {:id id :identity-status :explicit
                                                 :from (:from node) :content-from (:to node)
                                                 :syntax [] :children []}))
               :close (if (= 1 (count stack))
                        (update state :diagnostics conj {:kind :unmatched-block-close
                                                        :from (:from node) :to (:to node)})
                        (let [frame (peek stack)
                              block (-> frame (dissoc :content-from)
                                        (assoc :to (:to node)
                                               :regions (group-regions frame (:from node))))]
                          (assoc state :stack (append-block (pop stack) block))))
               :invalid (update state :diagnostics conj {:kind :invalid-block-marker
                                                        :from (:from node) :to (:to node)})
               ;; Inside a group, multiple Markdown nodes belong to that ONE
               ;; semantic block. They do not each create a semantic child.
               (if (> (count stack) 1)
                 (update-in state [:stack (dec (count stack)) :syntax] conj node)
                 (update state :stack append-block (plain-block document-id revision node))))))
         initial nodes)
        blocks (:children (first stack))
        all-blocks (mapcat #(tree-seq (comp seq :children) :children %) blocks)
        duplicates (->> all-blocks (map :id) frequencies
                        (keep (fn [[id n]] (when (> n 1) id))) set)
        diagnostics (into diagnostics
                          (concat (for [frame (rest stack)]
                                    {:kind :unclosed-block :from (:from frame)})
                                  (for [block all-blocks :when (contains? duplicates (:id block))]
                                    {:kind :duplicate-block-id :from (:from block) :to (:to block)})))]
    {:profile profile :revision revision :blocks blocks
     :status (if (seq diagnostics) :invalid :ready) :diagnostics diagnostics}))

(defn replace-region
  "Revision-checked edit to ONE own-content region. Never overwrite a nested
   semantic child implicitly. The caller still authorizes/publishes the edit."
  [document-id source expected-revision block-id region-index replacement]
  (when-not (= expected-revision (markdown/revision source))
    (throw (ex-info "Document changed before region edit" {:type ::conflict})))
  (let [before (markdown/parse source)
        projected (project document-id before)
        all (mapcat #(tree-seq (comp seq :children) :children %) (:blocks projected))
        block (first (filter #(= block-id (:id %)) all))
        region (when (and (integer? region-index) (<= 0 region-index)
                           (< region-index (count (:regions block))))
                 (nth (:regions block) region-index))]
    (when-not (and (= :ready (:status projected)) region (string? replacement))
      (throw (ex-info "No editable block region at this revision" {:type ::invalid})))
    (let [patched (markdown/patch source expected-revision [(assoc region :text replacement)])
          result (project document-id patched)
          identity-tree (fn identity-tree [blocks]
                          (mapv (fn [b] [(:id b) (identity-tree (:children b))])
                                (filter #(= :explicit (:identity-status %)) blocks)))]
      (when-not (and (= (:frontmatter before) (:frontmatter patched))
                     (= :ready (:status result))
                     (= (identity-tree (:blocks projected)) (identity-tree (:blocks result))))
        (throw (ex-info "Region edit changes explicit block structure" {:type ::invalid})))
      patched)))

(defn entity-tx
  "Source-free semantic structure for an immutable document-view DB. These
   identities are separate from disposable Markdown syntax nodes."
  [{:keys [id from to ordinal identity-status children]}]
  (cond-> {:document.block/id id :document.block/from (long from) :document.block/to (long to)
           :document.block/ordinal (long ordinal) :document.block/identity-status identity-status}
    (seq children) (assoc :document.block/children (mapv entity-tx children))))
