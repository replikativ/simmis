(ns is.simm.documents.block-reference
  "Source-only, bounded block lookup in one immutable owned repository snapshot.
   No store selection, projection installation, execution or retained cache."
  (:require [datahike.api :as d]
            [geschichte.query :as query]
            [is.simm.documents.blocks :as blocks]
            [is.simm.documents.files :as files]
            [is.simm.documents.identity :as identity]
            [is.simm.documents.markdown :as markdown]))

(def max-files 512)
(def max-total-bytes (* 8 1024 1024))
(def max-total-nodes 100000)

(defn- refuse! [reason]
  (throw (ex-info "Block reference resolution refused" {:type ::refused :reason reason})))

(defn selection
  "Source-free descriptor of ALL current Markdown, not a path-hint fast hit.
   Comparing descriptors after a read detects additions, removals and renames
   without another parse or invalidation from unrelated JSON/accounting writes."
  [db]
  (let [repository (:geschichte.repo/id (query/repository db))
        rows (vec (take (inc files/max-index-entries) (d/datoms db :avet :geschichte.work/path)))]
    (when-not (uuid? repository) (refuse! :missing-repository))
    (when (> (count rows) files/max-index-entries) (refuse! :metadata-limit))
    (let [selected
          (reduce
           (fn [{:keys [entries bytes]} row]
             (let [path (:v row)]
               (if-not (= :markdown (files/file-kind path))
                 {:entries entries :bytes bytes}
                 (let [_ (files/path! path)
                       entry (files/entry db path)
                       size (:size entry)]
                   (when-not (and (uuid? (:content entry)) (integer? size) (<= 0 size markdown/max-source-bytes)
                                  (integer? (:mode entry)) (= 0100000 (bit-and 0170000 (:mode entry))))
                     (refuse! :unsupported-source))
                   (when (>= (count entries) max-files) (refuse! :file-limit))
                   (when (> (+ bytes size) max-total-bytes) (refuse! :byte-limit))
                   {:entries (assoc entries path entry) :bytes (+ bytes size)}))))
           {:entries (sorted-map) :bytes 0} rows)]
      {:repository-id repository :files (:entries selected)})))

(defn request!
  "Logical repository identity constrains the actual owned target; it never
   chooses a different repository/world. Paths are optional location hints."
  [{:keys [repository-id block-id path-hint] :as request}]
  (when-not (and (map? request)
                 (#{#{:repository-id :block-id} #{:repository-id :block-id :path-hint}} (set (keys request)))
                 (uuid? repository-id) (uuid? block-id))
    (refuse! :invalid-request))
  (when (contains? request :path-hint) (files/path! path-hint))
  request)

(defn scan-selected
  "HOST-only ephemeral parsed documents from an exact bounded source selection.
   ASTs/source never leave the owning read operation or become retained state."
  [db selected]
  (when-not (= selected (selection db)) (refuse! :selection-changed))
  (reduce-kv
   (fn [{:keys [documents nodes]} path entry]
     (let [source (markdown/decode (files/source-bytes db path entry))
           parsed (markdown/parse source)
           amount (count (take (inc (- max-total-nodes nodes))
                               (mapcat #(tree-seq (comp seq :children) :children %) (:nodes parsed))))
           _ (when (> (+ nodes amount) max-total-nodes) (refuse! :node-limit))
           document (identity/document (:repository-id selected) path parsed)
           semantic (when-let [id (:id document)] (blocks/project id parsed))]
       (when-not (and (empty? (:diagnostics document)) (= :ready (:status semantic)))
         (refuse! :invalid-source))
       {:nodes (+ nodes amount)
        :documents (conj documents {:path path :entry entry :document document
                                    :parsed parsed :semantic semantic})}))
   {:documents [] :nodes 0} (:files selected)))

(defn resolve-selected
  "Resolve globally, not from a stale index or the first matching hint.
   Invalid source prevents proof of uniqueness and therefore refuses lookup.
   The host owns the supplied snapshot/descriptor and its lifetime."
  [db selected request]
  (let [{:keys [repository-id block-id]} (request! request)]
    (when-not (= repository-id (:repository-id selected)) (refuse! :repository-changed))
    (let [scanned (scan-selected db selected)
          matches (into []
                        (mapcat (fn [{:keys [path entry parsed document semantic]}]
                                  (for [block (mapcat #(tree-seq (comp seq :children) :children %) (:blocks semantic))
                                        :when (and (= :explicit (:identity-status block)) (= block-id (:id block)))]
                                    {:path path :entry entry :document-id (:id document)
                                     :block-id block-id :revision (:revision parsed)
                                     :from (:from block) :to (:to block)})))
                        (:documents scanned))]
      (case (count matches)
        0 {:status :missing :repository-id repository-id :block-id block-id}
        1 (assoc (first matches) :status :resolved :repository-id repository-id)
        {:status :ambiguous :repository-id repository-id :block-id block-id}))))

(defn resolve-in [db request]
  (resolve-selected db (selection db) request))
