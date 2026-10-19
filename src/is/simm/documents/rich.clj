(ns is.simm.documents.rich
  "Host validation for one source-preserving rich region edit."
  (:require [is.simm.documents.markdown :as markdown]
            [is.simm.documents.blocks :as blocks]
            [is.simm.documents.identity :as identity]
            [is.simm.documents.rich-text :as rich]))

(defn apply-edit [repository-id path source expected-revision from to model]
  (let [before (markdown/parse source)
        document-id (:id (identity/document repository-id path before))
        semantic (when document-id (blocks/project document-id before))
        node (first (filter #(and (= from (:from %)) (= to (:to %))) (:nodes before)))
        baseline (when node (rich/model node))]
    (when-not (= expected-revision (:revision before))
      (throw (ex-info "Draft changed before the rich edit could be applied" {:type ::conflict})))
    (when-not (and (= :ready (:status semantic)) baseline)
      (throw (ex-info "This region requires Source editing" {:type ::unsupported})))
    (let [serialized (rich/markdown (rich/normalize model))
          roundtrip (mapv rich/model (:nodes (markdown/parse serialized)))
          decoded {:type "doc" :content (vec (mapcat :content roundtrip))}
          _ (when-not (and (every? some? roundtrip)
                            (= (rich/normalize model) (rich/normalize decoded)))
              (throw (ex-info "This formatting does not round-trip yet; use Source or simplify the region"
                              {:type ::unsupported})))
          source (rich/apply-region source {:from from :to to :model baseline} model)
          after (markdown/parse source)
          next-semantic (blocks/project document-id after)
          identity-tree (fn identity-tree [bs]
                          (mapv (fn [b] [(:id b) (identity-tree (:children b))])
                                (filter #(= :explicit (:identity-status %)) bs)))]
      (when-not (and (= (:frontmatter before) (:frontmatter after))
                     (= :ready (:status next-semantic))
                     (= (identity-tree (:blocks semantic)) (identity-tree (:blocks next-semantic))))
        (throw (ex-info "Rich edits cannot change document metadata or explicit block structure"
                        {:type ::invalid})))
      source)))

(defn apply-edits
  "Validate a bounded, disjoint set of original source spans. Descending edits
   leave lower offsets unchanged; metadata/block invariants are checked per edit."
  [repository-id path source revision edits]
  (when-not (and (string? source) (vector? edits) (<= (count edits) 200)
                 (<= (* (count source) (max 1 (count edits))) (* 8 1024 1024))
                 (= revision (markdown/revision source)))
    (throw (ex-info "Invalid rich document edit set" {:type ::invalid})))
  (let [sorted (sort-by :from > edits)]
    (loop [remaining sorted upper (count source)]
      (when-let [{:keys [from to] :as edit} (first remaining)]
        (when-not (and (= #{:from :to :model} (set (keys edit)))
                       (integer? from) (integer? to) (<= 0 from to upper) (< from to))
          (throw (ex-info "Rich edit spans must be disjoint original regions" {:type ::invalid})))
        (recur (next remaining) from)))
    (reduce (fn [text {:keys [from to model]}]
              (apply-edit repository-id path text (markdown/revision text) from to model)) source sorted)))
