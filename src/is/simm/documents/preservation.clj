(ns is.simm.documents.preservation
  "Versioned conservative policy for automated wiki maintenance, NOT a general
   editor restriction, canonical schema validator or publication certificate."
  (:require [is.simm.documents.blocks :as blocks]
            [is.simm.documents.identity :as identity]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.metadata :as metadata]))

(def profile {:id :simmis/wiki-metadata-preservation :version 1})

(defn- decoded [document]
  ;; Documents come from the trusted bounded Markdown parser, not a transport
  ;; DTO asserting that somebody already validated its syntax or definitions.
  (when (and (= markdown/syntax-profile (:syntax-profile document))
             (vector? (:nodes document)) (empty? (:diagnostics document)))
    (let [raw (get-in document [:frontmatter :properties])
          id (identity/canonical-uuid (get raw "simmis-id"))]
      (when id
        (let [semantic (blocks/project id document)
              ids (into #{} (comp (mapcat #(tree-seq (comp seq :children) :children %))
                                  (filter #(= :explicit (:identity-status %))) (map :id))
                        (:blocks semantic))
              result (metadata/decode raw ids)]
          (when (and (= :ready (:status semantic))
                     (#{:plain :ready} (:status result)))
            (assoc result :explicit-blocks ids)))))))

(defn- scope-diagnostics [before after path]
  (let [old-bindings (:bindings before) new-bindings (:bindings after)
        old-values (:values before) new-values (:values after)]
    (vec
     (concat
      (when-not (every? (set (:types after)) (:types before))
        [{:kind :removed-type-assignment :path path}])
      (for [[alias binding] old-bindings :when (not= binding (get new-bindings alias))]
        {:kind :changed-property-binding :path (conj path alias)})
      (for [[alias value] old-values
            :when (if (contains? old-bindings alias)
                    (not (contains? new-values alias))
                    (or (not (contains? new-values alias)) (not= value (get new-values alias))))]
        {:kind (if (contains? old-bindings alias) :removed-property-value :changed-unknown-metadata)
         :path (conj path alias)})
      ;; Mutable keys come from BEFORE, not proposed declarations. Otherwise
      ;; adding a binding could launder an unknown field into mutable data.
      (for [alias (keys new-bindings)
            :when (and (not (contains? old-bindings alias)) (contains? old-values alias))]
        {:kind :reinterpreted-unbound-property :path (conj path alias)})))))

(defn check
  "Compare two trusted bounded parsed Markdown documents, preserving parsed
   metadata semantics and explicit identities. Types/bindings may be added;
   existing values may change where independent canonical validation permits.
   Missing values differ from false, zero and explicit null. Existing unknown
   metadata is immutable to this workflow, including nested values.

   This does NOT preserve comments, formatting or body text, prove truth, query
   trusted schema, authorize a write or establish an unchanged reviewed basis.
   Explicit human editing may intentionally remove/rebind things this policy
   refuses. New files need canonical validation, not a fictitious empty BEFORE."
  [{:keys [before after] :as request}]
  (let [old (when (and (map? request) (= #{:before :after} (set (keys request)))) (decoded before))
        new (when old (decoded after))
        diagnostics
        (if-not (and old new)
          [{:kind :invalid-preservation-input}]
          (vec
           (concat
            (when-not (= (:document-id old) (:document-id new))
              [{:kind :changed-document-identity}])
            (when-not (every? (:explicit-blocks new) (:explicit-blocks old))
              [{:kind :removed-explicit-block-identity}])
            (scope-diagnostics (:page old) (:page new) [:page])
            (mapcat (fn [[id scope]]
                      (if-let [next-scope (get (:blocks new) id)]
                        (scope-diagnostics scope next-scope [:block id])
                        [{:kind :removed-block-metadata :path [:block id]}]))
                    (:blocks old)))))]
    {:profile profile :status (if (seq diagnostics) :invalid :valid)
     :diagnostics diagnostics}))
