(ns is.simm.documents.chart
  "One-snapshot saved Markdown chart projection. The HOST supplies an authorized
   native immutable DB and retains its lifetime; this module grants no access."
  (:require [geschichte.repo :as repo]
            [is.simm.documents.canonical :as canonical]
            [is.simm.documents.chart-profile :as feature]
            [is.simm.documents.chart-spec :as spec]
            [is.simm.documents.files :as files]
            [is.simm.documents.metadata :as metadata]
            [is.simm.documents.properties :as properties]))

(defn- refuse! [reason]
  (throw (ex-info "Saved chart projection was refused" {:type ::refused :reason reason})))

(defn inspect-in
  "Resolve an explicit saved semantic block and its declared spec path from ONE
   immutable DB. Expected source identities are preconditions, not selectors.
   No draft, arbitrary spec path, expression, callback or foreign repository."
  [db {:keys [path block expected revision] :as request}]
  (when-not (and (= #{:path :block :expected :revision} (set (keys request)))
                 (uuid? block) (uuid? revision))
    (refuse! :invalid-request))
  (let [profile (feature/inspect db)]
    (when-not (= :ready (:status profile)) (refuse! :feature-unavailable))
    (let [file (files/read-file-in db path)
          analysis (:analysis file)]
      (when-not (and (= expected (:entry file)) (= revision (:revision analysis)))
        (refuse! :source-changed))
      (when-not (and (= :markdown (:kind file)) (= repo/default-mode (:mode expected))
                     (= :explicit (:identity-status analysis)) (empty? (:diagnostics analysis)))
        (refuse! :invalid-or-provisional-document))
      (let [explicit (into {} (for [node (mapcat #(tree-seq (comp seq :children) :children %)
                                                  (get-in analysis [:semantic :blocks]))
                                    :when (= :explicit (:identity-status node))]
                                [(:id node) node]))
            _ (when-not (contains? explicit block) (refuse! :missing-explicit-block))
            decoded (metadata/decode (get-in analysis [:frontmatter :properties]) (set (keys explicit)))
            _ (when (= :invalid (:status decoded)) (refuse! :invalid-declarations))
            scope (get-in decoded [:blocks block] {})
            _ (when-not (some #{feature/type-id} (:types scope)) (refuse! :not-a-chart))
            compiled (canonical/compile-declaration db (canonical/declaration :block scope))
            candidate (properties/decode-candidate compiled (:values scope {}))
            _ (when (or (seq (:diagnostics candidate)) (seq (:claims candidate)))
                (refuse! :unsupported-block-properties))
            spec-path (get-in candidate [:candidate-values feature/storage-attribute])
            _ (files/path! spec-path)
            selected (files/entry db spec-path)]
        (when-not (and (map? selected) (= repo/default-mode (:mode selected))
                       (integer? (:size selected)) (<= 0 (:size selected) spec/max-bytes))
          (refuse! :missing-or-oversized-spec))
        (let [chart (spec/decode (files/source-bytes db spec-path selected))]
          {:status :ready :path path :document (:document-id analysis)
           :block block :expected expected :revision revision
           :scope-revision (:revision compiled) :feature profile
           :spec-path spec-path :spec-entry selected :chart chart})))))
