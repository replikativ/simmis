(ns is.simm.documents.property-model
  "HOST read/preparation model for source-backed properties. Reads ONE supplied
   immutable native DB; no target resolution, authority, callbacks or writes."
  (:require [clojure.string :as str]
            [geschichte.repo :as repo]
            [is.simm.documents.canonical :as canonical]
            [is.simm.documents.files :as files]
            [is.simm.documents.metadata :as metadata]
            [is.simm.documents.properties :as properties]
            [is.simm.documents.property-source :as property-source]
            [is.simm.documents.space :as space]
            [is.simm.documents.view :as view]))

(def profile {:id :simmis/source-property-model :version 1})

(defn- refuse! [reason]
  (throw (ex-info "Source property action was refused" {:type ::refused :reason reason})))

(defn- target! [target]
  (when-not (and (map? target)
                 (or (= {:kind :page} target)
                     (and (= #{:kind :id} (set (keys target)))
                          (= :block (:kind target)) (uuid? (:id target)))))
    (refuse! :invalid-scope-target))
  target)

(defn- capture [db path target]
  (target! target)
  (when-not (space/provisioned? db) (refuse! :unprovisioned-canonical-space))
  (let [file (files/read-file-in db path)
        analysis (:analysis file)]
    (when-not (and (= :markdown (:kind file)) (= repo/default-mode (get-in file [:entry :mode]))
                   (= :explicit (:identity-status analysis))
                   (empty? (:diagnostics analysis)))
      (refuse! :invalid-or-provisional-document))
    (let [blocks (into {} (for [block (mapcat #(tree-seq (comp seq :children) :children %)
                                              (get-in analysis [:semantic :blocks]))
                               :when (= :explicit (:identity-status block))]
                           [(:id block) block]))
          decoded (metadata/decode (get-in analysis [:frontmatter :properties]) (set (keys blocks)))
          _ (when (= :invalid (:status decoded)) (refuse! :invalid-declarations))
          _ (when (and (= :block (:kind target)) (not (contains? blocks (:id target))))
              (refuse! :missing-explicit-block))
          scope (if (= :page (:kind target)) (:page decoded)
                    (get-in decoded [:blocks (:id target)] {}))
          compiled (canonical/compile-declaration db (canonical/declaration (:kind target) scope))
          candidate (if (= :page (:kind target))
                      (canonical/page-candidate compiled (:values scope {}) path)
                      (properties/decode-candidate compiled (:values scope {})))]
      {:file file :target target :compiled compiled :values (:values scope {})
       :candidate candidate
       :values-path (if (= :page (:kind target)) [] ["simmis" "blocks" (str (:id target)) "values"])})))

(defn- scalar-type? [type]
  (contains? #{:db.type/string :db.type/boolean :db.type/long :db.type/double
               :db.type/uuid :db.type/instant :db.type/ref} type))

(defn- restriction [binding alias values]
  (let [m (:morphism binding)]
    (cond
      (not (:owned? binding)) :owner-managed-property
      (get-in binding [:schema :db/isComponent]) :component-property
      (or (:morphism/validation m) (get-in m [:morphism/dst :object/base-type])
          (seq (get-in m [:morphism/dst :object/enum-values]))) :property-needs-validator
      (not= :one (:cardinality binding)) :structured-source-edit-required
      (not (scalar-type? (get-in binding [:schema :db/valueType]))) :unsupported-value-type
      (nil? alias) :missing-source-binding
      (not (contains? values alias)) :missing-source-value)))

(defn- fields [{:keys [file target compiled values values-path]}]
  (let [aliases (into {} (for [[alias binding] (:bindings compiled)]
                          [(get-in binding [:morphism :entity/uuid]) alias]))
        parsed (assoc (:analysis file) :source (:text file))]
    (mapv
     (fn [[id binding]]
       (let [m (:morphism binding) alias (get aliases id)
             generated? (and (= :page (:kind target)) (= id (:title-property canonical/profile)))
             reason (or (when generated? :generated-property)
                        (restriction binding alias values)
                        (try (property-source/scalar-span parsed (conj values-path alias)) nil
                             (catch clojure.lang.ExceptionInfo _ :source-edit-required)))]
         {:id id :name (:entity/name m) :label (or alias (last (str/split (:entity/name m) #"/")))
          :declaring-type (get-in m [:morphism/src :entity/uuid]) :alias alias
          :value (if generated? (:path file) (when alias (get values alias)))
          :present? (boolean (or generated? (and alias (contains? values alias))))
          :required? (false? (:morphism/optional? m)) :cardinality (:cardinality binding)
          :value-type (get-in binding [:schema :db/valueType]) :render-hint (:morphism/property-type m)
          :editable? (nil? reason) :reason reason}))
     (:properties compiled))))

(defn inspect-in
  "Safe field DTOs from actual source/trusted definitions, not executable
   definitions or a permission grant. Host must authorize/own DB and pin blobs."
  [db path target]
  (let [{:keys [file compiled candidate] :as captured} (capture db path target)]
    {:profile profile :path path :target target :expected (:entry file)
     :revision (get-in file [:analysis :revision]) :scope-revision (:revision compiled)
     :types (mapv (fn [[id row]] {:id id :name (:entity/name row)}) (:types compiled))
     :fields (fields captured)
     :diagnostics (:diagnostics candidate)}))

(defn prepare-edit-in
  "Closed scalar edit preparation from ONE captured Save-time DB. Re-derive
   all bindings from its source, compare exact file/source/scope stamps, patch
   one existing scalar and validate the WHOLE prospective canonical selection.
   Returns source for the EXISTING writer, never a tx/DB or saved receipt.
   Host must publish with this SAME snapshot's validated-basis and fresh auth."
  [db options {:keys [path target alias property expected revision scope-revision value] :as request}]
  (when-not (and (map? request)
                 (= #{:path :target :alias :property :expected :revision :scope-revision :value}
                    (set (keys request)))
                 (string? alias) (uuid? property) (uuid? revision) (uuid? scope-revision))
    (refuse! :invalid-edit))
  (let [{:keys [file compiled values values-path]} (capture db path target)
        binding (get-in compiled [:bindings alias])]
    (when-not (and (= expected (:entry file)) (= revision (get-in file [:analysis :revision]))
                   (= scope-revision (:revision compiled)))
      (refuse! :stale-property-source-or-schema))
    (when-not (= property (get-in binding [:morphism :entity/uuid]))
      (refuse! :property-binding-changed))
    (when (= property (:title-property canonical/profile)) (refuse! :generated-property))
    (when-let [reason (restriction binding alias values)] (refuse! reason))
    (let [patched (property-source/replace-scalar (:text file) revision (conj values-path alias) value)
          preview (view/validate-replacement db options {:path path :expected expected :source (:source patched)})]
      (when-not (= :valid (:status preview))
        (throw (ex-info "Property edit violates the prospective knowledge selection"
                        {:type ::refused :reason :invalid-selection :diagnostics (:diagnostics preview)})))
      {:path path :expected expected :text (:source patched)})))
