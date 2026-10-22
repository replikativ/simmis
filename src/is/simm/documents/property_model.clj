(ns is.simm.documents.property-model
  "HOST read/preparation model for source-backed properties. Reads ONE supplied
   immutable native DB; no target resolution, authority, callbacks or writes."
  (:require [clojure.string :as str]
            [datahike.api :as d]
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
      {:file file :target target :compiled compiled :scope scope :values (:values scope {})
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
             reference? (= :db.type/ref (get-in binding [:schema :db/valueType]))
             span (try ((if reference? property-source/reference-span property-source/scalar-span)
                        parsed (conj values-path alias))
                       (catch clojure.lang.ExceptionInfo _ nil))
             reason (or (when generated? :generated-property)
                        (restriction binding alias values)
                        (when-not span :source-edit-required))]
         {:id id :name (:entity/name m) :label (or alias (last (str/split (:entity/name m) #"/")))
          :declaring-type (get-in m [:morphism/src :entity/uuid]) :alias alias
          :value (if generated? (:path file) (when alias (get values alias)))
          ;; Exact text for controls: serializing a Long to a browser number
          ;; before filling an input would already lose precision.
          :input (let [value (cond generated? (:path file)
                                  (and reference? span) (:id span)
                                  alias (get values alias))]
                   (if (nil? value) {:null true}
                       {:text (if (instance? java.util.Date value)
                                (str (.toInstant ^java.util.Date value)) (str value))}))
          :present? (boolean (or generated? (and alias (contains? values alias))))
          :required? (false? (:morphism/optional? m)) :cardinality (:cardinality binding)
          :value-type (get-in binding [:schema :db/valueType]) :render-hint (:morphism/property-type m)
          :relation (when (and reference? span)
                      {:representation (:representation span) :target (:id span) :path-hint (:path-hint span)
                       :codomain (get-in m [:morphism/dst :entity/uuid])
                       :clearable? (and (= :scalar (:representation span)) (not (false? (:morphism/optional? m))))})
          :editable? (nil? reason) :reason reason}))
     (:properties compiled))))

(defn inspect-in
  "Safe field DTOs from actual source/trusted definitions, not executable
   definitions or a permission grant. Host must authorize/own DB and pin blobs."
  [db path target]
  (let [{:keys [file compiled candidate scope] :as captured} (capture db path target)]
    {:profile profile :path path :target target :expected (:entry file)
     :revision (get-in file [:analysis :revision]) :scope-revision (:revision compiled)
     :assigned-types (:types scope [])
     :types (mapv (fn [[id row]] {:id id :name (:entity/name row)}) (:types compiled))
     :fields (fields captured)
     :diagnostics (:diagnostics candidate)}))

(defn input-value
  "Parse a closed UI input using a HOST-derived storage type. No EDN reader,
   evaluation or JavaScript numeric roundtrip. Null is distinct from empty text.
   This is decoding only, not schema validation or publication authority."
  [value-type input]
  (when-not (and (map? input)
                 (or (= {:null true} input)
                     (and (= #{:text} (set (keys input))) (string? (:text input))
                          (<= (count (:text input)) (* 1024 1024)))))
    (refuse! :invalid-property-input))
  (when-not (= {:null true} input)
    (let [text (:text input)]
      (try
        (case value-type
          :db.type/string text
          :db.type/boolean (case text "true" true "false" false (refuse! :invalid-property-input))
          :db.type/long (if (re-matches #"[+-]?[0-9]+" text) (Long/parseLong text)
                            (refuse! :invalid-property-input))
          :db.type/double (if (re-matches #"[+-]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:[eE][+-]?[0-9]+)?" text)
                            (let [value (Double/parseDouble text)]
                              (when-not (Double/isFinite value) (refuse! :invalid-property-input))
                              value)
                            (refuse! :invalid-property-input))
          (:db.type/uuid :db.type/ref)
          (if (= text (str (java.util.UUID/fromString text))) text (refuse! :invalid-property-input))
          :db.type/instant (do (java.util.Date/from (java.time.Instant/parse text)) text)
          (refuse! :unsupported-value-type))
        (catch IllegalArgumentException _ (refuse! :invalid-property-input))
        (catch java.time.DateTimeException _ (refuse! :invalid-property-input))))))

(defn- prepare-in
  [db options payload {:keys [path target alias property expected revision scope-revision] :as request}]
  (when-not (and (map? request)
                 (= (conj #{:path :target :alias :property :expected :revision :scope-revision} payload)
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
    (let [value (if (= :input payload) (input-value (get-in binding [:schema :db/valueType]) (:input request))
                    (:value request))
          patched ((if (= :db.type/ref (get-in binding [:schema :db/valueType]))
                     property-source/replace-reference property-source/replace-scalar)
                   (:text file) revision (conj values-path alias) value)
          preview (view/validate-replacement db options {:path path :expected expected :source (:source patched)})]
      (when-not (= :valid (:status preview))
        (throw (ex-info "Property edit violates the prospective knowledge selection"
                        {:type ::refused :reason :invalid-selection :diagnostics (:diagnostics preview)})))
      {:path path :expected expected :text (:source patched)})))

(defn prepare-edit-in
  "Prepare one typed scalar from ONE Save-time DB. Re-derive source bindings,
   verify source/scope stamps and validate the whole prospective selection.
   Host must publish using this SAME snapshot's validated-basis and fresh auth."
  [db options request]
  (prepare-in db options :value request))

(defn prepare-input-in
  "UI equivalent of prepare-edit-in. Decode :input only AFTER re-deriving its
   actual trusted binding; never accept a client value type or EDN program."
  [db options request]
  (prepare-in db options :input request))

(defn- proposed-scope [db captured types additions input?]
  (when-not (and (vector? types) (<= (count types) 63) (every? uuid? types)
                 (= (count types) (count (set types)))
                 (vector? additions) (<= (count additions) 64)
                 (every? (fn [item]
                           (and (map? item)
                                (= (cond-> #{:alias :property :declaring-type} input? (conj :input))
                                   (set (keys item)))
                                (string? (:alias item)) (<= 1 (count (:alias item)) 128)
                                (not (str/blank? (:alias item)))
                                (not (#{"simmis" "simmis-id"} (:alias item)))
                                (uuid? (:property item)) (uuid? (:declaring-type item)))) additions)
                 (= (count additions) (count (set (map :alias additions)))))
    (refuse! :invalid-declaration-edit))
  (let [{:keys [scope target values]} captured
        opposite-role (if (= :page (:kind target)) (:block-role canonical/profile) (:page-role canonical/profile))
        _ (when (some #{opposite-role} types) (refuse! :incompatible-structural-role))
        bindings (reduce (fn [bindings {:keys [alias property declaring-type]}]
                           ;; Existing unknown metadata cannot silently acquire
                           ;; new meaning; author an explicit source edit instead.
                           (when (or (contains? bindings alias) (contains? values alias))
                             (refuse! :existing-binding-or-value))
                           (assoc bindings alias {:id property :declaring-type declaring-type}))
                         (:bindings scope {}) additions)
        compiled (canonical/compile-declaration db (canonical/declaration (:kind target)
                                                                          {:types types :bindings bindings}))]
    ;; Compilation refuses orphaned/duplicate bindings after type removal. No
    ;; deletion of aliases or values is smuggled into an assignment operation.
    compiled))

(defn- type-choices [db]
  (let [meta-type (:db/id (d/pull db [:db/id] [:entity/name "S/EntityType"]))
        _ (when-not meta-type (refuse! :missing-categorical-schema))
        ;; Bound acquisition BEFORE metadata pulls. This is a small-room picker,
        ;; not an unbounded cross-repository schema search.
        rows (vec (take 129 (d/datoms db :avet :instance/of-role meta-type)))]
    (when (> (count rows) 128) (refuse! :too-many-type-choices))
    (->> rows
         (map #(d/pull db [:entity/uuid :entity/name :object/primitive?] (:e %)))
         (filter #(and (uuid? (:entity/uuid %)) (string? (:entity/name %))
                       (not (:object/primitive? %))
                       (not (#{(:page-role canonical/profile) (:block-role canonical/profile)} (:entity/uuid %)))))
         (sort-by (juxt :entity/name :entity/uuid))
         (mapv #(hash-map :id (:entity/uuid %) :name (subs (:entity/name %) 0 (min 256 (count (:entity/name %)))))))))

(defn preview-declaration-in
  "Prepare inert field DTOs and an actual prospective definition digest from
   ONE supplied DB. No schema installation, publication, authority or lease.
   Required values may be absent at preview; Save must supply them together."
  [db path target {:keys [types additions] :as declaration}]
  (when-not (and (map? declaration) (= #{:types :additions} (set (keys declaration))))
    (refuse! :invalid-declaration-edit))
  (let [{:keys [file compiled] :as captured} (capture db path target)
        proposed (proposed-scope db captured types additions false)]
    {:path path :target target :expected (:entry file)
     :revision (get-in file [:analysis :revision]) :scope-revision (:revision compiled)
     :proposed-scope-revision (:revision proposed)
     :types types :additions additions
     :type-choices (type-choices db)
     :fields
     (mapv (fn [field]
             (if (some #(= (:property %) (:id field)) additions)
               (let [binding (get-in proposed [:properties (:id field)])
                     reason (or (when (= (:id field) (:title-property canonical/profile)) :generated-property)
                                ;; Only the expected missing-value/token state
                                ;; is bypassed for a newly requested binding.
                                (restriction binding (:alias field) {(:alias field) nil}))]
                 (assoc field :new? true :editable? (nil? reason) :reason reason))
               (cond-> field (= :missing-source-binding (:reason field)) (assoc :needs-binding? true))))
           (fields (assoc captured :compiled proposed)))}))

(defn prepare-declaration-in
  "Prepare a closed type-assignment/binding batch from ONE fresh Save-time DB.
   Both old and prospective actual definitions are stamped. New required values
   are validated with the whole selection before publication; no schema datoms
   are written or assumed to travel through a Geschichte proposal."
  [db options {:keys [path target expected revision scope-revision proposed-scope-revision types additions]
               :as request}]
  (when-not (and (map? request)
                 (= #{:path :target :expected :revision :scope-revision :proposed-scope-revision :types :additions}
                    (set (keys request)))
                 (every? uuid? [revision scope-revision proposed-scope-revision]))
    (refuse! :invalid-declaration-edit))
  (let [{:keys [file compiled values-path] :as captured} (capture db path target)
        _ (when-not (and (= expected (:entry file)) (= revision (get-in file [:analysis :revision]))
                         (= scope-revision (:revision compiled)))
            (refuse! :stale-property-source-or-schema))
        proposed (proposed-scope db captured types additions true)
        _ (when-not (= proposed-scope-revision (:revision proposed))
            (refuse! :stale-proposed-definitions))
        scope-path (if (= :page (:kind target)) ["simmis"] ["simmis" "blocks" (str (:id target))])
        original (:text file)
        changed? (or (not= types (get-in captured [:scope :types] [])) (seq additions))
        parsed (:analysis file)
        parsed (if (or (not changed?) (contains? (get-in parsed [:frontmatter :properties]) "simmis"))
                 parsed
                 (property-source/insert-value original revision ["simmis" "version"] 1))
        typed (if-not changed?
                parsed
                (property-source/set-types (or (:source parsed) original) (:revision parsed)
                                           (conj scope-path "types") (mapv str types)))
        patched
        (reduce (fn [parsed {:keys [alias property declaring-type input]}]
                  (let [binding (get-in proposed [:bindings alias])
                        ;; Same supported scalar/reference policy as existing
                        ;; property editing, but the binding/value are NEW.
                        reason (restriction binding alias {alias nil})]
                    (when reason (refuse! reason))
                    (when (= property (:title-property canonical/profile)) (refuse! :generated-property))
                    (let [value (input-value (get-in binding [:schema :db/valueType]) input)
                          bound (property-source/insert-value (or (:source parsed) original) (:revision parsed)
                                                              (conj scope-path "properties" alias)
                                                              {"id" (str property) "type" (str declaring-type)})]
                      (property-source/insert-value (:source bound) (:revision bound) (conj values-path alias) value))))
                typed additions)
        text (or (:source patched) original)
        preview (view/validate-replacement db options {:path path :expected expected :source text})]
    (when-not (= :valid (:status preview))
      (throw (ex-info "Declaration edit violates the prospective knowledge selection"
                      {:type ::refused :reason :invalid-selection :diagnostics (:diagnostics preview)})))
    {:path path :expected expected :text text}))
