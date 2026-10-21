(ns is.simm.documents.properties
  "Typed frontmatter projection against captured, trusted categorical schema.
   Files never install attributes or execute property computations. Reflected
   domain properties remain owned by their existing governed writer."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [hasch.core :as hasch]
            [is.simm.documents.identity :as identity]
            [is.simm.model.morphism :as morphism]
            [is.simm.model.schema :as model-schema]))

(def profile {:id :simmis/markdown-properties :version 1})
(def ^:private system-namespaces
  #{"db" "entity" "block" "document" "geschichte" "morphism" "category"
    "object" "functor" "instance" "party" "room" "grant" "scope" "permission"
    "eacl" "kontor" "dvergr" "schedule" "wallet" "ledger"})

(defn- structural-attribute? [attr]
  ;; These namespaces are owned by source/projection/schema/authority systems,
  ;; not ordinary user properties. User categories need not be called S.
  (let [ns (or (namespace attr) "")]
    (some #(or (= ns %) (str/starts-with? ns (str % "."))) system-namespaces)))
(def ^:private binding-keys
  [:entity/uuid :entity/name :morphism/storage-attr :morphism/computation :morphism/validation
   :morphism/cardinality :morphism/optional? :morphism/attr-type :morphism/unique :morphism/property-type
   {:morphism/of-category [:entity/uuid :entity/name]}
   {:morphism/src [:entity/uuid :entity/name]}
   {:morphism/dst [:entity/uuid :entity/name :object/primitive? :object/enum-values
                   {:object/base-type [:entity/uuid :entity/name]}
                   {:object/of-category [:entity/uuid :entity/name]}]}])

(defn compile-schema
  "Bind portable YAML names to existing morphism UUIDs. Bindings are supplied
   by the authorized HOST, never taken from document declarations. The digest
   captures actual relevant schema facts, not only a caller's revision label."
  [db bindings]
  (when-not (and (map? bindings) (<= (count bindings) 64)
                 (every? (fn [[k id]] (and (string? k) (<= 1 (count k) 128)
                                           (not (#{"simmis" "simmis-id"} k)) (uuid? id))) bindings))
    (throw (ex-info "Invalid trusted property bindings" {:type ::invalid-schema})))
  (let [schema (d/schema db)
        pull-pattern (filterv #(contains? schema (if (map? %) (first (keys %)) %)) binding-keys)
        compiled
        (into (sorted-map)
              (for [[name id] bindings]
                (let [eid (when (contains? schema :entity/uuid)
                            (:e (first (d/datoms db :avet :entity/uuid id))))
                      m (when eid (d/pull db pull-pattern eid))
                      attr (when (:entity/name m) (morphism/attr-of m))
                      attr-schema (get schema attr)
                      cardinality ({:db.cardinality/one :one :db.cardinality/many :many}
                                   (:db/cardinality attr-schema))
                      owned? (and (not (morphism/reflected? m)) (nil? (:morphism/computation m))
                                   (= attr (morphism/name->attr-ident (:entity/name m))))]
                  (when-not (and (:entity/uuid m) attr-schema cardinality
                                 (= cardinality (:morphism/cardinality m)))
                    (throw (ex-info "Property binding does not match installed schema"
                                    {:type ::invalid-schema :property name})))
                  (when-let [declared (:morphism/unique m)]
                    (when-not (and (#{:db.unique/value :db.unique/identity} declared)
                                   (= declared (:db/unique attr-schema)))
                      (throw (ex-info "Property uniqueness does not match installed schema"
                                      {:type ::invalid-schema :property name}))))
                  ;; An apparent ordinary morphism must not smuggle values into
                  ;; source/projection/identity/authority internals. Reflected
                  ;; properties are retained for an explicit owner-managed
                  ;; diagnostic, but can never contribute decoded facts.
                  (when (and owned? (structural-attribute? attr))
                    (throw (ex-info "Property targets an internal or foreign attribute"
                                    {:type ::invalid-schema :property name})))
                  [name {:morphism m :attr attr :schema (select-keys attr-schema
                                                                   [:db/valueType :db/cardinality
                                                                    :db/unique :db/isComponent])
                         :owned? owned? :cardinality cardinality}])))]
    (when-not (= (count compiled) (count (set (map :attr (vals compiled)))))
      (throw (ex-info "Property aliases target the same attribute" {:type ::invalid-schema})))
    {:profile profile :bindings compiled :revision (hasch/uuid [profile compiled])}))

(defn- scalar [type value]
  (case type
    :db.type/string (when (string? value) {:value value})
    :db.type/boolean (when (boolean? value) {:value value})
    :db.type/long (when (and (integer? value) (<= Long/MIN_VALUE value Long/MAX_VALUE))
                    {:value (long value)})
    :db.type/double (when (and (number? value) (Double/isFinite (double value)))
                      {:value (double value)})
    :db.type/uuid (when (string? value)
                    (try (let [id (java.util.UUID/fromString value)]
                           (when (= value (str id)) {:value id}))
                         (catch IllegalArgumentException _ nil)))
    :db.type/instant (cond
                       (instance? java.util.Date value) {:value value}
                       (string? value) (try {:value (java.util.Date/from (java.time.Instant/parse value))}
                                            (catch java.time.format.DateTimeParseException _ nil)))
    nil))

(defn- reference-id [raw]
  (cond
    (string? raw) (identity/canonical-uuid raw)
    (and (map? raw) (every? #{"id" "path"} (keys raw))
         (or (not (contains? raw "path"))
             (and (string? (get raw "path")) (<= (count (get raw "path")) 512)
                  (not (re-find #"[\p{Cntrl}]" (get raw "path"))))))
    (identity/canonical-uuid (get raw "id"))))

(defn- field [binding value candidate?]
  (let [{:keys [schema cardinality owned? morphism]} binding
        type (:db/valueType schema)
        reference? (= type :db.type/ref)
        items (if (= :many cardinality) (when (sequential? value) value) [value])
        bounded? (and items (<= (count (take 1001 items)) 1000))
        decoded (when bounded?
                  (if (and candidate? reference?)
                    (mapv #(when-let [id (reference-id %)] {:value id}) items)
                    (mapv #(scalar type %) items)))
        problem (cond
                  (not owned?) :owner-managed-property
                  (:db/isComponent schema) :component-property
                  (and (:db/unique schema) (not candidate?)) :unique-property-needs-validation
                  (:morphism/validation morphism) :property-needs-validator
                  (get-in morphism [:morphism/dst :object/base-type]) :property-needs-validator
                  (seq (get-in morphism [:morphism/dst :object/enum-values])) :property-needs-validator
                  (and reference? (not candidate?)) :unresolved-property-reference
                  (and items (not bounded?)) :too-many-property-values
                  (or (nil? decoded) (some nil? decoded))
                  (if reference? :invalid-property-reference :invalid-property-value))]
    {:problem problem :reference? reference? :items (mapv :value decoded)}))

(defn- decode* [compiled raw candidate?]
  (reduce-kv
   (fn [result name value]
     (if-let [{:keys [attr schema cardinality morphism] :as binding} (get-in compiled [:bindings name])]
       (if (nil? value)
         result
         (let [{:keys [problem reference? items]} (field binding value candidate?)]
           (if problem
             (update result :diagnostics conj {:kind problem :property name})
             (cond-> result
               (not reference?)
               (assoc-in [:values attr] (if (= :many cardinality) items (first items)))
               (and candidate? reference?)
               (update :claims conj {:kind :property-reference :property name :attribute attr
                                     :property-id (:entity/uuid morphism) :cardinality cardinality
                                     :codomain (get-in morphism [:morphism/dst :entity/uuid]) :targets items})
               (and candidate? (:db/unique schema))
               (update :claims conj {:kind :unique-property :property name :attribute attr
                                     :property-id (:entity/uuid morphism) :reference? reference?
                                     :uniqueness (:db/unique schema) :items items})))))
       (assoc-in result [:unknown name] value)))
   (cond-> {:raw raw :unknown {} :values {}
    :diagnostics (vec (for [[name binding] (:bindings compiled)
                            :when (and (false? (get-in binding [:morphism :morphism/optional?]))
                                       (let [v (get raw name)]
                                         (or (nil? v) (and (sequential? v) (empty? v)))))]
                        {:kind :missing-required-property :property name}))}
     candidate? (assoc :claims []))
   (or raw {})))

(defn decode
  "Preserve raw/unknown metadata; strictly decode ordinary scalar properties.
   References/unique values still refuse here. No queries, evaluation or writes."
  [compiled raw]
  (decode* compiled raw false))

(def typed-profile {:id :simmis/markdown-typed-properties :version 1})

(defn- invalid-scope! [reason]
  (throw (ex-info "Markdown declarations do not match trusted definitions"
                  {:type ::invalid-schema :reason reason})))

(defn compile-scope
  "Compile one already-decoded metadata scope against ONE immutable trusted DB.
   Assignments are exact categorical entity types, not tags or inheritance.
   Enumerates ALL outgoing properties, including unaliased required fields.
   Does not install schema, resolve relation values, evaluate validators or
   publish entities. :revision includes the actual definitions and storage.
   Invalid/unavailable definitions refuse, rather than acquire file meaning."
  [db {:keys [types bindings]}]
  (when-not (and (vector? types) (<= (count types) 64)
                 (every? uuid? types) (= (count types) (count (set types)))
                 (map? bindings) (<= (count bindings) 64)
                 (every? (fn [[alias {:keys [id declaring-type]}]]
                           (and (string? alias) (<= 1 (count alias) 128)
                                (not (str/blank? alias))
                                (not (#{"simmis" "simmis-id"} alias))
                                (uuid? id) (uuid? declaring-type))) bindings))
    (invalid-scope! :invalid-declarations))
  (let [schema (d/schema db)
        _ (when-not (every? #(contains? schema %)
                            [:entity/uuid :entity/name :object/of-category
                             :instance/of-role :morphism/src :morphism/of-category])
            (invalid-scope! :missing-categorical-schema))
        meta-type (:e (first (d/datoms db :avet :entity/name "S/EntityType")))
        type-rows
        (into (sorted-map)
              (for [id types]
                (let [eid (:e (first (d/datoms db :avet :entity/uuid id)))
                      _ (when-not eid (invalid-scope! :unknown-or-nonentity-type))
                      row (d/pull db [:db/id :entity/uuid :entity/name :object/primitive?
                                     {:object/of-category [:db/id :entity/uuid :entity/name]}
                                     {:instance/of-role [:db/id]}]
                                  eid)]
                  (when-not (and meta-type (:entity/name row)
                                 (get-in row [:object/of-category :entity/uuid])
                                 (not (true? (:object/primitive? row)))
                                 (some #(= meta-type (:db/id %)) (:instance/of-role row)))
                    (invalid-scope! :unknown-or-nonentity-type))
                  [id row])))
        outgoing (vec (take 257 (mapcat (fn [[_ row]]
                                         (map :e (d/datoms db :avet :morphism/src (:db/id row))))
                                       type-rows)))
        _ (when (> (count outgoing) 256) (invalid-scope! :too-many-type-properties))
        definitions
        (into (sorted-map)
              (for [eid outgoing]
                (let [id (:entity/uuid (d/pull db [:entity/uuid] eid))
                      _ (when-not (uuid? id) (invalid-scope! :property-without-identity))
                      binding (get-in (compile-schema db {(str id) id}) [:bindings (str id)])
                      m (:morphism binding)
                      src (get-in m [:morphism/src :entity/uuid])
                      dst (:morphism/dst m)
                      category (get-in type-rows [src :object/of-category :entity/uuid])
                      attr-type (:morphism/attr-type m)
                      expected-type (or (model-schema/attr-type->db-type attr-type)
                                        (model-schema/codomain->db-type (:entity/name dst)))]
                  (when-not (= (get-in type-rows [src :object/of-category :entity/uuid])
                               (get-in m [:morphism/of-category :entity/uuid]))
                    (invalid-scope! :property-category-mismatch))
                  (when-not (and (:entity/uuid dst) (:entity/name dst)
                                 (= category (get-in dst [:object/of-category :entity/uuid])))
                    (invalid-scope! :invalid-property-codomain))
                  (when (and attr-type (not (contains? model-schema/attr-type->db-type attr-type)))
                    (invalid-scope! :unsupported-property-attr-type))
                  (when-not (= expected-type (get-in binding [:schema :db/valueType]))
                    (invalid-scope! :property-codomain-storage-mismatch))
                  [id binding])))
        compiled
        (into (sorted-map)
              (for [[alias {:keys [id declaring-type]}] bindings]
                (let [binding (get definitions id)]
                  (when-not (and (contains? type-rows declaring-type) binding
                                 (= declaring-type (get-in binding [:morphism :morphism/src :entity/uuid])))
                    (invalid-scope! :property-declaring-type-mismatch))
                  [alias binding])))]
    (when-not (= (count compiled) (count (set (map :attr (vals compiled)))))
      (invalid-scope! :ambiguous-property-alias))
    (when-not (= (count definitions) (count (set (map :attr (vals definitions)))))
      (invalid-scope! :ambiguous-type-storage))
    {:profile typed-profile :types type-rows :properties definitions :bindings compiled
     :revision (hasch/uuid [typed-profile type-rows definitions compiled])}))

(defn- scope-diagnostics [compiled decoded]
  (let [bound (set (map #(get-in % [:morphism :entity/uuid]) (vals (:bindings compiled))))]
    (update decoded :diagnostics
            (fn [diagnostics]
              (into (mapv (fn [diagnostic]
                            (if (and (= :missing-required-property (:kind diagnostic))
                                     (false? (get-in compiled [:bindings (:property diagnostic) :owned?])))
                              (assoc diagnostic :kind :required-owner-managed-property)
                              diagnostic)) diagnostics)
                    (for [[id binding] (:properties compiled)
                          :when (and (false? (get-in binding [:morphism :morphism/optional?]))
                                     (not (contains? bound id)))]
                      {:kind (if (:owned? binding) :missing-required-property :required-owner-managed-property)
                       :property-id id}))))))

(defn- scope-input! [compiled raw]
  (when-not (and (= typed-profile (:profile compiled)) (map? raw))
    (invalid-scope! :invalid-typed-values)))

(defn decode-scope
  "Strict scalar decoding, including required properties absent from aliases.
   References, unique values and unsupported constraints remain refusals."
  [compiled raw]
  (scope-input! compiled raw)
  (scope-diagnostics compiled (decode compiled raw)))

(def candidate-profile {:id :simmis/markdown-property-candidate :version 1})

(defn decode-candidate
  "Stage values for canonical preflight, NEVER final facts. Always :pending;
   :candidate-values excludes references, and explicit bounded :claims retain
   logical target UUIDs/unique values for the later whole-selection validator.
   No DB queries, target resolution, stubs, transaction data or allow-unique
   escape hatch. Unknown source stays intact; paths are bounded inert hints."
  [compiled raw]
  (scope-input! compiled raw)
  (let [decoded (scope-diagnostics compiled (decode* compiled raw true))]
    (-> decoded (dissoc :values)
        (assoc :profile candidate-profile :status :pending :candidate-values (:values decoded)))))
