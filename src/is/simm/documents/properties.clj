(ns is.simm.documents.properties
  "Typed frontmatter projection against captured, trusted categorical schema.
   Files never install attributes or execute property computations. Reflected
   domain properties remain owned by their existing governed writer."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [hasch.core :as hasch]
            [is.simm.model.morphism :as morphism]))

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
   :morphism/cardinality :morphism/optional? {:morphism/src [:entity/uuid :entity/name]}
   {:morphism/dst [:entity/uuid :entity/name]}])

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

(defn decode
  "Preserve all raw/unknown metadata. Only well-typed, host-bound ordinary
   property values become facts. No string-to-number/boolean guessing, lookup
   queries, formula evaluation or reflected-domain writes occur here."
  [compiled raw]
  (reduce-kv
   (fn [result name value]
     (if-let [{:keys [attr schema cardinality owned? morphism]} (get-in compiled [:bindings name])]
       (if (nil? value)
         result
         (let [type (:db/valueType schema)
               items (if (= :many cardinality) (when (sequential? value) value) [value])
               bounded? (and items (<= (count (take 1001 items)) 1000))
               decoded (when bounded? (mapv #(scalar type %) items))
               problem (cond
                         (not owned?) :owner-managed-property
                         (:db/isComponent schema) :component-property
                         (:db/unique schema) :unique-property-needs-validation
                         (:morphism/validation morphism) :property-needs-validator
                         (= type :db.type/ref) :unresolved-property-reference
                         (and items (not bounded?)) :too-many-property-values
                         (or (nil? decoded) (some nil? decoded)) :invalid-property-value)]
           (if problem
             (update result :diagnostics conj {:kind problem :property name})
             (assoc-in result [:values attr]
                       (if (= :many cardinality) (mapv :value decoded) (:value (first decoded)))))))
       (assoc-in result [:unknown name] value)))
   {:raw raw :unknown {} :values {}
    :diagnostics (vec (for [[name binding] (:bindings compiled)
                            :when (and (false? (get-in binding [:morphism :morphism/optional?]))
                                       (let [v (get raw name)]
                                         (or (nil? v) (and (sequential? v) (empty? v)))))]
                        {:kind :missing-required-property :property name}))}
   (or raw {})))
