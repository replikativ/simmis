(ns is.simm.documents.constraints
  "Whole-selection validation of trusted decoder candidates, not an RPC or
   authorization boundary. No source writes, stubs or transaction construction."
  (:require [datahike.api :as d]
            [is.simm.documents.properties :as properties]))

(def profile {:id :simmis/markdown-selection-constraints :version 1})
(def ^:private max-entities 20000)
(def ^:private max-work 50000)

(defn- refuse! [reason]
  (throw (ex-info "Canonical property constraints refused" {:type ::refused :reason reason})))

(defn- eid [db id]
  (:e (first (d/datoms db :avet :entity/uuid id))))

(defn- role? [db entity-id type-id]
  (let [type-eid (eid db type-id)]
    (and type-eid (seq (d/datoms db :eavt entity-id :instance/of-role type-eid)))))

(defn- codomain! [db id]
  (let [type-eid (eid db id)
        meta-eid (:e (first (d/datoms db :avet :entity/name "S/EntityType")))
        row (when type-eid (d/pull db [:object/primitive? :object/of-category] type-eid))]
    (when-not (and type-eid meta-eid (:object/of-category row)
                   (not (true? (:object/primitive? row)))
                   (seq (d/datoms db :eavt type-eid :instance/of-role meta-eid)))
      (refuse! :invalid-relation-codomain))))

(defn- input! [schema entities]
  (when-not (vector? entities) (refuse! :invalid-candidate-selection))
  (when (> (count entities) max-entities) (refuse! :too-many-candidate-entities))
  (when-not (= (count entities) (count (set (map :id entities))))
    (refuse! :duplicate-canonical-identity))
  (let [work
        (reduce
         (fn [work {:keys [id types candidate]}]
           (when-not (and (uuid? id) (set? types) (seq types) (<= (count types) 64) (every? uuid? types)
                          (= properties/candidate-profile (:profile candidate))
                          (= :pending (:status candidate)) (not (contains? candidate :values))
                          (map? (:candidate-values candidate)) (vector? (:claims candidate))
                          (<= (count (:candidate-values candidate)) 65) (<= (count (:claims candidate)) 130)
                          (empty? (:diagnostics candidate)))
             (refuse! :invalid-or-diagnosed-candidate))
           (let [value-work (reduce-kv
                             (fn [n attr value]
                               (let [definition (get schema attr)
                                     many? (= :db.cardinality/many (:db/cardinality definition))]
                                 (when-not (and definition (some? value)
                                                (not= :db.type/ref (:db/valueType definition))
                                                (or (not many?)
                                                    (and (vector? value) (<= (count value) 1000))))
                                   (refuse! :invalid-normalized-candidate-value))
                                 (+ n (if many? (count value) 1))))
                             0 (:candidate-values candidate))
                 work (+ work 1 (count types) value-work
                         (count (:claims candidate))
                         (reduce + 0 (map (fn [claim]
                                            (case (:kind claim)
                                              :property-reference
                                              (do (when-not (and (vector? (:targets claim))
                                                                 (<= (count (:targets claim)) 1000)
                                                                 (every? uuid? (:targets claim))
                                                                 (uuid? (:codomain claim)))
                                                    (refuse! :invalid-reference-claim))
                                                  (count (:targets claim)))
                                              :unique-property
                                              (do (when-not (and (vector? (:items claim)) (<= (count (:items claim)) 1000))
                                                    (refuse! :invalid-unique-claim))
                                                  (count (:items claim)))
                                              (refuse! :unknown-validation-claim)))
                                          (:claims candidate))))]
             (when (> work max-work) (refuse! :too-much-candidate-work))
             work)) 0 entities)]
    work))

(defn- target! [db table id codomain]
  (if-let [planned (get table id)]
    (do (when-not (contains? (:types planned) codomain) (refuse! :relation-target-type-mismatch))
        {:value (:tempid planned)})
    (let [target-eid (eid db id)]
      (when-not target-eid (refuse! :missing-relation-target))
      (when-not (role? db target-eid codomain) (refuse! :relation-target-type-mismatch))
      {:value target-eid :dependency {:uuid id :eid target-eid :codomain codomain}})))

(defn- reference-values [db schema table entity]
  (let [candidate (:candidate entity)
        claims (filterv #(= :property-reference (:kind %)) (:claims candidate))
        attrs (mapv :attribute claims)]
    (when-not (= (count attrs) (count (set attrs))) (refuse! :duplicate-reference-claim))
    (reduce
     (fn [{:keys [values] :as result} {:keys [attribute cardinality codomain targets]}]
       (when-not (and (= :db.type/ref (get-in schema [attribute :db/valueType]))
                      (= ({:one :db.cardinality/one :many :db.cardinality/many} cardinality)
                         (get-in schema [attribute :db/cardinality]))
                      (or (= :many cardinality) (= 1 (count targets)))
                      (not (contains? values attribute)))
         (refuse! :reference-storage-mismatch))
       (let [resolved (mapv #(target! db table % codomain) targets)]
         (-> result
             (assoc-in [:values attribute] (if (= :many cardinality) (mapv :value resolved) (:value (first resolved))))
             (assoc-in [:logical-references attribute] targets)
             (update :external into (keep :dependency resolved)))))
     {:values (:candidate-values candidate) :logical-references {} :external #{}} claims)))

(defn- unique-values [db schema table entity resolved seen]
  (let [claims (filterv #(= :unique-property (:kind %)) (get-in entity [:candidate :claims]))
        attrs (mapv :attribute claims)
        expected (set (filter #(get-in schema [% :db/unique]) (keys (:values resolved))))]
    (when-not (and (= (count attrs) (count (set attrs))) (= expected (set attrs)))
      (refuse! :missing-or-duplicate-unique-claim))
    (reduce
     (fn [{:keys [seen] :as result} {:keys [attribute reference? items uniqueness]}]
       (let [actual (get schema attribute)
             ref? (= :db.type/ref (:db/valueType actual))
             values (if ref? (get-in resolved [:logical-references attribute])
                        (if (= :db.cardinality/many (:db/cardinality actual))
                          (get-in resolved [:values attribute]) [(get-in resolved [:values attribute])]))]
         (when-not (and (contains? #{:db.unique/value :db.unique/identity} uniqueness)
                        (= uniqueness (:db/unique actual)) (= (boolean reference?) ref?)
                        (= items values) (not-any? nil? items))
           (refuse! :unique-storage-mismatch))
         (reduce
          (fn [{:keys [seen] :as result} value]
            (let [key [attribute value]
                  previous (get seen key)
                  planned? (and ref? (contains? table value))
                  base-value (if ref? (when-not planned? (eid db value)) value)]
              (when (and previous (not= previous (:id entity))) (refuse! :batch-unique-value-collision))
              (when (and (not planned?) (nil? base-value)) (refuse! :missing-unique-reference-target))
              (when (and (not planned?) (seq (d/datoms db :avet attribute base-value)))
                (refuse! :base-unique-value-collision))
              (-> result (assoc-in [:seen key] (:id entity))
                  (update :dependencies conj {:attribute attribute :logical-value value
                                              :base-value base-value :planned-target? (boolean planned?)}))))
          result items)))
     {:seen seen :dependencies #{}} claims)))

(defn resolve-selection
  "Validate ALL pending candidates before constructing any transaction. Input
   comes directly from the trusted typed compiler/decoder using this SAME
   immutable DB. Profile/status tags are not provenance or a certificate.
   Duplicate identities and
   aggregate budgets precede target lookup; planned targets receive explicit
   tempids. Returns resolved values and bounded freshness dependencies, never a
   DB or committed receipt. Refusal yields no partial result."
  [db entities]
  (let [schema (d/schema db)
        _ (input! schema entities)
        ordered (sort-by :id entities)
        table (into (sorted-map) (map-indexed (fn [index entity]
                                              [(:id entity) (assoc entity :tempid (- (inc index)))]) ordered))
        codomains (set (for [entity entities claim (get-in entity [:candidate :claims])
                              :when (= :property-reference (:kind claim))] (:codomain claim)))]
    (doseq [id (keys table)]
      (when (eid db id) (refuse! :existing-canonical-identity)))
    (doseq [id codomains] (codomain! db id))
    (let [result
          (reduce
           (fn [{:keys [seen] :as result} entity]
             (let [resolved (reference-values db schema table entity)
                   unique (unique-values db schema table entity resolved seen)]
               (-> result
                   (assoc :seen (:seen unique))
                   (update :entities conj (-> entity (dissoc :candidate)
                                              (assoc :values (:values resolved))))
                   (update-in [:dependencies :external-targets] into (:external resolved))
                   (update-in [:dependencies :unique-values] into (:dependencies unique)))))
           {:entities [] :seen {} :dependencies {:codomains codomains :external-targets #{} :unique-values #{}}}
           (vals table))]
      (-> result (dissoc :seen) (assoc :profile profile :status :ready)))))

(defn current?
  "Recheck only captured external targets and unique claims. The caller must
   separately check source identities, codecs and compiled schema. Own-derived
   skips uniqueness because the exact immutable projection contains its facts."
  [db dependencies own-derived?]
  (try
    (when-not (and (boolean? own-derived?) (map? dependencies)
                   (= #{:codomains :external-targets :unique-values} (set (keys dependencies)))
                   (every? set? (map dependencies [:codomains :external-targets :unique-values]))
                   (<= (reduce + (map count (vals dependencies))) max-work)
                   (every? uuid? (:codomains dependencies))
                   (every? (fn [{:keys [uuid eid codomain]}]
                             (and (uuid? uuid) (integer? eid) (pos? eid) (uuid? codomain)))
                           (:external-targets dependencies))
                   (every? (fn [{:keys [attribute base-value planned-target? logical-value]}]
                             (and (keyword? attribute) (boolean? planned-target?)
                                  (if planned-target? (and (uuid? logical-value) (nil? base-value))
                                      (some? base-value))))
                           (:unique-values dependencies)))
      (refuse! :invalid-validation-dependencies))
    (doseq [id (:codomains dependencies)] (codomain! db id))
    (and (every? (fn [{:keys [uuid codomain] target-eid :eid}]
                   (and (= target-eid (eid db uuid))
                        (role? db target-eid codomain))) (:external-targets dependencies))
         (or (true? own-derived?)
             (every? (fn [{:keys [attribute base-value planned-target?]}]
                       (or planned-target?
                           (and (some? base-value) (empty? (d/datoms db :avet attribute base-value)))))
                     (:unique-values dependencies))))
    (catch clojure.lang.ExceptionInfo error
      (if (= ::refused (:type (ex-data error))) false (throw error)))))
