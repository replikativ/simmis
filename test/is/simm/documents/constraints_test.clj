(ns is.simm.documents.constraints-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.db :as db]
            [is.simm.documents.constraints :as constraints]
            [is.simm.documents.properties :as properties]))

(def person #uuid "11111111-1111-4111-8111-111111111111")
(def other-type #uuid "22222222-2222-4222-8222-222222222222")
(def external #uuid "33333333-3333-4333-8333-333333333333")
(def a #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")
(def b #uuid "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb")
(def c #uuid "cccccccc-cccc-4ccc-8ccc-cccccccccccc")

(defn- fixture []
  (let [schema [{:db/ident :entity/uuid :db/valueType :db.type/uuid
                 :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
                {:db/ident :entity/name :db/valueType :db.type/string
                 :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
                {:db/ident :instance/of-role :db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
                {:db/ident :object/of-category :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
                {:db/ident :object/primitive? :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}]
        schema (into schema
                     (for [[attr type cardinality unique]
                           [[:Test/friend :db.type/ref :db.cardinality/one nil]
                            [:Test/friends :db.type/ref :db.cardinality/many nil]
                            [:Test/unique-friend :db.type/ref :db.cardinality/one :db.unique/identity]
                            [:Test/identity :db.type/string :db.cardinality/one :db.unique/identity]
                            [:Test/value :db.type/long :db.cardinality/one :db.unique/value]
                            [:Test/flag :db.type/boolean :db.cardinality/one :db.unique/value]
                            [:Test/counts :db.type/long :db.cardinality/many nil]]]
                       (cond-> {:db/ident attr :db/valueType type :db/cardinality cardinality}
                         unique (assoc :db/unique unique))))
        empty (db/empty-db nil {:schema-flexibility :write :keep-history? false})]
    (-> empty (d/db-with schema)
        (d/db-with [{:db/id -1 :entity/uuid (random-uuid) :entity/name "S"}
                    {:db/id -2 :entity/uuid (random-uuid) :entity/name "S/EntityType"
                     :object/of-category -1 :instance/of-role [-2]}
                    {:db/id -3 :entity/uuid person :entity/name "S/Person"
                     :object/of-category -1 :instance/of-role [-2]}
                    {:db/id -4 :entity/uuid other-type :entity/name "S/Other"
                     :object/of-category -1 :instance/of-role [-2]}
                    {:entity/uuid external :instance/of-role [-3]}]))))

(defn- candidate [base values]
  (let [bindings (into {}
                       (for [attr (keys values)
                             :let [id (java.util.UUID/nameUUIDFromBytes (.getBytes (str attr) java.nio.charset.StandardCharsets/UTF_8))]]
                         [(name attr) {:attr attr :schema (get (d/schema base) attr) :owned? true
                                       :cardinality (if (= :db.cardinality/many (get-in (d/schema base) [attr :db/cardinality])) :many :one)
                                       :morphism {:entity/uuid id :morphism/dst {:entity/uuid person}}}]))
        compiled {:profile properties/typed-profile :bindings bindings
                  :properties (into {} (map (fn [binding] [(get-in binding [:morphism :entity/uuid]) binding]) (vals bindings)))}]
    (properties/decode-candidate compiled (into {} (map (fn [[attr value]] [(name attr) value]) values)))))

(defn- entity [base id values]
  {:id id :types #{person} :candidate (candidate base values)})

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo error (:reason (ex-data error)))))

(defn- tx [result]
  (mapv (fn [{:keys [id tempid values]}]
          (merge values {:db/id tempid :entity/uuid id :instance/of-role [[:entity/uuid person]]}))
        (:entities result)))

(deftest forward-backward-and-cyclic-targets-have-explicit-tempids
  (let [base (fixture)
        result (constraints/resolve-selection base [(entity base a {:Test/friend (str b)})
                                                    (entity base b {:Test/friend (str a)})])
        by-id (into {} (map (juxt :id identity) (:entities result)))
        projected (d/db-with base (tx result))]
    (is (= :ready (:status result)))
    (is (= (:tempid (by-id b)) (get-in (by-id a) [:values :Test/friend])))
    (is (= (:tempid (by-id a)) (get-in (by-id b) [:values :Test/friend])))
    (is (= #{[a b] [b a]}
           (d/q '[:find ?id ?target :where [?e :entity/uuid ?id] [?e :Test/friend ?t]
                  [?t :entity/uuid ?target]] projected)))
    (is (empty? (d/datoms base :avet :entity/uuid a)))))

(deftest targets-require-stable-identity-and-exact-codomain
  (let [base (fixture)
        input (entity base a {:Test/friend (str b)})]
    (is (= :missing-relation-target (reason #(constraints/resolve-selection base [input]))))
    (is (= :relation-target-type-mismatch
           (reason #(constraints/resolve-selection base [input (assoc (entity base b {}) :types #{other-type})]))))
    (is (= :duplicate-canonical-identity
           (reason #(constraints/resolve-selection base [input input]))))
    (is (= :existing-canonical-identity
           (reason #(constraints/resolve-selection base [(entity base external {})]))))
    (is (= :invalid-relation-codomain
           (reason #(constraints/resolve-selection
                     (d/db-with base [{:entity/uuid person :object/primitive? true}])
                     [(entity base a {:Test/friend (str external)})]))))
    (let [wrong-type (d/db-with base [[:db/retract [:entity/uuid external] :instance/of-role [:entity/uuid person]]
                                     {:entity/uuid external :instance/of-role [[:entity/uuid other-type]]}])]
      (is (= :relation-target-type-mismatch
             (reason #(constraints/resolve-selection wrong-type [(entity base a {:Test/friend (str external)})])))))))

(deftest external-target-and-unique-dependencies-drive-freshness
  (let [base (fixture)
        result (constraints/resolve-selection base [(entity base a {:Test/friend {"id" (str external) "path" "misleading.md"}
                                                                         :Test/identity "fresh"})])
        deps (:dependencies result)
        projected (d/db-with base (tx result))
        role-removed (d/db-with base [[:db/retract [:entity/uuid external] :instance/of-role [:entity/uuid person]]])
        deleted (d/db-with base [[:db/retractEntity [:entity/uuid external]]])
        type-no-longer-entity (d/db-with base [{:entity/uuid person :object/primitive? true}])
        collision (d/db-with base [{:entity/uuid b :Test/identity "fresh"}])
        unrelated (d/db-with base [{:entity/uuid b :entity/name "Unrelated"}])]
    (is (= 1 (count (:external-targets deps))))
    (is (constraints/current? base deps false))
    (is (constraints/current? projected deps true))
    (is (not (constraints/current? role-removed deps false)))
    (is (not (constraints/current? deleted deps false)))
    (is (not (constraints/current? type-no-longer-entity deps false)))
    (is (not (constraints/current? collision deps false)))
    (is (not (constraints/current? collision deps "not an exact boolean")))
    (is (not (constraints/current? base deps "not an exact boolean")))
    (is (not (constraints/current? base nil false)))
    (is (constraints/current? unrelated deps false))))

(deftest both-uniqueness-modes-refuse-batch-and-base-collisions
  (let [base (fixture)]
    (doseq [[attr value] [[:Test/identity "same"] [:Test/value 0] [:Test/flag false]
                         [:Test/unique-friend (str external)]]]
      (is (= :batch-unique-value-collision
             (reason #(constraints/resolve-selection base [(entity base a {attr value}) (entity base b {attr value})]))))
      (let [base-value (if (= attr :Test/unique-friend)
                         (:db/id (d/pull base [:db/id] [:entity/uuid external])) value)
            occupied (d/db-with base [{:entity/uuid c attr base-value}])]
        (is (= :base-unique-value-collision
               (reason #(constraints/resolve-selection occupied [(entity occupied a {attr value})]))))))
    (is (= :batch-unique-value-collision
           (reason #(constraints/resolve-selection base [(entity base a {:Test/unique-friend (str b)})
                                                         (entity base b {})
                                                         (entity base c {:Test/unique-friend (str b)})]))))))

(deftest planned-reference-uniqueness-never-looks-up-a-nil-base-id
  (let [base (fixture)
        result (constraints/resolve-selection base [(entity base a {:Test/unique-friend (str b)})
                                                    (entity base b {})])
        projected (d/db-with base (tx result))
        dependency (-> result :dependencies :unique-values first)]
    (is (= :ready (:status result)))
    (is (true? (:planned-target? dependency)))
    (is (nil? (:base-value dependency)))
    (is (= b (:logical-value dependency)))
    (is (constraints/current? base (:dependencies result) false))
    (is (= b (get-in (d/pull projected [{:Test/unique-friend [:entity/uuid]}] [:entity/uuid a])
                     [:Test/unique-friend :entity/uuid])))))

(deftest missing-claims-diagnostics-and-aggregate-work-never-produce-partial-values
  (let [base (fixture)
        unique (entity base a {:Test/identity "new"})
        missing (assoc-in unique [:candidate :claims] [])
        diagnosed (assoc-in unique [:candidate :diagnostics] [{:kind :invalid-property-value}])
        large (candidate base {:Test/friends (vec (repeat 1000 (str external)))})
        scalar-large (candidate base {:Test/counts (vec (repeat 1000 1))})]
    (is (= :missing-or-duplicate-unique-claim (reason #(constraints/resolve-selection base [missing]))))
    (is (= :invalid-or-diagnosed-candidate (reason #(constraints/resolve-selection base [diagnosed]))))
    (doseq [candidate [large scalar-large]]
      (is (= :too-much-candidate-work
             (reason #(constraints/resolve-selection base
                                                     (mapv (fn [_] {:id (random-uuid) :types #{person} :candidate candidate})
                                                           (range 51)))))))
    (let [types (set (repeatedly 64 random-uuid))
          empty-candidate (candidate base {})]
      (is (= :too-much-candidate-work
             (reason #(constraints/resolve-selection base
                                                     (mapv (fn [_] {:id (random-uuid) :types types :candidate empty-candidate})
                                                           (range 1000))))))
      (is (= :invalid-or-diagnosed-candidate
             (reason #(constraints/resolve-selection base
                                                     [{:id a :types (conj types a) :candidate empty-candidate}])))))))
