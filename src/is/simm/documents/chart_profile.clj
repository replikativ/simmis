(ns is.simm.documents.chart-profile
  "Optional fixed typed-chart-v1 definitions. Not part of the mandatory
   canonical-knowledge-v1 foundation; file reads never install or repair them."
  (:require [datahike.api :as d]
            [hasch.core :as hasch]
            [is.simm.documents.chart-contract :as contract]
            [is.simm.documents.properties :as properties]
            [is.simm.documents.space :as space]
            [is.simm.model.seed :as seed]))

(def profile contract/profile)
(def type-id contract/type-id)
(def property-id contract/property-id)
(def type-name "S/ChartV1")
(def property-name "S/ChartV1/spec-path")
(def storage-attribute :S.ChartV1/spec-path)
(def ^:private storage
  {:db/ident storage-attribute :db/valueType :db.type/string :db/cardinality :db.cardinality/one})

(defn- refuse! [reason]
  (throw (ex-info "Typed chart definitions were refused" {:type ::refused :reason reason})))

(defn- eid [db attribute value]
  (:e (first (d/datoms db :avet attribute value))))

(defn- shape [compiled]
  (let [type (get-in compiled [:types type-id])
        binding (get-in compiled [:properties property-id])]
    {:type (-> type (dissoc :db/id)
               (update :object/of-category dissoc :db/id)
               (update :instance/of-role #(set (map :db/id %))))
     :binding binding}))

(defn inspect
  "Return :absent only when ALL reserved identities, names and storage are
   absent. Partial/colliding/modified profiles refuse. The digest includes the
   actual compiler-checked definition, storage and destination, not a marker."
  [db]
  (when-not (space/provisioned? db) (refuse! :unprovisioned-space))
  (let [present [(eid db :entity/uuid type-id) (eid db :entity/uuid property-id)
                 (eid db :entity/name type-name) (eid db :entity/name property-name)
                 (get (d/schema db) storage-attribute)]]
    (if (every? nil? present)
      {:status :absent :profile profile}
      (let [compiled (try (properties/compile-scope db {:types [type-id] :bindings {}})
                          (catch clojure.lang.ExceptionInfo _ (refuse! :incompatible-definitions)))
            type (get-in compiled [:types type-id])
            type-extra (d/pull db [:object/base-type :object/enum-values] (:db/id type))
            binding (get-in compiled [:properties property-id])
            m (:morphism binding)
            category (:entity/uuid seed/category-S)
            string-id (:entity/uuid seed/object-String)
            meta-eid (eid db :entity/uuid (:entity/uuid seed/object-EntityType))
            expected-m {:entity/uuid property-id :entity/name property-name
                        :morphism/src {:entity/uuid type-id :entity/name type-name}
                        :morphism/dst {:entity/uuid string-id :entity/name "S/String"
                                       :object/primitive? true
                                       :object/of-category {:entity/uuid category :entity/name "S"}}
                        :morphism/of-category {:entity/uuid category :entity/name "S"}
                        :morphism/cardinality :one :morphism/optional? false}]
        (when-not (and (every? some? present)
                       (= (nth present 0) (nth present 2)) (= (nth present 1) (nth present 3))
                       (= type-name (:entity/name type))
                       (false? (:object/primitive? type))
                       (nil? (:object/base-type type-extra)) (empty? (:object/enum-values type-extra))
                       (= {:entity/uuid category :entity/name "S"}
                          (dissoc (:object/of-category type) :db/id))
                       (= #{meta-eid} (set (map :db/id (:instance/of-role type))))
                       (= #{property-id} (set (keys (:properties compiled))))
                       (= expected-m (dissoc m :db/id))
                       (= {:db/valueType :db.type/string :db/cardinality :db.cardinality/one}
                          (:schema binding))
                       (:owned? binding) (= storage-attribute (:attr binding)))
          (refuse! :incompatible-definitions))
        {:status :ready :profile profile
         :digest (hasch/uuid [profile (shape compiled)])}))))

(defn installation-tx
  "Prepare one atomic additive install against an exclusively owned new DB or
   a HOST-serialized current write snapshot. This grants no authority and must
   not be called from open/read paths. Exact existing installs are no-ops."
  [db]
  (if (= :ready (:status (inspect db)))
    []
    [storage
     {:db/id -1 :entity/uuid type-id :entity/name type-name :object/primitive? false
      :object/of-category [:entity/uuid (:entity/uuid seed/category-S)]
      :instance/of-role [:entity/uuid (:entity/uuid seed/object-EntityType)]}
     {:entity/uuid property-id :entity/name property-name
      :morphism/src -1 :morphism/dst [:entity/uuid (:entity/uuid seed/object-String)]
      :morphism/of-category [:entity/uuid (:entity/uuid seed/category-S)]
      :morphism/cardinality :one :morphism/optional? false}
     {:entity/uuid (:entity/uuid seed/category-S) :category/objects -1}]))
