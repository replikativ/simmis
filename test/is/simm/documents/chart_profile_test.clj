(ns is.simm.documents.chart-profile-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [is.simm.documents.chart-profile :as chart]
            [is.simm.documents.space :as space]
            [kontor.governance :as governance]))

(defn- check-profile [conn]
  (let [absent @conn before (:max-tx absent)]
    (is (= :absent (:status (chart/inspect absent))))
    (is (= before (:max-tx @conn)))
    (d/transact conn (chart/installation-tx absent))
    (let [ready @conn result (chart/inspect ready)]
      (is (= :ready (:status result)))
      (is (uuid? (:digest result)))
      (is (= [] (chart/installation-tx ready)))
      (is (= :absent (:status (chart/inspect absent))))
      (testing "modified definitions refuse without repair or execution"
        (doseq [tx [[{:entity/uuid chart/type-id :entity/name "S/Other"}]
                    [{:entity/uuid chart/type-id :object/primitive? true}]
                    [{:entity/uuid chart/type-id :object/base-type [:entity/name "S/String"]}]
                    [{:entity/uuid chart/type-id :object/enum-values ["bad"]}]
                    [{:entity/uuid chart/property-id :morphism/optional? true}]
                    [{:entity/uuid chart/property-id :morphism/computation "(throw (Exception.))"}]
                    [{:entity/uuid chart/property-id :morphism/validation "(System/exit 0)"}]
                    [{:entity/uuid chart/property-id :morphism/dst [:entity/name "S/Number"]}]
                    [[:db/retractEntity [:entity/uuid chart/property-id]]]]]
          (let [changed (d/db-with ready tx)]
            (is (thrown? clojure.lang.ExceptionInfo (chart/inspect changed)))
            (is (thrown? clojure.lang.ExceptionInfo (chart/installation-tx changed)))))))
    (testing "name, identity and attribute collisions independently refuse"
      (doseq [tx [[{:entity/uuid chart/type-id :entity/name "S/Wrong"}]
                  [{:entity/uuid (random-uuid) :entity/name chart/type-name}]
                  [{:db/ident chart/storage-attribute :db/valueType :db.type/string
                    :db/cardinality :db.cardinality/one}]]]
        (is (thrown? clojure.lang.ExceptionInfo
                     (chart/installation-tx (d/db-with absent tx))))))))

(deftest optional-profile-is-atomic-exact-and-never-repaired
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false
             :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (space/provision! conn)
        (check-profile conn)
        (finally
          (governance/ungovern! conn)
          (d/release conn)
          (d/delete-database cfg))))))
