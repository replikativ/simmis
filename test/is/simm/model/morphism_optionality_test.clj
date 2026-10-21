(ns is.simm.model.morphism-optionality-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [is.simm.model.crud :as crud]
            [is.simm.model.schema :as schema]
            [is.simm.model.seed :as seed]))

(deftest explicit-requiredness-survives-the-shared-writer
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (d/transact conn schema/full-schema)
        (seed/ensure-seed-data! conn)
        (crud/create-type! conn {:name "OptionalityFixture"})
        (doseq [[suffix options expected]
                [["required" {:optional? false} false]
                 ["optional" {:optional? true} true]
                 ["unspecified" {} nil]
                 ["nil" {:optional? nil} nil]]]
          (testing suffix
            (let [id (crud/create-morphism!
                      conn (merge {:name (str "S/OptionalityFixture/" suffix)
                                   :src "S/OptionalityFixture" :dst "S/String"}
                                  options))
                  row (d/pull @conn [:morphism/optional?] [:entity/uuid id])]
              (is (= expected (:morphism/optional? row)))
              (is (= (some? expected) (contains? row :morphism/optional?))))))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))
