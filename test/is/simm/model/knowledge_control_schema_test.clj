(ns is.simm.model.knowledge-control-schema-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.authority.world :as world]
            [is.simm.model.system-db :as system-db]))

(deftest fresh-control-store-includes-all-operational-knowledge-attributes
  (let [conn (world/fresh-system-conn)]
    (try
      (doseq [ident [:knowledge.stream/id :knowledge.stream/batch-generation :knowledge.stream/state
                     :knowledge.pin/payload :knowledge.pin/retirement-id :knowledge.pin/retirement-stream-binding
                     :knowledge.batch/selection :knowledge.batch/frozen :knowledge.batch/outcome
                     :knowledge.batch/recovery-generation-after :knowledge.item/pending-stream
                     :knowledge.batch/disposition :schedule.occurrence/knowledge-batch]]
        (is (= ident (:db/ident (d/pull @conn '[*] [:db/ident ident])))))
      (is (= :db.unique/value (:db/unique (d/pull @conn '[*] [:db/ident :knowledge.batch/scheduled-occurrence]))))
      (is (true? (:db/index (d/pull @conn '[*] [:db/ident :work.request/knowledge-batch]))))
      (is (empty? (d/datoms @conn :avet :schedule.activation/id)))
      (is (empty? (d/datoms @conn :avet :knowledge.stream/id)))
      (finally (d/release conn)))))

(deftest schema-deduplication-preserves-order-and-rejects-conflicting-definitions
  (let [a {:db/ident :test/a :db/valueType :db.type/uuid :db/cardinality :db.cardinality/one}
        b (assoc a :db/ident :test/b)]
    (is (= [a b] (@#'system-db/merge-schemas [a] [a b] [b])))
    (is (thrown? Exception (@#'system-db/merge-schemas [a] [(assoc a :db/index true)])))
    (is (thrown? Exception (@#'system-db/merge-schemas [{:db/valueType :db.type/uuid}])))))

(deftest aggregated-identifiers-are-unique
  (let [idents (map :db/ident system-db/schema)]
    (is (= (count idents) (count (set idents))))))
