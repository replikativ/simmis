(ns is.simm.fixtures.accounting-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.tx-preds :as predicates]
            [dvergr.chat.schema :as chat-schema]
            [is.simm.fixtures.accounting :as fixture]
            [is.simm.model.accounting :as accounting]
            [kontor.governance :as governance]
            [kontor.schema :as schema]))

(defn- error-types [f]
  (try (f) nil
       (catch Throwable error
         (set (keep (comp :type ex-data) (take-while some? (iterate ex-cause error)))))))

(deftest synthetic-book-retains-real-writer-governors-and-restores-the-installer
  (let [id (random-uuid)
        cfg {:store {:backend :memory :id id} :schema-flexibility :write :keep-history? false}
        original @#'accounting/ensure-accounting-schema!]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (chat-schema/ensure-full-schema! conn)
        (fixture/with-schema-only #(accounting/ensure-accounting-schema! conn))
        (is (identical? original @#'accounting/ensure-accounting-schema!)
            "Construction's temporary installer must be gone before the test body")
        (is (every? #(contains? (d/schema @conn) (:db/ident %)) schema/all))
        (is (ifn? (predicates/tx-pred-for id)))
        (is (empty? (d/q '[:find ?account :where [?account :kontor.account/path _]] @conn)))
        (is (not (contains? (d/schema @conn) :entity/name))
            "This synthetic setup does not install category-S ledger projections")
        (is (contains?
             (error-types #(d/transact conn [{:db/id "tx" :kontor.transaction/external-id "unbalanced-fixture"}
                                             {:db/id "unit" :kontor.commodity/symbol "FIXTURE"}
                                             {:kontor.posting/transaction "tx" :kontor.posting/commodity "unit"
                                              :kontor.posting/amount 1M}]))
             :validation/sum-to-zero))
        (is (contains? (error-types #(d/transact conn [{:attempt/id (random-uuid)}]))
                       :dvergr.agent.attempt.governance/unauthorized-attempt-create))
        (finally (governance/ungovern! conn) (d/release conn) (d/delete-database cfg))))))
