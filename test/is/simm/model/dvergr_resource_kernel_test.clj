(ns is.simm.model.dvergr-resource-kernel-test
  "Cross-stack contract for the governed resource kernel used by every Room.

   Simmis resolves Datahike directly, so its pin overrides Dvergr's transitive
   version.  This test exercises the writer boundary that requires operation
   provenance; a dependency downgrade must fail here rather than later while a
   production Room is only partially provisioned."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [dvergr.chat.schema :as chat-schema]
            [dvergr.resource :as resource]
            [dvergr.room.store :as room-store]
            [is.simm.model.accounting :as accounting]
            [is.simm.model.store :as simm-store]
            [kontor.book :as book]
            [kontor.governance :as governance]))

(deftest resolved-datahike-supports-dvergr-resource-governance
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write
             :keep-history? true}
        room-id :resource-kernel-contract
        chat-id (random-uuid)]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (chat-schema/ensure-full-schema! conn)
        (d/transact conn
                    [(merge (chat-schema/create-chat-entity
                             {:id chat-id :title "Resource kernel contract"})
                            {:room/slug (room-store/room-id->slug room-id)
                             :room/type :internal})])

        (testing "Dvergr can install the fail-closed Kontor governor"
          (is (identical? conn
                          (resource/install-connection! conn room-id chat-id))))

        (testing "the governed bootstrap durably creates its unit and root wallet"
          (is (some? (d/q '[:find ?unit .
                            :in $ ?symbol
                            :where [?unit :kontor.commodity/symbol ?symbol]]
                          @conn resource/microdollars)))
          (is (some? (d/q '[:find ?account .
                            :in $ ?id
                            :where [?account :kontor.resource-account/id ?id]]
                          @conn (resource/room-wallet-id room-id)))))

        (testing "the resource spine is not mistaken for the full accounting kernel"
          (is (false? (accounting/accounting-installed? conn)))
          ;; This is the production Room extension boundary: category-S first,
          ;; then the accounting projection and its composed governors.
          (simm-store/install! conn)
          (is (true? (accounting/accounting-installed? conn))))

        (testing "technical resource accounts do not suppress the starter chart"
          (let [expected (into #{}
                               (keep :kontor.account/path)
                               accounting/starter-book)
                actual (set (d/q '[:find [?path ...]
                                   :where [_ :kontor.account/path ?path]]
                                 @conn))]
            (is (every? #(contains? actual %) expected)))
          (book/entry! conn
                       {:debit-account [:kontor.account/path "Assets:Bank"]
                        :credit-account [:kontor.account/path "Equity:Opening"]
                        :amount 1M
                        :commodity [:kontor.commodity/symbol "USD"]
                        :journal-type :general
                        :effective-date #inst "2026-08-31"})
          (is (= 2 (d/q '[:find (count ?posting) .
                          :where [?posting :kontor.posting/amount _]]
                        @conn))))

        (testing "Kontor still validates raw writer transactions after composition"
          (let [posting (d/q '[:find ?p . :where
                               [?p :kontor.posting/account ?account]
                               [?account :kontor.account/path "Assets:Bank"]]
                             @conn)
                before (:kontor.posting/amount
                        (d/pull @conn [:kontor.posting/amount] posting))
                error (try
                        ;; Deliberately bypass the high-level book verb: this
                        ;; checks the composed mandatory writer predicate.
                        (d/transact conn [{:db/id posting
                                           :kontor.posting/amount 2M}])
                        nil
                        (catch Throwable e e))
                types (keep #(-> % ex-data :type)
                            (take-while some? (iterate ex-cause error)))]
            (is (some #{:sealing/silent-retract-of-posted
                        :validation/sum-to-zero}
                      types)
                "the rejection must come from Kontor's accounting invariants")
            (is (= before (:kontor.posting/amount
                           (d/pull @conn [:kontor.posting/amount] posting))))))

        (testing "Simmis accounting installation preserves Dvergr Attempt governance"
          (is (thrown-with-msg?
               Throwable #"trusted writer"
               (d/transact conn [{:attempt/id (random-uuid)}]))))

        (testing "ordinary writes remain possible after the governor is active"
          (d/transact conn [{:chat/id chat-id
                             :chat/title "Resource kernel installed"}])
          (is (= "Resource kernel installed"
                 (d/q '[:find ?title .
                        :in $ ?id
                        :where [?chat :chat/id ?id]
                               [?chat :chat/title ?title]]
                      @conn chat-id))))
        (finally
          (governance/ungovern! conn)
          (d/release conn)
          (d/delete-database cfg))))))
