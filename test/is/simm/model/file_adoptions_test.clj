(ns is.simm.model.file-adoptions-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.model.file-adoptions :as adoptions]))

(defn- fixture []
  (let [component (random-uuid)]
    {:proposal (random-uuid) :component component :manifest component
     :room (random-uuid) :actor (random-uuid) :filing (apply str (repeat 64 "a"))
     :disposition (random-uuid)
     :target {:repository (random-uuid) :store (random-uuid) :backend :memory
              :branch :main :ref "main" :registry-system (random-uuid)
              :runtime-system "registered-owned-repository"}}))

(defn- with-store [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :keep-history? false :schema-flexibility :write}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (d/transact conn
                    (into adoptions/schema
                          (for [[ident type] [[:proposal.fork/scope :db.type/uuid]
                                              [:proposal.fork/system-type :db.type/keyword]
                                              [:proposal.fork/status :db.type/keyword]]]
                            {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one})))
        (let [binding (fixture)
              report (d/transact conn [{:db/id -1 :proposal.fork/scope (:component binding)
                                        :proposal.fork/system-type :file-output}])]
          (f conn (get (:tempids report) -1) binding))
        (finally (d/release conn))))))

(defn- claim! [conn eid operation]
  (d/transact conn [[:db.fn/call adoptions/claim-tx-data eid operation]]))

(defn- transition! [conn eid operation state fields]
  (d/transact conn [[:db.fn/call adoptions/transition-tx-data
                     eid operation state fields (java.util.Date.)]]))

(deftest closed-typed-integrity-and-fresh-invocation-identities
  (let [binding (fixture) token (random-uuid) now (java.util.Date.)
        a (adoptions/make binding token now)
        b (adoptions/make binding (random-uuid) now)]
    (is (= a (adoptions/validate! a)))
    (is (= (:intent a) (:intent b)))
    (is (not= (:id a) (:id b)))
    (doseq [bad [(assoc binding :manifest (random-uuid))
                 (assoc binding :filing "not-a-digest")
                 (update binding :target dissoc :registry-system)
                 (assoc-in binding [:target :ref] " ")
                 (assoc binding :body "no bytes in journal")
                 (dissoc binding :disposition)]]
      (is (thrown? Exception (adoptions/make bad token now))))
    (doseq [bad [(assoc a :actor (random-uuid)) (assoc a :token (random-uuid))
                 (assoc a :state :unknown) (assoc a :body "not allowed")
                 (assoc a :reason :premature) (assoc a :noop? false)
                 (assoc a :updated-at (java.util.Date. 0))]]
      (is (thrown? Exception (adoptions/validate! bad))))
    (doseq [fields [{:noop? false} {:noop? false :commit (random-uuid)}
                    {:noop? false :commit (random-uuid) :head (random-uuid)}
                    {:noop? true :commit (random-uuid)}]]
      (is (thrown? Exception (adoptions/validate! (merge a fields {:state :accepted})))))
    (is (= :accepted (:state (adoptions/validate! (assoc a :state :accepted :noop? true)))))))

(deftest acknowledged-claim-is-not-replayable-and-latest-token-is-cas
  (with-store
    (fn [conn eid binding]
      (let [a (adoptions/make binding (random-uuid) (java.util.Date.))]
        (claim! conn eid a)
        (is (= a (adoptions/operation @conn (:id a))))
        ;; Exact replay, including the same token, never grants ownership.
        (is (thrown? Exception (claim! conn eid a)))
        (is (thrown? Exception
                     (claim! conn eid (adoptions/make binding (random-uuid) (java.util.Date.)))))
        (doseq [bad [(assoc a :token (random-uuid))
                     (adoptions/make (assoc binding :actor (random-uuid)) (random-uuid) (java.util.Date.))]]
          (is (thrown? Exception (transition! conn eid bad :applying {}))))
        (is (thrown? Exception (transition! conn eid a :accepted {:noop? false})))
        (transition! conn eid a :applying {})
        (let [applying (adoptions/operation @conn (:id a))]
          (is (= :applying (:state applying)))
          (is (thrown? Exception (transition! conn eid a :aborted {:reason :stale})))
          (is (thrown? Exception (transition! conn eid applying :aborted {:reason :unsafe})))
          (is (thrown? Exception (transition! conn eid applying :claimed {})))
          (is (thrown? Exception (claim! conn eid a)))
          (let [commit (random-uuid)]
            (transition! conn eid applying :accepted {:noop? false :commit commit :head commit})
            (let [accepted (adoptions/operation @conn (:id a))]
              (is (= :accepted (:state accepted)))
              (is (= commit (:commit accepted)))
              (is (= (:actor binding) (:actor accepted)))
              (is (thrown? Exception (transition! conn eid accepted :aborted {:reason :too-late})))
              (is (thrown? Exception (claim! conn eid a))))))))))

(deftest aborted-attempt-remains-audit-only-after-new-claim
  (with-store
    (fn [conn eid binding]
      (let [a (adoptions/make binding (random-uuid) (java.util.Date.))]
        (claim! conn eid a)
        (transition! conn eid a :aborted {:reason :witnessed-predispatch-failure})
        (let [aborted (adoptions/operation @conn (:id a))
              b (adoptions/make (assoc binding :actor (random-uuid)) (random-uuid) (java.util.Date.))]
          (is (= :aborted (:state aborted)))
          (is (thrown? Exception (claim! conn eid (adoptions/make
                                                   (assoc binding :disposition (random-uuid))
                                                   (random-uuid) (java.util.Date.)))))
          (claim! conn eid b)
          (is (= aborted (adoptions/operation @conn (:id a))))
          (doseq [old [a aborted]]
            (is (thrown? Exception (transition! conn eid old :applying {})))
            (is (thrown? Exception (transition! conn eid old :aborted {:reason :stale}))))
          (transition! conn eid b :applying {})
          (is (= :applying (:state (adoptions/operation @conn (:id b))))))))))

(deftest concurrent-claims-have-exactly-one-winner
  (with-store
    (fn [conn eid binding]
      (let [start (promise)
            candidates (repeatedly 4 #(adoptions/make binding (random-uuid) (java.util.Date.)))
            workers (mapv (fn [a]
                            (future @start
                                    (try (claim! conn eid a) (:id a)
                                         (catch Exception _ ::refused)))) candidates)]
        (deliver start true)
        (let [results (mapv #(deref % 10000 ::timeout) workers)
              winners (filterv uuid? results)]
          (is (not-any? #{::timeout} results))
          (is (= 1 (count winners)))
          (is (= 3 (count (filter #{::refused} results)))))))))

(deftest corrupt-links-and-resolved-components-never-claim
  (with-store
    (fn [conn eid binding]
      (let [a (adoptions/make binding (random-uuid) (java.util.Date.))]
        (d/transact conn [{:db/id -1 :file.adoption/state :claimed}
                          [:db/add eid :proposal.fork/file-adoption -1]])
        (is (thrown? Exception (claim! conn eid a)))
        (d/transact conn [[:db/retract eid :proposal.fork/file-adoption]])
        (d/transact conn [[:db/add eid :proposal.fork/status :dismissed]])
        (is (thrown? Exception (claim! conn eid a)))))))

(deftest one-operation-cannot-own-two-components
  (with-store
    (fn [conn eid binding]
      (let [a (adoptions/make binding (random-uuid) (java.util.Date.))]
        (claim! conn eid a)
        (d/transact conn [{:proposal.fork/scope (:component binding)
                           :proposal.fork/system-type :file-output
                           :proposal.fork/file-adoption [:file.adoption/id (:id a)]}])
        (is (thrown? Exception (transition! conn eid a :applying {})))))))

(deftest transition-clock-cannot-move-backwards
  (with-store
    (fn [conn eid binding]
      (let [a (adoptions/make binding (random-uuid) (java.util.Date. 1000))]
        (claim! conn eid a)
        (d/transact conn [[:db.fn/call adoptions/transition-tx-data
                           eid a :applying {} (java.util.Date. 3000)]])
        (let [applying (adoptions/operation @conn (:id a))]
          (is (thrown? Exception
                       (d/transact conn [[:db.fn/call adoptions/transition-tx-data
                                          eid applying :accepted {:noop? true} (java.util.Date. 2000)]])))
          (d/transact conn [[:db.fn/call adoptions/transition-tx-data
                             eid applying :accepted {:noop? true} (java.util.Date. 3000)]])
          (is (= :accepted (:state (adoptions/operation @conn (:id a))))))))))
