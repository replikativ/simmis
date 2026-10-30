(ns is.simm.ops.personal-connections-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.git.http :as http]
            [is.simm.model.personal-connections :as accounts]
            [is.simm.model.github-credentials :as grants]
            [is.simm.model.github-integrations-test :as fixtures]
            [is.simm.model.github-connections :as connections]
            [is.simm.model.mail-credentials :as envelope]
            [is.simm.ops.github-credentials :as ops]
            [is.simm.ops.github-credentials-test :as credential-fixtures]
            [is.simm.ops.github-host :as host]
            [is.simm.ops.personal-connections :as personal]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.operation-lifetime :as lifetime]))

(deftest selected-personal-access-is-checked-at-every-request-and-send-admission
  (#'credential-fixtures/fixture
   (fn [conn]
     (d/transact conn accounts/schema)
     (let [by (random-uuid) account (random-uuid) rev (random-uuid)
           remote (#'fixtures/connection) remote-id (connections/connection-id remote)
           context {:conn conn :resolved {:party by} :require-personal-credential? true}
           calls (atom 0)]
       (d/transact conn (accounts/replace-tx @conn by account nil rev "Work" {:nonce "n" :ciphertext "c"}))
       (d/transact conn (grants/bind-tx @conn by remote-id nil (random-uuid) account rev))
       (with-redefs [host/connection! (fn [_] {:connection remote}) host/current! (fn [_])
                     envelope/decrypt (fn [id nonce ciphertext]
                                        (is (= account id)) (is (= ["n" "c"] [nonce ciphertext]))
                                        {:version 1 :kind :github-account :by by :account-id account :token (apply str (repeat 24 "x"))})
                     http/send-http (fn [_] (swap! calls inc) {:status 200})]
         (is (= (str account) (:account-id (ops/selection! context))))
         (ops/with-transport!
           context
           (fn [bound]
             (is (= 200 (:status ((:git-send bound) {:method :post :url "https://github.com/owner/repository.git/git-receive-pack"}))))
             (d/transact conn (accounts/replace-tx @conn by account (accounts/current-in @conn by account)
                                                   (random-uuid) "Work" {:nonce "next" :ciphertext "next"}))
             (is (thrown? clojure.lang.ExceptionInfo (ops/writer-current! bound @conn)))
             (is (thrown? clojure.lang.ExceptionInfo
                          ((:git-send bound) {:method :post :url "https://github.com/owner/repository.git/git-receive-pack"})))
             (is (= 1 @calls))))
         (d/transact conn (accounts/revoke-tx @conn by account (accounts/current-in @conn by account) (random-uuid)))
         (is (true? (ops/selected? context)) "A selected but revoked account must not trigger anonymous fallback")
         (is (false? (:configured? (ops/selection! context))))
         (is (thrown? clojure.lang.ExceptionInfo (ops/with-transport! context identity))))))))

(deftest new-personal-only-preparation-never-falls-back-to-a-legacy-token
  (#'credential-fixtures/fixture
   (fn [conn]
     (let [by (random-uuid) remote (#'fixtures/connection) remote-id (connections/connection-id remote)]
       (d/transact conn (grants/replace-tx @conn by remote-id nil (random-uuid) {:nonce "n" :ciphertext "c"}))
       (with-redefs [host/connection! (fn [_] {:connection remote})
                     envelope/decrypt (fn [& _] (is false "Legacy token must not be opened"))]
         (is (thrown? clojure.lang.ExceptionInfo
                      (ops/with-transport! {:conn conn :resolved {:party by} :require-personal-credential? true} identity))))))))

(deftest personal-vault-save-rotate-and-revoke-use-the-real-serialized-writer
  (#'credential-fixtures/fixture
   (fn [conn]
     (d/transact conn (concat accounts/schema
                              [{:db/ident :party/id :db/valueType :db.type/uuid :db/cardinality :db.cardinality/one :db/unique :db.unique/value}
                               {:db/ident :party/type :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}]))
     (let [by (random-uuid) id (str (random-uuid)) owner (lifetime/create)
           host {::reads/owner {:control-conn conn}}]
       (d/transact conn [{:party/id by :party/type :human}])
       (lifetime/open! owner)
       (try
         (with-redefs [application/capture-operation (fn [] (fn [f] (lifetime/with-operation! owner #(f host))))
                       reads/current-host-control-db! (fn [_] @conn)
                       envelope/encrypt (fn [& _] {:nonce "nonce" :ciphertext "encrypted"})]
           (let [created ((personal/prepare by :save {:id id :revision nil :name "GitHub" :token (apply str (repeat 24 "x"))}))
                 revision (:revision (first created))]
             (is (= [{:id id :revision revision :provider :github :name "GitHub" :configured? true}] created))
             (is (= created ((personal/prepare by :list {}))))
             (let [rotated ((personal/prepare by :save {:id id :revision revision :name "Work" :token (apply str (repeat 24 "y"))}))
                   next-revision (:revision (first rotated))]
               (is (not= revision next-revision))
               (is (thrown? clojure.lang.ExceptionInfo
                            ((personal/prepare by :revoke {:id id :revision revision}))))
               (let [revoked ((personal/prepare by :revoke {:id id :revision next-revision}))]
                 (is (false? (:configured? (first revoked))))))
             (let [deferred (personal/prepare by :list {})]
               (d/transact conn [{:db/id [:party/id by] :party/type :agent}])
               (is (thrown? clojure.lang.ExceptionInfo (deferred))))))
         (finally (lifetime/fence! owner) (lifetime/join! owner 1000)))))))
