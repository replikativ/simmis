(ns is.simm.documents.space-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.query :as query]
            [is.simm.documents.properties :as properties]
            [is.simm.documents.repository :as documents]
            [is.simm.documents.space :as space]
            [kontor.governance :as governance]))

(defn- with-new-store [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false
             :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (f conn)
           (finally (governance/ungovern! conn) (d/release conn) (d/delete-database cfg))))))

(deftest explicitly-new-space-installs-the-real-shared-model
  (with-new-store
    (fn [conn]
      (is (not (space/provisioned? @conn)))
      (is (identical? conn (space/provision! conn)))
      (is (space/provisioned? @conn))
      (is (uuid? (:geschichte.repo/id (query/repository @conn))))
      (is (= :db.unique/value (get-in (d/schema @conn) [:S.Page/title :db/unique])))
      (is (true? (contains? (d/schema @conn) :kontor.posting/amount)))
      (is (contains? (d/schema @conn) :document/id))
      (let [page-id (:entity/uuid (d/pull @conn [:entity/uuid] [:entity/name "S/Page"]))
            block-id (:entity/uuid (d/pull @conn [:entity/uuid] [:entity/name "S/Block"]))]
        (is (= #{page-id} (set (keys (:types (properties/compile-scope @conn {:types [page-id] :bindings {}}))))))
        (is (= #{block-id} (set (keys (:types (properties/compile-scope @conn {:types [block-id] :bindings {}})))))))
      (let [before (:max-tx @conn)]
        (is (thrown? clojure.lang.ExceptionInfo (space/provision! conn)))
        (is (= before (:max-tx @conn)))))))

(deftest an-existing-unclassified-repository-is-never-upgraded-on-open
  (with-new-store
    (fn [conn]
      (documents/initialize! conn)
      (let [before (:max-tx @conn)]
        (is (not (space/provisioned? @conn)))
        (is (thrown? clojure.lang.ExceptionInfo (space/provision! conn)))
        (is (= before (:max-tx @conn)))
        (is (not (contains? (d/schema @conn) :document.repository/profile)))))))

(deftest even-a-transaction-without-application-data-is-not-pristine
  (with-new-store
    (fn [conn]
      (d/transact conn [])
      (let [before (:max-tx @conn)]
        (is (thrown? clojure.lang.ExceptionInfo (space/provision! conn)))
        (is (= before (:max-tx @conn)))
        (is (not (space/provisioned? @conn)))))))
