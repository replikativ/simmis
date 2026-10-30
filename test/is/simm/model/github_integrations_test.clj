(ns is.simm.model.github-integrations-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [is.simm.model.github-connections :as connections]
            [is.simm.model.github-integrations :as integrations]))

(defn- connection []
  {:version 1 :room (random-uuid)
   :source {:kind :github :repository "owner/repository"}
   :ref "refs/heads/main" :target-stamp (random-uuid)
   :target {:registry-system (random-uuid) :runtime-system "repository"
            :repository (random-uuid) :store (random-uuid) :backend :file
            :branch :geschichte.workspace/main :ref "refs/heads/main"}})

(deftest closed-connections-bind-the-source-and-exact-physical-target
  (let [value (connection) id (connections/connection-id value)]
    (is (= value (connections/connection! value)))
    (is (= id (connections/connection-id (into {} (reverse (seq value))))))
    (doseq [changed [(assoc-in value [:target :store] (random-uuid))
                     (assoc value :target-stamp (random-uuid))
                     (assoc-in value [:source :repository] "owner/another")]]
      (is (not= id (connections/connection-id changed))))
    (doseq [invalid [(assoc value :extra true)
                     (assoc value :version 2)
                     (assoc value :ref "refs/heads/other")
                     (assoc-in value [:source :repository] "Owner/Repository")
                     (assoc-in value [:target :store] "untrusted-config")
                     (assoc value :ref "refs/heads/main\n")]]
      (is (thrown? clojure.lang.ExceptionInfo (connections/connection! invalid))))))

(defn- with-store [f]
  (let [config {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write}]
    (d/create-database config)
    (let [conn (d/connect config)]
      (try (d/transact conn integrations/schema) (f conn)
           (finally (d/release conn) (d/delete-database config))))))

(deftest initialization-is-idempotent-and-never-overwrites-later-state
  (with-store
    (fn [conn]
      (let [value (connection) creator (random-uuid) head (random-uuid)
            creation {:room/creation-intent (random-uuid) :room/creation-basis (random-uuid)}
            oid (apply str (repeat 40 "a"))
            body (integrations/import-body value creator creation head oid)
            store (artifact/datahike-store conn)
            cref (artifact/publish-value! store value identity)
            pref (artifact/publish-value! store body identity)
            args [(:room value) cref (connections/connection-id value) head oid pref (integrations/import-id body)]
            initialize! #(d/transact conn [(into [:db.fn/call integrations/initialize-tx] args)])]
        (is (nil? (integrations/current-in @conn (:room value))))
        (initialize!)
        (let [before (integrations/current-in @conn (:room value))
              later (random-uuid)]
          (is (= head (:github.integration/head before)))
          (is (= body (:value (artifact/get-bounded-value store pref 16384))))
          (initialize!)
          (is (= before (integrations/current-in @conn (:room value))))
          ;; Emulate a future authorized cursor advance. Initialization must
          ;; preserve it; this release itself supplies no advance operation.
          (d/transact conn [{:db/id (:db/id before) :github.integration/head later}])
          (let [after (integrations/current-in @conn (:room value))]
            (initialize!)
            (is (= after (integrations/current-in @conn (:room value))))
            (is (= head (:github.integration/import-head after)))
            (is (= later (:github.integration/head after)))
            (is (= (:db/id before) (:db/id after)))
            (is (= 1 (count (d/datoms @conn :avet :github.integration/room))))
            (doseq [changed [(assoc args 2 (random-uuid)) (assoc args 3 (random-uuid))
                             (assoc args 6 (random-uuid))]]
              (is (thrown? clojure.lang.ExceptionInfo
                           (apply integrations/initialize-tx @conn changed))))
            (is (= after (integrations/current-in @conn (:room value))))))))))

(deftest invalid-import-data-does-not-install-a-cursor
  (with-store
    (fn [conn]
      (let [room (random-uuid) before @conn]
        (doseq [oid [nil "short" (apply str (repeat 40 "A"))]]
          (is (thrown? clojure.lang.ExceptionInfo
                       (integrations/initialize-tx @conn room (random-uuid) (random-uuid)
                                                   (random-uuid) oid (random-uuid) (random-uuid)))))
        (is (nil? (integrations/current-in @conn room)))
        (is (= before @conn))))))

(deftest sync-advancement-compares-the-whole-cursor-and-retains-the-import-anchor
  (with-store
    (fn [conn]
      (let [room (random-uuid) base (random-uuid) head (random-uuid)
            base-oid (apply str (repeat 40 "a")) oid (apply str (repeat 40 "b"))
            sync-id (random-uuid) proof (random-uuid) proof-id (random-uuid)]
        (d/transact conn [[:db.fn/call integrations/initialize-tx room (random-uuid) (random-uuid)
                          base base-oid (random-uuid) (random-uuid)]])
        (let [expected (integrations/current-in @conn room)
              advance #(integrations/advance-sync-tx @conn expected sync-id base base-oid head oid proof proof-id)]
          (is (= [] (integrations/advance-sync-tx @conn expected sync-id base base-oid base base-oid proof proof-id)))
          (doseq [[wrong-head wrong-oid] [[(random-uuid) base-oid] [base oid]]]
            (is (thrown? clojure.lang.ExceptionInfo
                         (integrations/validate-sync-base! @conn expected wrong-head wrong-oid))))
          (is (thrown? clojure.lang.ExceptionInfo
                       (integrations/advance-sync-tx @conn expected sync-id base base-oid base oid proof proof-id)))
          (is (= expected (integrations/current-in @conn room)))
          (d/transact conn (advance))
          (let [actual (integrations/current-in @conn room)]
            (is (= head (:github.integration/head actual)))
            (is (= oid (:github.integration/oid actual)))
            (is (= proof (:github.integration/proof actual)))
            (is (= proof-id (:github.integration/proof-id actual)))
            (is (= sync-id (:github.integration/sync actual)))
            (is (uuid? (:github.integration/previous actual)))
            (is (= (select-keys expected [:github.integration/import-head :github.integration/import-oid
                                         :github.integration/import-proof :github.integration/import-proof-id
                                         :github.integration/connection :github.integration/connection-id])
                   (select-keys actual [:github.integration/import-head :github.integration/import-oid
                                       :github.integration/import-proof :github.integration/import-proof-id
                                       :github.integration/connection :github.integration/connection-id])))
            (is (thrown? clojure.lang.ExceptionInfo (advance)))
            (is (= actual (integrations/current-in @conn room)))))))))
