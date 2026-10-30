(ns is.simm.model.personal-connections-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.model.personal-connections :as accounts]
            [is.simm.model.github-credentials :as grants]))

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (d/transact conn (concat accounts/schema grants/schema)) (f conn)
           (finally (d/release conn) (d/delete-database cfg))))))

(def envelope {:nonce "nonce" :ciphertext "encrypted-token"})

(deftest personal-credentials-remain-owner-only-and-public-projections-have-no-secret
  (fixture
   (fn [conn]
     (let [owner (random-uuid) other (random-uuid) id (random-uuid) rev (random-uuid)]
       (d/transact conn (accounts/replace-tx @conn owner id nil rev "Work GitHub" envelope))
       (is (= [{:id (str id) :provider :github :name "Work GitHub" :revision (str rev) :configured? true}]
              (accounts/list-in @conn owner)))
       (is (empty? (accounts/list-in @conn other)))
       (is (nil? (accounts/current-in @conn other id)))
       (is (thrown? clojure.lang.ExceptionInfo (accounts/replace-tx @conn other id nil (random-uuid) "Stolen" envelope)))
       (is (thrown? clojure.lang.ExceptionInfo (accounts/revoke-tx @conn other id nil (random-uuid))))
       (is (thrown? clojure.lang.ExceptionInfo (accounts/replace-tx @conn owner id nil (random-uuid) "Stale" envelope)))))))

(deftest one-personal-connection-supports-independent-memory-grants-and-rotation
  (fixture
   (fn [conn]
     (let [owner (random-uuid) id (random-uuid) rev (random-uuid) a (random-uuid) b (random-uuid)]
       (d/transact conn (accounts/replace-tx @conn owner id nil rev "GitHub" envelope))
       (doseq [remote [a b]] (d/transact conn (grants/bind-tx @conn owner remote nil (random-uuid) id rev)))
       (let [ready-a (grants/binding-in @conn (grants/current-in @conn owner a))
             ready-b (grants/binding-in @conn (grants/current-in @conn owner b))
             row (accounts/current-in @conn owner id)]
         (is (= ready-a (grants/current! @conn ready-a)))
         (is (= ready-b (grants/current! @conn ready-b)))
         (d/transact conn (accounts/replace-tx @conn owner id row (random-uuid) "Rotated GitHub" envelope))
         (doseq [ready [ready-a ready-b]] (is (thrown? clojure.lang.ExceptionInfo (grants/current! @conn ready))))
         (let [new-a (grants/binding-in @conn (grants/current-in @conn owner a))
               new-b (grants/binding-in @conn (grants/current-in @conn owner b))]
           (is (some? new-a) "New reviews can use the rotated token without rebinding every Memory")
           (is (= (:github.credential/revision ready-a) (:github.credential/revision new-a)))
           (d/transact conn (grants/revoke-tx @conn owner a (grants/current-in @conn owner a) (random-uuid)))
           (is (nil? (grants/binding-in @conn (grants/current-in @conn owner a))))
           (is (= new-b (grants/current! @conn new-b)))
           (is (some? (accounts/binding-of (accounts/current-in @conn owner id))))
           (d/transact conn (accounts/revoke-tx @conn owner id (accounts/current-in @conn owner id) (random-uuid)))
           (is (nil? (grants/binding-in @conn (grants/current-in @conn owner b))))
           (is (thrown? clojure.lang.ExceptionInfo (grants/current! @conn new-b)))))))))

(deftest selection-requires-the-inspected-personal-revision-and-current-grant
  (fixture
   (fn [conn]
     (let [owner (random-uuid) other (random-uuid) id (random-uuid) rev (random-uuid) remote (random-uuid)]
       (d/transact conn (accounts/replace-tx @conn owner id nil rev "GitHub" envelope))
       (is (thrown? clojure.lang.ExceptionInfo (grants/bind-tx @conn other remote nil (random-uuid) id rev)))
       (is (thrown? clojure.lang.ExceptionInfo (grants/bind-tx @conn owner remote nil (random-uuid) id (random-uuid))))
       (d/transact conn (grants/bind-tx @conn owner remote nil (random-uuid) id rev))
       (is (thrown? clojure.lang.ExceptionInfo (grants/bind-tx @conn owner remote nil (random-uuid) id rev)))))))
