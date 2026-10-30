(ns is.simm.model.github-push-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [hasch.core :as hasch]
            [is.simm.model.github-push :as push]
            [is.simm.model.github-sync :as sync]))

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write :keep-history? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (d/transact conn (into sync/schema push/schema)) (f conn)
           (finally (d/release conn) (d/delete-database cfg))))))

(defn- binding-for [room]
  (let [by (random-uuid) client (random-uuid)]
    {:id (sync/request-id by room client) :by by :room room :client-id client :claim (random-uuid)}))

(defn- prepare! [conn binding kind]
  (let [body {:binding (dissoc binding :claim)}]
    (artifact/publish-value! (artifact/datahike-store conn) body
                             #(d/transact conn [[:db.fn/call sync/prepare-tx binding % (hasch/uuid body) kind]]))
    (sync/sync-in @conn (:id binding))))

(defn- transition! [conn row state]
  (let [body {:state state :id (:github.sync/id row)}]
    (artifact/publish-value!
     (artifact/datahike-store conn) body
     (fn [ref]
       (d/transact conn [[:db.fn/call push/transition-tx row state ref (hasch/uuid body)
                          (when (= :ready state)
                            ;; Pure journal test only: the fixed HOST separately
                            ;; attests actual binary pack bytes and retention.
                            {:github.push/pack ref :github.push/pack-size 32})]])))
    (sync/sync-in @conn (:github.sync/id row))))

(deftest push-and-incoming-share-one-pending-slot-but-never-transitions
  (fixture
   (fn [conn]
     (let [room (random-uuid) binding (binding-for room) row (prepare! conn binding :push)]
       (is (= :push (sync/kind row)))
       (is (= row (sync/active-in @conn room)))
       (is (= row (prepare! conn (assoc binding :claim (random-uuid)) :push)))
       (is (thrown? clojure.lang.ExceptionInfo (prepare! conn (binding-for room) :incoming)))
       (is (thrown? clojure.lang.ExceptionInfo (sync/transition-tx @conn row :allocated (random-uuid) (random-uuid))))
       (let [closed (transition! conn row :closed)]
         (is (= :closed (:github.sync/state closed)))
         (is (nil? (sync/active-in @conn room)))
         (is (= :incoming (sync/kind (prepare! conn (binding-for room) :incoming)))))))))

(deftest unknown-sending-cannot-be-closed-or-made-ready-again
  (fixture
   (fn [conn]
     (let [room (random-uuid)
           original (prepare! conn (binding-for room) :push)
           retaining (transition! conn original :retaining)
           ready (transition! conn retaining :ready)
           sending (transition! conn ready :sending)]
       (is (uuid? (:github.push/pack ready)))
       (doseq [state [:ready :closed :retaining]]
         (is (thrown? clojure.lang.ExceptionInfo (transition! conn sending state))))
       (is (thrown? clojure.lang.ExceptionInfo (transition! conn ready :closed)))
       (is (thrown? clojure.lang.ExceptionInfo (prepare! conn (binding-for room) :incoming)))
       (is (= sending (sync/active-in @conn room)))
       (let [observed (transition! conn sending :observed)]
         (is (= :observed (:github.sync/state observed)))
         (is (= (:github.push/intent sending) (:github.push/intent observed))))))))

(deftest ready-requires-retention-and-a-finite-typed-pack-reference
  (fixture
   (fn [conn]
     (let [original (prepare! conn (binding-for (random-uuid)) :push)
           retaining (transition! conn original :retaining)]
       (is (thrown? clojure.lang.ExceptionInfo
                    (push/transition-tx @conn retaining :ready (random-uuid) (random-uuid) nil)))
       (doseq [pack [{:github.push/pack "opaque string" :github.push/pack-size 32}
                     {:github.push/pack (random-uuid) :github.push/pack-size (inc push/max-pack-bytes)}
                     {:github.push/pack (random-uuid) :github.push/pack-size 32 :extra :field}]]
         (is (thrown? clojure.lang.ExceptionInfo
                      (push/transition-tx @conn retaining :ready (random-uuid) (random-uuid) pack))))
       (is (= retaining (sync/active-in @conn (:github.sync/room retaining))))))))
