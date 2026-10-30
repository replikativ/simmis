(ns is.simm.model.git-policy-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.model.git-policy :as policy]))

(defn- fixture [f]
  (let [config {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write}]
    (d/create-database config)
    (let [conn (d/connect config)]
      (try
        (d/transact conn (into policy/schema
                              [{:db/ident :proposal/id :db/valueType :db.type/uuid :db/cardinality :db.cardinality/one :db/unique :db.unique/value}
                               {:db/ident :proposal/room :db/valueType :db.type/uuid :db/cardinality :db.cardinality/one}
                               {:db/ident :proposal/status :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
                               {:db/ident :proposal/github-key :db/valueType :db.type/uuid :db/cardinality :db.cardinality/one}]))
        (f conn (random-uuid) (random-uuid) (random-uuid))
        (finally (d/release conn) (d/delete-database config))))))

(def defaults {:enabled? false :interval-seconds 60 :review-mode :fast-forward})

(defn- configure! [conn room connection by settings now]
  (let [current (policy/effective @conn room connection)]
    (d/transact conn (policy/configure-tx @conn room connection (:git.policy/revision current)
                                         (random-uuid) by settings now))
    (policy/effective @conn room connection)))

(deftest defaults-are-inert-and-polling-does-not-change-manual-review-epochs
  (fixture
   (fn [conn room connection by]
     (let [before (d/commit-id @conn) default (policy/effective @conn room connection)
           token (policy/review-token default)]
       (is (= before (d/commit-id @conn)))
       (is (false? (:git.policy/enabled? default)))
       (is (= :fast-forward (:git.policy/review-mode default)))
       (is (false? (boolean (policy/due? default (java.util.Date. 0)))))
       (configure! conn room connection by (assoc defaults :enabled? true) (java.util.Date. 0))
       (is (= token (policy/review-current! @conn token)))
       (configure! conn room connection by (assoc defaults :interval-seconds 300) (java.util.Date. 100))
       (is (= token (policy/review-current! @conn token)))
       (is (thrown? clojure.lang.ExceptionInfo (policy/effective @conn room (random-uuid))))))))

(deftest review-policy-epochs-reject-stale-preparations-including-aba
  (fixture
   (fn [conn room connection by]
     (let [default (policy/review-token (policy/effective @conn room connection))
           all (configure! conn room connection by (assoc defaults :review-mode :review-all) (java.util.Date. 0))]
       (is (thrown? clojure.lang.ExceptionInfo (policy/review-current! @conn default)))
       (is (= (policy/review-token all) (policy/review-current! @conn (policy/review-token all))))
       (let [back (configure! conn room connection by defaults (java.util.Date. 100))]
         (is (not= default (policy/review-token back)))
         (is (thrown? clojure.lang.ExceptionInfo (policy/review-current! @conn default)))
         (is (thrown? clojure.lang.ExceptionInfo (policy/review-current! @conn (policy/review-token all)))))
       (is (thrown? clojure.lang.ExceptionInfo
                    (policy/configure-tx @conn room connection (:git.policy/revision all) (random-uuid) by defaults (java.util.Date. 200))))))))

(deftest incoming-proposal-pointer-is-bounded-and-does-not-enable-polling
  (fixture
   (fn [conn room connection by]
     (let [id (random-uuid) next-id (random-uuid)
           token (policy/review-token (policy/effective @conn room connection))]
       (d/transact conn (into (policy/pending-tx @conn room connection by id)
                              [{:proposal/id id :proposal/room room :proposal/status :open :proposal/github-key (random-uuid)}]))
       (let [current (policy/effective @conn room connection)]
         (is (false? (:git.policy/enabled? current)))
         (is (= token (policy/review-token current)))
         (is (= id (policy/pending-proposal @conn current)))
         (is (thrown? clojure.lang.ExceptionInfo (policy/pending-tx @conn room connection by next-id))))
       (d/transact conn [[:db/add [:proposal/id id] :proposal/status :accepted]])
       (is (nil? (policy/pending-proposal @conn (policy/effective @conn room connection))))
       (d/transact conn (into (policy/pending-tx @conn room connection by next-id)
                              [{:proposal/id next-id :proposal/room room :proposal/status :open :proposal/github-key (random-uuid)}]))
       (is (= next-id (policy/pending-proposal @conn (policy/effective @conn room connection))))
       (d/transact conn [[:db/retractEntity [:proposal/id next-id]]])
       (is (thrown? clojure.lang.ExceptionInfo (policy/pending-proposal @conn (policy/effective @conn room connection))))))))

(deftest observations-have-bounded-backoff-and-cannot-overwrite-newer-policy-or-time
  (fixture
   (fn [conn room connection by]
     (let [active (configure! conn room connection by (assoc defaults :enabled? true) (java.util.Date. 0))
           revision (:git.policy/revision active)]
       (is (policy/due? active (java.util.Date. 0)))
       (d/transact conn (policy/checked-tx @conn room connection revision (java.util.Date. 1000) :failed))
       (let [failed (policy/effective @conn room connection)]
         (is (= 1 (:git.policy/failures failed)))
         (is (= 121000 (.getTime ^java.util.Date (:git.policy/next-check failed))))
         (is (empty? (policy/checked-tx @conn room connection revision (java.util.Date. 1000) :failed))))
       (d/transact conn (policy/checked-tx @conn room connection revision (java.util.Date. 2000) :busy))
       (is (zero? (:git.policy/failures (policy/effective @conn room connection))))
       (configure! conn room connection by defaults (java.util.Date. 3000))
       (is (empty? (policy/checked-tx @conn room connection revision (java.util.Date. 4000) :synced)))
       (is (false? (boolean (policy/due? (policy/effective @conn room connection) (java.util.Date. 999999)))))))))

(deftest settings-admission-rejects-unbounded-or-unknown-policy
  (fixture
   (fn [conn room connection by]
     (doseq [settings [(assoc defaults :interval-seconds 0) (assoc defaults :interval-seconds 59)
                       (assoc defaults :interval-seconds 86401) (assoc defaults :enabled? :yes)
                       (assoc defaults :review-mode :merge-everything) (assoc defaults :extra true)]]
       (is (thrown? clojure.lang.ExceptionInfo (configure! conn room connection by settings (java.util.Date. 0)))))
     (is (nil? (policy/current-in @conn room))))))
