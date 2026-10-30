(ns is.simm.ops.github-ancestry-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.budget :as budget]
            [geschichte.repo :as repo]
            [is.simm.ops.github-ancestry :as ancestry]))

(defn- operation
  ([] (operation {}))
  ([limits] (budget/create {:limits (merge {:commit-visits 64 :graph-edges 128} limits) :timeout-ms 30000})))

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write :commit-graph? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (repo/init! conn) (f conn)
           (finally (d/release conn) (d/delete-database cfg))))))

(deftest ancestry-proves-paths-without-confusing-another-branch-or-a-frontier
  (fixture
   (fn [conn]
     (let [[a b c side frontier] (repeatedly 5 random-uuid)]
       (d/transact conn [{:geschichte.commit/id a :geschichte.commit/time (java.util.Date. 0)}
                         {:geschichte.commit/id b :geschichte.commit/parents [{:geschichte.commit/id a}]}
                         {:geschichte.commit/id c :geschichte.commit/parents [{:geschichte.commit/id b}]}
                         {:geschichte.commit/id side :geschichte.commit/parents [{:geschichte.commit/id a}]}
                         {:geschichte.commit/id frontier :geschichte.commit/git-oid (apply str (repeat 40 "a"))}])
       (is (ancestry/proven-ancestor? @conn a c (operation)))
       (is (ancestry/proven-ancestor? @conn b c (operation)))
       (is (ancestry/proven-ancestor? @conn c c (operation)))
       (is (false? (ancestry/proven-ancestor? @conn c b (operation))))
       (is (false? (ancestry/proven-ancestor? @conn b side (operation))))
       (is (thrown? clojure.lang.ExceptionInfo (ancestry/proven-ancestor? @conn a frontier (operation))))
       (doseq [limits [{:commit-visits 0} {:graph-edges 0}]]
         (is (thrown? clojure.lang.ExceptionInfo (ancestry/proven-ancestor? @conn a c (operation limits)))))))))

(deftest cyclic-history-is-a-refusal-rather-than-false-reachability
  (fixture
   (fn [conn]
     (let [[a b target] (repeatedly 3 random-uuid)]
       (d/transact conn [{:geschichte.commit/id a :geschichte.commit/parents [{:geschichte.commit/id b}]}
                         {:geschichte.commit/id b :geschichte.commit/parents [{:geschichte.commit/id a}]}])
       (is (thrown? clojure.lang.ExceptionInfo (ancestry/proven-ancestor? @conn target a (operation))))))))
