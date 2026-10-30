(ns is.simm.ops.github-push-pack-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.blob :as blob]
            [geschichte.budget :as budget]
            [is.simm.model.github-push :as push]
            [is.simm.ops.github-push-pack :as pack]))

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (d/transact conn [{:db/ident :test/pack :db/valueType :db.type/store-ref :db/cardinality :db.cardinality/one}])
        (f conn)
        (finally (d/release conn) (d/delete-database cfg))))))

(defn- operation [size] (budget/create {:limits {:pack-bytes size} :timeout-ms 30000}))

(deftest binary-pack-has-a-real-typed-reference-and-bounded-verified-read
  (fixture
   (fn [conn]
     (let [value (byte-array [0 1 2 3 4]) ref (blob/blob-id value)]
       (pack/publish! conn value #(d/transact conn [{:test/pack %}]))
       (is (= ref (d/q '[:find ?ref . :where [_ :test/pack ?ref]] @conn)))
       (is (= (seq value) (seq (pack/read! conn ref 5 (operation 5)))))
       (is (thrown? clojure.lang.ExceptionInfo (pack/read! conn ref 4 (operation 5))))
       (is (thrown? clojure.lang.ExceptionInfo (pack/read! conn ref 6 (operation 6))))
       (is (thrown? clojure.lang.ExceptionInfo (pack/read! conn ref 5 (operation 4))))
       (is (thrown? clojure.lang.ExceptionInfo (pack/read! conn ref (inc push/max-pack-bytes) (operation push/max-pack-bytes))))))))
