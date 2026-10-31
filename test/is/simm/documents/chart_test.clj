(ns is.simm.documents.chart-test
  (:require [clj-yaml.core :as yaml]
            [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [geschichte.repo :as repo]
            [is.simm.documents.chart :as chart]
            [is.simm.documents.chart-profile :as feature]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.space :as space]
            [is.simm.fixtures.accounting :as accounting-fixture]
            [kontor.governance :as governance]))

(def page-id #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")
(def block-id #uuid "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb")
(def declaration
  {"simmis-id" (str page-id)
   "simmis" {"version" 1 "blocks"
             {(str block-id) {"types" [(str feature/type-id)]
                              "properties" {"data" {"id" (str feature/property-id)
                                                      "type" (str feature/type-id)}}
                              "values" {"data" "data/sales.json"}}}}})
(def spec "{\"version\":1,\"kind\":\"line\",\"series\":[{\"label\":\"Sales\",\"points\":[[0,1],[1,2]]}]}")
(def body (str "# Projection\n\n<!-- simmis:block " block-id " -->\n\n"
               "An explanation that stays editable.\n\n<!-- /simmis:block -->\n"))

(defn source [metadata body] (str "---\n" (yaml/generate-string metadata) "---\n" body))
(defn- write! [conn path text] (repo/write! conn path (markdown/source-bytes text)))
(defn- request [db]
  (let [file (files/read-file-in db "wiki/sales.md")]
    {:path "wiki/sales.md" :block block-id :expected (:entry file)
     :revision (get-in file [:analysis :revision])}))

(defn- checks [conn]
  (write! conn "wiki/sales.md" (source declaration body))
  (write! conn "data/sales.json" spec)
  (is (thrown? clojure.lang.ExceptionInfo (chart/inspect-in @conn (request @conn))))
  (d/transact conn (feature/installation-tx @conn))
  (let [db @conn before (:max-tx db) r (request db) result (chart/inspect-in db r)]
    (is (= :ready (:status result)))
    (is (= page-id (:document result)))
    (is (= block-id (:block result)))
    (is (= "data/sales.json" (:spec-path result)))
    (is (= [[0.0 1.0] [1.0 2.0]] (get-in result [:chart :series 0 :points])))
    (is (= before (:max-tx @conn)))
    (testing "expected source identity never selects another revision or path"
      (doseq [bad [(assoc r :expected nil) (assoc r :revision (random-uuid))
                   (assoc r :block (random-uuid)) (assoc r :spec-path "foreign.json")
                   (assoc r :world :parent) (assoc r :repository (random-uuid))]]
        (is (thrown? clojure.lang.ExceptionInfo (chart/inspect-in db bad)))))
    (testing "a spec-only write changes the projection, while the old immutable DB is coherent"
      (write! conn "data/sales.json" (.replace spec "[1,2]" "[1,3]"))
      (let [changed (chart/inspect-in @conn r)]
        (is (= (:expected result) (:expected changed)))
        (is (not= (:spec-entry result) (:spec-entry changed)))
        (is (= [[0.0 1.0] [1.0 3.0]] (get-in changed [:chart :series 0 :points])))
        (is (= result (chart/inspect-in db r)))))
    (testing "ordinary fences, provisional blocks and missing declarations are inert"
      (doseq [[metadata content] [[{"simmis-id" (str page-id)} body]
                                  [declaration "# Plain\n\n```chart\n{}\n```\n"]
                                  [(dissoc declaration "simmis-id") body]
                                  [(assoc-in declaration ["simmis" "blocks" (str block-id) "values" "data"]
                                             "../private.json") body]]]
        (write! conn "wiki/sales.md" (source metadata content))
        (is (thrown? clojure.lang.ExceptionInfo (chart/inspect-in @conn (request @conn))))))
    (write! conn "wiki/sales.md" (source declaration body))
    (testing "hostile or missing specs never produce a chart"
      (doseq [text ["#=(System/exit 0)" "{\"url\":\"https://example.com\"}" (apply str (repeat 65537 " "))]]
        (write! conn "data/sales.json" text)
        (is (thrown? clojure.lang.ExceptionInfo (chart/inspect-in @conn (request @conn))))))
    (is (= (source declaration body) (:text (files/read-file-in @conn "wiki/sales.md"))))))

(deftest saved-chart-is-a-pure-one-snapshot-projection
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false
             :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (accounting-fixture/with-schema-only #(space/provision! conn))
        (checks conn)
        (finally (governance/ungovern! conn) (d/release conn) (d/delete-database cfg))))))
