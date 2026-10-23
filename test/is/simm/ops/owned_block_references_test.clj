(ns is.simm.ops.owned-block-references-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [geschichte.repo :as repo]
            [is.simm.documents.block-reference :as reference]
            [is.simm.documents.chart-test :as chart-data]
            [is.simm.documents.markdown :as markdown]
            [is.simm.ops.dvergr :as principals]
            [is.simm.ops.owned-block-references :as references]
            [is.simm.ops.owned-charts-test :as fixture]
            [is.simm.runtimes.operation-lifetime :as lifetime]))

(defn request [context]
  (merge (select-keys (@#'fixture/request context) [:expected-target-stamp])
         {:repository-id (:repository-id (reference/selection @(:owned context)))
          :block-id chart-data/block-id}))

(defn- read! [{:keys [host w]} request]
  (references/read! host (principals/party-principal (:member w)) (str (:room w)) request))

(deftest reads-use-owned-target-lifetimes-and-current-human-authority
  (@#'fixture/with-fixture
   (fn [{:keys [host w owned attached owner hydrations] :as context}]
     (let [r (request context) before @owned other @attached result (read! context r)]
       (is (= :resolved (:status result)))
       (is (= "wiki/sales.md" (:path result)))
       (is (= (:expected-target-stamp r) (:target-stamp result)))
       (is (= before @owned)) (is (= other @attached))
       (is (not-any? #(contains? result %) [:db :conn :room :system :lease :control-db]))
       (let [hydrated (count @hydrations)]
         (doseq [principal [(principals/party-principal (:non-member w))
                           (assoc (principals/party-principal (:member w)) :principal/kind :agent)]]
           (is (thrown? clojure.lang.ExceptionInfo (references/read! host principal (str (:room w)) r))))
         (is (= hydrated (count @hydrations))))
       (lifetime/fence! owner)
       (is (thrown? clojure.lang.ExceptionInfo (read! context r)))))))

(deftest source-descriptor-and-authority-are-rechecked-after-parsing
  (@#'fixture/with-fixture
   (fn [{:keys [conn w owned attached-descriptor] :as context}]
     (let [r (request context) resolve reference/resolve-selected]
       (testing "unrelated JSON changes permit the same source resolution"
         (with-redefs [reference/resolve-selected
                       (fn [db selected request]
                         (let [result (resolve db selected request)]
                           (repo/write! owned "data/unrelated.json" (markdown/source-bytes "{}")) result))]
           (is (= :resolved (:status (read! context r))))))
       (testing "a newly added duplicate cannot cross the read boundary"
         (with-redefs [reference/resolve-selected
                       (fn [db selected request]
                         (let [result (resolve db selected request)]
                           (repo/write! owned "wiki/duplicate.md"
                                        (markdown/source-bytes (chart-data/source chart-data/declaration chart-data/body)))
                           result))]
           (is (thrown? clojure.lang.ExceptionInfo (read! context r))))
         (repo/remove! owned "wiki/duplicate.md"))
       (testing "revoking read membership during parsing refuses the result"
         (with-redefs [reference/resolve-selected
                       (fn [db selected request]
                         (let [result (resolve db selected request)]
                           (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
                           result))]
           (is (thrown? clojure.lang.ExceptionInfo (read! context r))))
         (d/transact conn [{:room/id (:room w) :room/parties [[:party/id (:member w)]]}]))
       (testing "unchanged source cannot retarget a replaced default repository"
         (with-redefs [reference/resolve-selected
                       (fn [db selected request]
                         (let [result (resolve db selected request)]
                           (d/transact conn [{:room/id (:room w) :room/default-repository
                                             [:system/id (:registry-system attached-descriptor)]}])
                           result))]
           (is (thrown? clojure.lang.ExceptionInfo (read! context r)))))))))
