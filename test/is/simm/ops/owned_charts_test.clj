(ns is.simm.ops.owned-charts-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [dvergr.catalog.workspace :as catalog]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.repo :as repo]
            [is.simm.documents.chart :as chart]
            [is.simm.documents.chart-profile :as feature]
            [is.simm.documents.chart-test :as data]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.space :as space]
            [is.simm.fixtures.accounting :as accounting-fixture]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.owned-charts :as charts]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [kontor.governance :as governance]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg]
            [yggdrasil.adapters.geschichte :as gy]))

(defn- with-fixture [f]
  (let [open! catalog/open-memory-workspace! calls (atom 0)]
    (with-redefs [catalog/open-memory-workspace!
                  (fn [room]
                    (if (= 1 (swap! calls inc))
                      (let [cfg (assoc workspace/repository-options :store {:backend :memory :id (random-uuid)})]
                        (d/create-database cfg)
                        (let [conn (d/connect cfg)]
                          (accounting-fixture/with-schema-only #(space/provision! conn))
                          (binding [ec/*execution-context* (:ctx room)]
                            (ygg/register! (gy/create conn {:system-name (str "chart-fixture-" (random-uuid))})))
                          conn))
                      (open! room)))]
      (@#'fixture/with-fixture
       true
       (fn [{:keys [host owned] :as context}]
         (let [owner (lifetime/create)]
           (lifetime/open! owner)
           (try
             (d/transact owned (feature/installation-tx @owned))
             (repo/write! owned "wiki/sales.md" (markdown/source-bytes (data/source data/declaration data/body)))
             (repo/write! owned "data/sales.json" (markdown/source-bytes data/spec))
             (f (assoc context :owner owner
                       :host (assoc-in host [:config :host-lifecycle :with-operation]
                                       #(lifetime/with-operation! owner %))))
             (finally (lifetime/fence! owner) (lifetime/join! owner 5000)
                      (governance/ungovern! owned)))))))))

(defn- request [{:keys [host w owned]}]
  (let [binding (reads/resolve-host-read! host (ops/party-principal (:member w)) (str (:room w)))
        file (files/read-file-in @owned "wiki/sales.md")]
    {:path "wiki/sales.md" :block data/block-id :expected (:entry file)
     :revision (get-in file [:analysis :revision]) :expected-target-stamp (:target-stamp binding)}))

(defn- read! [{:keys [host w]} request]
  (charts/read! host (ops/party-principal (:member w)) (str (:room w)) request))

(deftest current-human-read-does-not-require-merge-or-use-selected-attachment
  (with-fixture
    (fn [{:keys [host w owned attached hydrations owner] :as context}]
      (let [r (request context) selected (workspace/selected-system-id)
            before @owned other @attached result (read! context r)]
        (is (= :ready (:status result)))
        (is (= (:expected-target-stamp r) (:target-stamp result)))
        (is (= [[0.0 1.0] [1.0 2.0]] (get-in result [:chart :series 0 :points])))
        (is (= selected (workspace/selected-system-id)))
        (is (= before @owned)) (is (= other @attached))
        (is (not-any? #(contains? result %) [:db :conn :room :system :compiled :lease :control-db]))
        (let [before (count @hydrations)]
          (doseq [principal [(ops/party-principal (:non-member w))
                              (assoc (ops/party-principal (:member w)) :principal/kind :agent)]]
            (is (thrown? clojure.lang.ExceptionInfo (charts/read! host principal (str (:room w)) r))))
          (is (= before (count @hydrations))))
        (lifetime/fence! owner)
        (is (thrown? clojure.lang.ExceptionInfo (read! context r)))))))

(deftest final-gates-refuse-revocation-source-race-and-default-replacement
  (with-fixture
    (fn [{:keys [conn w owned attached-descriptor] :as context}]
      (let [r (request context) inspect chart/inspect-in]
        (testing "a spec-only write during the read cannot return stale values"
          (let [calls (atom 0)]
            (with-redefs [chart/inspect-in (fn [db query]
                                           (let [result (inspect db query)]
                                             (when (= 1 (swap! calls inc))
                                               (repo/write! owned "data/sales.json"
                                                            (markdown/source-bytes (.replace data/spec "[1,2]" "[1,3]"))))
                                             result))]
              (is (thrown? clojure.lang.ExceptionInfo (read! context r))))))
        (testing "revocation is checked again after body reads"
          (let [calls (atom 0)]
            (with-redefs [chart/inspect-in (fn [db query]
                                           (let [result (inspect db query)]
                                             (when (= 1 (swap! calls inc))
                                               (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties
                                                                  [:party/id (:member w)]]]))
                                             result))]
              (is (thrown? clojure.lang.ExceptionInfo (read! context r)))))
          (d/transact conn [{:room/id (:room w) :room/parties [[:party/id (:member w)]]}]))
        (testing "an unchanged document cannot retarget a replaced default"
          (let [calls (atom 0)]
            (with-redefs [chart/inspect-in (fn [db query]
                                           (let [result (inspect db query)]
                                             (when (= 1 (swap! calls inc))
                                               (d/transact conn [{:room/id (:room w) :room/default-repository
                                                                 [:system/id (:registry-system attached-descriptor)]}]))
                                             result))]
              (is (thrown? clojure.lang.ExceptionInfo (read! context r))))))))))
