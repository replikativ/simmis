(ns is.simm.ops.owned-page-views-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.runtime.preparation :as preparation]
            [geschichte.repo :as repo]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.page-view-reader :as reader]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.owned-charts-test :as fixture]
            [is.simm.ops.owned-page-views :as views]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- with-fixture [f]
  (@#'fixture/with-fixture
   (fn [{:keys [owned] :as context}]
     (let [descriptor {:version 1 :id (random-uuid) :source {:kind :pages :roots ["page-view-data"]}
                       :presentation :table :columns [{:kind :title}]}]
       ;; The inherited attachment fixture intentionally has provisional
       ;; wiki/file.md. Keep it outside this exact valid source selection.
       (repo/write! owned "page-view-data/sales.md"
                    (markdown/source-bytes (str "---\nsimmis-id: " (random-uuid) "\n---\n# Sales\n")))
       (repo/write! owned "views/report.md"
                    (markdown/source-bytes (str "# Saved table\n\n```simmis-view\n" (pr-str descriptor) "\n```\n")))
       (f context)))))

(defn- request [{:keys [host w owned]}]
  (let [binding (reads/resolve-host-read! host (ops/party-principal (:member w)) (str (:room w)))
        file (files/read-file-in @owned "views/report.md")
        fence (first (get-in file [:analysis :page-views]))]
    (merge {:path "views/report.md" :expected (:entry file) :revision (get-in file [:analysis :revision])
            :expected-target-stamp (:target-stamp binding)} (select-keys fence [:from :to :view]))))

(defn- read! [{:keys [host w]} request]
  (views/read! host (ops/party-principal (:member w)) (str (:room w)) request))

(deftest page-view-is-human-read-only-and-uses-the-owned-not-selected-source
  (with-fixture
    (fn [{:keys [host w owned attached hydrations owner] :as context}]
      (let [sent (request context) before @owned other @attached result (read! context sent)]
        (is (= :ready (:status result)))
        (is (= ["page-view-data/sales.md"] (mapv :path (:rows result))))
        (is (= (:expected-target-stamp sent) (:target-stamp result)))
        (is (= before @owned)) (is (= other @attached))
        (is (not-any? #(contains? result %) [:db :conn :room :system :compiled :lease :control-db]))
        (let [count-before (count @hydrations)]
          (doseq [principal [(ops/party-principal (:non-member w))
                            (assoc (ops/party-principal (:member w)) :principal/kind :agent)]]
            (is (thrown? clojure.lang.ExceptionInfo (views/read! host principal (str (:room w)) sent))))
          (is (= count-before (count @hydrations))))
        (lifetime/fence! owner)
        (is (thrown? clojure.lang.ExceptionInfo (read! context sent)))))))

(deftest authority-and-source-races-are-checked-after-table-acquisition
  (with-fixture
    (fn [{:keys [conn w owned attached-descriptor] :as context}]
      (let [sent (request context) inspect reader/inspect-in]
        (doseq [change! [(fn [] (repo/write! owned "page-view-data/new.md"
                                           (markdown/source-bytes (str "---\nsimmis-id: " (random-uuid) "\n---\nNew source\n"))))
                         (fn [] (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]]))
                         (fn [] (d/transact conn [{:room/id (:room w) :room/default-repository
                                                   [:system/id (:registry-system attached-descriptor)]}]))]]
          (let [calls (atom 0)]
            (with-redefs [reader/inspect-in (fn [db query]
                                             (let [result (inspect db query)]
                                               (when (= 1 (swap! calls inc)) (change!))
                                               result))]
              (is (thrown? clojure.lang.ExceptionInfo (read! context sent)))))
          ;; Restore membership for the following independent gate; no grant
          ;; or publication is installed by this fixture action.
          (d/transact conn [{:room/id (:room w) :room/parties [[:party/id (:member w)]]}]))))))

(deftest physical-body-read-retains-the-original-app-until-actual-exit
  (with-fixture
    (fn [{:keys [owner] :as context}]
      (let [sent (request context) original files/read-file-in
            entered (CountDownLatch. 1) release (CountDownLatch. 1) first? (atom true)]
        (with-redefs [files/read-file-in
                      (fn [db path]
                        (when (and (= "views/report.md" path) (compare-and-set! first? true false))
                          (.countDown entered)
                          (when-not (.await release 10 TimeUnit/SECONDS)
                            (throw (ex-info "Test body read did not resume" {}))))
                        (original db path))]
          (let [worker (future (read! context sent))]
            (try
              (is (.await entered 10 TimeUnit/SECONDS))
              (lifetime/fence! owner)
              (is (thrown? clojure.lang.ExceptionInfo (lifetime/join! owner 20)))
              (is (not (realized? worker)))
              (finally (.countDown release)))
            (is (= :ready (:status (deref worker 10000 {:status :timeout}))))
            (is (nil? (lifetime/join! owner 5000)))))))))

(deftest final-metadata-gates-follow-the-last-body-read-without-reread-loops
  (doseq [change [:revoke :default :room-cancel :third-source]]
    (with-fixture
      (fn [{:keys [conn w room owned attached-descriptor] :as context}]
        (let [sent (request context) inspect reader/inspect-in calls (atom 0)
              entered (CountDownLatch. 1) release (CountDownLatch. 1)]
          (with-redefs [reader/inspect-in
                        (fn [database query]
                          (let [n (swap! calls inc) result (inspect database query)]
                            (case n
                              1 (repo/write! owned "unrelated.txt" (markdown/source-bytes "Force one revalidation"))
                              2 (do (.countDown entered)
                                    (loop []
                                      (let [outcome (try (.await release 10 TimeUnit/SECONDS)
                                                         (catch InterruptedException _ :interrupted))]
                                        ;; Physical IO may absorb cancellation.
                                        ;; Its valid result must still be gated.
                                        (case outcome :interrupted (recur) true nil
                                              (throw (ex-info "Test read did not resume" {}))))))
                              (throw (AssertionError. "No third body-read loop is permitted")))
                            result))]
            (let [worker (future (try (read! context sent) {:status :unexpected-return}
                                     (catch Exception _ {:status :refused})))]
              (try
                (let [entered? (.await entered 10 TimeUnit/SECONDS)]
                  (is entered?)
                  (when entered?
                    (case change
                      :revoke (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
                      :default (d/transact conn [{:room/id (:room w) :room/default-repository
                                                  [:system/id (:registry-system attached-descriptor)]}])
                      :third-source (repo/write! owned "another-unrelated.txt" (markdown/source-bytes "Newer version"))
                      :room-cancel
                      (let [leases (binding [ec/*execution-context* (:ctx room)]
                                     (vec (vals (get @(:meta room) preparation/leases-key))))]
                        (is (seq leases))
                        (doseq [lease leases] (preparation/cancel! lease))))))
                (finally (.countDown release)))
              (is (= {:status :refused} (deref worker 10000 {:status :timeout})))
              (is (= 2 @calls)))))))))
