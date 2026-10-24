(ns is.simm.ops.owned-metadata-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [dvergr.room.registry :as registry]
            [dvergr.runtime.preparation :as preparation]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.ops.dvergr :as principals]
            [is.simm.ops.owned-charts-test :as fixture]
            [is.simm.ops.owned-metadata :as metadata]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.impl.simple :as simple])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(def request {:folders ["" "wiki"] :paths ["wiki/sales.md" "wiki/missing.md"]})

(defn- read! [{:keys [host w]}]
  (metadata/read! host (principals/party-principal (:member w)) (str (:room w)) request))

(deftest metadata-projects-one-owned-snapshot-and-explicit-absence
  (@#'fixture/with-fixture
   (fn [{:keys [owned attached] :as context}]
     (let [before @owned other @attached raw (files/metadata-in before request)
           result (read! context)]
       (is (= 2 (:protocol-version result)))
       (is (= (:folders raw) (:folders result)))
       (is (= (:entries raw) (:entries result)))
       (is (contains? (:entries result) "wiki/missing.md"))
       (is (nil? (get-in result [:entries "wiki/missing.md"])))
       (is (= (hasch/uuid [:simmis/owned-memory-metadata-v1 (:target-stamp result) (:revision raw)])
              (:revision result)))
       (is (= (:definition-basis raw) (:definition-basis result)))
       (is (= {:write? true :evaluate? false} (:permissions result)))
       (is (= before @owned)) (is (= other @attached))
       (is (not-any? #(contains? result %) [:db :conn :room :system :lease :control-db]))))))

(deftest metadata-revalidates-authority-and-default-but-allows-as-of-source
  (@#'fixture/with-fixture
   (fn [{:keys [conn w owned attached-descriptor] :as context}]
     (let [project files/metadata-in]
       (testing "a same-target source write does not invalidate an immutable observation"
         (let [before (project @owned request) calls (atom 0)]
           (with-redefs [files/metadata-in
                         (fn [db demand]
                           (let [result (project db demand)]
                             (when (= 1 (swap! calls inc))
                               (repo/write! owned "wiki/new.md" (markdown/source-bytes "# New\n")))
                             result))]
             (let [result (read! context)]
               (is (= (:folders before) (:folders result)))
               (is (= (:definition-basis before) (:definition-basis result)))
               (is (= 1 @calls) "No second projection or global commit equality gate")))))
       (testing "revoked read cannot publish even a successfully projected snapshot"
         (with-redefs [files/metadata-in
                       (fn [db demand]
                         (let [result (project db demand)]
                           (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties
                                              [:party/id (:member w)]]])
                           result))]
           (is (thrown? clojure.lang.ExceptionInfo (read! context))))
         (d/transact conn [{:room/id (:room w) :room/parties [[:party/id (:member w)]]}]))
       (testing "default replacement refuses the old projection"
         (with-redefs [files/metadata-in
                       (fn [db demand]
                         (let [result (project db demand)]
                           (d/transact conn [{:room/id (:room w) :room/default-repository
                                             [:system/id (:registry-system attached-descriptor)]}])
                           result))]
           (is (thrown? clojure.lang.ExceptionInfo (read! context)))))))))

(deftest malformed-draining-and-retired-reads-do-not-hydrate
  (@#'fixture/with-fixture
   (fn [{:keys [host w hydrations owner] :as context}]
     (let [before (count @hydrations) principal (principals/party-principal (:member w))]
       (doseq [bad [nil {:folders [] :paths [] :host :injected}
                    {:folders (vec (repeat 65 "wiki")) :paths []}
                    {:folders [] :paths ["../escape.md"]}]]
         (is (thrown? clojure.lang.ExceptionInfo
                      (metadata/read! host principal (str (:room w)) bad))))
       (with-redefs [simple/on-drain-thread? (constantly true)]
         (is (thrown? clojure.lang.ExceptionInfo (read! context))))
       (is (= before (count @hydrations)))
       (lifetime/fence! owner)
       (is (thrown? clojure.lang.ExceptionInfo (read! context)))
       (is (= before (count @hydrations)))))))

(deftest final-permission-flags-use-current-control-without-denying-a-reader
  (@#'fixture/with-fixture
   (fn [{:keys [conn host w]}]
     (let [project files/metadata-in
           principal (principals/party-principal (:owner w))
           read-owner #(metadata/read! host principal (str (:room w)) request)]
       (is (true? (get-in (read-owner) [:permissions :evaluate?])))
       (with-redefs [files/metadata-in
                     (fn [db demand]
                       (let [result (project db demand)]
                         (d/transact conn [[:db/retract [:room/id (:room w)] :room/owner
                                            [:party/id (:owner w)]]])
                         result))]
         (let [result (read-owner)]
           (is (= 2 (:protocol-version result)))
           (is (= {:write? true :evaluate? false} (:permissions result)))))))))

(deftest actual-room-retirement-cancels-the-read-and-releases-its-native-lease
  (@#'fixture/with-fixture
   (fn [{:keys [room] :as context}]
     (let [project files/metadata-in entered (CountDownLatch. 1) resume (CountDownLatch. 1)
           unregistered? (atom false)]
       (with-redefs [files/metadata-in
                     (fn [db demand]
                       (let [result (project db demand)]
                         (.countDown entered)
                         ;; Even a projection that swallows its interruption
                         ;; cannot publish after the native lease was cancelled.
                         (try (.await resume) (catch InterruptedException _ nil))
                         result))]
         (let [worker (future (try (read! context) (catch Throwable error error)
                                   (finally (Thread/interrupted))))]
           (try
             (is (.await entered 30 TimeUnit/SECONDS))
             ;; Real lifecycle cancellation interrupts the blocked projection
             ;; and joins its native lease. No fake currentness predicate.
             (registry/unregister! (:id room))
             (reset! unregistered? true)
             (is (instance? Throwable (deref worker 5000 ::timeout)))
             (is (not-any? #(= :host-operation (:kind %))
                           (vals (get @(:meta room) preparation/leases-key))))
             (finally
               (.countDown resume)
               (when (= ::not-joined (deref worker 5000 ::not-joined))
                 (throw (ex-info "Metadata projection worker did not exit; preserve fixture resources" {})))
               ;; Restore only after unregister's actual physical join returned.
               (when @unregistered? (registry/register! room))))))))))
