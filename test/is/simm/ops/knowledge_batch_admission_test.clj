(ns is.simm.ops.knowledge-batch-admission-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [dvergr.jobs :as jobs]
            [dvergr.resource :as resource]
            [dvergr.room.registry :as registry]
            [dvergr.runtime.preparation :as preparation]
            [geschichte.content :as content]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.knowledge-batch-proof :as proof]
            [is.simm.ops.knowledge-batch-proof-test :as fixture]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflows :as source]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.core :as sp]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- claim [{:keys [conn host w room submission prepared]}]
  (@#'work/claim-batch-prepared! conn (:execution-ctx host) (ops/party-principal (:owner w))
                                (:room w) room submission prepared))

(deftest fixed-claim-publishes-one-exact-joint-receipt
  (@#'fixture/fixture
   (fn [{:keys [conn ready] :as data}]
     (let [result (claim data) row (:request result)
           forbidden (fn [& _] (throw (AssertionError. "Retry must not read source or prepare providers")))]
       (is (:claimed? result))
       (is (= :admitting (:work.request/state row)))
       (is (= (:batch ready) (:work.request/knowledge-batch row)))
       (is (= (:batch-origin ready) (:origin (proof/request! conn row))))
       (with-redefs [source/snapshot-in forbidden content/read-by-id forbidden work/prepare forbidden]
         (is (false? (:claimed? (claim data)))))
       (is (= 1 (count (d/q '[:find ?e :in $ ?batch :where [?e :work.request/knowledge-batch ?batch]] @conn (:batch ready)))))))))

(deftest lost-joint-ack-is-resolved-only-by-both-canonical-sides
  (@#'fixture/fixture
   (fn [{:keys [conn ready] :as data}]
     (let [publish artifact/publish-value!]
       (with-redefs [artifact/publish-value! (fn [store value callback]
                                             (let [result (publish store value callback)]
                                               (when (get-in value [:input :knowledge/batch])
                                                 (throw (ex-info "Lost joint ACK" {})))
                                               result))]
         (is (:claimed? (claim data))))
       (is (= :submitted (:knowledge.batch/state (batches/batch @conn (:batch ready)))))
       (is (false? (:claimed? (claim data))))))))

(deftest partial-linkage-never-authorizes-a-job
  (@#'fixture/fixture
   (fn [{:keys [conn ready] :as data}]
     (let [publish artifact/publish-value!]
       (with-redefs [artifact/publish-value! (fn [store value callback]
                                             (let [result (publish store value callback)]
                                               (when (get-in value [:input :knowledge/batch])
                                                 (let [row (batches/batch @conn (:batch ready))]
                                                   (d/transact conn [[:db/retract (:db/id row) :knowledge.batch/request
                                                                     (:knowledge.batch/request row)]])))
                                               result))]
         (is (thrown? Exception (claim data))))))))

(deftest writer-rechecks-mapping-and-permissions-after-the-final-host-observation
  (doseq [change [:default :permission]]
    (@#'fixture/fixture
     (fn [{:keys [conn w attached-descriptor] :as data}]
       (let [transact d/transact injected? (atom false)]
         (with-redefs [d/transact (fn [c tx]
                                   (when (and (identical? c conn) (= :db.fn/call (ffirst tx))
                                              (compare-and-set! injected? false true))
                                     (transact conn [(case change
                                                       :default {:room/id (:room w) :room/default-repository
                                                                 [:system/id (:registry-system attached-descriptor)]}
                                                       :permission {:room/id (:room w) :room/owner [:party/id (:member w)]})]))
                                   (transact c tx))]
           (is (thrown? Exception (claim data))))
         (is @injected?)
         (is (empty? (d/q '[:find ?e :where [?e :work.request/knowledge-batch _]] @conn))))))))

(deftest original-app-and-room-ownership-cannot-be-borrowed
  (@#'fixture/fixture
   (fn [{:keys [prepared room conn] :as data}]
     (is (thrown? Exception (claim (assoc data :prepared (dissoc prepared :knowledge/owner)))))
     (let [worker (future (try (claim data) :unexpected (catch Exception _ :refused)))]
       (is (= :refused (deref worker 5000 ::expired))))
     (with-redefs [registry/lookup (constantly (assoc room :incarnation (random-uuid)))]
       (is (thrown? Exception (claim data))))
     (is (empty? (d/q '[:find ?e :where [?e :work.request/knowledge-batch _]] @conn))))))

(deftest a-real-job-uses-native-lifecycle-without-retaining-the-source-lease
  (@#'fixture/fixture
   (fn [{:keys [conn host w room submission prepared] :as data}]
     (let [principal (ops/party-principal (:owner w)) ctx (:execution-ctx host)
           grant (work/resources (:limits submission)) entered (atom 0)
           prepared (assoc prepared :start! (fn [_]
                                              (is (empty? (get @(:meta room) preparation/leases-key)))
                                              (swap! entered inc)
                                              (sp/spin :done)))]
       ;; Disposable native accounting only, no provider or model inference.
       (binding [ec/*execution-context* (:ctx room)] (resource/mint! room {:id (random-uuid) :resources grant}))
       (let [result (work/submit-prepared! conn ctx principal (:room w) room submission prepared)
             status (jobs/status (:job result) 15000)]
         (is (= :completed (:status status)))
         (is (:settled? status))
         (is (= 1 @entered))
         (is (= grant (binding [ec/*execution-context* (:ctx room)] (resource/balance room))))
         (is (= (:job result) (:job (work/submit-prepared! conn ctx principal (:room w) room submission prepared))))
         (is (= 1 @entered)))))))

(deftest fixed-handoff-rejects-a-different-outer-submission
  (doseq [[field value] [[:request-id (random-uuid)] [:model "different-model"]
                        [:limits (assoc fixture/limits :max-dispatches 1)]]]
    (@#'fixture/fixture
     (fn [{:keys [conn submission] :as data}]
       (is (thrown? Exception (claim (assoc data :submission (assoc submission field value)))))
       (is (empty? (d/q '[:find ?e :where [?e :work.request/knowledge-batch _]] @conn)))))))

(deftest app-fenced-after-joint-claim-never-starts-a-job
  (@#'fixture/fixture
   (fn [{:keys [conn host w room submission prepared app-owner]}]
     (let [claim! @#'work/claim-batch-prepared!
           principal (ops/party-principal (:owner w)) ctx (:execution-ctx host)]
       (with-redefs-fn
         {#'work/claim-batch-prepared!
          (fn [& args]
            (let [result (apply claim! args)]
              (is (nil? (deref (future (lifetime/fence! app-owner)) 5000 ::fence-timeout)))
              result))
          #'jobs/start! (fn [& _] (throw (AssertionError. "Fenced App may not start a Job")))
          #'resource/allocate-run! (fn [& _] (throw (AssertionError. "Fenced App may not allocate resources")))}
         #(is (thrown? Exception (work/submit-prepared! conn ctx principal (:room w) room submission prepared))))
       (let [row (requests/get-request conn (requests/request-id (:owner w) (:room w) (:request-id submission)))]
         (is (= :interrupted (:work.request/state row)))
         (is (nil? (:work.request/job row)))
         (is (= (get-in prepared [:basis :knowledge/batch]) (:origin (proof/request! conn row)))))))))
