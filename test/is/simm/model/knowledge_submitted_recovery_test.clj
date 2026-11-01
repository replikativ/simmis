(ns is.simm.model.knowledge-submitted-recovery-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.room.store :as native]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.knowledge-batch-recovery :as unlinked]
            [is.simm.model.knowledge-submitted-recovery :as recovery]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.knowledge-schedule-batches :as scheduled]
            [is.simm.model.workflow-schedules :as schedules]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.dvergr :as principals]
            [is.simm.ops.knowledge-batch-proof :as proof]
            [is.simm.ops.knowledge-batch-proof-test :as fixture]
            [is.simm.ops.owned-knowledge-discovery :as discovery]))

(defn- with-submitted [f]
  (@#'fixture/fixture
   (fn [{:keys [conn w ready] :as data}]
     (d/transact conn (concat unlinked/schema recovery/schema))
     (let [row (@#'fixture/joint-claim! data)
           id (:work.request/id row) job (random-uuid)]
       (requests/record-job! conn id (:work.request/claim row) job)
       (requests/mark-returned! conn id)
       (let [row (requests/get-request conn id)
             p (proof/retained! conn (:owner w) (:room w) (:batch-origin ready))
             native {:kind :candidate-failed :job job :status :completed
                     :candidate (random-uuid) :attempt-content (random-uuid)
                     :core (native/job-cleanup-ack-id job :core)
                     :adapter (native/job-cleanup-ack-id job :adapter)
                     :family (native/family-return-ack-id job)}
             expected (recovery/expected p row native nil)
             decision (recovery/decision {:batch (:batch ready) :request id :room (:room w)
                                          :by (:member w) :client (random-uuid) :at (java.util.Date. 1000)
                                          :reason "Make this evidence eligible for a fresh update"})]
         (f (assoc data :request id :proof p :expected expected :decision decision)))))))

(defn- commit! [{:keys [conn expected decision]}]
  (d/transact conn [[:db.fn/call recovery/requeue-tx expected decision]]))

(deftest failed-update-recovery-retains-history-and-never-dispatches
  (with-submitted
    (fn [{:keys [conn ready request decision] :as data}]
      (let [before (streams/stream @conn (:stream ready))
            original (requests/get-request conn request)
            generation (:knowledge.stream/batch-generation before)]
        (commit! data)
        (let [after (streams/stream @conn (:stream ready))
              b (batches/batch @conn (:batch ready))]
          (is (= (-> before (dissoc :knowledge.stream/active-batch)
                     (assoc :knowledge.stream/batch-generation (inc generation))) after))
          (is (= original (requests/get-request conn request)))
          (is (= :closed (:knowledge.batch/state b)))
          (is (= :failed-requeued (:knowledge.batch/outcome b)))
          (is (= (:id decision) (:knowledge.batch/recovery-id b)))
          (is (= (:by decision) (:knowledge.batch/recovery-by b)))
          (is (recovery/recovered? @conn (:batch ready) request))
          (is (not-any? :knowledge.item/batch (streams/items @conn (:stream ready))))
          (is (every? #(= :pending (:knowledge.item/state %)) (streams/items @conn (:stream ready))))
          (is (= 1 (count (requests/requests conn)))))))))

(deftest historical-replay-cannot-clear-a-newer-batch-or-repeat-generation
  (with-submitted
    (fn [{:keys [conn ready w host batch-input expected decision request] :as data}]
      (commit! data)
      (let [next (discovery/prepare-batch! host (principals/party-principal (:owner w)) (str (:room w)) batch-input)
            before (streams/stream @conn (:stream ready))
            replay (assoc decision :at (java.util.Date. 99999))]
        (is (= :ready (:status next)))
        (is (not= (:batch ready) (:batch next)))
        (is (recovery/recovered? @conn (:batch ready) request))
        (is (= [] (recovery/requeue-tx @conn expected replay)))
        (d/transact conn [[:db.fn/call recovery/requeue-tx expected replay]])
        (is (= before (streams/stream @conn (:stream ready))))
        (is (= (:at decision) (:knowledge.batch/recovery-at (batches/batch @conn (:batch ready)))))))))

(deftest changed-bindings-and-partial-receipts-stay-held
  (with-submitted
    (fn [{:keys [conn ready expected decision request] :as data}]
      (let [db @conn b (batches/batch db (:batch ready))
            s (streams/stream db (:stream ready))
            row (requests/get-request conn request)
            item (first (filter #(= (:batch ready) (:knowledge.item/batch %)) (streams/items db (:stream ready))))
            damaged [[{:db/id (:db/id b) :knowledge.batch/state :review}]
                     [{:db/id (:db/id b) :knowledge.batch/recovery-client (random-uuid)}]
                     [{:db/id (:db/id b) :knowledge.batch/request-intent (random-uuid)}]
                     [{:db/id (:db/id row) :work.request/return-pending? true}]
                     [{:db/id [:knowledge.stream/id (:stream ready)] :knowledge.stream/revision (inc (:knowledge.stream/revision s))}]
                     [{:db/id (:db/id item) :knowledge.item/state :pending}]]]
        (doseq [tx damaged]
          (let [current (:db-after (d/with db tx))]
            (is (thrown? Exception (recovery/requeue-tx current expected decision)) (pr-str tx))))
        (is (thrown? Exception (recovery/requeue-tx db (assoc-in expected [:native :family] (random-uuid)) decision)))
        (is (thrown? Exception (recovery/requeue-tx db expected (assoc decision :request (random-uuid)))))
        (let [native (:native expected)
              complete (dispositions/make {:batch (:batch ready) :request request :intent (:work.request/intent-id row)
                                           :job (:job native) :candidate (:candidate native)
                                           :attempt-content (:attempt-content native) :basis (:work.request/publication-basis-id row)
                                           :output (random-uuid) :kind :no-op} 2)
              disposition-tx (merge {:db/id (:db/id row)}
                                    (into {} (map (fn [[k v]] [(keyword "knowledge.disposition" (name k)) v])) complete))]
          (doseq [change [{:db/id (:db/id row) :knowledge.disposition/id (random-uuid)} disposition-tx]]
            (is (thrown? Exception (recovery/requeue-tx (:db-after (d/with db [change])) expected decision)))))
        (is (nil? (:knowledge.batch/recovery-id (batches/batch @conn (:batch ready)))))
        (commit! data)
        (let [recovered @conn b (batches/batch recovered (:batch ready))
              required [:knowledge.batch/claim :knowledge.batch/selection :knowledge.batch/selection-id
                        :knowledge.batch/frozen :knowledge.batch/frozen-id]
              changes (concat
                       (map (fn [k] [[:db/retract (:db/id b) k (get b k)]]) required)
                       [[{:db/id (:db/id row) :knowledge.disposition/id (random-uuid)}]
                        [{:work.request/id (random-uuid) :work.request/knowledge-batch (:batch ready)}]
                        [[:db/retract [:knowledge.stream/id (:stream ready)] :knowledge.stream/batch-generation
                          (:knowledge.stream/batch-generation (streams/stream recovered (:stream ready)))]]
                        [{:db/id [:knowledge.stream/id (:stream ready)] :knowledge.stream/by (random-uuid)}]])]
          (doseq [tx changes]
            (is (false? (recovery/recovered? (:db-after (d/with recovered tx)) (:batch ready) request)) (pr-str tx)))
          (doseq [changed [(assoc decision :reason "Different reason") (assoc decision :client (random-uuid))]]
            (is (thrown? Exception (recovery/requeue-tx recovered expected changed)))))))))

(deftest native-job-failure-cannot-acquire-a-fictional-candidate-attempt
  (let [id (random-uuid)
        value {:kind :job-failed :job id :status :failed
               :core (native/job-cleanup-ack-id id :core) :adapter (native/job-cleanup-ack-id id :adapter)
               :family (native/family-return-ack-id id)}]
    (is (= value (recovery/native-proof! value)))
    (is (thrown? Exception (recovery/native-proof! (assoc value :candidate (random-uuid) :attempt-content (random-uuid)))))))

(deftest scheduled-recovery-requires-both-links-and-the-exact-failure
  (with-submitted
    (fn [{:keys [conn w ready proof expected decision request]}]
      (d/transact conn (concat schedules/schema scheduled/schema))
      (let [row (requests/get-request conn request) bid (:batch ready)
            batch (batches/batch @conn bid) oid (:work.request/client-id row)
            aid (random-uuid) revision (random-uuid) n (:native expected)
            activation {:schedule.activation/id aid :schedule.activation/by (:owner w)
                        :schedule.activation/room (:room w) :schedule.activation/revision revision
                        :schedule.activation/enabled? false :schedule.activation/last-terminal-failure oid}
            occurrence {:schedule.occurrence/id oid :schedule.occurrence/activation aid
                        :schedule.occurrence/revision revision :schedule.occurrence/state :linked
                        :schedule.occurrence/request request :schedule.occurrence/knowledge-batch bid
                        :schedule.occurrence/batch-ref (random-uuid) :schedule.occurrence/batch-revision (random-uuid)
                        :schedule.occurrence/failed-candidate (:candidate n)
                        :schedule.occurrence/failed-attempt-content (:attempt-content n)}]
        (d/transact conn [activation occurrence {:db/id (:db/id batch) :knowledge.batch/scheduled-occurrence oid}])
        (let [o (schedules/occurrence conn oid)
              e (recovery/expected (assoc proof :row (batches/batch @conn bid)) row n o)]
          (is (thrown? Exception (recovery/requeue-tx @conn (assoc-in e [:native :candidate] (random-uuid)) decision)))
          (d/transact conn [[:db.fn/call recovery/requeue-tx e decision]])
          (let [db @conn b (batches/batch db bid)]
            (is (recovery/recovered? db bid request))
            (is (nil? (schedules/resume-blocked-reason db activation)))
            (doseq [tx [[[:db/retract (:db/id b) :knowledge.batch/scheduled-occurrence oid]]
                        [[:db/retract (:db/id o) :schedule.occurrence/knowledge-batch bid]]
                        [{:db/id (:db/id o) :schedule.occurrence/failed-candidate (random-uuid)}]]]
              (let [current (:db-after (d/with db tx))]
                (is (false? (recovery/recovered? current bid request)))
                (is (= :reconciliation-required (schedules/resume-blocked-reason current activation)))))))))))
