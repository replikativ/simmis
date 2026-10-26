(ns is.simm.model.knowledge-batch-recovery-test
  "Real frozen proof and atomic metadata transitions. Authorization and native
   physical exclusion belong to the fixed HOST adapter, tested separately."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [clojure.edn :as edn]
            [dvergr.catalog.room :as catalog]
            [geschichte.repo :as repo]
            [is.simm.model.knowledge-schedule-batches :as scheduled]
            [is.simm.model.workflow-schedules :as schedules]
            [is.simm.ops.knowledge-schedule-proof-test :as scheduled-fixture]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.workflows :as workflows]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-batch-recovery :as recovery]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.dvergr :as principals]
            [is.simm.ops.knowledge-batch-proof :as proof]
            [is.simm.ops.knowledge-batch-proof-test :as fixture]
            [is.simm.ops.owned-knowledge-discovery :as discovery]))

(defn- with-ready [f]
  (@#'fixture/fixture
   (fn [{:keys [conn w ready] :as data}]
     (d/transact conn (concat recovery/schema
                              [{:db/ident :knowledge.batch/adoption :db/valueType :db.type/uuid :db/cardinality :db.cardinality/one}
                               {:db/ident :knowledge.batch/disposition :db/valueType :db.type/uuid :db/cardinality :db.cardinality/one}
                               {:db/ident :knowledge.batch/decision-filing :db/valueType :db.type/string :db/cardinality :db.cardinality/one}]))
     (let [p (proof/ready! conn (:owner w) (:room w) (:batch-origin ready))
           expected (recovery/expected p nil)
           decision (recovery/decision {:batch (:batch ready) :room (:room w) :by (:owner w)
                                        :client (random-uuid) :at (java.util.Date. 0)
                                        :mode :skip :reason "Explicitly decline this source version"})]
       (f (assoc data :proof p :expected expected :decision decision))))))

(defn- commit! [{:keys [conn expected decision]}]
  (d/transact conn [[:db.fn/call recovery/abandon-tx expected decision]]))

(deftest explicit-skip-closes-exact-members-without-changing-discovery-or-its-root
  (with-ready
    (fn [{:keys [conn ready decision] :as data}]
      (let [before (streams/stream @conn (:stream ready))]
        (commit! data)
        (let [b (batches/batch @conn (:batch ready)) after (streams/stream @conn (:stream ready))]
          (is (= (dissoc before :knowledge.stream/active-batch) after))
          (is (= :closed (:knowledge.batch/state b)))
          (is (= :abandoned-skip (:knowledge.batch/outcome b)))
          (is (= (:id decision) (:knowledge.batch/recovery-id b)))
          (is (= (:by decision) (:knowledge.batch/recovery-by b)))
          (is (= (:at decision) (:knowledge.batch/recovery-at b)))
          (is (= 1 (count (filter #(= :skipped (:knowledge.item/state %)) (streams/items @conn (:stream ready))))))
          (is (= 1 (count (streams/pending-items @conn (:stream ready)))))
          (is (not-any? #(= :processed (:knowledge.item/state %)) (streams/items @conn (:stream ready))))
          (is (empty? (requests/requests conn))))))))

(deftest historical-recovery-preserves-a-genuine-later-batch-and-original-time
  (with-ready
    (fn [{:keys [conn ready w host batch-input expected decision] :as data}]
      (commit! data)
      (let [next-batch (discovery/prepare-batch! host (principals/party-principal (:owner w)) (str (:room w)) batch-input)
            before (streams/stream @conn (:stream ready))
            replay (assoc decision :at (java.util.Date. 99999))]
        (is (= :ready (:status next-batch)))
        (is (not= (:batch ready) (:batch next-batch)))
        (is (= [] (recovery/abandon-tx @conn expected replay)))
        (d/transact conn [[:db.fn/call recovery/abandon-tx expected replay]])
        (is (= before (streams/stream @conn (:stream ready))))
        (is (= (:at decision) (:knowledge.batch/recovery-at (batches/batch @conn (:batch ready)))))))))

(deftest a-late-request-cannot-claim-an-abandoned-ready-batch
  (with-ready
    (fn [{:keys [conn] :as data}]
      (commit! data)
      (is (thrown? Exception (@#'fixture/joint-claim! data)))
      (is (empty? (requests/requests conn))))))

(deftest indexed-orphan-request-marker-prevents-pretending-a-batch-was-unlinked
  (with-ready
    (fn [{:keys [conn ready] :as data}]
      (@#'fixture/joint-claim! data)
      (let [b (batches/batch @conn (:batch ready))]
        (d/transact conn [[:db/retract (:db/id b) :knowledge.batch/request (:knowledge.batch/request b)]
                          [:db/retract (:db/id b) :knowledge.batch/request-intent (:knowledge.batch/request-intent b)]
                          {:db/id (:db/id b) :knowledge.batch/state :ready}]))
      (is (thrown? Exception (commit! data)))
      (is (nil? (:knowledge.batch/recovery-id (batches/batch @conn (:batch ready))))))))

(deftest partial-terminal-receipts-and-different-intents-cannot-report-recovery
  (doseq [damage [:item :operation :outcome :active-pointer :reason :adoption :disposition :decision-filing]]
    (with-ready
      (fn [{:keys [conn ready decision expected] :as data}]
        (commit! data)
        (let [b (batches/batch @conn (:batch ready))
              i (first (filter #(= (:batch ready) (:knowledge.item/batch %)) (streams/items @conn (:stream ready))))]
          (case damage
            :item (d/transact conn [{:db/id (:db/id i) :knowledge.item/state :claimed}])
            :operation (d/transact conn [[:db/retract (:db/id b) :knowledge.batch/recovery-id (:id decision)]])
            :outcome (d/transact conn [{:db/id (:db/id b) :knowledge.batch/outcome :no-op}])
            :active-pointer (d/transact conn [{:db/id [:knowledge.stream/id (:stream ready)] :knowledge.stream/active-batch (:batch ready)}])
            :adoption (d/transact conn [{:db/id (:db/id b) :knowledge.batch/adoption (random-uuid)}])
            :disposition (d/transact conn [{:db/id (:db/id b) :knowledge.batch/disposition (random-uuid)}])
            :decision-filing (d/transact conn [{:db/id (:db/id b) :knowledge.batch/decision-filing "Conflicting native filing"}])
            :reason nil)
          (is (thrown? Exception (recovery/abandon-tx @conn expected
                                  (cond-> decision (= :reason damage) (assoc :reason "A conflicting instruction"))))))))))


(deftest a-foreign-valid-scheduled-anchor-cannot-withdraw-another-batchs-occurrence
  (@#'scheduled-fixture/with-schedule
   (fn [{:keys [conn owned host w ready target anchor] :as data}]
     (d/transact conn recovery/schema)
     (let [principal (principals/party-principal (:owner w)) handle (str (:room w))
           epoch (random-uuid)
           declaration (assoc (edn/read-string (get-in ready [:snapshot :files "input-batching.edn"])) :epoch epoch)]
       ;; A genuinely different program/epoch makes a second legitimate active
       ;; stream in the SAME control DB, each with its own scheduled binding.
       (repo/write! owned "workflows/wiki/input-batching.edn" (.getBytes (pr-str declaration) "UTF-8"))
       (let [snapshot (workflows/snapshot-in @owned "wiki")
             revision (parse-uuid (get-in (sources/details! host principal handle {:name "wiki"}) [:data :revision]))
             bundle (:id (catalog/bundle "wiki" (:files snapshot)))
             second (discovery/prepare-batch! host principal handle
                                             {:name "wiki" :expected-source revision :expected-bundle bundle :epoch epoch})
             second-target (-> target (assoc :snapshot snapshot)
                               (assoc-in [:execution :expected-owned-source] revision)
                               (assoc-in [:execution :expected-bundle] bundle)
                               (assoc :source-origin {:revision revision :target-stamp (get-in second [:source-origin :target-stamp])
                                                      :target (get-in second [:source-origin :target]) :manifest (:manifest snapshot)}))
             a (schedules/create! conn {:id (random-uuid) :room (:room w) :by (:owner w) :target second-target
                                        :now (java.util.Date. 0) :next-fire (java.util.Date. 60000)})
             _ (schedules/claim-due! conn a {:interval-ms 60000} (java.util.Date. 60000))
             o (first (schedules/occurrences conn (:schedule.activation/id a)))
             claim (schedules/admit! conn (:schedule.occurrence/id o))
             v (scheduled/binding-value {:activation (:schedule.activation/id a) :revision (:schedule.activation/revision a)
                                         :target-ref (:schedule.activation/target-ref a) :by (:owner w) :room (:room w)
                                         :occurrence (:schedule.occurrence/id o) :claim claim :batch (:batch-origin second)})
             retained (scheduled/retain! conn v)
             other-anchor {:value v :reference (:schedule.occurrence/batch-ref retained)}
             p (proof/retained! conn (:owner w) (:room w) (:batch-origin ready))
             expected (recovery/expected p other-anchor)
             value (recovery/decision {:batch (:batch ready) :room (:room w) :by (:owner w)
                                       :client (random-uuid) :at (java.util.Date.) :mode :skip :reason "Cannot use another anchor"})
             before @conn]
         (is (= :ready (:status second)))
         (is (not= (:batch ready) (:batch second)))
         (is (thrown? Exception (d/transact conn [[:db.fn/call recovery/abandon-tx expected value]])))
         (doseq [bid [(:batch ready) (:batch second)]]
           (is (= (batches/batch before bid) (batches/batch @conn bid))))
         (doseq [oid [(get-in anchor [:value :occurrence]) (:occurrence v)]]
           (is (= (schedules/occurrence (atom before) oid) (schedules/occurrence conn oid)))))))))
