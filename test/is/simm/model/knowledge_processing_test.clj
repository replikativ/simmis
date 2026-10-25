(ns is.simm.model.knowledge-processing-test
  "Real canonical metadata transactions with synthetic successful disposition
   data. This verifies pure composition, not an executed or certified Attempt."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.knowledge-processing :as processing]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.knowledge-batch-proof :as proof]
            [is.simm.ops.knowledge-batch-proof-test :as fixture]))

(defn- fixture [kind f]
  (@#'fixture/fixture
   (fn [{:keys [conn ready] :as data}]
     (d/transact conn (into dispositions/schema processing/schema))
     (let [request (@#'fixture/joint-claim! data)
           request (requests/record-job! conn (:work.request/id request) (:work.request/claim request) (random-uuid))
           _ (requests/mark-returned! conn (:work.request/id request))
           row (requests/get-request conn (:work.request/id request))
           expected (processing/expected (proof/request! conn row))
           fields {:request (:work.request/id row) :intent (:work.request/intent-id row)
                   :job (:work.request/job row) :candidate (random-uuid) :attempt-content (random-uuid)
                   :basis (:work.request/publication-basis-id row) :output (random-uuid)
                   :batch (:batch ready) :kind kind}
           manifest (random-uuid)
           value (dispositions/make (cond-> fields
                                     (= :proposed kind) (assoc :manifest manifest
                                                              :proposal (hasch/uuid [:simmis/knowledge-file-proposal-v1 manifest]))))]
       (f (assoc data :row row :expected expected :value value :binding (dispositions/request-binding row)))))))

(defn- commit! [{:keys [conn expected value binding]}]
  (d/transact conn [[:db.fn/call (fn [db]
                                 (into (dispositions/tx-data db binding value)
                                       (processing/filing-tx db expected value)))]]))

(deftest certified-no-op-fragment-processes-only-the-exact-selected-set
  (fixture :no-op
   (fn [{:keys [conn ready expected value] :as data}]
     (commit! data)
     (let [batch (batches/batch @conn (:batch ready)) items (streams/items @conn (:stream ready))]
       (is (= :closed (:knowledge.batch/state batch)))
       (is (= :no-op (:knowledge.batch/outcome batch)))
       (is (= (:id value) (:knowledge.batch/disposition batch)))
       (is (nil? (:knowledge.stream/active-batch (streams/stream @conn (:stream ready)))))
       (is (= 1 (count (filter #(= :processed (:knowledge.item/state %)) items))))
       (is (= 1 (count (streams/pending-items @conn (:stream ready)))))
       (is (= [] (processing/filing-tx @conn expected value)))))))

(deftest proposed-fragment-holds-the-stream-and-does-not-process-sources
  (fixture :proposed
   (fn [{:keys [conn ready expected value] :as data}]
     (commit! data)
     (is (= :review (:knowledge.batch/state (batches/batch @conn (:batch ready)))))
     (is (= (:batch ready) (:knowledge.stream/active-batch (streams/stream @conn (:stream ready)))))
     (is (= 1 (count (filter #(= :claimed (:knowledge.item/state %)) (streams/items @conn (:stream ready))))))
     (is (not-any? #(= :processed (:knowledge.item/state %)) (streams/items @conn (:stream ready))))
     ;; This fragment-only fixture deliberately has no canonical Proposal.
     ;; A historical read must refuse that partial joint publication.
     (is (thrown? Exception (processing/filing-tx @conn expected value))))))

(deftest exact-replay-never-clears-a-later-batch
  (fixture :no-op
   (fn [{:keys [conn ready expected value] :as data}]
     (commit! data)
     (let [later (random-uuid)]
       (d/transact conn [[:db/add [:knowledge.stream/id (:stream ready)] :knowledge.stream/active-batch later]])
       (is (= [] (processing/filing-tx @conn expected value)))
       (is (= later (:knowledge.stream/active-batch (streams/stream @conn (:stream ready)))))))))

(deftest partial-filing-is-not-repaired-by-a-retry
  (doseq [damage [:disposition-only :batch-only :processed-item :missing-item]]
    (fixture :no-op
     (fn [{:keys [conn ready expected value binding]}]
       (let [batch (batches/batch @conn (:batch ready))
             item (first (filter #(= (:batch ready) (:knowledge.item/batch %)) (streams/items @conn (:stream ready))))]
         (d/transact conn
           (case damage
             :disposition-only (dispositions/tx-data @conn binding value)
             :batch-only [{:db/id (:db/id batch) :knowledge.batch/disposition (:id value)}]
             :processed-item [{:db/id (:db/id item) :knowledge.item/state :processed}]
             :missing-item [[:db/retract (:db/id item) :knowledge.item/batch (:batch ready)]]))
         (is (thrown? Exception (processing/filing-tx @conn expected value))))))))

(deftest complete-membership-and-source-identity-are-checked-in-the-writer
  (doseq [damage [:extra-member :changed-content :changed-frozen :changed-owner :selection-ref :frozen-ref :stray-adoption]]
    (fixture :no-op
     (fn [{:keys [conn ready expected value]}]
       (let [items (streams/items @conn (:stream ready))
             selected (first (filter #(= (:batch ready) (:knowledge.item/batch %)) items))
             pending (first (filter #(= :pending (:knowledge.item/state %)) items))
             batch (batches/batch @conn (:batch ready))]
         (d/transact conn
           (case damage
             :extra-member [{:db/id (:db/id pending) :knowledge.item/batch (:batch ready)}]
             :changed-content [{:db/id (:db/id selected) :knowledge.item/content (random-uuid)}]
             :changed-frozen [{:db/id (:db/id batch) :knowledge.batch/frozen-id (random-uuid)}]
             :changed-owner [{:db/id (:db/id batch) :knowledge.batch/by (random-uuid)}]
             :selection-ref [{:db/id (:db/id batch) :knowledge.batch/selection (random-uuid)}]
             :frozen-ref [{:db/id (:db/id batch) :knowledge.batch/frozen (random-uuid)}]
             :stray-adoption [{:db/id (:db/id batch) :knowledge.batch/adoption (random-uuid)}]))
         (is (thrown? Exception (processing/filing-tx @conn expected value))))))))
