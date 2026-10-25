(ns is.simm.ops.knowledge-batch-filing-test
  "Exercise the real filing writer and recovery composition using a synthetic
   trusted prepare seam. No assertion here certifies an executed Attempt."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.repo :as repo]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.knowledge-processing-test :as fixture]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.system-db :as sdb]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.file-output-plan :as plans]
            [is.simm.ops.knowledge-outputs :as outputs]
            [is.simm.ops.knowledge-filing :as filing]
            [is.simm.ops.knowledge-publish :as publish]
            [is.simm.ops.proposal-publication :as publication]))

(defn- fixture [f]
  (@#'fixture/fixture :no-op
   (fn [{:keys [conn w expected ready value binding] :as data}]
     (let [target (get-in ready [:publication-basis :target])
           prepared {:request-binding binding :disposition value :batch expected :plan nil
                     :party (:owner w) :room (:room w) :target target}]
       (with-redefs [sdb/get-conn (constantly conn)
                     filing/current-target (constantly target)
                     filing/prepare (constantly prepared)
                     publication/publish! (fn [& _] (throw (AssertionError. "No-op may not announce a Proposal")))]
         (f (assoc data :principal (ops/party-principal (:owner w)) :prepared prepared)))))))

(defn- file! [{:keys [conn principal w room value]}]
  (publish/file-completed-request! conn principal (:room w) room (:request value)))

(deftest filing-atomically-closes-no-op-batch-and-replays-without-recertification
  (fixture
   (fn [{:keys [conn value ready] :as data}]
     (is (= value (file! data)))
     (is (= :closed (:knowledge.batch/state (batches/batch @conn (:batch ready)))))
     (is (nil? (:knowledge.stream/active-batch (streams/stream @conn (:stream ready)))))
     (with-redefs [filing/prepare (fn [& _] (throw (AssertionError. "Historical filing may not recertify Attempt")))]
       (is (= value (file! data)))))))

(deftest lost-joint-filing-ack-verifies-complete-batch-and-item-transition
  (fixture
   (fn [{:keys [conn value] :as data}]
     (let [transact d/transact thrown? (atom false)]
       (with-redefs [d/transact (fn [c tx]
                                 (let [result (transact c tx)]
                                   (when (and (identical? c conn)
                                              (= (:id value) (:knowledge.disposition/id (requests/get-request conn (:request value))))
                                              (compare-and-set! thrown? false true))
                                     (throw (ex-info "Lost joint filing ACK" {})))
                                   result))]
         (is (= value (file! data))))
       (is @thrown?)
       (is (= value (dispositions/disposition (requests/get-request conn (:request value)))))))))

(deftest historical-disposition-alone-cannot-hide-partial-batch-filing
  (doseq [damage [:batch-link :item-state :active-pointer]]
    (fixture
     (fn [{:keys [conn ready] :as data}]
       (file! data)
       (let [batch (batches/batch @conn (:batch ready))
             item (first (filter #(= (:batch ready) (:knowledge.item/batch %)) (streams/items @conn (:stream ready))))]
         (d/transact conn
           [(case damage
              :batch-link [:db/retract (:db/id batch) :knowledge.batch/disposition (:knowledge.batch/disposition batch)]
              :item-state [:db/add (:db/id item) :knowledge.item/state :claimed]
              :active-pointer [:db/add [:knowledge.stream/id (:stream ready)] :knowledge.stream/active-batch (:batch ready)])])
         (is (thrown? Exception (file! data))))))))

(deftest missing-prepared-batch-proof-cannot-commit-a-disposition-alone
  (fixture
   (fn [{:keys [conn value prepared] :as data}]
     (with-redefs [filing/prepare (constantly (dissoc prepared :batch))]
       (is (thrown? Exception (file! data))))
     (is (nil? (dispositions/disposition (requests/get-request conn (:request value))))))))

(deftest proposed-batch-uses-the-real-proposal-and-retained-body-writer
  (@#'fixture/fixture :proposed
   (fn [{:keys [conn w room ready owned expected value binding]}]
     (repo/write! owned "out/wiki/from-batch.md" (.getBytes "Retained proposed knowledge" "UTF-8"))
     (let [retained (:input (requests/payload conn (requests/get-request conn (:request value))))
           output (outputs/capture-db @owned)
           plan (plans/plan {:request (:request value) :room (:room w) :attempt (:candidate value)
                             :environment (:environment retained) :workflow-bundle (parse-uuid (get-in retained [:workflow :bundle]))
                             :publication-basis (:publication-basis ready) :output output :retained-inputs (:inputs retained)})
           value (dispositions/make (-> value (dissoc :id :version)
                                        (assoc :manifest (get-in plan [:manifest :id]) :proposal (:proposal plan)
                                               :output (:id output))))
           prepared {:request-binding binding :disposition value :plan plan :batch expected
                     :party (:owner w) :room (:room w) :target (get-in ready [:publication-basis :target])}
           data {:conn conn :w w :room room :value value :principal (ops/party-principal (:owner w))}
           publications (atom [])]
       (with-redefs [sdb/get-conn (constantly conn)
                     filing/current-target (constantly (:target prepared))
                     filing/prepare (constantly prepared)
                     publication/publish! #(swap! publications conj %)]
         (is (= value (file! data)))
         (is (= [(:proposal plan)] @publications))
         (is (= :review (:knowledge.batch/state (batches/batch @conn (:batch ready)))))
         (is (= (:batch ready) (:knowledge.stream/active-batch (streams/stream @conn (:stream ready)))))
         (is (= 1 (count (filter #(= :claimed (:knowledge.item/state %)) (streams/items @conn (:stream ready))))))
         (is (not-any? #(= :processed (:knowledge.item/state %)) (streams/items @conn (:stream ready))))
         (with-redefs [filing/prepare (fn [& _] (throw (AssertionError. "Historical proposal may not recertify")))]
           (is (= value (file! data)))))))))
