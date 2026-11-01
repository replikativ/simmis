(ns is.simm.ops.owned-knowledge-submitted-recovery-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-submitted-recovery-test :as fixture]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.ops.dvergr :as principals]
            [is.simm.ops.knowledge-filing :as filing]
            [is.simm.ops.knowledge-failed-settlement :as native]
            [is.simm.ops.owned-knowledge-submitted-recovery :as recovery]))

(defn- input [{:keys [ready proof request]}]
  {:request request :batch (:batch ready) :expected-claim (get-in proof [:row :knowledge.batch/claim])
   :expected-frozen (get-in proof [:row :knowledge.batch/frozen-id])
   :request-id (random-uuid) :reason "Recover this failed test update"})

(deftest denied-actors-cannot-reach-artifacts-or-native-recovery
  (@#'fixture/with-submitted
   (fn [{:keys [conn host w] :as data}]
     (let [calls (atom 0) before (d/commit-id @conn)]
       (with-redefs [filing/recovery-context! (fn [& _] (swap! calls inc) (throw (AssertionError. "Unauthorized artifact read")))
                     native/verify! (fn [& _] (swap! calls inc) (throw (AssertionError. "Unauthorized native read")))]
         (doseq [party [(:member w) (:non-member w)]]
           (is (thrown? Exception
                        (recovery/recover! host (principals/party-principal party) (str (:room w)) (input data))))))
       (is (zero? @calls))
       (is (= before (d/commit-id @conn)))))))

(deftest authority-revoked-after-native-observation-cannot-requeue
  (@#'fixture/with-submitted
   (fn [{:keys [conn host w proof expected] :as data}]
     ;; Isolate native/provenance verification here: the real scripted daily
     ;; fixture covers those paths. This checks the later authority frontier.
     (with-redefs [filing/recovery-context!
                   (fn [& _] {:row (:request expected) :retained-proof proof})
                   native/verify!
                   (fn [& _]
                     (d/transact conn [{:room/id (:room w) :room/owner [:party/id (:member w)]}])
                     (:native expected))]
       (is (thrown? Exception
                    (recovery/recover! host (principals/party-principal (:owner w)) (str (:room w)) (input data)))))
     (let [bid (get-in data [:ready :batch]) batch (batches/batch @conn bid)]
       (is (= :submitted (:knowledge.batch/state batch)))
       (is (nil? (:knowledge.batch/recovery-id batch)))
       (is (= bid (:knowledge.stream/active-batch (streams/stream @conn (get-in data [:ready :stream])))))))))
