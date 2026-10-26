(ns is.simm.model.knowledge-stream-retirement-test
  "Pure retirement fragments. Projected finish states are NOT proof of native
   root release; actual source ownership and GC belong to the HOST tests."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-pins :as pins]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.knowledge-stream-retirement :as retirement]
            [is.simm.ops.owned-knowledge-discovery :as discovery]
            [is.simm.ops.knowledge-checkpoint-roots :as checkpoints]
            [is.simm.ops.owned-knowledge-requeue-test :as fixture]))

(defn- with-clean [f]
  (@#'fixture/with-ready
   (fn [{:keys [conn batch host principal handle request batch-input] :as data}]
     (d/transact conn retirement/schema)
     (discovery/abandon-ready! host principal handle request)
     (let [next (discovery/prepare-batch! host principal handle batch-input)
           next-row (batches/batch @conn (:batch next))]
       (discovery/abandon-ready! host principal handle
         {:batch (:batch next) :expected-claim (:knowledge.batch/claim next-row)
          :expected-frozen (:knowledge.batch/frozen-id next-row) :request-id (random-uuid) :reason "Finish clean retirement fixture"}))
     (let [stream (streams/stream @conn (:knowledge.batch/stream batch))
           pin (pins/intent @conn (:knowledge.stream/cursor-root stream))
           body (pins/payload conn pin)
           expected (retirement/expected stream pin body)
           value (retirement/decision {:stream (:knowledge.stream/id stream) :root (:knowledge.pin/id pin)
                                       :by (:knowledge.pin/by pin) :client (random-uuid) :at (java.util.Date. 0)
                                       :reason "Retire this completed source stream"})]
       (is (empty? (streams/pending-items @conn (:knowledge.stream/id stream))))
       (f (assoc data :stream stream :pin pin :payload body :expected expected :decision value))))))

(defn- begin! [{:keys [conn expected decision]}]
  (d/transact conn [[:db.fn/call retirement/begin-tx expected decision]]))

(deftest fence-and-projected-finish-preserve-cursor-and-original-audit
  (with-clean
    (fn [{:keys [conn stream pin expected decision] :as data}]
      (is (thrown? Exception (retirement/finish-tx @conn expected decision)))
      (begin! data)
      (let [s (streams/stream @conn (:knowledge.stream/id stream)) p (pins/intent @conn (:knowledge.pin/id pin))]
        (is (= :retiring (:knowledge.stream/state s)))
        (is (= (dissoc stream :knowledge.stream/state) (dissoc s :knowledge.stream/state)))
        (is (= :retained (:knowledge.pin/state p)))
        (is (= (:at decision) (:knowledge.pin/retirement-at p)))
        (is (= [] (retirement/begin-tx @conn expected (assoc decision :at (java.util.Date. 999)))))
        (let [projected (:db-after (d/with @conn (retirement/finish-tx @conn expected decision)))]
          (is (= :retired (:knowledge.stream/state (streams/stream projected (:knowledge.stream/id stream)))))
          (is (= :released (:knowledge.pin/state (pins/intent projected (:knowledge.pin/id pin)))))
          (is (= [] (retirement/begin-tx projected expected decision)))
          (is (= [] (retirement/finish-tx projected expected decision))))))))

(deftest partial-cleanliness-records-refuse-before-fencing
  (with-clean
    (fn [{:keys [conn stream pin expected decision]}]
      (let [sid (:knowledge.stream/id stream)
            item (:db/id (first (streams/items @conn sid)))
            event [:knowledge.discovery/id (:knowledge.pin/discovery pin)]]
        (doseq [tx [[[:db/add [:knowledge.stream/id sid] :knowledge.stream/active-batch (random-uuid)]]
                    [[:db/add item :knowledge.item/state :pending]]
                    [[:db/add item :knowledge.item/state :claimed]]
                    [[:db/add item :knowledge.item/pending-stream sid]]
                    [[:db/add (:db/id pin) :knowledge.pin/active-stream (random-uuid)]]
                    [[:db/add event :knowledge.discovery/stream (random-uuid)]]
                    [[:db/add event :knowledge.discovery/old-root-released? false]]]]
          (let [projected (:db-after (d/with @conn tx))]
            (is (thrown? Exception (retirement/begin-tx projected expected decision)))))
        (is (= :active (:knowledge.stream/state (streams/stream @conn sid))))))))

(deftest restarted-expectations-cannot-hide-binding-drift-or-missing-audit
  (with-clean
    (fn [{:keys [conn stream pin payload expected decision] :as data}]
      (begin! data)
      (let [sid (:knowledge.stream/id stream) pid (:knowledge.pin/id pin)]
        (doseq [tx [[[:db/add [:knowledge.stream/id sid] :knowledge.stream/batch-generation 99]]
                    [[:db/add (:db/id pin) :knowledge.pin/claim (random-uuid)]]
                    [[:db/retract (:db/id pin) :knowledge.pin/retirement-id (:id decision)]]
                    [[:db/retract (:db/id pin) :knowledge.pin/retirement-at (:at decision)]]
                    [[:db/add (:db/id pin) :knowledge.pin/retirement-reason "Different decision"]]
                    [[:db/add [:knowledge.stream/id sid] :knowledge.stream/state :retired]]
                    [[:db/add (:db/id pin) :knowledge.pin/state :released]]]]
          (let [projected (:db-after (d/with @conn tx))
                rebuilt (retirement/expected (streams/stream projected sid) (pins/intent projected pid) payload)]
            (is (thrown? Exception (retirement/begin-tx projected rebuilt decision)))
            (is (thrown? Exception (retirement/finish-tx projected rebuilt decision)))))
        (is (= [] (retirement/begin-tx @conn expected decision)))))))

(deftest lost-begin-ack-retains-the-fence-and-replays-without-a-new-timestamp
  (with-clean
    (fn [{:keys [conn expected decision pin] :as data}]
      (let [transact! d/transact lost? (atom false)]
        (with-redefs [d/transact
                      (fn [c tx]
                        (let [result (transact! c tx)]
                          (when (and (identical? c conn)
                                     (:knowledge.pin/retirement-id (pins/intent @conn (:knowledge.pin/id pin)))
                                     (compare-and-set! lost? false true))
                            (throw (ex-info "Lost begin ACK" {})))
                          result))]
          (is (thrown? Exception (begin! data))))
        (is @lost?)
        (is (= [] (retirement/begin-tx @conn expected (assoc decision :at (java.util.Date. 1000)))))
        (is (= (:at decision) (:knowledge.pin/retirement-at (pins/intent @conn (:knowledge.pin/id pin)))))))))

(deftest fenced-stream-refuses-fresh-journal-batch-and-native-root-entry
  (with-clean
    (fn [{:keys [conn stream pin payload proof owned] :as data}]
      (begin! data)
      (doseq [state [:retiring :retired]]
        (let [db (:db-after (d/with @conn [[:db/add [:knowledge.stream/id (:knowledge.stream/id stream)]
                                          :knowledge.stream/state state]]))
              current (streams/stream db (:knowledge.stream/id stream))
              value (:stream payload) basis (:basis payload)
              plan {:before basis :after basis :changes []}
              selected (assoc (:selection proof) :expected current)
              selected (assoc selected :id (hasch/uuid [:simmis/knowledge-batch-selection-v2 (dissoc selected :id)]))
              calls [#(pins/prepare-tx db value current (random-uuid) (random-uuid) (random-uuid) (random-uuid))
                     #(streams/discovery-tx db value current plan (random-uuid) (random-uuid))
                     #(batches/claim-tx db selected (random-uuid) (random-uuid))
                     #(checkpoints/create! (atom db) @owned {:intent pin :new? true})]]
          (doseq [call calls]
            (is (= :stream-closed (try (call) nil (catch clojure.lang.ExceptionInfo error (:reason (ex-data error))))))))))))
