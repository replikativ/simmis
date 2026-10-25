(ns is.simm.model.knowledge-batches-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-pins :as pins]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.knowledge-batch-inputs :as inputs]
            [is.simm.ops.knowledge-inputs :as evidence]
            [is.simm.ops.owned-knowledge-discovery-test :as host-fixture]))

(defn- fixture [f]
  (@#'host-fixture/with-fixture
   (fn [{:keys [conn owned] :as data}]
     (d/transact conn batches/schema)
     (@#'host-fixture/write! owned "inbox/second.md" "Another source")
     (let [input (@#'host-fixture/request data)
           discovered (@#'host-fixture/refresh data input)
           intent (pins/intent @conn (:operation discovered))
           value (:stream (pins/payload conn intent))
           expected (streams/stream @conn (:id value))
           pending (streams/pending-items @conn (:id value))
           selected (inputs/selection @owned value expected pending
                                     {:version 1 :trigger-roots ["inbox"] :context-roots ["wiki"] :exclude-prefixes []}
                                     (assoc inputs/default-policy :max-trigger-files 1) {:files 0 :bytes 0})]
       (f (assoc data :value value :expected expected :selected selected))))))

(defn- claim! [{:keys [conn selected]} & [token]]
  (let [token (or token (random-uuid)) selection (:batch selected)]
    (artifact/publish-value! (artifact/datahike-store conn) selection
      (fn [ref] (d/transact conn [[:db.fn/call batches/claim-tx selection ref token]])))
    (batches/claim-result @conn (:id selection) token)))

(defn- freeze! [{:keys [conn owned selected]} expected]
  (let [frozen (inputs/capture @owned selected) digest (hasch/uuid frozen)]
    (artifact/publish-value! (artifact/datahike-store conn) frozen
      (fn [ref] (d/transact conn [[:db.fn/call batches/freeze-tx expected ref digest]])))
    (batches/batch @conn (:knowledge.batch/id expected))))

(deftest claim-removes-only-the-exact-prefix-from-the-pending-index
  (fixture
    (fn [{:keys [conn value selected expected] :as data}]
      (let [first (claim! data) row (:batch first) id (get-in selected [:batch :id])]
        (is (:new? first))
        (is (= :capturing (:knowledge.batch/state row)))
        (is (= id (:knowledge.stream/active-batch (streams/stream @conn (:id value)))))
        (is (= ["inbox/second.md"] (mapv :knowledge.item/path (streams/pending-items @conn (:id value)))))
        (is (= [:claimed :pending] (mapv :knowledge.item/state (streams/items @conn (:id value)))))
        (is (false? (:new? (claim! data))))
        (is (= (:knowledge.stream/cursor-root expected)
               (:knowledge.stream/cursor-root (streams/stream @conn (:id value)))))
        (is (= :retained (:knowledge.pin/state (pins/intent @conn (:knowledge.stream/cursor-root expected)))))))))

(deftest replay-refuses-mutated-immutable-claim-bindings
  (fixture
    (fn [{:keys [conn selected] :as data}]
      (let [claimed (:batch (claim! data)) ready (freeze! data claimed)
            ref (:knowledge.batch/frozen ready) digest (:knowledge.batch/frozen-id ready)]
        (is (false? (:new? (batches/claim-result @conn (:knowledge.batch/id ready) (:knowledge.batch/claim ready)))))
        (doseq [field [:knowledge.batch/by :knowledge.batch/room :knowledge.batch/target]]
          (let [altered (:db-after (d/with @conn [[:db/add (:db/id ready) field (random-uuid)]]))]
            (is (thrown? Exception (batches/claim-tx altered (:batch selected) (:knowledge.batch/selection ready) (random-uuid))))
            (is (thrown? Exception (batches/freeze-tx altered claimed ref digest)))))
        (doseq [field [:knowledge.batch/stream :knowledge.batch/selection]]
          (is (thrown? Exception (batches/freeze-tx @conn (assoc claimed field (random-uuid)) ref digest))))))))

(deftest concurrent-manual-and-scheduled-claimants-have-one-capture-winner
  (fixture
    (fn [data]
      (let [gate (promise) tokens [(random-uuid) (random-uuid)]
            contenders (mapv (fn [token] (future @gate (claim! data token))) tokens)]
        (deliver gate true)
        (let [results (mapv #(deref % 15000 ::expired) contenders)]
          (is (= 1 (count (filter :new? results))))
          (is (apply = (map #(get-in % [:batch :knowledge.batch/id]) results))))))))

(deftest frozen-inputs-are-bound-to-the-original-claim-before-request-linking
  (fixture
    (fn [{:keys [conn] :as data}]
      (let [claimed (:batch (claim! data)) ready (freeze! data claimed)
            ref (:knowledge.batch/frozen ready) digest (:knowledge.batch/frozen-id ready)]
        (is (= :ready (:knowledge.batch/state ready)))
        (is (= [] (batches/freeze-tx @conn claimed ref digest)))
        (is (thrown? Exception (batches/freeze-tx @conn (assoc claimed :knowledge.batch/claim (random-uuid)) ref digest)))
        (is (thrown? Exception (batches/freeze-tx @conn ready (random-uuid) digest)))
        (let [stored (:value (artifact/get-bounded-value (artifact/datahike-store conn) ref (* 16 1024 1024)))]
          (is (= stored (inputs/verify stored)))
          (is (= digest (hasch/uuid stored))))))))

(deftest request-link-is-exact-and-refuses-benchmark-or-different-requesters
  (fixture
    (fn [{:keys [conn w] :as data}]
      (let [ready (freeze! data (:batch (claim! data)))
            origin {:batch (:knowledge.batch/id ready) :selection (:knowledge.batch/selection-id ready)
                    :frozen (:knowledge.batch/frozen-id ready)}
            digest (random-uuid) client (random-uuid)
            request {:work.request/id (requests/request-id (:owner w) (:room w) client)
                     :work.request/client-id client :work.request/intent-id digest :work.request/kind :workflow
                     :work.request/by (:owner w) :work.request/room (:room w)}]
        (doseq [request [(assoc request :work.request/kind :benchmark)
                         (assoc request :work.request/by (:member w))
                         (assoc request :work.request/id (random-uuid))
                         (assoc request :work.request/intent-id (random-uuid))]]
          (is (thrown? Exception (batches/request-tx @conn origin request digest))))
        (d/transact conn [[:db.fn/call (fn [db] (into (@#'requests/claim-tx db request digest)
                                                   (batches/request-tx db origin request digest)))]])
        (is (= :submitted (:knowledge.batch/state (batches/batch @conn (:batch origin)))))
        (is (= [] (batches/request-tx @conn origin request digest)))
        (let [without-request (:db-after (d/with @conn [[:db.fn/retractEntity [:work.request/id (:work.request/id request)]]]))]
          (is (thrown? Exception (batches/request-tx without-request origin request digest))))
        (is (thrown? Exception (batches/request-tx @conn origin (assoc request :work.request/id (random-uuid)) digest)))
        (is (thrown? Exception (batches/request-tx @conn origin request (random-uuid))))
        ;; Historical linkage remains a valid replay after a future certified
        ;; filing clears the active hold; it never authorizes a second request.
        (d/transact conn [[:db/retract [:knowledge.stream/id (:knowledge.batch/stream ready)]
                          :knowledge.stream/active-batch (:batch origin)]])
        (is (= [] (batches/request-tx @conn origin request digest)))))))

(deftest a-rehashed-plan-cannot-skip-the-canonical-pending-prefix
  (fixture
    (fn [{:keys [conn selected value] :as data}]
      (let [pending (streams/pending-items @conn (:id value))
            item (second pending)
            replacement {:id (:knowledge.item/id item) :order (:knowledge.item/order item)
                         :path (:knowledge.item/path item) :content (:knowledge.item/content item)
                         :size (:knowledge.item/size item) :mode (:knowledge.item/mode item)}
            changed (assoc-in (:batch selected) [:items 0] replacement)
            changed (assoc changed :id (hasch/uuid [:simmis/knowledge-batch-selection-v2 (dissoc changed :id)]))]
        (is (= :pending-prefix-changed
               (try (claim! (assoc data :selected {:status :selected :batch changed})) nil
                    (catch clojure.lang.ExceptionInfo error (:reason (ex-data error))))))
        (is (= 2 (count (streams/pending-items @conn (:id value)))))
        (is (nil? (:knowledge.stream/active-batch (streams/stream @conn (:id value)))))))))
