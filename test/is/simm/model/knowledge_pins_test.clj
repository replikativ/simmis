(ns is.simm.model.knowledge-pins-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-pins :as pins]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.knowledge-streams-test :as fixtures]))

(defn- fixture [f]
  (#'fixtures/fixture
   (fn [conn value]
     (d/transact conn pins/schema)
     (f conn value))))

(defn- confirmed-pin! [conn value expected basis]
  (let [{:keys [intent new?]} (pins/prepare! conn value expected basis)]
    (is new?)
    ;; Model-only fixture. The production HOST must verify a real native root
    ;; and its full immutable record before composing this transition.
    (d/transact conn [[:db.fn/call pins/mark-pinned-tx intent]])
    (pins/intent @conn (:knowledge.pin/id intent))))

(defn- publish! [conn prepared value expected plan]
  (artifact/publish-value! (artifact/datahike-store conn) plan
                           (fn [ref] (d/transact conn [[:db.fn/call pins/discovery-tx prepared value expected plan ref]])))
  (pins/intent @conn (:knowledge.pin/id prepared)))

(deftest concurrent-intent-preparation-permits-one-native-pin-winner
  (fixture
   (fn [conn value]
     (let [gate (promise)
           invoke (fn [] @gate (pins/prepare! conn value nil fixtures/basis))
           a (future (invoke)) b (future (invoke))]
       (deliver gate true)
       (let [results [@a @b]]
         (is (= 1 (count (filter :new? results))))
         (is (= 1 (count (set (map #(get-in % [:intent :knowledge.pin/id]) results)))))
         (is (= :creating (:knowledge.pin/state (pins/active @conn (:id value))))))))))

(deftest lost-preparation-ack-never-creates-another-native-pin-winner
  (fixture
   (fn [conn value]
     (let [transact d/transact]
       (with-redefs [d/transact (fn [& args] (apply transact args) (throw (ex-info "Lost ACK" {})))]
         (is (thrown? Exception (pins/prepare! conn value nil fixtures/basis))))
       (let [prior (pins/active @conn (:id value)) retry (pins/prepare! conn value nil fixtures/basis)]
         (is (= prior (:intent retry)))
         (is (false? (:new? retry)))
         (is (= {:version 1 :stream value :expected nil :basis fixtures/basis} (pins/payload conn prior))))))))

(deftest pin-discovery-and-cleanup-receipts-bind-one-checkpoint-through-two-transitions
  (fixture
   (fn [conn value]
     (let [first-pin (confirmed-pin! conn value nil fixtures/basis)
           first-committed (publish! conn first-pin value nil fixtures/initial)]
       (is (= :committed (:knowledge.pin/state first-committed)))
       (is (= (:knowledge.pin/id first-pin) (:knowledge.stream/cursor-root (streams/stream @conn (:id value)))))
       (d/transact conn [[:db.fn/call pins/finish-tx first-committed]])
       (is (nil? (pins/active @conn (:id value))))
       (is (= [] (pins/finish-tx @conn first-committed)))
       (let [before (streams/stream @conn (:id value))
             next-plan (#'fixtures/later fixtures/basis [{:path "inbox/a.md" :kind :modified
                                                         :before fixtures/file-a :after fixtures/file-b}])
             next-pin (confirmed-pin! conn value before (:after next-plan))]
         (is (thrown? Exception (publish! conn next-pin value before (assoc-in next-plan [:after :commit] (random-uuid)))))
         (is (= before (streams/stream @conn (:id value))))
         (let [committed (publish! conn next-pin value before next-plan)
               active (pins/active @conn (:id value))]
           (is (= (:knowledge.pin/id next-pin) (:knowledge.pin/id active)))
           (is (thrown? Exception (pins/prepare! conn value (streams/stream @conn (:id value)) (:after next-plan))))
           ;; Native old-root release occurs BEFORE this model-only fixture's
           ;; cleanup ACK. No test here claims native GC behavior.
           (d/transact conn [[:db.fn/call pins/finish-tx committed]])
           (is (= :released (:knowledge.pin/state (pins/intent @conn (:knowledge.pin/id first-pin)))))
           (is (= :retained (:knowledge.pin/state (pins/intent @conn (:knowledge.pin/id next-pin)))))
           (is (= [] (pins/finish-tx @conn committed)))
           (is (= [] (pins/finish-tx @conn first-committed)))
           (is (nil? (pins/active @conn (:id value))))))))))

(deftest retained-intent-validates-contents-even-when-the-artifact-hash-is-recomputed
  (doseq [damage [(fn [body] (update body :basis dissoc :configuration))
                  (fn [body] (assoc-in body [:stream :id] (random-uuid)))
                  (fn [body] (assoc body :expected false))
                  (fn [body] (assoc body :expected {:knowledge.stream/id (random-uuid)}))]]
    (fixture
     (fn [conn value]
       (let [original (:intent (pins/prepare! conn value nil fixtures/basis))
             bad (damage (pins/payload conn original))]
         (artifact/publish-value! (artifact/datahike-store conn) bad
           (fn [ref] (d/transact conn [{:db/id (:db/id original) :knowledge.pin/payload ref
                                       :knowledge.pin/payload-id (hasch/uuid bad)}])))
         (is (thrown? Exception (pins/payload conn (pins/intent @conn (:knowledge.pin/id original))))))))))

(deftest oversized-intent-is-refused-before-publication
  (fixture
   (fn [conn value]
     (let [oversized (assoc fixtures/basis :branch (keyword (apply str (repeat 20000 "x"))))
           calls (atom 0)]
       (with-redefs [artifact/publish-value! (fn [& _] (swap! calls inc) (throw (AssertionError. "No oversize publication")))]
         (is (thrown? Exception (pins/prepare! conn value nil oversized))))
       (is (zero? @calls))
       (is (nil? (pins/active @conn (:id value))))))))

(deftest stale-claims-and-premature-cleanup-cannot-advance-the-lifecycle
  (fixture
   (fn [conn value]
     (let [creating (:intent (pins/prepare! conn value nil fixtures/basis))
           foreign (assoc creating :knowledge.pin/claim (random-uuid))]
       (is (thrown? Exception (d/transact conn [[:db.fn/call pins/mark-pinned-tx foreign]])))
       (is (thrown? Exception (d/transact conn [[:db.fn/call pins/finish-tx creating]])))
       (is (= creating (pins/intent @conn (:knowledge.pin/id creating))))
       (d/transact conn [[:db.fn/call pins/mark-pinned-tx creating]])
       (let [pinned (pins/intent @conn (:knowledge.pin/id creating))]
         (is (thrown? Exception (d/transact conn [[:db.fn/call pins/finish-tx pinned]])))
         (is (thrown? Exception (publish! conn (assoc pinned :knowledge.pin/claim (random-uuid)) value nil fixtures/initial)))
         (is (nil? (streams/stream @conn (:id value))))
         (let [committed (publish! conn pinned value nil fixtures/initial)]
           (is (thrown? Exception (d/transact conn [[:db.fn/call pins/finish-tx (assoc committed :knowledge.pin/claim (random-uuid))]])))
           (is (= :committed (:knowledge.pin/state (pins/intent @conn (:knowledge.pin/id creating)))))))))))

(deftest orphaned-claimed-items-hold-before-allocating-a-pin-intent
  (fixture
   (fn [conn value]
     (streams/discover! conn value nil fixtures/initial (random-uuid))
     (d/transact conn [{:db/id [:knowledge.item/id (streams/item-id (:id value) "inbox/a.md" fixtures/file-a)]
                        :knowledge.item/state :claimed}])
     (is (thrown? Exception (pins/prepare! conn value (streams/stream @conn (:id value)) fixtures/basis)))
     (is (nil? (pins/active @conn (:id value)))))))
