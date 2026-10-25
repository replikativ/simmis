(ns is.simm.runtimes.capture-archive-claim-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.runtimes.screen-recording :as recording]))

(def owner (random-uuid))
(def sid (random-uuid))
(defn- with-source! [f]
  (let [config {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write :keep-history? false}]
    (d/create-database config)
    (let [conn (d/connect config) completed? (atom false)]
      (try
        (d/transact conn recording/recording-schema)
        (d/transact conn [{:recording.session/id sid :recording.session/started-at 1000
                           :recording.session/ended-at 5000 :recording.session/mime "video/webm"}
                          {:recording.segment/session [:recording.session/id sid]
                           :recording.segment/idx 0 :recording.segment/status :ready
                           :recording.segment/offset-ms 10 :recording.segment/duration-ms 1000
                           :recording.segment/bytes 9 :recording.segment/blob-id (apply str (repeat 64 "a"))}])
        (let [result (with-redefs-fn {(var recording/conn-for) (constantly conn)}
                       #(f conn (recording/archive-selection! owner sid 0)))]
          (reset! completed? true)
          result)
        ;; An unexpected callback/join failure retains diagnostic storage;
        ;; never release it under an unjoined worker.
        (finally (when @completed? (d/release conn) (d/delete-database config)))))))

(defn- claim! [source]
  (recording/claim-archive! source (random-uuid) (random-uuid) (random-uuid) "captures/session/observations/note.md"))

(deftest claim-is-one-shot-and-finish-requires-the-original-live-pending-claim
  (with-source!
    (fn [conn source]
      (is (= 10 (get-in source [:segment :offset-ms])))
      (let [claim (claim! source) token (:token claim) id (:segment-eid source) content (random-uuid)]
        (is (= claim (recording/validate-archive-claim! claim)))
        (is (thrown? Exception (claim! source)))
        (is (= token (:recording.segment/archive-claim (d/entity @conn id))))
        (is (thrown? Exception (recording/validate-archive-claim! (assoc claim :target (random-uuid)))))
        (is (thrown? Exception (recording/validate-archive-claim! (assoc claim :token nil))))
        (is (= {:state :complete :content content} (recording/finish-archive! claim content)))
        (is (= :complete (:recording.segment/archive-state (d/entity @conn id))))
        (is (thrown? Exception (recording/finish-archive! claim content)))
        (is (thrown? Exception (claim! source)))))))

(deftest a-changed-source-or-non-ready-selection-refuses
  (with-source!
    (fn [conn source]
      (let [claim (claim! source)]
        (d/transact conn [{:db/id (:segment-eid source) :recording.segment/bytes 10}])
        (is (thrown? Exception (recording/validate-archive-claim! claim)))
        (is (thrown? Exception (recording/claim-archive! source (random-uuid) (random-uuid) (random-uuid) "note.md")))
        (d/transact conn [{:db/id (:segment-eid source) :recording.segment/status :finalizing}])
        (is (thrown? Exception (recording/archive-selection! owner sid 0)))))))

(deftest lost-claim-ack-never-produces-a-token-for-publication
  (with-source!
    (fn [conn source]
      (let [transact d/transact]
        (with-redefs [d/transact (fn [& args] (apply transact args) (throw (java.io.IOException. "lost claim ack")))]
          (is (thrown? java.io.IOException (claim! source))))
        (is (uuid? (:recording.segment/archive-claim (d/entity @conn (:segment-eid source)))))
        (is (thrown? Exception (claim! source)))))))

(deftest lost-finish-ack-is-not-recovered-from-an-observed-complete-row
  (with-source!
    (fn [conn source]
      (let [claim (claim! source) content (random-uuid) transact d/transact]
        (with-redefs [d/transact (fn [& args] (apply transact args) (throw (java.io.IOException. "lost finish ack")))]
          (is (thrown? java.io.IOException (recording/finish-archive! claim content))))
        (is (= :complete (:recording.segment/archive-state (d/entity @conn (:segment-eid source)))))
        (let [writes (atom 0)]
          (with-redefs [d/transact (fn [& _] (swap! writes inc))]
            (is (thrown? Exception (recording/finish-archive! claim content))))
          (is (zero? @writes)))))))

(deftest source-change-at-writer-entry-cannot-be-claimed-or-completed
  (doseq [phase [:claim :finish]]
    (with-source!
      (fn [conn source]
        (let [claim (when (= phase :finish) (claim! source))
              transact d/transact]
          (with-redefs [d/transact (fn [connection tx]
                                     (transact connection [{:db/id (:segment-eid source) :recording.segment/bytes 10}])
                                     (transact connection tx))]
            (is (thrown? Exception
                         (if claim (recording/finish-archive! claim (random-uuid)) (claim! source)))))
          (is (not= :complete (:recording.segment/archive-state (d/entity @conn (:segment-eid source))))))))))

(deftest only-one-concurrent-claim-can-return-an-original-token
  (with-source!
    (fn [_ source]
      (let [start (promise)
            claim (fn [] @start (try (claim! source) (catch Exception _ ::refused)))
            first-result (future (claim)) second-result (future (claim))]
        (try
          (deliver start true)
          (let [results [(deref first-result 10000 ::timeout) (deref second-result 10000 ::timeout)]]
            (is (= 1 (count (filter map? results))))
            (is (= 1 (count (filter #{::refused} results)))))
          (finally
            (deliver start true)
            ;; No cancellation is used: a returned value acknowledges actual
            ;; worker exit. A failed finite join leaves fixture storage held.
            (doseq [worker [first-result second-result]]
              (when (= ::timeout (deref worker 10000 ::timeout))
                (throw (ex-info "Claim worker did not exit; retain fixture" {}))))))))))

(deftest malformed-identities-never-open-recording-storage
  (with-redefs-fn {(var recording/conn-for) (fn [& _] (throw (AssertionError. "storage touched")))}
    #(doseq [args [[nil sid 0] [owner nil 0] [owner sid -1] [owner sid 1000000]]]
       (is (thrown? clojure.lang.ExceptionInfo (apply recording/archive-selection! args))))))
