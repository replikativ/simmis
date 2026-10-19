(ns is.simm.model.workflow-schedules-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.model.workflow-schedules :as schedules]))

(defn- with-db [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (d/transact conn schedules/schema) (f conn)
           (finally (d/release conn))))))

(defn- create [conn]
  (schedules/create! conn {:id (random-uuid) :room (random-uuid) :by (random-uuid)
                           :now (java.util.Date. 0) :next-fire (java.util.Date. 60000)
                           :target {:kind :workflow :cadence {:interval-ms 60000 :tz "UTC"}
                                    :snapshot {:files {"task.md" "pinned"}}}}))

(deftest source-selection-retention-and-processed-cursor-are-claim-fenced
  (with-db
    (fn [conn]
      (let [a (create conn) aid (:schedule.activation/id a) input-id (random-uuid)]
        (schedules/claim-due! conn a {:interval-ms 60000} (java.util.Date. 60000))
        (let [o (first (schedules/occurrences conn aid)) oid (:schedule.occurrence/id o)
              claim (schedules/admit! conn oid)
              value {:version 1 :input-id input-id :selection {:trigger-id (random-uuid)}
                     :captured {:inputs {"/in/evidence.md" "Original"}}}]
          (is (= :capture (schedules/select-input! conn oid claim input-id false)))
          (is (thrown? clojure.lang.ExceptionInfo
                       (schedules/retain-input! conn oid (random-uuid) value)))
          (is (nil? (:schedule.occurrence/input-ref (schedules/occurrence conn oid))))
          ;; Pause after admission doesn't withdraw the already-owned claim.
          (schedules/set-enabled! conn aid false)
          (schedules/retain-input! conn oid claim value)
          (schedules/retain-input! conn oid claim value)
          (is (= value (schedules/retained-input conn (schedules/occurrence conn oid))))
          (is (thrown? clojure.lang.ExceptionInfo
                       (schedules/retain-input! conn oid claim (assoc-in value [:captured :inputs "/in/evidence.md"] "Later"))))
          (schedules/mark-input-processed! conn oid)
          (is (nil? (:schedule.activation/last-processed-input (schedules/activation conn aid))))
          (schedules/finish-admission! conn oid :linked (random-uuid) nil)
          (schedules/mark-input-processed! conn oid)
          (is (= input-id (:schedule.activation/last-processed-input (schedules/activation conn aid))))
          (schedules/clear-active! conn a oid))
        (schedules/set-enabled! conn aid true)
        (schedules/claim-due! conn a {:interval-ms 60000} (java.util.Date. 120000))
        (let [o (first (schedules/occurrences conn aid)) oid (:schedule.occurrence/id o)
              claim (schedules/admit! conn oid)]
          (is (= :unchanged (schedules/select-input! conn oid claim input-id false)))
          (is (= :skipped (:schedule.occurrence/state (schedules/occurrence conn oid))))
          (is (nil? (:schedule.activation/active-occurrence (schedules/activation conn aid)))))
        ;; Deleting the final evidence file is a changed input, not an initial
        ;; empty inbox. It needs a reconciliation attempt rather than a skip.
        (schedules/claim-due! conn a {:interval-ms 60000} (java.util.Date. 180000))
        (let [o (first (schedules/occurrences conn aid)) oid (:schedule.occurrence/id o)
              claim (schedules/admit! conn oid)]
          (is (= :capture (schedules/select-input! conn oid claim (random-uuid) true))))))))

(deftest empty-evidence-skip-never-creates-a-capture-or-processed-watermark
  (with-db
    (fn [conn]
      (let [a (create conn) aid (:schedule.activation/id a)]
        (schedules/claim-due! conn a {:interval-ms 60000} (java.util.Date. 60000))
        (let [o (first (schedules/occurrences conn aid)) oid (:schedule.occurrence/id o)
              claim (schedules/admit! conn oid)]
          (is (= :no-inputs (schedules/select-input! conn oid claim (random-uuid) true)))
          (is (nil? (:schedule.occurrence/input-ref (schedules/occurrence conn oid))))
          (is (nil? (:schedule.activation/active-occurrence (schedules/activation conn aid))))
          (is (nil? (:schedule.activation/last-processed-input (schedules/activation conn aid)))))))))

(deftest retained-target-cursor-and-occurrence-are-durable
  (with-db
    (fn [conn]
      (let [a (create conn) id (:schedule.activation/id a)]
        (is (= "pinned" (get-in (schedules/target conn a) [:snapshot :files "task.md"])))
        (schedules/claim-due! conn a {:interval-ms 60000} (java.util.Date. 190000))
        (let [[o] (schedules/occurrences conn id)]
          (is (= (java.util.Date. 180000) (:schedule.occurrence/intended-at o)))
          (is (= 2 (:schedule.occurrence/coalesced o)))
          (is (= :pending (:schedule.occurrence/state o)))
          (is (= (java.util.Date. 240000) (:schedule.activation/next-fire (schedules/activation conn id))))
          (schedules/claim-due! conn a {:interval-ms 60000} (java.util.Date. 190000))
          (is (= 1 (count (schedules/occurrences conn id))))
          (is (uuid? (schedules/admit! conn (:schedule.occurrence/id o))))
          (is (nil? (schedules/admit! conn (:schedule.occurrence/id o))))
          (schedules/claim-due! conn a {:interval-ms 60000} (java.util.Date. 250000))
          (is (= [:skipped :admitting] (mapv :schedule.occurrence/state (schedules/occurrences conn id))))
          (is (= :overlap (:schedule.occurrence/reason (first (schedules/occurrences conn id))))))))))

(deftest pause-prevents-new-claims-without-reinterpreting-past-work
  (with-db
    (fn [conn]
      (let [a (create conn) id (:schedule.activation/id a)]
        (schedules/set-enabled! conn id false)
        (schedules/claim-due! conn a {:interval-ms 60000} (java.util.Date. 190000))
        (is (empty? (schedules/occurrences conn id)))
        (schedules/set-enabled! conn id true)
        (schedules/claim-due! conn a {:interval-ms 60000} (java.util.Date. 190000))
        (let [o (first (schedules/occurrences conn id))]
          (schedules/set-enabled! conn id false)
          (is (nil? (schedules/admit! conn (:schedule.occurrence/id o))))
          (is (= :skipped (:schedule.occurrence/state (schedules/occurrence conn (:schedule.occurrence/id o)))))
          (schedules/set-enabled! conn id true)
          (is (nil? (schedules/admit! conn (:schedule.occurrence/id o)))))))))

(deftest concurrent-ticks-insert-only-one-occurrence
  (with-db
    (fn [conn]
      (let [a (create conn) start (promise)
            calls (doall (repeatedly 8 #(future @start (schedules/claim-due! conn a {:interval-ms 60000} (java.util.Date. 60000)))))]
        (deliver start true)
        (doseq [call calls] @call)
        (is (= 1 (count (schedules/occurrences conn (:schedule.activation/id a)))))))))

(deftest a-persisted-unadmitted-occurrence-is-superseded-by-the-latest-slot
  (with-db
    (fn [conn]
      (let [a (create conn) id (:schedule.activation/id a)]
        (schedules/claim-due! conn a {:interval-ms 60000} (java.util.Date. 60000))
        (let [old (first (schedules/occurrences conn id))]
          (schedules/claim-due! conn a {:interval-ms 60000} (java.util.Date. 600000))
          (let [[latest prior] (schedules/occurrences conn id)]
            (is (= :pending (:schedule.occurrence/state latest)))
            (is (= (java.util.Date. 600000) (:schedule.occurrence/intended-at latest)))
            (is (= 9 (:schedule.occurrence/coalesced latest)))
            (is (= :coalesced (:schedule.occurrence/reason prior)))
            (is (nil? (schedules/admit! conn (:schedule.occurrence/id old))))
            (is (uuid? (schedules/admit! conn (:schedule.occurrence/id latest))))))))))

(deftest calendar-times-use-explicit-zone-and-intended-anchoring
  (let [spec {:every :day :at "09:00" :tz "Europe/Berlin"}
        first-fire (java.util.Date/from (java.time.Instant/parse "2026-03-28T08:00:00Z"))
        now (java.util.Date/from (java.time.Instant/parse "2026-03-29T10:00:00Z"))
        slot (schedules/due-slot spec first-fire now)]
    (is (= #inst "2026-03-29T07:00:00.000Z" (:intended slot)))
    (is (= #inst "2026-03-30T07:00:00.000Z" (:next slot)))
    (is (= 1 (:coalesced slot)))
    (is (nil? (schedules/due-slot spec first-fire (java.util.Date. 0))))))
