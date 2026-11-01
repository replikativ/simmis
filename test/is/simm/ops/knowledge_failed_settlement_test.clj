(ns is.simm.ops.knowledge-failed-settlement-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.room.store :as store]
            [is.simm.ops.knowledge-failed-settlement :as settlement]))

(deftest discarded-projection-alone-does-not-establish-world-return
  (let [id (random-uuid)
        returned {:run/id id :run/world :original :run/settlement-status :discarded
                  :run/settlement-reason :failed :run/family-return-ack (store/family-return-ack-id id)}]
    (is (@#'settlement/world-returned? returned))
    (is (not (@#'settlement/world-returned? (dissoc returned :run/family-return-ack))))
    (is (not (@#'settlement/world-returned? (assoc returned :run/settlement-status :recovery-required))))
    ;; Evaluation's durability-first projection can precede physical disposal;
    ;; it must not be upgraded into a completion receipt by a Job cleanup ACK.
    (doseq [reason [nil :discard-unacknowledged :settlement-cleanup-failed
                    :evaluation-policy :evaluation-cancelled :evaluation-certification-failed]]
      (is (not (@#'settlement/world-returned? (assoc returned :run/settlement-reason reason)))))))
