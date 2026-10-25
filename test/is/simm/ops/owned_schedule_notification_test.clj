(ns is.simm.ops.owned-schedule-notification-test
  "Real original-Room posts and immutable intent retry. Proposal certification
   is the explicitly synthetic fixture; these tests make no model call."
  (:require [clojure.test :refer [deftest is]]
            [dvergr.discourse :as discourse]
            [is.simm.agents.room-agents :as rooms]
            [is.simm.ops.knowledge-batch-filing-test :as fixture]
            [is.simm.ops.proposal-publication :as publication]
            [is.simm.ops.proposals :as proposals]))

(deftest successful-proposal-notification-and-lost-ack-retry-use-the-original-room
  (@#'fixture/proposed-fixture
   (fn [{:keys [room w value]}]
     (let [binding {:room-id (:room w) :room room}
           mark! proposals/mark-message-published! lost? (atom false)]
       (with-redefs [rooms/live-room (fn [& _] (throw (AssertionError. "Fixed notification may not hydrate a replacement")))
                     proposals/mark-message-published!
                     (fn [id]
                       (if (compare-and-set! lost? false true)
                         (throw (ex-info "Lost post ACK before projection marker" {}))
                         (mark! id)))]
         (is (= :pending (publication/publish-original! (:proposal value) binding)))
         (is (= :pending (:proposal/message-status (proposals/get-proposal (:proposal value)))))
         (is (= :published (publication/publish-original! (:proposal value) binding)))
         (is (= :published (publication/publish-original! (:proposal value) binding)))
         (let [id (:proposal/message-id (proposals/get-proposal (:proposal value)))]
           (is (= 1 (count (filter #(= id (:id %)) (discourse/messages room)))))))))))

(deftest interrupted-original-room-post-remains-pending-and-cannot-report-normal-success
  (@#'fixture/proposed-fixture
   (fn [{:keys [room w value]}]
     (let [failure (ex-info "Native post interrupted" {} (InterruptedException. "Fixture cancellation"))
           observed (atom nil) irq (atom false)]
       (try
         (with-redefs [discourse/post! (fn [& _] (throw failure))]
           (try (publication/publish-original! (:proposal value) {:room-id (:room w) :room room})
                (catch Exception error (reset! observed error)))
           ;; STM test reporting may consume IRQ, so capture before assertions.
           (reset! irq (.isInterrupted (Thread/currentThread))))
         (finally (Thread/interrupted)))
       (is (identical? failure @observed))
       (is @irq)
       (is (= :pending (:proposal/message-status (proposals/get-proposal (:proposal value)))))))))

(deftest wrong-original-room-binding-cannot-post-or-resolve-another-incarnation
  (@#'fixture/proposed-fixture
   (fn [{:keys [room value]}]
     (with-redefs [rooms/live-room (fn [& _] (throw (AssertionError. "May not resolve replacement")))
                   discourse/post! (fn [& _] (throw (AssertionError. "May not post to wrong room")))]
       (is (= :pending (publication/publish-original! (:proposal value) {:room-id (random-uuid) :room room})))))))
