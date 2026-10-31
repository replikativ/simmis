(ns is.simm.uis.web.desktop.room-digest-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.uis.web.desktop.room-digest :as digest]
            [is.simm.model.room-summaries-test :as fixture]))

(deftest stale-replies-and-uncertain-launches-never-replay
  (let [connection [:socket 1] token (random-uuid)
        loading (digest/begin nil connection token :launch (random-uuid))
        request (random-uuid)
        uncertain (digest/receive loading connection token {:status :inspection-required :request request})]
    (is (= :inspection-required (:phase uncertain)))
    (is (= uncertain (digest/receive uncertain connection (random-uuid) {:status :submitted :request request})))
    (is (= :inspection-required (:phase (digest/begin uncertain connection (random-uuid) :launch (random-uuid)))))
    (is (= :inspection-required (:phase (digest/reconcile loading [:socket 2]))))
    (is (= request (:request (digest/hide uncertain))))
    (let [read (digest/begin uncertain connection (random-uuid) :result (random-uuid))]
      (is (= :result-loading (:phase read)))
      (is (true? (:effect-pending? read))))))

(deftest conversation-context-is-a-room-bound-navigation-hint
  (let [room (random-uuid) message (random-uuid)
        source {:path (str "inbox/conversations/room-activity/" room "/" message "/" (random-uuid) ".md")}]
    (is (= {:kind :message :room (str room) :message (str message)} (digest/message-context room source)))
    (is (nil? (digest/message-context (random-uuid) source)))
    (is (nil? (digest/message-context room {:path "inbox/other.md"})))))

(deftest persistent-notification-pointer-works-with-fresh-ui-state
  (let [request (random-uuid) digest-id (random-uuid)
        metadata {:kind :conversation-digest :object {:kind :conversation-digest :id digest-id}
                  :source :simmis/conversation-digest :notification/type :task-complete :notification/task request}
        pointer (digest/notification-reference metadata)
        loading (-> nil (assoc :request (:request pointer) :expected-digest-id (:digest-id pointer))
                    (digest/begin :socket :read :result (random-uuid)))]
    (is (= {:request request :digest-id digest-id} pointer))
    (is (= :result-loading (:phase loading)))
    (is (= request (:request loading)))
    (is (= digest-id (:expected-digest-id loading)))
    (is (nil? (digest/notification-reference (assoc metadata :source "invalid"))))
    (is (nil? (digest/notification-reference (assoc metadata :kind :ordinary-message))))))

(deftest uncertain-chat-publication-remains-sticky-even-after-reading-result-again
  (let [request (random-uuid) digest-id (random-uuid)
        verified {:request request :result {:digest-id digest-id}}
        publishing (digest/begin verified :socket :write :publish :message)
        uncertain (digest/receive publishing :socket :write {:status :inspection-required})]
    (is (= :publishing (:phase publishing)))
    (is (:publication-pending? uncertain))
    (is (= :inspection-required (:phase (digest/begin uncertain :socket :again :publish :message))))
    (is (= :inspection-required (:phase (digest/hide publishing))))
    (let [ack (digest/receive publishing :socket :write {:status :published :request request
                                                       :digest-id digest-id :message (random-uuid)})]
      (is (= :published (:phase ack)))
      (is (false? (:publication-pending? ack))))))

(deftest daily-summary-pointer-loads-frozen-coverage-and-context
  (let [request (random-uuid) revision (random-uuid)
        metadata {:kind :room-summary :object {:kind :room-summary :id revision}
                  :source :simmis/room-summary :notification/type :task-complete :notification/task request}
        selection (fixture/selection 1)
        reply {:status :verified :kind :room-summary :request request :digest-id revision
               :selection selection :digest (fixture/report selection)
               :originals (mapv #(assoc % :text "Exact frozen original") (:coverage selection))}
        state (-> {:request request :expected-digest-id revision}
                  (digest/begin :socket :read :result :message))]
    (is (= {:request request :digest-id revision} (digest/notification-reference metadata)))
    (is (= :verified (:phase (digest/receive state :socket :read reply))))
    (is (= :unavailable (:phase (digest/receive state :socket :read (assoc reply :digest-id (random-uuid))))))
    (is (= :unavailable (:phase (digest/receive state :socket :read (assoc reply :originals [])))))))
