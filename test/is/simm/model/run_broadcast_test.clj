(ns is.simm.model.run-broadcast-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.room.registry :as room-registry]
            [is.simm.agents.room-agents :as room-agents]
            [is.simm.model.run-broadcast :as run-broadcast]
            [org.replikativ.spindel.engine.context :as ctx]))

(deftest run-summary-verifies-the-exact-live-world-capability
  (let [room-id (random-uuid)
        runtime-room-id :room
        run-id (random-uuid)
        world-id :room_fork_review
        runtime-ctx (ctx/create-execution-context)
        run {:run/id run-id
             :run/room runtime-room-id
             :run/kind :agent-turn
             :run/trigger (random-uuid)
             :run/status :completed
             :run/world world-id
             :run/settlement-status :review}
        world {:id world-id
               :parent-id runtime-room-id
               :meta (atom {:run-id run-id})}]
    (with-redefs [room-agents/live-room (fn [id]
                                         (when (= room-id id)
                                           {:id runtime-room-id :ctx runtime-ctx}))
                  room-registry/lookup (fn [id] (when (= world-id id) world))]
      (is (true? (:world-live? (run-broadcast/run-summary room-id run))))
      (is (false? (:world-live?
                   (run-broadcast/run-summary room-id
                                              (assoc run :run/id (random-uuid)))))))
    (with-redefs [room-agents/live-room (constantly nil)]
      (is (false? (:world-live? (run-broadcast/run-summary room-id run)))))))
