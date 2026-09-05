(ns is.simm.uis.web.desktop.room-details-test
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.uis.web.desktop.room-details :as rd]))

(def room "room-1")

(deftest a-first-request-starts-a-load
  (let [[state action] (rd/begin {} room false)]
    (is (= :start action))
    (is (true? (get-in state [room :in-flight?])))
    (is (false? (get-in state [room :pending?])))))

(deftest an-unforced-request-rides-along-with-the-load-in-flight
  (let [[state _] (rd/begin {} room false)
        [state' action] (rd/begin state room false)]
    (is (= :skip action) "data will exist either way — one fetch answers both")
    (is (= state state'))))

(deftest a-forced-request-mid-flight-is-queued-not-dropped
  (testing "the in-flight answer predates the write, so it cannot be the last word"
    (let [[state _] (rd/begin {} room false)
          [state' action] (rd/begin state room true)]
      (is (= :queue action))
      (is (true? (get-in state' [room :pending?])))
      (testing "and it runs when the in-flight load retires"
        (let [[state'' action'] (rd/finish state' room)]
          (is (= :start action'))
          (is (true? (get-in state'' [room :in-flight?])))
          (is (false? (get-in state'' [room :pending?])))
          (testing "exactly once — the queue does not loop"
            (is (= [{} :idle] (rd/finish state'' room)))))))))

(deftest finishing-a-quiet-load-leaves-no-bookkeeping
  (let [[state _] (rd/begin {} room false)]
    (is (= [{} :idle] (rd/finish state room)))))

(deftest rooms-are-tracked-independently
  (let [[state _] (rd/begin {} room false)
        [state' action] (rd/begin state "room-2" false)]
    (is (= :start action) "another room's load is not blocked by this one")
    (is (true? (get-in state' [room :in-flight?])))
    (is (true? (get-in state' ["room-2" :in-flight?])))))

(deftest keyed-payloads-keep-two-rendered-panels-stable-in-either-completion-order
  (let [a {:room {:room/id room :room/name "A"}}
        b {:room {:room/id "room-2" :room/name "B"}}
        complete (fn [first-id first-payload second-id second-payload]
                   (-> {}
                       (rd/loading first-id)
                       (rd/loading second-id)
                       (rd/successful first-id first-payload)
                       (rd/successful second-id second-payload)))]
    (doseq [details [(complete room a "room-2" b)
                     (complete "room-2" b room a)]]
      ;; These are the selectors used by room-settings and agent-inspector.
      (is (= a (rd/data-for details room)))
      (is (= b (rd/data-for details "room-2")))
      (is (nil? (rd/error-for details room)))
      (is (nil? (rd/error-for details "room-2"))))))

(deftest force-and-error-affect-only-the-requested-room-panel
  (let [a {:room {:room/id room :room/name "A"}}
        b {:room {:room/id "room-2" :room/name "B"}}
        loaded (-> {}
                   (rd/successful room a)
                   (rd/successful "room-2" b))
        forced (rd/loading loaded room)
        errored (rd/failed forced room :unavailable)]
    (is (= a (rd/data-for forced room)) "force reload does not blank A")
    (is (= b (rd/data-for forced "room-2")) "force reload leaves B rendered")
    (is (= :unavailable (rd/error-for errored room)))
    (is (= b (rd/data-for errored "room-2")))
    (is (nil? (rd/error-for errored "room-2")))))

(deftest reopening-loaded-panel-does-not-need-another-request
  (let [details (rd/successful {} room {:room {:room/id room}})]
    (is (some? (rd/data-for details room))
        "the panel's keyed selector has data, so its nil guard does not load")))

(deftest a-forced-request-with-nothing-in-flight-starts-immediately
  (let [[state action] (rd/begin {} room true)]
    (is (= :start action))
    (is (false? (get-in state [room :pending?])))))
