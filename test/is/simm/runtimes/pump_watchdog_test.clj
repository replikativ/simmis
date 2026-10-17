(ns is.simm.runtimes.pump-watchdog-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.runtimes.pump-watchdog :as watchdog]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.pubsub.mult :as mult]
            [org.replikativ.spindel.spin.sync :as sync]))

(deftest lazy-undemanded-mult-is-not-a-wedged-consumer
  (let [ctx (context/create-execution-context)]
    (try
      (binding [ec/*execution-context* ctx]
        (let [mailbox (sync/create-mailbox ctx)
              m (mult/mult mailbox)
            ;; Tap creation alone must not force execution just to satisfy
            ;; observability. Probe an idle source with queued history.
              _ (mult/tap m)
              room {:id :lazy-fixture :ctx ctx
                    :bus {:source-mbox mailbox :source-mult m
                          :log (atom {:entries [] :cursor 0})}}
              probe (assoc (@#'watchdog/probe-room room) :mult-queued 3)]
          (is (nil? (mult/mult-pump m)))
          (is (false? (:mult-started? probe)))
          (is (not (@#'watchdog/mult-wedged? probe)))
          (is (nil? (mult/mult-pump m)) "Probing must not demand the pump")))
      (finally (context/close-context! ctx)))))

(deftest demanded-mult-with-a-queue-but-no-waiter-remains-an-alarm
  (is (@#'watchdog/mult-wedged? {:mult-started? true :mult-queued 3 :mult-waiters 0}))
  (is (not (@#'watchdog/mult-wedged? {:mult-started? true :mult-queued 0 :mult-waiters 0})))
  (is (not (@#'watchdog/mult-wedged? {:mult-started? true :mult-queued 3 :mult-waiters 1}))))
