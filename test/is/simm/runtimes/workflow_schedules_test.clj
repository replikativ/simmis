(ns is.simm.runtimes.workflow-schedules-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [dvergr.runtime.clock :as clock]
            [is.simm.ops.workflow-schedules :as schedules]
            [is.simm.runtimes.context :as context]
            [is.simm.runtimes.workflow-schedules :as runtime]
            [org.replikativ.spindel.core :as sp]
            [org.replikativ.spindel.engine.context :as execution]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.impl.simple :as simple]
            [org.replikativ.spindel.signal :as sig]
            [org.replikativ.spindel.spin.sync :as sync]))

(use-fixtures :each
  (fn [f]
    ;; A diagnostic test must never touch the live demo's worker or clock.
    (let [ctx (execution/create-execution-context)]
      (with-redefs-fn {#'runtime/service (atom nil) #'context/server-context ctx}
        #(binding [ec/*execution-context* ctx]
           (try (f)
                (finally
                  (runtime/stop!)
                  (execution/close-context! ctx))))))))

(defn- await-event [p]
  (let [result (deref p 30000 ::timeout)]
    (is (not= ::timeout result) "await a condition, never sleep to coordinate")
    result))

(deftest clock-egress-coalesces-off-engine-and-stop-physically-joins
  (let [ctx context/server-context
        clock-signal (binding [ec/*execution-context* ctx] (sig/signal (java.util.Date. 0)))
        entered (promise) release (promise) second-tick (promise)
        calls (atom []) recovered? (atom false)]
    (with-redefs [clock/clock-signal (constantly clock-signal)
                  clock/minute-now (constantly (java.util.Date. 60000))
                  schedules/recover! (fn [_ owner]
                                       (is (identical? ctx owner))
                                       (reset! recovered? true))
                  schedules/tick! (fn [_ owner _ now]
                                    (is @recovered?)
                                    (is (identical? owner ec/*execution-context*))
                                    (is (not simple/*in-drain?*))
                                    (let [n (count (swap! calls conj now))]
                                      (if (= 1 n)
                                        (do (deliver entered true) @release)
                                        (deliver second-tick now))))]
      (try
        (is (= :started (runtime/start! ::conn ctx ::resolve)))
        (await-event entered)
        (is (runtime/running?))
        (is (= :started (runtime/start! ::conn ctx ::resolve)))
        ;; A blocked admission never blocks this clock producer. Pending
        ;; notifications collapse to the latest observed minute.
        (binding [ec/*execution-context* ctx]
          (reset! clock-signal (java.util.Date. 120000)))
        (let [committed (promise)]
          ;; The actual clock writes from a Spin. Its engine-derived context
          ;; record is not identical? to ctx, but belongs to the same home.
          (binding [ec/*execution-context* ctx]
            (sp/spawn! (sp/spin (reset! clock-signal (java.util.Date. 180000))
                                (deliver committed true))))
          (is (true? (await-event committed))))
        (let [fork (execution/fork-context ctx)]
          (try
            (binding [ec/*execution-context* fork]
              (reset! clock-signal (java.util.Date. 999000)))
            ;; Even a stale ancestor home cell cannot grant a repointed
            ;; backend authority: home-context checks backend ownership too.
            (binding [ec/*execution-context* (assoc fork :home (:home ctx))]
              (reset! clock-signal (java.util.Date. 1000000)))
            (finally (execution/close-context! fork))))
        (deliver release true)
        (is (= (java.util.Date. 180000) (await-event second-tick)))
        (is (= :stopped (runtime/stop!)))
        (is (not (runtime/running?)))
        (binding [ec/*execution-context* ctx]
          (reset! clock-signal (java.util.Date. 240000)))
        (is (= [(java.util.Date. 60000) (java.util.Date. 180000)] @calls))
        (finally (deliver release true))))))

(deftest failed-recovery-never-admits-and-requires-an-explicit-joined-stop
  (let [ctx context/server-context
        clock-signal (binding [ec/*execution-context* ctx] (sig/signal (java.util.Date. 0)))
        calls (atom 0)]
    (with-redefs [clock/clock-signal (constantly clock-signal)
                  schedules/recover! (fn [& _] (throw (ex-info "scripted recovery failure" {})))
                  schedules/tick! (fn [& _] (swap! calls inc))]
      (is (thrown? clojure.lang.ExceptionInfo (runtime/start! ::conn ctx ::resolve)))
      (is (not (runtime/running?)))
      (is (thrown? clojure.lang.ExceptionInfo (runtime/start! ::conn ctx ::resolve)))
      (is (= 0 @calls))
      (is (= :stopped (runtime/stop!))))))

(deftest lifecycle-is-refused-on-the-engine-drain
  (let [result (promise) gate (sync/deferred)]
    (sp/spawn!
     (sp/spin
      (sp/await gate)
      (deliver result
               (binding [ec/*spin-id* nil simple/*in-drain?* false]
                 [(try (runtime/start! ::conn context/server-context ::resolve) ::unexpected
                       (catch clojure.lang.ExceptionInfo error (:type (ex-data error))))
                  (try (runtime/stop!) ::unexpected
                       (catch clojure.lang.ExceptionInfo error (:type (ex-data error))))]))))
    (sync/deliver! gate true)
    (is (= [::runtime/engine-thread ::runtime/engine-thread] (await-event result)))))

(deftest inherited-engine-bindings-do-not-refuse-a-native-lifecycle-owner
  (let [ctx context/server-context
        clock-signal (sig/signal (java.util.Date. 0))
        native (promise) gate (sync/deferred)]
    (with-redefs [clock/clock-signal (constantly clock-signal)
                  clock/minute-now (constantly (java.util.Date. 0))
                  schedules/recover! (fn [& _] nil)
                  schedules/tick! (fn [& _] nil)]
      (sp/spawn!
       (sp/spin
        (sp/await gate)
        (deliver native
                 (future
                   {:spin ec/*spin-id* :flag simple/*in-drain?*
                    :drain? (simple/on-drain-thread?)
                    :start (runtime/start! ::conn ctx ::resolve)
                    :stop (runtime/stop!)}))))
      (sync/deliver! gate true)
      (let [worker (await-event native)
            result (deref worker 30000 ::timeout)]
        (is (some? (:spin result)))
        (is (true? (:flag result)))
        (is (false? (:drain? result)))
        (is (= :started (:start result)))
        (is (= :stopped (:stop result)))
        (is (not (runtime/running?)))))))
