(ns is.simm.authority.workflow-blocking-test
  "Physical worker-boundary tests, separate from the real inbound authority
   regressions in workflows-remote-test and local-workflows-remote-test."
  (:require [clojure.test :refer [deftest is]]
            [is.simm.agents.room-agents :as rooms]
            [is.simm.model.access :as access]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.workflows :as workflows]
            [is.simm.uis.web.desktop.workflows-remote :as remote]
            [org.replikativ.spindel.blocking :as blocking]
            [org.replikativ.spindel.core :as sp]
            [org.replikativ.spindel.engine.context :as execution]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.impl.simple :as simple]
            [org.replikativ.spindel.spin.sync :as sync])
  (:import [java.util.concurrent CountDownLatch ExecutorService Executors TimeUnit]))

(defn- join-pool! [^ExecutorService pool]
  (.shutdown pool)
  (when-not (.awaitTermination pool 10 TimeUnit/SECONDS)
    (is false "Workflow test worker did not exit promptly; still joining, not assuming quiescence")
    (.shutdownNow pool)
    (loop []
      (when-not (.awaitTermination pool 10 TimeUnit/SECONDS)
        (recur)))))

(deftest caller-is-captured-before-the-worker-runs
  (let [original (random-uuid) replacement (random-uuid)
        ambient (atom original) queued (atom nil) observed (atom [])
        room (str (random-uuid)) args {:name "example"}]
    (with-redefs [access/authenticated-party-id #(deref ambient)
                  blocking/blocking (fn [f] (reset! queued f) ::queued)
                  remote/invoke-workflow (fn [& invocation]
                                           (swap! observed conj invocation)
                                           ::completed)]
      (is (= ::queued (@#'remote/workflow-on-worker room :simmis/workflow-source args)))
      (is (empty? @observed))
      (reset! ambient replacement)
      (is (= ::completed (@queued)))
      (is (= [[original room :simmis/workflow-source args]] @observed)))))

(deftest real-drain-entry-refuses-before-hydration-or-host-io
  (let [ctx (execution/create-execution-context)
        start (sync/create-deferred ctx) outcome (promise)
        forbidden (fn [& _] (throw (AssertionError. "Drain reached host work")))]
    (try
      (with-redefs [rooms/live-room forbidden ops/invoke forbidden workflows/details forbidden]
        (binding [ec/*execution-context* ctx]
          (sp/spawn!
           (sp/spin
            (sp/await start)
            (deliver outcome
                     {:drain? (simple/on-drain-thread?)
                      :failure (try
                                 (remote/invoke-workflow (random-uuid) (str (random-uuid))
                                                         :simmis/workflow-list {})
                                 ::unexpected
                                 (catch clojure.lang.ExceptionInfo error (ex-data error)))})))
          (sync/deliver! start true))
        (let [result (deref outcome 10000 ::timeout)]
          (is (true? (:drain? result)))
          (is (= {:type :validation-error} (:failure result)))))
      (finally (execution/close-context! ctx)))))

(deftest a-held-workflow-worker-does-not-block-its-engine
  (let [ctx (execution/create-execution-context)
        start (sync/create-deferred ctx) sentinel-start (sync/create-deferred ctx)
        entered (promise) exited (promise) outcome (promise) sentinel (promise)
        release (CountDownLatch. 1)
        pool (Executors/newSingleThreadExecutor)
        actual-blocking blocking/blocking
        party (random-uuid) room (str (random-uuid)) args {:name "example"}]
    (try
      (with-redefs [access/authenticated-party-id (constantly party)
                    ;; The real bridge, with test-owned workers so even a
                    ;; pre-entry failure has a physical queued/running join.
                    blocking/blocking (fn [f] (actual-blocking f {:pool pool}))
                    remote/invoke-workflow
                    (fn [& invocation]
                      (try
                        (deliver entered {:drain? (simple/on-drain-thread?) :invocation invocation})
                        (.await release)
                        ::completed
                        (finally (deliver exited true))))]
        (try
          ;; Capture the owned pool in the resource BEFORE admitting a parent.
          ;; Even a late parent after shutdown cannot escape to the default pool.
          (let [work (binding [ec/*execution-context* ctx]
                       (@#'remote/workflow-on-worker room :simmis/workflow-source args))]
            (binding [ec/*execution-context* ctx]
              (sp/spawn!
               (sp/spin
                (sp/await start)
                (let [before (simple/on-drain-thread?)
                      result (sp/await work)]
                  (deliver outcome {:before-drain? before :result result}))))
              (sync/deliver! start true)))
          (let [worker (deref entered 10000 ::timeout)]
            (is (false? (:drain? worker)))
            (is (= [party room :simmis/workflow-source args] (:invocation worker))))
          ;; Force another continuation through the SAME physical engine while
          ;; host work is held. Merely observing a worker name is insufficient.
          (binding [ec/*execution-context* ctx]
            (sp/spawn! (sp/spin (sp/await sentinel-start)
                                (deliver sentinel (simple/on-drain-thread?))))
            (sync/deliver! sentinel-start true))
          (is (true? (deref sentinel 10000 ::timeout)))
          (is (not (realized? outcome)))
          (.countDown release)
          (is (= {:before-drain? true :result ::completed} (deref outcome 10000 ::timeout)))
          (finally
            (.countDown release)
            ;; Join ALL queued/running work before restoring the function roots,
            ;; even if entry timed out. A timeout is not physical completion.
            (join-pool! pool)
            (when (realized? entered)
              (is (true? (deref exited 0 ::timeout))))
            (execution/close-context! ctx))))
      (finally
        (.countDown release)))))
