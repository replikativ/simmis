(ns is.simm.runtimes.worker-maintenance
  "Application-owned orphan reconciliation, independent of receipt delivery.
   This timer is a host I/O resource, never forkable room state. It performs
   no execution, import, certification or inferred physical shutdown."
  (:require [is.simm.model.work-requests :as requests]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.worker-recovery :as recovery]
            [is.simm.ops.worker-work :as local]
            [org.replikativ.spindel.engine.core :as ec]
            [taoensso.telemere :as log])
  (:import [java.util.concurrent ScheduledThreadPoolExecutor ThreadFactory TimeUnit]))

(def policy {:version 1 :reconcile-delay-ms 30000 :shutdown-ms 35000})
(defonce ^:private service (atom nil))

(defn reconcile!
  "One serial maintenance pass over pending worker admissions. A failed
   request does not abort the pass. Missing authority remains pending; all state
   transitions are the existing idempotent recovery adapter's responsibility."
  [conn host-ctx resolve-room]
  (binding [ec/*execution-context* host-ctx]
    (loop [rows (filter #(and (local/request-kinds (:work.request/kind %))
                              (or (:work.request/return-pending? %)
                                  (and (= :local-worker-benchmark (:work.request/kind %))
                                       (benchmarks/retirement-pending? host-ctx (:work.request/id %)))))
                       (requests/requests conn))
           counts {}]
      (if (or (empty? rows) (.isInterrupted (Thread/currentThread)))
        counts
        (let [id (:work.request/id (first rows))
              state (try
                      (or (:state (recovery/recover-request! conn host-ctx resolve-room id)) :unchanged)
                      (catch InterruptedException e (throw e))
                      (catch Exception e
                        (log/log! {:level :error :id ::request-held
                                   :msg "Local worker recovery remains unresolved"
                                   :data {:request id :exception-class (.getName (class e))}})
                        :error))]
          (recur (next rows) (update counts state (fnil inc 0))))))))

(defn running? []
  (let [{:keys [executor future]} @service]
    (boolean (and executor future (not (.isShutdown ^ScheduledThreadPoolExecutor executor))
                  (not (.isDone ^java.util.concurrent.Future future))))))

(defn- new-executor []
  (doto (ScheduledThreadPoolExecutor.
         1 (reify ThreadFactory
             (newThread [_ runnable]
               (doto (Thread. runnable "simmis-local-worker-recovery") (.setDaemon true)))))
    (.setRemoveOnCancelPolicy true)
    (.setExecuteExistingDelayedTasksAfterShutdownPolicy false)
    (.setContinueExistingPeriodicTasksAfterShutdownPolicy false)))

(defn claims-ready? []
  (and (running?) (true? (:claims-ready? @service))))

(defn- initial-pass! [conn host-ctx resolve-room]
  (recovery/recover! conn host-ctx resolve-room)
  ;; Recovery deliberately holds an unresolved room rather than guessing its
  ;; ownership. Such a skipped room is not successful fencing of its queued
  ;; assignments. Keep claims held until canonical room resolution succeeds.
  (binding [ec/*execution-context* host-ctx]
    (doseq [row (requests/requests conn)
            :when (and (local/request-kinds (:work.request/kind row)) (:work.request/return-pending? row))]
      (when-not (resolve-room (:work.request/room row))
        (throw (ex-info "Pending worker room has not been canonically resolved" {:type ::held}))))))

(defn prepare!
  "Attempt startup fencing before accepting new claims. Failure holds claim
   readiness but MUST NOT remove the existing lease shutdown transport."
  [conn host-ctx resolve-room]
  (locking service
    (when (:executor @service)
      (swap! service assoc :claims-ready? false)
      (throw (ex-info "Worker recovery resource still belongs to a prior startup" {:type ::held})))
    (reset! service {:claims-ready? false})
    (try
      (initial-pass! conn host-ctx resolve-room)
      (swap! service assoc :claims-ready? true)
      (catch Exception e
        (log/log! {:level :error :id ::startup-held
                   :msg "Worker claims await successful orphan reconciliation"
                   :data {:exception-class (.getName (class e))}})))))

(defn start!
  "Start after the synchronous startup recovery pass, before new MCP claims.
   Fixed delay is periodic reconciliation, not a synchronization/timing patch.
   One task, one thread, no overlapping passes or unbounded event queue."
  [conn host-ctx resolve-room]
  (locking service
    (if (:executor @service)
      (when-not (running?)
        (throw (ex-info "Local worker recovery must be stopped before replacement" {:type ::held})))
      (let [executor (new-executor)
            task (reify Runnable
                   (run [_]
                     (try
                       ;; Retry failed initial fencing without authorizing
                       ;; claims. Existing shutdown remains independent.
                       (when-not (:claims-ready? @service)
                         (initial-pass! conn host-ctx resolve-room)
                         (swap! service (fn [current]
                                          (if (and (identical? executor (:executor current))
                                                   (not (.isShutdown executor)))
                                            (assoc current :claims-ready? true) current))))
                       (reconcile! conn host-ctx resolve-room)
                       (catch InterruptedException _ (.interrupt (Thread/currentThread)))
                       (catch Exception e
                         (log/log! {:level :error :id ::pass-failed
                                    :msg "Local worker recovery pass failed"
                                    :data {:exception-class (.getName (class e))}})))))]
        ;; Retain ownership even if scheduling fails; stop! is the only place
        ;; which clears a physically joined service. No duplicate executor.
        (swap! service assoc :executor executor)
        (try
          (let [future (.scheduleWithFixedDelay executor task (:reconcile-delay-ms policy)
                                                (:reconcile-delay-ms policy) TimeUnit/MILLISECONDS)]
            (swap! service assoc :future future))
          (catch Throwable e
            (.shutdownNow executor)
            (throw e)))))))

(defn- await-stop! [^ScheduledThreadPoolExecutor executor]
  (.awaitTermination executor (:shutdown-ms policy) TimeUnit/MILLISECONDS))

(defn stop!
  "Interrupt and join the host resource before releasing it. A failed join
   retains the handle and refuses replacement; it never fakes cleanup."
  []
  (locking service
    (when-let [{:keys [executor]} @service]
      (when executor
        (.shutdownNow ^ScheduledThreadPoolExecutor executor)
        (when-not (await-stop! executor)
          (throw (ex-info "Local worker recovery has not physically stopped" {:type ::held}))))
      (reset! service nil))))
