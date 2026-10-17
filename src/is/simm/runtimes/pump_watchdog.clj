(ns is.simm.runtimes.pump-watchdog
  "ALARM-ONLY observability for the room bus. There is deliberately no
   healing here — the failure classes this module was born from are fixed
   at their roots:

   - lost wakeups / silent pump wedges: spindel ≥0.1.35 (#27 redesign —
     failure routing, transactional handoffs, thread-safe buffers);
   - unrecoverable pump position: dvergr's durable-cursor bus (log-first
     fan-out; the pump is supervised and resumes from its cursor).

   What remains here:
   (1) `install-fault-reporter!` routes the ENGINE-WIDE spindel fault hook
       into Telemere (continuation faults, executor-task faults, pubsub
       watcher/pump faults — spindel carries no logging dep);
   (2) a per-minute probe of every live room's bus. Under the
       durable-cursor model the meaningful signal is CURSOR LAG — a log
       head that stays ahead of the fan-out cursor across consecutive
       probes means the supervised pump is not draining (its restart
       budget may be exhausted → look for the ::pump-fatal fault).
       The mult stage is probed too, but only after its lazy pump has
       actually started. Undemanded history may validly queue beforehand.

   If an alarm here ever fires, it is a BUG REPORT with evidence — never
   a recovery path."
  (:require [dvergr.room.registry :as rreg]
            [dvergr.runtime.bus :as bus]
            [org.replikativ.spindel.engine.core :as rtc]
            [org.replikativ.spindel.pubsub.mult :as mult]
            [taoensso.telemere :as log]))

(defonce ^:private watchdog (atom nil))

;; room-id → cursor-lag observed at the previous probe. An alarm needs the
;; SAME lag (or worse) on two consecutive probes — a single snapshot of a
;; busy bus legitimately shows transient lag.
(defonce ^:private prev-lag (atom {}))

(defn install-fault-reporter!
  "Route spindel's engine-wide fault hook into Telemere. Since 0.1.35
   `mult/set-fault-reporter!` sets the ONE hook covering continuation
   faults (a resumed consumer threw — its spin was rejected loudly),
   executor task faults, pubsub watcher faults and pump rejections.
   They default to stderr in spindel (no logging dep); simmis wants
   them in the server log. Idempotent — safe to call on every boot."
  []
  (mult/set-fault-reporter!
   (fn [event data]
     (let [severe? (contains? #{"pump-rejected" "pump-fatal"} (name event))]
       (log/log! {:level (if severe? :error :warn)
                  :id (keyword "is.simm.runtimes.pump-watchdog" (name event))
                  :msg (str "spindel fault " event)
                  :data data})))))

(defn- probe-room [room]
  (try
    (binding [rtc/*execution-context* (:ctx room)]
      (let [b   (:bus room)
            st  @(.-state-atom (:source-mbox b))
            lag (- (count (bus/log b)) (bus/log-cursor b))]
        {:room (:id room)
         :cursor-lag lag
         :mult-started? (some? (mult/mult-pump (:source-mult b)))
         :mult-queued (count (:queue st))
         :mult-waiters (count (:waiters st))}))
    (catch Throwable e
      {:room (:id room) :probe-error (.getMessage e)})))

(defn- mult-wedged? [{:keys [mult-started? mult-queued mult-waiters]}]
  (and mult-started? (zero? (or mult-waiters 0)) (pos? (or mult-queued 0))))

(defn- check-all! []
  (doseq [room (rreg/list-rooms)]
    (let [{:keys [room cursor-lag mult-queued mult-waiters probe-error] :as r}
          (probe-room room)]
      (cond
        probe-error
        (log/log! {:level :warn :id ::probe-failed :data r})

        ;; Fan-out stalled: the log head stayed ahead of the cursor across
        ;; two consecutive probes (~1 min apart). The supervised pump
        ;; should have drained or been restarted — if this fires, its
        ;; restart budget is likely exhausted (see ::pump-fatal above).
        (and (pos? (or cursor-lag 0))
             (>= (or cursor-lag 0) (get @prev-lag room 0))
             (pos? (get @prev-lag room 0)))
        (log/log! {:level :error :id ::fanout-stalled
                   :msg (str "ROOM BUS FAN-OUT STALLED: " room
                             " — cursor lag " cursor-lag
                             " persisted across probes. This is a bug "
                             "report, not a recovery path.")
                   :data r})

        ;; Mults start on demand, not on tap creation. Queued messages with
        ;; no waiter are normal until the first routing consumer asks.
        (mult-wedged? r)
        (log/log! {:level :error :id ::pump-wedged
                   :msg (str "ROOM BUS MULT STAGE WEDGED: " room
                             " — " mult-queued " message(s) queued, no waiter.")
                   :data r}))
      (when-not probe-error
        (swap! prev-lag assoc room (or cursor-lag 0))))))

(defn start!
  "Start the per-minute bus probe. Idempotent."
  []
  (install-fault-reporter!)
  (when-not @watchdog
    (let [t (Thread.
             (fn []
               (loop []
                 (try (check-all!)
                      (catch Throwable e
                        (log/log! {:level :warn :id ::watchdog-error
                                   :data {:error (.getMessage e)}})))
                 (Thread/sleep 60000)
                 (when @watchdog (recur))))
             "simmis-pump-watchdog")]
      (.setDaemon t true)
      (reset! watchdog t)
      (.start t)
      (log/log! {:level :info :id ::started
                 :msg "Bus watchdog started (60s interval, alarm-only)"})))
  :started)

(defn stop! []
  (reset! watchdog nil)
  :stopped)
