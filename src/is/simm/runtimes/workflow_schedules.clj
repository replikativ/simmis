(ns is.simm.runtimes.workflow-schedules
  "Host-owned scheduled-work dispatcher. The clock listener only offers into
   a one-element queue; recovery and admissions run on one physical worker.
   The atom below owns I/O resources, never forkable application state."
  (:require [dvergr.runtime.clock :as clock]
            [dvergr.runtime.ctx :as rctx]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.workflow-schedules :as schedules]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.impl.simple :as simple]
            [taoensso.telemere :as log])
  (:import [java.util.concurrent ArrayBlockingQueue ExecutorService Executors
            Future ThreadFactory TimeUnit]
           [java.util.concurrent.atomic AtomicBoolean]))

(def policy {:version 1 :startup-ms 35000 :shutdown-ms 35000})
(defonce ^:private service (atom nil))

(defn held?
  "Whether any physical service handles remain, including failed startup or
   failed shutdown. Observational only; lifecycle is application-coordinated."
  []
  (some? @service))

(defn- off-engine! []
  (when (simple/on-drain-thread?)
    (throw (ex-info "Scheduled-work lifecycle must run off the engine drain"
                    {:type ::engine-thread}))))

(defn running? []
  (let [{:keys [executor future ready? accepting]} @service]
    (boolean (and ready? accepting (.get ^AtomicBoolean accepting)
                  executor (not (.isShutdown ^ExecutorService executor))
                  future (not (.isDone ^Future future))))))

(defn- offer! [{:keys [queue accepting]} now]
  ;; Only the listener and startup producer take this short lock; the worker
  ;; never holds it while doing I/O. At most one latest notification is kept.
  (locking queue
    (when (.get ^AtomicBoolean accepting)
      (when-not (.offer ^ArrayBlockingQueue queue now)
        (.poll ^ArrayBlockingQueue queue)
        (.offer ^ArrayBlockingQueue queue now)))))

(defn- original-operation! [host f]
  (let [with-operation (get-in host [:config :host-lifecycle :with-operation])]
    (when-not (fn? with-operation)
      (throw (ex-info "Owned scheduler has no original App lifetime" {:type ::held})))
    (with-operation #(do (lifetime/check-current!) (f)))))

(defn- owned-pass!
  "One actual worker operation, released before waiting for the next clock
   notification. Neither a queue closure nor an inherited ticket owns the App."
  [host operation now]
  (original-operation!
   host #(case operation
           :recover (schedules/recover-owned! host)
           :tick (do (schedules/tick-owned! host now)
                     ;; Literal fixed handler, resolved after App namespaces
                     ;; load to avoid the Git HOST -> App namespace cycle.
                     ((requiring-resolve 'is.simm.ops.git-polling/tick-owned!) host now)))))

(defn- start-service!
  "Start after canonical orphan/resource recovery. Recovery runs once on the
   owned worker before any admission; failure/timeout leaves ownership held
   and no ready service. Call stop! before retrying. Never starts the shared
   clock itself. Repeated starts are allowed only for the same live owner."
  [conn ctx resolve-room mode host]
  (off-engine!)
  (locking service
    (if-let [prior @service]
      (if (and (running?) (= mode (:mode prior)) (identical? conn (:conn prior))
               (if (= :owned mode)
                 (and (identical? ctx (:ctx prior))
                      (identical? (:app-owner prior) (:owner lifetime/*ticket*)))
                 (identical? (ec/home-context ctx) (ec/home-context (:ctx prior)))))
        :started
        (throw (ex-info "Prior scheduled-work resource must be stopped first" {:type ::held})))
      (let [root (rctx/root-ctx ctx)
            signal (binding [ec/*execution-context* root] (clock/clock-signal))
            key (keyword (str "workflow-schedules-" (random-uuid)))
            executor (Executors/newSingleThreadExecutor
                      (reify ThreadFactory
                        (newThread [_ runnable]
                          (doto (Thread. runnable "simmis-workflow-schedules") (.setDaemon true)))))
            ready (promise)
            owned {:conn conn :ctx ctx :mode mode
                   :app-owner (when (= :owned mode) (:owner lifetime/*ticket*))
                   :root root :signal signal :watch-key key
                   :executor executor :queue (ArrayBlockingQueue. 1)
                   :accepting (AtomicBoolean. true) :ready? false}]
        ;; Retain handles even after failed setup; only joined stop releases.
        (reset! service owned)
        (try
          (binding [ec/*execution-context* root]
            (add-watch signal key
                       (fn [_ _ _ now]
                         ;; Signal listeners are COW-local. A fork can inherit
                         ;; the closure, but may never wake real host work.
                         ;; Engine slices derive context records via assoc.
                         ;; Record identity is not world identity; the stable
                         ;; home remains shared only within this exact world.
                         (when (identical? (ec/home-context root)
                                           (ec/home-context ec/*execution-context*))
                           (offer! owned now)))))
          (let [worker (.submit ^ExecutorService executor
                                ^Runnable
                                (reify Runnable
                                  (run [_]
                                    (binding [ec/*execution-context* ctx
                                              ec/*spin-id* nil
                                              simple/*in-drain?* false]
                                      (try
                                        (case mode
                                          :owned (owned-pass! host :recover nil)
                                          :legacy (schedules/recover! conn ctx))
                                        (deliver ready [:ready])
                                        (loop []
                                          (when (.get ^AtomicBoolean (:accepting owned))
                                            (let [now (.take ^ArrayBlockingQueue (:queue owned))]
                                              (when (.get ^AtomicBoolean (:accepting owned))
                                                (case mode
                                                  :owned (owned-pass! host :tick now)
                                                  :legacy (schedules/tick! conn ctx resolve-room now))))
                                            (recur)))
                                        (catch InterruptedException _
                                          (deliver ready [:failed])
                                          (.interrupt (Thread/currentThread)))
                                        (catch Throwable error
                                          ;; Do not retry an uncertain pass in
                                          ;; this process: stop/recover first.
                                          (.set ^AtomicBoolean (:accepting owned) false)
                                          (deliver ready [:failed])
                                          (log/log! {:level :error :id ::dispatcher-held
                                                     :msg "Workflow scheduling held; stop and recover before restarting"
                                                     :data {:exception-class (.getName (class error))}})
                                          (throw error)))))))]
            (swap! service assoc :future worker)
            (let [outcome (deref ready (:startup-ms policy) [:timeout])]
              (when-not (= [:ready] outcome)
                (.set ^AtomicBoolean (:accepting owned) false)
                (throw (ex-info "Workflow schedule recovery did not complete" {:type ::held :outcome (first outcome)})))
              (swap! service assoc :ready? true)
              ;; Seed a pass even if the clock has not changed since boot.
              (offer! owned (clock/minute-now))
              :started))
          (catch Throwable error
            (.set ^AtomicBoolean (:accepting owned) false)
            (throw error)))))))

(defn start!
  "Legacy startup. Shares the single physical service slot with start-owned!;
   changing mode or owner requires an explicit physically joined stop."
  [conn ctx resolve-room]
  (start-service! conn ctx resolve-room :legacy nil))

(defn start-owned!
  "Fixed HOST startup over the original canonical owner. Validates
   current ownership even on an idempotent start. No selected fallback, caller
   resolver, new clock or parallel dispatcher; no startup installation here."
  [host]
  (off-engine!)
  (original-operation!
   host #(do (reads/current-host-control-db! host)
             (let [{:keys [control-conn execution-ctx]} (::reads/owner host)]
               (start-service! control-conn execution-ctx nil :owned host)))))

(defn stop!
  "Unsubscribe, interrupt and physically join before releasing ownership. A
   failed join retains every handle and prevents a replacement dispatcher.
   This stops admission only; existing Jobs retain their own lifecycle."
  []
  (off-engine!)
  (locking service
    (when-let [{:keys [root signal watch-key accepting executor]} @service]
      (.set ^AtomicBoolean accepting false)
      (binding [ec/*execution-context* root] (remove-watch signal watch-key))
      (.shutdownNow ^ExecutorService executor)
      (when-not (.awaitTermination ^ExecutorService executor (:shutdown-ms policy) TimeUnit/MILLISECONDS)
        (throw (ex-info "Workflow scheduling worker has not physically stopped" {:type ::held})))
      (reset! service nil)))
  :stopped)
