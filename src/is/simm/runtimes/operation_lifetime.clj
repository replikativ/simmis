(ns is.simm.runtimes.operation-lifetime
  "Native installation ownership, not forkable semantic state or an executor.
   Bodies must finish all native access before returning resource-independent
   results. Completion never needs the application installation monitor."
  (:require [org.replikativ.spindel.engine.impl.simple :as simple]))

(def ^:private capacity 64)
(def ^:dynamic *ticket* nil)
(def ^:dynamic ^:private *tickets* [])

(defn- refuse! [reason]
  (throw (ex-info "Application operation lifetime refused" {:type ::refused :reason reason})))

(defn- off-engine! []
  (when (simple/on-drain-thread?) (refuse! :engine-thread)))

(defn create [] {::state (atom {:phase :installing :tickets {}})})

(defn ready? [owner] (= :ready (:phase @(::state owner))))

(defn retained?
  "HOST writer-side liveness observation of one captured native ticket. This
   does NOT admit a calling thread or permit user computation: check-current!
   remains the actual-thread gate. Cleanup may remain retained after fencing;
   new admission must additionally check ready? on this same owner."
  [ticket]
  (when-let [state (::state (:owner ticket))]
    (locking state
      (and (identical? ticket (get-in @state [:tickets (:id ticket)]))
           (not (realized? (:completion ticket)))))))

(defn check-current!
  "Actual-thread admission check before entering new user computation after
   waiting on its interpreter gate. A previously admitted waiter cannot reuse
   a heap after failed cleanup fenced this original installation."
  []
  (off-engine!)
  (let [{:keys [owner thread id] :as ticket} *ticket* state (::state owner)]
    (when-not (and state (identical? thread (Thread/currentThread))) (refuse! :not-owner))
    (locking state
      (when-not (and (= :ready (:phase @state))
                     (identical? ticket (get-in @state [:tickets id])))
        (refuse! :not-ready))))
  nil)

(defn call-detached
  "Construct long-lived HOST computation without inheriting App tickets.
   F receives a short-lived WITH-OPERATION! function for synchronous native
   handoff on this same thread. It restores and checks every original ticket,
   then detaches again before F constructs Spins/futures. It cannot outlive
   this call, transfer ownership, or admit a different thread. Return only
   resource-independent handoff results; never save its callback in a Job."
  [f]
  (let [extent (volatile! {:ticket *ticket* :tickets *tickets* :thread (Thread/currentThread)})
        with-operation! (fn [handoff]
                          (let [{:keys [ticket tickets thread] :as saved} @extent]
                            (when-not (and saved (identical? thread (Thread/currentThread)))
                              (refuse! :detached-handoff-expired))
                            (binding [*ticket* ticket *tickets* tickets]
                              (when ticket (check-current!))
                              (doseq [original tickets]
                                (binding [*ticket* original] (check-current!)))
                              (handoff))))]
    (try
      (binding [*ticket* nil *tickets* []] (f with-operation!))
      ;; An accidentally retained, expired callback keeps only this empty
      ;; native cell, never the original App/outer-ticket graph.
      (finally (vreset! extent nil)))))

(defn open!
  "An owner can be opened once, only after installation. Never resurrect it."
  [owner]
  (off-engine!)
  (let [state (::state owner)]
    (locking state
      (when-not (= :installing (:phase @state)) (refuse! :cannot-open))
      (swap! state assoc :phase :ready)))
  nil)

(defn- assert-not-owner-in! [state]
  (when (some #(or (and (identical? (Thread/currentThread) (:thread %))
                        (or (nil? (:origin-completion %))
                            (not (realized? (:origin-completion %)))))
                   (when-let [active (:cleanup-thread %)]
                     (identical? (Thread/currentThread) @active)))
              (vals (:tickets state)))
    (refuse! :self-drain)))

(defn retain-operation-cleanup!
  "Transfer this actual worker's exact ticket slot to recoverable cleanup.
   cleanup! takes the remaining join milliseconds and must release only its
   captured original resources. It must not reenter application admission or
   lifecycle. Called while owned, even after fencing; no new capacity is needed.
   The original body ceases access to the transferred resources. Transfer fences
   independent application admission: unconfirmed cleanup cannot leave another
   Room reusing a shared interpreter. Recovery is explicit stop/restart."
  [cleanup!]
  (off-engine!)
  (when-not (fn? cleanup!) (refuse! :invalid-cleanup))
  (let [{:keys [owner thread id] :as ticket} *ticket*
        state (::state owner)]
    (when-not (and state (identical? thread (Thread/currentThread)))
      (refuse! :not-owner))
    (locking state
      (when-not (and (identical? ticket (get-in @state [:tickets id]))
                     (nil? (:origin-completion ticket))) (refuse! :not-owner))
      (let [recovery (assoc ticket :origin-completion (:completion ticket)
                            :completion (promise) :cleanup! cleanup!
                            :cleanup-monitor (Object.) :cleanup-thread (atom nil))]
        (swap! state #(-> % (assoc :phase :stopping) (assoc-in [:tickets id] recovery)))
        recovery))))

(defn- join-ticket! [owner ticket deadline]
  (let [remaining #(max 0 (quot (- deadline (System/nanoTime)) 1000000))
        state (::state owner)]
    (when-let [origin (:origin-completion ticket)]
      (when-not (true? (deref origin (remaining) nil)) (refuse! :physical-exit-unconfirmed))
      ;; A release may run user cleanup. Never hold the installation/state
      ;; monitor; serialize retries only on this original recovery ticket.
      (locking (:cleanup-monitor ticket)
        (when-not (realized? (:completion ticket))
          (reset! (:cleanup-thread ticket) (Thread/currentThread))
          (try
            ((:cleanup! ticket) (remaining))
            (locking state
              (when (identical? ticket (get-in @state [:tickets (:id ticket)]))
                (swap! state update :tickets dissoc (:id ticket)))
              (deliver (:completion ticket) true))
            (finally (reset! (:cleanup-thread ticket) nil))))))
    (when-not (true? (deref (:completion ticket) (remaining) nil))
      (refuse! :physical-exit-unconfirmed))))

(defn assert-not-owner!
  "Check every live ticket, including outer nested owners, before taking an
   application lifecycle coordinator or changing admission state."
  [owner]
  (let [state (::state owner)]
    (locking state (assert-not-owner-in! @state)))
  nil)

(defn fence!
  "Close independent admission permanently. Already-admitted same-thread
   nesting may finish; physical completion is a separate joined boundary."
  [owner]
  (off-engine!)
  (let [state (::state owner)]
    (locking state
      (assert-not-owner-in! @state)
      (swap! state assoc :phase :stopping)))
  nil)

(defn join!
  "Join physical exits within one deadline AFTER fencing. Failure retains the
   stopped owner and its tickets; callers must retain their host/root for retry.
   Waiting releases the owner monitor; completion must not need an application
   installation or lifecycle coordinator monitor."
  [owner timeout-ms]
  (off-engine!)
  (when-not (and (integer? timeout-ms) (<= 0 timeout-ms 60000)) (refuse! :invalid-deadline))
  (let [state (::state owner)
        deadline (+ (System/nanoTime) (* 1000000 timeout-ms))]
    (loop []
      (let [tickets (locking state
                      (assert-not-owner-in! @state)
                      (when-not (= :stopping (:phase @state)) (refuse! :not-fenced))
                      (vals (:tickets @state)))]
        (when (seq tickets)
          (doseq [ticket tickets] (join-ticket! owner ticket deadline))
          ;; An active worker may transfer its original slot AFTER this
          ;; snapshot. Its old ACK isn't the replacement cleanup's ACK.
          ;; Re-read by identity until the fenced registry is empty.
          (recur)))))
  nil)

(defn with-operation!
  "Own a synchronous operation against THIS exact installation, never the
   current replacement. A Thread-inherited dynamic binding is not ownership.
   No cancellation is inferred to be a physical-exit acknowledgement."
  [owner f]
  (off-engine!)
  (when-not (fn? f) (refuse! :invalid-body))
  (let [state (::state owner)
        thread (Thread/currentThread)
        acquisition
        (locking state
          (if-let [ticket (some #(when (and (identical? owner (:owner %))
                                            (identical? thread (:thread %))
                                            (identical? % (get-in @state [:tickets (:id %)]))) %)
                                (rseq *tickets*))]
            {:ticket ticket :nested? true}
            (do
              (when-not (= :ready (:phase @state)) (refuse! :not-ready))
              (when (>= (count (:tickets @state)) capacity) (refuse! :capacity-exhausted))
              (let [ticket {:id (random-uuid) :owner owner :thread thread :completion (promise)}]
                (swap! state assoc-in [:tickets (:id ticket)] ticket)
                {:ticket ticket :nested? false}))))
        {:keys [ticket nested?]} acquisition]
    (binding [*ticket* ticket *tickets* (conj *tickets* ticket)]
      (if nested?
        (f)
        (try (f)
             (finally
               (locking state
                 (try
                   (when (identical? ticket (get-in @state [:tickets (:id ticket)]))
                     (swap! state update :tickets dissoc (:id ticket)))
                   ;; Join observes removal and final ACK in one monitor interval.
                   ;; Body/native resource access has already ended.
                   (finally (deliver (:completion ticket) true))))))))))
