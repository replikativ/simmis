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
  (when (some #(identical? (Thread/currentThread) (:thread %)) (vals (:tickets state)))
    (refuse! :self-drain)))

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
        tickets (locking state
                  (assert-not-owner-in! @state)
                  (when-not (= :stopping (:phase @state)) (refuse! :not-fenced))
                  (vals (:tickets @state)))
        deadline (+ (System/nanoTime) (* 1000000 timeout-ms))]
    (doseq [ticket tickets]
      (when-not (true? (deref (:completion ticket)
                              (max 0 (quot (- deadline (System/nanoTime)) 1000000)) nil))
        (refuse! :physical-exit-unconfirmed))))
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
