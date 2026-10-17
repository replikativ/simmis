(ns is.simm.ops.worker-wait
  "Owned host wait for an external assignment's physical-stop receipt.
   This is a protocol adapter primitive, not a scheduler or execution model."
  (:require [datahike.api :as d]
            [dvergr.agent.run :as run]
            [is.simm.model.workers :as workers])
  (:import [java.util Date]
           [java.util.concurrent ArrayBlockingQueue TimeUnit]))

(defn watch!
  "Register BEFORE publishing an assignment to an external worker. The host
   must await-stop! after publication, and close! only after that acknowledgement
   and its own DB cleanup. Handles are lexical physical resources, never SCI or
   fork-visible application state. cancelled? is a trusted host predicate."
  [conn id {:keys [cancelled? clock cancel!] :or {cancelled? (constantly false) clock #(Date.)}}]
  (let [key (random-uuid) wake (ArrayBlockingQueue. 1)
        interrupted? (volatile! false)
        signal! (fn [& _] (.offer wake true))]
    (d/listen conn key signal!)
    (try
      (run/register-cancel-hook! id key signal!)
      {:conn conn :id id :key key :wake wake :cancelled? cancelled? :clock clock
       :cancel! (or cancel! #(workers/cancel! conn id (clock)))
       :interrupted? interrupted?}
      (catch Throwable error
        (d/unlisten conn key)
        (throw error)))))

(defn- attempt [interrupted? f]
  ;; Cancellation can arrive before the next blocking call. Clear it for
  ;; cleanup DB operations, but never lose the owner's cancellation signal.
  (when (Thread/interrupted) (vreset! interrupted? true))
  (try {:value (f)}
       (catch InterruptedException _
         ;; FutureTask cancellation is not physical completion. Remember it,
         ;; request durable cancellation, then keep the supervisor worker live.
         (vreset! interrupted? true)
         {:interrupted? true})
       (catch Exception error
         (when (or (Thread/interrupted)
                   (some #(instance? InterruptedException %)
                         (take 8 (take-while some? (iterate ex-cause error)))))
           (vreset! interrupted? true))
         {:unavailable? true})))

(defn- await-event! [^ArrayBlockingQueue wake interrupted? remaining-ms]
  (attempt interrupted?
           #(if remaining-ms
              (.poll wake (long (max 1 remaining-ms)) TimeUnit/MILLISECONDS)
              (.take wake))))

(defn await-stop!
  "Reread durable state after every event. Expiry requests cancellation but
   NEVER releases physical ownership. Failed reads/writes hold this wait until
   another DB/cancellation event; they do not invent a stopped remote process.
   A missing assignment remains unresolved unless a trusted same-intent
   cancel! installs a durable withdrawn fence against later publication.

   Returns {:assignment row :cancelled? boolean}. Interrupt status is restored
   by close!, AFTER the owner's remaining cleanup, not before its DB writes."
  [{:keys [conn id wake cancelled? clock cancel! interrupted?]}]
  (loop [cancel? @interrupted?]
    (let [observation (attempt interrupted? #(workers/assignment conn id))
          row (:value observation)
          control (attempt interrupted? cancelled?)
          now (when row (attempt interrupted? clock))
          expiry (:simmis.worker.assignment/expires-at row)
          remaining (when (and expiry (nil? (:simmis.worker.assignment/stop-ack-at row))
                               (instance? Date (:value now)))
                      (- (.getTime ^Date expiry) (.getTime ^Date (:value now))))
          cancel? (boolean (or cancel? @interrupted? (:value control)
                               (:unavailable? observation) (:unavailable? control) (:unavailable? now)
                               (and row (not (instance? Date (:value now))))
                               (and remaining (<= remaining 0))))
          ;; Only this owner thread transacts; listeners/hooks never do.
          cancellation (when (and cancel? (nil? (:simmis.worker.assignment/cancel-at row)))
                         (attempt interrupted? cancel!))
          closed (:value cancellation)
          row (if (= id (:simmis.worker.assignment/id closed)) closed row)]
      (if (:simmis.worker.assignment/stop-ack-at row)
        {:assignment row :cancelled? (boolean (or cancel? @interrupted?
                                                  (:simmis.worker.assignment/cancel-at row)))}
        (let [;; A failed/ambiguous cancellation is reobserved ONCE before
              ;; waiting. The next event, not an expired timeout, retries it.
              reread (when cancellation (attempt interrupted? #(workers/assignment conn id)))
              acknowledged (:value reread)]
          (if (:simmis.worker.assignment/stop-ack-at acknowledged)
            {:assignment acknowledged :cancelled? true}
            (let [deadline (when (and (not cancel?) (not (:unavailable? observation))
                                      (not (:simmis.worker.assignment/cancel-at row))
                                      (= :claimed (:simmis.worker.assignment/state row))
                                      remaining (pos? remaining))
                             remaining)
                  event (await-event! wake interrupted? deadline)]
              (recur (boolean (or cancel? (:interrupted? event)))))))))))

(defn close!
  "Release listener/hook after physical stop and host cleanup; restore any
   remembered interrupt only after those operations. Does not stop a worker."
  [{:keys [conn id key interrupted?]}]
  (try
    (run/unregister-cancel-hook! id key)
    (finally
      (try (d/unlisten conn key)
           (finally (when @interrupted? (.interrupt (Thread/currentThread))))))))
