(ns is.simm.ops.owned-room-operation
  "Internal native lifetime extents for fixed HOST operations. Neither an
   executor nor forkable room state. Callbacks are implementation bodies, never
   client-supplied functions, authority, or permission to cross a world fork."
  (:require [dvergr.runtime.preparation :as preparation]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.runtimes.operation-lifetime :as lifetime]))

(defn with-app! [host f]
  (when (.isInterrupted (Thread/currentThread))
    (throw (InterruptedException. "Owned operation interrupted")))
  (let [with-operation (get-in host [:config :host-lifecycle :with-operation])]
    (when-not (fn? with-operation)
      (throw (ex-info "Owned operation has no original App lifetime" {:type ::held})))
    (with-operation #(do (lifetime/check-current!) (f)))))

(defn with-current!
  "Caller owns its original App operation and captures the Room once. Hold
   physical Room lifetime through f, outside the Room monitor; only the short
   liveness checks take that monitor. Release before provider prep or fork.
   Returning a native handle does not extend its lease into the next phase."
  [host principal handle original-room f]
  (let [check-room! #(do (lifetime/check-current!)
                        (reads/validate-host-room! host principal handle original-room))]
    (check-room!)
    (preparation/with-host-operation!
     (:room original-room) {}
     (fn [lease]
       (let [check! #(do (check-room!)
                         (preparation/with-host-current! lease (constantly true)))]
         (check!)
         (let [result (f)] (check!) result))))))
