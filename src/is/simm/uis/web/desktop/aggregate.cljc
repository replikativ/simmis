(ns is.simm.uis.web.desktop.aggregate
  "ONE coalescing gate for the perspective aggregates.

   Feed, Tasks, Accounting and Proposals each fetch by RPC into a signal, and
   each needs the same two properties: never run two fetches at once, and never
   lose an invalidation that arrives while one is running.

   Both were implemented — twice, in different places, and the pair defeated
   each other. Each view owned a `loading?` flag that returned early, and
   `perspectives-sync` owned an in-flight/stale pair around the call. The view's
   flag won: a nudge arriving during the fetch that the view itself had started
   hit the early return, which called `done` as though the fetch had landed, so
   the sync cleared its stale bit and the write that caused the nudge was never
   reflected. Two correct-looking guards, one dropped update.

   So the gate lives HERE, once, next to the fetch it guards — and the
   invalidation layer just asks for a refresh without knowing anything about
   timing. No timers: a burst coalesces because the pending bit survives the
   in-flight fetch, and it settles the moment writes stop."
  (:refer-clojure :exclude [run!])
  #?(:cljs (:require [is.simm.uis.web.desktop.connection-epoch :as connection-epoch])))

(defn gate
  "State for one aggregate. `defonce` it beside the signal it fills."
  []
  (atom {}))

(defn run!
  "Run `fetch!` under `g`, coalescing concurrent requests.

   `fetch!` takes [done current?]. Check current? BEFORE writing results or
   starting dependent work, and call done on success/failure. A new epoch can
   supersede a hung request; late or repeated completions cannot open its gate.
   Coalescing retains the LATEST fetch closure (including requested parameters)."
  ([g fetch!]
   #?(:cljs (let [epoch (connection-epoch/snapshot)]
              (when epoch
                (run! g epoch #(connection-epoch/current? epoch) fetch!)))
      :clj (run! g :local (constantly true) fetch!)))
  ([g epoch valid? fetch!]
   (loop []
     (let [old @g
           busy? (and (= epoch (:epoch old)) (:token old))
           token (when-not busy? (random-uuid))
           next (if busy?
                  (assoc old :pending [epoch valid? fetch!])
                  {:epoch epoch :token token :pending nil})]
       (if-not (compare-and-set! g old next)
         (recur)
         (when-not busy?
           (let [current? #(and (= token (:token @g)) (valid?))
                 done (fn []
                        (loop []
                          (let [state @g]
                            (when (= token (:token state))
                              (if-not (compare-and-set! g state (dissoc state :token :pending))
                                (recur)
                                (when-let [[epoch valid? fetch!] (:pending state)]
                                  (when (valid?) (run! g epoch valid? fetch!))))))))]
             (try (fetch! done current?)
                  (catch #?(:clj Throwable :cljs :default) e
                    (done)
                    (throw e))))))))))

(defn reopen!
  "Force the gate open.

   For the one case the pending bit cannot cover: a fetch that never completes
   because the socket dropped mid-invoke, leaving `:in-flight?` set forever.
   Call this on reconnect — real coordination on a real event, not a timeout."
  [g]
  (swap! g dissoc :token :pending))
