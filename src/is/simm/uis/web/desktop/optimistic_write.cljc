(ns is.simm.uis.web.desktop.optimistic-write
  "A browser write: shown at once, made durable only by the server.

   The editor used to write a KB page with `datahike.optimistic/transact!`. That
   is a WRITER-backed overlay entry: the durable half is the conn's own writer,
   which for a client replica is datahike's KabelWriter — a raw
   `datahike.kabel/dispatch` of the transaction to the server. That path does
   not exist here. Since datahike 0.8.1882 the KabelWriter calls
   `kabel.remote/invoke`; simmis composes distributed-scope's middleware, not
   kabel.remote's, on both ends, so the invoke waits for a route that never
   appears — forever, with no frame sent and no error. The edit showed, stayed
   pending, and was gone after a reload. And the server must not serve it
   either: a raw transaction skips `can?` and `block_remote`, which is the
   point of `doc/authority.md`'s refusal of `dispatch`.

   So a write here is a PREDICTION (`opt/predict!`), whose durable half is
   owned by the caller: an RPC through `block_remote`, gated like any other.

   - The RPC answers with the store's head after the write (`:max-tx`). The
     prediction is reconciled once the replica's base reaches that head — the
     replica then holds the write, or whatever superseded it.
   - Sync can be faster than the reply. If the base is already at the head
     when the reply arrives, nothing would ever re-test the prediction, so it
     is dropped then and there.
   - A refusal or failure REJECTS the prediction, so the view returns to what
     the server holds, and `on-failure` tells the user. A write that is not
     acknowledged must never be able to look like one that was.
   - The RPCs of one overlay are SENT IN ORDER, each after the previous one
     answered. The server runs inbound calls concurrently, and the editor's
     writes depend on each other: Enter creates a block that the next keystroke
     edits, Backspace deletes one whose editor then blurs with a last save.
     Sent together, the edit can land before the create (a stray entity
     carrying only `:block/content`) or after the delete (the block back from
     the dead). The writer the replica used to have was serial too."
  (:require [datahike.optimistic :as opt]
            #?(:clj [clojure.core.async :as a :refer [go <!]]
               :cljs [cljs.core.async :as a :refer [<!]]))
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go]])))

(defn caught-up?
  "Does `db` hold the write the server reported at head `max-tx`?"
  [db max-tx]
  (boolean (and max-tx (:max-tx db) (>= (:max-tx db) max-tx))))

(defn- failure? [v]
  (or (nil? v)
      (instance? #?(:clj Throwable :cljs js/Error) v)
      (not (map? v))
      (nil? (:max-tx v))))

(defonce ^:private tails
  ;; overlay -> a channel that closes when the last RPC queued on it has
  ;; answered. One entry per overlay (one per connected KB), replaced on every
  ;; write.
  (atom {}))

(defn- in-order!
  "Run `(send!)` on `overlay`'s queue — after every RPC queued on it before has
   answered — and hand what it yielded to `k`. Returns a channel yielding
   `k`'s value."
  [overlay send! k]
  (let [done (a/promise-chan)
        [old _] (swap-vals! tails assoc overlay done)
        previous (get old overlay)]
    (go
      (when previous (<! previous))
      (k (try (<! (send!))
              (finally (a/close! done)))))))

(defn- as-error [r]
  (if (instance? #?(:clj Throwable :cljs js/Error) r)
    r
    (ex-info "Write was not acknowledged" {:response r})))

(defn send-in-order!
  "Make `(send!)` durable in order with the other writes on `overlay`, with
   nothing predicted: for a write the view can only show once the server has
   it (a schema declaration — the replica cannot apply what it does not yet
   know the type of). `send!` yields `{:max-tx head …}` or an error.

   `on-failure` is called with the failure. Returns a channel yielding the
   result, or nil when it failed."
  [overlay send! {:keys [on-failure]}]
  (in-order! overlay send!
             (fn [r]
               (if (failure? r)
                 (do (when on-failure (on-failure (as-error r)))
                     nil)
                 r))))

(defn predict-remote!
  "Show `tx-data` on `overlay` now and make it durable with `(send!)`, a
   channel-returning RPC that yields `{:max-tx head …}` on success and an error
   (or nil) on failure. `send!` is called after the prediction is visible.

   `on-failure` is called with the failure after the prediction is rejected.

   Returns a channel yielding the RPC's result, or nil when it failed."
  [overlay tx-data send! {:keys [on-failure]}]
  (let [head (atom nil)
        {:keys [ov-id]} (opt/predict! overlay tx-data
                                      (fn [durable-db] (caught-up? durable-db @head)))]
    (in-order! overlay send!
               (fn [r]
                 (if (failure? r)
                   (do (opt/reject! overlay ov-id (as-error r))
                       (when on-failure (on-failure r))
                       nil)
                   (let [t (:max-tx r)]
                     (reset! head t)
                     (opt/ack! overlay ov-id r)
                     ;; The replica may already hold this write: sync can land
                     ;; before the reply, and the predicate is only re-tested
                     ;; on the next base change. The durable outcome is known,
                     ;; so drop the prediction.
                     (when (caught-up? @(:conn overlay) t)
                       (opt/abandon! overlay ov-id :reconciled))
                     r))))))
