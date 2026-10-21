(ns is.simm.uis.web.desktop.branching-sync
  "Client-side: keep `sig/kb-branches` in sync with server-side branch
   lifecycle events via the `:branching/event` kabel.pubsub topic.

   The server (see is.simm.model.branching-broadcast) publishes events of
   the form
     {:type :branch/created | :branch/discarded | :branch/merged
            | :branch/tx-occurred
      :db-scope <uuid-string>
      :branch   <branch-keyword>
      :parent   <branch-keyword>   ; on :created / :merged
      :commit   <commit-id-string> ; on :tx-occurred / :merged
      :author   <party-keyword>    ; future — currently nil
      :at       <inst>}

   We update the `kb-branches` signal accordingly. Branch metadata itself
   travels with the existing kabel store sync — by the time we receive
   the event, the local datahike conn typically already knows about the
   branch via `(d/branches local-conn)`. There is a small race we handle
   defensively in places that consume the local view.

   Client-side filtering: we only react to events for KBs in the current
   `user-rooms` data. Long-term this filter belongs on the server (per-KB
   topics + ACL); for v1 it's a hygiene measure here."
  (:require [org.replikativ.spindel.engine.core :as rtc]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.branching-remote :as br-remote]
            [is.simm.uis.web.desktop.aggregate :as aggregate]
            [is.simm.uis.web.desktop.connection-epoch :as epoch]
            [is.simm.uis.web.desktop.topic-subscription :as subscription]
            [is.simm.runtimes.web :as web]))

(def ^:private topic :branching/event)
(def ^:private owner (subscription/owner))
(def ^:private branch-gates (atom {}))
(declare refresh-kb-branches!)

;; --- helpers ----------------------------------------------------------------

(defn- kb-id-str
  "Canonical form for keying signals — yggdrasil sends the db-scope as a
   string per `branching-broadcast/emit-event!`, so we keep it uniform."
  [v]
  (cond
    (string? v) v
    (uuid? v) (str v)
    :else (str v)))

(defn- ensure-kb-row
  "Ensure `(get kb-branches kb-id-str)` contains a trunk entry. Idempotent."
  [m kb-id]
  (update m kb-id (fnil identity {:db {:parent nil :status :open}})))

(defonce ^:private extra-listeners
  ;; Other subsystems that want the same events. ONE subscription per topic:
  ;; a second `pubsub/subscribe!` for `:branching/event` would install a
  ;; competing strategy for the same topic rather than adding a listener, so
  ;; consumers register here instead. Kept as an atom of fns so no namespace
  ;; below this one has to be required from it (perspectives-sync pulls in the
  ;; views; branching-sync must not).
  (atom {}))

(defn add-listener!
  "Register `f` (called with each event) under `k`. Idempotent per key."
  [k f]
  (swap! extra-listeners assoc k f))

(defn- notify-extras! [event]
  (doseq [[k f] @extra-listeners]
    (try (f event)
         (catch :default e
           (js/console.warn "[branching-sync] listener failed:" (str k) e)))))

(defn- apply-event!
  "Mutate the kb-branches signal in response to one pubsub event. Filters
   by KB-access via the snapshot of `sig/user-rooms`; ignores events for
   KBs the user can't see."
  [event]
  (binding [rtc/*execution-context* runtime]
    ;; Extras first, and OUTSIDE the KB-roster filter below: a
    ;; `:book/tx-occurred` carries a ROOM scope, which is not in the KB list
    ;; and would be dropped by a filter written for branch bookkeeping.
    (notify-extras! event)
    (let [kb-id (kb-id-str (:db-scope event))
          ;; Filter: is this KB in our roster?
          ur @sig/user-rooms
          kbs (when (map? ur) (:knowledge-bases ur))
          accessible? (some #(= (kb-id-str (:kb/db-scope %)) kb-id) kbs)]
      (when accessible?
        (case (:type event)
          :branch/created
          (swap! sig/kb-branches
                 (fn [m]
                   (-> m
                       (ensure-kb-row kb-id)
                       (assoc-in [kb-id (:branch event)]
                                 {:parent (:parent event)
                                  :author (:author event)
                                  :created-at (:at event)
                                  :last-tx-at (:at event)
                                  :status :open}))))

          :branch/discarded
          (swap! sig/kb-branches
                 (fn [m]
                   (-> m
                       (ensure-kb-row kb-id)
                       (assoc-in [kb-id (:branch event) :status] :discarded))))

          :branch/merged
          (swap! sig/kb-branches
                 (fn [m]
                   (-> m
                       (ensure-kb-row kb-id)
                       (assoc-in [kb-id (:source event) :status] :merged)
                       (assoc-in [kb-id (:branch event) :last-tx-at] (:at event)))))

          :branch/tx-occurred
          (swap! sig/kb-branches
                 (fn [m]
                   (-> m
                       (ensure-kb-row kb-id)
                       (update-in [kb-id (:branch event)]
                                  (fn [b]
                                    (assoc (or b {:parent nil :status :open})
                                           :last-tx-at (:at event)))))))

          ;; Unknown event type — log + skip.
          (js/console.warn "[branching-sync] unknown event type:"
                           (pr-str (:type event))))
        ;; A snapshot in flight can predate this event. Queue a new snapshot
        ;; rather than allowing its older branch set to erase the event.
        (refresh-kb-branches! kb-id)))))

;; --- initial load -----------------------------------------------------------

(defn refresh-kb-branches!
  "Fetch the current branch set for `db-scope` from the server and seed
   the `kb-branches` signal. Called when a KB first becomes accessible
   (on user-rooms load) so the sidebar tree is populated even before any
   pubsub event arrives.

   Wraps the spin-remote in a runtime context binding because this runs
   from a DOM/load callback outside any spin."
  [db-scope]
  (let [kb-id (kb-id-str db-scope)
        gate (get (swap! branch-gates #(if (contains? % kb-id) % (assoc % kb-id (aggregate/gate)))) kb-id)]
    (aggregate/run!
     gate
     (fn [done current?]
       (binding [rtc/*execution-context* runtime]
         (let [s (br-remote/list-kb-branches! web/server-id kb-id)]
           (s (fn [branches]
                (try
                  (when (and (current?) (sequential? branches))
                    (binding [rtc/*execution-context* runtime]
                      (when (some #(= kb-id (str (:kb/db-scope %))) (:knowledge-bases @sig/user-rooms))
                        ;; Replace, do not union: branches deleted while offline
                        ;; must disappear from the next authoritative snapshot.
                        (swap! sig/kb-branches update kb-id
                               (fn [existing]
                                 (into {} (map (fn [b] [b (assoc (or (get existing b)
                                                                     {:parent (when-not (= b :db) :db)})
                                                                 :status :open)]) branches)))))))
                  (finally (done))))
              (fn [err]
                (try (when (current?) (js/console.error "[branching-sync] branch load failed:" kb-id err))
                     (finally (done)))))))))))

;; --- subscription -----------------------------------------------------------

(defn subscribe!
  "One global topic owner per authenticated epoch. Transport readiness is also
   sent to registered consumers so already-loaded aggregates can catch up."
  []
  (subscription/ensure!
   owner {:connection (epoch/snapshot) :peer (:client (web/current-session)) :topic topic
          :on-message apply-event!
          :on-ready #(notify-extras! {:type :transport/ready})}))
