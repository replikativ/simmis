(ns is.simm.uis.web.desktop.perspectives-sync
  "Keep the perspectives live, so none of them needs a Refresh button.

   Feed, Tasks, Accounting and Proposals are server-side aggregates fetched by
   RPC, which made them stale the moment anything changed and left each view
   with a button asking the user to do the system's job. The reactive pattern
   already existed here — `user-rooms-sync`, `message-notify-sync` and
   `branching-sync` all invalidate on a kabel pubsub nudge — it had simply not
   been applied to the newer views.

   Events, from the topic that already carries them:
     :branch/tx-occurred               a KB changed        → Feed, Tasks
     :branch/created|merged|discarded  a fork branch moved → Feed, Tasks, Proposals
     :proposal/tx-occurred             a ForkSet was filed
                                       or resolved         → Feed, Tasks, Proposals
     :book/tx-occurred                 a room's book moved → Accounting

   This namespace holds NO timing logic. Coalescing belongs to the gate beside
   each fetch (`uis.desktop.aggregate`); an earlier version guarded here as
   well, and the two guards defeated each other — the view's own flag made a
   dropped fetch look completed, so a write arriving mid-fetch was never
   reflected. Asking for a refresh is all this does.

   It registers with `branching-sync` rather than opening a second subscription:
   `:branching/event` already has an owner, and subscribing twice installs a
   competing strategy for one topic instead of adding a listener."
  (:require [org.replikativ.spindel.engine.core :as rtc]
            [is.simm.uis.web.desktop.branching-sync :as br-sync]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.views.feed :as feed]
            [is.simm.uis.web.desktop.views.tasks :as tasks]
            [is.simm.uis.web.desktop.views.accounting :as accounting]
            [is.simm.uis.web.desktop.views.book :as book]
            [is.simm.uis.web.desktop.views.history-subway :as subway]
            [is.simm.uis.web.desktop.views.proposals :as proposals]))

(defn- refresh-tasks! []
  (tasks/load-tasks! (boolean (:include-done? @sig/tasks-data))))

(defn- on-event! [{:keys [type db-scope]}]
  (binding [rtc/*execution-context* runtime]
    ;; The per-system subway is the one aggregate still fed by RPC — the
    ;; Timelines rail derives its past from the local replicas and needs no
    ;; nudge. Any branch event can move a graph, so this sits outside the case.
    (when db-scope (subway/invalidate! db-scope))
    (case type
      :book/tx-occurred
      (do (accounting/load-position!)
          (book/reload-all!))

      ;; A ForkSet moving changes what Tasks lists AND produces a Feed row
      ;; (`ops.feed` emits :proposal-filed / :proposal-resolved), so Feed belongs
      ;; here too — it was missing, and those rows stayed stale until an
      ;; unrelated KB write happened to nudge them.
      (:branch/created :branch/merged :branch/discarded)
      (do (refresh-tasks!)
          (proposals/load-proposals!)
          (feed/load-feed!))

      ;; A ForkSet's BRANCHES are created when the agent opens its overlay;
      ;; the ForkSet itself is filed later, and only this event says so. Same
      ;; three views: a filing produces a Task row, a Feed row and an inbox
      ;; card, and before this existed all three could be minutes stale.
      :proposal/tx-occurred
      (do (refresh-tasks!)
          (proposals/load-proposals!)
          (feed/load-feed!))

      :branch/tx-occurred
      (do (feed/load-feed!)
          (refresh-tasks!))

      ;; an unknown type is not an error — the topic is shared and other
      ;; producers may add kinds; ignoring beats logging on every one
      nil)))

(defn install!
  "Register with `branching-sync`, which owns the ONE subscription to the topic.
   Idempotent — registration is keyed."
  []
  (br-sync/add-listener! ::perspectives on-event!)
  (js/console.log "[perspectives-sync] listening for aggregate invalidations"))
