(ns is.simm.agents.runs
  "Who started a Run, and so who may stop it (doc/authority.md, \"Runs\").

   A dvergr Run records the agent that runs (`:run/actor`) and what triggered
   it (`:run/trigger`, a message id), but not the principal who caused it. The
   starter is derived from what the Run does record, in `run-starter`; whether
   a principal may stop it is `access/may-cancel-run?`."
  (:require [dvergr.agent.run :as agent-run]
            [dvergr.jobs :as jobs]
            [dvergr.room.store :as rstore]
            [is.simm.model.access :as access]
            [org.replikativ.spindel.engine.core :as rtc]))

(defn- trigger-message
  "The message `trigger-id` in live Room `room`'s store, or nil (no store, or no
   such message: a job's trigger is a fresh uuid that names no message)."
  [room trigger-id]
  (when-let [st (and trigger-id (:store room))]
    (binding [rtc/*execution-context* (or (:ctx room) rtc/*execution-context*)]
      (first (rstore/-list-messages st (rstore/conversation-id room)
                                    {:message-ids [trigger-id]})))))

(defn run-starter
  "The party uuid of whoever started Run `run` (a Run map, live snapshot or
   durable) of live Room `room`, or nil when nobody determinable did:

   - triggered by a message: that message's author — its logical initiator
     when it carries one (`:metadata :from`, delegated work), else `:from`. A
     human's message names the human; an agent's names the agent party (so
     only the room owner stops a Run one agent set off in another).
   - a scheduled Run: the trigger is the scheduler's message (`:from
     :scheduler`); dvergr's schedule row records no owner, so nil.
   - a job (`:run/kind` in `dvergr.jobs/kinds`): dvergr records the job's
     starter as the Run's actor (`jobs/start! :actor`), so that party — nil for
     `:dvergr`, the process.
   - anything else (the process started it, the trigger is not in the store):
     nil.

   nil is not an error: such a Run is the room owner's (or an admin's) to
   stop (`access/may-cancel-run?`)."
  [room run]
  (if-let [msg (trigger-message room (:run/trigger run))]
    (access/actor->party-uuid (or (get-in msg [:metadata :from]) (:from msg)))
    (when (contains? jobs/kinds (:run/kind run))
      (access/actor->party-uuid (:run/actor run)))))

(defn live-run
  "The live Run `run-id` of live Room `room` (its snapshot), or nil when it is
   not running there: knowing a Run id grants nothing in another room."
  [room run-id]
  (when room
    (some #(when (= run-id (:run/id %)) %) (agent-run/active-runs (:id room)))))
