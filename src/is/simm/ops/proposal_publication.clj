(ns is.simm.ops.proposal-publication
  "Idempotent projection of canonical Proposals into their originating chat.

   The system DB owns publication intent and the reserved message UUID. Dvergr
   owns durability-before-visibility for the room message. A crash between the
   two commits is safe: retrying posts the same immutable message envelope, and
   Dvergr's room store is atomically first-write-wins by message UUID."
  (:require [dvergr.discourse :as discourse]
            [is.simm.agents.room-agents :as room-agents]
            [is.simm.ops.proposals :as proposals]
            [org.replikativ.spindel.engine.core :as rtc]
            [taoensso.telemere :as log]))

(defn- proposal-content
  [{:proposal/keys [title summary]}]
  (str "Proposal ready: " title
       (when (seq summary) (str "\n\n" summary))))

(defn publish!
  "Publish proposal-id's reserved room message, or leave it durably pending.

   Returns :published when complete, :pending when the room is unavailable or
   the post fails, and :missing for an unknown/roomless legacy proposal."
  [proposal-id]
  (if-let [{:proposal/keys [id room message-id message-status] :as proposal}
           (proposals/get-proposal proposal-id)]
    (cond
      (= :published message-status) :published
      (or (nil? room) (nil? message-id)) :missing
      :else
      (try
        (if-let [live-room (room-agents/live-room room)]
          (let [message (binding [rtc/*execution-context* (:ctx live-room)]
                          (-> (discourse/message
                               :simmis/proposals :_room-log
                               (proposal-content proposal) nil
                               {:role :system
                                :kind :proposal/filed
                                :object {:kind :proposal :id id}})
                              (assoc :id message-id
                                     :thread-root-id message-id)))]
            ;; Room post is durability-before-visibility. Only after it returns
            ;; may the system projection say :published.
            (binding [rtc/*execution-context* (:ctx live-room)]
              (discourse/post! live-room message))
            (proposals/mark-message-published! id)
            (log/log! {:level :info :id ::published
                       :data {:proposal id :room room :message message-id}})
            :published)
          (do
            (proposals/mark-message-publication-failed!
             id "originating room is not available")
            :pending))
        (catch Throwable t
          (proposals/mark-message-publication-failed! id (ex-message t))
          (log/log! {:level :warn :id ::publication-failed
                     :data {:proposal id :room room :message message-id
                            :error (ex-message t)}})
          :pending)))
    :missing))

(defn retry-pending!
  "Retry all pending canonical messages, optionally only for room-id."
  ([] (retry-pending! nil))
  ([room-id]
   (->> (proposals/list-proposals)
        (filter #(and (= :pending (:proposal/message-status %))
                      (or (nil? room-id)
                          (= room-id (:proposal/room %)))))
        (mapv (fn [proposal]
                [(:proposal/id proposal)
                 (publish! (:proposal/id proposal))])))))
