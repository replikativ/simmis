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

(defn- post-proposal! [{:proposal/keys [id room message-id] :as proposal} live-room]
  (let [message (binding [rtc/*execution-context* (:ctx live-room)]
                  (-> (discourse/message :simmis/proposals :_room-log (proposal-content proposal) nil
                                         {:role :system :kind :proposal/filed :object {:kind :proposal :id id}})
                      (assoc :id message-id :thread-root-id message-id)))]
    ;; Native post owns enqueue/storage/quiescence ordering. A preparation
    ;; lease here could refuse a subscriber's fork after durable visibility.
    (binding [rtc/*execution-context* (:ctx live-room)] (discourse/post! live-room message))
    (proposals/mark-message-published! id)
    (log/log! {:level :info :id ::published :data {:proposal id :room room :message message-id}})
    :published))

(defn- pending! [id error]
  (proposals/mark-message-publication-failed! id error)
  (log/log! {:level :warn :id ::publication-failed :data {:proposal id :error error}})
  :pending)

(defn- pending-error! [id error & [propagate-interruption?]]
  (let [interrupted? (or (.isInterrupted (Thread/currentThread))
                         (loop [cause error seen #{}]
                           (cond (nil? cause) false
                                 (instance? InterruptedException cause) true
                                 (contains? seen cause) false
                                 :else (recur (.getCause ^Throwable cause) (conj seen cause)))))]
    (when interrupted? (.interrupt (Thread/currentThread)))
    ;; The existing durable publication intent is already pending. Do not
    ;; turn cancellation of an owned native post into a normal return value.
    (if (and propagate-interruption? interrupted?)
      (throw error)
      (pending! id (ex-message error)))))

(defn publish!
  "Publish proposal-id's reserved room message, or leave it durably pending.
   This convenience entry may resolve the current logical room. Owned native
   operations use publish-original! to preserve their captured incarnation."
  [proposal-id]
  (if-let [{:proposal/keys [id room message-id message-status] :as proposal}
           (proposals/get-proposal proposal-id)]
    (cond
      (= :published message-status) :published
      (or (nil? room) (nil? message-id)) :missing
      :else (try
              (if-let [live-room (room-agents/live-room room)]
                (post-proposal! proposal live-room)
                (pending! id "originating room is not available"))
              (catch Throwable error (pending-error! id error))))
    :missing))

(defn publish-original!
  "Fixed HOST notification to the already captured Room. No hydration or
   adoption of a replacement incarnation. Native post refuses a fenced Room."
  [proposal-id original-room]
  (if-let [{:proposal/keys [id room message-id message-status] :as proposal}
           (proposals/get-proposal proposal-id)]
    (cond
      (= :published message-status) :published
      (or (nil? room) (nil? message-id)) :missing
      (or (not= room (:room-id original-room)) (nil? (:room original-room)))
      (pending! id "original room binding is unavailable")
      :else (try (post-proposal! proposal (:room original-room))
                 (catch Throwable error (pending-error! id error true))))
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
