(ns is.simm.model.run-broadcast
  "Room-scoped projection of Dvergr Run lifecycle events onto Kabel Pub/Sub.

   Dvergr identifies a live Room by its slug keyword. Simmis clients identify
   that same room by UUID. This namespace is the single translation boundary;
   clients never need a slug registry and never receive another room's Runs."
  (:require [datahike.api :as d]
            [dvergr.agent.run :as run]
            [is.simm.agents.room-agents :as room-agents]
            [is.simm.model.parties :as parties]
            [is.simm.model.system-db :as system-db]
            [kabel.pubsub :as pubsub]
            [kabel.pubsub.protocol :as proto]
            [taoensso.telemere :as log]))

(def ^:private watcher-key ::publisher)
(defonce ^:private peer-ref (atom nil))

(defn run-topic
  "Private room lifecycle topic. Subscription authorization is enforced in
   `is.simm.runtimes.web/data-plane-authorized?`."
  [room-id]
  (keyword "runs" (str room-id)))

(defn run-topic-room
  "Parse the Simmis room UUID from a Run topic, or nil for another/malformed
   topic. Kept public so the data-plane authorization gate can delegate here."
  [topic]
  (when (and (keyword? topic) (= "runs" (namespace topic)))
    (try (java.util.UUID/fromString (name topic))
         (catch Exception _ nil))))

(defn- runtime-room->room-id [runtime-room]
  (when-let [conn (system-db/get-conn)]
    (d/q '[:find ?rid .
           :in $ ?slug
           :where
           [?r :room/slug ?slug]
           [?r :room/id ?rid]]
         @conn (name runtime-room))))

(defn ensure-topic-registered!
  "Register a room Run topic on the current peer. Idempotent."
  [room-id]
  (when-let [peer @peer-ref]
    (let [topic (run-topic room-id)]
      (when-not (pubsub/topic-registered? peer topic)
        (pubsub/register-topic! peer topic
                                {:strategy (proto/pub-sub-only-strategy nil)}))
      topic)))

(defn- actor-name [actor]
  (or (some-> actor room-agents/actor-kw->party-uuid parties/get-party
              :party/display-name)
      (some-> actor name)))

(defn run-summary
  "Serializable, UI-sized projection. Live ChatContexts and cancellation
   handles never cross this boundary."
  [room-id r]
  (cond-> {:id (str (:run/id r))
           :kind (:run/kind r)
           :room-id (str room-id)
           :actor (:run/actor r)
           :actor-name (actor-name (:run/actor r))
           :trigger-id (str (:run/trigger r))
           :status (:run/status r)
           :world-id (some-> (:run/world r) name)
           :isolation (:run/isolation r)
           :settlement-policy (:run/settlement-policy r)
           :settlement-status (:run/settlement-status r)
           :settlement-reason (:run/settlement-reason r)
           :created-at (some-> ^java.util.Date (:run/created-at r) .getTime)
           :started-at (some-> ^java.util.Date (:run/started-at r) .getTime)
           :updated-at (some-> ^java.util.Date (:run/updated-at r) .getTime)}
    (:run/parent r) (assoc :parent-id (str (:run/parent r)))
    (:run/ended-at r) (assoc :ended-at (.getTime ^java.util.Date (:run/ended-at r)))
    (:run/reason r) (assoc :reason (:run/reason r))
    (:run/error r) (assoc :error (str (:run/error r)))))

(defn room-snapshot
  "Current live Runs plus bounded durable history for a Simmis room."
  ([room-id] (room-snapshot room-id 24))
  ([room-id limit]
   (if-let [room (room-agents/live-room room-id)]
     {:room-id (str room-id)
      :active (mapv #(run-summary room-id %) (run/active-runs (:id room)))
      :recent (mapv #(run-summary room-id %) (run/runs room {:limit limit}))}
     {:room-id (str room-id) :active [] :recent []})))

(defn publish-run!
  "Publish one authoritative durable Run projection to every subscribed room
   client. Lifecycle changes normally arrive through the Dvergr watcher; later
   human settlement decisions use this explicit bridge because they happen
   after the execution lifecycle has finished."
  [room-id type r]
  (try
    (when-let [topic (ensure-topic-registered! room-id)]
      (pubsub/publish! @peer-ref topic
                       {:type type
                        :room-id (str room-id)
                        :run (run-summary room-id r)}))
    (catch Throwable t
      (log/log! {:level :warn :id ::publish-failed
                 :data {:event-type type :error (ex-message t)}}
                "Failed to publish Run event"))))

(defn- publish-event! [{:keys [type run] :as event}]
  (try
    ;; The installation snapshot usually precedes client subscriptions. Initial
    ;; state is therefore served by room-snapshot; ordered deltas begin here.
    (when (and run (not= type :runs/snapshot))
      (when-let [room-id (runtime-room->room-id (:run/room run))]
        (publish-run! room-id type run)))
    (catch Throwable t
      (log/log! {:level :warn :id ::publish-failed
                 :data {:event-type type :error (ex-message t)}}
                "Failed to publish Run lifecycle event"))))

;; Dvergr invokes watchers while holding its lifecycle frontier lock. Queue the
;; application broadcast on one ordered agent so database lookup or a slow
;; network peer can never hold that execution lock. The agent is transport
;; machinery, not application state; Run projections themselves live durably
;; in Dvergr and reactively in the client's Spindel execution context.
(defonce ^:private publisher (agent nil))

(defn install!
  "Install the single Dvergr lifecycle watcher and register existing rooms.
   Safe across dev reloads and server restarts."
  [peer]
  (reset! peer-ref peer)
  (when-let [conn (system-db/get-conn)]
    (doseq [room-id (d/q '[:find [?rid ...] :where [_ :room/id ?rid]] @conn)]
      (ensure-topic-registered! room-id)))
  (run/watch-runs! watcher-key
                   (fn [event]
                     (send-off publisher (fn [_] (publish-event! event) nil))))
  (log/log! {:level :info :id ::installed :msg "Run lifecycle broadcaster installed"})
  true)

(defn uninstall! []
  (run/unwatch-runs! watcher-key)
  (reset! peer-ref nil)
  nil)
