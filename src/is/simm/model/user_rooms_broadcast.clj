(ns is.simm.model.user-rooms-broadcast
  "Server-side broadcast of `load-rooms!` invalidation hints.

   After any system-DB transaction that affects a party's roster (KBs they
   own/share, contacts, room membership), publishes the set of affected
   party-ids on a single pubsub topic. Clients subscribe to that topic on
   login and re-run `load-rooms!` whenever their own party-id appears in the
   payload.

   Privacy: payload is just a set of party-ids — no roster data is sent over
   the topic. Each client still goes through the existing `load-rooms!` RPC
   to fetch its filtered view."
  (:require [datahike.api :as d]
            [is.simm.model.system-db :as system-db]
            [kabel.pubsub :as pubsub]
            [kabel.pubsub.protocol :as proto]
            [taoensso.telemere :as log]))

(def topic
  "Single topic used for all roster invalidations."
  :user-rooms/dirty)

(def ^:private listener-key ::user-rooms-broadcaster)

(defonce ^:private peer-ref
  ;; Dvergr assignment changes do not transact against the Simmis system DB,
  ;; so callers need a small explicit invalidation path as well.
  (atom nil))

(defn parties-affected-by-tx
  "Walk tx-data; return a set of party-ids whose `load-rooms!` result may
   have changed. Considers KB ownership/sharing, contacts, and room
   membership.

   Schema notes:
   - :kb/owner :kb/shared-with :room/created-by are :db.type/uuid
     (value is the party-id directly).
   - :room/parties (dvergr's attribute) and :party/contacts are :db.type/ref
     (value is an eid; resolve via :party/id)."
  [db-after tx-data]
  (let [eid->pid (fn [eid]
                   (when (integer? eid)
                     (:v (first (d/datoms db-after :eavt eid :party/id)))))
        affected (transient #{})]
    (doseq [{:keys [e a v]} tx-data]
      (case a
        (:kb/owner :kb/shared-with :room/created-by)
        (when (uuid? v) (conj! affected v))

        :room/parties
        (when-let [pid (eid->pid v)] (conj! affected pid))

        :party/contacts
        (do (when-let [pid (eid->pid e)] (conj! affected pid))
            (when-let [pid (eid->pid v)] (conj! affected pid)))

        ;; KB attachment grants: the roster/KB list of every member of the
        ;; grant's room may change. For :grant/room the datom value IS the
        ;; room eid (works for retractions too); for permission changes
        ;; resolve the grant's room via db-after.
        (:grant/room :grant/permission)
        (when-let [room-eid (if (= a :grant/room)
                              v
                              (:v (first (d/datoms db-after :eavt e :grant/room))))]
          (doseq [d (d/datoms db-after :eavt room-eid :room/parties)]
            (when-let [pid (eid->pid (:v d))]
              (conj! affected pid))))

        nil))
    (persistent! affected)))

(defn ensure-topic-registered!
  "Register the user-rooms topic if not already. Idempotent."
  [server-peer]
  (reset! peer-ref server-peer)
  (when-not (pubsub/topic-registered? server-peer topic)
    (pubsub/register-topic! server-peer topic
                            {:strategy (proto/pub-sub-only-strategy nil)})
    (log/log! {:level :info :id ::topic-registered :msg "user-rooms broadcast topic registered"})))

(defn install-listener!
  "Install the system-DB tx listener that publishes affected party-ids.
   Idempotent — safe to call on dev reload."
  [server-peer]
  (ensure-topic-registered! server-peer)
  (when-let [conn (system-db/get-conn)]
    (d/listen conn listener-key
              (fn [{:keys [tx-data db-after]}]
                (let [pids (parties-affected-by-tx db-after tx-data)]
                  (when (seq pids)
                    (log/log! {:level :debug :id ::publishing
                               :msg "Publishing user-rooms invalidation"
                               :data {:party-count (count pids)}})
                    (pubsub/publish! server-peer topic {:party-ids pids})))))
    (log/log! {:level :info :id ::listener-installed
               :msg "user-rooms tx listener installed"})))

(defn notify-parties!
  "Tell connected clients for `party-ids` to reload their room roster.

   This complements the system-DB listener for room-facing state stored in
   another database, currently Dvergr's room-agent assignments. No-op during
   boot before the server peer has registered the topic."
  [party-ids]
  (when-let [peer @peer-ref]
    (let [party-ids (set party-ids)]
      (when (seq party-ids)
        (pubsub/publish! peer topic {:party-ids party-ids})
        (log/log! {:level :debug :id ::publishing-explicit
                   :msg "Publishing explicit user-rooms invalidation"
                   :data {:party-count (count party-ids)}})))))

(defn uninstall-listener!
  "Remove the listener (server shutdown or restart)."
  []
  (when-let [conn (system-db/get-conn)]
    (d/unlisten conn listener-key)))
