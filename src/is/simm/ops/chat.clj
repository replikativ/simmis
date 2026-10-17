(ns is.simm.ops.chat
  "Canonical human chat dispatch, shared by browser and external clients.
   Ordinary conversation uses the room's existing agent budgets, not the
   bounded isolated-work admission API."
  (:require [datahike.api :as d]
            [is.simm.agents.room-agents :as agents]
            [is.simm.model.access :as access]
            [is.simm.model.parties :as parties]
            [is.simm.model.room-databases :as room-dbs]
            [is.simm.model.rooms :as rooms]
            [is.simm.model.system-db :as system-db]
            [malli.core :as m]))

(def schema
  [:map {:closed true}
   [:room :uuid] [:id :uuid] [:content [:string {:min 1 :max 16000}]]
   [:in-reply-to {:optional true} [:maybe :uuid]]])

(defn ensure-room-chatroom! [conn room-id room-name]
  (when-not (d/q '[:find ?e . :in $ ?uuid :where [?e :entity/uuid ?uuid]] @conn room-id)
    (d/transact conn [{:entity/uuid room-id :entity/created-at (java.util.Date.)
                       :entity/name (str "S.ChatRoom/" room-name)
                       :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-00000000002a"]
                       :S.ChatRoom/name room-name}])))

(defn ensure-party-projection! [conn party-id]
  (when-not (d/q '[:find ?e . :in $ ?uuid :where [?e :entity/uuid ?uuid]] @conn party-id)
    (when-let [party (parties/get-party party-id)]
      (d/transact conn
                  [(cond-> {:entity/uuid party-id :entity/created-at (java.util.Date.)
                            :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-000000000029"]
                            :S.User/display-name (or (:party/display-name party) "Unknown")}
                     (:party/handle party) (assoc :S.User/handle (:party/handle party))
                     (:party/email party) (assoc :S.User/email (:party/email party))
                     (= :agent (:party/type party)) (assoc :S.User/is-ai true))]))))

(defn post!
  ([principal input] (post! principal input :web))
  ([principal {:keys [room id content in-reply-to] :as input} source]
   (when-not (m/validate schema input)
     (throw (ex-info "Invalid chat message" {:type ::invalid})))
   (let [party (access/actor->party-uuid (:principal/id principal))]
     (when-not (and party (access/can? @(system-db/get-conn) party :write {:room room}))
       (throw (ex-info "Room write permission required" {:type ::denied})))
     (let [info (rooms/get-room room)
           conn (some-> (:room/content-db-scope info) room-dbs/connect-room-database)]
       (when-not conn
         (throw (ex-info "Room content store is unavailable" {:type ::unavailable})))
       (ensure-room-chatroom! conn room (or (:room/name info) "Chat"))
       (ensure-party-projection! conn party)
       (agents/post-user-message! room content party conn id in-reply-to source)))))
