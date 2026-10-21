(ns is.simm.ops.knowledge-admission
  "Host-only default workspace binding. Never attachment-order fallback."
  (:require [datahike.api :as d]
            [dvergr.room.registry :as registry]
            [dvergr.room.store :as store]
            [dvergr.system.rooms :as rooms]
            [is.simm.agents.room-agents :as room-agents]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- canonical-room! [room-id room registered]
  (when-not (and (uuid? room-id) (some? room) (some? (:ctx room))
                 (identical? room registered)
                 (identical? (:ctx room) (rooms/room-ctx-for room-id)))
    (throw (ex-info "Knowledge admission requires the canonical live room"
                    {:type ::unavailable}))))

(defn- target-value [{:keys [conn registry-system runtime-system]}]
  {:conn conn :identity {:registry-system registry-system :runtime-system runtime-system}})

(defn default-binding
  "Resolve after room authorization. Reject a stale/forked runtime Room before
   reading any knowledge files. The returned connection is the only source of
   the admission snapshot; retain identity data, never these live handles.
   The explicit DB arity is for serialized filing: room mapping and default
   ownership come from the writer's snapshot, never an ambient connection."
  ([room-id room]
   (when-not (uuid? room-id)
     (throw (ex-info "Knowledge admission requires the canonical live room" {:type ::unavailable})))
   (binding [ec/*execution-context* (:ctx room)]
     (canonical-room! room-id room (room-agents/registered-room room-id))
     (rooms/default-workspace room-id)))
  ([registry-db room-id room]
   (when-not (uuid? room-id)
     (throw (ex-info "Knowledge admission requires the canonical live room" {:type ::unavailable})))
   (binding [ec/*execution-context* (:ctx room)]
     (let [slug (:room/slug (d/entity registry-db [:room/id room-id]))
           registered (when slug (registry/lookup (store/slug->room-id slug)))]
       (canonical-room! room-id room registered)
       (rooms/default-workspace registry-db room-id)))))

(defn default-target
  "Compatibility projection of the host default binding; no new selection.
   Use default-binding when an explicit descriptor consumer needs the exact
   validated SystemIdentity as well as its connection."
  ([room-id room]
   (target-value (default-binding room-id room)))
  ([registry-db room-id room]
   (target-value (default-binding registry-db room-id room))))
