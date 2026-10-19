(ns is.simm.ops.knowledge-admission
  "Host-only default workspace binding. Never attachment-order fallback."
  (:require [dvergr.system.rooms :as rooms]
            [is.simm.agents.room-agents :as room-agents]))

(defn default-target
  "Resolve after room authorization. Reject a stale/forked runtime Room before
   reading any knowledge files. The returned connection is the only source of
   the admission snapshot; retain identity data, never these live handles."
  [room-id room]
  (when-not (and (uuid? room-id)
                 (identical? room (room-agents/registered-room room-id))
                 (identical? (:ctx room) (rooms/room-ctx-for room-id)))
    (throw (ex-info "Knowledge admission requires the canonical live room"
                    {:type ::unavailable})))
  (let [{:keys [conn registry-system runtime-system]} (rooms/default-workspace room-id)]
    {:conn conn :identity {:registry-system registry-system :runtime-system runtime-system}}))
