(ns is.simm.agents.tool-authorization
  "Simmis's ReBAC gate for Dvergr tools.

   Dvergr decides which tool definitions an AgentDef receives. This namespace
   adds the independent application-object decision: the acting party must
   still be allowed to perform the tool's conservative action on the room when
   the call actually executes. Removing an agent from a room therefore revokes
   its room tools without rebuilding the participant or its prompt.

   `access/can?` is deliberately the seam, rather than duplicating its current
   Datahike traversal here. It can move to EACL's IAuthorization implementation
   without changing the Dvergr tool contract."
  (:require [is.simm.model.access :as access]))

(def ^:private read-tools
  #{"read_file" "grep" "glob" "clj_kondo" "knowledge_search"})

(defn room-tool-action
  "The least room permission sufficient for a tool. Unknown tools are treated
   as writes: under-classifying a newly added effect would fail open."
  [tool-name]
  (if (contains? read-tools tool-name) :read :write))

(defn room-decision
  "Return the durable, backend-neutral authorization receipt for one room tool.

   The two sources are independent gates. `:agent-tool-grant` means Dvergr
   admitted the named tool into this agent's executable map; `:simmis-rebac`
   means Simmis admitted the acting party on the target room. Kontor will add a
   third source and grant id for resource-consuming effects."
  [party-id room-id tool-name]
  (let [action (room-tool-action tool-name)
        authorized? (access/can? party-id action {:room room-id})]
    (cond-> {:decision (if authorized? :authorized :denied)
             :sources #{:agent-tool-grant :simmis-rebac}
             :subject-type :party
             :subject-id (str party-id)
             :action action
             :resource-type :room
             :resource-id (str room-id)}
      (not authorized?)
      (assoc :reason (str "Party " party-id " may not " (name action)
                          " room " room-id)))))

(defn authorize-room-tools
  "Attach a late-bound ReBAC authorizer to every Dvergr tool definition."
  [tool-map party-id room-id]
  (into {}
        (map (fn [[tool-name tool]]
               [tool-name
                (assoc tool :authorize
                       (fn [_input _tool-ctx]
                         (room-decision party-id room-id tool-name)))]))
        tool-map))
