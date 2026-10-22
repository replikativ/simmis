(ns is.simm.model.personal-rooms
  "HOST bootstrap of the human owner's default personal workspace. Existing
   rooms are preserved, never converted or reseeded. New rooms are canonical
   before publication; this installs no model allowance or enabled schedule."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [is.simm.agents.templates :as templates]
            [is.simm.documents.space :as space]
            [is.simm.model.knowledge-rooms :as knowledge]
            [is.simm.model.parties :as parties]
            [is.simm.model.rooms :as rooms]
            [is.simm.model.system-db :as system-db]
            [is.simm.runtimes.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.impl.simple :as simple]))

(defn- refuse! [reason]
  (throw (ex-info "Personal workspace needs explicit setup or recovery"
                  {:type ::unavailable :reason reason})))

(defn- existing-id [db owner]
  ;; Membership is not ownership: joining somebody else's personal room must
  ;; never make it the joining person's default workspace.
  (let [ids (vec (take 2 (d/q '[:find [?id ...] :in $ ?owner
                                :where [?p :party/id ?owner] [?r :room/owner ?p]
                                [?r :room/type :personal-ai] [?r :room/id ?id]] db owner)))]
    (when (> (count ids) 1) (refuse! :ambiguous-owned-rooms))
    (when-let [id (first ids)]
      (let [row (d/pull db '[:room/id :room/created-by :room/content-db-scope
                             :room/provisioning-state :room/provisioning-version :room/archived-at
                             {:room/parties [:party/id]}
                             {:room/default-repository [:system/repository-profile]}] [:room/id id])
            state (:room/provisioning-state row)]
        (when-not (and (nil? (:room/archived-at row))
                       (uuid? (:room/content-db-scope row))
                       (some #(= owner (:party/id %)) (:room/parties row))
                       (or (and (nil? state) (nil? (:room/provisioning-version row))
                                (nil? (get-in row [:room/default-repository :system/repository-profile])))
                           (and (= :ready state) (= 1 (:room/provisioning-version row))
                                (= owner (:room/created-by row))
                                (= space/profile (get-in row [:room/default-repository :system/repository-profile])))))
          (refuse! :existing-room-not-ready))
        id))))

(defn ensure!
  "Exclusive HOST account/bootstrap lifecycle, not a transport endpoint.
   New creation records native personal type atomically; uncertain/competing
   intent is not retried. Existing source, defaults and custom workflows remain
   untouched. A pending or ambiguous default is an explicit recovery problem."
  [owner display-name]
  (when (or (simple/on-drain-thread?)
            (and (ec/execution-context-bound?)
                 (not (identical? context/server-context (ec/current-execution-context)))))
    (refuse! :requires-host-root))
  (when-not (uuid? owner) (refuse! :invalid-owner))
  (let [conn (or (system-db/get-conn) (refuse! :system-unavailable))
        db @conn party (d/entity db [:party/id owner])]
    (when-not (and (:db/id party) (= :human (or (:actor/kind party) (:party/type party))))
      (refuse! :invalid-owner))
    (let [room (if-let [id (existing-id db owner)]
                 (rooms/get-room id)
                 (knowledge/create-personal! owner
                                             (if (str/blank? display-name) "My Assistants"
                                                 (str display-name "'s Assistants"))))
          template templates/secretary-template]
      (when-not (some #(and (= (:id template) (:party/template %))
                            (= owner (get-in % [:party/owner :party/id])))
                      (rooms/get-room-agents (:room/id room)))
        (let [agent (parties/create-agent!
                     owner
                     (assoc (templates/agent-options (:name template) template)
                            :handle (str "vár-" (subs (str owner) 0 8)) :avatar "🤖"))]
          (rooms/add-party! (:room/id room) (:party/id agent))))
      room)))
