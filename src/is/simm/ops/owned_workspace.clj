(ns is.simm.ops.owned-workspace
  "Inactive host-only owned-default bindings. No external routing is installed.
   A binding/stamp is neither a capability nor a lease. Callers supply a current
   registry snapshot and transport-authenticated identity for each operation."
  (:require [datahike.api :as d]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.query :as query]
            [hasch.core :as hasch]
            [is.simm.documents.files :as files]
            [is.simm.model.access :as access]
            [is.simm.model.knowledge-publication :as publication]
            [is.simm.ops.knowledge-admission :as admission]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec]
            [yggdrasil.protocols :as protocols]))

(defn- refuse! [reason]
  (throw (ex-info "Owned workspace binding was refused" {:type ::refused :reason reason})))

(defn- authorize! [db principal action room-id]
  (let [party (if (uuid? principal) principal
                  (access/actor->party-uuid (:principal/id principal)))]
    (when-not (and (uuid? room-id) party (#{:read :write :merge} action)
                   (access/can? db party :read {:room room-id})
                   (access/can? db party action {:room room-id}))
      (refuse! :denied))
    party))

(defn- facts [db eid attrs]
  (vec (mapcat (fn [attr]
                 (map (fn [datom] [eid attr (:v datom) (:tx datom)])
                      (d/datoms db :eavt eid attr))) attrs)))

(defn- mapping-revision [db room-id registry-system]
  (let [control-store (get-in db [:config :store :id])
        room (d/entity db [:room/id room-id])
        system (d/entity db [:system/id registry-system])
        rid (:db/id room) sid (:db/id system)
        grants (when (and rid sid)
                 (d/q '[:find [?g ...] :in $ ?room ?system
                        :where [?g :grant/room ?room] [?g :grant/system ?system]
                        [?g :grant/permission :owner]] db rid sid))]
    (when-not (and (uuid? control-store) rid sid (seq grants)
                   (= sid (:db/id (:room/default-repository room)))
                   (= :repo (:system/type system)))
      (refuse! :invalid-mapping))
    (let [rows (concat (facts db rid [:room/id :room/slug :room/default-repository])
                       (facts db sid [:system/id :system/type :system/scope])
                       (mapcat #(facts db % [:grant/room :grant/system :grant/permission]) grants))]
      (hasch/uuid [:simmis/owned-default-mapping-v1 control-store (sort-by pr-str rows)]))))

(defn- target-stamp [room-id mapping target workspace-config]
  (hasch/uuid [:simmis/owned-workspace-target-v2 room-id mapping target workspace-config]))

(defn resolve!
  "Resolve after authentication, using exactly this registry DB for authority,
   Room mapping and default ownership. Returns live HOST handles plus one
   immutable target DB; never persist this value or accept it from a client.
   No current-workspace fallback, hydration or parent selection mutation."
  [registry-db principal action room-id room]
  (let [party (authorize! registry-db principal action room-id)
        {:keys [system conn registry-system runtime-system] :as descriptor}
        (admission/default-binding registry-db room-id room)
        native (binding [ec/*execution-context* (:ctx room)]
                 ;; The native explicit resolver also rejects a differently
                 ;; named Geschichte OR Datahike alias of this connection.
                 ;; Ambient selection cannot choose another system's policy.
                 (workspace/workspace-for-system! system))
        _ (when-not (and (= runtime-system (str (protocols/system-id system)))
                         (identical? system (:system native))
                         (identical? conn (:conn native)))
            (refuse! :invalid-runtime))
        workspace-config (dissoc (:repository native) :conn)
        db @conn
        target {:registry-system registry-system :runtime-system runtime-system
                :repository (:geschichte.repo/id (query/repository db))
                :store (get-in db [:config :store :id]) :backend (get-in db [:config :store :backend])
                :branch (get-in db [:config :branch]) :ref (query/current-ref db)}
        _ (when-not (and (publication/host-target? target) (m/validate publication/target-schema target))
            (refuse! :invalid-target))
        ;; Descriptor and file projection must describe ONE coherent snapshot.
        ;; HEAD changes are fine; a config/ref switch between dereferences is
        ;; refused, not silently stamped using pieces of two different targets.
        _ (when-not (and (= (:config db) (:config workspace-config))
                         (= (select-keys target [:repository :store :backend :branch :ref])
                            (::workspace/physical-target native)))
            (refuse! :target-changed))
        mapping (mapping-revision registry-db room-id registry-system)
        stamp (target-stamp room-id mapping target workspace-config)]
    {:room room :room-id room-id :party party :action action
     :descriptor descriptor :system system :conn conn :db db
     :target target :target-stamp stamp :workspace-config workspace-config}))

(defn validate!
  "Re-resolve with the caller's CURRENT registry snapshot. Submitted stamp or
   binding data never selects a target. Returns the newly resolved snapshot,
   refusing stale/default-replaced or mismatched host handles even if bytes
   happen to be identical. This is not a write/publication admission gate."
  [registry-db principal action room-id room binding]
  (let [current (resolve! registry-db principal action room-id room)]
    (when-not (and (= (:target-stamp binding) (:target-stamp current))
                   (= (:target binding) (:target current))
                   (= (:workspace-config binding) (:workspace-config current))
                   (identical? (:system binding) (:system current))
                   (identical? (:conn binding) (:conn current))
                   (identical? (:room binding) room))
      (refuse! :target-changed))
    current))

(defn read-file
  "Inactive host composition: current authority/target validation followed by
   a single immutable file projection. Expected stamp is only a precondition."
  [registry-db principal room-id room path expected-stamp]
  (when-not (or (nil? expected-stamp) (uuid? expected-stamp))
    (refuse! :invalid-stamp))
  (let [binding (resolve! registry-db principal :read room-id room)]
    (when (and expected-stamp (not= expected-stamp (:target-stamp binding)))
      (refuse! :target-changed))
    (assoc (files/read-file-in (:db binding) path) :target-stamp (:target-stamp binding))))
