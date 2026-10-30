(ns is.simm.model.github-sync
  "Typed HOST journal for private Git preparation and reviewed advancement.
   These pure fragments grant no authority or native creation/cleanup rights.
   The fixed HOST validates artifacts and current grants in the SAME writer DB."
  (:require [datahike.api :as d]
            [hasch.core :as hasch]))

(def schema
  (mapv (fn [[ident type]]
          (cond-> {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one}
            (#{:github.sync/id :github.sync/active-room} ident) (assoc :db/unique :db.unique/value)
            (= :github.sync/room ident) (assoc :db/index true)))
        [[:github.sync/id :db.type/uuid] [:github.sync/client-id :db.type/uuid]
         [:github.sync/by :db.type/uuid] [:github.sync/room :db.type/uuid]
         [:github.sync/claim :db.type/uuid] [:github.sync/active-room :db.type/uuid]
         [:github.sync/state :db.type/keyword] [:github.sync/held-from :db.type/keyword]
         [:github.sync/kind :db.type/keyword]
         [:github.sync/preparation :db.type/store-ref] [:github.sync/preparation-id :db.type/uuid]
         [:github.sync/workspace :db.type/store-ref] [:github.sync/workspace-id :db.type/uuid]
         [:github.sync/ready :db.type/store-ref] [:github.sync/ready-id :db.type/uuid]
         [:github.sync/retention :db.type/store-ref] [:github.sync/retention-id :db.type/uuid]
         [:github.sync/filing :db.type/store-ref] [:github.sync/filing-id :db.type/uuid]
         [:github.sync/outcome :db.type/store-ref] [:github.sync/outcome-id :db.type/uuid]
         [:github.sync/closure :db.type/store-ref] [:github.sync/closure-id :db.type/uuid]
         [:github.sync/incident :db.type/store-ref] [:github.sync/incident-id :db.type/uuid]]))

(defn- refuse! [reason]
  (throw (ex-info "Git sync journal refused" {:type ::refused :reason reason})))

(defn request-id [by room client-id]
  (when-not (every? uuid? [by room client-id]) (refuse! :invalid-identity))
  (hasch/uuid [:simmis/github-sync 1 by room client-id]))

(defn sync-in [db id]
  (when-let [entity (:db/id (d/entity db [:github.sync/id id]))]
    (d/pull db '[*] entity)))

(defn active-in [db room]
  (when-let [entity (:db/id (d/entity db [:github.sync/active-room room]))]
    (d/pull db '[*] entity)))

(defn- binding! [{:keys [id by room client-id claim] :as binding}]
  (when-not (and (map? binding) (= #{:id :by :room :client-id :claim} (set (keys binding)))
                 (every? uuid? (vals binding)) (= id (request-id by room client-id)))
    (refuse! :invalid-identity))
  {:github.sync/id id :github.sync/by by :github.sync/room room
   :github.sync/client-id client-id :github.sync/claim claim})

(defn kind [row] (or (:github.sync/kind row) :fast-forward))

(defn prepare-tx
  "Reserve one intent and the Room's exclusive pending-sync slot. Exact replay
   is a no-op, NEVER permission to fork again. Kind is immutable; incoming
   preparation cannot be dispatched through the legacy fast-forward protocol."
  ([db binding preparation-ref preparation-id]
   (prepare-tx db binding preparation-ref preparation-id :fast-forward))
  ([db binding preparation-ref preparation-id operation-kind]
   (when-not (#{:fast-forward :incoming :push} operation-kind) (refuse! :invalid-kind))
   (let [identity (binding! binding)
         immutable (assoc (dissoc identity :github.sync/claim)
                          :github.sync/preparation preparation-ref :github.sync/preparation-id preparation-id)]
     (when-not (every? uuid? [preparation-ref preparation-id]) (refuse! :invalid-preparation))
     (if-let [row (sync-in db (:id binding))]
       (do (when-not (and (= operation-kind (kind row))
                          (= immutable (select-keys row (keys immutable))))
             (refuse! :different-intent)) [])
       (do (when (active-in db (:room binding)) (refuse! :room-busy))
           [(cond-> (merge {:db/id -1 :github.sync/state :creating :github.sync/active-room (:room binding)}
                           identity immutable)
              (not= :fast-forward operation-kind) (assoc :github.sync/kind operation-kind))])))))

(def ^:private next-states
  {:creating #{:allocated :held}
   :allocated #{:ready :retaining :held :closing}
   :retaining #{:ready :held}
   :ready #{:applying :filing :closing :held}
   :filing #{:filed :held}
   :filed #{:closing :held}
   :applying #{:applied :satisfied-observed :held}
   :applied #{:closing :held}
   :satisfied-observed #{:closing :held}
   :closing #{:closed :held}
   :held #{:ready :closing :satisfied-observed}
   :closed #{}})

(def ^:private receipt-slot
  {:allocated :workspace :retaining :retention :ready :ready :filing :filing
   :filed :outcome :applied :outcome :satisfied-observed :outcome
   :closed :closure :held :incident})

(defn transition-tx
  "Whole-row CAS; expected must be the exact observed typed row. The HOST must
   separately attest native exit/ownership before cleanup and verify each
   immutable artifact. :satisfied-observed attests CURRENT desired state, never
   who performed a historical advance. Only positive closure frees the slot."
  [db expected to-state ref receipt-id]
  (let [row (sync-in db (:github.sync/id expected))
        from (:github.sync/state row)
        slot (receipt-slot to-state)
        ref-key (when slot (keyword "github.sync" (name slot)))
        id-key (when slot (keyword "github.sync" (str (name slot) "-id")))
        receipt (if slot {ref-key ref id-key receipt-id} {})]
    (when-not (and row (= expected row)) (refuse! :stale-observation))
    ;; Push shares identity and the exclusive pending-room slot, but its
    ;; separate model owns its transition graph and publication receipts.
    (when (= :push (kind row)) (refuse! :wrong-kind))
    (when (or (and (= :incoming (kind row)) (#{:applying :applied :satisfied-observed} to-state))
              (and (not= :incoming (kind row)) (#{:retaining :filing :filed} to-state)))
      (refuse! :wrong-kind))
    (when-not (if slot (every? uuid? [ref receipt-id]) (and (nil? ref) (nil? receipt-id)))
      (refuse! :invalid-receipt))
    (when-not (or (= from to-state) (contains? (next-states from) to-state)) (refuse! :invalid-transition))
    (when (and (= :incoming (kind row)) (= :ready to-state)
               (not (and (every? uuid? [(:github.sync/retention row) (:github.sync/retention-id row)])
                         (or (#{:retaining :ready} from)
                             (and (= :held from) (#{:retaining :ready} (:github.sync/held-from row)))))))
      ;; Ordered durable intent is necessary, never sufficient: the HOST must
      ;; still verify all native roots before acknowledging READY.
      (refuse! :retention-required))
    (when (and (= :held from) (= :ready to-state)
               (not (or (#{:allocated :ready} (:github.sync/held-from row))
                        (and (= :incoming (kind row)) (= :retaining (:github.sync/held-from row))))))
      (refuse! :uncertain-apply))
    (when (and slot (not= :incident slot) (contains? row ref-key)
               (not= receipt (select-keys row (keys receipt))))
      (refuse! :different-receipt))
    (if (and (= from to-state) (= receipt (select-keys row (keys receipt)))) []
        (do
          (when-not (= (:github.sync/room row) (:github.sync/active-room row))
            (refuse! :inactive-intent))
          (cond-> [(merge {:db/id (:db/id row) :github.sync/state to-state} receipt
                          (when (= :held to-state)
                            {:github.sync/held-from (if (= :held from) (:github.sync/held-from row) from)}))]
            (and (not= :held to-state) (:github.sync/held-from row))
            (conj [:db/retract (:db/id row) :github.sync/held-from (:github.sync/held-from row)])
            (= :closed to-state)
            (conj [:db/retract (:db/id row) :github.sync/active-room (:github.sync/active-room row)]))))))
