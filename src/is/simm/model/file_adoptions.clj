(ns is.simm.model.file-adoptions
  "Typed control-DB ownership of retained-file publication. INTERNAL storage
   composition, not an endpoint or authority. The host must check canonical
   owner, manifest/disposition, EACL and target against the SAME writer DB
   before composing these datoms. No artifact IO, callbacks or execution here."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-publication :as publication]
            [malli.core :as m]))

(def schema
  (conj
   (mapv (fn [[ident type]]
           (cond-> {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one}
             (= ident :file.adoption/id) (assoc :db/unique :db.unique/value)
             (= ident :file.adoption/component) (assoc :db/index true)))
         [[:file.adoption/id :db.type/uuid] [:file.adoption/version :db.type/long]
          [:file.adoption/intent :db.type/uuid] [:file.adoption/token :db.type/uuid]
          [:file.adoption/proposal :db.type/uuid] [:file.adoption/component :db.type/uuid]
          [:file.adoption/manifest :db.type/uuid] [:file.adoption/room :db.type/uuid]
          [:file.adoption/actor :db.type/uuid] [:file.adoption/filing :db.type/string]
          [:file.adoption/disposition :db.type/uuid]
          [:file.adoption/repository :db.type/uuid] [:file.adoption/store :db.type/uuid]
          [:file.adoption/backend :db.type/keyword] [:file.adoption/branch :db.type/keyword]
          [:file.adoption/ref :db.type/string] [:file.adoption/registry-system :db.type/uuid]
          [:file.adoption/runtime-system :db.type/string]
          [:file.adoption/state :db.type/keyword] [:file.adoption/created-at :db.type/instant]
          [:file.adoption/updated-at :db.type/instant] [:file.adoption/reason :db.type/keyword]
          [:file.adoption/commit :db.type/uuid] [:file.adoption/head :db.type/uuid]
          [:file.adoption/noop? :db.type/boolean] [:file.adoption/reconciled-by :db.type/uuid]])
   ;; Not a component: replacing an aborted attempt must retain its audit row.
   {:db/ident :proposal.fork/file-adoption :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}))

(def binding-schema
  [:map {:closed true}
   [:proposal :uuid] [:component :uuid] [:manifest :uuid] [:room :uuid]
   [:actor :uuid] [:filing [:re #"[0-9a-f]{64}"]] [:disposition :uuid]
   [:target publication/target-schema]])

(def ^:private binding-keys
  [:proposal :component :manifest :room :actor :filing :disposition :target])
(def ^:private target-keys
  [:repository :store :backend :branch :ref :registry-system :runtime-system])

(defn- refuse! [reason]
  (throw (ex-info "Retained file adoption ownership is invalid"
                  {:type ::invalid :reason reason})))

(defn- binding! [value]
  (when-not (and (m/validate binding-schema value)
                 (= (:component value) (:manifest value))
                 (publication/host-target? (:target value))
                 (not (str/blank? (get-in value [:target :ref]))))
    (refuse! :invalid-binding))
  value)

(defn validate!
  "Integrity only; neither a matching hash nor a claim token grants authority."
  [value]
  (binding! (select-keys value binding-keys))
  (when-not (and (= #{:id :version :intent :token :proposal :component :manifest
                      :room :actor :filing :disposition :target :state
                      :created-at :updated-at}
                    (set (keys (apply dissoc value
                                      [:reason :commit :head :noop? :reconciled-by]))))
                 (= 1 (:version value))
                 (every? uuid? (map value [:id :intent :token]))
                 (instance? java.util.Date (:created-at value))
                 (instance? java.util.Date (:updated-at value))
                 (not (.before ^java.util.Date (:updated-at value) (:created-at value)))
                 (= (:intent value)
                    (hasch/uuid [:simmis/file-adoption-intent-v1
                                 (select-keys value binding-keys)]))
                 (= (:id value)
                    (hasch/uuid [:simmis/file-adoption-operation-v1
                                 (:intent value) (:token value)])))
    (refuse! :invalid-operation))
  (let [outcome (select-keys value [:commit :head :noop? :reconciled-by])]
    (case (:state value)
      (:claimed :applying)
      (when (or (seq outcome) (contains? value :reason)) (refuse! :unexpected-outcome))
      :aborted
      (when-not (and (keyword? (:reason value)) (empty? outcome)) (refuse! :invalid-abort))
      :accepted
      (when-not (and (boolean? (:noop? value))
                     (not (contains? value :reason))
                     (every? uuid? (vals (dissoc outcome :noop?)))
                     (if (:noop? value)
                       (not (contains? value :commit))
                       (and (uuid? (:commit value)) (= (:commit value) (:head value)))))
        (refuse! :invalid-outcome))
      (refuse! :invalid-state)))
  value)

(defn make
  "Create a fresh invocation's proposed claim. The host supplies a fresh token,
   never an external caller. Only an ACKNOWLEDGED claim transaction can win;
   readback or exact replay does not confer publisher ownership."
  [binding token now]
  (binding! binding)
  (let [intent (hasch/uuid [:simmis/file-adoption-intent-v1 binding])]
    (validate! (merge binding
                      {:id (hasch/uuid [:simmis/file-adoption-operation-v1 intent token])
                       :version 1 :intent intent :token token :state :claimed
                       :created-at now :updated-at now}))))

(defn- entity [value]
  (into {} (map (fn [[k v]] [(keyword "file.adoption" (name k)) v]))
        (merge (dissoc value :target) (:target value))))

(defn- metadata [row]
  (validate!
   (assoc (into {} (keep (fn [[k v]]
                           (when (and (= "file.adoption" (namespace k))
                                      (not (some #{(keyword (name k))} target-keys)))
                             [(keyword (name k)) v]))) row)
          :target (into {} (for [k target-keys]
                             [k (get row (keyword "file.adoption" (name k)))])))))

(defn operation
  "Read one typed journal row. Authorize before exposing any projection, and
   never expose the invocation token as an external capability."
  [db id]
  (when-not (uuid? id) (refuse! :invalid-id))
  (when-let [eid (:e (first (d/datoms db :avet :file.adoption/id id)))]
    (metadata (d/pull db '[*] eid))))

(defn- fork! [db fork-eid value]
  (let [fork (when (integer? fork-eid)
               (d/pull db [:db/id :proposal.fork/scope :proposal.fork/system-type
                           :proposal.fork/status {:proposal.fork/file-adoption [:db/id :file.adoption/id]}]
                       fork-eid))]
    (when-not (and (:db/id fork) (= :file-output (:proposal.fork/system-type fork))
                   (= (:component value) (:proposal.fork/scope fork))
                   (nil? (:proposal.fork/status fork)))
      (refuse! :component-not-open))
    fork))

(defn- latest [db fork]
  (when-let [link (:proposal.fork/file-adoption fork)]
    (let [id (:file.adoption/id link)
          row (when (uuid? id) (operation db id))
          owners (vec (take 2 (d/datoms db :avet :proposal.fork/file-adoption (:db/id link))))]
      (when-not (and row (= [(:db/id fork)] (mapv :e owners)))
        (refuse! :invalid-journal-link))
      row)))

(declare current-operation)

(defn linked-operation!
  "INTERNAL integrity check of an exact open component's latest journal row.
   This is NOT claim ownership: live readback or replay can never replace the
   original invocation's acknowledged claim transaction. Authorize projections."
  [db fork-eid expected]
  (validate! expected)
  (let [fork (fork! db fork-eid expected)
        current (current-operation db (:db/id fork))]
    (when-not (= expected current) (refuse! :claim-changed))
    current))

(defn- operation-fork
  "Schema-aware bounded header, shared by two SEPARATE state-specific readers."
  [db fork-eid]
  (let [link-installed? (seq (d/datoms db :avet :db/ident :proposal.fork/file-adoption))
        ;; Before the journal schema is installed, typed Datahike cannot hold
        ;; this link. Do not pull an unknown attribute; partially installed or
        ;; malformed *present* links must still fail closed through latest.
        pattern (cond-> [:db/id :proposal.fork/scope :proposal.fork/system-type :proposal.fork/status]
                  link-installed? (conj {:proposal.fork/file-adoption [:db/id :file.adoption/id]}))
        fork (when (integer? fork-eid) (d/pull db pattern fork-eid))]
    {:fork fork :link-installed? link-installed?}))

(defn- checked-history
  [db fork link-installed?]
  (let [row (latest db fork)
        history (when link-installed?
                  (vec (take 1001 (d/datoms db :avet :file.adoption/component
                                            (:proposal.fork/scope fork)))))]
    (when (or (> (count history) 1000) (and (seq history) (nil? row)))
      (refuse! :invalid-journal-history))
    (when (and row (not= (:component row) (:proposal.fork/scope fork)))
      (refuse! :component-binding-changed))
    (doseq [datom history
            :let [historical (metadata (d/pull db '[*] (:e datom)))]]
      (when-not (and row
                     (= (dissoc (select-keys row binding-keys) :actor)
                        (dissoc (select-keys historical binding-keys) :actor))
                     (or (= (:id row) (:id historical)) (= :aborted (:state historical))))
        (refuse! :invalid-journal-history)))
    row))

(defn current-operation
  "INTERNAL integrity-only latest journal read for an OPEN retained component.
   Authorize first; this never confers publication ownership. Corrupt, shared,
   foreign or orphaned active history refuses. No terminal/readback ownership."
  [db fork-eid]
  (let [{:keys [fork link-installed?]} (operation-fork db fork-eid)]
    (when-not (and (:db/id fork) (= :file-output (:proposal.fork/system-type fork))
                   (uuid? (:proposal.fork/scope fork))
                   (nil? (:proposal.fork/status fork)))
      (refuse! :component-not-open))
    (checked-history db fork link-installed?)))

(defn accepted-operation
  "INTERNAL terminal integrity ONLY, not a broadened open-component guard.
   Require accepted component AND accepted latest journal, with identical
   bounded history/link checks. Current authorization is the host's duty;
   this reader never confers publication or reconciliation ownership."
  [db fork-eid]
  (let [{:keys [fork link-installed?]} (operation-fork db fork-eid)]
    (when-not (and (:db/id fork) (= :file-output (:proposal.fork/system-type fork))
                   (uuid? (:proposal.fork/scope fork))
                   (= :accepted (:proposal.fork/status fork)))
      (refuse! :component-not-accepted))
    (let [row (checked-history db fork link-installed?)]
      (when-not (= :accepted (:state row)) (refuse! :journal-not-accepted))
      row)))

(defn claim-tx-data
  "INTERNAL pure writer composition. Existing claimed/applying/accepted rows,
   including exact replay, always refuse. No timeout takeover. Host must check
   all authority/immutable canonical bindings against this same DB first."
  [db fork-eid value]
  (validate! value)
  (when-not (= :claimed (:state value)) (refuse! :not-a-claim))
  (let [fork (fork! db fork-eid value)
        previous (current-operation db (:db/id fork))]
    (when (and previous
               (not= (dissoc (select-keys previous binding-keys) :actor)
                     (dissoc (select-keys value binding-keys) :actor)))
      (refuse! :component-binding-changed))
    (when (or (operation db (:id value))
              (and previous (not= :aborted (:state previous))))
      (refuse! :already-owned))
    [(entity value)
     [:db/add fork-eid :proposal.fork/file-adoption [:file.adoption/id (:id value)]]]))

(defn transition-tx-data
  "INTERNAL exact-CAS transition. Claimed->applying requires current host
   authorization; claimed->aborted requires witnessed physical predispatch
   completion; ->accepted requires an exact observed publication receipt.
   Those are HOST obligations, not conclusions this storage helper can infer.
   Applying/accepted can NEVER release ownership or authorize a replay."
  [db fork-eid expected next-state fields now]
  (linked-operation! db fork-eid expected)
  (when-not (and (instance? java.util.Date now)
                 (not (.before ^java.util.Date now (:updated-at expected)))
                 (case next-state
                   :applying (and (= :claimed (:state expected)) (empty? fields))
                   :aborted (and (= :claimed (:state expected)) (= #{:reason} (set (keys fields))))
                   :accepted (and (= :applying (:state expected))
                                  (contains? fields :noop?)
                                  (every? #{:commit :head :noop? :reconciled-by} (keys fields)))
                   false)
                 (map? fields))
    (refuse! :invalid-transition))
  (let [next (validate! (merge expected fields {:state next-state :updated-at now}))]
    [(assoc (entity next) :db/id [:file.adoption/id (:id next)])]))
