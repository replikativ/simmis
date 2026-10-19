(ns is.simm.model.workflow-schedules
  "Host-only schedule activations and a durable occurrence outbox. Definitions
   remain room files; execution status remains owned by work requests/Jobs."
  (:require [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [dvergr.scheduler.cron :as cron]
            [hasch.core :as hasch]))

(def schema
  (mapv (fn [[ident type]]
          (cond-> {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one}
            (#{:schedule.activation/id :schedule.occurrence/id} ident) (assoc :db/unique :db.unique/value)
            (#{:schedule.activation/room :schedule.activation/next-fire :schedule.occurrence/activation
               :schedule.occurrence/state} ident) (assoc :db/index true)))
        [[:schedule.activation/id :db.type/uuid]
         [:schedule.activation/room :db.type/uuid]
         [:schedule.activation/by :db.type/uuid]
         [:schedule.activation/revision :db.type/uuid]
         [:schedule.activation/target-ref :db.type/store-ref]
         [:schedule.activation/enabled? :db.type/boolean]
         [:schedule.activation/created-at :db.type/instant]
         [:schedule.activation/next-fire :db.type/instant]
         [:schedule.activation/last-occurrence :db.type/uuid]
         [:schedule.activation/active-occurrence :db.type/uuid]
         [:schedule.activation/reason :db.type/keyword]
         [:schedule.activation/last-processed-input :db.type/uuid]
         [:schedule.occurrence/id :db.type/uuid]
         [:schedule.occurrence/activation :db.type/uuid]
         [:schedule.occurrence/revision :db.type/uuid]
         [:schedule.occurrence/intended-at :db.type/instant]
         [:schedule.occurrence/created-at :db.type/instant]
         [:schedule.occurrence/coalesced :db.type/long]
         [:schedule.occurrence/state :db.type/keyword]
         [:schedule.occurrence/claim :db.type/uuid]
         [:schedule.occurrence/input-id :db.type/uuid]
         [:schedule.occurrence/input-ref :db.type/store-ref]
         [:schedule.occurrence/input-revision :db.type/uuid]
         [:schedule.occurrence/request :db.type/uuid]
         [:schedule.occurrence/announced? :db.type/boolean]
         [:schedule.occurrence/reason :db.type/keyword]]))

(defn- pull-row [db attr id]
  ;; A missing lookup ref throws in Datahike's pull; absence is normal for
  ;; idempotent creation and must be distinguished from storage failures.
  (when-let [eid (:e (first (d/datoms db :avet attr id)))]
    (d/pull db '[*] eid)))

(defn activation [conn id]
  (pull-row @conn :schedule.activation/id id))

(defn occurrence [conn id]
  (pull-row @conn :schedule.occurrence/id id))

(defn activations [conn]
  (mapv #(d/pull @conn '[*] (:e %)) (d/datoms @conn :aevt :schedule.activation/id)))

(defn occurrences [conn activation-id]
  (->> (d/q '[:find [(pull ?e [*]) ...] :in $ ?id
              :where [?e :schedule.occurrence/activation ?id]] @conn activation-id)
       (sort-by (juxt :schedule.occurrence/intended-at :schedule.occurrence/id) #(compare %2 %1))
       vec))

(defn recent-occurrences
  "Keep at most limit rows in memory while scanning this activation's index.
   Runtime passes use its active pointer, never historical occurrence scans."
  [conn activation-id limit]
  (reduce (fn [recent datom]
            (->> (conj recent (d/pull @conn '[*] (:e datom)))
                 (sort-by (juxt :schedule.occurrence/intended-at :schedule.occurrence/id) #(compare %2 %1))
                 (take limit) vec))
          [] (d/datoms @conn :avet :schedule.occurrence/activation activation-id)))

(defn admitting-occurrences [conn]
  (map #(d/pull @conn '[*] (:e %))
       (d/datoms @conn :avet :schedule.occurrence/state :admitting)))

(defn target [conn row]
  (let [value (artifact/get-value (artifact/datahike-store conn) (:schedule.activation/target-ref row))]
    (when-not (= (:schedule.activation/revision row) (hasch/uuid value))
      (throw (ex-info "Schedule target failed its content identity" {:type ::corrupt})))
    value))

(defn- create-tx [db row]
  (if-let [prior (pull-row db :schedule.activation/id (:schedule.activation/id row))]
    (do (when-not (= (:schedule.activation/revision prior) (:schedule.activation/revision row))
          (throw (ex-info "Schedule identity already names another target" {:type ::conflict}))) [])
    (do
      (when (>= (count (d/datoms db :aevt :schedule.activation/id)) 100)
        (throw (ex-info "This host already has its bounded100 workflow schedules" {:type ::capacity})))
      [row])))

(defn create! [conn {:keys [id room by target now next-fire]}]
  (let [revision (hasch/uuid target)]
    (artifact/publish-value!
     (artifact/datahike-store conn) target
     (fn [ref]
       (d/transact conn [[:db.fn/call create-tx
                          {:schedule.activation/id id :schedule.activation/room room
                           :schedule.activation/by by :schedule.activation/revision revision
                           :schedule.activation/target-ref ref :schedule.activation/enabled? true
                           :schedule.activation/created-at now :schedule.activation/next-fire next-fire}]])))
    (activation conn id)))

(defn- enabled-tx [db id enabled?]
  (let [row (pull-row db :schedule.activation/id id)
        oid (:schedule.activation/active-occurrence row)
        o (when oid (pull-row db :schedule.occurrence/id oid))]
    (when-not row (throw (ex-info "No such workflow schedule" {:type ::missing})))
    ;; Pausing withdraws queued, not yet admitted work. Serialize against
    ;; admit-tx so either admission owns it already, or it can never replay.
    (cond-> [{:db/id (:db/id row) :schedule.activation/enabled? enabled?}]
      (and enabled? (:schedule.activation/reason row))
      (conj [:db/retract (:db/id row) :schedule.activation/reason (:schedule.activation/reason row)])
      (and (not enabled?) (= :pending (:schedule.occurrence/state o)))
      (into [{:db/id (:db/id o) :schedule.occurrence/state :skipped :schedule.occurrence/reason :paused}
             [:db/retract (:db/id row) :schedule.activation/active-occurrence oid]]))))

(defn set-enabled! [conn id enabled?]
  (d/transact conn [[:db.fn/call enabled-tx id enabled?]])
  (activation conn id))

(defn due-slot
  "Latest intended slot <=now, preserving interval anchoring. Calendar schedules
   reuse Dvergr's zone-aware rules; excessive backlog fails visibly, not loops."
  [spec ^java.util.Date first-fire ^java.util.Date now]
  (when-not (.after first-fire now)
    (if-let [interval (or (:interval-ms spec) (:every-ms spec))]
      (let [skipped (quot (- (.getTime now) (.getTime first-fire)) interval)
            at (+ (.getTime first-fire) (* skipped interval))]
        {:intended (java.util.Date. at) :next (java.util.Date. (+ at interval)) :coalesced skipped})
      (loop [at first-fire skipped 0]
        (when (> skipped 4096)
          (throw (ex-info "Schedule backlog requires explicit reactivation" {:type ::backlog})))
        (let [next (java.util.Date/from (cron/next-fire-time spec (.toInstant at)))]
          (when-not (.after next at)
            (throw (ex-info "Schedule does not advance" {:type ::invalid})))
          (if (.after next now) {:intended at :next next :coalesced skipped}
              (recur next (inc skipped))))))))

(defn- due-tx [db id revision spec now]
  (let [row (pull-row db :schedule.activation/id id)]
    (when (and (:schedule.activation/enabled? row)
               (= revision (:schedule.activation/revision row)))
      (when-let [{:keys [intended next coalesced]} (due-slot spec (:schedule.activation/next-fire row) now)]
        (let [oid (hasch/uuid [:simmis/scheduled-occurrence-v1 id revision intended])
              active (:schedule.activation/active-occurrence row)
              prior (when active (pull-row db :schedule.occurrence/id active))
              pending? (= :pending (:schedule.occurrence/state prior))
              busy? (and (some? active) (not pending?))]
          (cond-> [(cond-> {:schedule.occurrence/id oid :schedule.occurrence/activation id
                            :schedule.occurrence/revision revision :schedule.occurrence/intended-at intended
                            :schedule.occurrence/created-at now :schedule.occurrence/coalesced (long (+ coalesced (if pending? 1 0)))
                            :schedule.occurrence/state (if busy? :skipped :pending)}
                     busy? (assoc :schedule.occurrence/reason :overlap))
                   (cond-> {:db/id (:db/id row) :schedule.activation/next-fire next
                            :schedule.activation/last-occurrence oid}
                     (not busy?) (assoc :schedule.activation/active-occurrence oid))]
            pending? (conj {:db/id (:db/id prior) :schedule.occurrence/state :skipped
                            :schedule.occurrence/reason :coalesced})))))))

(defn claim-due! [conn row spec now]
  ;; The outbox insert and cursor advance share the serialized writer. Lost
  ;; responses leave a discoverable pending row, never an unrecorded firing.
  (d/transact conn [[:db.fn/call due-tx (:schedule.activation/id row)
                     (:schedule.activation/revision row) spec now]]))

(defn- admit-tx [db oid claim]
  (let [o (pull-row db :schedule.occurrence/id oid)
        a (when o (pull-row db :schedule.activation/id (:schedule.occurrence/activation o)))]
    (when (and (= :pending (:schedule.occurrence/state o))
               (:schedule.activation/enabled? a)
               (= oid (:schedule.activation/active-occurrence a)))
      [{:db/id (:db/id o) :schedule.occurrence/state :admitting :schedule.occurrence/claim claim}])))

(defn admit! [conn oid]
  (let [claim (random-uuid)]
    (d/transact conn [[:db.fn/call admit-tx oid claim]])
    (when (= claim (:schedule.occurrence/claim (occurrence conn oid))) claim)))

(defn- claimed-occurrence! [db oid claim]
  (let [o (pull-row db :schedule.occurrence/id oid)
        a (when o (pull-row db :schedule.activation/id (:schedule.occurrence/activation o)))]
    (when-not (and (uuid? claim) (= claim (:schedule.occurrence/claim o))
                   (= :admitting (:schedule.occurrence/state o))
                   (= (:schedule.occurrence/revision o) (:schedule.activation/revision a))
                   (= oid (:schedule.activation/active-occurrence a)))
      (throw (ex-info "Scheduled input admission no longer owns its occurrence" {:type ::conflict})))
    ;; Pausing withdraws PENDING work, not this already-owned admission.
    [o a]))

(defn- select-input-tx [db oid claim input-id empty-input?]
  (let [[o a] (claimed-occurrence! db oid claim)
        prior (:schedule.activation/last-processed-input a)
        reason (cond (= input-id prior) :unchanged
                     (and empty-input? (nil? prior)) :no-inputs)]
    (when (and (:schedule.occurrence/input-id o)
               (not= input-id (:schedule.occurrence/input-id o)))
      (throw (ex-info "Occurrence already selected different evidence" {:type ::conflict})))
    (cond-> [{:db/id (:db/id o) :schedule.occurrence/input-id input-id}]
      reason (into [{:db/id (:db/id o) :schedule.occurrence/state :skipped
                     :schedule.occurrence/reason reason}
                    [:db/retract (:db/id a) :schedule.activation/active-occurrence oid]]))))

(defn select-input!
  "Claim-fenced metadata decision BEFORE allowance/provider work. Skip and
   release are one transaction; only a prior successfully processed execution
   suppresses unchanged evidence. This cursor says nothing about adoption."
  [conn oid claim input-id empty-input?]
  (when-not (and (uuid? input-id) (boolean? empty-input?))
    (throw (ex-info "Invalid scheduled input selection" {:type ::conflict})))
  (d/transact conn [[:db.fn/call select-input-tx oid claim input-id empty-input?]])
  (let [o (occurrence conn oid)]
    (if (= :skipped (:schedule.occurrence/state o)) (:schedule.occurrence/reason o) :capture)))

(defn- retain-input-tx [db oid claim input-id revision ref]
  (let [[o _] (claimed-occurrence! db oid claim)]
    (when-not (= input-id (:schedule.occurrence/input-id o))
      (throw (ex-info "Capture differs from selected scheduled evidence" {:type ::conflict})))
    (if (:schedule.occurrence/input-ref o)
      (do (when-not (= revision (:schedule.occurrence/input-revision o))
            (throw (ex-info "Occurrence already retained different inputs" {:type ::conflict}))) [])
      [{:db/id (:db/id o) :schedule.occurrence/input-ref ref
        :schedule.occurrence/input-revision revision}])))

(defn retain-input!
  "Retain complete frozen input data before ordinary work submission. Retrying
   cannot replace this occurrence's source snapshot with newer room contents."
  [conn oid claim value]
  (when-not (and (map? value) (= 1 (:version value)) (uuid? (:input-id value)))
    (throw (ex-info "Invalid retained scheduled inputs" {:type ::conflict})))
  (let [revision (hasch/uuid value)]
    (artifact/publish-value!
     (artifact/datahike-store conn) value
     (fn [ref]
       (d/transact conn [[:db.fn/call retain-input-tx oid claim (:input-id value) revision ref]])))
    (occurrence conn oid)))

(defn retained-input [conn o]
  (when-let [ref (:schedule.occurrence/input-ref o)]
    (let [value (artifact/get-value (artifact/datahike-store conn) ref)]
      (when-not (and (= 1 (:version value))
                     (= (:schedule.occurrence/input-revision o) (hasch/uuid value))
                     (= (:schedule.occurrence/input-id o) (:input-id value)))
        (throw (ex-info "Scheduled inputs failed their content identity" {:type ::corrupt})))
      value)))

(defn- processed-input-tx [db oid]
  (let [o (pull-row db :schedule.occurrence/id oid)
        a (when o (pull-row db :schedule.activation/id (:schedule.occurrence/activation o)))]
    (when (and (= :linked (:schedule.occurrence/state o))
               (= oid (:schedule.activation/active-occurrence a))
               (= (:schedule.occurrence/revision o) (:schedule.activation/revision a))
               (uuid? (:schedule.occurrence/request o))
               (:schedule.occurrence/input-ref o) (:schedule.occurrence/input-id o))
      [{:db/id (:db/id a) :schedule.activation/last-processed-input (:schedule.occurrence/input-id o)}])))

(defn mark-input-processed!
  "Host calls only after the linked Job settled and its candidate Attempt
   was certified successful against the request's exact environment.
   Successful processing/proposal is not knowledge adoption."
  [conn oid]
  (d/transact conn [[:db.fn/call processed-input-tx oid]]))

(defn finish-admission! [conn oid state request reason]
  (d/transact conn [(cond-> {:db/id [:schedule.occurrence/id oid] :schedule.occurrence/state state}
                      request (assoc :schedule.occurrence/request request)
                      reason (assoc :schedule.occurrence/reason reason))]))

(defn- clear-active-tx [db id oid]
  (let [row (pull-row db :schedule.activation/id id)]
    (when (= oid (:schedule.activation/active-occurrence row))
      [[:db/retract (:db/id row) :schedule.activation/active-occurrence oid]])))

(defn clear-active! [conn row oid]
  (d/transact conn [[:db.fn/call clear-active-tx (:schedule.activation/id row) oid]]))

(defn mark-announced! [conn oid]
  (d/transact conn [{:db/id [:schedule.occurrence/id oid] :schedule.occurrence/announced? true}]))

(defn block! [conn row reason]
  (d/transact conn [{:db/id (:db/id row)
                     :schedule.activation/enabled? false :schedule.activation/reason reason}]))
