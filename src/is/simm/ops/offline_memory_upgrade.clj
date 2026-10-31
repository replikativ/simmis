(ns is.simm.ops.offline-memory-upgrade
  "Operator-only, stopped-service migration. Caller exclusively owns BOTH
   connections throughout inspect/apply/recovery; an App/Room lease is not an
   exclusivity proof. No public transport, implicit connect/create or retry."
  (:require [datahike.api :as d]
            [geschichte.query :as query]
            [hasch.core :as hasch]
            [is.simm.documents.knowledge-upgrade :as foundation]
            [is.simm.documents.profile :as profile]
            [is.simm.documents.space :as space]
            [is.simm.documents.view :as view]
            [is.simm.model.access :as access]
            [is.simm.model.memory-upgrades :as receipts]
            [is.simm.ops.owned-workspace :as owned]))

(defn- held! [reason]
  (throw (ex-info "Offline Memory upgrade held" {:type ::held :reason reason})))

(defn- authorize! [db actor room]
  (let [party (d/entity db [:party/id actor])
        owner (:party/id (:room/owner (d/entity db [:room/id room])))]
    (when-not (and (uuid? actor) (uuid? room) (= actor owner)
                   (= :human (or (:actor/kind party) (:party/type party)))
                   (access/can? db actor :admin {:room room})
                   (access/can? db actor :merge {:room room}))
      (held! :owner-and-merge-required))))

(defn- room-state [db room]
  (let [row (d/pull db '[:room/id :room/type :room/slug :room/created-by :room/content-db-scope
                         :room/provisioning-state :room/provisioning-version :room/archived-at
                         {:room/owner [:party/id]} {:room/parties [:party/id]}
                         {:room/default-repository [:system/id :system/type :system/scope
                                                    :system/repository-profile]}] [:room/id room])
        grants (when (contains? (d/schema db) :grant/room)
                 (d/q '[:find [?g ...] :in $ ?room :where [?r :room/id ?room] [?g :grant/room ?r]] db room))]
    (assoc row :default-rooms (set (d/q '[:find [?id ...] :in $ ?system
                                        :where [?s :system/id ?system] [?r :room/default-repository ?s]
                                        [?r :room/id ?id]] db (get-in row [:room/default-repository :system/id])))
               :owned-mapping (owned/control-revision db room (get-in row [:room/default-repository :system/id]))
               :grants (mapv #(d/pull db '[*] %) (sort grants))
               :room/parties (vec (sort-by (comp str :party/id) (:room/parties row))))))

(defn- idle! [db room]
  (doseq [[room-attribute flag]
          [[:schedule.activation/room :schedule.activation/enabled?]
           [:git.policy/room :git.policy/enabled?]]
          :when (and (contains? (d/schema db) room-attribute) (contains? (d/schema db) flag))]
    (when (seq (d/q '[:find ?e :in $ ?a ?flag ?room :where [?e ?a ?room] [?e ?flag true]]
                    db room-attribute flag room)) (held! :enabled-background-work)))
  (doseq [attribute [:github.sync/active-room :github.push/active-room]
          :when (contains? (d/schema db) attribute)]
    (when (seq (d/q '[:find ?e :in $ ?a ?room :where [?e ?a ?room]] db attribute room))
      (held! :unresolved-git-operation)))
  (when (and (contains? (d/schema db) :schedule.activation/active-occurrence)
             (seq (d/q '[:find ?e :in $ ?room :where [?e :schedule.activation/room ?room]
                         [?e :schedule.activation/active-occurrence _]] db room)))
    (held! :active-occurrence))
  (when (contains? (d/schema db) :proposal/room)
    (let [proposals (d/q '[:find [?p ...] :in $ ?room :where [?p :proposal/room ?room]] db room)]
      (doseq [id proposals :let [proposal (d/entity db id)]]
        ;; Native proposals use :open. Missing/unknown states and unfinished
        ;; forks must also hold rather than masquerading as resolved work.
        (when-not (and (#{:accepted :dismissed} (:proposal/status proposal))
                       (every? #(#{:accepted :dismissed} (:proposal.fork/status %))
                               (:proposal/forks proposal)))
          (held! :pending-proposal))))))

(defn- target! [control-db repo-db actor room expected]
  (authorize! control-db actor room)
  (idle! control-db room)
  (let [row (room-state control-db room)
        system (:room/default-repository row)
        actual {:system (:system/id system) :scope (:system/scope system)
                :repository (:geschichte.repo/id (query/repository repo-db))
                :store (get-in repo-db [:config :store :id])}]
    (when-not (and (= expected actual) (= #{room} (:default-rooms row))
                   (every? uuid? (map actual [:system :repository :store]))
                   (string? (:scope actual)) (= :repo (:system/type system))
                   (nil? (:room/archived-at row))
                   (uuid? (:room/content-db-scope row))
                   (some #(= actor (:party/id %)) (:room/parties row))
                   (#{:personal-ai :project :group} (:room/type row))
                   (nil? (:room/provisioning-state row)) (nil? (:room/provisioning-version row)))
      (held! :target-changed-or-unsupported))
    {:row row :target actual :basis (hasch/uuid [:simmis/memory-upgrade-control-v1 row])}))

(defn inspect!
  "Read-only prepare on exclusive existing connections. Freeze this closed
   intent for apply. A returned plan is not permission or a write receipt."
  [control-conn repo-conn {:keys [id actor room target at] :as request}]
  (when-not (and (= #{:id :actor :room :target :at} (set (keys request)))
                 (uuid? id) (inst? at)) (held! :invalid-intent))
  (let [control-db @control-conn db @repo-conn
        resolved (target! control-db db actor room target)
        _ (when (or (get-in resolved [:row :room/default-repository :system/repository-profile])
                    (when (contains? (d/schema control-db) :room/knowledge-upgrade)
                      (:room/knowledge-upgrade (d/entity control-db [:room/id room]))))
            (held! :existing-upgrade-or-profile))
        plan (foundation/prepare db id at)]
    {:version 1 :request request :control-basis (:basis resolved)
     :basis (:basis plan) :source (:source plan) :definition-revision (:definition-revision plan)
     :plan (hasch/uuid plan) :documents (:documents plan)}))

(defn- row-for [inspection]
  (let [{:keys [id actor room target at]} (:request inspection)]
    {:memory.upgrade/id id :memory.upgrade/room room :memory.upgrade/by actor
     :memory.upgrade/system (:system target) :memory.upgrade/system-scope (:scope target)
     :memory.upgrade/repository (:repository target) :memory.upgrade/store (:store target)
     :memory.upgrade/at at :memory.upgrade/state :preparing
     :memory.upgrade/basis (:basis inspection) :memory.upgrade/source (:source inspection)
     :memory.upgrade/definition-revision (:definition-revision inspection)
     :memory.upgrade/control-basis (:control-basis inspection) :memory.upgrade/plan (:plan inspection)}))

(defn- record-tx [db row]
  (let [room (:memory.upgrade/room row) actor (:memory.upgrade/by row)
        current (room-state db room)]
    (authorize! db actor room) (idle! db room)
    (when-not (and (= (:memory.upgrade/control-basis row)
                     (hasch/uuid [:simmis/memory-upgrade-control-v1 current]))
                   (nil? (:room/knowledge-upgrade (d/entity db [:room/id room])))
                   (nil? (receipts/in db (:memory.upgrade/id row)))) (held! :control-changed))
    [row {:db/id (:db/id (d/entity db [:room/id room])) :room/knowledge-upgrade (:memory.upgrade/id row)}]))

(defn- transition-tx [db id expected next-state]
  (let [row (receipts/in db id) room (:memory.upgrade/room row)
        target (get-in (room-state db room) [:room/default-repository])]
    (authorize! db (:memory.upgrade/by row) room) (idle! db room)
    (when-not (and (= expected (:memory.upgrade/state row))
                   (= id (:room/knowledge-upgrade (d/entity db [:room/id room])))
                   (= (:memory.upgrade/control-basis row)
                      (hasch/uuid [:simmis/memory-upgrade-control-v1 (room-state db room)])))
      (held! :control-changed))
    (cond-> [{:db/id (:db/id row) :memory.upgrade/state next-state}]
      (= next-state :published)
      (conj {:db/id (:db/id (d/entity db [:system/id (:system/id target)]))
             :system/repository-profile space/profile}))))

(defn- verify-repository! [repo-conn row]
  (let [db @repo-conn repository (query/repository db)]
    (when-not (and (= (:memory.upgrade/id row) (:document.repository/upgrade repository))
                   (= (:memory.upgrade/basis row) (:document.repository/upgrade-basis repository))
                   (= (:memory.upgrade/repository row) (:geschichte.repo/id repository))
                   (= (:memory.upgrade/store row) (get-in db [:config :store :id]))
                   (= (:memory.upgrade/source row) (foundation/source-id db)))
      (held! :repository-receipt-or-source-mismatch))
    (profile/validate-foundation! db)
    (when-not (= :ready (:status (view/prepare db profile/selection-options)))
      (held! :invalid-documents))
    (profile/prepare-open! repo-conn {:profile space/profile})))

(defn recover!
  "Explicit inspection of a recorded intent and actual repository receipt.
   Never infer installation from a journal phase or replay an uncertain write."
  [control-conn repo-conn id]
  (let [row (or (receipts/in @control-conn id) (held! :missing-intent))
        actor (:memory.upgrade/by row) room (:memory.upgrade/room row)
        expected {:system (:memory.upgrade/system row) :scope (:memory.upgrade/system-scope row)
                  :repository (:memory.upgrade/repository row) :store (:memory.upgrade/store row)}
        _ (target! @control-conn @repo-conn actor room expected)
        _ (when-not (= (:memory.upgrade/definition-revision row) (foundation/definition-revision))
            (held! :implementation-changed))
        repository (query/repository @repo-conn)]
    (if (= :published (:memory.upgrade/state row))
      (do (when-not (and (receipts/published-for? @control-conn room (:system expected))
                         (= space/profile (get-in (room-state @control-conn room)
                                                   [:room/default-repository :system/repository-profile])))
            (held! :publication-mismatch))
          (verify-repository! repo-conn row))
      (do
        (when-not (:document.repository/upgrade repository)
          (when-not (= :preparing (:memory.upgrade/state row)) (held! :missing-repository-receipt))
          (let [plan (foundation/prepare @repo-conn id (:memory.upgrade/at row))]
            (when-not (and (= (:memory.upgrade/basis row) (:basis plan))
                           (= (:memory.upgrade/plan row) (hasch/uuid plan))) (held! :source-or-plan-changed))
            (foundation/install! repo-conn plan)))
        (verify-repository! repo-conn row)
        (when (= :preparing (:memory.upgrade/state row))
          (d/transact control-conn [[:db.fn/call transition-tx id :preparing :repository-ready]]))
        (d/transact control-conn [[:db.fn/call transition-tx id :repository-ready :published]])))
    {:status :published :id id :room room :repository (:repository expected)}))

(defn upgrade!
  "Apply a fresh inspected intent once. After an uncertain outcome call
   recover!, never this entrypoint with a new identity. Service must stay stopped."
  [control-conn repo-conn inspection]
  (let [fresh (inspect! control-conn repo-conn (:request inspection))]
    (when-not (= inspection fresh) (held! :inspection-changed))
    (d/transact control-conn receipts/schema)
    (d/transact control-conn [[:db.fn/call record-tx (row-for inspection)]])
    (recover! control-conn repo-conn (get-in inspection [:request :id]))))
