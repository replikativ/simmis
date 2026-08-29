(ns is.simm.ops.run-world-proposals
  "Bridge retained Dvergr Run worlds into Simmis's existing ForkSet governance.

   Dvergr/Spindel own world construction, affine partitioning and settlement.
   This namespace owns only the Simmis projection: durable adoption records,
   Proposal component rows and a process-local index of live capabilities."
  (:require [datahike.api :as d]
            [dvergr.agent.run :as agent-run]
            [dvergr.discourse :as discourse]
            [dvergr.room.registry :as room-registry]
            [dvergr.rooms.forks :as room-forks]
            [is.simm.agents.room-agents :as room-agents]
            [is.simm.model.system-db :as system-db]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg]
            [taoensso.telemere :as log]))

(defonce ^:private live-adoptions
  ;; {adoption-id {:proposal-id uuid :tree transferred-capability-tree
  ;;               :components {scope {:node path-independent-leaf ...}}}}
  ;; Handles are deliberately process-local. Durable rows contain descriptors.
  (atom {}))

(defn- conn [] (system-db/get-conn))
(defn- now [] (java.util.Date.))
(defn- descriptor-edn [descriptor] (pr-str descriptor))
(defn- capability-id [descriptor]
  (or (:fork/settlement-id descriptor) (:fork/id descriptor)))

(defn- adoption-tx!
  [adoption-id attrs]
  (d/transact (conn)
              [(merge {:world-adoption/id adoption-id
                       :world-adoption/updated-at (now)}
                      attrs)]))

(defn- clear-adoption-error!
  [adoption-id]
  (when-let [error (:world-adoption/error
                    (d/pull @(conn) '[:world-adoption/error]
                            [:world-adoption/id adoption-id]))]
    (d/transact (conn)
                [[:db/retract [:world-adoption/id adoption-id]
                  :world-adoption/error error]])))

(defn- record-recovery-required!
  [adoption-id error]
  (try
    (adoption-tx! adoption-id
                  {:world-adoption/status :recovery-required
                   :world-adoption/error (ex-message error)})
    (catch Throwable persistence-error
      (log/log! {:level :error :id ::recovery-projection-failed
                 :data {:adoption adoption-id
                        :error (ex-message error)
                        :persistence-error (ex-message persistence-error)}}))))

(defn- adoption-id-for-promotion!
  [run-id]
  (if-let [existing
           (d/q '[:find (pull ?adoption [*]) .
                  :in $ ?run
                  :where [?adoption :world-adoption/run ?run]]
                @(conn) run-id)]
    (if (= :failed (:world-adoption/status existing))
      (:world-adoption/id existing)
      (throw (ex-info "Run world already has an adoption"
                      {:run run-id
                       :adoption (:world-adoption/id existing)
                       :status (:world-adoption/status existing)})))
    (random-uuid)))

(defn- exact-retained-world!
  [room-id run-id]
  (let [room (or (room-agents/live-room room-id)
                 (throw (ex-info "Room is not live" {:room room-id})))
        run (or (agent-run/run room run-id)
                (throw (ex-info "Unknown Run in Room"
                                {:room room-id :run run-id})))
        world-id (:run/world run)]
    (when-not (and world-id (= :review (:run/settlement-status run)))
      (throw (ex-info "Run has no retained review world"
                      {:room room-id :run run-id
                       :settlement-status (:run/settlement-status run)})))
    (let [world (binding [ec/*execution-context* (:ctx room)]
                  (room-registry/lookup world-id))]
      (when-not (and world
                     (= (:id room) (:parent-id world))
                     (= run-id (some-> world :meta deref :run-id)))
        (throw (ex-info "Run world capability is unavailable or mismatched"
                        {:room room-id :run run-id :world world-id})))
      (when-not (seq (:fork/systems (discourse/fork-descriptor world)))
        (throw (ex-info "Run world has no forked substrate to propose"
                        {:room room-id :run run-id :world world-id})))
      {:room room :run run :world world})))

(defn- component-rows
  [descriptors author authority-scope]
  (mapv (fn [descriptor]
          (let [[system-id] (keys (:fork/systems descriptor))
                settlement-id (capability-id descriptor)]
            (when-not (= 1 (count (:fork/systems descriptor)))
              (throw (ex-info "A Proposal component must govern exactly one system"
                              {:systems (keys (:fork/systems descriptor))})))
            {:scope settlement-id
             :authority-scope authority-scope
             :branch (str settlement-id)
             :system-type :world
             :world-system-id system-id
             :settlement-id settlement-id
             :settlement-state :open
             :descriptor descriptor
             :author author}))
        descriptors))

(defn- install-live!
  [adoption-id proposal-id tree rows]
  (let [nodes (or (:fork/partitions tree) [tree])
        by-id (into {} (map (fn [node]
                              [(capability-id (:fork/descriptor node)) node])
                            nodes))
        components
        (into {} (map (fn [row]
                        (let [node (get by-id (:settlement-id row))]
                          [(:scope row)
                           {:node node
                            :settlement-id (:settlement-id row)
                            ;; Keep the exact in-memory id (keyword, string,
                            ;; etc.) from the authoritative descriptor. The DB
                            ;; string is a portable display/index projection.
                            :system-id (first
                                        (keys (get-in node
                                                      [:fork/descriptor
                                                       :fork/systems])))}]))
                      rows))]
    (when (some (comp nil? :node val) components)
      (throw (ex-info "Partition descriptors do not match live capabilities"
                      {:adoption adoption-id :proposal proposal-id})))
    (swap! live-adoptions assoc adoption-id
           {:proposal-id proposal-id :tree tree :components components})
    proposal-id))

(defn- file-adopted-proposal!
  [{:keys [adoption-id proposal-id room-id run-id title summary author intent]}
   descriptors]
  (let [rows (component-rows descriptors author room-id)
        file! (requiring-resolve 'is.simm.ops.proposals/file-proposal!)]
    ;; file-proposal! is one Datahike transaction. Settlement IDs are unique
    ;; identities, so retrying an ambiguous commit cannot duplicate components.
    (file! {:id proposal-id :title title :summary summary :author author
            :room room-id :run run-id :adoption adoption-id :intent intent
            :forks rows})
    (adoption-tx! adoption-id
                  {:world-adoption/status :open
                   :world-adoption/proposal [:proposal/id proposal-id]})
    (clear-adoption-error! adoption-id)
    rows))

(defn- partition-and-file!
  [transfer params systems]
  (let [{:keys [adoption-id proposal-id]} params]
    (room-forks/partition-adoption!
     transfer
     (mapv (fn [system-id]
             {:systems #{system-id}
              :owner [:proposal/id proposal-id]
              :purpose :proposal-component})
           systems)
     {:prepare! (fn [plan]
                  (adoption-tx!
                   adoption-id
                   {:world-adoption/status :partitioning
                    :world-adoption/descriptor
                    (descriptor-edn (:fork/descriptor plan))})
                  adoption-id)
      :abort! (fn [_]
                (adoption-tx! adoption-id
                              {:world-adoption/status :prepared}))
      :commit! (fn [_ descriptors]
                 (file-adopted-proposal! params descriptors))})))

(defn- proposal-rows
  [proposal-id]
  (let [proposal ((requiring-resolve 'is.simm.ops.proposals/get-proposal)
                  proposal-id)]
    (mapv (fn [fork]
            {:scope (:proposal.fork/scope fork)
             :settlement-id (:proposal.fork/settlement-id fork)
             :world-system-id (:proposal.fork/world-system-id fork)})
          (:proposal/forks proposal))))

(defn- retain-promotion-recovery!
  [adoption-id proposal-id tree pending error & [extra]]
  (record-recovery-required! adoption-id error)
  (swap! live-adoptions assoc adoption-id
         (merge {:proposal-id proposal-id :tree tree :pending pending} extra))
  {:status :recovery-required :tree tree :error error})

(defn- expose-partitioned!
  [adoption-id proposal-id pending partitioned]
  (let [status (get-in partitioned [:fork/partition-commit :status])]
    (if (= :committed status)
      (try
        (let [rows (proposal-rows proposal-id)]
          (install-live! adoption-id proposal-id partitioned rows)
          {:status :open :tree partitioned :rows rows})
        (catch Throwable error
          ;; The partition capability and Proposal already exist. Retain that
          ;; exact tree so recovery only rebuilds the process-local index.
          (retain-promotion-recovery! adoption-id proposal-id partitioned
                                      pending error)))
      (let [error (or (get-in partitioned [:fork/partition-commit :error])
                      (ex-info "Partition projection did not commit"
                               {:status status}))]
        (retain-promotion-recovery! adoption-id proposal-id partitioned
                                    pending error)))))

(defn promote!
  "Promote one exact retained Run world into a durable Simmis Proposal.

   The adoption row is committed before affine authority leaves Dvergr. Worlds
   with multiple systems are exhaustively partitioned into independently
   reviewable components; the Proposal becomes visible only after the exact
   child descriptors commit. Returns ids and status, never a live handle."
  [room-id run-id {:keys [title summary author intent]}]
  {:pre [(string? title)]}
  (let [{:keys [world]} (exact-retained-world! room-id run-id)
        ;; :world-adoption/run is unique. A pre-transfer abort is safe to retry
        ;; using that same durable identity; every later state means ownership
        ;; was or may have been transferred and must use the recovery API.
        adoption-id (adoption-id-for-promotion! run-id)
        proposal-id (random-uuid)
        owner [:world-adoption/id adoption-id]
        params {:adoption-id adoption-id :proposal-id proposal-id
                :room-id room-id :run-id run-id :title title :summary summary
                :author author :intent intent}
        transfer
        (room-forks/adopt!
         world owner
         {:prepare! (fn [descriptor]
                      (adoption-tx!
                       adoption-id
                       {:world-adoption/run run-id
                        :world-adoption/room room-id
                        :world-adoption/status :prepared
                        :world-adoption/descriptor (descriptor-edn descriptor)})
                      adoption-id)
          :abort! (fn [_]
                    (adoption-tx! adoption-id
                                  {:world-adoption/status :failed}))})]
    (when-not (:ok? transfer)
      (throw (ex-info "Dvergr world adoption failed"
                      {:adoption adoption-id :run run-id :error (:error transfer)})))
    (let [systems (keys (get-in transfer [:fork/descriptor :fork/systems]))
          result
          (if (= 1 (count systems))
            (try
              (let [rows (file-adopted-proposal! params [(:fork/descriptor transfer)])]
                (install-live! adoption-id proposal-id transfer rows)
                {:status :open :tree transfer :rows rows})
              (catch Throwable error
                (record-recovery-required! adoption-id error)
                (swap! live-adoptions assoc adoption-id
                       {:proposal-id proposal-id :tree transfer :pending params})
                {:status :recovery-required :error error :tree transfer}))
            (try
              (expose-partitioned!
               adoption-id proposal-id params
               (partition-and-file! transfer params systems))
              (catch Throwable error
                ;; Affine adoption already succeeded. Preserve the unpartitioned
                ;; transferred root so recovery partitions it exactly once; never
                ;; fall back to filing a multi-system component.
                (retain-promotion-recovery!
                 adoption-id proposal-id transfer params error
                 {:needs-partition? true}))))]
      (log/log! {:level (if (= :open (:status result)) :info :warn)
                 :id ::promoted
                 :data {:adoption adoption-id :proposal proposal-id :run run-id
                        :components (count systems) :status (:status result)}})
      {:adoption-id adoption-id :proposal-id proposal-id
       :status (:status result)})))

(defn- adoption-for-fork [fork]
  (let [proposal-id (d/q '[:find ?pid .
                           :in $ ?fork
                           :where
                           [?p :proposal/forks ?fork]
                           [?p :proposal/id ?pid]]
                         @(conn) (:db/id fork))]
    (some (fn [[adoption-id live]]
            (when (= proposal-id (:proposal-id live)) [adoption-id live]))
          @live-adoptions)))

(defn- live-component! [fork]
  (let [scope (:proposal.fork/scope fork)
        [adoption-id live] (or (adoption-for-fork fork)
                               (throw (ex-info "Adopted world capability is unavailable"
                                               {:scope scope})))
        component (get-in live [:components scope])]
    (when-not (and component
                   (= (:proposal.fork/settlement-id fork)
                      (:settlement-id component)))
      (throw (ex-info "Proposal component does not name the live affine capability"
                      {:scope scope :adoption adoption-id})))
    [adoption-id live component]))

(defn- fork-for-scope! [scope]
  (let [eid (d/q '[:find ?f . :in $ ?scope
                   :where [?f :proposal.fork/scope ?scope]]
                 @(conn) scope)]
    (or (when eid
          (d/pull @(conn) '[*] eid))
        (throw (ex-info "Unknown adopted-world component" {:scope scope})))))

(defn head-id [scope branch]
  (let [fork (fork-for-scope! scope)
        [_ _ {:keys [node]}] (live-component! fork)
        descriptor (ygg/fork-descriptor (:fork/handle node))]
    (str (if (= branch :world/trunk) "base" "head") ":"
         (capability-id descriptor) ":" (:fork/status descriptor))))

(defn delta [scope]
  (let [[_ _ {:keys [node system-id]}]
        (live-component! (fork-for-scope! scope))]
    (get (ygg/fork-diff (:fork/handle node)) system-id)))

(defn conflicts [scope]
  (let [[_ _ {:keys [node]}]
        (live-component! (fork-for-scope! scope))]
    (vec (ygg/fork-conflicts (:fork/handle node)))))

(defn- replace-node [tree settlement-id replacement]
  (cond
    (= settlement-id (capability-id (:fork/descriptor tree))) replacement
    (seq (:fork/partitions tree))
    (update tree :fork/partitions
            #(mapv (fn [node] (replace-node node settlement-id replacement)) %))
    :else tree))

(defn settle!
  "Settle one adopted Proposal component through Dvergr's durable frontier."
  [fork operation]
  (let [[adoption-id live {:keys [node settlement-id]}] (live-component! fork)
        fork-eid (:db/id fork)
        lifecycle
        {:prepare! (fn [_]
                     (d/transact (conn)
                                 [{:db/id fork-eid
                                   :proposal.fork/settlement-state :settling
                                   :proposal.fork/settlement-operation operation}])
                     settlement-id)
         :abort! (fn [_]
                   (d/transact (conn)
                               [[:db/retract fork-eid
                                 :proposal.fork/settlement-state :settling]
                                [:db/retract fork-eid
                                 :proposal.fork/settlement-operation operation]]))
         :commit! (fn [_ terminal]
                    (d/transact
                     (conn)
                     [{:db/id fork-eid
                       :proposal.fork/status (if (= operation :merge)
                                               :accepted :dismissed)
                       :proposal.fork/settlement-state :committed
                       :proposal.fork/settlement-operation operation
                       :proposal.fork/descriptor
                       (descriptor-edn (:fork/descriptor terminal))}]))}
        settled (room-forks/settle-adoption! node operation lifecycle)
        status (get-in settled [:fork/settlement :status])]
    (swap! live-adoptions update adoption-id
           (fn [entry]
             (-> entry
                 (assoc :tree (replace-node (:tree entry) settlement-id settled))
                 (assoc-in [:components (:proposal.fork/scope fork) :node] settled))))
    (when-not (= :committed status)
      (throw (ex-info "World settlement needs durable recovery"
                      {:adoption adoption-id :scope (:proposal.fork/scope fork)
                       :status status})))
    settled))

(defn settle-scope!
  "Neutral branching backend entry point."
  [scope operation]
  (settle! (fork-for-scope! scope) operation))

(defn retry-promotion!
  "Recover Proposal promotion after affine authority transfer. Adoption is
   never repeated. If partition construction failed, partition the retained
   transferred root; otherwise retry only its durable commit callback."
  [adoption-id]
  (let [{:keys [proposal-id tree pending needs-partition?]}
        (or (get @live-adoptions adoption-id)
            (throw (ex-info "No live adoption recovery capability"
                            {:adoption adoption-id})))
        commit! (fn [_ descriptors]
                  (file-adopted-proposal! pending descriptors))
        partitioned
        (if needs-partition?
          (try
            (partition-and-file!
             tree pending
             (keys (get-in tree [:fork/descriptor :fork/systems])))
            (catch Throwable error
              (record-recovery-required! adoption-id error)
              (throw error)))
          tree)
        recovered
        (if (= :failed (get-in partitioned [:fork/partition-commit :status]))
          (try
            (room-forks/retry-partition-commit! partitioned commit!)
            (catch Throwable error
              (record-recovery-required! adoption-id error)
              (throw error)))
          partitioned)
        descriptors (mapv :fork/descriptor
                          (or (:fork/partitions recovered) [recovered]))
        partition-tree? (seq (:fork/partitions recovered))]
    (if partition-tree?
      (let [result (expose-partitioned! adoption-id proposal-id pending recovered)]
        {:adoption-id adoption-id :proposal-id proposal-id
         :status (:status result)})
      (try
        (when-not ((requiring-resolve 'is.simm.ops.proposals/get-proposal)
                   proposal-id)
          (file-adopted-proposal! pending descriptors))
        (let [rows (proposal-rows proposal-id)]
          (install-live! adoption-id proposal-id recovered rows)
          (adoption-tx! adoption-id {:world-adoption/status :open})
          (clear-adoption-error! adoption-id)
          {:adoption-id adoption-id :proposal-id proposal-id :status :open})
        (catch Throwable error
          (retain-promotion-recovery! adoption-id proposal-id recovered pending error)
          {:adoption-id adoption-id :proposal-id proposal-id
           :status :recovery-required})))))

(defn retry-settlement!
  "Recover a failed durable settlement commit or compensation for one scope.
   Only persistence callbacks are replayed; affine substrate settlement is not."
  [scope]
  (let [fork (fork-for-scope! scope)
        [adoption-id live {:keys [node settlement-id]}] (live-component! fork)
        fork-eid (:db/id fork)
        {:keys [status operation]} (:fork/settlement node)
        commit! (fn [_ terminal]
                  (d/transact
                   (conn)
                   [{:db/id fork-eid
                     :proposal.fork/status (if (= operation :merge)
                                             :accepted :dismissed)
                     :proposal.fork/settlement-state :committed
                     :proposal.fork/settlement-operation operation
                     :proposal.fork/descriptor
                     (descriptor-edn (:fork/descriptor terminal))}]))
        abort! (fn [_]
                 (d/transact
                  (conn)
                  [[:db/retract fork-eid :proposal.fork/settlement-state
                    :settling]
                   [:db/retract fork-eid :proposal.fork/settlement-operation
                    operation]]))
        recovered (case status
                    :commit-failed
                    (room-forks/retry-settlement-commit! node commit!)

                    :abort-failed
                    (room-forks/retry-settlement-abort! node abort!)

                    (throw (ex-info "World component has no retryable settlement"
                                    {:scope scope :status status}))) ]
    (swap! live-adoptions update adoption-id
           (fn [entry]
             (-> entry
                 (assoc :tree (replace-node (:tree entry) settlement-id recovered))
                 (assoc-in [:components scope :node] recovered))))
    (when (= :commit-failed status)
      ((requiring-resolve 'is.simm.ops.proposals/reconcile-status!)
       (:proposal-id live)))
    {:scope scope :status (if (= :commit-failed status) :committed :open)}))

(defn release-proposal!
  "Release Dvergr ancestry after every component decision is durable."
  [proposal-id]
  (when-let [[adoption-id live]
             (some (fn [[aid entry]]
                     (when (= proposal-id (:proposal-id entry)) [aid entry]))
                   @live-adoptions)]
    (try
      ;; Persist release intent before consuming ancestry. If this write fails,
      ;; the capability remains live and can be retried safely. If the final
      ;; projection fails after release, :releasing truthfully records the
      ;; durable ambiguity rather than incorrectly leaving :open.
      (adoption-tx! adoption-id {:world-adoption/status :releasing})
      (room-forks/release-adoption! (:tree live))
      ;; release-adoption! consumes the ancestry capability. Drop it before
      ;; projecting status so a Datahike failure cannot offer an invalid retry.
      (swap! live-adoptions dissoc adoption-id)
      (try
        (adoption-tx! adoption-id {:world-adoption/status :released})
        (clear-adoption-error! adoption-id)
        (catch Throwable error
          (log/log! {:level :error :id ::release-projection-failed
                     :data {:proposal proposal-id :adoption adoption-id
                            :error (ex-message error)}})))
      true
      (catch Throwable error
        ;; The affine capability is still live only when substrate release itself
        ;; failed. Keep it for an explicit retry and project that state best-effort.
        (record-recovery-required! adoption-id error)
        (log/log! {:level :error :id ::release-failed
                   :data {:proposal proposal-id :adoption adoption-id
                          :error (ex-message error)}})
        false))))

(defn live-proposal?
  "True only when this process holds the exact Proposal capability tree."
  [proposal-id]
  (boolean (some #(= proposal-id (:proposal-id %)) (vals @live-adoptions))))

(defn live-scope?
  "True only when this process holds the exact affine component capability."
  [scope]
  (try
    (boolean (live-component! (fork-for-scope! scope)))
    (catch Throwable _ false)))
