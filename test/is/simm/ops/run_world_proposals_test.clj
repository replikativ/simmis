(ns is.simm.ops.run-world-proposals-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [dvergr.agent.run :as agent-run]
            [dvergr.discourse :as discourse]
            [dvergr.room.registry :as room-registry]
            [dvergr.rooms.forks :as room-forks]
            [is.simm.agents.room-agents :as room-agents]
            [is.simm.model.system-db :as sdb]
            [is.simm.ops.proposals :as proposals]
            [is.simm.ops.run-world-proposals :as world-proposals]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.yggdrasil :as ygg]))

(defn- fresh-system-db []
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (doto (d/connect cfg) (d/transact sdb/schema))))

(defn- live-registry []
  @(var-get #'is.simm.ops.run-world-proposals/live-adoptions))

(use-fixtures :each
  (fn [f]
    (reset! (var-get #'is.simm.ops.run-world-proposals/live-adoptions) {})
    (try (f)
         (finally
           (reset! (var-get #'is.simm.ops.run-world-proposals/live-adoptions) {})))))

(defn- fixture-world []
  (let [room-id (random-uuid)
        runtime-room-id :proposal-room
        run-id (random-uuid)
        world-id :proposal-room_run_world
        room {:id runtime-room-id :ctx (context/create-execution-context)}
        run {:run/id run-id :run/world world-id :run/settlement-status :review}
        world {:id world-id :parent-id runtime-room-id
               :meta (atom {:run-id run-id})}]
    {:room-id room-id :run-id run-id :room room :run run :world world}))

(defn- with-promotion-mocks [sys-conn fixture f]
  (let [{:keys [room-id run-id room run world]} fixture]
    (with-redefs [sdb/get-conn (constantly sys-conn)
                  room-agents/live-room #(when (= room-id %) room)
                  agent-run/run (fn [r id]
                                  (when (and (= room r) (= run-id id)) run))
                  room-registry/lookup #(when (= (:id world) %) world)
                  discourse/fork-descriptor
                  (constantly {:fork/systems {:messages {} :knowledge {}}})
                  room-forks/adopt!
                  (fn [_ owner {:keys [prepare!]}]
                    (let [descriptor {:fork/id (random-uuid)
                                      :fork/owner owner :fork/status :open
                                      :fork/systems {:messages {} :knowledge {}}
                                      :dvergr/room-id (:id world)
                                      :dvergr/parent-room-id (:parent-id world)}]
                      (prepare! descriptor)
                      {:ok? true :fork/descriptor descriptor
                       :fork/handle :root :fork/room-id (:id world)}))
                  room-forks/partition-adoption!
                  (fn [transfer partitions {:keys [prepare! commit!]}]
                    (prepare! {:fork/descriptor (:fork/descriptor transfer)
                               :fork/partitions partitions})
                    (let [nodes (mapv (fn [{:keys [systems owner]}]
                                        {:fork/handle (first systems)
                                         :fork/descriptor
                                         (assoc (:fork/descriptor transfer)
                                                :fork/settlement-id (random-uuid)
                                                :fork/owner owner
                                                :fork/systems
                                                (select-keys
                                                 (get-in transfer
                                                         [:fork/descriptor :fork/systems])
                                                 systems))})
                                      partitions)]
                      (commit! :receipt (mapv :fork/descriptor nodes))
                      (assoc transfer :fork/partitions nodes
                             :fork/partition-commit {:status :committed})))
                  ygg/fork-descriptor
                  (fn [handle]
                    (let [[_ live] (first
                                    (filter (fn [[_ entry]]
                                              (some #(= handle
                                                        (get-in % [:node :fork/handle]))
                                                    (vals (:components entry))))
                                            (live-registry)))
                          component (some #(when (= handle
                                                   (get-in % [:node :fork/handle])) %)
                                          (vals (:components live)))]
                      (or (some-> component :node :fork/descriptor)
                          {:fork/id (random-uuid) :fork/status :open})))
                  ygg/fork-conflicts (constantly [])
                  ygg/fork-diff (constantly {:messages {:summary {:added-datoms 1}}})]
      (f))))

(deftest promotion-files-exact-partition-components-before-exposing-proposal
  (let [sys-conn (fresh-system-db)
        fixture (fixture-world)]
    (with-promotion-mocks
      sys-conn fixture
      (fn []
        (let [{:keys [proposal-id adoption-id status]}
              (world-proposals/promote!
               (:room-id fixture) (:run-id fixture)
               {:title "Investigate in isolation" :summary "Two substrates"})
              proposal (proposals/get-proposal proposal-id)
              adoption (d/pull @sys-conn '[*]
                               [:world-adoption/id adoption-id])]
          (is (= :open status))
          (is (= (:run-id fixture) (:proposal/run proposal)))
          (is (= 2 (count (:proposal/forks proposal))))
          (is (= #{":messages" ":knowledge"}
                 (set (map :proposal.fork/world-system-id
                           (:proposal/forks proposal)))))
          (is (every? :proposal.fork/settlement-id (:proposal/forks proposal)))
          (is (every? #(= (:room-id fixture)
                          (:proposal.fork/authority-scope %))
                      (:proposal/forks proposal)))
          (is (= :open (:world-adoption/status adoption)))
          (is (world-proposals/live-proposal? proposal-id))
          (is (every? :proposal.fork/capability-live?
                      (:proposal/forks
                       (first (proposals/with-capability-availability
                               [proposal]))))))))))

(deftest partition-construction-failure-retains-transferred-root-for-recovery
  (let [sys-conn (fresh-system-db)
        fixture (fixture-world)]
    (with-promotion-mocks
      sys-conn fixture
      (fn []
        (let [partition! room-forks/partition-adoption!
              attempts (atom 0)
              promoted
              (with-redefs [room-forks/partition-adoption!
                            (fn [& args]
                              (if (= 1 (swap! attempts inc))
                                (throw (ex-info "partition unavailable" {}))
                                (apply partition! args)))]
                (world-proposals/promote!
                 (:room-id fixture) (:run-id fixture)
                 {:title "Recover partition construction"}))
              {:keys [adoption-id proposal-id]} promoted
              retained (get (live-registry) adoption-id)]
          (is (= :recovery-required (:status promoted)))
          (is (nil? (proposals/get-proposal proposal-id)))
          (is (:needs-partition? retained))
          (is (= :recovery-required
                 (:world-adoption/status
                  (d/pull @sys-conn '[*]
                          [:world-adoption/id adoption-id]))))
          (with-redefs [room-forks/partition-adoption! partition!]
            (is (= :open
                   (:status (world-proposals/retry-promotion! adoption-id)))))
          (is (= 2 (count (:proposal/forks
                           (proposals/get-proposal proposal-id)))))
          (is (not (:needs-partition?
                    (get (live-registry) adoption-id)))))))))

(deftest failed-pre-transfer-adoption-reuses-its-durable-run-identity
  (let [sys-conn (fresh-system-db)
        fixture (fixture-world)]
    (with-promotion-mocks
      sys-conn fixture
      (fn []
        (let [adopt! room-forks/adopt!
              attempts (atom 0)]
          (with-redefs [room-forks/adopt!
                        (fn [world owner {:keys [prepare! abort!] :as lifecycle}]
                          (if (= 1 (swap! attempts inc))
                            (let [descriptor
                                  {:fork/id (random-uuid)
                                   :fork/owner owner :fork/status :open
                                   :fork/systems {:messages {} :knowledge {}}}]
                              (prepare! descriptor)
                              (abort! :receipt)
                              {:ok? false :error
                               (ex-info "transfer refused" {})})
                            (adopt! world owner lifecycle)))]
            (is (thrown-with-msg?
                 clojure.lang.ExceptionInfo #"adoption failed"
                 (world-proposals/promote!
                  (:room-id fixture) (:run-id fixture)
                  {:title "First transfer attempt"})))
            (let [failed-id
                  (:world-adoption/id
                   (d/q '[:find (pull ?adoption [*]) .
                          :in $ ?run
                          :where [?adoption :world-adoption/run ?run]]
                        @sys-conn (:run-id fixture)))
                  retried (world-proposals/promote!
                           (:room-id fixture) (:run-id fixture)
                           {:title "Retry transfer"})]
              (is (= :open (:status retried)))
              (is (= failed-id (:adoption-id retried)))
              (is (= 1 (d/q '[:find (count ?adoption) .
                              :where [?adoption :world-adoption/id]]
                            @sys-conn))))))))))

(deftest released-affine-capability-is-dropped-even-if-status-projection-fails
  (let [sys-conn (fresh-system-db)
        fixture (fixture-world)
        released (atom 0)
        transact! d/transact
        transactions (atom 0)]
    (with-promotion-mocks
      sys-conn fixture
      (fn []
        (let [{:keys [proposal-id adoption-id]}
              (world-proposals/promote! (:room-id fixture) (:run-id fixture)
                                        {:title "Release exactly once"})]
          (with-redefs [room-forks/release-adoption!
                        (fn [_] (swap! released inc))
                        d/transact
                        (fn [& args]
                          (if (= 2 (swap! transactions inc))
                            (throw (ex-info "projection unavailable" {}))
                            (apply transact! args)))]
            (is (true? (world-proposals/release-proposal! proposal-id))))
          (is (= 1 @released))
          (is (false? (world-proposals/live-proposal? proposal-id)))
          (is (= :releasing
                 (:world-adoption/status
                  (d/pull @sys-conn '[*]
                          [:world-adoption/id adoption-id]))))
          (is (nil? (world-proposals/release-proposal! proposal-id)))
          (is (= 1 @released)))))))

(deftest durable-settlement-failure-never-becomes-a-dismissal
  (let [sys-conn (fresh-system-db)
        fixture (fixture-world)]
    (with-promotion-mocks
      sys-conn fixture
      (fn []
        (let [{:keys [proposal-id]}
              (world-proposals/promote! (:room-id fixture) (:run-id fixture)
                                        {:title "Govern this world"})
              fork (first (:proposal/forks (proposals/get-proposal proposal-id)))]
          (with-redefs [room-forks/settle-adoption!
                        (fn [node operation {:keys [prepare!]}]
                          (let [receipt (prepare! {:fork/operation operation})]
                            (assoc node :fork/settlement
                                   {:status :commit-failed
                                    :operation operation
                                    :receipt receipt
                                    :commit-value
                                    {:fork/operation operation
                                     :fork/descriptor (:fork/descriptor node)}})))]
            (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                  #"durable recovery"
                                  (proposals/dismiss-fork!
                                   proposal-id
                                   (:proposal.fork/scope fork)
                                   (:proposal.fork/branch fork))))
            (let [unchanged (proposals/find-fork
                             (proposals/get-proposal proposal-id)
                             (:proposal.fork/scope fork)
                             (:proposal.fork/branch fork))]
              (is (nil? (:proposal.fork/status unchanged)))
              (is (= :open (:proposal/status
                            (proposals/get-proposal proposal-id))))
              (is (= :settling (:proposal.fork/settlement-state unchanged))))
            (with-redefs [room-forks/retry-settlement-commit!
                          (fn [node commit!]
                            (commit! (get-in node [:fork/settlement :receipt])
                                     (get-in node [:fork/settlement :commit-value]))
                            (assoc-in node [:fork/settlement :status] :committed))]
              (is (= :committed
                     (:status (world-proposals/retry-settlement!
                               (:proposal.fork/scope fork)))))
              (is (= :dismissed
                     (:proposal.fork/status
                      (proposals/find-fork
                       (proposals/get-proposal proposal-id)
                       (:proposal.fork/scope fork)
                       (:proposal.fork/branch fork))))))))))))

(deftest accepted-components-release-the-world-only-after-the-last-decision
  (let [sys-conn (fresh-system-db)
        fixture (fixture-world)
        released (atom 0)
        operations (atom [])]
    (with-promotion-mocks
      sys-conn fixture
      (fn []
        (let [{:keys [proposal-id]}
              (world-proposals/promote! (:room-id fixture) (:run-id fixture)
                                        {:title "Land both components"})
              forks (:proposal/forks (proposals/get-proposal proposal-id))]
          (with-redefs [room-forks/settle-adoption!
                        (fn [node operation {:keys [prepare! commit!]}]
                          (when (:fork/settlement node)
                            (throw (ex-info "capability already settled" {})))
                          (swap! operations conj operation)
                          (let [receipt (prepare! {:fork/operation operation})
                                terminal {:fork/operation operation
                                          :fork/descriptor (:fork/descriptor node)}]
                            (commit! receipt terminal)
                            (assoc node :fork/settlement {:status :committed})))
                        room-forks/release-adoption!
                        (fn [_] (swap! released inc))]
            (let [a (first forks)]
              (proposals/accept-fork! proposal-id
                                      (:proposal.fork/scope a)
                                      (:proposal.fork/branch a))
              (is (zero? @released)))
            (let [b (second forks)]
              (proposals/accept-fork! proposal-id
                                      (:proposal.fork/scope b)
                                      (:proposal.fork/branch b))
              (is (= 1 @released))
              (is (= [:merge :merge] @operations))
              (is (= :accepted (:proposal/status
                                (proposals/get-proposal proposal-id)))))))))))

(deftest whole-proposal-dismissal-releases-adopted-world-ancestry
  (let [sys-conn (fresh-system-db)
        fixture (fixture-world)
        released (atom 0)]
    (with-promotion-mocks
      sys-conn fixture
      (fn []
        (let [{:keys [proposal-id adoption-id]}
              (world-proposals/promote! (:room-id fixture) (:run-id fixture)
                                        {:title "Refuse the whole world"})]
          (with-redefs [room-forks/settle-adoption!
                        (fn [node operation {:keys [prepare! commit!]}]
                          (let [receipt (prepare! {:fork/operation operation})
                                terminal {:fork/operation operation
                                          :fork/descriptor (:fork/descriptor node)}]
                            (commit! receipt terminal)
                            (assoc node :fork/settlement {:status :committed})))
                        room-forks/release-adoption!
                        (fn [_] (swap! released inc))]
            (is (= {:status :dismissed}
                   (proposals/dismiss-proposal! proposal-id))))
          (is (= 1 @released))
          (is (false? (world-proposals/live-proposal? proposal-id)))
          (is (= :released
                 (:world-adoption/status
                  (d/pull @sys-conn '[*]
                          [:world-adoption/id adoption-id])))))))))
