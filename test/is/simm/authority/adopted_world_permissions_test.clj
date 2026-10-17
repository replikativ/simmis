(ns is.simm.authority.adopted-world-permissions-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.string :as str]
            [datahike.api :as d]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.run :as run]
            [dvergr.jobs :as jobs]
            [dvergr.resource :as resource]
            [dvergr.room.store :as room-store]
            [dvergr.substrate.datahike :as substrate]
            [dvergr.system.db :as registry]
            [is.simm.agents.room-agents :as agents]
            [is.simm.authority.world :as world]
            [is.simm.model.access :as access]
            [is.simm.ops.proposals :as proposals]
            [is.simm.ops.run-world-proposals :as adopted]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.work :as work]
            [is.simm.ops.work-test]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg]))

(use-fixtures :each
  (fn [f]
    ;; These tests may run in a live REPL. Never erase its adopted capabilities.
    (with-redefs-fn {(var is.simm.ops.run-world-proposals/live-adoptions) (atom {})} f)))

(defn world-fork [room-id runtime-id]
  {:proposal.fork/scope (random-uuid) :proposal.fork/branch "component"
   :proposal.fork/system-type :world :proposal.fork/authority-scope room-id
   :proposal.fork/world-system-id runtime-id})

(defn- install-registry-locators! [conn]
  ;; The shared authority fixture predates registry-backed runtime names.
  ;; Production provisioning always gives these systems real locators.
  (d/transact conn
              (mapv (fn [system]
                      {:system/id (:system/id system)
                       :system/scope (str "/fixture/" (:system/id system))})
                    (filter #(#{:kb :repo :msgs :data :book} (:system/type %))
                            (registry/all-systems)))))

(deftest adopted-world-uses-room-and-exact-target-authority-not-bare-room-uuid
  (world/with-world [conn w]
    (install-registry-locators! conn)
    (d/transact conn [{:system/id (:kb-granted-sys w) :system/scope "/fixture/handbook"}
                      {:system/id (:repo-sys w) :system/scope "/fixture/repository"}])
    (let [fork (world-fork (:room w) "room-kb-handbook")
          annotated (fn [party]
                      (-> (proposals/with-merge-authority @conn party [{:proposal/forks [fork]}])
                          first :proposal/forks first :proposal.fork/may-merge?))]
      (is (false? (access/can? @conn (:owner w) :merge (:room w))) "Room id is not a store scope")
      (is (true? (access/may-merge-proposal-fork? @conn (:owner w) fork)))
      (is (true? (annotated (:owner w))))
      (is (nil? (#'proposals/authorize-forks! (random-uuid) (:owner w) [fork])))
      (doseq [party [(:member w) (:non-member w) (:admin w) nil]]
        (is (false? (access/may-merge-proposal-fork? @conn party fork)))
        (is (false? (annotated party))))
      (doseq [bad [(dissoc fork :proposal.fork/authority-scope)
                   (assoc fork :proposal.fork/world-system-id "missing-system")
                   (assoc fork :proposal.fork/authority-scope (:other-room w))]]
        (is (false? (access/may-merge-proposal-fork? @conn (:owner w) bad))))
      (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:owner w)]]
                        [:db/add [:room/id (:room w)] :room/owner [:party/id (:non-member w)]]])
      (is (false? (annotated (:owner w))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (#'proposals/authorize-forks! (random-uuid) (:owner w) [fork]))))))

(deftest room-owner-with-only-read-write-on-another-persons-kb-cannot-land-it
  (world/with-world [conn w]
    (install-registry-locators! conn)
    (d/transact conn [{:system/id (:kb-stranger-sys w) :system/scope "/fixture/external-kb"}])
    (registry/attach! (:room w) (:kb-stranger-sys w) :read-write)
    (let [fork (world-fork (:room w) "room-kb-external-kb")]
      (is (access/can? @conn (:owner w) :merge {:room (:room w)}))
      (is (access/can? @conn (:owner w) :write (:kb-stranger-sys w)))
      (is (false? (access/may-merge-proposal-fork? @conn (:owner w) fork)))
      (is (thrown? clojure.lang.ExceptionInfo
                   (#'proposals/authorize-forks! (random-uuid) (:owner w) [fork])))
      (registry/attach! (:room w) (:kb-stranger-sys w) :merge)
      (is (true? (access/may-merge-proposal-fork? @conn (:owner w) fork)))
      (registry/detach! (:room w) (:kb-stranger-sys w))
      (is (false? (access/may-merge-proposal-fork? @conn (:owner w) fork))))))

(deftest registry-identity-collision-refuses-instead-of-selecting-another-grant
  (world/with-world [conn w]
    (install-registry-locators! conn)
    (d/transact conn [{:system/id (:kb-granted-sys w) :system/scope "/one/collision"}
                      {:system/id (:kb-stranger-sys w) :system/scope "/two/collision"}])
    (is (false? (access/may-merge-proposal-fork? @conn (:owner w)
                                               (world-fork (:room w) "room-kb-collision"))))))

(defn- with-retained-kb-world [conn w room f]
  (let [scope (str "/fixture/adopted-kb-" (random-uuid))
        sid (str "room-kb-" (last (str/split scope #"/")))
        cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
             :keep-history? true :commit-graph? true}
        target (substrate/provision!
                {:cfg cfg :system-name sid :schema? false
                 :extra-schema [{:db/ident :datum/id :db/valueType :db.type/uuid
                                 :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
                                {:db/ident :datum/text :db/valueType :db.type/string :db/cardinality :db.cardinality/one}]})]
    (try
      ;; All registry targets need the same canonical locators production has.
      (d/transact conn (mapv (fn [system]
                              {:system/id (:system/id system)
                               :system/scope (if (= (:system/id system) (:kb-granted-sys w)) scope
                                                 (str "/fixture/" (:system/id system)))})
                            (registry/all-systems)))
      (d/transact conn [{:room/id (:room w) :room/slug (room-store/room-id->slug (:id room))}])
      (registry/attach! (:room w) (:kb-granted-sys w) :read-write)
      (d/transact target [{:datum/id (:room w) :datum/text "parent"}])
      (let [[input prepared] (@#'is.simm.ops.work-test/fixture)
            input (-> input (assoc-in [:limits :settlement] "review") (assoc-in [:limits :timeout-ms] 10000))
            setup (evaluation/make-world-setup
                   {:id :fixture/adopted-kb :version 1 :basis sid
                    :prepare (fn [{world :room}]
                               (binding [ec/*execution-context* (:ctx world)]
                                 (d/transact (:conn (ygg/system sid)) [{:datum/id (:room w) :datum/text "proposed"}]))
                               {:seeded true})})
            base (:environment prepared)
            env (environment/make-environment
                 {:id :fixture/adopted-kb :task "Deterministic adoption fixture"
                  :verifier {:id :simmis/work-completion :version 1 :basis "simmis-work-completion-v1"}
                  :limits (assoc (:environment/limits base) :timeout-ms 10000)
                  :world (assoc (:environment/world base) :settlement :review
                                :setup (evaluation/world-setup-ref setup))})
            prepared (assoc prepared :environment env :world-setup setup)
            principal (ops/party-principal (:owner w))]
        (resource/mint! room {:id (random-uuid) :resources (work/resources (:limits input))})
        (let [admission (work/submit-prepared! conn ec/*execution-context* principal (:room w) room input prepared)
              status (jobs/status (:job admission) 25000)
              child (first (remove #(= (:job admission) (:run/id %))
                                   (run/runs room {:root-run-id (:job admission) :limit 10})))]
          (is (= :completed (:status status)))
          (is (= :review (:run/settlement-status child)))
          (with-redefs [agents/live-room #(when (= % (:room w)) room)]
            (let [promoted (adopted/promote! (:room w) (:run/id child)
                                           {:title "Permission fixture" :author (:owner w)})
                  pid (:proposal-id promoted)]
              (try
                (is (= :open (:status promoted)))
                (f target sid pid)
                (finally
                  (when (= :open (:proposal/status (proposals/get-proposal pid)))
                    (proposals/dismiss-proposal! pid :by (:owner w)))))))))
      (finally (d/release target) (d/delete-database cfg)))))

(deftest actual-authorized-singleton-proposal-lands-only-when-accepted
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [room]
       (with-retained-kb-world
         conn w room
         (fn [target sid pid]
           (let [fork (first (filter #(= sid (:proposal.fork/world-system-id %))
                                     (:proposal/forks (proposals/get-proposal pid))))]
             (is (= "parent" (d/q '[:find ?text . :where [_ :datum/text ?text]] @target)))
             (is (access/may-merge-proposal-fork? @conn (:owner w) fork))
             (is (= :accepted (:status (proposals/accept-fork! pid (:proposal.fork/scope fork)
                                                              (:proposal.fork/branch fork) :by (:owner w)))))
             (is (= "proposed" (d/q '[:find ?text . :where [_ :datum/text ?text]] @target)))
             (is (= :accepted (:proposal/status (proposals/get-proposal pid)))))))))))

(deftest actual-adopted-component-refuses-revoked-room-grants-before-settling-intent
  (doseq [revocation [:downgrade :detach]]
    (world/with-world [conn w]
      (@#'is.simm.ops.work-test/with-room
       (fn [room]
         (with-retained-kb-world
           conn w room
           (fn [target sid pid]
             (let [fork (first (filter #(= sid (:proposal.fork/world-system-id %))
                                       (:proposal/forks (proposals/get-proposal pid))))]
               (is fork)
               (is (access/may-merge-proposal-fork? @conn (:owner w) fork))
               (if (= revocation :downgrade)
                 (registry/attach! (:room w) (:kb-granted-sys w) :read)
                 (registry/detach! (:room w) (:kb-granted-sys w)))
               ;; Human still owns this KB and the room: the fresh ROOM grant
               ;; gate, not the earlier human affordance, must stop this merge.
               (is (access/may-merge-proposal-fork? @conn (:owner w) fork))
               (is (thrown-with-msg? clojure.lang.ExceptionInfo #"may no longer write"
                                     (proposals/accept-fork! pid (:proposal.fork/scope fork)
                                                             (:proposal.fork/branch fork) :by (:owner w))))
               (is (= "parent" (d/q '[:find ?text . :where [_ :datum/text ?text]] @target)))
               (let [after (d/pull @conn '[*] (:db/id fork))]
                 (is (nil? (:proposal.fork/status after)))
                 (is (= :open (:proposal.fork/settlement-state after))))
               (is (= :dismissed (:status (proposals/dismiss-fork! pid (:proposal.fork/scope fork)
                                                                  (:proposal.fork/branch fork) :by (:owner w)))))))))))))
