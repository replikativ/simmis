(ns is.simm.ops.benchmarks-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.experiment.runner :as runner]
            [dvergr.agent.roster :as roster]
            [dvergr.agent.trajectory :as trajectory]
            [dvergr.agent.workflow :as workflow]
            [dvergr.catalog.room :as catalog]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.jobs :as jobs]
            [dvergr.resource :as resource]
            [dvergr.room.store :as store]
            [dvergr.room.store.memory :as memory]
            [is.simm.authority.world :as world]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.work :as work]
            [is.simm.ops.work-test]
            [is.simm.ops.workflows :as workflows]
            [is.simm.ops.workflows-test :as fixture]
            [org.replikativ.spindel.engine.core :as ec]))

(use-fixtures :each
  (fn [f]
    ;; Tests may run inside a live dev server; own the physical capacity cell.
    (with-redefs-fn {#'benchmarks/checker-slot (java.util.concurrent.Semaphore. 1)} f)))

(defn snapshot [files]
  {:files files :manifest (mapv (fn [[path text]] {:path path :size (count (.getBytes text "UTF-8"))}) files)})

(defn input [files]
  {:request-id (random-uuid) :name "echo" :expected-bundle (:id (catalog/bundle "echo" files))
   :models ["fixture-model"] :repetitions 1
   :limits {:budget-microdollars 10000 :max-dispatches 4 :max-model-steps 3
            :timeout-ms 2000 :settlement "discard"}})

(defn candidates [{:keys [models]}]
  (let [ids (mapv #(keyword (str "candidate-" %)) (range (count models)))]
    {:ids ids :team (reduce (fn [team [id model]]
                              (roster/make-agent team {:id id :program {:kind :echo}
                                                       :model-policy {:model model :provider :openai}}))
                            (roster/make-roster {:id :fixture/team}) (map vector ids models))}))

(defn prepared [files request]
  (with-redefs [workflow/candidates candidates runner/runtime-identity (constantly {:libraries {}})]
    (benchmarks/prepare (snapshot files) request)))

(deftest benchmark-pins-existing-experiment-definitions-and-conserved-cell-limits
  (let [files (merge fixture/fixture {"cases/a/gold.edn" "{:expected 1}"
                                      "cases/b/gold.edn" "{:expected 2}"})
        request (-> (input files)
                    (assoc :models ["fixture-one" "fixture-two"] :repetitions 2)
                    (assoc-in [:limits :max-dispatches] 8))
        result (prepared files request)
        evaluator (get-in result [:plan :capabilities :evaluator])]
    (is (= 8 (:cells result)))
    (is (= 1250 (get-in result [:cell-limits :budget-microdollars])))
    (is (= 1 (get-in result [:cell-limits :max-dispatches])))
    (is (= :ad-hoc (:tier evaluator)))
    (doseq [env (get-in result [:plan :environments])]
      (is (= work/effects (get-in env [:environment/world :effects])))
      (is (= (:verifier/basis (evaluation/evaluator-ref evaluator))
             (get-in env [:environment/verifier :verifier/basis])))
      (is (= env (environment/validate-environment env))))))

(deftest preparation-rejects-revision-case-model-and-budget-ambiguity
  (let [files fixture/fixture
        request (input files)]
    (doseq [bad [(assoc request :expected-bundle (random-uuid))
                 (assoc request :models ["same" "same"])
                 (assoc request :cases ["missing"])
                 (assoc-in request [:limits :settlement] "review")
                 (assoc request :models ["a" "b"] :repetitions 2
                        :limits (assoc (:limits request) :max-dispatches 1))]]
      (is (thrown? clojure.lang.ExceptionInfo (prepared files bad))))))

(deftest unsafe-bundle-policies-are-not-carried-into-benchmark-worlds
  (doseq [extra [":judge {:model \"extra-spend\"}" ":capture [\"/\"]"
                 ":web :live" ":profile \"admin\"" ":allowed-sources [\"example.com\"]"]]
    (let [files (assoc fixture/fixture "workflow.edn"
                       (str "{:title \"Echo\" :task \"Write /out/result.md\" " extra "}"))]
      (is (thrown? clojure.lang.ExceptionInfo (prepared files (input files)))))))

(deftest rendered-task-is-bounded-before-candidate-preparation
  (let [files (assoc fixture/fixture "workflow.edn"
                     (pr-str {:title "Large expansion" :task "{payload}"
                              :params {:payload (apply str (repeat 16001 "x"))}}))]
    (with-redefs [workflow/candidates (fn [& _] (throw (AssertionError. "must refuse before resolving candidates")))]
      (is (thrown? clojure.lang.ExceptionInfo (benchmarks/prepare (snapshot files) (input files)))))))

(deftest checker-execution-requires-both-installation-and-room-authority
  (world/with-world [conn w]
    (doseq [party [(:owner w) (:member w) (:non-member w) (:admin w) nil]]
      (is (thrown? clojure.lang.ExceptionInfo
                   (benchmarks/authorize! conn (ops/party-principal party) (:room w)))))
    (d/transact conn [{:party/id (:owner w) :party/role :admin}])
    (is (= (:owner w) (benchmarks/authorize! conn (ops/party-principal (:owner w)) (:room w))))))

(deftest experiment-adapter-keeps-records-in-control-room-and-refunds-after-cleanup
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [control]
       (let [host-ctx ec/*execution-context*
             clean (discourse/make-room {:id (keyword (str "fixture-" (random-uuid)))
                                         :store (memory/make)})
             files (assoc fixture/fixture "checker.clj"
                          "(ns checker) (defn check [{:keys [files]}] {:checks {:empty-output? (empty? files)} :reward 1})")
             request (input files)
             base (prepared files request)
             [_ echo] (@#'is.simm.ops.work-test/fixture)
             plan (-> (:plan base)
                      (assoc :team (:team echo))
                      ;; Echo is deliberately not an LLM: use the same
                      ;; isolated worlds/wallets but omit LLM-only limits.
                      (update :environments
                              (fn [envs]
                                (mapv (fn [env]
                                        (let [ref (:environment/verifier env)]
                                          (environment/make-environment
                                           {:id (:environment/id env) :task (:environment/task env)
                                            :verifier {:id (:verifier/id ref) :version (:verifier/version ref)
                                                       :basis (:verifier/basis ref)}
                                            :world (:environment/world env)
                                            :limits (dissoc (:environment/limits env) :max-model-steps :budget-dollars)
                                            :metadata (:environment/metadata env)}))) envs))))
             experiment (runner/experiment-def
                         (assoc plan :id :fixture/benchmark :repetitions 1 :metadata {}))
             prepared (-> base (assoc :plan (assoc plan :experiment experiment))
                          (assoc-in [:basis :team] (:team echo))
                          (assoc-in [:basis :experiment] experiment))
             retired (atom 0)]
         (try
           (workspace/ensure-workspace! control)
           (workspace/seed! control {"/private-gold.md" "must not leak into candidate"})
           (workflows/write! control (ops/party-principal (:owner w)) {:name "echo" :files files})
           (d/transact conn [{:party/id (:owner w) :party/role :admin}])
           (resource/mint! control {:id (random-uuid) :resources (work/resources (:limits request))})
           (with-redefs-fn {#'benchmarks/prepare (fn [& _] prepared)
                            #'benchmarks/fixture-room! (fn [& _] clean)
                            #'benchmarks/retire-fixture! (fn [& _] (swap! retired inc))}
             (fn []
               (let [submitted (benchmarks/submit! conn host-ctx (ops/party-principal (:owner w))
                                                   (:room w) control request)
                     status (jobs/status (:job submitted) 25000)]
                 (is (= :completed (:status status)))
                 (is (:settled? status))
                 (is (= 1 @retired))
                 (is (= (work/resources (:limits request)) (resource/balance control)))
                 (is (false? (:return-pending? (work/status conn host-ctx (ops/party-principal (:owner w))
                                                            (:room w) (:id submitted)))))
                 (is (= {} (workspace/read-tree clean "/")) "fixture never inherited the control room's answer")
                 (let [entry (first (store/-list-attempts (:store control) (store/conversation-id control) {}))
                       attempt (store/-load-attempt (:store control) (store/conversation-id control) (:attempt/id entry))
                       evidence (:attempt/evidence attempt)
                       exported (trajectory/trajectory control attempt)]
                   (doseq [key [:result :trace :files]]
                     (is (contains? evidence key) (str "trusted evidence retained: " key)))
                   (is (= (:resources evidence) (get-in attempt [:attempt/receipt :attempt/resources])))
                   (is (seq (get-in evidence [:trace :runs])))
                   (is (= (str (:attempt/id attempt)) (:attempt exported)))
                   (is (vector? (:messages exported))))
                 (workflows/write! control (ops/party-principal (:owner w))
                                   {:name "echo" :files {"gold.edn" "{:changed true}"}})
                 (is (= (:job submitted)
                        (:job (benchmarks/submit! conn host-ctx (ops/party-principal (:owner w))
                                                  (:room w) control request))))
                 (is (= 1 (count (jobs/jobs control))))
                 (is (thrown? clojure.lang.ExceptionInfo
                              (benchmarks/submit! conn host-ctx (ops/party-principal (:owner w))
                                                  (:room w) control (assoc request :repetitions 2)))))))
           (finally (discourse/close-room! clean))))))))

(deftest physical-capacity-permit-is-exclusive-owned-and-returned-once
  (let [a (benchmarks/adapter nil nil {} nil)
        b (benchmarks/adapter nil nil {} nil)
        slot @#'benchmarks/checker-slot]
    ((:admit! a) nil)
    (is (zero? (.availablePermits slot)))
    (is (thrown? clojure.lang.ExceptionInfo ((:admit! b) nil)))
    ((:settled! b) nil)
    (is (zero? (.availablePermits slot)) "loser cannot release winner's permit")
    ((:settled! a) nil)
    ((:settled! a) nil)
    (is (= 1 (.availablePermits slot)))))

(deftest fixture-recovery-covers-pointerless-and-already-refunded-crash-windows
  (let [row (fn [more] (merge {:work.request/id (random-uuid) :work.request/kind :benchmark} more))
        interrupted (row {:work.request/state :interrupted})
        admitting (row {:work.request/state :admitting})
        completed (row {:work.request/job (random-uuid) :work.request/return-pending? false})
        live (row {:work.request/job (random-uuid) :work.request/return-pending? false})
        pending (row {:work.request/job (random-uuid) :work.request/return-pending? true})
        statuses {(:work.request/job completed) {:status :completed :settled? true}
                  (:work.request/job live) {:status :running :settled? false}
                  (:work.request/job pending) {:status :failed :settled? true}}
        retired (atom [])]
    (with-redefs-fn {#'requests/requests (constantly [interrupted admitting completed live pending])
                     #'jobs/status statuses
                     #'benchmarks/retire-fixture! (fn [_ id] (swap! retired conj id))}
      #(benchmarks/recover! nil nil))
    (is (= [(:work.request/id interrupted) (:work.request/id completed)] @retired))))
