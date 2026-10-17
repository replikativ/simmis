(ns is.simm.ops.experiments-test
  "The leaderboard of a room that keeps a real certified experiment, read
   through dvergr's ops: two candidates on the same cases, one always right,
   one always wrong."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as dh]
            [dvergr.agent.environment :as environment-def]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.experiment :as experiment]
            [dvergr.agent.roster :as roster]
            [dvergr.discourse :as d]
            [dvergr.principal :as principal]
            [dvergr.room.registry :as rreg]
            [dvergr.room.store.memory :as memory]
            [is.simm.ops.experiments :as experiments]
            [org.replikativ.spindel.engine.core :as ec]))

(def ^:private exact-evaluator
  (evaluation/make-evaluator
   {:id :simmis-test/exact :version 1 :basis "simmis-experiments-test:v1"
    :capture (fn [_] {:files {"/out/result.md" "captured attempt output"}})
    :observe (fn [{:keys [default] captured :execution/evidence}]
               (assoc default :files (:files captured)))
    :verify (fn [definition evidence]
              (let [ok? (= (:environment/task definition) (:result evidence))]
                {:checks {:exact? ok?} :reward (if ok? 1.0 0.0)}))}))

(defn- environment [i]
  (environment-def/make-environment
   {:id (keyword "simmis-test" (str "t" i)) :task {:value i}
    :verifier {:id :simmis-test/exact :version 1 :basis "simmis-experiments-test:v1"}
    :limits {:timeout-ms 2000 :cancel-timeout-ms 1000}
    :world {:isolation :ctx :settlement :discard}}))

(def ^:private cases 8)

(deftest completeness-flags-survive-both-leaderboard-projections
  (let [flags {:priced? false :notional-priced? false :usage-reported? false
               :unpriced-attempts 1 :unreported-attempts 1}]
    (with-redefs-fn {#'experiments/op (fn [& _] {:experiment {} :leaderboard [{:candidate "local" :spend flags}]})
                     #'experiments/frontier (fn [& _] {:resources {}})}
      (fn []
        (is (= flags (select-keys (first (:candidates (@#'experiments/completed nil nil "id"))) (keys flags))))))
    (with-redefs-fn {#'experiments/op (fn [& _] {})}
      (fn []
        (is (= flags (select-keys (first (:candidates (@#'experiments/in-progress nil nil
                                                                  {:candidates [(assoc flags :candidate/id :local :done 1)]})))
                                 (keys flags))))))))

(deftest workflow-provenance-is-bounded-and-never-arbitrary-metadata
  (let [bundle "12345678-1234-1234-1234-123456789abc"
        info {:name "review" :title (apply str (repeat 1000 "x")) :bundle bundle
              :secret "must not be projected"}
        project (fn [workflow]
                  (#'experiments/workflow-info {:experiment/metadata {:simmis/workflow workflow
                                                                      :private "not projected"}}))]
    (is (= {:name "review" :title (apply str (repeat 256 "x")) :bundle bundle} (project info)))
    (is (= {:name "review" :bundle bundle} (project (assoc info :title {:unexpected "shape"}))))
    (doseq [bad [(assoc info :name "../another-workflow")
                 (assoc info :name (apply str (repeat 65 "x")))
                 (assoc info :bundle "not-a-bundle")
                 (assoc info :bundle {:not "an identity"}) nil]]
      (is (nil? (project bad))))))

(defn- with-experiment-room
  "`(f room)` with a room that keeps a completed experiment of two
   candidates, `right` (echoes the task: passes) and `wrong` (fails), on
   `cases` cases."
  [f]
  (let [team (-> (roster/make-roster {:id :simmis-test/team})
                 (roster/make-agent {:id :right :program {:kind :echo}})
                 (roster/make-agent {:id :wrong :program {:kind :scripted :result :nope}}))
        definition (experiment/make-experiment
                    {:id :simmis-test/exp
                     :dataset (experiment/make-dataset
                               {:id :simmis-test/tasks
                                :environments (mapv environment (range cases))})
                     :candidates [(roster/agent team :right) (roster/agent team :wrong)]
                     :repetitions 1
                     :metadata {:simmis/workflow {:name "saved-review" :title "Frozen review title"
                                                  :bundle "12345678-1234-1234-1234-123456789abc"}}})
        room (d/make-room {:id :simmis-experiments-test :store (memory/make)})]
    (try
      (binding [ec/*execution-context* (:ctx room)]
        (rreg/register! room)
        (let [{:keys [scorecard]} @(experiment/run room team definition
                                                   {(:ref exact-evaluator) exact-evaluator}
                                                   {:parallelism 2})]
          (is (some? scorecard))
          (f room)))
      (finally
        (try (rreg/unregister! (:id room)) (catch Throwable _ nil))
        (d/close-room! room)))))

(deftest a-completed-experiment-is-its-scorecard-and-its-paired-comparison
  (with-experiment-room
    (fn [room]
      (let [{:keys [experiments running]} (experiments/leaderboard
                                                  room principal/local-principal)
            [e :as es] experiments]
        (is (= 1 (count es)))
        (is (= 0 running))
        (testing "the Scorecard's leaderboard, in dvergr's order"
          (is (true? (:complete? e)))
          (is (= "simmis-test/exp" (:experiment-id e)))
          (is (= {:name "saved-review" :title "Frozen review title"
                  :bundle "12345678-1234-1234-1234-123456789abc"}
                 (:workflow e)))
          (is (= (* 2 cases) (:cells e)))
          (is (= ["right" "wrong"] (mapv :candidate (:candidates e))))
          (let [[r w] (:candidates e)]
            (is (= [cases cases 1.0] ((juxt :attempts :passed :pass-rate) r)))
            (is (= [cases 0 0.0] ((juxt :attempts :passed :pass-rate) w)))
            (let [[lo hi] (:pass-rate-interval r)]
              (is (< 0.5 lo 1.0)) (is (= 1.0 hi)))
            (testing "with the frontier's time per candidate"
              (is (number? (:median-seconds r))))))
        (testing "the paired comparison: right only on every case, decided"
          (let [{:keys [policy comparisons]} (:paired e)
                [c] comparisons]
            (is (= "all-pairs" policy))
            (is (= 1 (count comparisons)))
            (is (= ["right" "wrong" cases 0 cases 0 0]
                   ((juxt :a :b :cases :both-passed :a-only :b-only :both-failed) c)))
            (is (< (:p c) 0.01) "8 against 0: p = 2/256")
            (is (= "a-better" (:verdict c)))))))))

(deftest a-store-keeps-experiments-when-it-holds-an-experiment-attempt
  (let [cfg {:store {:backend :memory :id (random-uuid)} :keep-history? false :schema-flexibility :write}
        _ (dh/create-database cfg)
        c (dh/connect cfg)]
    (try
      (is (false? (experiments/store-keeps-experiments? @c)) "no such attribute yet")
      (dh/transact c [{:db/ident :attempt/id :db/valueType :db.type/uuid
                       :db/unique :db.unique/identity :db/cardinality :db.cardinality/one}
                      {:db/ident :attempt/experiment-content-id :db/valueType :db.type/uuid
                       :db/cardinality :db.cardinality/one}])
      (dh/transact c [{:attempt/id (random-uuid)}])
      (is (false? (experiments/store-keeps-experiments? @c)) "an Attempt of no experiment")
      (dh/transact c [{:attempt/id (random-uuid) :attempt/experiment-content-id (random-uuid)}])
      (is (true? (experiments/store-keeps-experiments? @c)))
      (is (false? (experiments/store-keeps-experiments? nil)))
      (finally (dh/release c)))))
