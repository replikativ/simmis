(ns is.simm.uis.attempt-board-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [is.simm.uis.web.desktop.attempt-board :as board]))

(def ^:private schema
  ;; The typed Attempt and Run attributes the board reads, as dvergr defines them.
  (into [{:db/ident :run/id :db/valueType :db.type/uuid :db/unique :db.unique/identity :db/cardinality :db.cardinality/one}
         {:db/ident :run/kind :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
         {:db/ident :run/status :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
         {:db/ident :run/parent :db/valueType :db.type/uuid :db/cardinality :db.cardinality/one}
         {:db/ident :run/started-at :db/valueType :db.type/instant :db/cardinality :db.cardinality/one}
         {:db/ident :attempt/id :db/valueType :db.type/uuid :db/unique :db.unique/identity :db/cardinality :db.cardinality/one}
         {:db/ident :attempt/run :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
         {:db/ident :scorecard/experiment-content-id :db/valueType :db.type/uuid :db/cardinality :db.cardinality/one}]
        (for [[k t] {:attempt/model :string :attempt/provider :keyword :attempt/status :keyword
                     :attempt/reward :double :attempt/elapsed-ms :long :attempt/started-at :instant
                     :attempt/environment-id :keyword :attempt/microdollars :long
                     :attempt/experiment-content-id :uuid :attempt/experiment-candidate :keyword}]
          {:db/ident k :db/valueType (keyword "db.type" (name t)) :db/cardinality :db.cardinality/one})))

(defn- conn []
  (let [cfg {:store {:backend :memory :id (random-uuid)} :keep-history? false :schema-flexibility :write}]
    (d/create-database cfg)
    (doto (d/connect cfg) (d/transact schema))))

(defn- run! [c id kind status & [parent]]
  (d/transact c [(cond-> {:run/id id :run/kind kind :run/status status :run/started-at (java.util.Date.)}
                   parent (assoc :run/parent parent))]))

(defn- attempt! [c run-id m]
  (d/transact c [(merge {:attempt/id (random-uuid) :attempt/run [:run/id run-id]
                         :attempt/provider :openai :attempt/status :completed
                         :attempt/elapsed-ms 1000 :attempt/started-at (java.util.Date.)
                         :attempt/environment-id :env}
                        m)]))

(deftest the-board-ranks-models-and-follows-a-job-live
  (let [c (conn)
        job (random-uuid)
        [a b x] [(random-uuid) (random-uuid) (random-uuid)]]
    (run! c job :workflow :running)
    (doseq [r [a b x]] (run! c r :agent-task :running job))
    (testing "a running job with no Attempt yet"
      (let [{:keys [jobs total]} (board/board @c)]
        (is (= [{:children 3 :running 3 :attempts 0}]
               (map #(assoc (select-keys % [:children :running]) :attempts (get-in % [:summary :attempts])) jobs)))
        (is (= 0 (:attempts total)))))
    (run! c a :agent-task :completed job)
    (attempt! c a {:attempt/model "cheap" :attempt/reward 1.0 :attempt/microdollars 100})
    (run! c b :agent-task :completed job)
    (attempt! c b {:attempt/model "strong" :attempt/reward 1.0 :attempt/microdollars 3000})
    (run! c x :agent-task :failed job)
    (attempt! c x {:attempt/model "cheap" :attempt/reward 0.0 :attempt/status :failed :attempt/microdollars 50})
    (let [{:keys [jobs models total]} (board/board @c)
          [j] jobs]
      (testing "the job's progress moves as its children finish"
        (is (= 0 (:running j)))
        (is (= 3 (get-in j [:summary :attempts]))))
      (testing "models rank by mean reward, then by cost per pass"
        (is (= ["strong" "cheap"] (map :model models)))
        (is (= {:attempts 2 :passed 1 :failed 1 :microdollars 150 :microdollars-per-pass 150}
               (select-keys (second models) [:attempts :passed :failed :microdollars :microdollars-per-pass]))))
      (is (= 3150 (:microdollars total))))))

(deftest experiments-group-by-candidate-and-show-a-certified-scorecard
  (let [c (conn)
        exp (random-uuid)
        [a b] [(random-uuid) (random-uuid)]]
    (run! c a :agent-task :completed)
    (run! c b :agent-task :completed)
    (attempt! c a {:attempt/model "m" :attempt/reward 1.0 :attempt/experiment-content-id exp
                   :attempt/experiment-candidate :fast})
    (attempt! c b {:attempt/model "m" :attempt/reward 0.5 :attempt/experiment-content-id exp
                   :attempt/experiment-candidate :slow})
    (let [[e] (:experiments (board/board @c))]
      (is (= (str exp) (:id e)))
      (is (false? (:scorecard? e)))
      (is (= [:fast :slow] (map :candidate (:candidates e))))
      (is (nil? (get-in e [:summary :microdollars])) "no spend recorded is not $0"))
    (d/transact c [{:scorecard/experiment-content-id exp}])
    (is (true? (:scorecard? (first (:experiments (board/board @c))))))))

(deftest a-replica-without-attempts-is-an-empty-board
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write}]
    (d/create-database cfg)
    (let [{:keys [total models jobs experiments]} (board/board @(d/connect cfg))]
      (is (= 0 (:attempts total)))
      (is (every? empty? [models experiments])))))

(deftest dollars
  (is (= "—" (board/dollars nil)))
  (is (= "$0" (board/dollars 0)))
  (is (= "$0.0012" (board/dollars 1234)))
  (is (= "$1.50" (board/dollars 1500000))))
