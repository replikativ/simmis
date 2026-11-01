(ns is.simm.ops.native-execution-return-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.agent.run :as run]
            [dvergr.jobs :as jobs]
            [dvergr.room.store :as store]
            [is.simm.ops.native-execution-return :as native-return]))

(deftest domain-recovery-requires-family-return-without-inventing-job-settlement
  (let [room {:id :native-return-test} job (random-uuid) child (random-uuid)
        row (fn [id] {:run/id id :run/room (:id room) :run/status :failed
                      :run/family-return-ack (store/family-return-ack-id id)})
        graph [(row job) (assoc (row child) :run/parent job)]
        observation {:id job :owner-state :missing :execution-lease :absent
                     :family-return {:state :acknowledged}
                     :core-cleanup {:state :unknown} :adapter-cleanup {:state :unknown}
                     :settled? false}
        classify (fn [observed rows active]
                   (with-redefs [jobs/observe-settlement (fn [r id]
                                                           (is (= [room job] [r id])) observed)
                                 run/active-runs (constantly active)]
                     (native-return/state room job rows)))]
    (is (= :returned (classify observation graph [])))
    (is (= :live (classify (assoc observation :owner-state :live) graph [])))
    (is (= :live (classify observation graph [{:run/id child}])))
    (is (= :live (classify observation (assoc-in graph [1 :run/status] :running) [])))
    (doseq [rows [[] [(first graph)] (conj graph (first graph))
                  (update-in graph [0] dissoc :run/family-return-ack)
                  (update-in graph [1] dissoc :run/family-return-ack)
                  (assoc-in graph [1 :run/family-return-ack] (random-uuid))
                  (assoc-in graph [1 :run/room] :foreign)]]
      ;; A root-only graph is separately valid: graph completeness is proved
      ;; by the bounded domain reconciler, not inferred by this fact adapter.
      (is (= (if (= rows [(first graph)]) :returned :unknown)
             (classify observation rows []))))
    (doseq [observed [nil (assoc observation :execution-lease :held)
                      (assoc observation :family-return {:state :unknown})]]
      (is (= :unknown (classify observed graph []))))))
