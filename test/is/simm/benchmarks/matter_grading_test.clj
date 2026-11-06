(ns is.simm.benchmarks.matter-grading-test
  (:require [clojure.test :refer [deftest is]] [is.simm.benchmarks.matter-grading :as grading]))
(def packets {"memo" {:packet-id "p1" :subject-id "attempt" :criterion-ids ["C1" "C2"]}
              "tracker" {:packet-id "p2" :subject-id "attempt" :criterion-ids ["C3"]}})
(defn result [packet verdict]
  {:completed? true :settled? true :checks (zipmap grading/required-checks (repeat true))
   :assessment {:packet-id (:packet-id packet) :subject-id (:subject-id packet)
                :criteria (mapv #(hash-map :id % :verdict verdict) (:criterion-ids packet))}})
(def results (into {} (map (fn [[id p]] [id (result p :pass)]) packets)))
(defn aggregate [results] (grading/aggregate ["C1" "C2" "C3"] packets results))
(deftest fixed-denominator-never-hides-missing-or-invalid-batches
  (is (:all-pass? (aggregate results)))
  (doseq [bad [(dissoc results "memo")
               (assoc-in results ["memo" :settled?] false)
               (assoc-in results ["memo" :completed?] false)
               (assoc-in results ["memo" :checks :allowed-quotes?] false)
               (assoc-in results ["memo" :assessment :packet-id] "foreign")
               (assoc-in results ["memo" :assessment :criteria 1 :id] "C1")]]
    (is (= {:denominator 3 :passed 1 :failed 0 :unresolved 2}
           (select-keys (aggregate bad) [:denominator :passed :failed :unresolved])))))
(deftest negative-and-unassessable-verdicts-remain-distinct
  (let [failed (aggregate (into {} (map (fn [[id p]] [id (result p :fail)]) packets)))
        unknown (aggregate (assoc results "memo" (result (get packets "memo") :unassessable)))]
    (is (= [0 3 0] ((juxt :passed :failed :unresolved) failed)))
    (is (= [1 0 2] ((juxt :passed :failed :unresolved) unknown)))
    (is (false? (:all-pass? failed)))
    (is (false? (:all-pass? unknown)))
    (is (:assessment-complete? failed))
    (is (false? (:assessment-complete? unknown)))))
(deftest overlapping-or-incomplete-partitions-refuse
  (doseq [bad [(dissoc packets "memo")
               (assoc-in packets ["tracker" :criterion-ids] ["C1" "C3"])
               (assoc-in packets ["tracker" :subject-id] "foreign-subject")
               (assoc-in packets ["tracker" :packet-id] "p1")
               (assoc-in packets ["memo" :subject-id] nil)]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"partition" (grading/aggregate ["C1" "C2" "C3"] bad results)))))
