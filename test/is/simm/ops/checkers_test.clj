(ns is.simm.ops.checkers-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.agent.evaluation :as evaluation]
            [is.simm.ops.checkers :as checkers]))

(deftest checker-executes-synchronously-and-returns-only-validated-evidence
  (is (= {:checks {:correct? true} :reward 1.0}
         (checkers/run-checker
          "(ns checker) (defn check [{:keys [gold files]}] {:checks {:correct? (= gold (get files \"/out/result.md\"))} :reward 1})"
          {:gold "answer" :files {"/out/result.md" "answer"}}))))

(deftest invalid-checker-results-are-not-realized-or-printed
  (doseq [result ["nil" "[]" "{:checks (repeat true) :reward 1}"
                  "{:checks {} :reward 1}" "{:checks {:a 1} :reward 1}"
                  "{:checks {:a true} :reward ##NaN}"
                  "{:checks {:a true} :reward ##Inf}"
                  "{:checks {:a true} :reward -1}"
                  "{:checks (into {} (map (fn [i] [(keyword (str i)) true]) (range 101))) :reward 1}"]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (checkers/run-checker (str "(ns checker) (defn check [_] " result ")") {})))))

(deftest deadline-includes-top-level-code-and-is-sticky-through-catches
  (doseq [source ["(ns checker) (loop [] (recur))"
                  "(ns checker) (defn check [_] (loop [] (recur)))"
                  "(ns checker) (defn check [_] (try (loop [] (recur)) (catch Exception _ {:checks {:a true} :reward 1})))"]]
    (is (thrown? Exception
                 (checkers/run-checker source {}
                                       (assoc checkers/policy :timeout-ms 20))))))

(deftest verifier-identity-includes-effective-grading-parameters
  (let [bundle {:name "echo" :id (random-uuid) :definition {:params {:threshold 1}}}
        before (checkers/evaluator bundle)
        after (checkers/evaluator (assoc-in bundle [:definition :params :threshold] 2))]
    (is (not= (evaluation/evaluator-ref before) (evaluation/evaluator-ref after)))
    (is (= :ad-hoc (:tier before)))))

(deftest observer-preserves-trusted-trajectory-and-does-not-turn-authored-failure-into-success
  (let [evaluator (checkers/evaluator
                   {:name "echo" :id (random-uuid) :definition {}
                    :checker "(ns checker) (defn check [_] {:checks {:completed? false} :reward 0})"})
        evidence ((:observe evaluator) {:default {:result "answer" :trace {:runs []} :resources {}}
                                        :result {:run/status :completed}
                                        :execution/evidence {:files {"/out/result" "answer"}}})]
    (is (= "answer" (:result evidence)))
    (is (= {:runs []} (:trace evidence)))
    (is (contains? evidence :resources))
    (is (false? (get-in ((:verify evaluator) {} evidence) [:checks :completed?])))))
