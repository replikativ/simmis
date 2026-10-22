(ns is.simm.ops.checkers-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.catalog.room :as catalog]
            [dvergr.sandbox :as sandbox]
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
    (is (= {:status :not-provided} (:calibration evidence)))
    (is (contains? evidence :resources))
    (is (false? (get-in ((:verify evaluator) {} evidence) [:checks :completed?])))))

(def calibrated-bundle
  {:name "calibrated" :id (random-uuid) :definition {:params {:threshold 1}}
   :gold "answer"
   :checker "(ns checker) (defn check [{:keys [files gold]}] (let [ok (= gold (get files \"/out/answer\"))] {:checks {:correct? ok} :reward (if ok 1 0)}))"
   :calibration {:reference {"/out/answer" "answer"}
                 :damaged {:wrong {:files {"/out/answer" "wrong"} :loses [:correct?]}}}})

(deftest calibration-uses-reference-and-real-false-damaged-checks
  (is (:ok? (checkers/calibrate calibrated-bundle)))
  (is (:ok? (checkers/calibrate (-> calibrated-bundle
                                   (assoc :gold "wrong" :cases {"case-a" {:gold "answer"}})
                                   (assoc-in [:calibration :case] "case-a")))))
  (is (false? (:ok? (checkers/calibrate (assoc-in calibrated-bundle [:calibration :reference "/out/answer"] "wrong")))))
  (is (false? (:ok? (checkers/calibrate (assoc-in calibrated-bundle [:calibration :damaged :wrong :loses] [:absent?])))))
  (is (false? (:ok? (checkers/calibrate (assoc-in calibrated-bundle [:calibration :damaged :wrong :files "/out/answer"] "answer")))))
  (is (nil? (checkers/calibrate (dissoc calibrated-bundle :calibration)))))

(deftest calibration-validates-all-inputs-before-executing-code
  (doseq [bad [false {}
               {:reference {} :damaged {}}
               {:reference {"/private/answer" "answer"} :damaged (get-in calibrated-bundle [:calibration :damaged])}
               (assoc-in (:calibration calibrated-bundle) [:reference] {"/out/../answer" "answer"})
               (assoc-in (:calibration calibrated-bundle) [:damaged :wrong :loses] [])
               (assoc-in (:calibration calibrated-bundle) [:damaged :wrong :loses] [:x :x])
               (assoc (:calibration calibrated-bundle) :fetched {"https://example.com" "page"})
               (assoc (:calibration calibrated-bundle) :case "missing")
               (assoc-in (:calibration calibrated-bundle) [:reference]
                         (into {} (map (fn [n] [(str "/out/" n) "x"]) (range 100))))]]
    (with-redefs [checkers/run-checker (fn [& _] (throw (AssertionError. "pure validation only")))]
      (is (thrown? clojure.lang.ExceptionInfo
                   (checkers/calibrate (assoc calibrated-bundle :calibration bad)))))))

(deftest calibration-has-one-deadline-and-allocation-account-for-all-invocations
  (let [seen (atom [])]
    (with-redefs [checkers/run-checker
                  (fn [_ {:keys [files]} limits]
                    (swap! seen conj limits)
                    {:checks {:correct? (= "answer" (get files "/out/answer"))}
                     :reward (if (= "answer" (get files "/out/answer")) 1.0 0.0)})]
      (is (:ok? (checkers/calibrate calibrated-bundle))))
    (is (= 2 (count @seen)))
    (is (apply = (map :deadline-nanos @seen)))
    (is (apply identical? (map :allocation-check @seen))))
  (is (thrown? Exception
               (checkers/calibrate (assoc calibrated-bundle :checker "(ns checker) (defn check [_] (loop [] (recur)))")
                                   (assoc checkers/calibration-policy :timeout-ms 20)))))

(deftest calibration-scales-one-shared-account-and-deadline-through-the-max-answer-count
  (doseq [damaged-count [1 4]]
    (let [account (fn []) accounts (atom []) seen (atom [])
          damaged (into {} (for [i (range damaged-count)]
                             [(str "wrong-" i) {:files {"/out/answer" "wrong"} :loses [:correct?]}]))
          bundle (assoc-in calibrated-bundle [:calibration :damaged] damaged)
          before (System/nanoTime)
          report (with-redefs [sandbox/make-resource-limits
                              (fn [& options] (swap! accounts conj (vec options)) account)
                              checkers/run-checker
                              (fn [_ {:keys [files]} limits]
                                (swap! seen conj limits)
                                (let [ok (= "answer" (get files "/out/answer"))]
                                  {:checks {:correct? ok} :reward (if ok 1.0 0.0)}))]
                   (checkers/calibrate bundle (assoc checkers/calibration-policy
                                                    :timeout-ms 123 :max-allocated-bytes 4096)))
          after (System/nanoTime)
          answers (inc damaged-count)
          deadline (:deadline-nanos (first @seen))]
      (is (:ok? report))
      (is (= [[:max-bytes (* answers 4096)]] @accounts))
      (is (= answers (count @seen)))
      (is (every? #(identical? account (:allocation-check %)) @seen))
      (is (apply = (map :deadline-nanos @seen)))
      (is (<= before (- deadline (* 1000000 answers 123)) after))))
  (with-redefs [sandbox/make-resource-limits
                (fn [& _] (throw (AssertionError. "Invalid calibration allocated resources")))]
    (is (thrown? clojure.lang.ExceptionInfo
                 (checkers/calibrate
                   (assoc-in calibrated-bundle [:calibration :damaged]
                             (into {} (for [i (range 5)]
                                        [(str i) {:files {} :loses [:correct?]}]))))))))

(deftest calibration-budget-is-one-scoring-check-per-answer
  ;; Each call allocates well over half the per-answer budget: two of them
  ;; overrun one shared check's budget, not two checks' worth.
  (let [heavy (str "(ns checker) (defn check [{:keys [files gold]}]"
                   " (let [ok (= gold (get files \"/out/answer\"))]"
                   " (count (loop [i 0 acc []] (if (< i 400000) (recur (inc i) (conj acc i)) acc)))"
                   " {:checks {:correct? ok} :reward (if ok 1 0)}))")
        bundle (assoc calibrated-bundle :checker heavy)
        per-answer (* 96 1024 1024)]
    (is (thrown? Exception (checkers/run-checker heavy {:files {} :gold "answer"}
                                                 (assoc checkers/policy :max-allocated-bytes (quot per-answer 2))))
        "a single call needs more than half the per-answer budget")
    (is (:ok? (checkers/calibrate bundle (assoc checkers/calibration-policy :max-allocated-bytes per-answer))))))

(deftest per-answer-budget-has-a-distinct-calibration-setup-identity
  (let [setup-ref #(evaluation/world-setup-ref (checkers/world-setup calibrated-bundle))
        current (setup-ref)
        legacy-policy (-> checkers/calibration-policy
                          (assoc :version 1)
                          (dissoc :budget-scaling))
        legacy (with-redefs [checkers/calibration-policy legacy-policy] (setup-ref))
        env #(environment/make-environment
                {:id :fixture/calibration-policy :task "Same task"
                 :verifier {:id :fixture/checker :version 1} :world {:setup %}})]
    (is (= 2 (get-in current [:setup/basis :calibration-policy :version])))
    (is (= :per-answer (get-in current [:setup/basis :calibration-policy :budget-scaling])))
    (is (not= current legacy)
        "The same bundle cannot reuse the old admission-budget identity")
    (is (= current (setup-ref)))
    (is (not= (:environment/content-id (env current))
              (:environment/content-id (env legacy))))
    (is (= (dissoc (:setup/basis current) :calibration-policy)
           (dissoc (:setup/basis legacy) :calibration-policy)))))

(deftest calibrated-setup-does-not-seed-answers-and-keeps-policy-in-identity
  (let [contexts (atom [])]
    (with-redefs [catalog/world-setup
                  (fn [_] (evaluation/make-world-setup
                           {:id :fixture/setup :prepare (fn [ctx] (swap! contexts conj ctx) {:fixtures "digest"})}))]
      (let [setup (checkers/world-setup calibrated-bundle)
            context {:room :fake-world}
            evidence ((:prepare setup) context)
            observed ((:observe (checkers/evaluator calibrated-bundle))
                      {:default {} :result {:run/status :completed} :setup/evidence evidence})]
        (is (= [context] @contexts) "the fixture preparer receives no answer files")
        (is (:ok? (:calibration evidence)))
        (is (= evidence (:setup observed)))
        (is (= (:calibration evidence) (:calibration observed)))
        (is (= checkers/calibration-policy (get-in (evaluation/world-setup-ref setup) [:setup/basis :calibration-policy]))))
      (reset! contexts [])
      (let [setup (checkers/world-setup (assoc-in calibrated-bundle [:calibration :reference "/out/answer"] "wrong"))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"reference checks" ((:prepare setup) {})))
        (is (empty? @contexts) "failed calibration cannot proceed to candidate fixture setup")))))
