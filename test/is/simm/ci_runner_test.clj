(ns is.simm.ci-runner-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.ci-runner :as ci]
            [kaocha.api :as api]
            [kaocha.config :as config]
            [kaocha.load :as load]
            [kaocha.runner :as runner]))

(deftest partition-coordinates-are-explicit-and-bounded
  (is (= [0 1] (ci/coordinates nil nil)))
  (is (= [1 2] (ci/coordinates "1" "2")))
  (is (= [3 4] (ci/coordinates "3" "4")))
  (doseq [[i n] [[nil "2"] ["0" nil] ["" "2"] ["-1" "2"]
                 ["0" "0"] ["2" "2"] ["x" "2"]
                 ["999999999999999999999999" "2"]]]
    (is (thrown? clojure.lang.ExceptionInfo (ci/coordinates i n)))))

(deftest partitions-cover-every-namespace-exactly-once
  (let [names '[z-test a-test d-test b-test c-test e-test f-test]]
    (doseq [total [1 2 3 4]]
      (let [parts (mapv #(ci/partition-namespaces names % total) (range total))]
        (is (every? seq parts))
        (is (= (frequencies names) (frequencies (mapcat identity parts))))
        (is (= parts (mapv #(ci/partition-namespaces (reverse names) % total) (range total))))))
    (doseq [[names i n] [[[] 0 1] [['a-test 'a-test] 0 1]
                         [['a-test] 0 2] [['a-test] -1 1] [['a-test] 1 1]]]
      (is (thrown? clojure.lang.ExceptionInfo (ci/partition-namespaces names i n))))))

(deftest partitioning-preserves-suite-policy-and-literal-names
  (let [effective (assoc (config/load-config) :kaocha.filter/skip-meta [:integration]
                         :kaocha/reporter ['kaocha.report/documentation])
        names '[is.simm.a+b-test is.simm.z-test]]
    (with-redefs [load/find-test-nss (fn [paths patterns]
                                      (is (= ["test"] paths))
                                      (is (= ["-test$"] (mapv str patterns)))
                                      names)]
      (let [{:keys [config manifest]} (ci/partition-config effective 0 2)
            patterns (mapv re-pattern (get-in config [:kaocha/tests 0 :kaocha/ns-patterns]))]
        (is (= names (seq (:all manifest))))
        (is (= ['is.simm.a+b-test] (:selected manifest)))
        (is (= (dissoc effective :kaocha/tests) (dissoc config :kaocha/tests)))
        (is (= (dissoc (first (:kaocha/tests effective)) :kaocha/ns-patterns)
               (dissoc (first (:kaocha/tests config)) :kaocha/ns-patterns)))
        (is (some #(re-find % "is.simm.a+b-test") patterns))
        (is (not-any? #(re-find % "isXsimmXab-test") patterns))))
    (doseq [change [(fn [c] (update c :kaocha/tests conj (first (:kaocha/tests c))))
                    (fn [c] (assoc-in c [:kaocha/tests 0 :kaocha/ns-patterns] ["custom"]))]]
      (is (thrown? clojure.lang.ExceptionInfo (ci/partition-config (change effective) 0 1))))))

(deftest unreadable-discovery-cannot-disappear-from-the-gate
  (with-redefs [load/find-test-nss (fn [& _] [[:lambdaisland.tools.namespace.find/reader-exception "bad.clj" (Exception.)]])]
    (is (thrown? clojure.lang.ExceptionInfo (ci/partition-config (config/load-config) 0 1)))))

(deftest real-discovery-is-exhaustively-partitioned
  (let [effective (config/load-config)
        all (get-in (ci/partition-config effective 0 1) [:manifest :all])]
    (is (some #{'is.simm.ci-runner-test} all))
    (doseq [total [1 2 3 4]]
      (let [parts (mapv #(get-in (ci/partition-config effective % total) [:manifest :selected]) (range total))]
        (is (every? seq parts))
        (is (= (frequencies all) (frequencies (mapcat identity parts))))))))

(deftest literal-selected-pattern-survives-the-runners-config-roundtrip
  (let [options {:skip-meta [:integration] :reporter 'kaocha.report/documentation}
        effective (config/merge-config (config/load-config) (config/normalize options))
        forwarded (with-redefs [ci/coordinates (fn [& _] [0 1])
                               clojure.core/spit (fn [& _])
                               runner/exec-fn identity]
                    (ci/run! options))
        ;; Exercise one actual discovered namespace through Kaocha's loader.
        ;; Full discovery coverage is checked separately above; avoid loading
        ;; every fixture merely to smoke-test the runner's config normalization.
        patterns (filterv #(re-find (re-pattern %) "is.simm.ci-runner-test")
                          (get-in forwarded [:kaocha/tests 0 :kaocha/ns-patterns]))
        selected (assoc-in forwarded [:kaocha/tests 0 :kaocha/ns-patterns] patterns)
        roundtrip (config/merge-config (config/load-config) (config/normalize selected))
        plan (api/test-plan roundtrip)
        ;; Kaocha plugins may return a sequence rather than a vector of suites.
        namespaces (map :kaocha.ns/name (-> plan :kaocha.test-plan/tests first :kaocha.test-plan/tests))]
    (is (= 1 (count patterns)))
    (is (= ['is.simm.ci-runner-test] (vec namespaces)))
    (is (= (dissoc effective :kaocha/tests) (dissoc roundtrip :kaocha/tests)))))

(deftest duration-bins-balance-work-without-dropping-new-or-renamed-namespaces
  (let [names '[a-test b-test c-test d-test new-test]
        costs {'a-test 100 'b-test 90 'c-test 10 'd-test 5 'retired-test 999}
        parts (mapv #(ci/partition-namespaces names % 2 costs) (range 2))]
    (is (= (frequencies names) (frequencies (mapcat identity parts))))
    (is (= parts (mapv #(ci/partition-namespaces (reverse names) % 2 costs) (range 2))))
    (is (not-any? #(and (some #{'a-test} %) (some #{'b-test} %)) parts))
    (doseq [bad [{'a-test -1} {'a-test 0} {'a-test ##NaN} {'a-test ##Inf} {"a-test" 1}]]
      (is (thrown? clojure.lang.ExceptionInfo (ci/partition-namespaces names 0 2 bad))))))
