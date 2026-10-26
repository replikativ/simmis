(ns is.simm.ops.benchmark-resolution-plan-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.model.benchmark-resolutions :as resolutions]
            [is.simm.ops.benchmark-source :as source]
            [is.simm.ops.benchmarks-test :as benchmarks]
            [is.simm.ops.workflows-test :as workflows]))

(defn- compile-prepared [files input]
  (source/compile-plan (source/retain "echo" files) (:basis (benchmarks/prepared files input))))

(deftest actual-preparation-has-stable-semantics-across-request-nonces
  (let [files workflows/fixture input (benchmarks/input files)
        other (assoc input :request-id (random-uuid))
        a (benchmarks/prepared files input) b (benchmarks/prepared files other)
        retained (source/retain "echo" files)
        compiled-a (source/compile-plan retained (:basis a))
        compiled-b (source/compile-plan retained (:basis b))]
    (is (not= (get-in a [:basis :experiment :experiment/content-id])
              (get-in b [:basis :experiment :experiment/content-id])))
    (is (= compiled-a compiled-b))
    (is (= (resolutions/plan-id (:plan compiled-a)) (resolutions/plan-id (:plan compiled-b))))
    (is (= [{:kind :default}] (mapv :case (get-in compiled-a [:plan :cases]))))
    (is (= (get-in a [:basis :team]) (get-in compiled-a [:semantic :benchmark :team])))
    (is (= (dissoc (get-in a [:basis :experiment]) :experiment/id :experiment/content-id)
           (get-in compiled-a [:semantic :benchmark :experiment])))))

(deftest policy-budget-model-and-source-changes-change-the-semantic-identity
  (let [files workflows/fixture input (benchmarks/input files)
        prepared (benchmarks/prepared files input) retained (source/retain "echo" files)
        id #(get-in % [:plan :semantic-id])
        original (id (source/compile-plan retained (:basis prepared)))]
    (doseq [changed [(compile-prepared files (assoc input :models ["other-fixture-model"]))
                     (compile-prepared files (assoc-in input [:limits :max-model-steps] 4))
                     (source/compile-plan retained (assoc (:basis prepared) :policy {:version :other}))
                     (let [files (assoc files "fixtures/docs/input.md" "changed evidence")]
                       (compile-prepared files (benchmarks/input files)))]]
      (is (not= original (id changed))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (source/compile-plan retained (assoc-in (:basis prepared) [:submission :request-id] (random-uuid)))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (source/compile-plan retained (assoc (:basis prepared) :unknown-policy true))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (source/compile-plan retained (assoc-in (:basis prepared) [:experiment :experiment/repetitions] 2))))))

(deftest selected-case-order-comes-from-the-actual-canonical-dataset
  (let [files (assoc workflows/fixture
                     "cases/default/gold.edn" "{:expected 1}"
                     "cases/default/fixtures/docs/input.md" "selected case"
                     "cases/other/gold.edn" "{:expected 2}"
                     "cases/other/fixtures/docs/input.md" "unselected case")
        input (assoc (benchmarks/input files) :cases ["default"])
        compiled (compile-prepared files input)]
    (is (= [{:kind :named :name "default"}] (mapv :case (get-in compiled [:plan :cases]))))
    (is (= [0] (mapv :ordinal (get-in compiled [:plan :cases]))))
    (is (= (get-in compiled [:plan :source-id]) (source/source-id (:source compiled))))
    (is (not= (get-in compiled [:plan :semantic-id])
              (get-in (compile-prepared files (assoc input :cases ["other"])) [:plan :semantic-id])))
    (let [prepared (benchmarks/prepared files (assoc input :cases ["other" "default"]))
          basis (:basis prepared)
          two (source/compile-plan (source/retain "echo" files) basis)]
      (is (= (mapv #(get-in % [:environment/metadata :case])
                   (get-in basis [:experiment :experiment/dataset :dataset/environments]))
             (mapv #(get-in % [:case :name]) (get-in two [:plan :cases]))))
      (is (= [0 1] (mapv :ordinal (get-in two [:plan :cases])))))))
