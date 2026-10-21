(ns is.simm.ops.benchmark-dataset-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.ops.benchmark-dataset :as dataset]))

(def manifest {:version 1 :splits {:development ["a"] :held-out ["b"]}})
(def files {"benchmark.edn" (pr-str manifest)})

(deftest split-manifest-is-a-closed-disjoint-complete-partition
  (is (= manifest (dissoc (dataset/manifest files ["a" "b"]) :id)))
  (is (= (dataset/manifest files ["a" "b"]) (dataset/manifest files ["b" "a"])))
  (is (not= (:id (dataset/manifest files ["a" "b"]))
            (:id (dataset/manifest (assoc files "cases/a/fixtures/input.edn" "new evidence") ["a" "b"]))))
  (is (= (:id (dataset/manifest files ["a" "b"]))
         (:id (dataset/manifest (assoc files "checker.clj" "different evaluator") ["a" "b"]))))
  (doseq [bad [(assoc manifest :version 2) (assoc manifest :execute "code")
               (assoc-in manifest [:splits :development] [])
               (assoc-in manifest [:splits :development] 1)
               (assoc-in manifest [:splits :development] {:a "b"})
               (assoc-in manifest [:splits :development] ["a" "a"])
               (assoc-in manifest [:splits :held-out] ["a"])
               (assoc-in manifest [:splits :held-out] ["missing"])
               (assoc manifest :splits {:held-out ["a" "b"]})
               (assoc-in manifest [:splits :unknown] ["x"]) nil []]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (dataset/manifest {"benchmark.edn" (pr-str bad)} ["a" "b"]))))
  (doseq [text ["{" "#unknown/tag {}" (str (pr-str manifest) " {}") (apply str (repeat 16385 " "))]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (dataset/manifest {"benchmark.edn" text} ["a" "b"])))))

(deftest held-out-cases-require-explicit-intent-and-cannot-cross-splits
  (doseq [request [{} {:cases ["a"]} {:split "development" :cases ["b"]}
                   {:split "held-out" :cases ["a"]} {:split "validation"}
                   {:split "development" :cases []}
                   {:split "development" :cases ["a" "a"]} {:split "unknown"} {:split false}]]
    (is (thrown? clojure.lang.ExceptionInfo (dataset/select-cases files ["a" "b"] request))))
  (doseq [[split cases] [["development" ["a"]] ["held-out" ["b"]]]]
    (let [selection (dataset/select-cases files ["a" "b"] {:split split})]
      (is (= cases (:cases selection)))
      (is (= split (get-in selection [:metadata :split])))
      (is (= cases (get-in selection [:metadata :cases])))
      (is (string? (get-in selection [:metadata :id]))))))

(deftest legacy-workflows-retain-selection-without-claiming-a-split
  (is (= {:cases [] :metadata nil} (dataset/select-cases {} [] {})))
  (is (= {:cases ["a" "b"] :metadata nil} (dataset/select-cases {} ["b" "a"] {})))
  (is (= ["b"] (:cases (dataset/select-cases {} ["a" "b"] {:cases ["b"]}))))
  (is (thrown? clojure.lang.ExceptionInfo (dataset/select-cases {} ["a"] {:split "held-out"}))))
