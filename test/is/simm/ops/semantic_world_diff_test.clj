(ns is.simm.ops.semantic-world-diff-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.ops.semantic-diff :as semantic]
            [is.simm.runtimes.branching :as branching]))

(deftest adopted-file-world-exposes-native-paths-and-bounded-patch
  (let [limit @#'semantic/max-patch-chars
        patch (apply str (repeat (inc limit) "x"))
        files [{:path "out/result.md" :status :added}]]
    (with-redefs [branching/delta (fn [& _] {:files files :patch patch :stat "1 file"
                                             :summary {:files-changed 1 :insertions 1 :deletions 0}})]
      (let [diff (semantic/semantic-diff (random-uuid) :work :world)]
        (is (= files (:files diff)))
        (is (= 1 (get-in diff [:counts :files-changed])))
        (is (= limit (count (:patch diff))))
        (is (true? (:patch-truncated? diff)))))))

(deftest adopted-data-world-does-not-expose-raw-datoms-as-files
  (with-redefs [branching/delta (fn [& _] {:added [[:private :datom]] :removed []
                                           :summary {:entities-touched 1 :added-datoms 1 :removed-datoms 0}})]
    (let [diff (semantic/semantic-diff (random-uuid) :work :world)]
      (is (= 1 (get-in diff [:counts :entities-touched])))
      (is (not (contains? diff :files)))
      (is (not (contains? diff :added)))
      (is (not (contains? diff :removed))))))

(deftest unavailable-adopted-diff-is-not-an-empty-success
  (doseq [delta [nil {:error :unavailable}]]
    (with-redefs [branching/delta (constantly delta)]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"change details are unavailable"
                            (semantic/semantic-diff (random-uuid) :work :world))))))
