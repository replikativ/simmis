(ns is.simm.ops.github-native-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.git.budget :as budget]
            [geschichte.repo :as repo]
            [geschichte.workspace :as workspace]
            [is.simm.ops.github-merge :as merge]
            [is.simm.ops.github-native :as native]))

(defn- operation [] (budget/create (assoc (merge/operation-policy) :timeout-ms 30000)))

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
             :keep-history? true :commit-graph? true}]
    (d/create-database cfg)
    (let [canonical (d/connect cfg)]
    (try
      (repo/init! canonical {:branch "refs/heads/main"})
      (repo/write! canonical "base.txt" (.getBytes "Base\n" "UTF-8"))
      (repo/stage-all! canonical)
      (repo/commit! canonical {:message "Base"})
      (workspace/fork! canonical "native-preflight")
      (let [private (d/connect (assoc cfg :branch :geschichte.workspace/native-preflight))]
        (try
          (repo/write! private "incoming.txt" (.getBytes "Incoming\n" "UTF-8"))
          (repo/stage-all! private)
          (let [commit (repo/commit! private {:message "Incoming candidate"})]
            (f canonical private (:geschichte.commit/id commit)))
          (finally (d/release private))))
      (finally (d/release canonical) (d/delete-database cfg))))))

(deftest finite-publication-and-advance-preserve-unrelated-refs
  (fixture
    (fn [canonical private head]
      (let [old (:geschichte.commit/id (repo/head-commit canonical))
            result (native/publish! canonical private "review" head (operation) (constantly nil))]
        (is (true? (:created? result)))
        (is (= old (:geschichte.commit/id (repo/head-commit canonical))))
        (is (= head (get (repo/refs canonical) "refs/heads/review")))
        (is (nil? (repo/read canonical "incoming.txt")))
        (let [accepted (native/advance! private canonical "refs/heads/main" (d/commit-id @canonical)
                                        (operation) (constantly nil))]
          (is (= head (:new accepted)))
          (is (= head (get (repo/refs canonical) "refs/heads/review"))))))))

(deftest historical-metadata-is-bounded-before-native-eager-projection
  (fixture
    (fn [canonical private head]
      (d/transact canonical [{:geschichte.commit/id (random-uuid)
                              :geschichte.commit/message (apply str (repeat 65537 "x"))}])
      (let [before @canonical calls (atom 0)]
        (with-redefs [workspace/publish! (fn [& _] (swap! calls inc) (throw (AssertionError. "Projection started")))]
          (is (= :invalid-native-metadata
                 (try (native/publish! canonical private "review" head (operation) (constantly nil))
                      nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))
        (is (zero? @calls))
        (is (= before @canonical))))))

(deftest only-the-inspected-source-tip-and-ref-can-be-published
  (fixture
    (fn [canonical private _]
      (let [before @canonical historical (:geschichte.commit/id (repo/head-commit canonical))]
        (is (= :uninspected-source-tip
               (try (native/publish! canonical private "review" historical (operation) (constantly nil))
                    nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
        (is (= :uninspected-source-tip
               (try (native/advance! private canonical "refs/heads/other" (d/commit-id @canonical)
                                     (operation) (constantly nil))
                    nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
        (is (= before @canonical))))))

(deftest a-single-historical-commit-cannot-expand-an-unbounded-parent-vector
  (fixture
    (fn [canonical private head]
      (let [parents (vec (repeatedly 17 #(hash-map :geschichte.commit/id (random-uuid))))]
        (d/transact canonical [{:geschichte.commit/id (random-uuid) :geschichte.commit/parents parents}]))
      (let [before @canonical]
        (is (= :native-parent-limit
               (try (native/publish! canonical private "review" head (operation) (constantly nil))
                    nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
        (is (= before @canonical))))))
