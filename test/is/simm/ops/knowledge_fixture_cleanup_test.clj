(ns is.simm.ops.knowledge-fixture-cleanup-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.ops.knowledge-filing-test :as fixture]
            [konserve.store :as stores]))

(defn- exists? [config]
  ;; This probes the memory registry without connecting/recreating the store.
  (stores/store-exists? (:store config) {:sync? true}))

(deftest fixture-deletes-only-its-two-exact-stores-after-release
  (let [unrelated {:store {:backend :memory :id (random-uuid)}
                   :schema-flexibility :write :keep-history? false}]
    (d/create-database unrelated)
    (try
      (let [configs (@#'fixture/with-fixture
                     (fn [{:keys [conn workspace]}]
                       (let [configs (mapv #(-> % deref :config) [conn workspace])]
                         (is (= 2 (count (set (map :store configs)))))
                         (is (every? exists? configs))
                         (is (true? (exists? unrelated)))
                         configs)))]
        (is (not-any? exists? configs))
        (is (true? (exists? unrelated)) "an unrelated live store must survive"))
      (finally (d/delete-database unrelated)))
    (is (false? (exists? unrelated)))))

(deftest fixture-also-deletes-its-stores-after-an-assertion-body-throws
  (let [captured (atom nil)]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Injected body failure"
                          (@#'fixture/with-fixture
                           (fn [{:keys [conn workspace]}]
                             (reset! captured (mapv #(-> % deref :config) [conn workspace]))
                             (is (every? exists? @captured))
                             (throw (ex-info "Injected body failure" {}))))))
    (is (= 2 (count @captured)))
    (is (not-any? exists? @captured))))
