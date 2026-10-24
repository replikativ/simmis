(ns is.simm.uis.web.desktop.memory-metadata-test
  (:require [is.simm.uis.web.desktop.memory-metadata :as metadata]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])))

(defn- request [paths] {:folders ["" "wiki"] :paths (vec paths)})

(deftest identical-and-subset-owners-share-a-whole-request
  (let [same (request ["wiki/a.md"])
        groups (metadata/batches {:sidebar same :first-tab same
                                  :second-tab {:folders [""] :paths []}})]
    (is (= 1 (count groups)))
    (is (= same (:request (first groups))))
    (is (= #{:sidebar :first-tab :second-tab} (:owners (first groups))))))

(deftest aggregate-overflow-keeps-each-owner-whole-without-dropping-paths
  (let [a (mapv #(str "wiki/a" % ".md") (range 40))
        b (mapv #(str "wiki/b" % ".md") (range 40))
        demands {:first (request a) :second (request b)}
        groups (metadata/batches demands)]
    (is (= 2 (count groups)))
    (is (= #{:first :second} (into #{} (mapcat :owners) groups)))
    (is (= (set (concat a b)) (into #{} (mapcat #(get-in % [:request :paths])) groups)))
    (doseq [{:keys [owners request]} groups]
      (is (<= (count (:paths request)) 64))
      (doseq [owner owners]
        (is (= (set (get-in demands [owner :paths])) (set (:paths request))))))))

(deftest exact-boundary-and-request-order-are-deterministic
  (let [paths (mapv #(str "wiki/" % ".md") (range 64))
        a (request (take 63 paths)) b (request [(last paths)])
        groups (metadata/batches (array-map :b b :a a))]
    (is (= 1 (count groups)))
    (is (= 64 (count (get-in groups [0 :request :paths]))))
    (is (= groups (metadata/batches (array-map :a a :b b))))
    (is (= [] (metadata/batches {})))
    (is (= {:folders ["" "wiki"] :paths ["wiki/a.md"]}
           (metadata/normalize-request {:folders ["wiki" "" "wiki"]
                                        :paths ["wiki/a.md" "wiki/a.md"]})))))

(deftest explicit-absence-is-covered-and-omitted-paths-or-folders-are-not
  (let [snapshot {:folders {"" {:entries []}} :entries {"wiki/missing.md" nil}}]
    (is (metadata/covered? snapshot {:folders [""] :paths ["wiki/missing.md"]}))
    (is (not (metadata/covered? snapshot {:folders [""] :paths ["wiki/unknown.md"]})))
    (is (not (metadata/covered? snapshot {:folders ["wiki"] :paths ["wiki/missing.md"]})))
    (is (not (metadata/covered? {} {:folders [] :paths []})))))

(deftest invalid-owner-demands-refuse-instead-of-silently-truncating
  (doseq [bad [nil {:folders [] :paths [] :extra true}
               {:folders (vec (repeat 65 "wiki")) :paths []}
               {:folders [] :paths (mapv str (range 65))}
               {:folders [] :paths [nil]} {:folders '() :paths []}]]
    (is (try (metadata/batches {:good (request []) :bad bad}) false
             (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
               (= :is.simm.uis.web.desktop.memory-metadata/invalid-demand (:type (ex-data e))))))))
