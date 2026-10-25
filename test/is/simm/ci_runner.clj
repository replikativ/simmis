(ns is.simm.ci-runner
  "Exhaustive namespace partitioning for isolated CircleCI containers."
  (:refer-clojure :exclude [run!])
  (:require [clojure.java.io :as io]
            [kaocha.config :as config]
            [kaocha.load :as load]
            [kaocha.runner :as runner]
            [lambdaisland.tools.namespace.find :as find])
  (:import (java.util.regex Pattern)))

(defn coordinates [index total]
  (if (and (nil? index) (nil? total))
    [0 1]
    (do
      (when-not (and (string? index) (re-matches #"[0-9]+" index)
                     (string? total) (re-matches #"[0-9]+" total))
        (throw (ex-info "Both CircleCI partition coordinates must be decimal integers" {})))
      (let [i (parse-long index) n (parse-long total)]
        (when-not (and i n (pos? n) (< i n))
          (throw (ex-info "Invalid CircleCI partition coordinates" {:index i :total n})))
        [i n]))))

(defn partition-namespaces [names index total]
  (let [names (vec (sort names))]
    (when-not (and (seq names) (every? symbol? names)
                   (= (count names) (count (set names)))
                   (integer? total) (pos? total) (<= total (count names))
                   (integer? index) (<= 0 index) (< index total))
      (throw (ex-info "Namespace partition must be nonempty, unique and exhaustive"
                      {:index index :total total :count (count names)})))
    (->> names (keep-indexed #(when (= index (mod %1 total)) %2)) vec)))

(defn partition-config [effective index total]
  (let [suites (:kaocha/tests effective) suite (first suites)]
    ;; A new suite or loader needs a deliberate partition implementation. Never
    ;; silently omit it just because today's baseline has one clojure.test suite.
    (when-not (and (= 1 (count suites))
                   (= :unit (:kaocha.testable/id suite))
                   (= :kaocha.type/clojure.test (:kaocha.testable/type suite))
                   (= ["test"] (:kaocha/test-paths suite))
                   (= ["-test$"] (:kaocha/ns-patterns suite)))
      (throw (ex-info "Unsupported test discovery configuration" {:suites suites})))
    (let [found (vec (load/find-test-nss (:kaocha/test-paths suite)
                                        (mapv re-pattern (:kaocha/ns-patterns suite))))]
      (when (some find/reader-exception? found)
        (throw (ex-info "Cannot partition unreadable test namespaces" {})))
      (let [selected (partition-namespaces found index total)
            manifest {:index index :total total :all (vec (sort found)) :selected selected}
            patterns (mapv #(str "^" (Pattern/quote (str %)) "$") selected)]
        {:manifest manifest
         :config (assoc effective :kaocha/tests [(assoc suite :kaocha/ns-patterns patterns)])}))))

(defn run! [options]
  (let [[index total] (coordinates (System/getenv "CIRCLE_NODE_INDEX")
                                    (System/getenv "CIRCLE_NODE_TOTAL"))
        effective (config/merge-config (config/load-config) (config/normalize options))
        {:keys [manifest config]} (partition-config effective index total)
        file (io/file "target/ci-partitions" (str "node-" index ".edn"))]
    (io/make-parents file)
    (spit file (pr-str manifest))
    ;; exec-fn retains the ordinary runner's nonzero failure/reader-error exit.
    ;; Pass original options so exec-fn merges defaults/plugins only once.
    (runner/exec-fn (assoc options :kaocha/tests (:kaocha/tests config)))))
