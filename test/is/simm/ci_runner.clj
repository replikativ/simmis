(ns is.simm.ci-runner
  "Exhaustive namespace partitioning for isolated CI JVMs. CI providers map
   their partition coordinates to the existing CIRCLE_NODE_INDEX/TOTAL inputs."
  (:refer-clojure :exclude [run!])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
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
        (throw (ex-info "Both CI partition coordinates must be decimal integers" {})))
      (let [i (parse-long index) n (parse-long total)]
        (when-not (and i n (pos? n) (< i n))
          (throw (ex-info "Invalid CI partition coordinates" {:index i :total n})))
        [i n]))))

(defn partition-namespaces
  ([names index total] (partition-namespaces names index total {}))
  ([names index total costs]
   (let [names (vec (sort names))]
     (when-not (and (seq names) (every? symbol? names)
                    (= (count names) (count (set names)))
                    (integer? total) (pos? total) (<= total (count names))
                    (integer? index) (<= 0 index) (< index total)
                    (map? costs)
                    (every? (fn [[name seconds]]
                              (and (symbol? name) (number? seconds)
                                   (Double/isFinite (double seconds)) (pos? seconds))) costs))
       (throw (ex-info "Namespace partition must be nonempty, unique and exhaustive"
                       {:index index :total total :count (count names)})))
     ;; Longest-processing-time bins use measured inclusive namespace costs.
     ;; New namespaces receive the median measured cost, never disappear. With
     ;; no history equal weights reduce to the original round-robin assignment.
     (let [measured (sort (keep costs names))
           fallback (if (seq measured) (nth measured (quot (count measured) 2)) 1)
           cost #(get costs % fallback)
           ordered (sort-by (juxt (comp - cost) str) names)
           bins (reduce (fn [bins name]
                          (let [i (first (sort-by (fn [i] [(get-in bins [i :seconds])
                                                          (count (get-in bins [i :names])) i])
                                                 (range total)))]
                            (-> bins (update-in [i :seconds] + (cost name))
                                (update-in [i :names] conj name))))
                        (vec (repeat total {:seconds 0 :names []})) ordered)]
       (vec (sort (get-in bins [index :names])))))))

(defn namespace-costs []
  (let [file (io/file ".circleci/test-durations.edn")]
    (if (.exists file)
      (let [value (edn/read-string (slurp file))]
        (when-not (= 1 (:format-version value))
          (throw (ex-info "Unsupported CI duration history" {})))
        (:namespace-seconds value))
      {})))

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
      (let [selected (partition-namespaces found index total (namespace-costs))
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
