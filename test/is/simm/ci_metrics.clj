(ns is.simm.ci-metrics
  "CI artifacts built from Kaocha's profiling measurements. Namespace residual
   includes :once fixtures and runner overhead, not just application work."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [kaocha.plugin :refer [defplugin]]
            [kaocha.plugin.profiling :as profiling]
            [kaocha.testable :as testable])
  (:import (java.lang.management ManagementFactory)))

(defn- registered-count [s]
  ;; Observe only already-loaded registries; never load a backend or retain its
  ;; connections/stores in timing artifacts. Named memory stores intentionally
  ;; outlive release until explicitly deleted by their fixture owner.
  (when-let [n (find-ns (symbol (namespace s)))]
    (when-let [v (ns-resolve n (symbol (name s)))]
      (let [registry @v]
        (when (instance? clojure.lang.IDeref registry)
          (let [entries @registry]
            (when (map? entries) (count entries))))))))

(defn resources []
  (let [os (ManagementFactory/getOperatingSystemMXBean)
        heap (.getHeapMemoryUsage (ManagementFactory/getMemoryMXBean))]
    {:at-ms (System/currentTimeMillis)
     :uptime-ms (.getUptime (ManagementFactory/getRuntimeMXBean))
     :process-cpu-ns (when (instance? com.sun.management.OperatingSystemMXBean os)
                       (.getProcessCpuTime ^com.sun.management.OperatingSystemMXBean os))
     :processors (.getAvailableProcessors os)
     :load-average (.getSystemLoadAverage os)
     :heap-used-bytes (.getUsed heap)
     :heap-committed-bytes (.getCommitted heap)
     :heap-max-bytes (.getMax heap)
     :threads (.getThreadCount (ManagementFactory/getThreadMXBean))
     :registries {:named-memory-stores (registered-count 'konserve.memory/memory-store-registry)
                  :datahike-connections (registered-count 'datahike.connections/*connections*)}
     :gc (mapv (fn [gc] {:name (.getName gc) :count (.getCollectionCount gc)
                         :elapsed-ms (.getCollectionTime gc)})
               (ManagementFactory/getGarbageCollectorMXBeans))}))

(defn measurement [node]
  (let [elapsed (::profiling/duration node)
        children (:kaocha.result/tests node)
        child-elapsed (keep ::profiling/duration children)]
    (cond-> {:id (:kaocha.testable/id node)
             :type (:kaocha.testable/type node)
             :elapsed-ns elapsed
             :counts (select-keys node [:kaocha.result/pass :kaocha.result/fail
                                        :kaocha.result/error :kaocha.result/pending])}
      (seq child-elapsed)
      (assoc :children-elapsed-ns (reduce + child-elapsed)
             ;; Includes once-fixture setup/cleanup and Kaocha/report overhead.
             ;; Each-fixtures stay in the individual test duration.
             :outside-children-ns (- elapsed (reduce + child-elapsed))))))

(defn- output-file [suffix]
  (io/file "target/ci-timings" (str "node-" (or (System/getenv "CIRCLE_NODE_INDEX") "0") suffix)))

(defn- write! [suffix value append?]
  (let [file (output-file suffix)]
    (io/make-parents file)
    (spit file (str (pr-str value) "\n") :append append?)))

(defplugin is.simm.ci-metrics/metrics
  (pre-load [config]
    (let [start (resources)]
      (write! "-progress.edn" {:phase :start :resources start} false)
      (assoc config ::start start)))
  (post-load [plan]
    (let [loaded (resources)]
      (write! "-progress.edn" {:phase :loaded :resources loaded} true)
      (assoc plan ::loaded loaded)))
  (pre-run [plan]
    (let [selected (->> (testable/test-seq plan)
                        (filter #(= :kaocha.type/var (:kaocha.testable/type %)))
                        (map :kaocha.testable/id) sort vec)]
      (write! "-selection.edn" {:tests selected} false)
      ;; Local focused probes can require an exact manifest before executing
      ;; anything, guarding against misspelled/ignored Kaocha filter options.
      (when-let [path (System/getenv "SIMMIS_CI_EXPECTED_TESTS")]
        (let [expected (vec (sort (edn/read-string (slurp path))))]
          (when-not (= expected selected)
            (throw (ex-info "Focused test selection differs from the expected manifest"
                            {:expected expected :selected selected}))))))
    plan)
  (pre-test [node _]
    (cond-> node
      (= :kaocha.type/ns (:kaocha.testable/type node))
      (assoc ::resources-before (resources))))
  (post-test [node _]
    (when (= :kaocha.type/ns (:kaocha.testable/type node))
      ;; Flush each completed namespace so a timeout still leaves useful data.
      (write! "-progress.edn"
              {:phase :namespace :measurement (measurement node)
               :before (::resources-before node) :after (resources)} true))
    node)
  (post-summary [result]
    (write! ".edn"
            {:format-version 1
             :revision (or (System/getenv "CIRCLE_SHA1") (System/getenv "GITHUB_SHA"))
             :start (::start result) :loaded (::loaded result) :end (resources)
             :elapsed-ns (::profiling/duration result)
             :measurements (mapv measurement
                                 (filter ::profiling/duration (testable/test-seq result)))} false)
    result))
