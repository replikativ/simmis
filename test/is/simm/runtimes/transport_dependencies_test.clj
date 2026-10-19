(ns is.simm.runtimes.transport-dependencies-test
  (:require [clojure.test :refer [deftest is]]
            [org.httpkit.server]))

(deftest httpkit-has-one-classpath-owner
  ;; The original and maintained-fork coordinates both define these classes.
  ;; Having both makes classpath order choose WebSocket behavior, independently
  ;; of the version selected for either coordinate by tools.deps.
  (doseq [resource ["org/httpkit/server/HttpServer.class" "org/httpkit/server.clj"]]
    (let [owners (->> (.getResources (clojure.lang.RT/baseLoader) resource)
                      enumeration-seq (map str) distinct vec)]
      (is (= 1 (count owners)) (str resource " must have one owner, found " owners)))))
