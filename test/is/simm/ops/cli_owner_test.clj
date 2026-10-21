(ns is.simm.ops.cli-owner-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.ops.cli-owner :as owner]))

(def properties
  {"Id" "fixture.service" "MainPID" "123" "ActiveState" "active"
   "ExitType" "main" "RemainAfterExit" "no" "KillMode" "control-group"
   "SendSIGKILL" "yes" "FinalKillSignal" "9" "TimeoutStopUSec" "3s"
   "ControlGroup" "/fixture.service" "InvocationID" (apply str (repeat 32 "a"))})

(deftest verifies-actual-jvm-and-whole-unit-cleanup
  (is (= "fixture.service" (:unit (owner/validate-owner "fixture.service" 123 "0::/fixture.service\n" "boot" properties))))
  (doseq [[key bad] [["MainPID" "124"] ["ActiveState" "inactive"] ["ExitType" "cgroup"]
                     ["RemainAfterExit" "yes"] ["KillMode" "process"] ["SendSIGKILL" "no"]
                     ["FinalKillSignal" "15"] ["TimeoutStopUSec" "infinity"]
                     ["InvocationID" ""] ["ControlGroup" "/other"] ["Id" "other.service"]]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (owner/validate-owner "fixture.service" 123 "0::/fixture.service\n" "boot" (assoc properties key bad)))))
  (is (thrown? clojure.lang.ExceptionInfo
               (owner/validate-owner "fixture.scope" 123 "0::/fixture.service\n" "boot" properties))))

(deftest disabled-by-default
  (when-not (System/getProperty "simmis.cli-benchmarks.enabled")
    (is (= ::owner/disabled (try (owner/ownership!) (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))
