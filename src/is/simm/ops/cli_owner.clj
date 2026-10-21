(ns is.simm.ops.cli-owner
  "Opt-in host boundary for subscription CLI benchmarks. Not a native-code sandbox.
   Crash recovery remains conservative: no automatic CLI wallet return."
  (:require [clojure.string :as str])
  (:import [java.lang ProcessHandle]))

(defn validate-owner
  "Pure verification of manager evidence against the actual JVM's cgroup."
  [unit pid cgroup boot properties]
  (when-not (and (string? unit) (str/ends-with? unit ".service")
                 (= unit (get properties "Id"))
                 (= (str pid) (get properties "MainPID"))
                 (= "active" (get properties "ActiveState"))
                 (= "main" (get properties "ExitType"))
                 (= "no" (get properties "RemainAfterExit"))
                 (= "control-group" (get properties "KillMode"))
                 (= "yes" (get properties "SendSIGKILL"))
                 (#{"9" "SIGKILL"} (get properties "FinalKillSignal"))
                 (re-matches #"[0-9a-f]{32}" (get properties "InvocationID" ""))
                 (seq boot) (seq (get properties "TimeoutStopUSec"))
                 (not (#{"infinity" "0"} (get properties "TimeoutStopUSec")))
                 (seq (get properties "ControlGroup"))
                 (some #{(str "0::" (get properties "ControlGroup"))} (str/split-lines cgroup)))
    (throw (ex-info "CLI benchmarks require a verified dedicated cgroup-v2 systemd service with bounded whole-cgroup cleanup"
                    {:type ::unavailable})))
  {:version 1 :unit unit :boot (str/trim boot)
   :invocation (get properties "InvocationID")})

(defn ownership!
  "Trusted operator JVM properties only; room scripts cannot opt themselves in.
   Manager queries are bounded and never infer ownership from environment alone."
  []
  (let [unit (System/getProperty "simmis.cli-benchmarks.unit")
        manager (System/getProperty "simmis.cli-benchmarks.manager" "user")]
    (when-not (and (= "true" (System/getProperty "simmis.cli-benchmarks.enabled"))
                   (string? unit) (re-matches #"[a-zA-Z0-9_.@-]+\.service" unit)
                   (#{"user" "system"} manager))
      (throw (ex-info "CLI benchmarks are disabled on this installation" {:type ::disabled})))
    (let [command (into ["/usr/bin/systemctl"]
                        (concat (when (= "user" manager) ["--user"])
                                ["show" unit "--no-pager"
                                 "--property=Id,MainPID,ActiveState,ExitType,RemainAfterExit,KillMode,SendSIGKILL,FinalKillSignal,TimeoutStopUSec,ControlGroup,InvocationID"]))
          process (.start (ProcessBuilder. ^java.util.List command))]
      (try
        (when-not (.waitFor process 3 java.util.concurrent.TimeUnit/SECONDS)
          (throw (ex-info "Service ownership query timed out" {:type ::unavailable})))
        (when-not (zero? (.exitValue process))
          (throw (ex-info "Service ownership cannot be verified" {:type ::unavailable})))
        (let [properties (into {} (map #(str/split % #"=" 2)
                                      (str/split-lines (slurp (.getInputStream process)))))]
          (assoc (validate-owner unit (.pid (ProcessHandle/current))
                                 (java.nio.file.Files/readString (java.nio.file.Path/of "/proc/self/cgroup" (make-array String 0)))
                                 (java.nio.file.Files/readString (java.nio.file.Path/of "/proc/sys/kernel/random/boot_id" (make-array String 0))) properties)
                 :manager manager))
        (finally
          (.destroyForcibly process)
          (.waitFor process 1000 java.util.concurrent.TimeUnit/MILLISECONDS))))))
