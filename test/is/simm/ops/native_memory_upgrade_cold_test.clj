(ns is.simm.ops.native-memory-upgrade-cold-test
  "The upgrade must survive actual native Room startup in a new process.
   Opening just the two Datahike stores missed this boundary previously."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util.concurrent TimeUnit)))

(defn- log-output [file]
  (with-open [reader (io/reader file)]
    (let [buffer (char-array 65536)
          length (.read reader buffer)]
      (when (pos? length) (String. buffer 0 length)))))

(defn- phase! [directory script & args]
  (let [log (io/file directory (str script "-" (or (second args) "prepare") ".log"))
        command (into [(str (io/file (System/getProperty "java.home") "bin/java"))
                       "-Xmx1024m" "-XX:MaxMetaspaceSize=384m"
                       "-Dspindel.missing-state=throw"
                       "-cp" (System/getProperty "java.class.path")
                       "clojure.main" (str "test/fixtures/memory-upgrade/" script ".clj")]
                      args)
        builder (doto (ProcessBuilder. ^java.util.List command)
                  (.redirectErrorStream true)
                  (.redirectOutput log))
        environment (.environment builder)]
    ;; No real installation, provider credentials, or source checkouts are used.
    (doseq [key (vec (.keySet environment))
            :when (or (.startsWith ^String key "SIMMIS_")
                      (.endsWith ^String key "_API_KEY")
                      (#{"JAVA_TOOL_OPTIONS" "JDK_JAVA_OPTIONS" "_JAVA_OPTIONS"} key))]
      (.remove environment key))
    (.put environment "DVERGR_HOME" (str (io/file directory "isolated-runtime")))
    (let [process (.start builder)]
      (try
        (when-not (.waitFor process 240 TimeUnit/SECONDS)
          (throw (ex-info "Cold Memory upgrade phase timed out"
                          {:script script :log (str log) :output (log-output log)})))
        (when-not (zero? (.exitValue process))
          (throw (ex-info "Cold Memory upgrade phase failed" {:script script :log (str log)
                                                              :output (log-output log)})))
        (finally
          (when (.isAlive process)
            (.destroyForcibly process)
            (.waitFor process)))))))

(deftest legacy-upgrade-survives-native-cold-start-and-later-file-edits
  (let [parent (io/file "target/test-tmp")
        _ (.mkdirs parent)
        directory (.toFile (Files/createTempDirectory (.toPath parent) "native-memory-upgrade-"
                                                      (make-array FileAttribute 0)))
        home (.getAbsolutePath (io/file directory "memory"))]
    (try
      ;; One synthetic legacy Room, then two genuinely fresh native starts.
      ;; The fixture verifies exact history, dirty files, grants and old flags;
      ;; the second start must still admit after a malformed authored edit.
      (phase! directory "prepare" home)
      (phase! directory "reopen" home "edit")
      (phase! directory "reopen" home "check")
      (is true "Completed receipt permits native Room startup without freezing authored files")
      (finally
        ;; All writers have physically joined before child success; remove
        ;; only the unique synthetic directory allocated by this test.
        (doseq [file (reverse (file-seq directory))]
          (io/delete-file file true))))))
