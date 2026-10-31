(ns is.simm.ops.offline-memory-upgrade-cli-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [is.simm.ops.offline-memory-upgrade :as upgrade]
            [is.simm.ops.offline-memory-upgrade-test :as fixture]
            [is.simm.ops.offline-memory-upgrade-cli :as cli]))

(deftest service-check-refuses-live-unmasked-or-listening-service
  (doseq [[state listeners expected]
          [["LoadState=masked\nActiveState=inactive\nMainPID=0\n" "" :stopped]
           ["LoadState=loaded\nActiveState=inactive\nMainPID=0\n" "" :held]
           ["LoadState=masked\nActiveState=active\nMainPID=0\n" "" :held]
           ["LoadState=masked\nActiveState=inactive\nMainPID=42\n" "" :held]
           ["LoadState=masked\nActiveState=inactive\nMainPID=0\n" "LISTEN 0 50 127.0.0.1:47888 0.0.0.0:*\n" :held]]]
    (with-redefs-fn
      {#'cli/command (fn [args] (if (= "systemctl" (first args)) state listeners))}
      #(if (= :stopped expected)
         (is (nil? (cli/require-stopped!)))
         (is (thrown? clojure.lang.ExceptionInfo (cli/require-stopped!)))))))

(deftest recovery-manifest-cannot-select-a-copied-scope-or-change-the-actor
  (fixture/with-upgrade
   (fn [{:keys [control repo request]}]
     (upgrade/upgrade! control repo (upgrade/inspect! control repo request))
     (is (= request (cli/verify-recovery-request! @control request)))
     (doseq [changed [(assoc request :actor (random-uuid))
                      (assoc request :room (random-uuid))
                      (assoc-in request [:target :scope] "/copied/scope")
                      (assoc-in request [:target :store] (random-uuid))
                      (assoc-in request [:target :repository] (random-uuid))]]
       (is (thrown? clojure.lang.ExceptionInfo (cli/verify-recovery-request! @control changed)))))))

(deftest private-manifest-permissions-use-the-real-filesystem-result
  (let [directory (.toPath (doto (io/file "target" "offline-upgrade-guards") .mkdirs))
        path (java.nio.file.Files/createTempFile directory "private-" ".edn"
                                                (make-array java.nio.file.attribute.FileAttribute 0))]
    (try
      (java.nio.file.Files/setPosixFilePermissions path
        (java.nio.file.attribute.PosixFilePermissions/fromString "rw-------"))
      (is (nil? (@#'cli/private-file! path)))
      (java.nio.file.Files/setPosixFilePermissions path
        (java.nio.file.attribute.PosixFilePermissions/fromString "rw-r--r--"))
      (is (thrown? clojure.lang.ExceptionInfo (@#'cli/private-file! path)))
      (finally (java.nio.file.Files/delete path)))))
