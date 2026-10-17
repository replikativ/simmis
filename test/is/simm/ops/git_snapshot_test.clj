(ns is.simm.ops.git-snapshot-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is]]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.rooms.files :as files]
            [geschichte.content :as content]
            [geschichte.query :as query]
            [is.simm.ops.git-snapshot :as git]
            [is.simm.ops.workflows-test :as fixture])
  (:import [java.nio.file Files]
           [java.util Arrays Base64]))

(deftest real-git-verifies-and-clones-binary-executable-and-empty-snapshots
  (let [dir (.toFile (Files/createTempDirectory "simmis-git-snapshot-" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (doseq [[name source] [["populated" {:id (random-uuid)
                                           :files {"docs/readme.md" {:bytes (.getBytes "Hello λ" "UTF-8") :mode 0x81A4}
                                                   "bin/run" {:bytes (.getBytes "#!/bin/sh\nexit 0\n" "UTF-8") :mode 0x81ED}
                                                   "data.bin" {:bytes (byte-array [0 1 -1 0]) :mode 0x81A4}}}]
                             ["empty" {:id (random-uuid) :files {}}]]]
        (let [result (git/bundle source)
              bundle (io/file dir (str name ".bundle"))
              checkout (io/file dir name)]
          (with-open [out (io/output-stream bundle)] (.write out ^bytes (:data result)))
          (is (Arrays/equals ^bytes (:data result) ^bytes (:data (git/bundle source))))
          (is (zero? (:exit (shell/sh "git" "init" "--bare" (str (io/file dir (str name "-verify")))))))
          (is (zero? (:exit (shell/sh "git" "bundle" "verify" (.getPath bundle)
                                      :dir (.getPath (io/file dir (str name "-verify")))))))
          (is (zero? (:exit (shell/sh "git" "clone" "-b" "simmis-snapshot" (.getPath bundle) (.getPath checkout)))))
          (is (= "1" (clojure.string/trim (:out (shell/sh "git" "rev-list" "--count" "HEAD" :dir (.getPath checkout))))))
          (when (= name "populated")
            (is (= "Hello λ" (slurp (io/file checkout "docs/readme.md"))))
            (is (Arrays/equals (byte-array [0 1 -1 0]) (Files/readAllBytes (.toPath (io/file checkout "data.bin")))))
            (is (.canExecute (io/file checkout "bin/run"))))))
      (finally
        (doseq [file (reverse (file-seq dir))] (Files/delete (.toPath file)))))))

(deftest chunks-pin-current-manifest-and-never-commit-or-mix-revisions
  (@#'fixture/with-workspace
   (fn [room]
     (workspace/seed! room {"/docs/readme.md" "before"})
     (let [before (query/worktree @(:conn (dvergr.substrate.geschichte/current-workspace)))
           descriptor (git/describe room)
           part (git/chunk room (:snapshot descriptor) 0 20)
           rest (git/chunk room (:snapshot descriptor) 20 (:chunk-bytes git/policy))
           combined (byte-array (concat (.decode (Base64/getDecoder) ^String (:base64 part))
                                        (.decode (Base64/getDecoder) ^String (:base64 rest))))]
       (is (= before (query/worktree @(:conn (dvergr.substrate.geschichte/current-workspace)))))
       (is (= 20 (:next-offset part)))
       (is (= (:bytes descriptor) (alength combined)))
       (is (false? (:history descriptor)))
       (is (= (:commit descriptor) (:commit part) (:commit rest)))
       (files/write! room {"/docs/readme.md" {:text "after"}} {})
       (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Workspace changed"
                             (git/chunk room (:snapshot descriptor) 20 20)))))))

(deftest snapshot-limits-and-unsafe-paths-refuse-before-reading-content
  (@#'fixture/with-workspace
   (fn [room]
     (doseq [entries [{"bad" {:size -1 :mode 0x81A4}}
                      {"huge" {:size 1048577 :mode 0x81A4}}
                      {"link" {:size 3 :mode 0xA1FF}}
                      {"../private" {:size 3 :mode 0x81A4}}
                      {".git/config" {:size 3 :mode 0x81A4}}
                      {"a//b" {:size 3 :mode 0x81A4}}
                      (into {} (for [i (range 513)] [(str i) {:size 0 :mode 0x81A4}]))
                      (into {} (for [i (range 9)] [(str i) {:size 1048576 :mode 0x81A4}]))]]
       (with-redefs [query/worktree (constantly entries)
                     content/read-by-id (fn [& _] (throw (AssertionError. "validate first")))]
         (is (thrown? clojure.lang.ExceptionInfo (git/snapshot room nil)))))
     (with-redefs [query/worktree (constantly {"bad-size" {:size 0 :mode 0x81A4 :content (random-uuid)}})
                   content/read-by-id (constantly (byte-array [1]))]
       (is (thrown-with-msg? clojure.lang.ExceptionInfo #"pinned size" (git/snapshot room nil)))))))

(deftest encoded-size-and-invalid-offsets-are-bounded
  (with-redefs [git/policy (assoc git/policy :max-bundle-bytes 1)]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Encoded bundle" (git/bundle {:id (random-uuid) :files {}}))))
  (doseq [[offset length] [[-1 10] [0 0] [0 16385] [9437185 1]]]
    (is (thrown? clojure.lang.ExceptionInfo (git/chunk nil (random-uuid) offset length)))))
