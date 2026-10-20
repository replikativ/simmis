(ns is.simm.ops.git-snapshot-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is]]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.rooms.files :as files]
            [dvergr.substrate.geschichte :as native]
            [geschichte.content :as content]
            [geschichte.query :as query]
            [hasch.core :as hasch]
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

(deftest immutable-exports-read-one-db-and-preserve-selected-v1-identity
  (@#'fixture/with-workspace
   (fn [room]
     (workspace/seed! room {"/docs/readme.md" "original λ" "/other.md" "other"})
     (let [conn (:conn (native/current-workspace)) db @conn
           entries (into (sorted-map) (query/worktree db))
           selected-id (hasch/uuid [:simmis/git-snapshot git/policy (:id room) entries])
           room-id (random-uuid) stamp (random-uuid)
           selected (git/snapshot room nil)
           original-read content/read-by-id calls (atom 0)]
       (is (= selected-id (:id selected)))
       (workspace/seed! room {"/docs/readme.md" "later" "/new.md" "new"})
       (with-redefs [native/current-workspace (fn [] (throw (AssertionError. "No selected lookup in immutable export")))
                     content/read-by-id (fn [reader id & options]
                                          (swap! calls inc)
                                          (is (identical? db @reader))
                                          (is (not (identical? conn reader)))
                                          (apply original-read reader id options))]
         (let [owned (git/snapshot-in db room-id stamp nil)]
           (is (pos? @calls))
           (is (not= selected-id (:id owned)))
           (is (= (keys (:files selected)) (keys (:files owned))))
           (doseq [path (keys (:files selected))]
             (is (Arrays/equals ^bytes (get-in selected [:files path :bytes])
                                ^bytes (get-in owned [:files path :bytes]))))
           (is (= (hasch/uuid [:simmis/git-snapshot-owned-v1 git/policy room-id stamp entries]) (:id owned)))))
       (is (not= selected-id (:id (git/snapshot room nil))))))))

(deftest identical-export-bytes-have-distinct-room-and-target-identities
  (@#'fixture/with-workspace
   (fn [room]
     (workspace/seed! room {"/same.md" "same"})
     (let [db @(:conn (native/current-workspace)) room-id (random-uuid) stamp (random-uuid)
           first (git/describe-in db room-id stamp)
           other (git/describe-in db room-id (random-uuid))
           other-room (git/describe-in db (random-uuid) stamp)]
       (is (= 3 (count (set [(:snapshot first) (:snapshot other) (:snapshot other-room)]))))
       (is (= 1 (:files first) (:files other) (:files other-room)))
       (is (= first (git/describe-in db room-id stamp)))
       (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Reject stale identity before content")))]
         (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Workspace changed"
                               (git/snapshot-in db room-id (random-uuid) (:snapshot first))))
         (doseq [[rid target] [[nil stamp] ["room" stamp] [room-id nil] [room-id (str stamp)]]]
           (is (thrown? clojure.lang.ExceptionInfo (git/snapshot-in db rid target nil)))))))))

(deftest immutable-export-chunks-pin-manifest-and-target-before-reading-bodies
  (@#'fixture/with-workspace
   (fn [room]
     (workspace/seed! room {"/readme.md" "Hello 🦋"})
     (let [conn (:conn (native/current-workspace)) db @conn
           room-id (random-uuid) stamp (random-uuid)
           descriptor (git/describe-in db room-id stamp)
           head (git/chunk-in db room-id stamp (:snapshot descriptor) 0 20)]
       (workspace/seed! room {"/readme.md" "changed"})
       (let [tail (git/chunk-in db room-id stamp (:snapshot descriptor) 20 (:chunk-bytes git/policy))
             combined (byte-array (concat (.decode (Base64/getDecoder) ^String (:base64 head))
                                          (.decode (Base64/getDecoder) ^String (:base64 tail))))]
         (is (= (:bytes descriptor) (alength combined)))
         (is (= (:commit descriptor) (:commit head) (:commit tail)))
         (is (Arrays/equals ^bytes (:data (git/bundle (git/snapshot-in db room-id stamp nil))) combined)))
       (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Invalid chunk must not read a body")))]
         (is (thrown? clojure.lang.ExceptionInfo (git/chunk-in @conn room-id stamp (:snapshot descriptor) 20 20)))
         (doseq [[offset length] [[-1 1] [0 0] [0 16385] [9437185 1] ["0" 1]]]
           (is (thrown? clojure.lang.ExceptionInfo
                        (git/chunk-in db room-id stamp (:snapshot descriptor) offset length)))))))))

(deftest immutable-export-metadata-is-checked-before-any-body-allocation
  (@#'fixture/with-workspace
   (fn [room]
     (workspace/seed! room {"/valid.md" "valid"})
     (let [db @(:conn (native/current-workspace)) entry (get (query/worktree db) "valid.md")
           room-id (random-uuid) stamp (random-uuid)]
       (doseq [invalid [(assoc entry :content (random-uuid)) (assoc entry :content "not-a-uuid")
                        (assoc entry :mode nil) (assoc entry :mode 0xA1FF)
                        (assoc entry :size nil) (assoc entry :size -1) (update entry :size inc)]]
         (with-redefs [query/worktree (constantly {"a.md" entry "z.md" invalid})
                       content/read-by-id (fn [& _] (throw (AssertionError. "Validate all metadata first")))]
           (is (thrown? clojure.lang.ExceptionInfo (git/snapshot-in db room-id stamp nil)))))))))
