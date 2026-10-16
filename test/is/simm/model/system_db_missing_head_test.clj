(ns is.simm.model.system-db-missing-head-test
  "A system-DB directory with no `:db` head — a swept store, a partial copy —
   must stop the boot with a message that says so, and must be left alone."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [dvergr.substrate.paths :as paths]
            [dvergr.system.db :as sdb]
            [is.simm.model.system-db :as system-db]))

(defn- delete-tree! [path]
  (when (.exists (io/file path))
    (run! io/delete-file (reverse (file-seq (io/file path))))))

(deftest refuses-a-store-without-a-db-head
  (let [original-home (paths/home)
        original-conn @system-db/conn
        home (str (io/file (System/getProperty "java.io.tmpdir")
                           (str "simmis-missing-head-" (random-uuid))))]
    (try
      (paths/set-home! home)
      (sdb/reset-conn!)
      (reset! system-db/conn nil)
      (let [dir (io/file (paths/system-db-dir))
            ;; what the wipe left: the directory and one unrelated key file
            leftover (io/file dir "branches.ksv")]
        (.mkdirs dir)
        (spit leftover "x")
        (let [e (try (system-db/init!) nil (catch clojure.lang.ExceptionInfo e e))]
          (is (some? e) "boot refuses")
          (is (= :system-db/missing-db-head (:type (ex-data e))))
          (is (re-find #"has no :db head \(data loss or partial copy\); refusing to create over it"
                       (ex-message e)))
          (is (= (paths/system-db-dir) (:path (ex-data e)))))
        (is (.exists leftover) "nothing was deleted"))
      (finally
        (sdb/reset-conn!)
        (reset! system-db/conn original-conn)
        (paths/set-home! original-home)
        (delete-tree! home)))))
