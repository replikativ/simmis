(ns is.simm.runtimes.screen-nil-id-test
  "An owner's delete of ONE screenshot / recording must never widen to all of
   them. A malformed session id parses to nil, and datahike reads a nil `:in`
   binding as unbound — so `delete-session!` matched every session and deleted
   the owner's whole recordings directory; `delete-frame!` with a nil blob-id
   retracted every screenshot."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [is.simm.runtimes.screen-intake :as intake]
            [is.simm.runtimes.screen-recording :as rec]))

(defn- mem-conn [schema]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (d/transact conn schema)
      conn)))

(defn- datoms [conn]
  (set (map (juxt :e :a :v) (d/datoms @conn :eavt))))

(defn- missing-id? [f]
  (try (f) false
       (catch clojure.lang.ExceptionInfo e
         (= :missing-id (:type (ex-data e))))))

(defn- files-under [^java.io.File dir]
  (set (map str (filter #(.isFile ^java.io.File %) (file-seq dir)))))

(deftest delete-session-refuses-malformed-id
  (let [owner (random-uuid)
        root (doto (.toFile (java.nio.file.Files/createTempDirectory
                             "simmis-rec-test"
                             (make-array java.nio.file.attribute.FileAttribute 0)))
               .deleteOnExit)
        conn (mem-conn @#'rec/recording-schema)
        sids [(random-uuid) (random-uuid)]]
    (swap! @#'rec/conns assoc owner conn)
    (try
      (with-redefs-fn {#'rec/root (str root)}
        (fn []
          (doseq [[i sid] (map-indexed vector sids)]
            (d/transact conn [{:recording.session/id sid
                               :recording.session/started-at i}
                              {:recording.segment/session [:recording.session/id sid]
                               :recording.segment/idx 0
                               :recording.segment/blob-id (str "blob-" i)}])
            (let [f (io/file root (str owner) (str sid) "seg-0.webm")]
              (io/make-parents f)
              (spit f "bytes")))
          (let [rows-before (datoms conn)
                files-before (files-under root)]
            (is (= 2 (count files-before)))
            (testing "a malformed id throws before any query or file op"
              (is (missing-id? #(rec/delete-session! owner "not-a-uuid")))
              (is (missing-id? #(rec/delete-session! owner nil))))
            (is (= rows-before (datoms conn)) "no session or segment row retracted")
            (is (= files-before (files-under root)) "no recording file deleted"))
          (testing "a well-formed id still deletes exactly its own session"
            (is (= {:deleted-segments 1}
                   (rec/delete-session! owner (str (first sids)))))
            (is (= 1 (count (files-under root)))))))
      (finally
        (swap! @#'rec/conns dissoc owner)
        (doseq [f (reverse (file-seq root))] (.delete ^java.io.File f))))))

(deftest delete-frame-refuses-nil-blob-id
  (let [party (random-uuid)
        conn (mem-conn @#'intake/screens-schema)]
    (swap! @#'intake/screens-conns assoc party conn)
    (try
      (d/transact conn [{:screenshot/at 1 :screenshot/blob-id "a" :screenshot/transcript "x"}
                        {:screenshot/at 2 :screenshot/blob-id "b" :screenshot/transcript "y"}])
      (let [before (datoms conn)]
        (is (missing-id? #(intake/delete-frame! party nil)))
        (is (= before (datoms conn)) "no screenshot retracted"))
      (is (= {:deleted 1} (intake/delete-frame! party "a")))
      (finally
        (swap! @#'intake/screens-conns dissoc party)))))
