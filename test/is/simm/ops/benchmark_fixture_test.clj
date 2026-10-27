(ns is.simm.ops.benchmark-fixture-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.discourse :as discourse]
            [dvergr.room.registry :as registry]
            [dvergr.room.store.memory :as memory]
            [dvergr.substrate.geschichte :as workspace]
            [dvergr.system.db :as control]
            [dvergr.system.rooms :as native]
            [geschichte.content :as content]
            [geschichte.git.local :as git-local]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [is.simm.ops.benchmark-fixture :as fixture]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- with-template [f]
  (let [path (.toFile (Files/createTempDirectory "simmis-template-" (make-array FileAttribute 0)))
        git! (fn [& args]
               (let [result (apply shell/sh "git" "-C" (str path) args)]
                 (when-not (zero? (:exit result)) (throw (ex-info "Test template Git failed" result)))))]
    (try
      (git! "init" "-q")
      (spit (io/file path "AGENTS.md") "Trusted fixture instructions\n")
      (git! "add" "AGENTS.md")
      (git! "-c" "user.email=fixture@example.invalid" "-c" "user.name=Fixture" "commit" "-qm" "Template")
      (with-redefs [workspace/sandbox-repo (constantly (str path))] (f))
      (finally (doseq [file (reverse (file-seq path))] (io/delete-file file true))))))

(defn- with-store [f]
  (let [config {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
                :keep-history? false :commit-graph? true}]
    (d/create-database config)
    (let [conn (d/connect config)]
      (try (f conn)
           (finally (d/release conn) (d/delete-database config))))))

(deftest fixture-profile-imports-real-template-once-and-opens-without-importing
  (with-template
    (fn []
      (with-store
        (fn [conn]
          (let [policy (get (fixture/policies) fixture/profile)
                options {:profile fixture/profile}]
            ((:initialize! policy) conn options)
            (let [db @conn tree (query/worktree db)]
              (is (= ["AGENTS.md"] (vec (keys tree))))
              (is (= "Trusted fixture instructions\n"
                     (String. ^bytes (content/read-by-id conn (get-in tree ["AGENTS.md" :content])) "UTF-8")))
              (is (:clean? (repo/status conn)))
              (with-redefs [git-local/import! (fn [& _] (throw (AssertionError. "Hydration must not import")))]
                ((:prepare-open! policy) conn options))
              (is (= (d/commit-id db) (d/commit-id @conn))))))))))

(deftest failed-template-import-never-falls-back-to-an-empty-published-fixture
  (with-template
    (fn []
      (with-store
        (fn [conn]
          (let [policy (get (fixture/policies) fixture/profile)]
            (with-redefs [git-local/import! (fn [& _] (throw (ex-info "Template unavailable" {:type ::unavailable})))]
              (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Template unavailable"
                                    ((:initialize! policy) conn {:profile fixture/profile}))))
            (is (empty? (query/worktree @conn)))))))))

(defn- with-retirement [f]
  (with-store
    (fn [conn]
      (let [root (context/create-execution-context)
            request-id (random-uuid) persistent-id (random-uuid) repository (random-uuid)
            owned (atom [])
            make! (fn []
                    (let [room (discourse/make-room
                                {:id (keyword (fixture/slug request-id)) :store (memory/make)
                                 :ctx (context/fork-context root :mode :frozen)})]
                      (swap! owned conj room) room))]
        (try
          (d/transact conn control/schema)
          (d/transact conn [{:system/id repository :system/type :repo :system/repository-profile fixture/profile}
                            {:room/id persistent-id :room/slug (fixture/slug request-id)
                             :room/default-repository [:system/id repository]}])
          ;; This fixture isolates Room close/archive ordering. Full native
          ;; lease retirement is exercised with real profiled Rooms separately.
          (with-redefs [control/get-conn (constantly conn)
                        native/retire-room-runtime!
                        (fn [id room]
                          (is (= persistent-id id))
                          (is (discourse/room-closed? room))
                          (is (inst? (:room/archived-at (d/entity @conn [:room/id id]))))
                          true)]
            (binding [ec/*execution-context* root]
              (try
                (f {:root root :conn conn :request-id request-id :make! make!
                    :receipt {:room (make!) :persistent-id persistent-id}})
                (finally
                  (doseq [room (reverse @owned)]
                    (when (identical? room (registry/lookup (:id room)))
                      (discourse/close-room! room)))))))
          (finally (context/close-context! root)))))))

(deftest retirement-requires-positive-archive-and-keeps-exact-retry
  (doseq [fault [:noop :lost-ack]]
    (with-retirement
      (fn [{:keys [root conn request-id receipt]}]
        (let [archive! control/archive-room!]
          (with-redefs [control/archive-room!
                        (fn [id] (when (= :lost-ack fault) (archive! id))
                          (when (= :lost-ack fault) (throw (ex-info "Lost archive ACK" {}))))]
            (is (thrown? clojure.lang.ExceptionInfo (fixture/retire! root request-id receipt))))
          (is (true? (fixture/retire! root request-id receipt)))
          (is (inst? (:room/archived-at (d/entity @conn [:room/id (:persistent-id receipt)])))))))))

(deftest replacement-between-close-and-archive-is-never-adopted
  (with-retirement
    (fn [{:keys [root conn request-id receipt make!]}]
      (let [close! discourse/close-room! replacement (atom nil)]
        (with-redefs [discourse/close-room! (fn [room] (close! room {}) (reset! replacement (make!)))]
          (is (thrown? clojure.lang.ExceptionInfo (fixture/retire! root request-id receipt))))
        (is (identical? @replacement (registry/lookup (keyword (fixture/slug request-id)))))
        (is (nil? (:room/archived-at (d/entity @conn [:room/id (:persistent-id receipt)]))))
        (is (thrown? clojure.lang.ExceptionInfo (fixture/retire! root request-id receipt)))
        (is (identical? @replacement (registry/lookup (keyword (fixture/slug request-id)))))))))
