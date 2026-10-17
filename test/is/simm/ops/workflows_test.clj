(ns is.simm.ops.workflows-test
  (:require [clojure.test :refer [deftest is testing]]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.room.store.memory :as memory]
            [geschichte.content :as content]
            [geschichte.query :as query]
            [is.simm.ops.workflows :as workflows]
            [org.replikativ.spindel.engine.core :as ec]))

(def principal {:principal/id "party/fixture-author"})

(deftest transport-preserves-directory-and-unicode-filenames-and-refuses-duplicates
  (let [path "fixtures/资料/输入.md"
        input {:name "echo" :files {(keyword path) "input"}}]
    (is (= {path "input"} (:files (workflows/transport-write input))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (workflows/transport-write (assoc-in input [:files path] "duplicate")))))
  (is (thrown? clojure.lang.ExceptionInfo
               (workflows/transport-write {:name "echo" :files {"workflow.edn" "{}"} :eval true}))))
(def fixture
  {"workflow.edn" "{:title \"Echo document\" :task \"Read /docs/input.md and write /out/result.md\" :capture [\"/out\"]}"
   "checker.clj" "(throw (ex-info \"This must NOT execute during shape validation\" {}))"
   "gold.edn" "{:expected \"original\"}"
   "fixtures/docs/input.md" "Original document"})

(defn- with-workspace [f]
  (let [room (discourse/make-room {:id (keyword (str "workflow-files-" (random-uuid)))
                                   :store (memory/make)})]
    (try
      (binding [ec/*execution-context* (:ctx room)]
        (workspace/ensure-workspace! room)
        (f room))
      (finally (discourse/close-room! room)))))

(deftest workflow-authoring-reuses-one-repository-commit-and-never-runs-checker
  (with-workspace
    (fn [room]
      (let [written (workflows/write! room principal {:name "echo" :files fixture})
            inspected (:workflow written)]
        (is (some? (get-in written [:write :commit])))
        (is (:ok inspected))
        (is (false? (:checker-executed? inspected)))
        (is (= inspected (workflows/inspect room "echo")))
        (is (= fixture (:files (workflows/snapshot room "echo"))))
        (is (nil? (get-in (workflows/write! room principal {:name "echo" :files fixture})
                          [:write :commit])) "identical content makes no extra commit")
        (let [removed (workflows/write! room principal
                                        {:name "echo" :files {"fixtures/docs/input.md" nil}})]
          (is (= ["/workflows/echo/fixtures/docs/input.md"] (get-in removed [:write :removed])))
          (is (false? (get-in removed [:workflow :ok])) "incomplete committed edits are acknowledged"))))))

(deftest workflow-capability-cannot-write-other-room-or-sensitive-paths
  (with-workspace
    (fn [room]
      (workspace/seed! room {"/AGENTS.md" "unchanged instructions"})
      (doseq [name ["../other" "a/b" "a.." "UPPER" "" "a\n"]]
        (is (thrown? clojure.lang.ExceptionInfo
                     (workflows/write! room principal {:name name :files {"workflow.edn" "{}"}}))))
      (doseq [path ["../other/workflow.edn" "../../AGENTS.md" ".git/config" ".env"
                    "/AGENTS.md" "fixtures/../input" "foo\\bar"]]
        (is (thrown? clojure.lang.ExceptionInfo
                     (workflows/write! room principal {:name "echo" :files {path "bad"}}))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (workflows/write! room principal {:name "echo"
                                                     :files {"docs//a" "one" "docs/a" "two"}})))
      (is (= {"/AGENTS.md" "unchanged instructions"} (workspace/read-tree room "/"))))))

(deftest byte-limits-refuse-multibyte-content-before-any-write
  (with-workspace
    (fn [room]
      (is (thrown? clojure.lang.ExceptionInfo
                   (workflows/write! room principal
                                     {:name "echo" :files {"large.md" (apply str (repeat 200000 "世"))}})))
      (is (= {} (workspace/read-tree room "/"))))))

(deftest snapshot-uses-pinned-content-not-files-modified-during-collection
  (with-workspace
    (fn [room]
      (workflows/write! room principal {:name "echo" :files fixture})
      (let [original-read content/read-by-id
            modified (atom false)
            snapshot (with-redefs [content/read-by-id
                                   (fn [& args]
                                     (when (compare-and-set! modified false true)
                                       (workspace/seed! room
                                                        {"/workflows/echo/gold.edn" "{:expected \"changed\"}"
                                                         "/workflows/echo/fixtures/docs/input.md" "Changed document"}))
                                     (apply original-read args))]
                       (workflows/snapshot room "echo"))]
        (is @modified)
        (is (= fixture (:files snapshot)))
        (is (not= (:bundle (workflows/inspect-snapshot "echo" snapshot))
                  (:bundle (workflows/inspect room "echo"))))))))

(deftest structural-validation-does-not-accept-non-map-definitions
  (with-workspace
    (fn [room]
      (let [result (workflows/write! room principal
                                     {:name "echo" :files (assoc fixture "workflow.edn" "42")})]
        (is (some? (get-in result [:write :commit])))
        (is (false? (get-in result [:workflow :ok])))))))

(deftest committed-writes-remain-acknowledged-if-inspection-fails
  (with-workspace
    (fn [room]
      (let [result (with-redefs [workflows/inspect (fn [& _] (throw (ex-info "inspection unavailable" {})))]
                     (workflows/write! room principal {:name "echo" :files fixture}))]
        (is (some? (get-in result [:write :commit])))
        (is (false? (get-in result [:workflow :ok])))
        (is (false? (get-in result [:workflow :checker-executed?])))
        (is (= fixture (:files (workflows/snapshot room "echo"))))))))

(deftest snapshot-refuses-nonregular-corrupt-and-invalid-text-entries
  (with-workspace
    (fn [room]
      (workflows/write! room principal {:name "echo" :files fixture})
      (let [read-tree query/worktree]
        (doseq [change [(fn [entry] (assoc entry :mode 0xA000))
                        (fn [entry] (update entry :size inc))]]
          (with-redefs [query/worktree
                        (fn [db] (update (read-tree db) "workflows/echo/gold.edn" change))]
            (is (thrown? clojure.lang.ExceptionInfo (workflows/snapshot room "echo"))))))
      (with-redefs [content/read-by-id (fn [& _] (byte-array [(unchecked-byte 0xC3)]))
                    query/worktree (fn [_] {"workflows/echo/invalid.txt"
                                            {:mode 0x81A4 :size 1 :content (random-uuid)}})]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"valid UTF-8"
                              (workflows/snapshot room "echo")))))))

(deftest snapshot-bounds-are-checked-before-reading-content
  (with-workspace
    (fn [room]
      (doseq [entries [(into {} (for [i (range (inc workflows/max-files))]
                                  [(str "workflows/echo/" i)
                                   {:mode 0x81A4 :size 0 :content (random-uuid)}]))
                       (into {} (for [i (range 5)]
                                  [(str "workflows/echo/" i)
                                   {:mode 0x81A4 :size workflows/max-file-bytes :content (random-uuid)}]))]]
        (with-redefs [query/worktree (constantly entries)
                      content/read-by-id (fn [& _] (throw (AssertionError. "must reject manifest first")))]
          (is (thrown? clojure.lang.ExceptionInfo (workflows/snapshot room "echo"))))))))

(deftest malformed-supporting-edn-is-structural-not-executable
  (with-workspace
    (fn [room]
      (doseq [path ["gold.edn" "calibration.edn"]]
        (let [result (workflows/write! room principal
                                       {:name "echo" :files (assoc fixture path "{:unclosed")})]
          (is (some? (get-in result [:write :commit])))
          (is (false? (get-in result [:workflow :ok])))
          (is (false? (get-in result [:workflow :checker-executed?]))))))))
