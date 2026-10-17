(ns is.simm.ops.workflows-test
  (:require [clojure.test :refer [deftest is testing]]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.room.store.memory :as memory]
            [datahike.api :as d]
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

(deftest listing-is-room-local-pinned-and-never-loads-source-blobs
  (with-workspace
    (fn [room]
      (workspace/seed! room (into {"/outside/ignored.md" "outside"}
                                  (for [n (range 28)]
                                    [(format "/workflows/draft-%02d/broken.edn" n) "{:unclosed"])))
      (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Listing must not read blobs")))]
        (let [first (workflows/list-page room {})
              next (workflows/list-page room {:offset (:next-offset first)
                                              :expected-revision (parse-uuid (:revision first))})]
          (is (= 28 (:total first)))
          (is (= 25 (count (:names first))))
          (is (= ["draft-25" "draft-26" "draft-27"] (:names next)))
          (is (nil? (:next-offset next)))
          (is (= (:revision first) (:revision next)))
          (workspace/seed! room {"/workflows/another/draft.md" "new"})
          (is (thrown? clojure.lang.ExceptionInfo
                       (workflows/list-page room {:offset 25 :expected-revision (parse-uuid (:revision first))}))))))))

(deftest listing-refuses-an-oversized-path-index-without-an-unbounded-scan
  (with-workspace
    (fn [room]
      (let [seen (atom 0)]
        (with-redefs [d/index-range (fn [& _]
                                      (map (fn [n] (swap! seen inc) {:v (str "workflows/draft/file-" n)})
                                           (range)))]
          (is (thrown? clojure.lang.ExceptionInfo (workflows/list-page room {})))
          ;; The lazy source may chunk; this input maps over an unchunked range.
          (is (= (inc workflows/max-list-entries) @seen)))))))

(deftest source-pages-are-bounded-revision-pinned-and-reassemble-utf8-bytes
  (with-workspace
    (fn [room]
      (let [text (apply str (repeat 5000 "a🦋世"))
            files (into (assoc fixture "large.md" text)
                        (for [n (range 12)] [(format "extra/%02d.md" n) (str n)]))]
        (workflows/write! room principal {:name "echo" :files files})
        (let [first (workflows/source-page room {:name "echo"})
              revision (:revision first)
              second (workflows/source-page room {:name "echo" :file-offset (:next-file-offset first)
                                                   :expected-revision revision})
              entries (concat (:files first) (:files second))
              output (java.io.ByteArrayOutputStream.)]
          (is (= 10 (count (:files first))))
          (is (= (sort (keys files)) (map :path entries)))
          (is (nil? (:next-file-offset second)))
          (is (= (count files) (:file-count first)))
          (is (false? (get-in first [:inspection :checker-executed?])))
          (is (not (contains? first :definition)))
          (loop [offset 0]
            (let [part (workflows/source-page room {:name "echo" :path "large.md" :offset offset
                                                     :expected-revision revision})
                  bytes (.decode (java.util.Base64/getDecoder) ^String (:base64 part))]
              (is (<= (alength bytes) 8192))
              (is (= offset (:offset part)))
              (.write output bytes 0 (alength bytes))
              (when-let [next (:next-offset part)] (recur next))))
          (is (= text (.toString output "UTF-8")))
          (doseq [args [{:path "large.md"} {:file-offset 10}
                       {:path "missing.md" :expected-revision revision}
                       {:path "large.md" :offset 100000 :expected-revision revision}]]
            (is (thrown? clojure.lang.ExceptionInfo (workflows/source-page room (assoc args :name "echo")))))
          (workflows/write! room principal {:name "echo" :files {"extra/00.md" "changed"}})
          (doseq [args [{:file-offset 10} {:path "large.md" :offset 8192}]]
            (is (thrown-with-msg? clojure.lang.ExceptionInfo #"source changed"
                                  (workflows/source-page room (assoc args :name "echo" :expected-revision revision))))))))))

(deftest editable-drafts-have-source-identities-even-before-they-are-valid
  (with-workspace
    (fn [room]
      (let [empty (workflows/details room "draft")
            initial (parse-uuid (:revision empty))]
        (is (uuid? initial))
        (is (false? (get-in empty [:inspection :ok])))
        (is (= (:revision empty) (get-in empty [:inspection :revision])))
        (workflows/write! room principal {:name "draft" :expected-revision initial
                                          :files {"workflow.edn" "{:unclosed"}})
        (let [draft (workflows/details room "draft")]
          (is (not= (:revision empty) (:revision draft)))
          (is (= {"workflow.edn" "{:unclosed"} (:files draft)))
          (is (false? (get-in draft [:inspection :ok])))
          (is (false? (get-in draft [:inspection :checker-executed?])))
          (is (= draft (workflows/details room "draft"))))))))

(deftest stale-saves-refuse-the-whole-batch-without-changing-files
  (with-workspace
    (fn [room]
      (workflows/write! room principal {:name "echo" :files fixture})
      (let [before (workflows/details room "echo")]
        (workflows/write! room principal {:name "echo" :files {"helper.clj" "newer helper"}})
        (let [newer (workflows/details room "echo")
              error (try
                      (workflows/write! room principal
                                        {:name "echo" :expected-revision (parse-uuid (:revision before))
                                         :files {"helper.clj" "stale helper" "workflow.edn" "stale task"}})
                      nil (catch clojure.lang.ExceptionInfo e e))]
          (is (= :is.simm.ops.workflows/conflict (:type (ex-data error))))
          (is (= newer (workflows/details room "echo")))
          (is (= "newer helper" (get-in newer [:files "helper.clj"]))))))))

(deftest concurrent-creators-with-one-source-revision-cannot-overwrite-each-other
  (with-workspace
    (fn [room]
      (let [revision (parse-uuid (:revision (workflows/details room "race")))
            start (promise)
            save (fn [text]
                   (future @start
                           (try (workflows/write! room principal
                                                  {:name "race" :expected-revision revision
                                                   :files {"draft.md" text}})
                                :written
                                (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
            a (save "one") b (save "two")]
        (deliver start true)
        (is (= {:written 1 :is.simm.ops.workflows/conflict 1} (frequencies [@a @b])))
        (is (contains? #{"one" "two"} (get-in (workflows/details room "race") [:files "draft.md"])))))))

(deftest source-revision-transport-is-explicit-and-name-and-mode-sensitive
  (let [revision (random-uuid)
        input {:name "echo" :expected-revision (str revision) :files {:draft.md "draft"}}
        snapshot {:manifest [{:path "draft.md" :mode 0x81A4 :size 0 :content (random-uuid)}]}]
    (is (= revision (:expected-revision (workflows/transport-write input))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (workflows/transport-write (assoc input :expected-revision (apply str (repeat 36 "x"))))))
    (is (not= (workflows/source-revision "one" snapshot) (workflows/source-revision "two" snapshot)))
    (is (not= (workflows/source-revision "one" snapshot)
              (workflows/source-revision "one" (assoc-in snapshot [:manifest 0 :mode] 0x81ED))))))

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
      (let [result (with-redefs [workflows/details (fn [& _] (throw (ex-info "inspection unavailable" {})))]
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
