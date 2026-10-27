(ns is.simm.ops.workflows-test
  (:require [clojure.test :refer [deftest is testing]]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.catalog.room :as catalog]
            [dvergr.discourse :as discourse]
            [dvergr.room.store.memory :as memory]
            [dvergr.substrate.geschichte :as native]
            [datahike.api :as d]
            [geschichte.content :as content]
            [geschichte.query :as query]
            [hasch.core :as hasch]
            [is.simm.ops.benchmark-dataset :as dataset]
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
          (is (true? (get-in removed [:workflow :ok])) "ordinary programs do not require synthetic fixtures")
          (is (false? (get-in removed [:workflow :benchmark-ready?]))))))))

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
      (let [db @(:conn (native/current-workspace))
            eid (:db/id (d/entity db [:geschichte.work/path "workflows/echo/gold.edn"]))]
        (doseq [[attr value] [[:geschichte.work/mode 0xA000]
                              [:geschichte.work/size 999]]]
          (let [corrupt (:db-after (d/with db [[:db/add eid attr value]]))]
            (is (thrown? clojure.lang.ExceptionInfo (workflows/snapshot-in corrupt "echo"))))))
      (let [db @(:conn (native/current-workspace))
            read-by-id content/read-by-id]
        (with-redefs [content/read-by-id
                      (fn [reader id & options]
                        (let [bytes (aclone ^bytes (apply read-by-id reader id options))]
                          (aset-byte bytes 0 (unchecked-byte 0xC3))
                          (aset-byte bytes 1 (unchecked-byte 0x28))
                          bytes))]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"valid UTF-8"
                                (workflows/snapshot-in db "echo"))))))))

(deftest snapshot-bounds-are-checked-before-reading-content
  (with-workspace
    (fn [room]
      (workspace/seed! room {"/seed.md" "seed"})
      (let [db @(:conn (native/current-workspace))
            content-id (:geschichte.content/id (:geschichte.work/content (d/entity db [:geschichte.work/path "seed.md"])))]
        (doseq [[message rows]
                [[#"too many files" (mapv (fn [i] {:geschichte.work/path (str "workflows/echo/" i)})
                                          (range (inc workflows/max-files)))]
                 [#"exceeds the byte limit"
                  (mapv (fn [i] {:geschichte.work/path (str "workflows/echo/" i)
                                 :geschichte.work/content [:geschichte.content/id content-id]
                                 :geschichte.work/mode 33188 :geschichte.work/size workflows/max-file-bytes})
                        (range 5))]]]
          (let [corrupt (:db-after (d/with db rows))]
            (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "must reject manifest first")))]
              (is (thrown-with-msg? clojure.lang.ExceptionInfo message
                                    (workflows/snapshot-in corrupt "echo"))))))))))

(deftest malformed-supporting-edn-is-structural-not-executable
  (with-workspace
    (fn [room]
      (doseq [path ["gold.edn" "calibration.edn"]]
        (let [result (workflows/write! room principal
                                       {:name "echo" :files (assoc fixture path "{:unclosed")})]
          (is (some? (get-in result [:write :commit])))
          (is (false? (get-in result [:workflow :ok])))
          (is (false? (get-in result [:workflow :checker-executed?]))))))))

(deftest immutable-workflow-projections-retain-bodies-and-portable-identities
  (with-workspace
    (fn [room]
      (workflows/write! room principal {:name "echo" :files fixture})
      (let [conn (:conn (native/current-workspace)) db @conn
            selected (workflows/details room "echo")
            original-read content/read-by-id reads (atom 0)]
        (workspace/seed! room {"/workflows/echo/gold.edn" "{:expected :later}"})
        (with-redefs [native/current-workspace (fn [] (throw (AssertionError. "No ambient selection in a projector")))
                      content/read-by-id (fn [reader id & options]
                                           (swap! reads inc)
                                           (is (identical? db @reader) "Content entities use the exact frozen DB too")
                                           (is (not (identical? conn reader)))
                                           (apply original-read reader id options))]
          (let [captured (workflows/snapshot-in db "echo")
                projected (workflows/details-in db "echo")]
            (is (pos? @reads))
            (is (= fixture (:files captured)))
            (is (= selected projected))
            (is (= (:revision selected) (str (workflows/source-revision "echo" captured))))
            (is (= (get-in selected [:inspection :bundle])
                   (:bundle (workflows/inspect-snapshot "echo" captured))))))
        (is (not= selected (workflows/details room "echo")))))))

(deftest immutable-workflow-pages-do-not-follow-later-manifests-or-bytes
  (with-workspace
    (fn [room]
      (let [text (apply str (repeat 5000 "a🦋世"))
            files (into (assoc fixture "large.md" text)
                        (for [n (range 12)] [(format "extra/%02d.md" n) (str n)]))]
        (workflows/write! room principal {:name "echo" :files files})
        (workspace/seed! room (into {} (for [n (range 27)]
                                         [(format "/workflows/draft-%02d/broken.edn" n) "{:unclosed"])))
        (let [db @(:conn (native/current-workspace))
              names (workflows/list-page-in db {})
              metadata (workflows/source-page-in db {:name "echo"})
              revision (:revision metadata)]
          (workspace/seed! room {"/workflows/another/new.edn" "{}"
                                 "/workflows/echo/large.md" "newer"})
          (with-redefs [native/current-workspace (fn [] (throw (AssertionError. "No ambient selection in paging")))]
            (let [next-names (workflows/list-page-in db {:offset (:next-offset names)
                                                         :expected-revision (parse-uuid (:revision names))})
                  next-files (workflows/source-page-in db {:name "echo" :file-offset (:next-file-offset metadata)
                                                           :expected-revision revision})
                  output (java.io.ByteArrayOutputStream.)]
              (is (= 28 (:total names)))
              (is (= 28 (count (concat (:names names) (:names next-names)))))
              (is (= (:revision names) (:revision next-names)))
              (is (= (sort (keys files)) (map :path (concat (:files metadata) (:files next-files)))))
              (loop [offset 0]
                (let [part (workflows/source-page-in db {:name "echo" :path "large.md" :offset offset
                                                         :expected-revision revision})
                      bytes (.decode (java.util.Base64/getDecoder) ^String (:base64 part))]
                  (is (<= (alength bytes) 8192))
                  (.write output bytes 0 (alength bytes))
                  (when-let [next (:next-offset part)] (recur next))))
              (is (= text (.toString output "UTF-8")))))
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"names changed"
                                (workflows/list-page room {:expected-revision (parse-uuid (:revision names))})))
          (is (not= revision (:revision (workflows/source-page room {:name "echo"})))))))))

(deftest immutable-workflow-metadata-and-page-bounds-fail-before-body-reads
  (with-workspace
    (fn [room]
      (workflows/write! room principal {:name "echo" :files fixture})
      (let [db @(:conn (native/current-workspace))
            entity (d/entity db [:geschichte.work/path "workflows/echo/gold.edn"])
            eid (:db/id entity)
            content-eid (:db/id (:geschichte.work/content entity))]
        (doseq [tx [[[:db/retract eid :geschichte.work/content]]
                    [[:db/retract eid :geschichte.work/mode]]
                    [[:db/retract eid :geschichte.work/size]]
                    [[:db/add eid :geschichte.work/mode 0xA1FF]]
                    [[:db/add eid :geschichte.work/size -1]]
                    [[:db/add eid :geschichte.work/size 999]]
                    [[:db/retract content-eid :geschichte.content/id]]
                    [[:db/retract content-eid :geschichte.content/size]]
                    [{:geschichte.work/path "workflows/echo/z-hidden.md"}]]]
          (let [corrupt (:db-after (d/with db tx))]
            (with-redefs [query/worktree (fn [& _] (throw (AssertionError. "No whole-worktree inner join")))
                          content/read-by-id (fn [& _] (throw (AssertionError. "Validate all metadata first")))]
              (is (thrown? clojure.lang.ExceptionInfo (workflows/snapshot-in corrupt "echo"))))))
        (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Reject invalid paging before decode")))]
          (doseq [request [{:offset -1} {:offset "0"} {:file-offset -1} {:file-offset 101}
                           {:file-offset 10} {:path "gold.edn"} {:offset 524289}]]
            (is (thrown? clojure.lang.ExceptionInfo
                         (workflows/source-page-in db (assoc request :name "echo"))))))))))

(deftest immutable-workflow-name-list-keeps-the-bounded-index-contract
  (let [db {:fixture :immutable-index-only} seen (atom 0)]
    (with-redefs [d/index-range (fn [actual _]
                                  (is (identical? db actual))
                                  (map (fn [n] (swap! seen inc) {:v (str "workflows/draft/file-" n)}) (range)))
                  content/read-by-id (fn [& _] (throw (AssertionError. "Listing never reads content")))]
      (is (thrown? clojure.lang.ExceptionInfo (workflows/list-page-in db {})))
      (is (= (inc workflows/max-list-entries) @seen)))))

(deftest workflow-prefix-overflow-is-lazy-and-precedes-all-entity-and-body-access
  (let [db {:fixture :prefix-only} seen (atom 0)]
    (with-redefs [d/index-range
                  (fn [actual options]
                    (is (identical? db actual))
                    (is (= {:attrid :geschichte.work/path :start "workflows/echo/" :end "workflows/echo0"}
                           options))
                    (map (fn [n] (swap! seen inc) {:e n :v (str "workflows/echo/" n)})
                         (range)))
                  d/entity (fn [& _] (throw (AssertionError. "Overflow must precede entity access")))
                  query/worktree (fn [& _] (throw (AssertionError. "No whole-index inner join")))
                  content/read-by-id (fn [& _] (throw (AssertionError. "Overflow must precede body access")))]
      (doseq [project [workflows/snapshot-in workflows/manifest-in]]
        (reset! seen 0)
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"too many files"
                              (project db "echo")))
        (is (= 101 @seen))))))

(deftest name-list-inclusive-upper-sentinel-does-not-consume-a-directory-row
  (with-workspace
    (fn [room]
      (workflows/write! room principal {:name "echo" :files fixture})
      (let [db (:db-after (d/with @(:conn (native/current-workspace))
                                  [{:geschichte.work/path "workflows0"}]))
            rows (d/index-range db {:attrid :geschichte.work/path :start "workflows/" :end "workflows0"})]
        (is (= 5 (count rows)))
        (is (= "workflows0" (:v (last rows))))
        (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Names never load source bodies")))
                      workflows/max-list-entries 4]
          (let [page (workflows/list-page-in db {})]
            (is (= ["echo"] (:names page)))
            (is (= (str (hasch/uuid [:workflow-names ["echo"]])) (:revision page)))))
        (with-redefs [workflows/max-list-entries 3]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"exceeds the listing limit"
                                (workflows/list-page-in db {}))))))))

(deftest real-prefix-boundary-preserves-manifests-source-and-portable-identities
  (with-workspace
    (fn [room]
      (workflows/write! room principal {:name "echo" :files fixture})
      (let [db @(:conn (native/current-workspace))
            entry (get (query/worktree db) "workflows/echo/gold.edn")
            extra (mapv (fn [n] {:geschichte.work/path (format "workflows/echo/extra-%03d.md" n)
                                 :geschichte.work/content [:geschichte.content/id (:content entry)]
                                 :geschichte.work/size (:size entry) :geschichte.work/mode (:mode entry)})
                        (range (- workflows/max-files (count fixture))))
            full (:db-after (d/with db (conj extra
                                             {:geschichte.work/path "unrelated/incomplete.md"}
                                           ;; The inclusive upper sentinel is
                                           ;; unrelated even with invalid attrs.
                                             {:geschichte.work/path "workflows/echo0"})))
            ;; Explicitly retain the former valid-file ordering/shape and ID
            ;; formulas, independently of the new collector.
            legacy-manifest (mapv (fn [[path entry]] (assoc entry :path (subs path (count "workflows/echo/"))))
                                  (into (sorted-map)
                                        (filter (fn [[path _]] (clojure.string/starts-with? path "workflows/echo/")))
                                        (query/worktree full)))
            overflow (:db-after (d/with full [(assoc (first extra) :geschichte.work/path "workflows/echo/overflow.md")]))]
        (is (= 100 (count legacy-manifest)))
        (is (= "workflows/echo0"
               (:v (last (d/index-range full {:attrid :geschichte.work/path
                                              :start "workflows/echo/" :end "workflows/echo0"})))))
        (with-redefs [query/worktree (fn [& _] (throw (AssertionError. "No whole-worktree query")))]
          (let [snapshot (workflows/snapshot-in full "echo")]
            (is (= legacy-manifest (:manifest snapshot)))
            (is (= (hasch/uuid {:name "echo" :manifest legacy-manifest})
                   (workflows/source-revision "echo" snapshot)))
            (is (= (str (:id (catalog/bundle "echo" (:files snapshot))))
                   (:bundle (workflows/inspect-snapshot "echo" snapshot))))))
        (with-redefs [d/entity (fn [& _] (throw (AssertionError. "101 rows refuse before metadata")))
                      content/read-by-id (fn [& _] (throw (AssertionError. "101 rows refuse before bodies")))]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"too many files"
                                (workflows/snapshot-in overflow "echo"))))))))

(deftest actual-utf8-byte-boundaries-are-inclusive-and-refuse-overflow-before-bodies
  (with-workspace
    (fn [room]
      (let [text (apply str (repeat (quot workflows/max-file-bytes 4) "🦋"))]
        (is (= workflows/max-file-bytes (alength (.getBytes text "UTF-8"))))
        ;; Identical content deduplicates, but each source file counts toward
        ;; the bundle's actual byte budget independently.
        (workspace/seed! room (into {"/one-byte.md" "x" "/oversized.md" (str text "x")}
                                    (for [n (range 4)] [(str "/workflows/echo/" n ".md") text])))
        (let [db @(:conn (native/current-workspace))
              snapshot (workflows/snapshot-in db "echo")
              alias-row (fn [source destination]
                          (let [entity (d/entity db [:geschichte.work/path source])]
                            {:geschichte.work/path destination
                             :geschichte.work/content [:geschichte.content/id (:geschichte.content/id (:geschichte.work/content entity))]
                             :geschichte.work/size (:geschichte.work/size entity)
                             :geschichte.work/mode (:geschichte.work/mode entity)}))]
          (is (= 4 (count (:files snapshot))))
          (is (every? #(= text %) (vals (:files snapshot))))
          (is (= workflows/max-bundle-bytes (reduce + (map :size (:manifest snapshot)))))
          (doseq [[message row]
                  [[#"exceeds the byte limit" (alias-row "one-byte.md" "workflows/echo/extra.md")]
                   [#"bounded regular text files" (alias-row "oversized.md" "workflows/echo/0.md")]]]
            (let [overflow (:db-after (d/with db [row]))]
              (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Metadata byte limit before bodies")))]
                (is (thrown-with-msg? clojure.lang.ExceptionInfo message
                                      (workflows/snapshot-in overflow "echo")))))))))))

(deftest manifest-projection-is-metadata-only-and-shares-the-complete-row-validator
  (with-workspace
    (fn [room]
      (workflows/write! room principal {:name "echo" :files fixture})
      (let [db @(:conn (native/current-workspace))
            expected (:manifest (workflows/snapshot-in db "echo"))
            corrupt (:db-after (d/with db [{:geschichte.work/path "workflows/echo/hidden.md"}]))]
        (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Manifest never loads bodies")))
                      query/worktree (fn [& _] (throw (AssertionError. "No omitting worktree join")))
                      native/current-workspace (fn [] (throw (AssertionError. "No ambient selection")))]
          (is (= expected (workflows/manifest-in db "echo")))
          (is (= [] (workflows/manifest-in db "empty")))
          (is (thrown? clojure.lang.ExceptionInfo (workflows/manifest-in corrupt "echo"))))))))

(deftest captured-details-and-pages-do-not-reacquire-source-or-follow-selected-edits
  (with-workspace
    (fn [room]
      (let [text (apply str (repeat 5000 "🦋世"))
            files (assoc fixture "large.md" text)]
        (workflows/write! room principal {:name "echo" :files files})
        (let [conn (:conn (native/current-workspace)) db @conn
              captured (workflows/snapshot-in db "echo")
              expected (workflows/details-in db "echo")
              expected-page (workflows/source-page-in db {:name "echo"})
              forbidden (fn [& _] (throw (AssertionError. "Captured formatter must not reacquire source")))]
          (workspace/seed! room {"/workflows/echo/gold.edn" "{:expected :newer}"})
          (with-redefs [d/index-range forbidden d/entity forbidden content/read-by-id forbidden
                        native/current-workspace forbidden workflows/snapshot forbidden workflows/snapshot-in forbidden
                        workflows/details forbidden workflows/details-in forbidden]
            (let [details (workflows/details-of "echo" captured)
                  revision (:revision details)
                  chunks (loop [offset 0 chunks []]
                           (let [page (workflows/source-page-of details {:name "echo" :path "large.md"
                                                                         :offset offset :expected-revision revision})
                                 bytes (.decode (java.util.Base64/getDecoder) ^String (:base64 page))]
                             (is (<= (alength bytes) 8192))
                             (if-let [next-offset (:next-offset page)]
                               (recur next-offset (conj chunks bytes))
                               (conj chunks bytes))))]
              (is (= expected details))
              (is (= files (:files details)))
              (is (= expected-page (workflows/source-page-of details {:name "echo"})))
              (is (= text (String. ^bytes (byte-array (mapcat seq chunks)) "UTF-8")))
              (let [host-revision (str (random-uuid))
                    retagged (-> details (assoc :revision host-revision)
                                 (assoc-in [:inspection :revision] host-revision))
                    page (workflows/source-page-of retagged {:name "echo" :expected-revision host-revision})]
                (is (= host-revision (:revision page)))
                (is (= (get-in details [:inspection :bundle]) (get-in page [:inspection :bundle])))))))))))

(deftest captured-page-validation-and-invalid-drafts-remain-pure
  (let [empty-source {:files (sorted-map) :manifest []}
        draft-source {:files (sorted-map "workflow.edn" "{:unclosed")
                      :manifest [{:path "workflow.edn" :content (random-uuid) :size 10 :mode 33188}]}
        empty-details (workflows/details-of "empty" empty-source)
        draft-details (workflows/details-of "draft" draft-source)]
    (is (= (str (workflows/source-revision "empty" empty-source)) (:revision empty-details)))
    (is (false? (get-in empty-details [:inspection :ok])))
    (is (false? (get-in draft-details [:inspection :ok])))
    (is (= 0 (:file-count (workflows/source-page-of empty-details {:name "empty"}))))
    (is (= "workflow.edn" (get-in (workflows/source-page-of draft-details {:name "draft"}) [:files 0 :path])))
    (doseq [request [{:name "other"} {:name "draft" :offset -1}
                     {:name "draft" :path "workflow.edn"} {:name "draft" :file-offset 1}
                     {:name "draft" :expected-revision "not-a-uuid"}
                     {:name "draft" :expected-revision (str (random-uuid))}]]
      (is (thrown? clojure.lang.ExceptionInfo (workflows/source-page-of draft-details request))))))

(deftest inspection-propagates-direct-and-wrapped-interruption-instead-of-draft-problems
  (doseq [stage [:bundle :dataset] wrapped? [false true]]
    (let [bundle catalog/bundle calls (atom {:bundle 0 :dataset 0})
          interrupted (InterruptedException. "cancelled")
          failure (if wrapped? (ex-info "wrapped interruption" {} interrupted) interrupted)]
      (try
        (with-redefs [catalog/bundle (fn [& args]
                                       (swap! calls update :bundle inc)
                                       (if (= stage :bundle) (throw failure) (apply bundle args)))
                      dataset/manifest (fn [& _] (swap! calls update :dataset inc) (throw failure))]
          (let [error (try (workflows/details-of "echo" {:files fixture :manifest []})
                           nil (catch Exception error error))
                ;; clojure.test's report-counter STM may consume interruption.
                ;; Observe the operation BEFORE any assertion/reporting.
                interrupted? (.isInterrupted (Thread/currentThread))]
            (is (identical? failure error))
            (is interrupted?))
          (is (= {:bundle 1 :dataset (if (= stage :bundle) 0 1)} @calls)))
        (finally (Thread/interrupted))))))

(deftest ordinary-programs-and-benchmark-packs-have-distinct-readiness
  (let [files {"workflow.edn" "{:title \"Plain task\" :task \"Write /out/result.md\" :capture [\"/out\"]}"
               "instructions.md" "Instructions remain ordinary pinned files."}
        source (workflows/details-of "plain" {:files files :manifest []})]
    (is (true? (get-in source [:inspection :ok])))
    (is (false? (get-in source [:inspection :benchmark-ready?])))
    (is (= 2 (count (get-in source [:inspection :benchmark-problems]))))
    (is (= "Plain task" (get-in source [:definition :title])))
    (is (false? (:ok (workflows/inspect-snapshot "plain" {:files files :manifest []})))))
  (doseq [definition ["{:title \"Task\" :task \"do it\" :params {:review true}}"
                      "{:title \"Task\" :task \"do it\" :params :invalid}" "nil" "["]]
    (let [source (workflows/details-of "broken" {:files {"workflow.edn" definition} :manifest []})]
      (is (false? (get-in source [:inspection :ok])))
      (is (seq (get-in source [:inspection :problems]))))))
