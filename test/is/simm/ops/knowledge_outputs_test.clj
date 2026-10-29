(ns is.simm.ops.knowledge-outputs-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [datahike.api :as d]
            [datahike.blob :as blob]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.room.store.memory :as memory]
            [geschichte.content :as content]
            [geschichte.repo :as repo]
            [is.simm.documents.markdown :as markdown]
            [is.simm.ops.knowledge-outputs :as outputs])
  (:import [java.util Arrays]))

(defn- with-repository [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
             :keep-history? true :branch-history? true
             :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (repo/init! conn {:name "Exact knowledge output tests"}) (f conn)
           (finally (d/release conn))))))

(defn- write! [conn path text] (repo/write! conn path (markdown/source-bytes text)))

(deftest exact-utf8-bytes-preserve-bom-crlf-and-one-logical-content-read
  (with-repository
    (fn [conn]
      (let [text "\uFEFF# 知識\r\n\r\nLine\r\n" bytes (markdown/source-bytes text)
            original content/read-by-id calls (atom 0)]
        (write! conn "out/wiki/nested/page.md" text)
        (let [captured (with-redefs [content/read-by-id
                                     (fn [& args]
                                      ;; The two-arity API delegates to its
                                      ;; three-arity implementation; count calls
                                      ;; from the capturer, not that trampoline.
                                       (when (= 2 (count args)) (swap! calls inc))
                                       (apply original args))]
                         (outputs/capture-db @conn))
              descriptor (get-in captured [:files "wiki/nested/page.md"])]
          (is (= 1 @calls))
          (is (= (alength bytes) (:bytes descriptor)))
          (is (= 33188 (:mode descriptor)))
          (is (= (blob/blob-id bytes) (:content descriptor)))
          (is (Arrays/equals ^bytes bytes ^bytes (outputs/bytes! descriptor)))
          (is (= text (markdown/decode (outputs/bytes! descriptor))))
          (is (= captured (outputs/validate! captured))))))))

(deftest capture-is-pinned-to-one-immutable-worktree
  (with-repository
    (fn [conn]
      (write! conn "out/wiki/page.md" "Before")
      (let [db @conn]
        (write! conn "out/wiki/page.md" "After")
        (is (= "Before" (markdown/decode (outputs/bytes!
                                          (get-in (outputs/capture-db db) [:files "wiki/page.md"])))))
        (is (= "After" (markdown/decode (outputs/bytes!
                                         (get-in (outputs/capture-db @conn) [:files "wiki/page.md"])))))))))

(deftest outside-output-endpoint-does-not-consume-a-file-slot-or-read-its-body
  (with-repository
    (fn [conn]
      (write! conn "out/wiki/a.md" "A")
      (write! conn "out/wiki/b.md" "B")
      (let [db @conn expected (outputs/capture-db db)
            siblings (:db-after (d/with db [{:geschichte.work/path "out/wiki0"}]))
            entry (:db/id (d/entity db [:geschichte.work/path "out/wiki/a.md"]))
            file (d/pull db '[:geschichte.work/size :geschichte.work/mode
                              {:geschichte.work/content [:geschichte.content/id]}] entry)
            overflow (:db-after (d/with siblings [{:geschichte.work/path "out/wiki/c.md"
                                                   :geschichte.work/content
                                                   [:geschichte.content/id (get-in file [:geschichte.work/content :geschichte.content/id])]
                                                   :geschichte.work/size (:geschichte.work/size file)
                                                   :geschichte.work/mode (:geschichte.work/mode file)}]))
            original content/read-by-id calls (atom 0)]
        (is (= "out/wiki0" (:v (last (d/index-range siblings {:attrid :geschichte.work/path
                                                              :start "out/wiki/" :end "out/wiki0"})))))
        (with-redefs [outputs/policy (assoc outputs/policy :max-files 2)
                      content/read-by-id (fn [reader id & options]
                                           (when (empty? options) (swap! calls inc))
                                           (apply original reader id options))]
          (is (= expected (outputs/capture-db siblings)))
          (is (= 2 @calls)))
        (with-redefs [outputs/policy (assoc outputs/policy :max-files 2)
                      content/read-by-id (fn [& _] (throw (AssertionError. "Reject true overflow before bodies")))]
          (is (= :too-many-files
                 (:reason (ex-data (try (outputs/capture-db overflow)
                                        (catch clojure.lang.ExceptionInfo e e)))))))))))

(deftest unsupported-files-and-ancestor-occupancy-refuse-before-body-reads
  (doseq [[path mode] [["out" 33188] ["out/wiki" 40960]
                       ["out/wiki/page.md" 40960] ["out/wiki/page.md" 33261]
                       ["out/wiki/private.edn" 33188] ["out/wiki/../escape.md" 33188]]]
    (with-repository
      (fn [conn]
        (let [canonical (if (= path "out/wiki/../escape.md") "out/wiki/page.md" path)]
          (write! conn canonical "Unsupported")
          (d/transact conn [{:db/id [:geschichte.work/path canonical]
                             :geschichte.work/path path :geschichte.work/mode mode}]))
        (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Preflight read content")))]
          (is (thrown? clojure.lang.ExceptionInfo (outputs/capture-db @conn)) path))))))

(deftest byte-count-and-file-count-preflight-never-truncate-or-read-bodies
  (with-repository
    (fn [conn]
      (write! conn "out/wiki/a.md" "你好")
      (write! conn "out/wiki/b.md" "B")
      (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Limit check read content")))
                    outputs/policy (assoc outputs/policy :max-files 1)]
        (is (thrown? clojure.lang.ExceptionInfo (outputs/capture-db @conn))))
      (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Limit check read content")))
                    outputs/policy (assoc outputs/policy :max-total-bytes 6)]
        (is (thrown? clojure.lang.ExceptionInfo (outputs/capture-db @conn))))
      (d/transact conn [{:db/id [:geschichte.work/path "out/wiki/a.md"] :geschichte.work/size 1048577}])
      (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Limit check read content")))]
        (is (thrown? clojure.lang.ExceptionInfo (outputs/capture-db @conn)))))))

(deftest malformed-utf8-and-content-mismatches-are-not-text-projections
  (with-repository
    (fn [conn]
      (repo/write! conn "out/wiki/a.md" (byte-array [(unchecked-byte 0xc3) (unchecked-byte 0x28)]))
      (is (thrown? clojure.lang.ExceptionInfo (outputs/capture-db @conn)))
      (write! conn "out/wiki/a.md" "Right")
      (with-redefs [content/read-by-id (fn [& _] (markdown/source-bytes "Wrong"))]
        (is (thrown? clojure.lang.ExceptionInfo (outputs/capture-db @conn)))))))

(deftest retained-evidence-is-revalidated-and-unrelated-output-is-not-a-wiki-file
  (with-repository
    (fn [conn]
      (write! conn "out/result.edn" "{:ok true}")
      (is (empty? (:files (outputs/capture-db @conn))))
      (write! conn "out/wiki/a.md" "A")
      (let [captured (outputs/capture-db @conn)]
        (doseq [bad [(assoc-in captured [:files "wiki/a.md" :bytes] 0)
                     (assoc-in captured [:files "wiki/a.md" :mode] 33261)
                     (assoc-in captured [:files "wiki/a.md" :base64] "QQ==untrusted")
                     (assoc captured :id (random-uuid))]]
          (is (thrown? clojure.lang.ExceptionInfo (outputs/validate! bad))))))))

(deftest inconsistent-logical-size-refuses-before-body-allocation
  (with-repository
    (fn [conn]
      (write! conn "out/wiki/a.md" "Larger logical content")
      (d/transact conn [{:db/id [:geschichte.work/path "out/wiki/a.md"] :geschichte.work/size 1}])
      (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Preflight loaded lying content")))]
        (is (thrown? clojure.lang.ExceptionInfo (outputs/capture-db @conn)))))))

(deftest file-directory-and-file-ancestor-collisions-refuse-before-bodies
  (doseq [child? [true false]]
    (with-repository
      (fn [conn]
        (write! conn "out/wiki/a.md" "Parent file")
        ;; Ordinary writes now prevent these invalid trees. Construct an
        ;; immutable corrupt snapshot explicitly to retain the reader's
        ;; defensive validation coverage without weakening the write guard.
        (let [db @conn
              invalid (:db-after
                       (d/with db
                               [(if child?
                                  (assoc (d/pull db '[:geschichte.work/size :geschichte.work/mode
                                                      {:geschichte.work/content [:geschichte.content/id]}]
                                                 [:geschichte.work/path "out/wiki/a.md"])
                                         :geschichte.work/path "out/wiki/a.md/b.md")
                                  {:geschichte.work-dir/path "out/wiki/a.md"})]))]
          (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Collision loaded content")))]
            (is (thrown? clojure.lang.ExceptionInfo (outputs/capture-db invalid)))))))))

(deftest wrapper-preserves-portable-evidence-through-observe-and-versioned-ref
  (let [room (discourse/make-room {:id (keyword (str "output-wrapper-" (random-uuid)))
                                   :store (memory/make)})
        base (evaluation/make-evaluator
              {:id :fixture/completion :version 1 :basis :fixture
               :capture (fn [_] {:files {"/out/wiki/a.md" "Projection"}})
               :observe (fn [{:keys [execution/evidence]}] (select-keys evidence [:files]))
               :verify (fn [_ _] {:checks {:complete? true} :reward 1.0})})
        wrapped (outputs/with-capture base)]
    (try
      (workspace/ensure-workspace! room)
      (workspace/seed! room {"/out/wiki/a.md" "\uFEFFExact\r\n"})
      (let [evidence ((:capture wrapped) {:world/room room})
            observed ((:observe wrapped) {:execution/evidence evidence})
            portable (edn/read-string (pr-str observed))]
        (is (not= (evaluation/evaluator-ref base) (evaluation/evaluator-ref wrapped)))
        (is (= :simmis/knowledge-completion (:verifier/id (evaluation/evaluator-ref wrapped))))
        (is (= (evaluation/evaluator-ref base)
               (get-in (evaluation/evaluator-ref wrapped) [:verifier/basis :base])))
        (is (= (:files evidence) (:files portable)))
        (is (= (:knowledge-output evidence) (outputs/validate! (:knowledge-output portable))))
        (is (= "\uFEFFExact\r\n"
               (markdown/decode (outputs/bytes! (get-in portable [:knowledge-output :files "wiki/a.md"]))))))
      (finally (discourse/close-room! room)))))
