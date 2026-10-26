(ns is.simm.ops.benchmark-case-manifest-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.versioning :as versioning]
            [dvergr.catalog.workspace :as fixtures]
            [geschichte.bytes :as bytes]
            [geschichte.content :as content]
            [geschichte.fs :as fs]
            [geschichte.repo :as repo]
            [is.simm.ops.benchmark-case-manifest :as manifest]))

(defn- fixture [f]
  (let [config {:store {:backend :memory :id (random-uuid)}
                :schema-flexibility :write :keep-history? false :commit-graph? true}]
    (d/create-database config)
    (let [conn (d/connect config)]
      (try (repo/init! conn {:name "Manifest test"}) (f conn)
           (finally (d/release conn) (d/delete-database config))))))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo error (:reason (ex-data error)))))

(deftest complete-inventory-never-loads-bodies-and-retains-empty-directories
  (fixture
   (fn [conn]
     (repo/write! conn "wiki/Page.md" (bytes/utf8 "# Page €"))
     (repo/write! conn "bin/start.clj" (bytes/utf8 "(+ 1 2)") {:mode 33261})
     (fs/mkdir! conn "empty")
     (let [before @conn
           inventory (with-redefs [content/read-by-id
                                   (fn [& _] (throw (AssertionError. "No body reads")))]
                       (manifest/inventory before))]
       (is (= ["/bin/start.clj" "/wiki/Page.md"] (vec (keys (:files inventory)))))
       (is (= ["/" "/bin" "/empty" "/wiki"] (:directories inventory)))
       (is (= 33261 (get-in inventory [:files "/bin/start.clj" :mode])))
       (is (= 10 (get-in inventory [:files "/wiki/Page.md" :size])))
       (is (= inventory (manifest/inventory before)))
       (is (= (d/commit-id before) (d/commit-id @conn)))))))

(deftest expected-files-preserve-baseline-and-match-actual-writes
  (fixture
   (fn [conn]
     (repo/write! conn "exec.clj" (bytes/utf8 "before") {:mode 33261})
     (repo/write! conn "AGENTS.md" (bytes/utf8 "Trusted instructions"))
     (fs/mkdir! conn "empty")
     (let [baseline (manifest/inventory @conn)
           files {"/exec.clj" "after €" "/wiki/Company.md" "# Company"}
           expected (manifest/after-seeding baseline files fixtures/fixture-seed-limits)]
       (doseq [[path text] files]
         (repo/write! conn (subs path 1) (bytes/utf8 text)
                      {:mode (get-in baseline [:files path :mode] 33188)}))
       (is (= expected (manifest/inventory @conn)))
       (is (= (get-in baseline [:files "/AGENTS.md"])
              (get-in expected [:files "/AGENTS.md"])))
       (is (= 33261 (get-in expected [:files "/exec.clj" :mode])))
       (is (= baseline (manifest/after-seeding baseline {} fixtures/fixture-seed-limits)))
       (is (= :file-directory-collision
              (reason #(manifest/after-seeding baseline {"/empty" "file"} fixtures/fixture-seed-limits))))
       (is (= :file-directory-collision
              (reason #(manifest/after-seeding baseline {"/exec.clj/child" "x"} fixtures/fixture-seed-limits))))))))

(deftest template-identities-ignore-physical-repository-and-track-all-visible-state
  (fixture
   (fn [a]
     (fixture
      (fn [b]
        (doseq [conn [a b]] (repo/write! conn "a.md" (bytes/utf8 "Same bytes")))
        (let [first-id (manifest/template-id (manifest/inventory @a))
              program (random-uuid)]
          (is (= first-id (manifest/template-id (manifest/inventory @b))))
          (is (= (manifest/comparison-id program first-id)
                 (manifest/comparison-id program first-id)))
          (fs/mkdir! b "unexpected-empty")
          (is (not= first-id (manifest/template-id (manifest/inventory @b))))
          (is (not= (manifest/comparison-id program first-id)
                    (manifest/comparison-id program (manifest/template-id (manifest/inventory @b)))))))))))

(deftest incomplete-or-invalid-metadata-is-refused-instead-of-omitted
  (fixture
   (fn [conn]
     (d/transact conn [{:geschichte.work/path "hidden.md"}])
     (is (some? (reason #(manifest/inventory @conn)))))))

(deftest exact-datahike-checkpoint-retains-empty-directories-outside-logical-git-tree
  (fixture
   (fn [conn]
     (repo/write! conn "a.md" (bytes/utf8 "A"))
     (repo/stage-all! conn)
     (repo/commit! conn {:message "Logical tree"})
     (fs/mkdir! conn "empty")
     (fs/mkdir! conn "empty/nested")
     (let [checkpoint (d/commit-id @conn)
           expected (manifest/inventory @conn)
           branch (keyword (str "case-" (random-uuid)))]
       (versioning/branch! conn checkpoint branch)
       (let [child (d/connect (assoc (:config @conn) :branch branch))]
         (try
           (is (= expected (manifest/inventory @child)))
           (is (some #{"/empty/nested"} (:directories (manifest/inventory @child))))
           (is (:clean? (repo/status child)))
           (finally (d/release child) (versioning/delete-branch! conn branch))))))))

(deftest index-acquisition-stops-at-the-policy-bound-before-entity-lookups
  (let [reads (atom 0)]
    (with-redefs [d/datoms (fn [& _] (map (fn [n] (swap! reads inc) {:e n :v (str n)}) (range)))
                  d/entity (fn [& _] (throw (AssertionError. "Bound before entity reads")))]
      (is (= :entry-limit (reason #(manifest/inventory nil))))
      ;; A chunked producer may realize at most one additional chunk.
      (is (<= (inc (:max-files manifest/policy)) @reads (+ 32 (:max-files manifest/policy)))))))
