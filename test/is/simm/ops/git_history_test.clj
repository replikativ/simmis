(ns is.simm.ops.git-history-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.git.budget :as budget]
            [geschichte.git.http :as http]
            [geschichte.git.object :as object]
            [geschichte.git.project :as project]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [is.simm.ops.benchmark-case-manifest :as manifest]
            [is.simm.ops.git-history :as history]
            [is.simm.ops.github-merge :as preparation]
            [is.simm.ops.github-merge-test :as fixtures]))

(deftest genuine-fast-forward-selects-the-imported-commit-without-a-merge
  (#'fixtures/with-edited-import
   {"notes.md" "Incoming note\n"}
   (fn [{:keys [local fetched opts base-graph]}]
     (repo/reset! local (:upstream-base opts) {:mode :hard})
     (let [before {:visible {:ref (repo/current-ref local) :refs (query/refs @local)}}
           planned (preparation/plan-policy-fetched! local fetched (assoc opts :upstream-base-git-oid (:oid base-graph)))]
       (is (= :fast-forward (:relation planned)))
       (is (= (:upstream-base opts) (:geschichte.commit/id (repo/head-commit local))))
       (is (nil? (repo/read local "notes.md")) "Planning never applies files")
       (preparation/materialize-policy! local planned before (:operation opts))
       (is (= (:upstream-head planned) (:geschichte.commit/id (repo/head-commit local))))
       (is (= "Incoming note\n" (#'fixtures/read-text local "notes.md")))
       (is (= #{"refs/heads/main"} (set (keys (query/refs @local)))))))))

(deftest locally-authored-history-still-requires-a-proposal
  (#'fixtures/with-edited-import
   {"notes.md" "Incoming note\n"}
   (fn [{:keys [local local-head fetched opts base-graph]}]
     (let [planned (preparation/plan-policy-fetched! local fetched (assoc opts :upstream-base-git-oid (:oid base-graph)))]
       (is (= :diverged (:relation planned)))
       (is (:clean? (:plan planned)))
       (is (= local-head (:geschichte.commit/id (repo/head-commit local))))
       (is (= "Local edit\nMiddle\nLast\n" (#'fixtures/read-text local "README.md")))))))

(deftest a-claimed-ancestor-oid-does-not-authorize-losing-a-different-local-tree
  (#'fixtures/with-repositories
   (fn [upstream local]
     (repo/init! upstream {:branch "refs/heads/main"})
     (let [base (#'fixtures/commit! upstream {"README.md" "Base\n"} "Base")
           base-graph (project/project upstream base)]
       (repo/init! local {:branch "refs/heads/main"})
       (http/clone! (#'fixtures/sender base-graph) local "origin"
                    "https://github.com/owner/repository.git" (#'fixtures/options))
       (let [cursor (:geschichte.commit/id (repo/head-commit local))
             authored (#'fixtures/commit! local {"README.md" "Keep my local work\n"} "Local")
             intermediate (#'fixtures/commit! upstream {"README.md" "Remote intermediate\n"} "Intermediate")
             intermediate-oid (:oid (project/project upstream intermediate))
             _ (d/transact local [{:geschichte.commit/id authored :geschichte.commit/git-oid intermediate-oid}])
             tip (#'fixtures/commit! upstream {"notes.md" "Remote final\n"} "Final")
             opts (#'fixtures/options)
             fetched (http/fetch! (#'fixtures/sender (project/project upstream tip)) local "origin"
                                  "https://github.com/owner/repository.git" opts)
             error (try (preparation/plan-policy-fetched!
                         local fetched {:ref "refs/heads/main" :preparation-id (random-uuid)
                                        :upstream-base cursor :upstream-base-git-oid (:oid base-graph)
                                        :operation (:import-budget opts)})
                        nil (catch clojure.lang.ExceptionInfo e e))]
         (is (= :local-history-mismatch (:reason (ex-data error))))
         (is (= authored (:geschichte.commit/id (repo/head-commit local))))
         (is (= "Keep my local work\n" (#'fixtures/read-text local "README.md"))))))))

(deftest file-modes-and-empty-directories-are-part-of-local-integrity
  (#'fixtures/with-edited-import
   {"notes.md" "Incoming\n"}
   (fn [{:keys [local opts base-graph]}]
     (repo/reset! local (:upstream-base opts) {:mode :hard})
     (let [db @local current (manifest/inventory db) oid (:oid base-graph) op (:operation opts)]
       (is (true? (history/assert-local-tree! db db current oid op)))
       (is (= :local-history-mismatch
              (try (history/assert-local-tree! db db (assoc-in current [:files "/README.md" :mode] 33261) oid op)
                   nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
       (is (= :unrepresented-directories
              (try (history/assert-local-tree! db db (update current :directories conj "/empty") oid op)
                   nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))))

(deftest missing-raw-commit-evidence-is-not-an-ancestry-answer
  (#'fixtures/with-edited-import
   {}
   (fn [{:keys [local opts base-graph]}]
     (is (true? (history/proven-ancestor? @local (:oid base-graph) (:oid base-graph) (:operation opts))))
     (is (= :history-unavailable
            (try (history/proven-ancestor? @local (:oid base-graph) (apply str (repeat 40 "1")) (:operation opts))
                 nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))))

(deftest nested-paths-binary-files-and-executable-modes-match-real-git-trees
  (#'fixtures/with-repositories
   (fn [upstream local]
     (repo/init! upstream {:branch "refs/heads/main"})
     (repo/write! upstream "src/deep/main.clj" (object/utf8 "(println :hello)\n"))
     (repo/write! upstream "assets/picture.bin" (byte-array [0 1 -1 42]))
     (repo/write! upstream "bin/run" (object/utf8 "#!/bin/sh\nexit 0\n") {:mode 33261})
     (repo/stage-all! upstream)
     (let [id (:geschichte.commit/id (repo/commit! upstream {:message "Nested source"}))
           graph (project/project upstream id)]
       (repo/init! local {:branch "refs/heads/main"})
       (http/clone! (#'fixtures/sender graph) local "origin"
                    "https://github.com/owner/repository.git" (#'fixtures/options))
       (let [db @local op (:import-budget (#'fixtures/options))]
         (is (true? (history/assert-local-tree! db db (manifest/inventory db) (:oid graph) op))))))))

(deftest history-and-content-checks-obey-the-original-operation-limits
  (#'fixtures/with-edited-import
   {"notes.md" "Incoming\n"}
   (fn [{:keys [local fetched opts base-graph]}]
     (repo/reset! local (:upstream-base opts) {:mode :hard})
     (let [db @local current (manifest/inventory db) oid (:oid base-graph)
           fresh (fn [metric] (budget/create {:limits (assoc (:limits (preparation/operation-policy)) metric 0)
                                              :timeout-ms 30000}))]
       (is (= :commit-visits
              (try (history/proven-ancestor? db oid oid (fresh :commit-visits)) nil
                   (catch clojure.lang.ExceptionInfo e (:metric (ex-data e))))))
       (is (= :graph-edges
              (try (history/proven-ancestor? db oid (:oid (first (filter #(= "refs/heads/main" (:ref %)) (:refs fetched))))
                                            (fresh :graph-edges)) nil
                   (catch clojure.lang.ExceptionInfo e (:metric (ex-data e))))))
       (doseq [metric [:content-visits :content-read-bytes :tree-entries]]
         (is (= metric
                (try (history/assert-local-tree! db db current oid (fresh metric)) nil
                     (catch clojure.lang.ExceptionInfo e (:metric (ex-data e)))))))))))
