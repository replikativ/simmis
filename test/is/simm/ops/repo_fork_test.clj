(ns is.simm.ops.repo-fork-test
  "The code-fork lane: a geschichte workspace as the place an agent writes, and
   a named branch as the thing a reviewer decides.

   These pin the TRAPS, not the happy path. Every one of them was found by
   measurement during the build, and each is a silent failure — a wrong call
   here does not throw, it returns something plausible and does nothing. The
   happy path is easy to keep working; what needs a test is the shape of the
   mistake next to it.

   Deliberately at the geschichte/yggdrasil layer rather than through
   `branching/get-repo-system`, which resolves through dvergr's registry and a
   live room context. The invariants below belong to the substrate, so pinning
   them here keeps them fast and keeps them honest: a test that had to
   provision a room could pass because the resolution worked while the
   mechanics were wrong."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [geschichte.bytes :as gbytes]
            [geschichte.query :as gq]
            [geschichte.repo :as grepo]
            [geschichte.workspace :as gws]
            [yggdrasil.adapters.geschichte :as gy]
            [yggdrasil.protocols :as yp]))

(defn- fresh-repo
  "An initialized geschichte repository with one commit, in memory.

   `:commit-graph?` because branch ancestry is read from it and every merge
   below needs a graph to merge over. `:keep-history? false` matches how dvergr
   configures a real room repo — geschichte's commit graph IS the history, so
   datahike's temporal index is redundant there."
  []
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write
             :keep-history? false
             :commit-graph? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (grepo/init! conn {:name "test-repo"})
      (grepo/write! conn "a.txt" (gbytes/utf8 "base\n"))
      (grepo/stage-all! conn)
      (grepo/commit! conn {:message "base" :author "test"})
      conn)))

(defn- system [conn] (gy/create conn {:system-name "room-repo-test"}))

(defn- commit-in! [conn path content msg]
  (grepo/write! conn path (gbytes/utf8 content))
  (grepo/stage-all! conn)
  (grepo/commit! conn {:message msg :author "test"}))

(defn- paths [conn] (set (keys (gq/worktree @conn))))

(defn- ws-conn [overlay] (:conn @(:local-writes overlay)))

;; ---------------------------------------------------------------------------

(deftest a-workspace-isolates-writes-and-is-reclaimed
  (let [conn (fresh-repo)
        sys (system conn)
        ov (yp/overlay sys {})]
    (commit-in! (ws-conn ov) "b.txt" "in the workspace\n" "workspace commit")

    (testing "the canonical worktree does not see the workspace's work"
      (is (contains? (paths (ws-conn ov)) "b.txt"))
      (is (not (contains? (paths conn) "b.txt"))
          "a workspace that leaked into trunk would defeat the whole fork"))

    (testing "discard reclaims the physical branch"
      (is (= 1 (count (gws/list conn))))
      (yp/discard! ov)
      (is (zero? (count (gws/list conn)))
          "nothing else reclaims it — `discard-repo-branch!` deletes a REF, and
           a workspace branch is not a ref"))))

(deftest a-workspace-owns-its-ref-namespace
  ;; The property that makes an overlay stronger than a git worktree, and the
  ;; one that breaks every naive assumption built on top of it. The traps below
  ;; are consequences of exactly this.
  (let [conn (fresh-repo)
        sys (system conn)
        _ (yp/branch! sys :before-fork :main)
        ov (yp/overlay sys {})]
    (testing "refs existing at fork time are COPIED in, not shared"
      (is (contains? (set (keys (grepo/refs (ws-conn ov)))) "refs/heads/before-fork")))

    (testing "but a branch created canonically AFTER the fork is invisible"
      (yp/branch! sys :after-fork :main)
      (is (contains? (yp/branches sys) :after-fork))
      (is (not (contains? (set (keys (grepo/refs (ws-conn ov)))) "refs/heads/after-fork"))
          "the two ref namespaces diverge from the moment of the fork"))
    (yp/discard! ov)))

(deftest publish-by-ref-name-publishes-the-WRONG-commit
  ;; TRAP 1, and the nastiest of the three because it SUCCEEDS.
  ;;
  ;; A workspace commits on its CURRENT ref, which is whatever was checked out
  ;; at fork time — `refs/heads/main`. Its copy of the fork's branch ref
  ;; therefore never moves and still points at the fork POINT. So `publish!`
  ;; without an explicit `:commit` resolves the tip from that stale ref and
  ;; cheerfully publishes the base commit: no error, no warning, and a proposal
  ;; whose branch contains none of the agent's work. The reviewer sees an empty
  ;; fork and has no way to tell it apart from an agent that did nothing.
  (let [conn (fresh-repo)
        sys (system conn)
        _ (yp/branch! sys :fork-a :main)
        base (yp/snapshot-id sys)
        ov (yp/overlay sys {})]
    (commit-in! (ws-conn ov) "c.txt" "fork work\n" "fork commit")
    (let [tip (str (:geschichte.commit/id (grepo/head-commit (ws-conn ov))))]

      (testing "the workspace's copy of the branch ref is stale by construction"
        (is (= base (str (get (grepo/refs (ws-conn ov)) "refs/heads/fork-a"))))
        (is (not= base tip) "the work is on the workspace's main, not on fork-a"))

      (testing "publishing by name SUCCEEDS and lands the fork point"
        (gws/publish! conn (ws-conn ov) {:ref "refs/heads/fork-a" :create? true})
        (is (= base (some-> (yp/snapshot-meta sys :fork-a)
                            :geschichte.commit/id str))
            "no exception, and none of the agent's work — this is why
             `publish-repo-overlay!` passes `:commit` explicitly"))

      (testing "with the tip passed explicitly, the work actually lands"
        (gws/publish! conn (ws-conn ov)
                      {:ref "refs/heads/fork-a" :commit (parse-uuid tip)})
        (is (= tip (some-> (yp/snapshot-meta sys :fork-a)
                           :geschichte.commit/id str))
            "and the published branch is an ORDINARY branch, which is the whole
             point: the review lane needs no knowledge of workspaces"))

      (testing "trunk has not moved either way"
        (is (not (contains? (paths conn) "c.txt"))
            "publishing to a named branch must land nothing on trunk — that is
             what a proposal exists to prevent")))
    (yp/discard! ov)))

(deftest merge-from-parent-is-a-silent-no-op-on-a-workspace
  ;; TRAP 2, and the reason this file exists. `ygg/merge-from-parent!` reports
  ;; SUCCESS and changes nothing: it merges the parent's branch BY NAME, and
  ;; inside a workspace that name resolves to the workspace's own ref. simmis
  ;; shipped a refresh verb built on it that did nothing while saying it worked;
  ;; it was caught only by asserting on CONTENT rather than on the return value.
  ;;
  ;; If this test ever FAILS, upstream has fixed it (simmis task #67) and
  ;; `refresh-repo-fork!` can go back to the protocol call.
  (let [conn (fresh-repo)
        sys (system conn)
        ov (yp/overlay sys {})]
    (commit-in! (ws-conn ov) "shared.txt" "FORK\n" "fork edits shared")
    (commit-in! conn "shared.txt" "TRUNK\n" "trunk edits shared")

    (testing "the workspace does not see trunk's version"
      (is (= "FORK\n" (String. (grepo/read (ws-conn ov) "shared.txt")))))

    (testing "advance! REFUSES a diverged workspace rather than pretending"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"fast-forward"
           (gws/advance! conn (ws-conn ov)))
          "the honest primitive — a refusal a caller can report"))
    (yp/discard! ov)))

(deftest advance-fast-forwards-a-clean-workspace
  ;; The other half of update-from-trunk: when the fork has NOT diverged,
  ;; trunk's newer commits must actually arrive. A refresh that silently
  ;; declined every time would be indistinguishable from the trap above.
  (let [conn (fresh-repo)
        sys (system conn)
        ov (yp/overlay sys {})]
    (commit-in! conn "trunk-only.txt" "added after the fork\n" "trunk moves on")
    (is (not (contains? (paths (ws-conn ov)) "trunk-only.txt")))

    (gws/advance! conn (ws-conn ov))
    (is (contains? (paths (ws-conn ov)) "trunk-only.txt")
        "the fork now sits on trunk's current state")
    (yp/discard! ov)))

(deftest advance-refuses-a-dirty-workspace-without-losing-work
  (let [conn (fresh-repo)
        sys (system conn)
        ov (yp/overlay sys {})]
    (commit-in! conn "trunk-only.txt" "trunk\n" "trunk moves on")
    ;; staged but never committed
    (grepo/write! (ws-conn ov) "wip.txt" (gbytes/utf8 "uncommitted\n"))
    (grepo/stage-all! (ws-conn ov))

    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"dirty"
                          (gws/advance! conn (ws-conn ov))))
    (is (contains? (paths (ws-conn ov)) "wip.txt")
        "a refusal must not cost the agent its uncommitted work")
    (yp/discard! ov)))

(deftest conflicts-are-real-and-merge-refuses-atomically
  ;; Unlike the datahike adapter's `[]`, a repo reports genuine conflicts — so
  ;; a KB fork's silence means \"not computed\" while a repo fork's means
  ;; \"clean\". The proposals layer relies on that difference.
  (let [conn (fresh-repo)
        sys (system conn)
        _ (yp/branch! sys :left :main)
        _ (yp/branch! sys :right :main)]
    (yp/checkout sys :left)
    (commit-in! conn "shared.txt" "LEFT\n" "left")
    (yp/checkout sys :right)
    (commit-in! conn "shared.txt" "RIGHT\n" "right")
    (yp/checkout sys :main)

    (testing "each branch merges into TRUNK cleanly — they clash only with each other"
      (is (empty? (yp/conflicts sys :main :left)))
      (is (empty? (yp/conflicts sys :main :right)))
      "this is why the ForkSet warnings gate cannot catch a mutual conflict
       up front, and why accept-proposal! records each fork as it lands")

    (yp/merge! sys :left)
    (is (= "LEFT\n" (String. (grepo/read conn "shared.txt"))))

    (testing "the second now conflicts, and refuses atomically"
      (let [cs (yp/conflicts sys :main :right)
            head-before (yp/snapshot-id sys)]
        (is (seq cs))
        (is (= "shared.txt" (:path (first cs))))
        (is (thrown? clojure.lang.ExceptionInfo (yp/merge! sys :right)))
        (is (= head-before (yp/snapshot-id sys))
            "a refused merge must leave the repository exactly as it was")))))

(deftest a-merged-branch-still-needs-force-to-delete
  ;; Landing a fork deletes its branch. geschichte refuses that delete even
  ;; though the branch IS fully merged — its tip is a parent of trunk's tip —
  ;; so `discard-repo-branch!` passes `:force?`. Without it every landed code
  ;; fork leaks its branch forever.
  (let [conn (fresh-repo)
        sys (system conn)
        _ (yp/branch! sys :feature :main)]
    (yp/checkout sys :feature)
    (commit-in! conn "f.txt" "feature\n" "feature work")
    (let [tip (yp/snapshot-id sys)]
      (yp/checkout sys :main)
      (yp/merge! sys :feature)

      (is (contains? (yp/parent-ids sys) tip)
          "trunk's tip carries the fork tip as a parent — it is merged by any
           reading of the word")
      (is (thrown? clojure.lang.ExceptionInfo (yp/delete-branch! sys :feature))
          "and geschichte still refuses (upstream defect)")
      (yp/delete-branch! sys :feature {:force? true})
      (is (not (contains? (yp/branches sys) :feature))))))

(deftest parent-ids-reports-the-tip-s-real-parents
  ;; Both the repository API and the substrate must expose the tip's real
  ;; parents. Geschichte now includes these in head-commit's pull pattern.
  (let [conn (fresh-repo)
        sys (system conn)]
    (commit-in! conn "b.txt" "second\n" "second commit")
    (testing "the repository and substrate agree on ancestry"
      (let [parents (set (map :geschichte.commit/id
                              (:geschichte.commit/parents (grepo/head-commit conn))))]
        (is (seq parents))
        (is (= (set (map str parents)) (set (yp/parent-ids sys))))))))
