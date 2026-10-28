(ns is.simm.ops.checks-test
  "Enumerating a branch's tests, and reading a clojure.test report.

   The two halves that are pure enough to pin without a repository. The rest of
   `run-fork-checks!` is fork lifecycle, covered by `repo-fork-test`.

   Why the enumeration matters on its own: it is the ONE definition of \"the
   tests on this branch\", shared by the independent check and by the agent's
   own `run_tests`. Before it they disagreed — clojure.test enumerates the vars
   a session has LOADED, so an agent that wrote a test file and never required
   it ran zero tests and was told everything passed, while the check at review
   time read the file and found the failure. Green while working, red in front
   of the reviewer, which is the worst direction for the two to differ in."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [is.simm.ops.checks :as checks]
            [is.simm.runtimes.branching :as branching]
            [muschel.fs :as mfs]))

(defn- fake-fs
  "A muschel-shaped stand-in over a `{path → :dir|:file}` map. Only `stat` and
   `list-dir` are exercised by the enumeration, so a full filesystem would add
   setup without adding coverage."
  [entries]
  ;; PARTIAL on purpose — `muschel.fs/FS` is a wide protocol and the
  ;; enumeration touches exactly two methods. Anything else calling in would
  ;; throw rather than quietly get a default, which is what a stand-in should do.
  (reify mfs/FS
    (-stat [_ p] (when-let [t (get entries (str/replace (str p) #"/$" ""))]
                   {:type t}))
    (-list-dir [_ p]
      (let [d (str/replace (str p) #"/$" "")
            prefix (if (= d "") "/" (str d "/"))]
        (->> (keys entries)
             (keep (fn [k]
                     (when (and (str/starts-with? k prefix)
                                (not (str/includes? (subs k (count prefix)) "/")))
                       {:name (subs k (count prefix))})))
             vec)))))

(deftest finds-test-files-under-both-source-roots
  (let [fs (fake-fs {"" :dir "/src" :dir "/src/app" :dir
                     "/src/app/core.clj" :file
                     "/src/app/core_test.clj" :file
                     "/top_test.clj" :file
                     "/README.md" :file})
        nss (set (checks/test-namespaces-in fs))]
    (testing "both the root and src/, the two the sandbox load-fn searches"
      (is (contains? nss 'app.core-test))
      (is (contains? nss 'top-test)))
    (testing "and nothing that is not a test"
      (is (not (contains? nss 'app.core)))
      (is (= 2 (count nss))))))

(deftest path-to-namespace-follows-the-clojure-convention
  (let [fs (fake-fs {"" :dir "/src" :dir "/src/my_app" :dir
                     "/src/my_app/deep_thing_test.clj" :file})]
    (is (= '[my-app.deep-thing-test] (checks/test-namespaces-in fs))
        "underscores become hyphens and slashes become dots — get this wrong
         and every require fails, which would report as the fork's error")))

(deftest a-branch-with-no-tests-enumerates-nothing
  (let [fs (fake-fs {"" :dir "/src" :dir "/src/only.clj" :file})]
    (is (empty? (checks/test-namespaces-in fs))
        "which run-fork-checks! reports as :none — deliberately NOT a pass,
         since 'nothing to run' and 'everything ran and was fine' are
         different facts and only one is reassuring")))

(deftest the-report-leads-with-the-failure
  (let [raw (str "\nTesting user\n\nTesting calc\n\nTesting billing.webhook\n\n"
                 "FAIL in (adds) (calc_test.clj:2)\n"
                 "expected: (= 4 (calc/add 1 2))\n"
                 "  actual: (not (= 4 3))\n\n"
                 "Ran 4 tests containing 6 assertions.\n1 failures, 0 errors.")
        out (#'checks/interesting-output raw)]
    (testing "the Testing-<ns> preamble is gone"
      (is (not (str/includes? out "Testing user")))
      (is (str/starts-with? out "FAIL in (adds)")))
    (testing "and the assertion and totals survive"
      (is (str/includes? out "expected: (= 4 (calc/add 1 2))"))
      (is (str/includes? out "1 failures, 0 errors.")))))

(deftest a-passing-report-keeps-its-totals
  (let [out (#'checks/interesting-output
             "\nTesting user\n\nTesting calc\n\nRan 4 tests containing 6 assertions.\n0 failures, 0 errors.")]
    (is (= "Ran 4 tests containing 6 assertions.\n0 failures, 0 errors." out)
        "with no FAIL to lead with, the preamble is still dropped — a card
         showing a dozen 'Testing …' lines for a green run is noise")))


(deftest interpreter-copy-failure-is-a-check-error-not-a-filing-error
  (let [closed (atom [])]
    (with-redefs [branching/head-id (fn [& _] "checked-head")
                  branching/open-repo-fork! (fn [& _] (throw (ex-info "Cannot fork snapshot" {:type :sci.fork/affine-resource})))
                  branching/close-repo-fork! #(swap! closed conj %)]
      (is (= {:status :error :head "checked-head" :output "Cannot fork snapshot"}
             (checks/run-fork-checks! (random-uuid) :repo :main)))
      (is (empty? @closed) "No capability was returned to settle"))))

(deftest infrastructure-check-error-can-be-retried-without-editing-the-branch
  (let [calls (atom 0) scope (random-uuid)]
    (with-redefs [branching/head-id (fn [& _] "unchanged-head")
                  checks/run-fork-checks! (fn [& _] (if (= 1 (swap! calls inc))
                                                     {:status :error :head "unchanged-head"}
                                                     {:status :pass :head "unchanged-head"}))]
      (is (= :error (:status (checks/fork-checks scope :repo :main))))
      (is (= :pass (:status (checks/fork-checks scope :repo :main))))
      (is (= :pass (:status (checks/fork-checks scope :repo :main))))
      (is (= 2 @calls) "Only successful branch evidence is cached"))))


(deftest failed-construction-keeps-its-recovery-owner-exception
  (let [owner (Object.)
        error (ex-info "Cleanup remains owned" {:type :sci.fork/affine-resource :construction/owner owner})
        attempts (atom 0) closed (atom 0)]
    (with-redefs [branching/head-id (fn [& _] "owned-head")
                  branching/open-repo-fork! (fn [& _] (swap! attempts inc) (throw error))
                  branching/close-repo-fork! (fn [_] (swap! closed inc))]
      (is (identical? error (try (checks/fork-checks (random-uuid) :repo :main)
                                 (catch Exception e e))))
      (is (= 1 @attempts))
      (is (zero? @closed)))))

(deftest suppressed-cleanup-error-is-never-converted-to-a-check-result
  (let [error (ex-info "Refused fork" {:type :sci.fork/affine-resource})]
    (.addSuppressed error (ex-info "Cleanup failed" {}))
    (with-redefs [branching/head-id (fn [& _] "owned-head")
                  branching/open-repo-fork! (fn [& _] (throw error))]
      (is (identical? error (try (checks/run-fork-checks! (random-uuid) :repo :main)
                                 (catch Exception e e)))))))
