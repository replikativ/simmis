(ns is.simm.ops.checks
  "Do a code fork's tests pass?

   A proposal card shows what CHANGED and says nothing about whether it WORKS.
   For an agent-authored change that gap matters more than for a human-authored
   one, not less: reading a diff tells a reviewer what was intended, and the
   thing they actually want to know is whether it runs. A card that says
   \"37 tests pass on this branch\" turns review from reading and hoping into
   trusting a gate.

   Runs INDEPENDENTLY of the agent, which is the whole point. An agent could
   report its own test results, but a self-reported check is not a check — it
   is a claim, and the reviewer already has the agent's claim in the rationale.

   Isolation comes free from the fork machinery: a check opens a workspace on
   the branch (the same primitive an agent writes through), loads the fork's own
   test namespaces into a FRESH sandbox through that workspace's filesystem, and
   discards it. Nothing on the daemon's classpath is visible, and nothing the
   check does can reach trunk."
  (:require [clojure.string :as str]
            [is.simm.runtimes.branching :as branching]
            [muschel.fs :as mfs]
            [taoensso.telemere :as log]))

(def ^:private max-output
  "Ceiling on the report a card carries. A failing suite can print a lot, and a
   reviewer needs the first failures, not all of them."
  8000)

(defn- interesting-output
  "The part of a clojure.test report worth putting on a card.

   `run-all-tests` prints a `Testing <ns>` line for every namespace in the
   session — a dozen lines of nothing, and on a small screen they fill the box
   and push the actual failure out of sight. A reviewer opening a failed check
   wants the assertion, so drop the preamble and keep from the first FAIL or
   ERROR onward. If neither appears, the output was the preamble."
  [s]
  (let [lines (str/split-lines (or s ""))
        from (->> (map-indexed vector lines)
                  (some (fn [[i l]] (when (re-find #"^(FAIL|ERROR) in " l) i))))]
    (->> (if from (drop from lines) lines)
         (remove #(re-matches #"\s*Testing \S+\s*" %))
         (str/join "\n")
         str/trim)))

(defn- test-namespaces
  "Namespaces to load from a fork's filesystem: every `*_test.clj` under the
   root or `src/`, as namespace symbols.

   The same two source roots the sandbox's own `load-fn` searches — anything it
   cannot `require` is not worth enumerating, and a path this finds but that
   cannot resolve would report as an error the fork did not cause."
  [fs]
  (letfn [(walk [dir depth]
            (when (and (< depth 12) (= :dir (:type (mfs/stat fs dir))))
              (mapcat (fn [{:keys [name]}]
                        (let [p (str (str/replace dir #"/$" "") "/" name)]
                          (if (= :dir (:type (mfs/stat fs p)))
                            (walk p (inc depth))
                            (when (str/ends-with? name "_test.clj") [p]))))
                      (mfs/list-dir fs dir))))]
    (->> (concat (walk "/" 0) (walk "/src" 0))
         distinct
         (keep (fn [p]
                 ;; path → namespace: drop the source root, drop .clj,
                 ;; underscores back to hyphens, slashes to dots
                 (let [rel (-> p
                               (str/replace #"^/(src/)?" "")
                               (str/replace #"\.clj$" ""))]
                   (when-not (str/blank? rel)
                     (symbol (-> rel
                                 (str/replace "/" ".")
                                 (str/replace "_" "-")))))))
         distinct
         vec)))

(defn test-namespaces-in
  "Public form of `test-namespaces` — the test namespaces on a muschel fs."
  [fs]
  (test-namespaces fs))

(defn load-test-namespaces!
  "`require :reload` every test namespace on `fs` into the SCI context `eval-1`
   evaluates in. Returns `[loaded failed]`.

   Shared by the independent check and by the agent's own `run_tests`, so that
   \"green while I was working\" and \"green at review\" mean the same thing.
   Without it they do not: clojure.test enumerates the vars a session has
   LOADED, so an agent that wrote a test file and never required it runs zero
   tests and is told everything passed."
  [eval-1 fs]
  (let [nss (test-namespaces fs)
        results (mapv (fn [n]
                        (let [r (eval-1 (str "(require '" n " :reload)"))]
                          {:ns n :ok? (boolean (:success r))
                           :error (get-in r [:error :message])}))
                      nss)]
    [(filterv :ok? results) (filterv (complement :ok?) results)]))

(defn- run-in-sandbox
  "Load every test namespace on `fs` into a FRESH sandbox resolving only
   through it, then run clojure.test. Returns the raw `{:success :value
   :stdout}` of the run.

   Vanilla interpreter state on every call, and that is the point rather than
   an implementation detail. A result that depends on what a session happened to
   have loaded is not a property of the branch — it is a property of the
   session, and two runs of \"the tests on this branch\" would legitimately
   disagree. A fresh context makes the answer reproducible and makes the agent's
   run and the reviewer's run the same run."
  [fs]
  (let [create (requiring-resolve 'dvergr.sandbox/create-base-ctx)
        eval-code (requiring-resolve 'dvergr.sandbox/eval-code)
        roots-var (requiring-resolve 'dvergr.sandbox.workspace/*workspace-roots*)
        ctx (create)]
    ;; The fork's workspace is the ONLY root — a check must not silently pass
    ;; because a namespace resolved from the daemon's shared workspace instead
    ;; of the branch under review.
    (with-bindings {roots-var [{:id :fork-check :root "/" :fs fs}]}
      (let [nss (test-namespaces fs)
            [_ failed-loads] (load-test-namespaces! #(eval-code ctx %) fs)]
        (if (and (seq nss) (= (count failed-loads) (count nss)))
          ;; every namespace failed to load — running the suite would report a
          ;; vacuous pass over zero tests, which is worse than saying so
          {:load-failed failed-loads}
          (assoc (eval-code ctx "(clojure.test/run-all-tests)")
                 :load-failures failed-loads))))))

(defn run-fork-checks!
  "Run a code fork's tests on its branch. Returns

     {:status :pass|:fail|:error|:none :tests n :passed n :failed n :errors n
      :output \"…\" :head \"…\"}

   `:none` means the branch carries no test namespaces — reported as its own
   state rather than as a pass, because \"nothing to run\" and \"everything ran
   and was fine\" are different facts and only one of them is reassuring.

   Only meaningful for `:repo` forks; anything else returns nil rather than
   pretending a datom branch has a test suite."
  [scope system-type branch]
  (when (= :repo system-type)
    (let [head (branching/head-id scope :repo branch)
          opened (try
                   {:fork (branching/open-repo-fork! scope branch)}
                   (catch Exception e
                     ;; An incomplete construction may retain its sole cleanup
                     ;; owner in this exception. Never turn that into UI text.
                     (if (and (= :sci.fork/affine-resource (:type (ex-data e)))
                              (every? #{:type :resource} (keys (ex-data e)))
                              (nil? (ex-cause e))
                              (empty? (.getSuppressed e)))
                       {:error e}
                       (throw e))))]
      ;; Construction failure must not turn an already-filed proposal into a
      ;; reported filing failure. Cleanup still belongs to its original owner.
      (if-let [error (:error opened)]
        (do
          (log/log! {:level :warn :id ::check-construction-failed
                     :data {:scope (str scope) :branch (str branch)
                            :error (ex-message error)}})
          {:status :error :head head :output (ex-message error)})
        (let [fork (:fork opened)]
          (try
            (if-not fork
              {:status :error :output "the repository did not resolve" :head head}
              (let [{:keys [filesystem]} (branching/repo-fork-workspace fork)
                    nss (test-namespaces filesystem)]
                (if (empty? nss)
                  {:status :none :head head
                   :output "No test namespaces on this branch."}
                  (let [r (run-in-sandbox filesystem)]
                    (cond
                      (:load-failed r)
                      {:status :error :head head
                       :output (str "Could not load any test namespace:\n"
                                    (str/join "\n" (map #(str "  " (:ns %) " — " (:error %))
                                                        (:load-failed r))))}

                      (not (:success r))
                      {:status :error :head head
                       :output (str "The test run itself failed: "
                                    (get-in r [:error :message]))}

                      :else
                      (let [{:keys [test pass fail error]
                             :or {test 0 pass 0 fail 0 error 0}} (:value r)
                            broken? (pos? (+ fail error))
                            out (str (when (seq (:load-failures r))
                                       (str "Some namespaces did not load:\n"
                                            (str/join "\n" (map #(str "  " (:ns %) " — " (:error %))
                                                                (:load-failures r)))
                                            "\n\n"))
                                     (interesting-output (:stdout r)))]
                        {:status (if broken? :fail :pass)
                         :tests test :passed pass :failed fail :errors error
                         :head head
                         :output (subs (or out "") 0 (min max-output (count (or out ""))))}))))))
            (catch Exception e
              (log/log! {:level :warn :id ::check-failed
                         :data {:scope (str scope) :branch (str branch)
                                :error (.getMessage e)}})
              {:status :error :head head :output (.getMessage e)})
            (finally (branching/close-repo-fork! fork))))))))

(defonce ^:private cache
  ;; {[scope branch head] → result}. Keyed on the branch HEAD, so a fork that
  ;; advances re-runs rather than showing a reviewer a pass earned by code that
  ;; is no longer there — the same rule the diff and review caches follow.
  (atom {}))

(defn fork-checks
  "Cached `run-fork-checks!`. Runs once per branch head."
  [scope system-type branch]
  (when (= :repo system-type)
    (let [head (branching/head-id scope :repo branch)
          k [scope (str branch) head]]
      (if (contains? @cache k)
        (get @cache k)
        (let [r (run-fork-checks! scope system-type branch)]
          ;; Infrastructure failures are retryable without a branch edit.
          (when-not (= :error (:status r)) (swap! cache assoc k r))
          r)))))

(defn run-tests-on-fs
  "Run the tests on a muschel filesystem, in a vanilla sandbox. The ONE
   implementation — the review-time check and the agent's own `run_tests` both
   land here, so \"the tests pass\" means the same thing in both mouths.

   Same result shape as `run-fork-checks!` minus the commit id, since a caller
   holding a filesystem may not have a branch."
  [fs]
  (let [nss (test-namespaces fs)]
    (if (empty? nss)
      {:status :none :output "No test namespaces on this branch."}
      (let [r (run-in-sandbox fs)]
        (cond
          (:load-failed r)
          {:status :error
           :output (str "Could not load any test namespace:\n"
                        (str/join "\n" (map #(str "  " (:ns %) " — " (:error %))
                                            (:load-failed r))))}
          (not (:success r))
          {:status :error
           :output (str "The test run itself failed: " (get-in r [:error :message]))}
          :else
          (let [{:keys [test pass fail error] :or {test 0 pass 0 fail 0 error 0}} (:value r)
                broken? (pos? (+ fail error))
                out (str (when (seq (:load-failures r))
                           (str "Some namespaces did not load:\n"
                                (str/join "\n" (map #(str "  " (:ns %) " — " (:error %))
                                                    (:load-failures r)))
                                "\n\n"))
                         (interesting-output (:stdout r)))]
            {:status (if broken? :fail :pass)
             :tests test :passed pass :failed fail :errors error
             :output (subs (or out "") 0 (min max-output (count (or out ""))))}))))))
