(ns is.simm.ops.file-output-proposals-test
  "Fail-closed landing gates for the forthcoming retained-file review backend."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.model.system-db :as sdb]
            [is.simm.ops.proposals :as proposals]
            [is.simm.runtimes.branching :as branching]))

(defn- with-proposal [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write}
        _ (d/create-database cfg)
        conn (d/connect cfg)
        scope (random-uuid)]
    (try
      (d/transact conn sdb/schema)
      (with-redefs [sdb/get-conn (constantly conn)]
        (let [id (proposals/file-proposal!
                  {:title "Retained output review"
                   :forks [{:scope (random-uuid) :branch :ordinary :system-type :kb}
                           {:scope scope :branch :retained :system-type :kb}]})
              fork (first (filter #(= scope (:proposal.fork/scope %))
                                  (:proposal/forks (proposals/get-proposal id))))]
          ;; Inject a minimal/incomplete row deliberately: the landing gate must
          ;; refuse even before a retained manifest can be inspected.
          (d/transact conn [[:db/add (:db/id fork) :proposal.fork/system-type :file-output]])
          (f id scope)))
      (finally (d/release conn)))))

(deftest mixed-proposals-refuse-before-any-landing-or-review-dispatch
  (with-proposal
    (fn [id scope]
      (let [effects (atom [])]
        (with-redefs-fn {#'branching/land! (fn [& _] (swap! effects conj :land))
                         #'is.simm.ops.proposals/review-decision
                         (fn [_] (swap! effects conj :review-dispatch) nil)}
          #(doseq [force? [false true]]
             (is (thrown? clojure.lang.ExceptionInfo
                          (proposals/accept-proposal! id :force? force?)))
             (is (thrown? clojure.lang.ExceptionInfo
                          (proposals/accept-fork! id scope :retained :force? force?)))))
        (is (empty? @effects))
        (is (= :open (:proposal/status (proposals/get-proposal id))))))))

(deftest direct-backend-landing-refuses-without-resolving-a-trunk
  (let [calls (atom 0)]
    (with-redefs [branching/trunk-of (fn [& _] (swap! calls inc) :db)]
      (is (= :settlement-unavailable
             (try (branching/land! (random-uuid) :file-output :retained) nil
                  (catch clojure.lang.ExceptionInfo e (:error (ex-data e)))))))
    (is (zero? @calls))))

(deftest retained-output-never-falls-through-to-a-kb-backend
  (let [scope (random-uuid)]
    (with-redefs [branching/kb-diff (fn [& _] (throw (AssertionError. "Unexpected KB diff")))
                  branching/kb-conflicts (fn [& _] (throw (AssertionError. "Unexpected KB conflicts")))]
      (is (= (str scope) (branching/head-id scope :file-output :retained)))
      (is (= "target-not-checked" (branching/head-id scope :file-output :file-output/unverified-target)))
      (is (thrown? clojure.lang.ExceptionInfo (branching/delta scope :file-output :retained)))
      (is (thrown? clojure.lang.ExceptionInfo (branching/conflicts-with-trunk scope :file-output :retained)))
      (is (thrown? clojure.lang.ExceptionInfo (branching/fork-branch! scope :file-output "new"))))))

(deftest selecting-an-ordinary-component-cannot-bypass-a-whole-review-gate
  (with-proposal
    (fn [id _]
      (d/transact (sdb/get-conn) [{:db/id [:proposal/id id] :proposal/review (random-uuid)}])
      (let [ordinary (first (filter #(= :kb (:proposal.fork/system-type %))
                                    (:proposal/forks (proposals/get-proposal id))))
            effects (atom [])]
        (with-redefs-fn {#'is.simm.ops.proposals/review-decision
                         (fn [_] (swap! effects conj :review-dispatch)
                           (fn [& _] (swap! effects conj :whole-review-accept)))}
          #(doseq [force? [false true]]
             (is (thrown? clojure.lang.ExceptionInfo
                          (proposals/accept-fork! id (:proposal.fork/scope ordinary) :ordinary
                                                  :force? force?)))))
        (is (empty? @effects))))))

(deftest dismissal-records-resolution-without-deleting-a-branch
  (with-proposal
    (fn [id scope]
      (with-redefs [branching/discard-kb-branch!
                    (fn [& _] (throw (AssertionError. "Retained evidence is not a KB branch")))
                    branching/discard-repo-branch!
                    (fn [& _] (throw (AssertionError. "Retained evidence is not a Git branch")))]
        (is (= :dismissed (:status (proposals/dismiss-fork! id scope :retained))))
        (is (= :open (:proposal/status (proposals/get-proposal id))))))))
