(ns is.simm.ops.knowledge-inputs-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.content :as content]
            [geschichte.repo :as repo]
            [is.simm.documents.markdown :as markdown]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.workflow-runs :as workflow-runs]
            [is.simm.ops.workflow-runs-test :as workflow-fixture]))

(defn- with-repository [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? true :branch-history? true
             :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (repo/init! conn {:name "Knowledge workflow input tests"}) (f conn)
           (finally (d/release conn))))))

(defn- write! [conn path text] (repo/write! conn path (markdown/source-bytes text)))

(deftest frozen-inputs-are-directly-reusable-by-ordinary-workflow-preparation
  (with-repository
    (fn [conn]
      (write! conn "inbox/meeting.md" "# Meeting\nDecision: ship.")
      (write! conn "wiki/company.md" "# Company\nExisting memory.")
      (write! conn "out/earlier.md" "Not incoming evidence")
      (write! conn "workflows/example/README.md" "Task data, not source evidence")
      (let [db @conn before (:max-tx db)
            frozen (inputs/capture db inputs/default-policy)
            entries (:inputs frozen)
            manifest (edn/read-string (get entries "/in/knowledge/manifest.edn"))
            selection (assoc (select-keys (workflow-fixture/input) workflow-runs/source-fields)
                             :inputs entries)
            prepared (workflow-runs/prepare-source (workflow-fixture/snapshot workflow-fixture/source) selection)]
        (is (= before (:max-tx @conn)))
        (is (= #{:trigger :context} (set (map :role (:files manifest)))))
        (is (= 3 (count entries)))
        (is (= "# Meeting\nDecision: ship." (get entries "/in/knowledge/sources/inbox/meeting.md")))
        (is (= entries (select-keys (:inputs prepared) (keys entries))))
        (write! conn "inbox/meeting.md" "Later source edit")
        (is (= frozen (inputs/capture db inputs/default-policy)) "Old immutable snapshot captures the old bytes")
        (is (not= (:trigger-id frozen) (:trigger-id (inputs/capture @conn inputs/default-policy))))
        (is (= "# Meeting\nDecision: ship."
               (get-in prepared [:inputs "/in/knowledge/sources/inbox/meeting.md"])))))))

(deftest knowledge-output-and-staging-edits-do-not-retrigger-maintenance
  (with-repository
    (fn [conn]
      (write! conn "transcripts/call.md" "Evidence")
      (write! conn "wiki/a.md" "Prior")
      (let [initial (inputs/capture @conn inputs/default-policy)]
        (is (false? (:empty? initial)))
        (write! conn "wiki/a.md" "Adopted output")
        (let [next (inputs/capture @conn inputs/default-policy)]
          (is (= (:trigger-id initial) (:trigger-id next)))
          (is (not= (:execution-id initial) (:execution-id next))))
        (let [before (inputs/capture @conn inputs/default-policy)]
          (write! conn "out/wiki/a.md" "Proposal output")
          (write! conn "in/knowledge/sources/example.md" "Staging")
          (is (= before (inputs/capture @conn inputs/default-policy)))))))
  (with-repository #(is (:empty? (inputs/capture @% inputs/default-policy)))))

(deftest exact-exclusions-and-normalized-policy-are-deterministic
  (with-repository
    (fn [conn]
      (write! conn "inbox/private/a.md" "Excluded")
      (write! conn "inbox/private-extra/b.md" "Included")
      (let [policy (assoc inputs/default-policy :exclude-prefixes ["inbox/private"])
            captured (inputs/capture @conn policy)]
        (is (= ["inbox/private-extra/b.md"] (mapv :path (get-in captured [:manifest :files]))))
        (is (= captured (inputs/capture @conn (update policy :trigger-roots #(vec (concat (reverse %) %))))))))))

(deftest unchanged-decision-needs-no-source-bodies-or-allowance
  (with-repository
    (fn [conn]
      (write! conn "inbox/a.md" "Evidence")
      (write! conn "wiki/a.md" "Supporting context")
      (let [prior (:trigger-id (inputs/selection @conn inputs/default-policy))]
        (write! conn "wiki/a.md" "Adopted change")
        (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "No body reads for skip decision")))]
          (is (= prior (:trigger-id (inputs/selection @conn inputs/default-policy)))))))))

(deftest unsafe-role-and-staging-policies-refuse
  (doseq [policy [(assoc inputs/default-policy :trigger-roots [""])
                  (assoc inputs/default-policy :trigger-roots ["../private"])
                  (assoc inputs/default-policy :context-roots ["inbox/nested"])
                  (assoc inputs/default-policy :trigger-roots ["out"])
                  (assoc inputs/default-policy :context-roots ["workflows/custom"])
                  (assoc inputs/default-policy :eval "forged")]]
    (is (thrown? clojure.lang.ExceptionInfo (inputs/normalize-policy policy)))))

(deftest admission-resource-limits-refuse-before-reading-source-bodies
  (with-repository
    (fn [conn]
      (dotimes [n 100] (write! conn (str "inbox/" n ".md") "Evidence"))
      (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Must refuse before body reads")))]
        (is (= :too-many-input-files
               (try (inputs/capture @conn inputs/default-policy)
                    (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))))

(deftest forged-size-and-expanded-path-refuse-before-blob-allocation
  (with-repository
    (fn [conn]
      (write! conn "inbox/a.md" "Evidence")
      (d/transact conn [{:geschichte.work/path "inbox/a.md" :geschichte.work/size 1}])
      (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Must not read mismatched content")))]
        (is (= :content-size-mismatch
               (try (inputs/capture @conn inputs/default-policy)
                    (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))))
  (with-repository
    (fn [conn]
      (write! conn (str "inbox/" (apply str (repeat 490 "a")) ".md") "Evidence")
      (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Must not read overlong input paths")))]
        (is (= :input-path-too-long
               (try (inputs/capture @conn inputs/default-policy)
                    (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))))
