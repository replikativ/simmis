(ns is.simm.ops.github-merge-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.git.budget :as budget]
            [geschichte.git.http :as http]
            [geschichte.git.object :as object]
            [geschichte.git.pack :as pack]
            [geschichte.git.pkt-line :as pkt]
            [geschichte.git.project :as project]
            [geschichte.repo :as repo]
            [is.simm.ops.github-import :as import]
            [is.simm.ops.github-merge :as preparation]))

(defn- with-repositories [f]
  (let [configs (vec (repeatedly 2 #(hash-map :store {:backend :memory :id (random-uuid)}
                                              :schema-flexibility :write :attribute-refs? false
                                              :keep-history? true :crypto-hash? true :commit-graph? true)))]
    (doseq [config configs] (d/create-database config))
    (let [connections (mapv d/connect configs)]
      (try (apply f connections)
           (finally
             (doseq [conn connections] (d/release conn))
             (doseq [config configs] (d/delete-database config)))))))

(defn- commit! [conn files message]
  (doseq [[path text] files] (repo/write! conn path (object/utf8 text)))
  (repo/stage-all! conn)
  (:geschichte.commit/id (repo/commit! conn {:message message})))

(defn- sender [graph]
  (fn [{:keys [method body]}]
    (let [{:keys [oid objects]} graph]
      {:status 200
       :body (if (= :get method)
               (pkt/encode ["version 2\n" "ls-refs\n" "fetch\n" :flush])
               (if (= "command=ls-refs\n" (pkt/text (first (pkt/decode body))))
                 (pkt/encode [(str oid " HEAD symref-target:refs/heads/main\n")
                              (str oid " refs/heads/main\n") :flush])
                 (pkt/encode ["packfile\n" (object/concat-bytes (byte-array [1]) (pack/encode objects)) :flush])))})))

(defn- options []
  {:import-budget (budget/create {:limits (:limits (preparation/operation-policy)) :timeout-ms 30000})
   :haves [] :prefixes ["refs/heads/main"]})

(defn- read-text [conn path]
  (some-> (repo/read conn path) (String. "UTF-8")))

(defn- with-edited-import [upstream-files f]
  (with-repositories
    (fn [upstream local]
      (repo/init! upstream {:branch "refs/heads/main"})
      (let [base (commit! upstream {"README.md" "Original\nMiddle\nLast\n"} "Initial source")]
        (repo/init! local {:branch "refs/heads/main"})
        (http/clone! (sender (project/project upstream base)) local "origin"
                     "https://github.com/owner/repository.git" (options))
        (let [upstream-base (:geschichte.commit/id (repo/head-commit local))
              local-head (commit! local {"README.md" "Local edit\nMiddle\nLast\n"} "Saved local edit")
              upstream-head (if (seq upstream-files) (commit! upstream upstream-files "Upstream update") base)
              graph (project/project upstream upstream-head)
              opts (options)
              fetched (http/fetch! (sender graph) local "origin"
                                   "https://github.com/owner/repository.git" opts)]
          (f {:local local :local-head local-head :fetched fetched
              :base-graph (project/project upstream base)
              :opts {:ref "refs/heads/main" :preparation-id (random-uuid) :upstream-base upstream-base
                     :operation (:import-budget opts)}}))))))

(deftest imported-update-can-be-merged-after-a-local-autosave-commit
  (with-edited-import
    {"notes.md" "An upstream note\n"}
    (fn [{:keys [local local-head fetched opts]}]
      (let [{:keys [plan upstream-git-oid upstream-head]} (preparation/plan-fetched! local fetched opts)]
        (is (:clean? plan))
        (is (nil? (:geschichte.commit/git-oid (repo/commit-by-id local local-head))))
        (is (= local-head (:geschichte.commit/id (repo/head-commit local))))
        (is (= "Local edit\nMiddle\nLast\n" (read-text local "README.md")))
        (is (nil? (repo/read local "notes.md")) "Planning does not apply incoming files")
        (is (re-matches #"[0-9a-f]{40}" upstream-git-oid))
        ;; Applying here is deliberately explicit, on this disposable private
        ;; repository. Production publication still needs the normal Proposal.
        (repo/prepare-merge! local plan)
        (let [merged (repo/commit! local {:message "Reviewed integration"})]
          (is (= #{local-head upstream-head}
                 (set (map :geschichte.commit/id
                           (:geschichte.commit/parents
                            (repo/commit-by-id local (:geschichte.commit/id merged)))))))
          (is (= "Local edit\nMiddle\nLast\n" (read-text local "README.md")))
          (is (= "An upstream note\n" (read-text local "notes.md"))))))))

(deftest overlapping-upstream-and-local-edits-remain-an-unapplied-conflict
  (with-edited-import
    {"README.md" "Upstream edit\nMiddle\nLast\n"}
    (fn [{:keys [local local-head fetched opts]}]
      (let [{:keys [plan]} (preparation/plan-fetched! local fetched opts)]
        (is (false? (:clean? plan)))
        (is (seq (:conflicts plan)))
        (is (= local-head (:geschichte.commit/id (repo/head-commit local))))
        (is (= "refs/heads/main" (repo/current-ref local)))
        (is (= "Local edit\nMiddle\nLast\n" (read-text local "README.md")))))))

(deftest independent-edits-in-the-same-file-use-the-native-content-merge
  (with-edited-import
    {"README.md" "Original\nMiddle\nUpstream edit\n"}
    (fn [{:keys [local fetched opts]}]
      (let [{:keys [plan]} (preparation/plan-fetched! local fetched opts)]
        (is (:clean? plan))
        (repo/prepare-merge! local plan)
        (repo/commit! local {:message "Reviewed text integration"})
        (is (= "Local edit\nMiddle\nUpstream edit\n" (read-text local "README.md")))))))

(deftest refused-source-or-cancelled-budget-cannot-start-materialization
  (with-edited-import
    {"notes.md" "Incoming\n"}
    (fn [{:keys [local fetched opts]}]
      (let [before (d/commit-id @local)
            duplicate (update fetched :refs conj (first (filter #(= "refs/heads/main" (:ref %)) (:refs fetched))))]
        (is (thrown? clojure.lang.ExceptionInfo (preparation/plan-fetched! local duplicate opts)))
        (is (= before (d/commit-id @local)))
        (is (thrown? clojure.lang.ExceptionInfo
                     (preparation/plan-fetched! local fetched
                                                (assoc opts :operation
                                                       (budget/create {:limits (:limits (preparation/operation-policy)) :timeout-ms 30000
                                                                       :cancelled? (constantly true)})))))
        (is (= before (d/commit-id @local)))))))

(deftest unchanged-upstream-does-not-replace-saved-local-work
  (with-edited-import
    {}
    (fn [{:keys [local local-head fetched opts]}]
      (let [{:keys [plan]} (preparation/plan-fetched! local fetched opts)]
        (is (:clean? plan))
        (is (= (:base plan) (:theirs plan)))
        (is (= local-head (:geschichte.commit/id (repo/head-commit local))))
        (is (= "Local edit\nMiddle\nLast\n" (read-text local "README.md")))))))

(deftest unsaved-private-work-refuses-before-materialization
  (with-edited-import
    {"notes.md" "Incoming\n"}
    (fn [{:keys [local fetched opts]}]
      (repo/write! local "README.md" (object/utf8 "Uncommitted work\n"))
      (let [before (d/commit-id @local)]
        (is (thrown? clojure.lang.ExceptionInfo (preparation/plan-fetched! local fetched opts)))
        (is (= before (d/commit-id @local)))
        (is (= "Uncommitted work\n" (read-text local "README.md")))))))

(deftest cancellation-after-import-retains-the-partially-changed-private-owner
  (with-edited-import
    {"notes.md" "Incoming\n"}
    (fn [{:keys [local local-head fetched opts]}]
      (let [incoming (str "refs/heads/simmis-git-incoming-" (:preparation-id opts))
            operation (budget/create {:limits (:limits (preparation/operation-policy)) :timeout-ms 30000
                                      :cancelled? #(= incoming (repo/current-ref local))})]
        (is (thrown? clojure.lang.ExceptionInfo
                     (preparation/plan-fetched! local fetched (assoc opts :operation operation))))
        (is (= incoming (repo/current-ref local)) "The owner must quarantine this private repository")
        (is (= "Local edit\nMiddle\nLast\n"
               (String. ^bytes (repo/read-at local local-head "README.md") "UTF-8")))
        (is (thrown? clojure.lang.ExceptionInfo (preparation/plan-fetched! local fetched opts))
            "No blind retry on a partially prepared owner")))))

(deftest upstream-rewind-is-not-offered-as-an-ordinary-merge
  (with-edited-import
    {"notes.md" "New remote revision\n"}
    (fn [{:keys [local fetched opts base-graph]}]
      (let [observed (preparation/plan-fetched! local fetched opts)
            rewind-options (options)
            rewound (http/fetch! (sender base-graph) local "origin"
                                 "https://github.com/owner/repository.git" rewind-options)
            request (assoc opts :preparation-id (random-uuid)
                           :upstream-base (:upstream-head observed)
                           :operation (:import-budget rewind-options))]
        (try
          (preparation/plan-fetched! local rewound request)
          (is false "A remote rewind must not become a ready candidate")
          (catch clojure.lang.ExceptionInfo error
            (is (= :upstream-history-changed (:reason (ex-data error))))))
        (is (= "Local edit\nMiddle\nLast\n" (read-text local "README.md")))))))

(deftest incomplete-merge-profile-refuses-before-native-materialization
  (with-edited-import
    {"notes.md" "Incoming\n"}
    (fn [{:keys [local fetched opts]}]
      (let [checkpoint (d/commit-id @local)
            incomplete (budget/create {:limits import/limits :timeout-ms 30000})]
        (is (thrown? clojure.lang.ExceptionInfo
                     (preparation/plan-fetched! local fetched (assoc opts :operation incomplete))))
        (is (= checkpoint (d/commit-id @local)))))))

(deftest a-merged-result-must-fit-even-when-each-side-fits
  (with-edited-import
    {"remote.md" "Remote addition\n"}
    (fn [{:keys [local fetched opts]}]
      (commit! local {"local.md" "Local addition\n"} "Another saved local change")
      (with-redefs [import/limits (assoc import/limits :checkout-files 2)]
        (is (= :tree-limit
               (try (preparation/plan-fetched! local fetched opts) nil
                    (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))
      (is (= "Local addition\n" (read-text local "local.md")))
      (is (nil? (repo/read local "remote.md"))))))
