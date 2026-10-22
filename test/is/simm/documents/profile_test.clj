(ns is.simm.documents.profile-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.repo :as repo]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.profile :as profile]
            [is.simm.documents.space :as space]
            [is.simm.documents.starter-bundle :as starter]
            [is.simm.documents.view :as view]
            [is.simm.starter.wiki-maintenance :as wiki]
            [kontor.book :as book]
            [kontor.governance :as governance]))

(defn- with-store [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
             :attribute-refs? false :keep-history? false :crypto-hash? true :commit-graph? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (f conn)
           (finally (governance/ungovern! conn) (d/release conn) (d/delete-database cfg))))))

(def options {:profile space/profile})

(deftest pristine-initialization-provides-one-valid-typed-file-backed-wiki
  (with-store
    (fn [conn]
      (is (identical? conn (profile/initialize! conn options (starter/read-packaged!))))
      (let [before @conn prepared (view/prepare before profile/selection-options)]
        (is (= :ready (:status prepared)) (pr-str (:diagnostics prepared)))
        (is (= (into #{"wiki/Welcome.md" "wiki/People/Founder.md" "wiki/Company.md" "code/welcome.clj" "code/starter-provenance.edn"}
                     (concat (keys (:files (starter/read-packaged!)))
                             (map #(str "workflows/" wiki/name "/" %) (keys (wiki/source)))))
               (set (keys (repo/worktree conn)))))
        (is (= 0 (d/q '[:find ?value . :where [_ :S.Company/revenue ?value]] (:db prepared))))
        (is (= false (d/q '[:find ?value . :where [_ :S.Research/active ?value]] (:db prepared))))
        (is (= 1 (count (d/q '[:find ?company ?person :where
                               [?company :S.Company/founder ?person]
                               [?person :document/path "wiki/People/Founder.md"]] (:db prepared)))))
        (is (empty? (d/datoms before :aevt :document/id)) "Projection is derived, not another authored wiki")
        (is (true? (:clean? (repo/status conn))) "The entire starter tree is committed")
        (is (= (set (keys (repo/worktree conn))) (set (keys (repo/tree-at conn)))))
        (governance/ungovern! conn)
        (is (identical? conn (profile/prepare-open! conn options)))
        (is (= before @conn) "Reopening does not install, seed or transact")
        (is (thrown-with-msg? Throwable #"trusted writer" (d/transact conn [{:attempt/id (random-uuid)}])))
        (is (thrown? clojure.lang.ExceptionInfo (profile/initialize! conn options (starter/read-packaged!))))
        (is (= before @conn)))
      (book/entry! conn {:debit-account [:kontor.account/path "Assets:Bank"]
                         :credit-account [:kontor.account/path "Equity:Opening"]
                         :amount 1M :commodity [:kontor.commodity/symbol "USD"]
                         :journal-type :general :effective-date #inst "2026-10-21"})
      (let [posting (d/q '[:find ?p . :where [?p :kontor.posting/account ?account]
                           [?account :kontor.account/path "Assets:Bank"]] @conn)
            error (try (d/transact conn [{:db/id posting :kontor.posting/amount 2M}]) nil
                       (catch Throwable error error))]
        (is (some #{:sealing/silent-retract-of-posted :validation/sum-to-zero}
                  (keep #(-> % ex-data :type) (take-while some? (iterate ex-cause error)))))
        (is (= 1M (:kontor.posting/amount (d/pull @conn [:kontor.posting/amount] posting))))))))

(deftest invalid-authored-content-does-not-prevent-source-repair-but-missing-foundation-refuses
  (with-store
    (fn [conn]
      (profile/initialize! conn options (starter/read-packaged!))
      (repo/write! conn "wiki/Broken.md" (markdown/source-bytes "---\nsimmis-id: not-a-uuid\n---\n# Broken\n"))
      (let [before @conn prepared (view/prepare before profile/selection-options)]
        (is (= :invalid (:status prepared)))
        (is (nil? (:db prepared)) "No partially valid canonical query view")
        (is (identical? conn (profile/prepare-open! conn options)))
        (is (= before @conn))
        (doseq [broken [(d/db-with before [[:db/retractEntity [:db/ident :chat/description]]])
                        (d/db-with before [[:db/add (:e (first (d/datoms before :aevt :geschichte.repo/id)))
                                            :document.repository/profile :unknown/profile]])]]
          (is (thrown? clojure.lang.ExceptionInfo (profile/prepare-open! (atom broken) options))))
        (is (thrown? clojure.lang.ExceptionInfo (profile/prepare-open! conn {:profile :unknown/profile})))
        (is (= before @conn)))
      (repo/write! conn "wiki/Broken.md" (markdown/source-bytes (str "---\nsimmis-id: '" (random-uuid) "'\n---\n# Repaired\n")))
      (is (= :ready (:status (view/prepare @conn profile/selection-options)))))))

(deftest reopening-never-reinstalls-edited-or-deleted-default-workflow-files
  (with-store
    (fn [conn]
      (profile/initialize! conn options (starter/read-packaged!))
      (repo/write! conn "workflows/maintain-wiki/workflow.edn" (markdown/source-bytes "{:custom true}\n"))
      (repo/remove! conn "workflows/maintain-wiki/checker.clj")
      (let [before @conn]
        (with-redefs [wiki/source (fn [] (throw (AssertionError. "No packaged defaults during reopen")))]
          (is (identical? conn (profile/prepare-open! conn options))))
        (is (= before @conn))))))
