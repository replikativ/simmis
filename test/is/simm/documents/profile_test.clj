(ns is.simm.documents.profile-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [geschichte.repo :as repo]
            [dvergr.catalog.room :as catalog]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.profile :as profile]
            [is.simm.documents.space :as space]
            [is.simm.documents.starter-bundle :as starter]
            [is.simm.documents.view :as view]
            [is.simm.starter.wiki-maintenance :as wiki]
            [is.simm.starter.conversation-digest :as digest]
            [is.simm.starter.daily-room-summary :as daily]
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

(deftest import-foundation-never-seeds-files-or-local-git-history
  (with-store
    (fn [conn]
      (with-redefs [starter/read-packaged! (fn [] (throw (AssertionError. "No packaged sources during import")))
                    wiki/source (fn [] (throw (AssertionError. "No workflow defaults during import")))]
        (is (identical? conn (profile/initialize-foundation! conn options))))
      (is (space/provisioned? @conn))
      (is (empty? (repo/worktree conn)))
      (is (nil? (repo/head-commit conn)))
      (is (identical? conn (profile/prepare-open! conn options)))
      (is (thrown? clojure.lang.ExceptionInfo (profile/initialize-foundation! conn options))))))

(deftest encoded-benchmark-fixtures-do-not-acquire-live-wiki-identities
  (with-store
    (fn [conn]
      (profile/initialize! conn options (starter/read-packaged!))
      (let [original (:text (files/read-file-in @conn "wiki/Company.md"))
            packed (pr-str {:kind :dvergr/fixture-pack :version 1 :files {"/wiki/Company.md" original}})
            source (assoc (wiki/source) "cases/a/fixture-pack.edn" packed
                                        "cases/a/gold.edn" "{}"
                                        "cases/b/fixture-pack.edn" packed
                                        "cases/b/gold.edn" "{}")
            before (view/prepare @conn profile/selection-options)
            paths #(set (d/q '[:find [?path ...] :where [_ :document/path ?path]] (:db %)))
            bundle (catalog/bundle "maintain-wiki-development" source)]
        (is (= original (get-in bundle [:cases "a" :fixtures "/wiki/Company.md"])))
        (is (= original (get-in bundle [:cases "b" :fixtures "/wiki/Company.md"])))
        (doseq [[path text] source]
          (repo/write! conn (str "workflows/maintain-wiki-development/" path) (markdown/source-bytes text)))
        (let [after (view/prepare @conn profile/selection-options)]
          (is (= :ready (:status after)) (pr-str (:diagnostics after)))
          (is (= (paths before) (paths after)))
          (is (= 0 (d/q '[:find ?v . :where [_ :S.Company/revenue ?v]] (:db after))))
          (is (= false (d/q '[:find ?v . :where [_ :S.Research/active ?v]] (:db after))))
          (is (= 1 (count (d/q '[:find ?e :where [?e :document/path "wiki/Company.md"]] (:db after)))))
          (is (= original (:text (files/read-file-in @conn "wiki/Company.md"))))
          (is (= packed (:text (files/read-file-in @conn "workflows/maintain-wiki-development/cases/a/fixture-pack.edn")))))))))

(deftest pristine-initialization-provides-one-valid-typed-file-backed-wiki
  (with-store
    (fn [conn]
      (is (identical? conn (profile/initialize! conn options (starter/read-packaged!))))
      (let [before @conn prepared (view/prepare before profile/selection-options)]
        (is (= :ready (:status prepared)) (pr-str (:diagnostics prepared)))
        (is (= (into #{"wiki/Welcome.md" "wiki/People/Founder.md" "wiki/Company.md" "code/welcome.clj" "code/starter-provenance.edn" "summaries/index.md"}
                     (concat (keys (:files (starter/read-packaged!)))
                             (map #(str "workflows/" wiki/name "/" %) (keys (wiki/source)))
                             (map #(str "workflows/" digest/name "/" %) (keys (digest/source)))
                             (map #(str "workflows/" daily/name "/" %) (keys (daily/source)))))
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
        (doseq [broken [(d/db-with before [[:db/retractEntity [:db/ident :document/source-content]]])
                        (d/db-with before [[:db/add (:e (first (d/datoms before :aevt :geschichte.repo/id)))
                                            :document.repository/profile :unknown/profile]])]]
          (is (thrown? clojure.lang.ExceptionInfo (profile/prepare-open! (atom broken) options))))
        (is (thrown? clojure.lang.ExceptionInfo (profile/prepare-open! conn {:profile :unknown/profile})))
        (is (= before @conn)))
      (repo/write! conn "wiki/Broken.md" (markdown/source-bytes (str "---\nsimmis-id: '" (random-uuid) "'\n---\n# Repaired\n")))
      (is (= :ready (:status (view/prepare @conn profile/selection-options)))))))

(deftest reopening-requires-the-canonical-foundation-not-the-current-chat-vocabulary
  (with-store
    (fn [conn]
      (profile/initialize-foundation! conn options)
      (repo/write! conn "wiki/Existing.md" (markdown/source-bytes "# Existing\n\nPreserve this source.\n"))
      (let [before @conn before-files (repo/worktree conn)
            without (fn [ident] (d/db-with before [[:db/retractEntity [:db/ident ident]]]))]
        (doseq [ident [:message/compacted-message-ids :chat/description]]
          (testing (str "Missing optional runtime chat attribute " ident)
            (is (contains? (d/schema before) ident) "The new-store installer still supplies chat schema")
            (let [snapshot (without ident) old-connection (atom snapshot)]
              (is (not (contains? (d/schema snapshot) ident)))
              (with-redefs [d/transact (fn [& _] (throw (AssertionError. "Reopening must not install schema or write files")))]
                (is (identical? old-connection (profile/prepare-open! old-connection options))))
              (is (= snapshot @old-connection))
              (is (not (contains? (d/schema @old-connection) ident)))
              (is (= before @conn))
              (is (= before-files (repo/worktree conn))))))
        (testing "Optional does not permit malformed present chat attributes"
          (let [broken (d/db-with (without :chat/description)
                                  [{:db/ident :chat/description :db/valueType :db.type/long
                                    :db/cardinality :db.cardinality/one}])
                old-connection (atom broken)]
            (with-redefs [d/transact (fn [& _] (throw (AssertionError. "Reopening cannot repair incompatible schema")))]
              (is (= :incompatible-present-chat-schema
                     (try (profile/prepare-open! old-connection options) nil
                          (catch clojure.lang.ExceptionInfo error (:reason (ex-data error)))))))
            (is (= broken @old-connection))))
        (testing "Actual document foundation remains mandatory"
          (let [broken (without :document/source-content)
                old-connection (atom broken)]
            (with-redefs [d/transact (fn [& _] (throw (AssertionError. "Reopening cannot repair missing foundation")))]
              (is (= :incompatible-mandatory-schema
                     (try (profile/prepare-open! old-connection options) nil
                          (catch clojure.lang.ExceptionInfo error (:reason (ex-data error)))))))
            (is (= broken @old-connection))))
        (is (= before @conn))
        (is (= before-files (repo/worktree conn)))))))

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
