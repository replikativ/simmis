(ns is.simm.documents.knowledge-upgrade-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.repo :as repo]
            [is.simm.documents.files :as files]
            [is.simm.documents.knowledge-upgrade :as upgrade]
            [is.simm.documents.markdown :as md]
            [is.simm.documents.profile :as profile]
            [is.simm.documents.space :as space]
            [is.simm.documents.view :as view]
            [kontor.governance :as governance]))

(defn with-repository [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
             :attribute-refs? false :keep-history? false :crypto-hash? true :commit-graph? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (repo/init! conn {:name "Existing authored memory"}) (f conn)
           (finally (governance/ungovern! conn) (d/release conn) (d/delete-database cfg))))))

(defn source-state [conn]
  {:worktree (repo/worktree conn) :head (repo/head-commit conn)
   :refs (d/q '[:find ?name ?target :where [?e :geschichte.ref/name ?name]
               [?e :geschichte.ref/target ?target]] @conn)
   :staged (d/q '[:find ?path ?state :where [?e :geschichte.stage/path ?path]
                 [?e :geschichte.stage/state ?state]] @conn)})

(deftest existing-history-source-and-dirty-files-survive-without-seeded-content
  (with-repository
   (fn [conn]
     (let [page (str "---\nsimmis-id: " (random-uuid) "\n---\n# Original article\n\nKeep this exact statement.\n")]
       (repo/write! conn "wiki/Original.md" (md/source-bytes page))
       (repo/stage-all! conn) (repo/commit! conn {:message "Original history"})
       (repo/write! conn "README.md" (md/source-bytes "# Ordinary Markdown\nNo authored ID needed.\n"))
       (repo/stage-all! conn)
       (let [before @conn source (source-state conn) id (random-uuid)
             plan (upgrade/prepare before id (java.util.Date.))]
         (is (not (space/provisioned? @conn)))
         (is (= (upgrade/basis before) (upgrade/basis @conn)) "Preview writes nothing")
         (is (= 2 (:documents plan)))
         (is (= id (:document.repository/upgrade (upgrade/install! conn plan))))
         (is (= source (source-state conn)))
         (is (= page (:text (files/read-file-in @conn "wiki/Original.md"))))
         (is (nil? (files/entry @conn "wiki/Getting Started.md")))
         (is (nil? (files/entry @conn "wiki/Company.md")))
         (is (empty? (d/q '[:find ?title :where [_ :S.Page/title ?title]] @conn))
             "Canonical page entities remain disposable projections")
         (is (space/provisioned? @conn))
         (is (= :ready (:status (view/prepare @conn profile/selection-options))))
         (is (identical? conn (profile/prepare-open! conn {:profile space/profile})))
         (is (thrown? clojure.lang.ExceptionInfo (upgrade/install! conn plan))))))))

(deftest changed-source-rejects-the-whole-installation
  (with-repository
   (fn [conn]
     (let [plan (upgrade/prepare @conn (random-uuid) (java.util.Date.))]
       (repo/write! conn "late.md" (md/source-bytes "# A later edit\n"))
       (is (thrown? clojure.lang.ExceptionInfo (upgrade/install! conn plan)))
       (is (not (space/provisioned? @conn)))
       (is (not (contains? (d/schema @conn) :entity/uuid)))
       (is (files/entry @conn "late.md"))))))

(deftest malformed-document-installs-nothing
  (with-repository
   (fn [conn]
     (repo/write! conn "bad.md" (md/source-bytes "---\nsimmis-id: broken\n---\n# Bad ID\n"))
     (is (thrown? clojure.lang.ExceptionInfo (upgrade/prepare @conn (random-uuid) (java.util.Date.))))
     (is (not (contains? (d/schema @conn) :entity/uuid))))))
