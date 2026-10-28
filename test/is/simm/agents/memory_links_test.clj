(ns is.simm.agents.memory-links-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.repo :as repo]
            [is.simm.agents.source-memory-test :as fixture]
            [is.simm.documents.files :as files]
            [is.simm.documents.links :as links]
            [is.simm.documents.block-reference :as blocks]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg]))

(defn- group-source [id] (str "<!-- simmis:block " id " -->\nText\n<!-- /simmis:block -->\n"))

(deftest shared-file-links-use-memory-and-missing-links-create-nothing
  (#'fixture/with-room
    (fn [room]
      (files/save-file! room {:path "wiki/A.md" :text "# A\n" :expected nil})
      (let [api (#'fixture/install! room {}) result ((api 'link) "wiki/A.md" {:label "A [page]"})]
        (is (= :ready (:status result)))
        (is (= :memory-file (:kind (links/parse-route (:href result)))))
        (is (not (.contains (:href result) "dh://")))
        (is (.contains (:markdown result) "A \\[page\\]"))
        (is (= {:status :missing :path "missing.md"} ((api 'link) "missing.md")))
        (is (nil? (files/entry @(:conn (workspace/current-workspace)) "missing.md")))))))

(deftest explicit-block-links-use-canonical-uniqueness-and-survive-a-move
  (#'fixture/with-room
    (fn [room]
      (let [id (random-uuid) source (group-source id)]
        (files/save-file! room {:path "a.md" :text source :expected nil})
        (let [api (#'fixture/install! room {}) result ((api 'link) "a.md" {:block id})
              target (links/parse-route (:href result))]
          (is (= :memory-block (:kind target)))
          (files/move-file! room "a.md" "nested/moved.md" (:entry (files/read-file room "a.md")))
          (is (= "nested/moved.md"
                 (:path (blocks/resolve-in @(:conn (workspace/current-workspace))
                                          {:repository-id (parse-uuid (:repository target))
                                           :block-id id :path-hint "a.md"})))))))))

(deftest duplicate-block-identities-never-produce-a-ready-link
  (#'fixture/with-room
    (fn [room]
      (let [id (random-uuid) source (group-source id)]
        (files/save-file! room {:path "a.md" :text source :expected nil})
        (files/save-file! room {:path "b.md" :text source :expected nil})
        (let [api (#'fixture/install! room {})]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ambiguous"
                                ((api 'link) "a.md" {:block id}))))))))

(deftest private-proposal-and-child-world-files-do-not-claim-shared-urls
  (#'fixture/with-room
    (fn [room]
      (files/save-file! room {:path "a.md" :text "parent" :expected nil})
      (let [proposal (ygg/fork!)
            proposal-ws (binding [ec/*execution-context* (:child-ctx proposal)] (workspace/current-workspace))
            api (#'fixture/install! room {:proposal-workspace (constantly proposal-ws)})
            child (ygg/fork!)]
        (try
          (is (= :unpublished (:status ((api 'link) "a.md"))))
          (is (nil? (:href ((api 'link) "a.md"))))
          (binding [ec/*execution-context* (:child-ctx child)]
            (repo/write! (:conn (workspace/current-workspace)) "child.md" (.getBytes "child" "UTF-8"))
            (is (= :isolated-world (:reason ((api 'link) "child.md"))))
            (is (nil? (:href ((api 'link) "child.md")))))
          (finally (ygg/discard-fork! child) (ygg/discard-fork! proposal)))))))

(deftest duplicate-block-identities-within-one-file-refuse-too
  (#'fixture/with-room
    (fn [room]
      (let [id (random-uuid) group (group-source id)]
        (files/save-file! room {:path "duplicate.md" :text (str group "\n" group) :expected nil})
        (let [api (#'fixture/install! room {})]
          (is (thrown? clojure.lang.ExceptionInfo ((api 'link) "duplicate.md" {:block id}))))))))
