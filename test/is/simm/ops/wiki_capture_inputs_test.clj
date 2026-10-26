(ns is.simm.ops.wiki-capture-inputs-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.catalog.room :as catalog]
            [geschichte.content :as content]
            [geschichte.repo :as repo]
            [is.simm.documents.markdown :as markdown]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.workflow-runs :as runs]
            [is.simm.starter.wiki-maintenance :as starter]))

(def root "captures/2026-10-25/05-00-00--91359c37-d5a7-4771-a66c-7c3114a1b9a2")
(def note-path (str root "/observations/05-00-01--000000.md"))
(def image-path (str root "/images/frame.jpg"))
(def video-path (str root "/recordings/05-00-01--000000.webm"))
(def note
  "---\nsimmis-id: 7ebacbe7-949c-4b1e-a987-1db2110bce18\nsource-kind: screen-observation\ntimestamp-basis: server-received\nrecording-alignment: estimated\n---\n# Screen observations\n\n![Screenshot](../images/frame.jpg)\n\n[Original recording](../recordings/05-00-01--000000.webm)\n\n```text\nVisible draft: delivery date remains unconfirmed.\n```\n")

(defn- with-repository [f]
  (let [config {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
                :keep-history? true :branch-history? true}]
    (d/create-database config)
    (let [conn (d/connect config)]
      (try (repo/init! conn {:name "Capture workflow inputs"}) (f conn)
           (finally (d/release conn) (d/delete-database config))))))
(defn- write! [conn path text] (repo/write! conn path (markdown/source-bytes text)))
(defn- policy [] (inputs/declaration (starter/source 3)))

(deftest packaged-capture-workflow-is-versioned-without-changing-earlier-source
  (let [source (starter/source) old (starter/source 2)
        instructions (get source "fixtures/workflow-context/wiki-maintenance.txt")]
    (is (= 4 starter/version))
    (is (= (policy) (inputs/declaration source)))
    (is (= ["captures" "inbox" "transcripts"] (:trigger-roots (policy))))
    (is (= inputs/default-policy (inputs/declaration old)))
    (is (= ["wiki"] (:context-roots (policy))))
    (is (str/includes? instructions "estimated"))
    (is (str/includes? instructions "Verify the recorded content identity"))
    (is (str/includes? instructions "do not copy a relative link"))
    (is (str/includes? instructions "do not authorize computer input"))))

(deftest captures-join-other-evidence-without-loading-binary-or-unselected-bodies
  (with-repository
    (fn [conn]
      (doseq [[path text] {note-path note "inbox/interview.md" "The delivery date is still a question."
                           "transcripts/meeting.md" "Follow up next week."
                           "wiki/customer.md" "Existing knowledge" "private/unselected.md" "Unselected"}]
        (write! conn path text))
      (repo/write! conn image-path (byte-array 32))
      (repo/write! conn video-path (byte-array 64))
      (let [worktree (repo/worktree conn)
            forbidden (set (map #(get-in worktree [% :content]) [image-path video-path "private/unselected.md"]))
            read content/read-by-id reads (atom [])
            captured (with-redefs [content/read-by-id
                                   (fn [conn id & [options]]
                                     (is (not (contains? forbidden id)))
                                     (swap! reads conj id) (read conn id options))]
                       (inputs/capture @conn (policy)))
            entries (get-in captured [:manifest :files])]
        (is (= 4 (count @reads)))
        (is (= #{note-path "inbox/interview.md" "transcripts/meeting.md" "wiki/customer.md"} (set (map :path entries))))
        (is (= note (get-in captured [:inputs (str "/in/knowledge/sources/" note-path)])))
        (is (= :trigger (:role (first (filter #(= note-path (:path %)) entries)))))
        (is (= :context (:role (first (filter #(= "wiki/customer.md" (:path %)) entries)))))
        (is (= captured (inputs/verify-capture (policy) captured)))
        (let [source (starter/source 3)
              prepared (runs/prepare-source {:files source}
                                            {:name starter/name :expected-bundle (:id (catalog/bundle starter/name source))
                                             :inputs (:inputs captured)})]
          (is (= (:inputs captured) (select-keys (:inputs prepared) (keys (:inputs captured)))))
          (is (= (get source "fixtures/workflow-context/wiki-maintenance.txt")
                 (get-in prepared [:inputs "/workflow-context/wiki-maintenance.txt"])))
          (is (= {} (get-in prepared [:workflow :params]))))
        (let [before (inputs/selection @conn (policy))]
          (repo/write! conn image-path (byte-array 33))
          (is (= before (inputs/selection @conn (policy))) "Media changes alone do not bill another text-workflow run")
          (write! conn "wiki/customer.md" "Adopted wiki update")
          (let [after (inputs/selection @conn (policy))]
            (is (= (:trigger-id before) (:trigger-id after)) "Wiki adoption does not self-trigger")
            (is (not= (:execution-id before) (:execution-id after)))))))))
