(ns is.simm.documents.files-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.effects :as effects]
            [dvergr.room.store.memory :as memory]
            [dvergr.rooms.files :as room-files]
            [geschichte.content :as content]
            [geschichte.repo :as repo]
            [is.simm.documents.files :as files]))

(defn- with-room [f]
  (let [room (discourse/make-room {:id (keyword (str "memory-editors-" (random-uuid)))
                                   :store (memory/make)})]
    (try (workspace/ensure-workspace! room) (f room)
         (finally (discourse/close-room! room)))))

(defn- save! [room path text expected]
  (files/save-file! room {:path path :text text :expected expected}))

(deftest one-tree-includes-notes-code-and-attachments-without-loading-blobs
  (with-room
    (fn [room]
      (save! room "wiki/research.md" "# Research" nil)
      (save! room "src/project/core.clj" "(throw (ex-info \"Must not execute\" {}))" nil)
      (save! room "notes-outside-wiki.md" "Also knowledge" nil)
      (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Metadata listing read a blob")))]
        (is (= ["src" "wiki" "notes-outside-wiki.md"] (mapv :name (:entries (files/list-folder room "")))))
        (is (= :markdown (-> (files/list-folder room "wiki") :entries first :kind)))
        (is (= :text (-> (files/list-folder room "src/project") :entries first :kind))))
      (is (= :markdown (:kind (files/read-file room "notes-outside-wiki.md"))))
      (is (= :source-only (:execution-policy (files/read-file room "src/project/core.clj")))))))

(deftest source-open-save-and-no-op-are-byte-preserving-and-revision-checked
  (with-room
    (fn [room]
      (let [path "wiki/资料.md" text "\uFEFF---\r\ncustom: true\r\n---\r\n# Hello 🦋\r\n"
            initial (save! room path text nil)
            opened (files/read-file room path)]
        (is (= (:entry initial) (:entry opened)))
        (is (= text (:text opened)))
        (is (nil? (:commit (save! room path text (:entry opened)))))
        (save! room path (str text "New\r\n") (:entry opened))
        (is (thrown? clojure.lang.ExceptionInfo (save! room path "stale" (:entry opened))))
        (is (= (str text "New\r\n") (:text (files/read-file room path))))
        (is (thrown? clojure.lang.ExceptionInfo (save! room path "second creator" nil)))))))

(deftest file-saves-use-the-canonical-writers-own-opened-snapshot
  (with-room
    (fn [room]
      (save! room "wiki/a.md" "opened" nil)
      (let [opened (files/read-file room "wiki/a.md")
            worktree repo/worktree
            raced? (atom false)]
        (with-redefs [repo/worktree (fn [conn]
                                     (when (compare-and-set! raced? false true)
                                       (repo/write! conn "wiki/a.md" (.getBytes "agent edit" "UTF-8")))
                                     (worktree conn))]
          (is (thrown? clojure.lang.ExceptionInfo
                       (save! room "wiki/a.md" "stale human" (:entry opened)))))
        (is @raced?)
        (is (= "agent edit" (:text (files/read-file room "wiki/a.md"))))))))

(deftest moving-keeps-source-and-refuses-an-occupied-destination
  (with-room
    (fn [room]
      (let [text "---\nsimmis-id: '00000000-0000-4000-8000-000000000001'\n---\n# Stable"
            opened (save! room "wiki/old.md" text nil)]
        (files/move-file! room "wiki/old.md" "research/new.md" (:entry opened))
        (is (thrown? clojure.lang.ExceptionInfo (files/read-file room "wiki/old.md")))
        (is (= text (:text (files/read-file room "research/new.md"))))
        (save! room "occupied.md" "existing" nil)
        (is (thrown? clojure.lang.ExceptionInfo
                     (files/move-file! room "research/new.md" "occupied.md" (:entry opened))))
        (is (= "existing" (:text (files/read-file room "occupied.md"))))
        (is (= text (:text (files/read-file room "research/new.md"))))))))

(deftest exact-paths-and-closed-write-shapes-protect-the-source-boundary
  (with-room
    (fn [room]
      (doseq [path ["../outside.md" "/absolute.md" "a//b" "a/./b" "a\\b" ".env" ".git/config"]]
        (is (thrown? clojure.lang.ExceptionInfo (save! room path "bad" nil))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (files/save-file! room {:path "wiki/a.md" :text "no expected"})))
      (is (thrown? clojure.lang.ExceptionInfo
                   (files/save-file! room {:path "wiki/a.md" :expected nil :text "bad" :eval true})))
      (is (empty? (:entries (files/list-folder room "")))))))

(deftest executable-code-preserves-mode-through-repeated-save-and-move
  (with-room
    (fn [room]
      (room-files/write! room {"script.sh" {:text "echo one\n"}} {:file-modes {"/script.sh" 33261}})
      (let [opened (files/read-file room "script.sh")
            saved (save! room "script.sh" "echo two\n" (:entry opened))
            again (save! room "script.sh" "echo three\n" (:entry saved))
            moved (files/move-file! room "script.sh" "src/script.sh" (:entry again))]
        (is (= 33261 (get-in saved [:entry :mode])))
        (is (= 33261 (get-in moved [:entry :mode])))
        (is (= (:entry moved) (:entry (files/read-file room "src/script.sh"))))))))

(deftest an-intercepted-effect-is-not-a-physical-save-acknowledgement
  (with-room
    (fn [room]
      (with-redefs [room-files/host-boundary
                    (fn [& _] (constantly {:handlers [(fn [_effect _next] nil)]}))]
        (is (thrown? clojure.lang.ExceptionInfo (save! room "skipped.md" "not written" nil))))
      (is (empty? (:entries (files/list-folder room "")))))))

(deftest an-acknowledgement-never-adopts-a-later-agent-edit
  (with-room
    (fn [room]
      (let [write room-files/write!
            saved (with-redefs [room-files/write!
                               (fn [& args]
                                 (let [result (apply write args)]
                                   (files/with-workspace room
                                     #(repo/write! % "wiki/a.md" (.getBytes "newer agent" "UTF-8")))
                                   result))]
                    (save! room "wiki/a.md" "human version" nil))]
        (is (not= (:entry saved) (:entry (files/read-file room "wiki/a.md"))))
        (is (thrown? clojure.lang.ExceptionInfo (save! room "wiki/a.md" "next human" (:entry saved))))
        (is (= "newer agent" (:text (files/read-file room "wiki/a.md"))))))))

(deftest invalid-explicit-identities-are-not-treated-as-missing
  (doseq [value ["null" "false" "not-a-uuid"]]
    (let [analysis (files/analyze (random-uuid) "wiki/a.md" (str "---\nsimmis-id: " value "\n---\nbody"))]
      (is (= :invalid (:identity-status analysis)))
      (is (some #(= :invalid-document-id (:kind %)) (:diagnostics analysis))))))
