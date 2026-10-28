(ns is.simm.agents.memory-write-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [dvergr.effects :as effects]
            [dvergr.tools :as tools]
            [dvergr.substrate.geschichte :as workspace]
            [is.simm.agents.source-memory-test :as fixture]
            [is.simm.documents.files :as files]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg]
            [sci.core :as sci]))

(defn- api! [room policy]
  (#'fixture/install! room (merge {:authorize-write! (constantly true)
                                 :proposal-workspace! (constantly nil)} policy)))

(deftest guarded-source-edit-shares-editor-bytes-and-rejects-stale-entry
  (#'fixture/with-room
    (fn [room]
      (let [api (api! room {}) text "# Company\n\nA finding.\n"
            result ((api 'write!) {:path "wiki/Company.md" :text text :expected nil})]
        (is (= :written (:status result)))
        (is (= :worktree (:publication result)))
        (is (not (contains? result :commit)) "No logical commit is invented")
        (is (= text (:text (files/read-file room "wiki/Company.md"))))
        (let [opened (:entry ((api 'read) "wiki/Company.md"))]
          (is (= (:entry result) opened))
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"changed"
                                ((api 'write!) {:path "wiki/Company.md" :text "overwrite" :expected nil})))
          (files/save-file! room {:path "wiki/Editor.md" :text text :expected nil})
          (is (= opened (:entry (files/read-file room "wiki/Editor.md"))) "Editor and REPL retain identical bytes/mode")
          ((api 'write!) {:path "wiki/Company.md" :text "# Changed\n" :expected opened})
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"changed"
                                ((api 'write!) {:path "wiki/Company.md" :text "stale" :expected opened}))))))))

(deftest saved-repl-write-follows-child-world-and-discard-leaves-parent-alone
  (#'fixture/with-room
    (fn [room]
      (files/save-file! room {:path "a.md" :text "parent" :expected nil})
      (let [api (api! room {}) expected (:entry ((api 'read) "a.md"))
            interpreter (sci/init {:runtime-mode :forkable :namespaces {'memory api}})
            _ (sci/eval-string* interpreter "(def saved-write memory/write!)")
            child-interpreter (sci/fork interpreter)
            child (ygg/fork!)]
        (try
          (binding [ec/*execution-context* (:child-ctx child)]
            (is (= :written (:status (sci/eval-string* child-interpreter
                                      (str "(saved-write " (pr-str {:path "a.md" :text "child" :expected expected}) ")")))))
            (is (= "child" (get-in ((api 'read) "a.md") [:document :source]))))
          (is (= "parent" (:text (files/read-file room "a.md"))))
          (finally
            (sci/retire! child-interpreter)
            (sci/retire! interpreter)
            (ygg/discard-fork! child)))
        (is (= "parent" (:text (files/read-file room "a.md"))))))))

(deftest skipped-effect-cannot-fabricate-a-success-receipt
  (#'fixture/with-room
    (fn [room]
      (let [api (api! room {})]
        (with-redefs [effects/perform! (fn [& _] "/fake-success.md")]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"did not apply"
                                ((api 'write!) {:path "skipped.md" :text "must not land" :expected nil}))))
        (is (nil? (files/entry @(:conn (workspace/current-workspace)) "skipped.md")))))))

(deftest revocation-at-native-effect-gate-refuses-the-edit
  (#'fixture/with-room
    (fn [room]
      (let [calls (atom 0)
            api (api! room {:authorize-write! (fn [_]
                                               (when (>= (swap! calls inc) 3)
                                                 (throw (ex-info "revoked" {}))))})]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"revoked"
                              ((api 'write!) {:path "forbidden.md" :text "no" :expected nil})))
        (is (nil? (files/entry @(:conn (workspace/current-workspace)) "forbidden.md")))))))

(deftest concurrent-mode-change-is-not-lost-behind-a-content-only-check
  (#'fixture/with-room
    (fn [room]
      (files/save-file! room {:path "a.md" :text "old" :expected nil})
      (let [api (api! room {}) expected (:entry ((api 'read) "a.md"))
            conn (:conn (workspace/current-workspace)) write! tools/workspace-write!]
        (with-redefs [tools/workspace-write! (fn [context path content opts]
                                              (d/transact conn [{:geschichte.work/path "a.md" :geschichte.work/mode 33261}])
                                              (write! context path content opts))]
          (is (thrown? clojure.lang.ExceptionInfo
                       ((api 'write!) {:path "a.md" :text "must not overwrite" :expected expected}))))
        (is (= "old" (:text (files/read-file room "a.md"))))
        (is (= 33261 (:mode (files/entry @conn "a.md"))))))))

(deftest dispatched-failure-is-pending-and-never-automatically-retried
  (#'fixture/with-room
    (fn [room]
      (let [api (api! room {}) calls (atom 0)]
        (with-redefs [tools/workspace-write! (fn [context & _]
                                              (swap! calls inc)
                                              (vreset! (:basis-write-state context) :dispatched)
                                              (throw (ex-info "Lost acknowledgement" {})))]
          (try
            ((api 'write!) {:path "pending.md" :text "unknown" :expected nil})
            (is false "A pending write is not a success")
            (catch clojure.lang.ExceptionInfo error
              (is (= :is.simm.agents.memory-write/pending (:type (ex-data error))))))
        (is (= 1 @calls))))))
)

(deftest acknowledged-write-retains-a-later-effect-error-without-rollback
  (#'fixture/with-room
    (fn [room]
      (let [api (api! room {})]
        (with-redefs [effects/perform! (fn [_ _ thunk]
                                        (thunk)
                                        (throw (ex-info "Later handler failed" {:type :outer-handler})))]
          (let [result ((api 'write!) {:path "applied.md" :text "acknowledged" :expected nil})]
            (is (= :written (:status result)))
            (is (= :outer-handler (get-in result [:effect-error :type])))))
        (is (= "acknowledged" (:text (files/read-file room "applied.md"))))))))

(deftest recovery-bearing-native-error-propagates-by-identity
  (#'fixture/with-room
    (fn [room]
      (let [api (api! room {}) failure (ex-info "Recovery owns pending work" {:construction/owner (Object.)})]
        (with-redefs [tools/workspace-write! (fn [context & _]
                                              (vreset! (:basis-write-state context) :dispatched)
                                              (throw failure))]
          (is (identical? failure
                          (try ((api 'write!) {:path "owned.md" :text "pending" :expected nil})
                               (catch Throwable error error)))))))))

(deftest ordinary-proposal-edits-use-the-existing-proposal-workspace
  (#'fixture/with-room
    (fn [room]
      (files/save-file! room {:path "a.md" :text "parent" :expected nil})
      (let [proposal (ygg/fork!)
            ws (binding [ec/*execution-context* (:child-ctx proposal)] (workspace/current-workspace))
            api (api! room {:proposal-workspace (constantly ws) :proposal-workspace! (constantly ws)})
            expected (:entry ((api 'read) "a.md"))]
        (try
          (is (= :written (:status ((api 'write!) {:path "a.md" :text "proposal" :expected expected}))))
          (is (= "proposal" (get-in ((api 'read) "a.md") [:document :source])))
          (is (= "parent" (:text (files/read-file room "a.md"))))
          (finally (ygg/discard-fork! proposal)))))))
