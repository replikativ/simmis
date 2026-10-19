(ns is.simm.agents.source-memory-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.effects :as effects]
            [dvergr.room.store.memory :as store]
            [dvergr.runtime.ctx :as runtime-ctx]
            [dvergr.substrate.geschichte :as geschichte]
            [geschichte.repo :as repo]
            [is.simm.agents.source-memory :as source]
            [is.simm.agents.room-agents :as room-agents]
            [is.simm.documents.files :as files]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg]
            [sci.core :as sci]
            [yggdrasil.protocols :as systems]))

(defn- with-room [f]
  (let [room (discourse/make-room {:id (keyword (str "source-memory-" (random-uuid)))
                                   :store (store/make)})]
    (try
      (workspace/ensure-workspace! room)
      (binding [ec/*execution-context* (:ctx room)] (f room))
      (finally (discourse/close-room! room)))))

(defn- install! [room policy]
  (let [cap (random-uuid) owner (:ctx room)
        sid (systems/system-id (geschichte/current-system))]
    (runtime-ctx/install-sandbox-binding!
     owner cap {:agent-id :mcp/source-test :room-runtime-id (:id room)
                :simmis/source-memory {:room-id (random-uuid) :agent-id :mcp/source-test :system-id sid}})
    (with-meta
      (source/bindings owner cap nil
                       (merge {:authorize! (constantly true)
                               :proposal-workspace (constantly nil)
                               :options {:schema-revision :source-memory-test-v1}} policy))
      {:capability-id cap})))

(deftest saved-read-selects-child-world-not-parents-proposal-and-never-writes
  (with-room
    (fn [room]
      (files/save-file! room {:path "a.md" :text "parent" :expected nil})
      (let [proposal (ygg/fork!)
            proposal-ws (binding [ec/*execution-context* (:child-ctx proposal)]
                          (geschichte/current-workspace))
            _ (repo/write! (:conn proposal-ws) "a.md" (.getBytes "proposal" "UTF-8"))
            proposal-reads (atom 0)
            api (install! room {:proposal-workspace (fn [_] (swap! proposal-reads inc) proposal-ws)})
            interpreter (sci/init {:namespaces {'memory api}})
            _ (sci/eval-string* interpreter "(def saved-read memory/read)")
            child-interpreter (sci/fork interpreter)
            saved-read #(sci/eval-string* interpreter (str "(saved-read " (pr-str %) ")"))
            child (ygg/fork!)]
        (try
          (is (= "proposal" (get-in (saved-read "a.md") [:document :source])))
          (binding [ec/*execution-context* (:child-ctx child)]
            (let [ws (geschichte/current-workspace)]
              (repo/write! (:conn ws) "a.md" (.getBytes "child" "UTF-8"))
              (is (= "child" (get-in (sci/eval-string* child-interpreter "(saved-read \"a.md\")")
                                      [:document :source])))))
          (is (= 1 @proposal-reads) "Child never consults the parent's global proposal overlay")
          (is (= "parent" (:text (files/read-file room "a.md"))))
          (finally (ygg/discard-fork! child) (ygg/discard-fork! proposal)))))))

(deftest missing-selected-system-refuses-instead-of-using-another-world
  (with-room
    (fn [room]
      (files/save-file! room {:path "a.md" :text "parent" :expected nil})
      (let [api (install! room {}) child (ygg/fork! {:systems :none})]
        (try
          (binding [ec/*execution-context* (:child-ctx child)]
            ;; Another attached repository must not satisfy the missing exact
            ;; expected system, even though current-workspace can resolve it.
            (workspace/ensure-workspace! (assoc room :ctx (:child-ctx child)))
            (is (some? (geschichte/current-workspace)))
            (is (= :is.simm.agents.source-memory/missing-repository
                   (try ((api 'read) "a.md")
                        (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))
          (finally (ygg/discard-fork! child)))))))

(deftest live-revocation-and-world-read-denial-happen-before-projection
  (with-room
    (fn [room]
      (let [allowed? (atom true) proposals (atom 0)
            api (install! room {:authorize! (fn [_] (when-not @allowed? (throw (ex-info "Revoked" {}))))
                                :proposal-workspace (fn [_] (swap! proposals inc) nil)})]
        (is (= :ready (:status ((api 'documents)))))
        (reset! allowed? false)
        (is (thrown? clojure.lang.ExceptionInfo ((api 'documents))))
        (reset! allowed? true)
        (effects/install-world! (:ctx room) [[:admit #{}]])
        (is (thrown? clojure.lang.ExceptionInfo ((api 'documents))))
        (is (= 1 @proposals))))))

(deftest sibling-callers-cannot-use-a-retained-source-capability
  (with-room
    (fn [room]
      (let [child (ygg/fork!)
            sibling (ygg/fork!)
            api (binding [ec/*execution-context* (:child-ctx child)]
                  (install! (assoc room :ctx (:child-ctx child)) {}))]
        (try
          (binding [ec/*execution-context* (:child-ctx sibling)]
            (is (= :is.simm.agents.source-memory/wrong-world
                   (try ((api 'documents))
                        (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))
          (finally (ygg/discard-fork! sibling) (ygg/discard-fork! child)))))))

(deftest proposal-routing-uses-captured-scope-not-current-first-repository
  (let [room (random-uuid) agent (random-uuid) scope (random-uuid) another (random-uuid)
        target {:room-id room :agent-id (keyword "party" (str agent)) :scope scope}
        own (atom {:ctx-forks {[scope agent] :expected [another agent] :wrong}})
        campaign (atom {:members #{agent} :ctx-forks {[scope agent] :campaign}})
        select #'room-agents/source-proposal-fork]
    (is (= :expected (select {} {[room agent] own} target)))
    (is (= :campaign (select {room campaign} {[room agent] own} target)))
    (swap! campaign assoc :members #{})
    (is (= :expected (select {room campaign} {[room agent] own} target)))
    (is (nil? (select {} {[room agent] own} (assoc target :scope (random-uuid)))))
    (is (nil? (select {} {} target)))))

(deftest identity-changing-rebind-does-not-retain-the-parents-read-authority
  (with-room
    (fn [room]
      (let [checks (atom 0)
            api (install! room {:authorize! (fn [_] (swap! checks inc))})
            cap (:capability-id (meta api))]
        (is (= :ready (:status ((api 'documents)))))
        (runtime-ctx/update-sandbox-binding! (:ctx room) cap assoc :agent-id :party/other)
        (is (= :is.simm.agents.source-memory/missing-binding
               (try ((api 'documents))
                    (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
        (is (= 1 @checks))))))

(deftest proposal-from-another-repository-is-not-a-fallback-source
  (with-room
    (fn [room]
      (with-room
        (fn [_]
          (let [other (geschichte/current-workspace)]
            (binding [ec/*execution-context* (:ctx room)]
              (let [api (install! room {:proposal-workspace (constantly other)})]
                (is (= :is.simm.agents.source-memory/wrong-proposal-repository
                       (try ((api 'documents))
                            (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))))))))
