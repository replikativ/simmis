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
            [is.simm.documents.execution-view :as execution]
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

(deftest execution-memory-reads-canonical-pages-not-duplicated-staging
  (with-room
    (fn [room]
      (let [source (str "---\nsimmis-id: '" (random-uuid) "'\n---\n# Company\n")]
        (files/save-file! room {:path "wiki/Company.md" :text source :expected nil})
        (let [api (install! room {})
              fork (ygg/fork!)]
          (binding [ec/*execution-context* (:child-ctx fork)]
            (let [conn (:conn (geschichte/current-workspace))]
              (doseq [path ["in/knowledge/sources/wiki/Company.md" "out/wiki/Company.md"]]
                (repo/write! conn path (.getBytes source "UTF-8"))))
            (is (= :invalid (:status ((api 'documents)))))
            (execution/install! {:ctx (:child-ctx fork)})
            (is (= :ready (:status ((api 'documents)))))
            (is (= #{"wiki/Company.md"} (set (map :path (:documents ((api 'documents)))))))
            (is (nil? (:document ((api 'read) "out/wiki/Company.md")))))
          (let [[a b] (ygg/copy-fork! fork 2)]
            (try
              (doseq [copy [a b]]
                (binding [ec/*execution-context* (:child-ctx copy)]
                  (is (= :ready (:status ((api 'documents)))))
                  (is (= source (get-in ((api 'read) "wiki/Company.md") [:document :source])))))
              (is (= :ready (:status ((api 'documents)))) "Ordinary parent stays unchanged")
              (finally
                (ygg/discard-fork! a)
                (ygg/discard-fork! b)))))))))

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

(deftest captured-observation-search-and-read-retain-provenance-in-the-selected-world
  (with-room
    (fn [room]
      (let [path "captures/2026-10-25/session/observations/interval.md"
            id (random-uuid) image (random-uuid) video (random-uuid) session (random-uuid)
            text (str "---\nsimmis-id: " id "\nsource-kind: screen-observation\ncapture-session: \"" session
                      "\"\ntimestamp-basis: server-received\nrecording-alignment: estimated\n"
                      "screenshots: [{\"path\": \"captures/session/images/frame.jpg\", \"content\": \"" image "\", \"size\": 32, \"at\": 1100}]\n"
                      "recording: {\"path\": \"captures/session/recordings/interval.mp4\", \"content\": \"" video "\", \"size\": 64, \"offset-ms\": 10}\n"
                      "---\n# Screen observations\n\nEstimated recording position: 90 ms.\n\n```text\nThe delivery date remains unconfirmed.\n```\n")
            _ (files/save-file! room {:path path :text text :expected nil})
            allowed? (atom true)
            api (install! room {:authorize! (fn [_] (when-not @allowed? (throw (ex-info "Revoked" {}))))})
            read (api 'read) search (api 'search)
            result (search "delivery date remains unconfirmed")
            hit (first (:matches result))
            document (:document (read (:path hit)))]
        (is (= :ready (:status result)))
        (is (= path (:path hit)))
        (is (= id (:id hit)))
        (is (= (:content hit) (:content document)))
        (is (uuid? (:content hit)))
        (is (= text (:source document)))
        (is (= (str session) (get-in document [:properties :raw "capture-session"])))
        (is (= (str image) (get (first (get-in document [:properties :raw "screenshots"])) "content")))
        (is (= 32 (get (first (get-in document [:properties :raw "screenshots"])) "size")))
        (is (= (str video) (get-in document [:properties :raw "recording" "content"])))
        (is (= "estimated" (get-in document [:properties :raw "recording-alignment"])))
        (is (nil? (:db document)))
        (let [fork (ygg/fork!)]
          (try
            (binding [ec/*execution-context* (:child-ctx fork)]
              (repo/write! (:conn (geschichte/current-workspace)) path (.getBytes (str text "\nChild-only hypothesis.\n") "UTF-8"))
              (is (= path (get-in (search "Child-only hypothesis") [:matches 0 :path])))
              (is (not= (:content document) (get-in (read path) [:document :content]))))
            (is (empty? (:matches (search "Child-only hypothesis"))))
            (is (= text (get-in (read path) [:document :source])))
            (finally (ygg/discard-fork! fork))))
        (reset! allowed? false)
        (is (thrown? Exception (search "delivery date")))
        (is (thrown? Exception (read path)))))))
