(ns is.simm.agents.memory-policy-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.runtime.ctx :as runtime-ctx]
            [dvergr.substrate.geschichte :as workspace]
            [dvergr.tools :as tools]
            [dvergr.system.db :as registry]
            [is.simm.agents.room-agents]
            [is.simm.agents.source-memory :as source]
            [is.simm.agents.source-memory-test :as fixture]
            [is.simm.documents.files :as files]
            [is.simm.model.access-test :as authority]
            [is.simm.model.system-db :as system-db]
            [is.simm.runtimes.branching :as branching]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg]
            [yggdrasil.protocols :as systems]))

(defn- policy-var [name] (ns-resolve 'is.simm.agents.room-agents name))

(defn- api! [room scope]
  (let [cap (random-uuid) owner (:ctx room)
        actor (keyword "party" (str authority/room-mate))]
    (runtime-ctx/install-sandbox-binding!
     owner cap {:agent-id actor :room-runtime-id (:id room)
                :simmis/source-memory
                {:room-id authority/room-id :agent-id actor :scope scope
                 :system-id (systems/system-id (workspace/current-system))}})
    (source/bindings owner cap nil
                     {:authorize! @(policy-var 'authorize-source-memory!)
                      :authorize-write! @(policy-var 'authorize-source-memory-write!)
                      :proposal-workspace @(policy-var 'existing-source-proposal-workspace)
                      :proposal-workspace! @(policy-var 'source-proposal-workspace!)
                      :write-route-guard @(policy-var 'source-write-route-guard)
                      :options {:schema-revision :memory-policy-test-v1}})))

(defn- entry [path] (files/entry @(:conn (workspace/current-workspace)) path))

(defn- with-authority [permission f]
  (let [conn (#'authority/test-db) scope (random-uuid)]
    (try
      (d/transact conn [{:db/id "repo" :system/id scope :system/type :repo}
                       {:grant/room [:room/id authority/room-id]
                        :grant/system "repo" :grant/permission permission}])
      (with-redefs-fn {#'system-db/get-conn (constantly conn)
                      (policy-var 'room-repo-scope) (constantly scope)
                      (policy-var 'room-campaigns) (atom {})
                      (policy-var 'proposal-overlays) (atom {})}
        #(f scope))
      (finally (d/release conn)))))

(deftest read-grant-can-read-but-cannot-mint-a-write-proposal
  (with-authority :read
    (fn [scope]
      (#'fixture/with-room
       (fn [room]
         (files/save-file! room {:path "a.md" :expected nil :text "existing"})
         (let [minted (atom 0)]
           (with-redefs-fn {(policy-var 'active-overlay) (constantly nil)
                           (policy-var 'repo-fork!) (fn [& _] (swap! minted inc))}
             (fn []
               (let [api (api! room scope)]
                 (is (= "existing" (get-in ((api 'read) "a.md") [:document :source])))
                 (is (thrown? clojure.lang.ExceptionInfo
                              ((api 'write!) {:path "b.md" :expected nil :text "denied"})))
                 (is (zero? @minted))
                 (is (nil? (entry "b.md"))))))))))))

(deftest closed-cached-proposal-refuses-owner-but-does-not-entangle-child
  (with-authority :read-write
    (fn [scope]
      (#'fixture/with-room
       (fn [room]
         (let [fork (ygg/fork!) calls (atom 0)
               overlay (atom {:closed true
                              :ctx-forks {[scope authority/room-mate] fork}})]
           (try
             (with-redefs-fn {(policy-var 'active-overlay)
                             (fn [& _] (swap! calls inc) overlay)
                             (policy-var 'proposal-overlays)
                             (atom {[authority/room-id authority/room-mate] overlay})
                             (policy-var 'existing-source-proposal-workspace) (constantly nil)}
               (fn []
                 (let [api (api! room scope)]
                   (is (thrown-with-msg? clojure.lang.ExceptionInfo #"closed"
                                        ((api 'write!) {:path "a.md" :expected nil :text "denied"})))
                   (is (nil? (entry "a.md")))
                   (reset! calls 0)
                   (binding [ec/*execution-context* (:child-ctx fork)]
                     (is (= :written (:status ((api 'write!) {:path "a.md" :expected nil :text "child"}))))
                     (is (= "child" (get-in ((api 'read) "a.md") [:document :source]))))
                   (is (zero? @calls) "Child never consults the host proposal registry")
                   (is (nil? (entry "a.md"))))))
             (finally (ygg/discard-fork! fork)))))))))

(deftest a-proposal-started-after-selection-cannot-write-the-old-trunk-route
  (with-authority :read-write
    (fn [scope]
      (#'fixture/with-room
       (fn [room]
         (let [api (api! room scope)
               native tools/workspace-write!
               registry @(policy-var 'proposal-overlays)]
           (with-redefs [tools/workspace-write!
                         (fn [& args]
                           (swap! registry assoc [authority/room-id authority/room-mate]
                                  (atom {:title "New intent" :forks {}}))
                           (apply native args))]
             (is (thrown-with-msg? clojure.lang.ExceptionInfo #"routing changed"
                                  ((api 'write!) {:path "a.md" :expected nil :text "wrong route"})))
             (is (nil? (entry "a.md"))))))))))

(deftest source-default-is-explicit-not-the-first-writable-grant
  (let [conn (#'authority/test-db)
        earlier #uuid "00000000-0000-0000-0000-000000000001"
        chosen #uuid "00000000-0000-0000-0000-000000000002"]
    (try
      (d/transact conn [{:db/ident :room/default-repository :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
                       {:db/ident :system/name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
                       {:db/ident :system/repository-profile :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
                       {:db/ident :system/scope :db/valueType :db.type/string :db/cardinality :db.cardinality/one}])
      (d/transact conn [{:db/id "earlier" :system/id earlier :system/type :repo :system/scope "/test/earlier"}
                       {:db/id "chosen" :system/id chosen :system/type :repo :system/scope "/test/chosen"}
                       {:db/id "early-grant" :grant/room [:room/id authority/room-id]
                        :grant/system "earlier" :grant/permission :owner}
                       {:db/id "chosen-grant" :grant/room [:room/id authority/room-id]
                        :grant/system "chosen" :grant/permission :owner}
                       {:room/id authority/room-id :room/default-repository "chosen"}])
      (with-redefs [registry/get-conn (constantly conn)]
        (is (= chosen (@(policy-var 'room-repo-scope) authority/room-id)))
        (d/transact conn [[:db/retract [:room/id authority/room-id] :room/default-repository [:system/id chosen]]])
        (is (nil? (@(policy-var 'room-repo-scope) authority/room-id)))
        (d/transact conn [{:room/id authority/room-id :room/default-repository [:system/id chosen]}])
        (let [grant (d/q '[:find ?g . :in $ ?scope :where [?s :system/id ?scope] [?g :grant/system ?s]] @conn chosen)]
          (d/transact conn [[:db/retractEntity grant]]))
        (is (nil? (@(policy-var 'room-repo-scope) authority/room-id))
            "An attached alternate owner repo is never substituted"))
      (finally (d/release conn)))))

(deftest a-default-retarget-before-dispatch-cannot-write-the-old-memory
  (with-authority :read-write
    (fn [scope]
      (#'fixture/with-room
       (fn [room]
         (let [selected (atom scope) native tools/workspace-write!]
           (with-redefs-fn {(policy-var 'room-repo-scope) (fn [_] @selected)}
             (fn []
               (let [api (api! room scope)]
                 (with-redefs [tools/workspace-write!
                               (fn [& args] (reset! selected (random-uuid)) (apply native args))]
                   (is (thrown-with-msg? clojure.lang.ExceptionInfo #"target changed"
                                        ((api 'write!) {:path "a.md" :expected nil :text "old memory"}))))
                 (is (nil? (entry "a.md"))))))))))))

(deftest releasing-the-selected-proposal-before-dispatch-refuses-the-old-workspace
  (with-authority :read-write
    (fn [scope]
      (#'fixture/with-room
       (fn [room]
         (let [fork (ygg/fork!)
               key [scope authority/room-mate]
               overlay (atom {:title "Selected" :forks {key "old"} :ctx-forks {key fork}})
               native tools/workspace-write!]
           (try
             (with-redefs-fn
               {(policy-var 'proposal-overlays) (atom {[authority/room-id authority/room-mate] overlay})
                #'branching/repo-fork-workspace
                (fn [selected]
                  (binding [ec/*execution-context* (:child-ctx selected)]
                    {:workspace (workspace/current-workspace)}))}
               (fn []
                 (let [api (api! room scope)]
                   (with-redefs [tools/workspace-write!
                                 (fn [& args] (reset! overlay nil) (apply native args))]
                     (is (thrown-with-msg? clojure.lang.ExceptionInfo #"routing changed"
                                          ((api 'write!) {:path "a.md" :expected nil :text "old proposal"}))))
                   (is (nil? (entry "a.md")))
                   (binding [ec/*execution-context* (:child-ctx fork)]
                     (is (nil? (entry "a.md")))))))
             (finally (ygg/discard-fork! fork)))))))))
