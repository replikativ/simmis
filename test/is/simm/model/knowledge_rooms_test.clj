(ns is.simm.model.knowledge-rooms-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.db :as value-db]
            [dvergr.chat.schema :as chat-schema]
            [dvergr.system.db :as registry]
            [dvergr.system.room-intent :as intent]
            [dvergr.system.rooms :as native]
            [dvergr.room.registry :as live-registry]
            [is.simm.agents.room-agents :as agents]
            [is.simm.documents.space :as space]
            [is.simm.documents.starter-bundle :as starter]
            [is.simm.model.knowledge-rooms :as rooms]
            [is.simm.model.rooms :as legacy]
            [is.simm.model.system-db :as system-db]
            [is.simm.model.workspace-intent :as creation]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- fixture []
  (let [creator (random-uuid) member (random-uuid) id (random-uuid)
        systems (mapv (fn [type]
                        (cond-> {:system/id (random-uuid) :system/type type
                                 :system/name (str "fixture-" (name type))
                                 :system/scope (str "/virtual/" (name type))
                                 :system/owner [:party/id creator]}
                          (= :repo type) (assoc :system/repository-profile space/profile))) [:repo :msgs :kb])
        by-type (into {} (map (juxt :system/type identity)) systems)
        schema (vals (into {} (map (juxt :db/ident identity))
                           (concat registry/schema chat-schema/actor-schema system-db/schema)))
        db (d/db-with (value-db/empty-db schema {:schema-flexibility :write})
                      (into [{:party/id creator :party/type :human} {:party/id member :party/type :human}]
                            (concat systems
                                    [{:room/id id :room/slug "fixture" :room/name "Fixture" :room/type :project
                                      :room/owner [:party/id creator] :room/provisioning-state :ready
                                      :room/provisioning-version 1
                                      :room/default-repository [:system/id (get-in by-type [:repo :system/id])]}]
                                    (map (fn [system]
                                           {:grant/id (random-uuid) :grant/room [:room/id id]
                                            :grant/system [:system/id (:system/id system)] :grant/permission :owner}) systems))))
        provisioned (reduce (fn [out [type id-key path-key]]
                              (assoc out id-key (get-in by-type [type :system/id])
                                     path-key (get-in by-type [type :system/scope])))
                            {:room-id id} [[:repo :repo-id :repo-path] [:msgs :msgs-id :msgs-path] [:kb :kb-id :kb-path]])]
    {:db db :creator creator :member member :id id :provisioned provisioned :systems by-type}))

(deftest final-publication-is-one-pure-control-transaction
  (let [{:keys [db creator member id provisioned]} (fixture)
        expected (@#'rooms/prepared-basis! db provisioned creator) scope (random-uuid)]
    (with-redefs [native/store-id (fn [& _] (throw (AssertionError. "No native IO in transaction")))]
      (let [after (d/db-with db [[:db.fn/call @#'rooms/publish-tx expected creator #{creator member} scope]])
            row (d/pull after [:room/created-by :room/content-db-scope {:room/parties [:party/id]}] [:room/id id])]
        (is (= creator (:room/created-by row)))
        (is (= scope (:room/content-db-scope row)))
        (is (= #{creator member} (set (map :party/id (:room/parties row)))))
        (is (nil? (:room/content-db-scope (d/pull db [:room/content-db-scope] [:room/id id]))))
        (is (thrown? clojure.lang.ExceptionInfo
                     (@#'rooms/publish-tx after expected creator #{creator member} scope)))))))

(deftest requested-personal-type-is-a-precondition-not-derived-from-the-row
  (let [{:keys [db creator id provisioned]} (fixture)
        personal (d/db-with db [[:db/add [:room/id id] :room/type :personal-ai]])
        expected (@#'rooms/prepared-basis! personal provisioned creator :personal-ai)
        scope (random-uuid)]
    (is (thrown? clojure.lang.ExceptionInfo (@#'rooms/prepared-basis! personal provisioned creator :project)))
    (is (thrown? clojure.lang.ExceptionInfo (@#'rooms/publish-tx personal expected creator #{creator} scope :project)))
    (let [after (d/db-with personal [[:db.fn/call @#'rooms/publish-tx expected creator #{creator} scope :personal-ai]])]
      (is (= :personal-ai (:room/type (d/entity after [:room/id id]))))
      (is (= creator (:room/created-by (d/entity after [:room/id id])))))))

(deftest stale-preparation-and-membership-are-refused-before-visibility
  (let [{:keys [db creator member id provisioned systems]} (fixture)
        expected (@#'rooms/prepared-basis! db provisioned creator)
        grant (:grant/id (first (:attachments expected)))
        scope (random-uuid)]
    (doseq [tx [[[:db/add [:room/id id] :room/owner [:party/id member]]]
                [[:db/add [:room/id id] :room/provisioning-state :pending]]
                [[:db/add [:room/id id] :room/archived-at (java.util.Date.)]]
                [[:db/add [:system/id (get-in systems [:repo :system/id])] :system/repository-profile :foreign/profile]]
                [[:db/add [:system/id (get-in systems [:msgs :system/id])] :system/scope "/virtual/foreign"]]
                [[:db/add [:grant/id grant] :grant/permission :read]]
                [[:db/add [:room/id id] :room/parties [:party/id member]]]
                [[:db/retractEntity [:party/id member]]]]]
      (let [changed (d/db-with db tx)]
        (is (thrown? clojure.lang.ExceptionInfo
                     (@#'rooms/publish-tx changed expected creator #{creator member} scope)))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (@#'rooms/prepared-basis! db (assoc provisioned :repo-id (random-uuid)) creator)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (@#'rooms/prepared-basis! db (assoc provisioned :msgs-path "/virtual/foreign") creator)))))

(deftest invalid-members-never-acquire-a-bundle-or-record-intent
  (let [{:keys [db creator member]} (fixture)]
    (with-redefs [system-db/get-conn (constantly (atom db))
                  starter/read-packaged! (fn [] (throw (AssertionError. "No bundle before member validation")))
                  native/provision-profiled-room! (fn [& _] (throw (AssertionError. "No invalid intent")))]
      (doseq [members [[(random-uuid)] [nil] (repeat 257 member)]]
        (is (thrown? clojure.lang.ExceptionInfo (rooms/create! creator "Fixture" members)))))))

(deftest already-selected-child-cannot-be-promoted-to-server-root
  (with-redefs [system-db/get-conn (fn [] (throw (AssertionError. "No acquisition before host validation")))]
    (binding [ec/*execution-context* {:parent-ctx {}}]
      (is (= :requires-host-root
             (try (rooms/create! (random-uuid) "Child" [])
                  (catch clojure.lang.ExceptionInfo error (:reason (ex-data error)))))))))

(deftest quarantined-native-owner-is-not-bypassed-by-a-cached-room
  (let [{:keys [db id]} (fixture) failure (ex-info "Constructor ownership uncertain" {})]
    (with-redefs [legacy/get-room (constantly {:room/slug "fixture"})
                  registry/get-conn (constantly (atom db))
                  native/room-ctx-for (fn [_] (throw failure))
                  live-registry/lookup (fn [_] (throw (AssertionError. "Do not bypass owner gate")))]
      (is (identical? failure (try (agents/registered-room id)
                                   (catch Throwable error error))))
      (is (identical? failure (try (agents/live-room id)
                                   (catch Throwable error error)))))))

(deftest explicit-creation-receipt-and-membership-publish-in-one-transaction
  (let [{:keys [db creator member id provisioned]} (fixture)
        request (creation/request! {:id (str (random-uuid)) :name "Fixture" :members [(str member)]})
        prepared (d/db-with db [[:db/add [:room/id id] :room/slug (creation/slug creator request)]])
        expected (@#'rooms/prepared-basis! prepared provisioned creator)
        scope (random-uuid)
        tx [[:db.fn/call @#'rooms/publish-tx expected creator #{creator member} scope :project request]]
        after (d/db-with prepared tx)]
    (is (= (creation/receipt creator request)
           (dissoc (d/pull after [:room/creation-intent :room/creation-basis] [:room/id id]) :db/id)))
    (is (= {:status :created :room {:room/id (str id) :room/name "Fixture" :room/type :project}}
           (creation/observe after creator request)))
    (is (nil? (:room/creation-intent (d/entity prepared [:room/id id]))))
    (is (nil? (:room/content-db-scope (d/entity prepared [:room/id id]))))
    (is (thrown? clojure.lang.ExceptionInfo (d/db-with after tx)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (d/db-with (d/db-with prepared [[:db/add [:party/id creator] :party/type :agent]]) tx)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (@#'rooms/publish-tx prepared expected creator #{creator member} scope :project
                  (assoc request :name "Changed"))))))

(deftest final-publication-rechecks-agent-ownership-after-native-preparation
  (let [{:keys [db creator member id provisioned]} (fixture)
        agent (random-uuid)
        request (creation/request! {:id (str (random-uuid)) :name "Fixture" :members [(str agent)]})
        prepared (d/db-with db [{:party/id agent :party/type :agent :party/owner [:party/id creator]}
                                [:db/add [:room/id id] :room/slug (creation/slug creator request)]])
        expected (@#'rooms/prepared-basis! prepared provisioned creator)
        tx [[:db.fn/call @#'rooms/publish-tx expected creator #{creator agent} (random-uuid) :project request]]]
    (is (= :created (:status (creation/observe (d/db-with prepared tx) creator request))))
    (doseq [change [[[:db/add [:party/id agent] :party/owner [:party/id member]]]
                    [[:db/retractEntity [:party/id agent]]]
                    [[:db/retractEntity [:party/id creator]]]]]
      (is (thrown? clojure.lang.ExceptionInfo (d/db-with (d/db-with prepared change) tx))))))

(deftest github-publication-requires-exact-pre-network-receipt-and-host-source-proof
  (let [{:keys [db creator id provisioned]} (fixture)
        request (creation/request! {:id (str (random-uuid)) :name "Fixture" :members []
                                    :source {:kind :github :repository "owner/repo"}})
        pending (d/db-with db [[:db/add [:room/id id] :room/slug (creation/slug creator request)]
                               [:db/add [:room/id id] :room/provisioning-state :pending]])
        recorded (d/db-with pending (creation/record-source-tx pending creator request))
        ready (d/db-with recorded [[:db/add [:room/id id] :room/provisioning-state :ready]])
        expected (@#'rooms/prepared-basis! ready provisioned creator)
        proof {:repository "owner/repo" :ref "refs/heads/main" :head (random-uuid)
               :registry-system (:repo-id provisioned)}
        scope (random-uuid)
        publish (fn [snapshot request proof]
                  (@#'rooms/publish-tx snapshot expected creator #{creator} scope :project request proof))
        after (d/db-with ready (publish ready request proof))]
    (is (= :created (:status (creation/observe after creator request))))
    (is (= "refs/heads/main" (:room/github-ref (d/entity after [:room/id id]))))
    (is (= (:head proof) (:room/github-import-head (d/entity after [:room/id id]))))
    (is (nil? (:room/content-db-scope (d/entity ready [:room/id id]))))
    (doseq [changed [nil (assoc proof :repository "other/repo") (assoc proof :head nil)
                     (assoc proof :registry-system (random-uuid))
                     (assoc proof :ref "refs/heads/main\n") (assoc proof :extra true)]]
      (is (thrown? clojure.lang.ExceptionInfo (publish ready request changed))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (publish (d/db-with ready [[:db/add [:room/id id] :room/github-source "other/repo"]]) request proof)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (publish (d/db-with ready [[:db/retract [:room/id id] :room/creation-basis
                                            (:room/creation-basis (d/entity ready [:room/id id]))]]) request proof)))
    (is (thrown? clojure.lang.ExceptionInfo (publish ready (dissoc request :source) nil)))))
