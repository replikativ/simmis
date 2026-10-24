(ns is.simm.model.workspace-intent-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.db :as value-db]
            [dvergr.chat.schema :as chat-schema]
            [dvergr.system.db :as registry]
            [is.simm.documents.space :as space]
            [is.simm.model.system-db :as system-db]
            [is.simm.model.workspace-intent :as intent]))

(defn- fixture []
  (let [owner (random-uuid) other (random-uuid) repo (random-uuid) room (random-uuid)
        request (intent/request! {:id (str (random-uuid)) :name "Research" :members []})
        schema (vals (into {} (map (juxt :db/ident identity))
                           (concat registry/schema chat-schema/actor-schema system-db/schema)))
        empty (value-db/empty-db schema {:schema-flexibility :write})
        initial (d/db-with empty [{:party/id owner :party/type :human}
                                  {:party/id other :party/type :human}])
        pending (d/db-with initial [{:system/id repo :system/type :repo :system/name "workspace"
                                    :system/scope "/virtual/repo" :system/repository-profile space/profile}
                                   {:room/id room :room/slug (intent/slug owner request) :room/name "Research"
                                    :room/type :project :room/owner [:party/id owner]
                                    :room/default-repository [:system/id repo]
                                    :room/provisioning-state :pending :room/provisioning-version 1}])]
    {:owner owner :other other :repo repo :room room :request request :initial initial :pending pending}))

(deftest stable-native-exclusion-is-distinct-from-request-identity
  (let [{:keys [owner other request]} (fixture)
        changed (assoc request :name "Another name")]
    (is (= (intent/slug owner request) (intent/slug owner changed)))
    (is (not= (intent/basis owner request) (intent/basis owner changed)))
    (is (not= (intent/slug owner request) (intent/slug other request)))
    (is (not= (intent/basis owner request) (intent/basis other request)))
    (is (= "Research" (:name (intent/request! {:id (str (:id request)) :name " Research " :members []}))))))

(deftest malformed-or-oversized-wire-requests-refuse-before-any-native-work
  (let [id (str (random-uuid))]
    (doseq [request [nil {} {:id id :name "" :members []} {:id id :name "   " :members []}
                     {:id id :name "Name" :members (vec (repeat 256 id))}
                     {:id id :name "Name" :members [nil]}
                     {:id id :name "Name" :members ["bad"]}
                     {:id id :name "Name" :members [] :creator id}
                     {:id (random-uuid) :name "Name" :members []}]]
      (is (thrown? clojure.lang.ExceptionInfo (intent/request! request))))))

(deftest observation-never-promotes-pending-or-prepared-native-resources
  (let [{:keys [owner request initial pending room]} (fixture)
        ready (d/db-with pending [[:db/add [:room/id room] :room/provisioning-state :ready]])]
    (is (= {:status :not-observed} (intent/observe initial owner request)))
    (is (= {:status :preparing} (intent/observe pending owner request)))
    (is (= {:status :prepared-not-published} (intent/observe ready owner request)))
    (is (= {:status :conflict} (intent/observe pending owner (assoc request :name "Changed"))))
    (is (nil? (:room/content-db-scope (d/entity ready [:room/id room]))))))

(deftest published-receipt-observes-exact-request-and-current-authority
  (let [{:keys [owner other request pending room]} (fixture)
        published (d/db-with pending [(merge {:room/id room :room/provisioning-state :ready
                                             :room/created-by owner :room/content-db-scope (random-uuid)
                                             :room/parties [[:party/id owner]]}
                                            (intent/receipt owner request))])]
    (is (= {:status :created :room {:room/id (str room) :room/name "Research" :room/type :project}}
           (intent/observe published owner request)))
    (is (= {:status :conflict} (intent/observe published owner (assoc request :members [other]))))
    (is (= {:status :not-observed} (intent/observe published other request)))
    (is (= {:status :unavailable}
           (intent/observe (d/db-with published [[:db/add [:room/id room] :room/archived-at (java.util.Date.)]]) owner request)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (intent/observe (d/db-with published [[:db/retractEntity [:party/id owner]]]) owner request)))))

(deftest membership-permission-is-a-current-snapshot-check
  (let [{:keys [owner other request initial]} (fixture)
        agent (random-uuid)
        db (d/db-with initial [{:party/id agent :party/type :agent :party/owner [:party/id owner]}])
        requested (assoc request :members [agent])]
    (is (= requested (intent/authorize! db owner requested)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (intent/authorize! (d/db-with db [[:db/add [:party/id agent] :party/owner [:party/id other]]]) owner requested)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (intent/authorize! (d/db-with db [[:db/retractEntity [:party/id agent]]]) owner requested)))
    (is (thrown? clojure.lang.ExceptionInfo (intent/authorize! db agent request)))))
