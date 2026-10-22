(ns is.simm.model.personal-rooms-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.db :as value-db]
            [dvergr.system.db :as registry]
            [is.simm.agents.templates :as templates]
            [is.simm.documents.space :as space]
            [is.simm.model.knowledge-rooms :as knowledge]
            [is.simm.model.parties :as parties]
            [is.simm.model.personal-rooms :as personal]
            [is.simm.model.rooms :as rooms]
            [is.simm.model.system-db :as system-db]
            [is.simm.starter.wiki-maintenance :as wiki]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- fixture []
  (let [owner (random-uuid) other (random-uuid)
        schema (vals (into {} (map (juxt :db/ident identity)) (concat registry/schema system-db/schema)))
        db (d/db-with (value-db/empty-db schema {:schema-flexibility :write})
                      [{:party/id owner :party/type :human} {:party/id other :party/type :human}])]
    {:db db :owner owner :other other}))

(defn- room-row [owner]
  {:room/id (random-uuid) :room/type :personal-ai :room/owner [:party/id owner]
   :room/content-db-scope (random-uuid) :room/parties [[:party/id owner]]})

(defn- existing-assistant [owner]
  {:party/id (random-uuid) :party/template (:id templates/secretary-template)
   :party/owner {:party/id owner}})

(deftest shared-membership-is-not-the-default-owners-identity
  (let [{:keys [db owner other]} (fixture)
        shared (assoc (room-row other) :room/parties [[:party/id other] [:party/id owner]])
        conn (atom (d/db-with db [shared])) created (atom []) own-room {:room/id (random-uuid)}]
    (with-redefs [system-db/get-conn (constantly conn)
                  knowledge/create-personal! (fn [by name] (swap! created conj [by name]) own-room)
                  rooms/get-room (fn [& _] (throw (AssertionError. "Do not adopt another owner's personal room")))
                  rooms/get-room-agents (constantly [(existing-assistant owner)])]
      (binding [ec/*execution-context* nil]
        (is (= own-room (personal/ensure! owner "Ada"))))
      (is (= [[owner "Ada's Assistants"]] @created)))))

(deftest existing-owned-legacy-room-and-custom-workflow-are-preserved
  (let [{:keys [db owner]} (fixture) row (room-row owner) conn (atom (d/db-with db [row]))
        before @conn]
    (with-redefs [system-db/get-conn (constantly conn)
                  rooms/get-room (fn [id] (is (= (:room/id row) id)) row)
                  rooms/get-room-agents (constantly [(existing-assistant owner)])
                  knowledge/create-personal! (fn [& _] (throw (AssertionError. "No existing-room conversion")))
                  wiki/source (fn [] (throw (AssertionError. "No default workflow reinstall")))
                  parties/create-agent! (fn [& _] (throw (AssertionError. "No duplicate owned assistant")))]
      (binding [ec/*execution-context* nil]
        (is (= row (personal/ensure! owner "Ada"))))
      (is (identical? before @conn)))))

(deftest pending-or-ambiguous-owned-rooms-never-trigger-another-creation
  (let [{:keys [db owner]} (fixture)
        row (room-row owner)]
    (doseq [rows [[(assoc row :room/provisioning-state :pending)]
                  [(dissoc row :room/content-db-scope)]
                  [row (room-row owner)]
                  [(assoc row :room/provisioning-state :ready :room/provisioning-version 1)]]]
      (with-redefs [system-db/get-conn (constantly (atom (d/db-with db rows)))
                    knowledge/create-personal! (fn [& _] (throw (AssertionError. "No retry around pending identity")))
                    rooms/get-room-agents (fn [& _] (throw (AssertionError. "No agent before readiness")))]
        (binding [ec/*execution-context* nil]
          (is (thrown? clojure.lang.ExceptionInfo (personal/ensure! owner "Ada"))))))))

(deftest only-the-requested-owners-assistant-satisfies-the-default
  (let [{:keys [db owner other]} (fixture) row (room-row owner)
        conn (atom (d/db-with db [row])) calls (atom []) agent (random-uuid)]
    (with-redefs [system-db/get-conn (constantly conn) rooms/get-room (constantly row)
                  rooms/get-room-agents (constantly [(existing-assistant other)])
                  parties/create-agent! (fn [by _] (swap! calls conj [:create by]) {:party/id agent})
                  rooms/add-party! (fn [rid aid] (swap! calls conj [:add rid aid]))]
      (binding [ec/*execution-context* nil] (personal/ensure! owner "Ada"))
      (is (= [[:create owner] [:add (:room/id row) agent]] @calls)))))

(deftest canonical-readiness-requires-the-owned-profile-and-final-membership
  (let [{:keys [db owner]} (fixture) row (room-row owner) repo (random-uuid)
        canonical (assoc row :room/provisioning-state :ready :room/provisioning-version 1
                         :room/created-by owner :room/default-repository [:system/id repo])
        db (d/db-with db [{:system/id repo :system/type :repo :system/repository-profile space/profile} canonical])]
    (is (= (:room/id row) (@#'personal/existing-id db owner)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (@#'personal/existing-id (d/db-with db [[:db/add [:system/id repo] :system/repository-profile :other]]) owner)))))

(deftest partial-canonical-markers-do-not-become-an-apparently-ready-legacy-room
  (let [{:keys [db owner]} (fixture) row (room-row owner) repo (random-uuid)]
    (doseq [tx [[(assoc row :room/provisioning-version 1)]
                [{:system/id repo :system/type :repo :system/repository-profile space/profile}
                 (assoc row :room/default-repository [:system/id repo])]
                [{:system/id repo :system/type :repo :system/repository-profile :other}
                 (assoc row :room/default-repository [:system/id repo])]]]
      (is (thrown? clojure.lang.ExceptionInfo (@#'personal/existing-id (d/db-with db tx) owner))))))
