(ns is.simm.model.knowledge-rooms-integration-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.discourse :as discourse]
            [dvergr.rooms :as live]
            [dvergr.room.registry :as registry]
            [dvergr.agent.room-context :as working]
            [dvergr.chat.context :as chat]
            [dvergr.chat.schema :as chat-schema]
            [dvergr.substrate.geschichte :as repository]
            [dvergr.substrate.paths :as paths]
            [dvergr.system.db :as control]
            [dvergr.system.rooms :as native]
            [geschichte.repo :as repo]
            [is.simm.documents.profile :as profile]
            [is.simm.documents.starter-bundle :as starter]
            [is.simm.documents.view :as view]
            [is.simm.documents.files :as files]
            [is.simm.model.knowledge-rooms :as rooms]
            [is.simm.model.personal-rooms :as personal]
            [is.simm.model.rooms :as room-model]
            [is.simm.model.store :as store]
            [is.simm.model.system-db :as system-db]
            [is.simm.runtimes.context :as server]
            [is.simm.starter.wiki-maintenance :as wiki]
            [kontor.governance :as governance]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg]
            [sci.core :as sci]))

;; All stores and native runtime owners are private to this fixture. A failure
;; to join runtime work or release stores preserves the exact ownership record;
;; never delete resources while their physical lifetime remains uncertain.
(defonce ^:private held-fixture (atom nil))

(defn- config []
  {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
   :keep-history? false :attribute-refs? false :crypto-hash? true :commit-graph? true})

(defn- fixture [f]
  (when @held-fixture (throw (ex-info "Previous native knowledge fixture needs reconciliation" {})))
  (let [cfg (config) configs (atom {}) connection (atom nil) root (context/create-execution-context)
        contexts (atom {}) errors (atom {}) released? (atom false)
        owner {:cfg cfg :configs configs :connection connection :root root :contexts contexts :errors errors
               :released? released?}
        config-for (fn [scope] (get (swap! configs #(if (contains? % scope) % (assoc % scope (config)))) scope))]
    (try
      (d/create-database cfg)
      (let [conn (d/connect cfg)]
        (reset! connection conn)
        (with-redefs-fn
          {#'control/get-conn (constantly conn) #'system-db/get-conn (constantly conn)
           #'server/server-context root #'paths/systems-dir (constantly "/virtual/knowledge-test")
           #'repository/repository-config config-for #'native/msgs-cfg config-for #'native/kb-cfg config-for
           #'native/store-id #(get-in (config-for %) [:store :id])
           #'native/room-ctxs contexts #'native/room-hydration-errors errors}
          #(binding [ec/*execution-context* root]
             (try
               (d/transact conn (vec (vals (into {} (map (juxt :db/ident identity))
                                                 (concat control/schema system-db/schema)))))
               ;; Match the owning production system-db initialization: actor
               ;; lookups and actual assistant creation require the actor core,
               ;; not merely the Simmis extension and native registry schema.
               (chat-schema/ensure-full-schema! conn)
               (f {:conn conn :root root :errors errors})
               (finally
                 (when (seq @errors)
                   (throw (ex-info "Uncertain preparation needs explicit fixture reconciliation" {})))
                 (doseq [room (registry/list-rooms)] (discourse/close-room! room {:close-context? false}))
                 ;; Retire the exact published acquisitions through the native
                 ;; shutdown owner while the host root is still available.
                 ;; Fixture cleanup must obey the production lifetime contract.
                 (doseq [store-cfg (vals @configs) :when (d/database-exists? store-cfg)]
                   (let [opened (d/connect store-cfg)]
                     (governance/ungovern! opened)
                     (d/release opened)))
                 (native/release-room-stores!)
                 (when (or (seq @contexts) (seq @errors))
                   (throw (ex-info "Native fixture retirement is incomplete" {})))
                 (context/close-context! root)
                 (reset! released? true))))))
      (finally
        (if-not @released?
          (reset! held-fixture owner)
          (try
            (doseq [store-cfg (vals @configs) :when (d/database-exists? store-cfg)]
              (d/delete-database store-cfg))
            (d/release @connection)
            (d/delete-database cfg)
            (catch Throwable error (reset! held-fixture owner) (throw error))))))))

(deftest real-creation-keeps-files-properties-messages-and-membership-coherent
  (fixture
   (fn [{:keys [conn root errors]}]
     (let [creator (random-uuid) member (random-uuid)]
       (d/transact conn [{:party/id creator} {:party/id member}])
       (let [room (rooms/create! creator "Knowledge integration" [member]) id (:room/id room)
             ctx (native/room-ctx-for id) registered (first (registry/list-rooms))]
         (is (= #{creator member} (:room/parties room)))
         (is (= creator (:room/created-by room)))
         (is (= (:room/content-db-scope room) (native/room-msgs-store-id id)))
         (is (identical? ctx (:ctx registered)))
         (binding [ec/*execution-context* ctx]
           (let [source (:conn (repository/current-system))
                 prepared (view/prepare @source profile/selection-options)
                 messages (native/room-msgs-conn id)]
             (is (not (identical? source messages)))
             (is (= :ready (:status prepared)))
             (is (= 0 (d/q '[:find ?value . :where [_ :S.Company/revenue ?value]] (:db prepared))))
             (is (true? (:clean? (repo/status source))))
             (is (contains? (d/schema @messages) :S.Page/title))
              ;; Test the real agent interpreter, not the intentionally smaller
              ;; human REPL. Creating its working context makes no model call.
             (let [cctx (working/ensure-ctx! registered :fixture/starter {:budget-dollars 0.0 :sandbox #{}})]
               (is (= true (sci/eval-string* (chat/sci-context-in cctx ctx)
                                             "(require 'dvergr.intake.core 'dvergr.mail.inbox) (fn? dvergr.intake.core/response-provenance)"))))
             (is (true? (native/canonical-messages-identity? @messages (:slug registered))))))
         (native/hydrate-rooms! {:repository-profiles (profile/policies (starter/read-packaged!))})
         (is (identical? ctx (native/room-ctx-for id)))
         (is (identical? registered (first (registry/list-rooms))))
          ;; A real cold native reopen, not the healthy-context fast path.
          ;; Join the actual agent interpreter/Room first, release our exclusive
          ;; fixture stores with propagated errors, then drop the private index.
         (try
           (discourse/close-room! registered {:close-context? false})
           (binding [ec/*execution-context* ctx]
             (doseq [conn (keep :conn (vals (ygg/registered-systems)))]
               (governance/ungovern! conn)))
           (native/release-room-stores!)
           (native/hydrate-rooms! {:repository-profiles (profile/policies (starter/read-packaged!))})
           (is (some? (native/room-ctx-for id)))
           (is (not (identical? ctx (native/room-ctx-for id))))
           (let [reopened (live/register-prepared-room! id)]
             (is (not (identical? registered reopened)))
             (is (identical? (:ctx reopened) (native/room-ctx-for id))))
           (catch Throwable error
              ;; A failed intermediate close/release may not be blindly retried
              ;; by the fixture's finalizer or followed by store deletion.
             (swap! errors #(if (contains? % id) % (assoc % id error)))
             (throw error)))
         (is (identical? root server/server-context)))))))

(deftest personal-birth-publishes-canonical-memory-and-an-inactive-ordinary-workflow
  (fixture
   (fn [{:keys [conn]}]
     (let [owner (random-uuid)]
       (d/transact conn [{:party/id owner :party/type :human}])
       (let [room (personal/ensure! owner "Ada") id (:room/id room)
             ctx (native/room-ctx-for id)
             source (binding [ec/*execution-context* ctx] (:conn (repository/current-system)))
             initial @source
             assistants (room-model/get-room-agents id)]
         (is (= :personal-ai (:room/type room)))
         (is (= (str "personal-" owner) (:room/slug room)))
         (is (= :ready (:room/provisioning-state (d/entity @conn [:room/id id]))))
         (is (= :ready (:status (view/prepare @source profile/selection-options))))
         (is (true? (:clean? (repo/status source))))
         (doseq [[path text] (wiki/source)]
           (is (= text (:text (files/read-file-in @source (str "workflows/" wiki/name "/" path))))))
         (is (= 1 (count assistants)))
         (is (= owner (get-in (first assistants) [:party/owner :party/id])))
         (is (empty? (d/datoms @conn :aevt :schedule.activation/id)))
         (is (empty? (d/datoms @conn :aevt :work.request/id)))
         (is (nil? (:room/budget-dollars (d/entity @conn [:room/id id]))))
         (is (= id (:room/id (personal/ensure! owner "Ada"))))
         (is (= assistants (room-model/get-room-agents id)))
         (is (= initial @source) "Repeated ensure does not reinstall or transact on wiki source"))))))

(deftest failed-messages-installation-never-publishes-membership
  (fixture
   (fn [{:keys [conn]}]
     (let [creator (random-uuid) failure (ex-info "Messages installer rejected" {})]
       (d/transact conn [{:party/id creator}])
       (let [error (with-redefs [store/ensure! (fn [& _] (throw failure))]
                     (try (rooms/create! creator "Withheld" []) nil (catch Throwable error error)))
             id (:room/id (ex-data error)) row (d/pull @conn '[*] [:room/id id])]
         (is (uuid? id))
         (is (identical? failure (.getCause error)))
         (is (= :ready (:room/provisioning-state row)))
         (is (nil? (:room/parties row)))
         (is (nil? (:room/content-db-scope row)))
         (is (empty? (registry/list-rooms))))))))

(deftest lost-final-ack-preserves-the-known-room-and-committed-membership
  (fixture
   (fn [{:keys [conn]}]
     (let [creator (random-uuid) transact d/transact failure (ex-info "Lost publication ACK" {})]
       (d/transact conn [{:party/id creator}])
       (let [error (with-redefs [d/transact
                                 (fn [target tx & more]
                                   (let [result (apply transact target tx more)]
                                     (when (and (vector? tx) (vector? (first tx))
                                                (identical? @#'rooms/publish-tx (second (first tx))))
                                       (throw failure))
                                     result))]
                     (try (rooms/create! creator "Committed without ACK" []) nil (catch Throwable error error)))
             id (:room/id (ex-data error))
             row (d/pull @conn [:room/created-by :room/content-db-scope {:room/parties [:party/id]}] [:room/id id])]
         (is (uuid? id))
         (is (identical? failure (.getCause error)))
         (is (= creator (:room/created-by row)))
         (is (= #{creator} (set (map :party/id (:room/parties row)))))
         (is (= 1 (count (registry/list-rooms))))
         (is (= (:room/content-db-scope row) (native/room-msgs-store-id id))))))))
