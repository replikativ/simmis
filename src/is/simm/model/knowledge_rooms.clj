(ns is.simm.model.knowledge-rooms
  "Explicit HOST creation of new file-backed knowledge project Rooms.
   Not a migration, transport endpoint or agent-facing provisioning capability."
  (:require [datahike.api :as d]
            [dvergr.rooms :as live]
            [dvergr.system.room-intent :as intent]
            [dvergr.system.rooms :as native]
            [is.simm.documents.profile :as profile]
            [is.simm.documents.space :as space]
            [is.simm.documents.starter-bundle :as starter]
            [is.simm.model.rooms :as rooms]
            [is.simm.model.store :as store]
            [is.simm.model.system-db :as system-db]
            [is.simm.model.workspace-intent :as creation]
            [is.simm.ops.github-import :as github]
            [is.simm.runtimes.context :as context]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.impl.simple :as simple]))

(defn- refuse! [reason]
  (throw (ex-info "Knowledge Room creation refused" {:type ::refused :reason reason})))

(defn- members! [snapshot creator members]
  (when-not (and (uuid? creator) (set? members) (contains? members creator)
                 (<= 1 (count members) 256)
                 (every? #(and (uuid? %) (d/entity snapshot [:party/id %])) members))
    (refuse! :invalid-members)))

(defn- publish-tx
  ([snapshot expected creator members scope]
   (publish-tx snapshot expected creator members scope :project))
  ([snapshot expected creator members scope requested-type]
   (publish-tx snapshot expected creator members scope requested-type nil))
  ([snapshot expected creator members scope requested-type request]
   (publish-tx snapshot expected creator members scope requested-type request nil))
  ([snapshot expected creator members scope requested-type request source-proof]
  ;; Native preparation is not a cross-store transaction. The final control
  ;; transaction rechecks its exact metadata/grants before exposing membership.
   (members! snapshot creator members)
   (when request
     (creation/authorize! snapshot creator request)
     (when-not (and (= :project requested-type) (= members (into #{creator} (:members request)))
                    (= (creation/slug creator request) (get-in expected [:room :room/slug]))
                    (= (:name request) (get-in expected [:room :room/name])))
       (refuse! :creation-request-changed)))
   (let [id (get-in expected [:room :room/id])
         actual (intent/basis snapshot id)
         msgs (filter #(= :msgs (get-in % [:grant/system :system/type])) (:attachments actual))]
     (when-not (and (= expected actual) (= :ready (get-in actual [:room :room/provisioning-state]))
                    (= creator (get-in actual [:room :room/owner :party/id]))
                    (= 1 (count msgs)) (= :owner (:grant/permission (first msgs)))
                    (uuid? scope) (#{:project :personal-ai} requested-type)
                    (= requested-type (get-in actual [:room :room/type]))
                    (nil? (:room/archived-at (d/pull snapshot [:room/archived-at] [:room/id id])))
                    (not-any? #(seq (d/datoms snapshot :eavt
                                              (:db/id (d/entity snapshot [:room/id id])) %))
                              [:room/content-db-scope :room/created-by :room/parties])
                    (if (:source request)
                      (let [row (d/pull snapshot [:room/creation-intent :room/creation-basis :room/github-source]
                                        [:room/id id])]
                        (and (= (creation/receipt creator request)
                                (select-keys row [:room/creation-intent :room/creation-basis]))
                             (= (get-in request [:source :repository]) (:room/github-source row))
                             (= #{:repository :ref :head :registry-system} (set (keys source-proof)))
                             (= (:room/github-source row) (:repository source-proof))
                             (= (:registry-system source-proof)
                                (get-in actual [:room :room/default-repository :system/id]))
                             (github/reference? (:ref source-proof)) (uuid? (:head source-proof))))
                      (and (nil? source-proof)
                           (not-any? #(seq (d/datoms snapshot :eavt
                                                    (:db/id (d/entity snapshot [:room/id id])) %))
                                     [:room/creation-intent :room/creation-basis :room/github-source]))))
       (refuse! :prepared-room-changed))
     [(cond-> (assoc {:room/id id :room/created-by creator :room/content-db-scope scope}
                     :room/parties (mapv #(vector :party/id %) (sort-by str members)))
        request (merge (creation/receipt creator request))
        source-proof (assoc :room/github-ref (:ref source-proof)
                            :room/github-import-head (:head source-proof)
                            :room/github-repository [:system/id (:registry-system source-proof)]))])))

(defn- prepared-basis!
  ([snapshot provisioned creator] (prepared-basis! snapshot provisioned creator :project))
  ([snapshot provisioned creator requested-type]
   (let [basis (intent/basis snapshot (:room-id provisioned))
         row (:room basis)
         systems (into {} (map (juxt #(get-in % [:grant/system :system/type]) :grant/system)) (:attachments basis))]
     (when-not (and (= :ready (:room/provisioning-state row)) (= 1 (:room/provisioning-version row))
                    (nil? (:room/archived-at (d/pull snapshot [:room/archived-at] [:room/id (:room-id provisioned)])))
                    (#{:project :personal-ai} requested-type) (= requested-type (:room/type row))
                    (= creator (get-in row [:room/owner :party/id]))
                    (= (:repo-id provisioned) (get-in row [:room/default-repository :system/id]))
                    (= 3 (count (:attachments basis))) (= #{:repo :msgs :kb} (set (keys systems)))
                    (every? #(= :owner (:grant/permission %)) (:attachments basis))
                    (= space/profile (get-in systems [:repo :system/repository-profile]))
                    (every? (fn [[type id-key path-key]]
                              (and (= (id-key provisioned) (get-in systems [type :system/id]))
                                   (= (path-key provisioned) (get-in systems [type :system/scope]))
                                   (= creator (get-in systems [type :system/owner :party/id]))))
                            [[:repo :repo-id :repo-path] [:msgs :msgs-id :msgs-path] [:kb :kb-id :kb-path]]))
       (refuse! :prepared-room-changed))
     basis)))

(defn- create-in!
  "HOST ONLY: new project with typed Markdown and the pinned sandbox sources.
   Members become visible only after native preparation, messages installation
   and live registration succeed. Failure preserves the known Room/data; it
   never deletes resources or automatically retries creation."
  [creator name party-ids requested-type slug request current!]
  ;; An ordinary unbound HOST may enter its configured root, but an already
  ;; selected child/foreign world must never be promoted by rebinding it.
  (when (or (simple/on-drain-thread?)
            (and (ec/execution-context-bound?)
                 (not (identical? context/server-context (ec/current-execution-context)))))
    (refuse! :requires-host-root))
  (let [conn (or (system-db/get-conn) (refuse! :system-unavailable))
        _ (current!)
        _ (when-not (and (string? name) (<= 1 (count name) 512)) (refuse! :invalid-name))
        _ (when-not (and (coll? party-ids) (<= (bounded-count 257 party-ids) 256))
            (refuse! :invalid-members))
        members (into #{creator} party-ids)
        _ (members! @conn creator members)
        _ (when request (creation/authorize! @conn creator request))
        ;; Acquire immutable reviewed source before recording any durable intent.
        policies (when-not (:source request) (profile/policies (starter/read-packaged!)))]
    (context/with-server-context
      (let [provision! #(native/provision-profiled-room!
                        {:slug slug :name name :owner-id creator :type requested-type
                         :repository-profile space/profile} %)
            {:keys [room-id room-ctx msgs-path] :as provisioned}
            (if (:source request)
              (github/with-admission!
               (fn []
                 (lifetime/call-detached
                  (fn [restore!]
                    (let [policies (restore! (fn [] (github/policies conn creator request #(restore! current!))))]
                      (provision! policies))))))
              (provision! policies))]
        (try
          (let [expected (prepared-basis! @conn provisioned creator requested-type)
                scope (native/store-id msgs-path)
                messages (binding [ec/*execution-context* room-ctx] (native/room-msgs-conn room-id))
                source-proof (when (:source request)
                               (let [workspace (native/default-workspace room-id)]
                                 (assoc (github/source-metadata! (:conn workspace) request)
                                        :registry-system (:registry-system workspace))))]
            (when-not (and messages (= scope (native/room-msgs-store-id room-id)))
              (refuse! :messages-unavailable))
            ;; No membership/content-scope yet; avoid publishing a UI watch
            ;; against a half-installed or not-yet-registered live Room.
            (binding [ec/*execution-context* room-ctx] (store/ensure! messages scope))
            (current!)
            (when request (creation/authorize! @conn creator request))
            (live/register-prepared-room! room-id)
            (current!)
            (d/transact conn [[:db.fn/call publish-tx expected creator members scope requested-type request source-proof]])
            (rooms/get-room room-id))
          (catch Throwable error
            (throw (ex-info "Knowledge Room prepared but Simmis activation failed"
                            {:type ::activation-failed :room/id room-id} error))))))))

(defn create!
  "HOST new canonical project with typed wiki, sandbox source and inactive
   ordinary wiki-maintenance workflow. Never funds or enables execution."
  [creator name party-ids]
  (create-in! creator name party-ids :project (str "knowledge-" (random-uuid)) nil (constantly nil)))

(defn create-for-intent!
  "One-shot HOST creation. The owner's client UUID fixes native exclusion;
   an existing slug refuses rather than retrying or implying successful recovery."
  [creator wire-request current!]
  (when-not (fn? current!) (refuse! :missing-owner-guard))
  (let [request (creation/request! wire-request)]
    (create-in! creator (:name request) (:members request) :project
                (creation/slug creator request) request current!)))

(defn create-personal!
  "HOST new owned canonical personal workspace. Its deterministic owner slug
   uses native atomic exclusion; a competing/pending intent refuses rather
   than becoming an implicit retry or post-publication type change."
  [creator name]
  (when-not (uuid? creator) (refuse! :invalid-owner))
  (create-in! creator name [] :personal-ai (str "personal-" creator) nil (constantly nil)))
