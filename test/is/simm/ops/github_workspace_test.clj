(ns is.simm.ops.github-workspace-test
  "Private native creation with scripted Git transport; no external network."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.discourse :as discourse]
            [dvergr.artifact :as artifact]
            [dvergr.room.registry :as registry]
            [dvergr.system.rooms :as native]
            [dvergr.substrate.geschichte :as substrate]
            [geschichte.git.http :as http]
            [geschichte.git.object :as object]
            [geschichte.git.pack :as pack]
            [geschichte.git.pkt-line :as pkt]
            [geschichte.git.project :as project]
            [geschichte.repo :as repo]
            [is.simm.documents.profile :as profile]
            [is.simm.documents.view :as view]
            [is.simm.model.knowledge-rooms :as rooms]
            [is.simm.model.github-integrations :as integrations]
            [is.simm.model.github-connections :as git-connections]
            [is.simm.model.rooms :as roster]
            [is.simm.model.workspace-intent :as intent]
            [is.simm.ops.benchmark-checkpoint-native-test :as host]
            [is.simm.ops.github-import-test :as upstream]
            [is.simm.ops.github-import :as github]
            [is.simm.ops.github-connection :as connection]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.runtimes.operation-lifetime :as lifetime]))

(defn- with-private-host [f]
  (let [original @#'http/send-http
        deny (fn [_] (throw (ex-info "External Git HTTP is forbidden in this fixture" {})))]
    (alter-var-root #'http/send-http (constantly deny))
    (try (@#'host/with-native-host f)
         (finally
           ;; A retained native owner keeps the fail-closed root until explicit
           ;; reconciliation or this disposable verifier process exits.
           (when-not @@#'host/held-host
             (alter-var-root #'http/send-http (constantly original)))))))

(defn- with-upstream [intercept f]
  (@#'upstream/with-stores
   (fn [source _ _]
     (repo/init! source)
     (repo/write! source "README.md" (object/utf8 "# Imported source\n\nOrdinary Markdown stays intact.\n"))
     (repo/write! source "build.clj" (object/utf8 "(throw (Exception. \"Do not execute imported code\"))\n"))
     (repo/stage-all! source)
     (let [head (repo/commit! source {:message "Synthetic GitHub source"})
           graph (project/project source (:geschichte.commit/id head))
           oid (:oid graph) packed (pack/encode (:objects graph))]
       (with-redefs [http/send-http
                     (fn [{:keys [method body] :as request}]
                       (or (intercept request)
                           (if (= :get method)
                         {:status 200 :body (pkt/encode ["version 2\n" "ls-refs\n" "fetch\n" :flush])}
                         (if (= "command=ls-refs\n" (pkt/text (first (pkt/decode body))))
                           {:status 200 :body (pkt/encode [(str oid " HEAD symref-target:refs/heads/main\n")
                                                          (str oid " refs/heads/main\n") :flush])}
                           {:status 200 :body (pkt/encode ["packfile\n"
                                                          (object/concat-bytes (byte-array [1]) packed) :flush])}))))]
         (f {:oid oid}))))))

(deftest import-is-invisible-until-native-validation-and-final-membership
  (with-private-host
   (fn [{:keys [conn root]}]
     (let [creator (random-uuid) owner (doto (lifetime/create) lifetime/open!)
           request {:id (str (random-uuid)) :name "Public source" :members []
                    :source {:kind :github :repository "owner/repository"}}
           normalized (intent/request! request) entered (promise) release (promise)]
       (d/transact conn [{:party/id creator :party/type :human}])
       (try
         (with-upstream
          (fn [{:keys [method]}]
            (when (= :get method)
              (deliver entered true)
              (when-not (= true (deref release 120000 ::timeout))
                (throw (ex-info "Scripted Git fixture was not released" {})))))
          (fn [{:keys [oid]}]
            (let [current! #(do (lifetime/check-current!) (reads/current-control-db! conn root))
                  worker (future
                           (lifetime/with-operation!
                            owner #(rooms/create-for-intent! creator request current!)))]
              (try
                (is (= true (deref entered 120000 ::timeout)))
                (let [row (d/entity @conn [:room/slug (intent/slug creator normalized)])]
                  (is (uuid? (:room/id row)))
                  (is (= "owner/repository" (:room/github-source row)))
                  (is (nil? (:room/parties row)))
                  (is (nil? (:room/content-db-scope row)))
                  (is (nil? (integrations/current-in @conn (:room/id row))))
                  (is (nil? (native/room-ctx-for (:room/id row))))
                  (is (empty? (registry/list-rooms)))
                  (is (empty? (roster/get-party-rooms creator)))
                  (is (= {:status :preparing} (intent/observe @conn creator normalized))))
                (deliver release true)
                (let [room (deref worker 180000 ::timeout)]
                  (is (not= ::timeout room))
                  (let [workspace (native/default-workspace (:room/id room))
                        source (:conn workspace) row (d/entity @conn [:room/id (:room/id room)])]
                    (is (= #{creator} (:room/parties room)))
                    (is (= 1 (count (registry/list-rooms))))
                    (is (= 1 (count (roster/get-party-rooms creator))))
                    (is (= #{"README.md" "build.clj"} (set (keys (repo/worktree source)))))
                    (is (= oid (:geschichte.commit/git-oid (repo/commit-by-id source (:room/github-import-head row)))))
                    (is (= "refs/heads/main" (:room/github-ref row)))
                    (let [cursor (integrations/current-in @conn (:room/id room))
                          store (artifact/datahike-store conn)
                          connection (:value (artifact/get-bounded-value store (:github.integration/connection cursor) 16384))
                          receipt (:value (artifact/get-bounded-value store (:github.integration/import-proof cursor) 16384))]
                      (is (= (:room/github-import-head row) (:github.integration/head cursor)))
                      (is (= oid (:github.integration/oid cursor)))
                      (is (= (:registry-system workspace) (get-in connection [:target :registry-system])))
                      (is (= (:runtime-system workspace) (get-in connection [:target :runtime-system])))
                      (is (= (get-in @source [:config :store :id]) (get-in connection [:target :store])))
                      (is (= (:github.integration/connection-id cursor) (git-connections/connection-id connection)))
                      (is (= receipt (integrations/import-body connection creator (intent/receipt creator normalized)
                                                               (:room/github-import-head row) oid)))
                      (is (= (:github.integration/import-proof-id cursor) (integrations/import-id receipt)))
                      (is (nil? (:github.integration/proposal cursor)) "Creation invents no acceptance or actor Run"))
                    (is (= :ready (:status (view/prepare @source profile/selection-options))))
                    (is (= :created (:status (intent/observe @conn creator normalized))))))
                (finally
                  (deliver release true)
                  ;; Join before restoring scripted transport or retiring any
                  ;; native fixture owner; cancellation is not a cleanup ACK.
                  (when (= ::timeout (deref worker 180000 ::timeout))
                    (throw (ex-info "Git import fixture still owns native work" {}))))))))
         (finally
           (lifetime/fence! owner)
           (lifetime/join! owner 5000)
           (doseq [room (registry/list-rooms)] (discourse/close-room! room {:close-context? false}))
           (native/release-room-stores!)))))))

(defn- run-failure! [scenario]
  (with-private-host
   (fn [{:keys [conn root contexts errors configs]}]
     (let [creator (random-uuid) owner (doto (lifetime/create) lifetime/open!)
           request {:id (str (random-uuid)) :name "Interrupted source" :members []
                    :source {:kind :github :repository "owner/repository"}}
           normalized (intent/request! request) entered (promise) release (promise)]
       (d/transact conn [{:party/id creator :party/type :human :actor/kind :human}])
       (try
         (with-redefs [github/limits (cond-> github/limits (= :budget-refused scenario) (assoc :checkout-bytes 1))]
           (with-upstream
            (fn [{:keys [method]}]
              (when (= :get method)
                (deliver entered true)
                (when-not (= true (deref release 120000 ::timeout))
                  (throw (ex-info "Scripted Git fixture was not released" {})))
                (when (= :invalid-upstream scenario)
                  {:status 200 :body (byte-array [1 2 3 4])})))
            (fn [_]
              (let [current! #(do (lifetime/check-current!) (reads/current-control-db! conn root))
                    worker (future
                             (try {:ok (lifetime/with-operation!
                                       owner #(rooms/create-for-intent! creator request current!))}
                                  (catch Throwable error {:error error})))]
                (try
                  (is (= true (deref entered 120000 ::timeout)))
                  (case scenario
                    :app-fenced (lifetime/fence! owner)
                    :creator-revoked (d/transact conn [[:db/add [:party/id creator] :actor/kind :agent]])
                    nil)
                  (deliver release true)
                  (let [result (deref worker 180000 ::timeout)
                        row (d/entity @conn [:room/slug (intent/slug creator normalized)])
                        id (:room/id row)]
                    (is (map? result))
                    (is (some? (:error result)))
                    (is (nil? (:ok result)))
                    (is (nil? (:room/parties row)))
                    (is (nil? (:room/content-db-scope row)))
                    (is (nil? (integrations/current-in @conn id)))
                    (is (empty? (registry/list-rooms)))
                    (is (empty? (roster/get-party-rooms creator)))
                    (when-not (= :creator-revoked scenario)
                      ;; Failed exclusive repository initialization has already
                      ;; positively aborted, released and deleted its store.
                      ;; Clear ONLY this fixture's plain diagnostic after those
                      ;; proofs, never an opaque retained native owner.
                      (let [error (get @errors id)
                            causes (tree-seq some? (fn [t] (remove nil? (cons (ex-cause t) (seq (.getSuppressed ^Throwable t))))) error)
                            clean? (and error (not (contains? @contexts id))
                                        (not-any? substrate/initialization-recovery-required? causes)
                                        (not-any? d/database-exists? (vals @configs)))]
                        (is clean? "Expected refusal must have positive native cleanup")
                        (when-not clean? (throw (ex-info "Native import cleanup remains owned" {})))
                        (swap! errors (fn [observed]
                                        (when-not (identical? error (get observed id))
                                          (throw (ex-info "Fixture failure identity changed" {})))
                                        (dissoc observed id))))))
                  (finally
                    (deliver release true)
                    (when (= ::timeout (deref worker 180000 ::timeout))
                      (throw (ex-info "Git failure fixture still owns native work" {})))))))))
         (finally
           (lifetime/fence! owner)
           (lifetime/join! owner 5000)
           (doseq [room (registry/list-rooms)] (discourse/close-room! room {:close-context? false}))
           (native/release-room-stores!)))))))

(deftest app-fencing-and-source-refusals-never-publish-a-memory
  (doseq [scenario [:app-fenced :budget-refused :invalid-upstream :creator-revoked]]
    (run-failure! scenario)))

(defn- run-cursor-publication-fault! [scenario]
  (with-private-host
    (fn [{:keys [conn root]}]
      (let [owner (doto (lifetime/create) lifetime/open!) creator (random-uuid)
            request {:id (str (random-uuid)) :name "Cursor fault" :members []
                     :source {:kind :github :repository "owner/repository"}}
            normalized (intent/request! request)
            actual-prepare @#'connection/prepare-import! actual-publish @#'artifact/publish-value!
            actual-transact @#'d/transact captured (atom nil) publications (atom 0) gets (atom 0)]
        (d/transact conn [{:party/id creator :party/type :human :actor/kind :human}])
        (try
          (with-upstream
            (fn [{:keys [method]}] (when (= :get method) (swap! gets inc)) nil)
            (fn [_]
              (with-redefs [connection/prepare-import!
                            (fn [& args]
                              (reset! captured (nth args 4))
                              (let [result (apply actual-prepare args)]
                                (when (= :capture scenario) (throw (ex-info "Injected post-registration refusal" {})))
                                result))
                            artifact/publish-value!
                            (fn [store value write!]
                              (when (and (= :artifact scenario) (= :initial-import (:kind value)))
                                (throw (ex-info "Injected receipt publication refusal" {})))
                              (actual-publish store value write!))
                            d/transact
                            (fn [target tx]
                              (let [publication? (and (identical? conn target)
                                                       (identical? @#'rooms/publish-tx (second (first tx))))
                                    result (actual-transact target tx)]
                                (when publication? (swap! publications inc))
                                (when (and publication? (= :lost-control-ack scenario))
                                  (throw (ex-info "Injected committed membership ACK loss" {})))
                                result))]
                (is (thrown? clojure.lang.ExceptionInfo
                             (lifetime/with-operation!
                              owner #(rooms/create-for-intent! creator request
                                                               (fn [] (lifetime/check-current!)
                                                                 (reads/current-control-db! conn root))))))
                (let [row (d/entity @conn [:room/slug (intent/slug creator normalized)])
                      id (:room/id row) native-room @captured]
                  (is (some? native-room))
                  (is (identical? native-room (first (registry/list-rooms))))
                  (is (identical? (:ctx native-room) (native/room-ctx-for id)))
                  (if (= :lost-control-ack scenario)
                    (let [first-observation (intent/observe @conn creator normalized)
                          second-observation (intent/observe @conn creator normalized)]
                      (is (= :created (:status first-observation)))
                      (is (= first-observation second-observation))
                      (is (= (str id) (get-in first-observation [:room :room/id])))
                      (is (= 1 @publications))
                      (is (some? (integrations/current-in @conn id)))
                      (is (= 1 @gets) "Observation never repeats import or construction"))
                    (do
                      (is (nil? (:room/parties row)))
                      (is (nil? (:room/content-db-scope row)))
                      (is (nil? (integrations/current-in @conn id)))
                      (is (empty? (roster/get-party-rooms creator)))
                      (is (= :prepared-not-published (:status (intent/observe @conn creator normalized))))
                      (is (zero? @publications))))
                  ;; The failed post-registration call retains its exact known
                  ;; native owner. Retire that object through normal ownership;
                  ;; artifact existence/absence never substitutes for cleanup.
                  (discourse/close-room! native-room {:close-context? false})
                  (native/release-room-stores!)
                  (is (empty? (registry/list-rooms)))
                  (is (nil? (native/room-ctx-for id)))))))
          (finally
            (lifetime/fence! owner)
            (lifetime/join! owner 5000)
            (doseq [room (registry/list-rooms)] (discourse/close-room! room {:close-context? false}))
            (native/release-room-stores!)))))))

(deftest post-registration-cursor-failures-retain-native-ownership-without-public-membership
  (doseq [scenario [:capture :artifact :lost-control-ack]]
    (run-cursor-publication-fault! scenario)))
