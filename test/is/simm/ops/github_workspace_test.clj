(ns is.simm.ops.github-workspace-test
  "Private native creation with scripted Git transport; no external network."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.discourse :as discourse]
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
            [is.simm.model.rooms :as roster]
            [is.simm.model.workspace-intent :as intent]
            [is.simm.ops.benchmark-checkpoint-native-test :as host]
            [is.simm.ops.github-import-test :as upstream]
            [is.simm.ops.github-import :as github]
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
