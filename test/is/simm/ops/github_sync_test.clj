(ns is.simm.ops.github-sync-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.versioning :as versioning]
            [dvergr.discourse :as discourse]
            [dvergr.room.registry :as registry]
            [dvergr.system.rooms :as native]
            [geschichte.git.http :as http]
            [geschichte.git.object :as object]
            [geschichte.git.project :as project]
            [geschichte.git.transport :as transport]
            [geschichte.repo :as repo]
            [geschichte.workspace :as workspace]
            [is.simm.model.github-sync :as journal]
            [is.simm.model.knowledge-rooms :as rooms]
            [is.simm.ops.github-import-test :as upstream]
            [is.simm.ops.github-import :as import]
            [is.simm.ops.github-sync :as sync]
            [is.simm.ops.github-sync-data :as facts]
            [is.simm.ops.github-sync-workspace-test :as transport-fixture]
            [is.simm.ops.github-workspace-test :as fixture]
            [is.simm.ops.owned-mcp :as mcp]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.memory-changes :as changes]
            [is.simm.runtimes.operation-lifetime :as lifetime]))

(defn- with-imported [f]
  (#'fixture/with-private-host
   (fn [{:keys [conn root] :as environment}]
     (#'upstream/with-stores
      (fn [upstream _ _]
        (repo/init! upstream)
        (repo/write! upstream "README.md" (object/utf8 "# Original source\n"))
        (repo/stage-all! upstream)
        (let [commit (repo/commit! upstream {:message "C1"})
              graph (atom (project/project upstream (:geschichte.commit/id commit)))
              owner (doto (lifetime/create) lifetime/open!) creator (random-uuid)]
          (d/transact conn [{:party/id creator :party/type :human :actor/kind :human}])
          (try
            (with-redefs [http/send-http (#'transport-fixture/sender graph)]
              (let [request {:id (str (random-uuid)) :name "Git sync source" :members []
                             :source {:kind :github :repository "owner/repository"}}
                    imported (lifetime/with-operation!
                               owner #(rooms/create-for-intent! creator request
                                                                (fn [] (lifetime/check-current!)
                                                                  (reads/current-control-db! conn root))))
                    room-id (:room/id imported)
                    host (mcp/host conn root owner)
                    principal {:principal/kind :human :principal/id (str "party/" creator)}
                    room (:room (reads/resolve-host-room! host principal (str room-id)))
                    bound (owned/resolve! @conn principal :merge room-id room)]
                ;; Only transport installation is stubbed. The original App
                ;; owner, native Room and canonical authority are all real.
                (with-redefs [application/capture-operation
                              (fn [] (fn [body] (lifetime/with-operation! owner #(body host))))]
                  (f (assoc environment :upstream upstream :graph graph :owner owner :creator creator
                            :room-id room-id :native-room room :parent (:conn bound) :stamp (:target-stamp bound)
                            :call-as (fn [party operation request] ((sync/prepare party room-id operation request)))
                            :call (fn [operation request] ((sync/prepare creator room-id operation request))))))))
            (finally
              (lifetime/fence! owner)
              (lifetime/join! owner 5000)
              (doseq [room (registry/list-rooms)] (discourse/close-room! room {:close-context? false}))
              (native/release-room-stores!)))))))))

(defn- c2! [{:keys [upstream graph]}]
  (repo/write! upstream "reports/new.md" (object/utf8 "New upstream report\n"))
  (repo/stage-all! upstream)
  (let [commit (repo/commit! upstream {:message "C2"})]
    (reset! graph (project/project upstream (:geschichte.commit/id commit)))))

(deftest review-apply-and-exact-observation-use-the-original-native-owner
  (with-imported
    (fn [{:keys [call conn creator room-id parent stamp root native-room] :as environment}]
      (let [client (random-uuid) wire {:client-id (str client) :expected-target-stamp (str stamp)}
            initial (repo/read parent "README.md")]
        (c2! environment)
        (let [ready (call :prepare wire)
              id (journal/request-id creator room-id client)
              row (journal/sync-in @conn id)
              branch (facts/branch id (:github.sync/claim row))]
          (is (= :ready (:status ready)))
          (is (= [{:path "reports/new.md" :kind :added
                   :after (get-in ready [:changes 0 :after])}] (:changes ready)))
          (is (= :ready (:status (call :prepare wire))) "Replay observes without another fetch")
          (is (nil? (repo/read parent "reports/new.md")))
          (is (= :ready (:status (call :observe {:client-id (str client)}))))
          (let [initial-hint (promise) source-hint (promise) advance @#'workspace/advance!
                notifier (changes/open-owned! conn root
                                              (fn [party hint]
                                                (when (and (= creator party) (= :memory-changed (:kind hint)))
                                                  (if (get (repo/worktree parent) "reports/new.md")
                                                    (deliver source-hint hint)
                                                    (deliver initial-hint hint)))))
                result (try
                         ((:watch! notifier) room-id native-room creator)
                         (is (= room-id (:room-id (deref initial-hint 5000 ::timeout))))
                         (with-redefs [workspace/advance!
                                       (fn [& args]
                                         (let [result (apply advance args)]
                                          ;; Observe before the service can write
                                          ;; its outcome/closure control rows.
                                           (is (= room-id (:room-id (deref source-hint 5000 ::timeout)))
                                               "Native publication drives the existing Memory notifier")
                                           result))]
                           (call :apply {:client-id (str client) :ready-id (:ready-id ready)}))
                         (finally ((:close! notifier))))]
            (is (= :applied (:status result)))
            (is (= "New upstream report\n" (String. ^bytes (repo/read parent "reports/new.md") "UTF-8")))
            (is (= (vec initial) (vec (repo/read parent "README.md"))))
            (is (nil? (journal/active-in @conn room-id)))
            (is (not (contains? (versioning/branches parent) branch)))
            (is (= :applied (:status (call :observe {:client-id (str client)}))))))))))

(deftest review-basis-edit-refuses-apply-without-overwriting-local-bytes
  (with-imported
    (fn [{:keys [call parent stamp] :as environment}]
      (c2! environment)
      (let [client (str (random-uuid)) ready (call :prepare {:client-id client :expected-target-stamp (str stamp)})]
        (repo/write! parent "README.md" (object/utf8 "Local uncommitted edit\n"))
        (is (thrown? clojure.lang.ExceptionInfo
                     (call :apply {:client-id client :ready-id (:ready-id ready)})))
        (is (= "Local uncommitted edit\n" (String. ^bytes (repo/read parent "README.md") "UTF-8")))
        (is (nil? (repo/read parent "reports/new.md")))
        (let [refused (call :observe {:client-id client})]
          (is (= :held (:status refused)))
          (is (true? (:can-discard? refused))))
        (is (thrown? clojure.lang.ExceptionInfo (call :recover {:client-id client})))
        (is (= :closed (:status (call :discard {:client-id client}))))
        (is (= "Local uncommitted edit\n" (String. ^bytes (repo/read parent "README.md") "UTF-8")))
       ;; Only this fixture now explicitly restores its own test edit. Sync did
       ;; not reset anything; the next preparation captures this new checkpoint.
        (repo/write! parent "README.md" (object/utf8 "# Original source\n"))
        (let [next-client (str (random-uuid))]
          (is (= :ready (:status (call :prepare {:client-id next-client :expected-target-stamp (str stamp)}))))
          (is (= :closed (:status (call :discard {:client-id next-client})))))))))

(deftest lost-native-ack-never-resends-and-recovery-is-explicitly-observational
  (with-imported
    (fn [{:keys [call conn creator room-id parent stamp] :as environment}]
      (c2! environment)
      (let [client (random-uuid) ready (call :prepare {:client-id (str client) :expected-target-stamp (str stamp)})
            advance @#'workspace/advance! calls (atom 0)]
        (with-redefs [workspace/advance! (fn [& args] (swap! calls inc) (apply advance args)
                                           (throw (ex-info "Lost native publication ACK" {})))]
          (is (thrown? clojure.lang.ExceptionInfo
                       (call :apply {:client-id (str client) :ready-id (:ready-id ready)})))
          (is (= :unconfirmed (:status (call :observe {:client-id (str client)}))))
          (is (thrown? clojure.lang.ExceptionInfo
                       (call :apply {:client-id (str client) :ready-id (:ready-id ready)})))
          (is (= :satisfied-observed (:status (call :recover {:client-id (str client)}))))
          (is (= 1 @calls)))
        (is (= "New upstream report\n" (String. ^bytes (repo/read parent "reports/new.md") "UTF-8")))
        (is (= :closed (:github.sync/state (journal/sync-in @conn (journal/request-id creator room-id client)))))
        (is (nil? (journal/active-in @conn room-id)))))))

(deftest another-native-git-owner-refuses-before-journal-or-fork
  (with-imported
    (fn [{:keys [call conn creator room-id parent stamp] :as environment}]
      (c2! environment)
      (let [client (random-uuid) before (versioning/branches parent)
            entered (promise) release (promise)
            operation (future (import/with-admission! #(do (deliver entered true) @release)))]
        (try
          (is (= true (deref entered 5000 ::timeout)))
          (is (thrown? clojure.lang.ExceptionInfo
                       (call :prepare {:client-id (str client) :expected-target-stamp (str stamp)})))
          (is (nil? (journal/sync-in @conn (journal/request-id creator room-id client))))
          (is (= before (versioning/branches parent)))
          (deliver release true)
          (is (= true (deref operation 5000 ::timeout)))
          (is (= :ready (:status (call :prepare {:client-id (str client) :expected-target-stamp (str stamp)}))))
          (is (= :closed (:status (call :discard {:client-id (str client)}))))
          (finally
            (deliver release true)
            (when (= ::timeout (deref operation 5000 ::timeout))
              (throw (ex-info "Native Git capacity owner still running" {})))))))))

(deftest failed-private-pull-keeps-a-positive-last-checkpoint-for-discard
  (with-imported
    (fn [{:keys [call conn creator room-id parent stamp] :as environment}]
      (c2! environment)
      (let [client (random-uuid) pull @#'transport/pull!]
        (with-redefs [transport/pull!
                      (fn [request] (pull request) (throw (java.io.IOException. "Lost upstream response")))]
          (is (thrown? Exception (call :prepare {:client-id (str client) :expected-target-stamp (str stamp)}))))
        (let [row (journal/sync-in @conn (journal/request-id creator room-id client))]
          (is (= :held (:github.sync/state row)))
          (is (= :allocated (:github.sync/held-from row)))
          (is (uuid? (:github.sync/workspace row)))
          (is (uuid? (:github.sync/incident row)))
          (is (true? (:can-discard? (call :observe {:client-id (str client)}))))
          (is (= :closed (:status (call :discard {:client-id (str client)}))))
          (is (nil? (repo/read parent "reports/new.md"))))))))

(deftest new-room-owner-can-retire-a-revoked-creators-known-private-preparation
  (with-imported
    (fn [{:keys [call call-as conn creator room-id parent stamp w] :as environment}]
      (c2! environment)
      (let [client (random-uuid)
            ready (call :prepare {:client-id (str client) :expected-target-stamp (str stamp)})
            id (journal/request-id creator room-id client)]
        (is (= :ready (:status ready)))
        (d/transact conn [[:db/add [:room/id room-id] :room/owner [:party/id (:owner w)]]
                          [:db/add [:party/id creator] :actor/kind :agent]])
        (is (thrown? clojure.lang.ExceptionInfo (call :observe {:client-id (str client)})))
        (is (= :closed (:status (call-as (:owner w) :retire {:sync-id (str id)}))))
        (is (= creator (:github.sync/by (journal/sync-in @conn id))) "Cleanup does not impersonate the creator")
        (is (nil? (journal/active-in @conn room-id)))
        (is (nil? (repo/read parent "reports/new.md")))))))

(deftest revocation-after-private-pull-retains-allocation-proof-for-owner-cleanup
  (with-imported
    (fn [{:keys [call call-as conn creator room-id parent stamp w] :as environment}]
      (c2! environment)
      (let [client (random-uuid) pull @#'transport/pull!
            id (journal/request-id creator room-id client)]
        (with-redefs [transport/pull!
                      (fn [request]
                        (let [result (pull request)]
                          (d/transact conn [[:db/add [:room/id room-id] :room/owner [:party/id (:owner w)]]
                                            [:db/add [:party/id creator] :actor/kind :agent]])
                          result))]
          (is (thrown? Exception (call :prepare {:client-id (str client) :expected-target-stamp (str stamp)}))))
        (let [row (journal/sync-in @conn id)]
          (is (= :allocated (:github.sync/state row)))
          (is (uuid? (:github.sync/workspace row)))
          (is (nil? (:github.sync/ready row)))
          (is (= :closed (:status (call-as (:owner w) :retire {:sync-id (str id)}))))
          (is (nil? (journal/active-in @conn room-id)))
          (is (nil? (repo/read parent "reports/new.md"))))))))
