(ns is.simm.ops.github-sync-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [datahike.gc-roots :as gc-roots]
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
            [is.simm.model.github-integrations :as integrations]
            [is.simm.model.github-pins :as pins]
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
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [is.simm.uis.web.desktop.github-remote :as remote]))

;; Each fixture represents a fresh HOST. Production has no policy reset.
(use-fixtures :each
  (fn [f]
    (with-redefs-fn
      {#'native/storage-gc-homes (atom #{})
       #'native/storage-gc-admission (atom {:ever-admitted? false :prohibited-by nil})}
      f)))

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
                            :remote-call (fn [operation request]
                                           ((remote/prepare-action creator (str room-id) operation request)))
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
            initial (repo/read parent "README.md")
            initial-cursor (integrations/current-in @conn room-id)]
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
            (let [cursor (integrations/current-in @conn room-id)
                  outcome (journal/sync-in @conn id)]
              (is (= (:head ready) (:github.integration/head cursor)))
              (is (= (:git-oid ready) (:github.integration/oid cursor)))
              (is (= id (:github.integration/sync cursor)))
              (is (= (:github.sync/outcome outcome) (:github.integration/proof cursor)))
              (is (= (:github.sync/outcome-id outcome) (:github.integration/proof-id cursor)))
              (is (= (:github.integration/import-head initial-cursor) (:github.integration/import-head cursor))))
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
    (fn [{:keys [call conn creator room-id parent stamp upstream graph] :as environment}]
      (c2! environment)
      (let [client (random-uuid) ready (call :prepare {:client-id (str client) :expected-target-stamp (str stamp)})
            advance @#'workspace/advance! calls (atom 0)
            cursor (integrations/current-in @conn room-id)]
        (with-redefs [workspace/advance! (fn [& args] (swap! calls inc) (apply advance args)
                                           (throw (ex-info "Lost native publication ACK" {})))]
          (is (thrown? clojure.lang.ExceptionInfo
                       (call :apply {:client-id (str client) :ready-id (:ready-id ready)})))
          (is (= :unconfirmed (:status (call :observe {:client-id (str client)}))))
          (is (thrown? clojure.lang.ExceptionInfo
                       (call :apply {:client-id (str client) :ready-id (:ready-id ready)})))
          (is (= :satisfied-observed (:status (call :recover {:client-id (str client)}))))
          (is (= cursor (integrations/current-in @conn room-id))
              "Observing the desired files is not proof of an accepted upstream integration")
          (is (= 1 @calls)))
        (is (= "New upstream report\n" (String. ^bytes (repo/read parent "reports/new.md") "UTF-8")))
        (is (= :closed (:github.sync/state (journal/sync-in @conn (journal/request-id creator room-id client)))))
        (is (nil? (journal/active-in @conn room-id)))
        ;; Optional metadata must not turn observational recovery into a
        ;; permanent regression of the existing checkpoint-guarded FF path.
        (repo/write! upstream "README.md" (object/utf8 "# Upstream after recovery\n"))
        (repo/stage-all! upstream)
        (let [commit (repo/commit! upstream {:message "After observational recovery"})]
          (reset! graph (project/project upstream (:geschichte.commit/id commit))))
        (let [next-client (str (random-uuid))
              next-ready (call :prepare {:client-id next-client :expected-target-stamp (str stamp)})]
          (is (= :ready (:status next-ready)))
          (is (= :applied (:status (call :apply {:client-id next-client :ready-id (:ready-id next-ready)}))))
          (is (= "# Upstream after recovery\n" (String. ^bytes (repo/read parent "README.md") "UTF-8")))
          (is (= cursor (integrations/current-in @conn room-id))
              "Later FF success must not invent proof for the earlier unknown publication"))))))

(deftest fast-forward-without-integration-metadata-does-not-invent-a-base
  (with-imported
    (fn [{:keys [call conn room-id parent stamp] :as environment}]
      (let [cursor (integrations/current-in @conn room-id)]
        (d/transact conn [[:db/retractEntity (:db/id cursor)]]))
      (c2! environment)
      (let [client (str (random-uuid))
            ready (call :prepare {:client-id client :expected-target-stamp (str stamp)})]
        (is (= :ready (:status ready)))
        (is (= :applied (:status (call :apply {:client-id client :ready-id (:ready-id ready)}))))
        (is (= "New upstream report\n" (String. ^bytes (repo/read parent "reports/new.md") "UTF-8")))
        (is (nil? (integrations/current-in @conn room-id)))))))

(deftest lost-control-ack-observes-the-atomic-cursor-and-outcome-without-republication
  (with-imported
    (fn [{:keys [call conn creator room-id stamp] :as environment}]
      (c2! environment)
      (let [client (random-uuid)
            ready (call :prepare {:client-id (str client) :expected-target-stamp (str stamp)})
            transact @#'d/transact advance @#'workspace/advance!
            lost? (atom false) advances (atom 0)
            result (with-redefs [d/transact
                                (fn [target tx & args]
                                  (let [result (apply transact target tx args)]
                                    (when (and (identical? conn target)
                                               (= :applied (:github.sync/state
                                                            (journal/sync-in @conn (journal/request-id creator room-id client))))
                                               (compare-and-set! lost? false true))
                                      (throw (ex-info "Lost control outcome ACK" {})))
                                    result))
                                workspace/advance!
                                (fn [& args] (swap! advances inc) (apply advance args))]
                     (call :apply {:client-id (str client) :ready-id (:ready-id ready)}))
            row (journal/sync-in @conn (journal/request-id creator room-id client))
            cursor (integrations/current-in @conn room-id)]
        (is (true? @lost?))
        (is (= 1 @advances))
        (is (= :applied (:status result)))
        (is (= :closed (:github.sync/state row)))
        (is (= (:head ready) (:github.integration/head cursor)))
        (is (= (:github.sync/outcome row) (:github.integration/proof cursor)))))))

(deftest a-cursor-change-after-native-publication-cannot-commit-a-partial-control-outcome
  (with-imported
    (fn [{:keys [call conn creator room-id parent stamp] :as environment}]
      (c2! environment)
      (let [client (random-uuid)
            ready (call :prepare {:client-id (str client) :expected-target-stamp (str stamp)})
            before (integrations/current-in @conn room-id)
            changed-proof (random-uuid)
            advance @#'workspace/advance!]
        (with-redefs [workspace/advance!
                      (fn [& args]
                        (let [result (apply advance args)]
                          (d/transact conn [{:db/id (:db/id before) :github.integration/proof changed-proof}])
                          result))]
          (is (thrown? clojure.lang.ExceptionInfo
                       (call :apply {:client-id (str client) :ready-id (:ready-id ready)}))))
        (let [row (journal/sync-in @conn (journal/request-id creator room-id client))
              cursor (integrations/current-in @conn room-id)]
          (is (= :applying (:github.sync/state row)))
          (is (nil? (:github.sync/outcome row)))
          (is (= (:github.integration/head before) (:github.integration/head cursor)))
          (is (= changed-proof (:github.integration/proof cursor)))
          (is (= "New upstream report\n" (String. ^bytes (repo/read parent "reports/new.md") "UTF-8")))
          (is (= :satisfied-observed (:status (call :recover {:client-id (str client)}))))
          (is (= cursor (integrations/current-in @conn room-id))))))))

(deftest successive-syncs-use-the-last-positive-upstream-and-preserve-no-op-proofs
  (with-imported
    (fn [{:keys [call conn room-id stamp upstream graph] :as environment}]
      (let [initial (integrations/current-in @conn room-id)
            apply-next! (fn []
                          (let [client (str (random-uuid))
                                ready (call :prepare {:client-id client :expected-target-stamp (str stamp)})]
                            (is (= :ready (:status ready)))
                            (is (= :applied (:status (call :apply {:client-id client :ready-id (:ready-id ready)}))))
                            ready))]
        (c2! environment)
        (let [first-update (apply-next!) first-cursor (integrations/current-in @conn room-id)]
          (repo/write! upstream "README.md" (object/utf8 "# Revision three\n"))
          (repo/stage-all! upstream)
          (let [commit (repo/commit! upstream {:message "C3"})]
            (reset! graph (project/project upstream (:geschichte.commit/id commit))))
          (let [second-update (apply-next!) second-cursor (integrations/current-in @conn room-id)]
            (is (= (:head first-update) (:base-head second-update)))
            (is (= (:head second-update) (:github.integration/head second-cursor)))
            (is (not= (:github.integration/proof first-cursor) (:github.integration/proof second-cursor)))
            (is (= (:github.integration/import-head initial) (:github.integration/import-head second-cursor)))
            (is (= 0 (:total (apply-next!))))
            (is (= second-cursor (integrations/current-in @conn room-id)))
            (is (nil? (journal/active-in @conn room-id)))))))))

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


(defn- pin-rows [db]
  (mapv #(d/pull db '[*] (:e %)) (d/datoms db :avet :github.pin/id)))

(deftest sync-preparation-durably-retains-the-exact-upstream-base
  (with-imported
    (fn [{:keys [call conn parent stamp] :as environment}]
      (c2! environment)
      (let [client (str (random-uuid)) wire {:client-id client :expected-target-stamp (str stamp)}
            ready (call :prepare wire) row (first (pin-rows @conn))]
        (is (= :ready (:status ready)))
        (is (= 1 (count (pin-rows @conn))))
        (is (= :retained (:github.pin/state row)))
        (is (= :retained (get-in ready [:base-retention :status])))
        (is (contains? (gc-roots/roots (:store @parent) {:sync? true}) (:github.pin/id row)))
        (is (= :ready (:status (call :prepare wire)))))
      (is (thrown? clojure.lang.ExceptionInfo (native/gc-room! :unused {:dry-run? true}))))))

(deftest a-lost-pin-ack-recovers-by-verification-without-creating-another-root
  (with-imported
    (fn [{:keys [conn stamp] call :remote-call :as environment}]
      (c2! environment)
      (let [wire {:client-id (str (random-uuid)) :expected-target-stamp (str stamp)}
            pin! gc-roots/pin! calls (atom 0)]
        (with-redefs [gc-roots/pin! (fn [& args]
                                    (swap! calls inc) (apply pin! args)
                                    (throw (ex-info "Lost native pin ACK" {})))]
          (let [ready (call :prepare wire)]
            (is (= :ready (:status ready)))
            (is (= :held (get-in ready [:base-retention :status])))
            (is (= :ready (:status (call :observe (select-keys wire [:client-id])))))
            (call :discard (select-keys wire [:client-id]))))
        (is (= :creating (:github.pin/state (first (pin-rows @conn)))))
        (with-redefs [gc-roots/pin! (fn [& _] (throw (AssertionError. "Recovery cannot recreate")))]
          (let [ready (call :prepare (assoc wire :client-id (str (random-uuid))))]
            (is (= :ready (:status ready)))
            (is (= :retained (get-in ready [:base-retention :status])))))
        (is (= 1 @calls))
        (is (= 1 (count (pin-rows @conn))))
        (is (= :retained (:github.pin/state (first (pin-rows @conn)))))))))

(deftest lost-control-ack-before-root-creation-holds-without-replaying-native-creation
  (with-imported
    (fn [{:keys [conn stamp] call :remote-call :as environment}]
      (c2! environment)
      (let [wire {:client-id (str (random-uuid)) :expected-target-stamp (str stamp)}
            transact d/transact]
        (with-redefs [d/transact
                      (fn [c tx]
                        (let [report (transact c tx)]
                          (when (and (identical? c conn)
                                     (empty? (pin-rows (:db-before report)))
                                     (seq (pin-rows (:db-after report))))
                            (throw (ex-info "Lost control pin ACK" {})))
                          report))]
          (let [ready (call :prepare wire)]
            (is (= :ready (:status ready)))
            (is (= :held (get-in ready [:base-retention :status])))
            (is (= :ready (:status (call :observe (select-keys wire [:client-id])))))
            (call :discard (select-keys wire [:client-id]))))
        (is (= 1 (count (pin-rows @conn))))
        (with-redefs [gc-roots/pin! (fn [& _] (throw (AssertionError. "Uncertain winner cannot create")))]
          (let [ready (call :prepare (assoc wire :client-id (str (random-uuid))))]
            (is (= :ready (:status ready)))
            (is (= :held (get-in ready [:base-retention :status])))))
        (is (= :creating (:github.pin/state (first (pin-rows @conn)))))))))

(deftest optional-corrupt-cursor-metadata-cannot-strand-the-existing-sync-flow
  (with-imported
    (fn [{:keys [conn room-id stamp] call :remote-call :as environment}]
      (let [cursor (integrations/current-in @conn room-id)]
        (d/transact conn [{:db/id (:db/id cursor) :github.integration/connection (random-uuid)}]))
      (c2! environment)
      (let [client (str (random-uuid))
            ready (call :prepare {:client-id client :expected-target-stamp (str stamp)})]
        (is (= :ready (:status ready)))
        (is (= {:status :held :reason :retention-unavailable} (:base-retention ready)))
        (is (= :ready (:status (call :observe {:client-id client}))))
        (is (= :applied (:status (call :apply {:client-id client :ready-id (:ready-id ready)}))))))))

(deftest a-missing-native-root-is-held-despite-a-retained-control-label
  (with-imported
    (fn [{:keys [call conn parent stamp] :as environment}]
      (c2! environment)
      (let [client (str (random-uuid))
            prepared (call :prepare {:client-id client :expected-target-stamp (str stamp)})
            row (first (pin-rows @conn))]
        (is (= :retained (get-in prepared [:base-retention :status])))
        (call :discard {:client-id client})
        (gc-roots/release! @parent (:github.pin/id row) {:sync? true})
        (with-redefs [gc-roots/pin! (fn [& _] (throw (AssertionError. "Must not recreate a missing root")))]
          (let [ready (call :prepare {:client-id (str (random-uuid)) :expected-target-stamp (str stamp)})]
            (is (= :ready (:status ready)))
            (is (= :held (get-in ready [:base-retention :status])))))
        (is (= 1 (count (pin-rows @conn))))))))
