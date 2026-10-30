(ns is.simm.ops.github-push-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [dvergr.system.rooms :as rooms]
            [geschichte.git.object :as object]
            [geschichte.git.http :as http]
            [geschichte.git.push :as git]
            [geschichte.repo :as repo]
            [is.simm.model.github-integrations :as integrations]
            [is.simm.model.github-credentials :as credentials]
            [is.simm.model.personal-connections :as accounts]
            [is.simm.model.mail-credentials :as envelope]
            [is.simm.model.github-sync :as journal]
            [is.simm.ops.github-host :as host]
            [is.simm.ops.github-push :as push]
            [is.simm.ops.github-service :as service]
            [is.simm.ops.github-sync-test :as fixture]
            [is.simm.runtimes.operation-lifetime :as lifetime]))

(use-fixtures :each
  (fn [f]
    (with-redefs-fn {#'rooms/storage-gc-homes (atom #{})
                     #'rooms/storage-gc-admission (atom {:ever-admitted? false :prohibited-by nil})} f)))

(defn- call [environment action request]
  ((host/capture (:creator environment) (:room-id environment) action #(push/in! % action request))))

(defn- with-ready [f]
  (#'fixture/with-imported
   (fn [{:keys [parent stamp] :as environment}]
     (repo/write! parent "local.md" (object/utf8 "Local authored content\n"))
     (repo/stage-all! parent)
     (repo/commit! parent {:message "Saved local work"})
     ;; Exercise the real bounded projection and original native/control owners.
     ;; Transport advertisement is covered separately by the real-Git fixture.
     (with-redefs [git/prepare-http! (fn [_ _conn _url head options]
                                       (assoc (git/prepare parent head options) :capabilities ["report-status"]))]
       (let [client (random-uuid)
             ready (call environment :prepare {:client-id client :expected-target-stamp stamp})]
         (is (= :ready (:status ready)))
         (f (assoc environment :client client :ready ready)))))))

(deftest preparation-does-not-send-and-positive-result-advances-cursor-atomically
  (let [sends (atom 0)]
    (with-redefs [git/send-http! (fn [_ _ prepared _]
                                   (swap! sends inc)
                                   (is (bytes? (:pack prepared)))
                                   {:status :pushed :ref (:ref prepared) :old (:old prepared) :new (:new prepared)})]
      (with-ready
        (fn [{:keys [client ready conn room-id creator] :as environment}]
          (is (zero? @sends))
          (is (= ready (call environment :observe {:client-id client})))
          (let [result (call environment :apply {:client-id client :ready-id (parse-uuid (:ready-id ready))})
                row (journal/sync-in @conn (journal/request-id creator room-id client))
                cursor (integrations/current-in @conn room-id)]
            (is (= :pushed (:status result)))
            (is (= (:git-oid ready) (:github.integration/oid cursor)))
            (is (= (parse-uuid (:head ready)) (:github.integration/head cursor)))
            (is (= (:github.push/outcome row) (:github.integration/proof cursor)))
            (is (= (:github.push/outcome-id row) (:github.integration/proof-id cursor)))
            (is (thrown? clojure.lang.ExceptionInfo
                         (call environment :apply {:client-id client :ready-id (parse-uuid (:ready-id ready))})))
            (is (= 1 @sends))
            (is (= :closed (:status (call environment :discard {:client-id client}))))
            (is (nil? (journal/active-in @conn room-id)))))))))

(deftest a-lost-send-ack-is-fenced-and-observation-never-retries
  (let [sends (atom 0)]
    (with-redefs [git/send-http! (fn [& _] (swap! sends inc) (throw (ex-info "Lost remote ACK" {})))]
      (with-ready
        (fn [{:keys [client ready conn room-id] :as environment}]
          (let [cursor (integrations/current-in @conn room-id)
                request {:client-id client :ready-id (parse-uuid (:ready-id ready))}]
            (is (thrown? clojure.lang.ExceptionInfo (call environment :apply request)))
            (is (= :unconfirmed (:status (call environment :observe {:client-id client}))))
            (is (thrown? clojure.lang.ExceptionInfo (call environment :apply request)))
            (is (thrown? clojure.lang.ExceptionInfo (call environment :discard {:client-id client})))
            (is (= cursor (integrations/current-in @conn room-id)))
            (is (= 1 @sends))))))))

(deftest a-lost-sending-control-ack-must-not-authorize-network-dispatch
  (with-ready
    (fn [{:keys [client ready creator room-id conn] :as environment}]
      (let [transact d/transact sends (atom 0) id (journal/request-id creator room-id client)]
        (with-redefs [d/transact (fn [target tx & args]
                                   (let [report (apply transact target tx args)]
                                     (when (and (identical? target conn)
                                                (= :sending (:github.sync/state (journal/sync-in (:db-after report) id))))
                                       (throw (ex-info "Lost Sending ACK" {})))
                                     report))
                      git/send-http! (fn [& _] (swap! sends inc) (throw (AssertionError. "Unacknowledged send")))]
          (is (thrown? clojure.lang.ExceptionInfo
                       (call environment :apply {:client-id client :ready-id (parse-uuid (:ready-id ready))})))
          (is (= :unconfirmed (:status (call environment :observe {:client-id client}))))
          (is (zero? @sends)))))))

(deftest local-edits-after-review-refuse-without-sending-or-overwriting
  (with-ready
    (fn [{:keys [client ready parent] :as environment}]
      (repo/write! parent "local.md" (object/utf8 "New local draft\n"))
      (with-redefs [git/send-http! (fn [& _] (throw (AssertionError. "Stale review sent")))]
        (is (thrown? clojure.lang.ExceptionInfo
                     (call environment :apply {:client-id client :ready-id (parse-uuid (:ready-id ready))}))))
      (is (= "New local draft\n" (String. ^bytes (repo/read parent "local.md") "UTF-8")))
      (is (= :closed (:status (call environment :discard {:client-id client})))))))

(deftest reconciliation-records-new-upstream-evidence-without-claiming-the-send-succeeded
  (doseq [remote [:new :old :other]]
    (with-ready
      (fn [{:keys [client ready conn room-id] :as environment}]
        (let [cursor (integrations/current-in @conn room-id) sends (atom 0)
              oid (case remote :new (:git-oid ready) :old (:before-git-oid ready)
                        :other (apply str (repeat 40 "a")))]
          (with-redefs [git/send-http! (fn [& _] (swap! sends inc) (throw (ex-info "Unknown send outcome" {})))
                        http/ls-remote! (fn [& _] [{:ref "refs/heads/main" :oid oid}])]
            (is (thrown? clojure.lang.ExceptionInfo
                         (call environment :apply {:client-id client :ready-id (parse-uuid (:ready-id ready))})))
            (if (= :other remote)
              (do
                (is (thrown? clojure.lang.ExceptionInfo (call environment :recover {:client-id client})))
                (is (= :unconfirmed (:status (call environment :observe {:client-id client}))))
                (is (= cursor (integrations/current-in @conn room-id))))
              (let [result (call environment :recover {:client-id client})]
                (is (= :closed (:status result)))
                (is (= (if (= :new remote) :upstream-observed :upstream-unchanged) (:outcome result)))
                (is (= (if (= :new remote) (:git-oid ready) (:github.integration/oid cursor))
                       (:github.integration/oid (integrations/current-in @conn room-id))))
                (is (nil? (journal/active-in @conn room-id)))))
            (is (= 1 @sends))))))))

(deftest an-edit-during-remote-advertisement-cannot-be-blessed-as-the-reviewed-basis
  (#'fixture/with-imported
   (fn [{:keys [parent stamp creator room-id conn] :as environment}]
     (repo/write! parent "local.md" (object/utf8 "First saved draft\n"))
     (repo/stage-all! parent)
     (repo/commit! parent {:message "Local saved draft"})
     (let [client (random-uuid)]
       (with-redefs [git/prepare-http!
                     (fn [_ _conn _url head options]
                       ;; Simulate an autosave while remote discovery blocked,
                       ;; before the pure projection captures its immutable DB.
                       (repo/write! parent "local.md" (object/utf8 "Concurrent saved draft\n"))
                       (repo/stage-all! parent)
                       (repo/commit! parent {:message "Concurrent autosave"})
                       (assoc (git/prepare parent head options) :capabilities ["report-status"]))]
         (is (thrown? clojure.lang.ExceptionInfo
                      (call environment :prepare {:client-id client :expected-target-stamp stamp}))))
       (is (= :creating (:github.sync/state (journal/sync-in @conn (journal/request-id creator room-id client)))))
       (is (= "Concurrent saved draft\n" (String. ^bytes (repo/read parent "local.md") "UTF-8")))
       (is (= :closed (:status (call environment :discard {:client-id client}))))))))

(deftest changing-room-origin-cannot-reuse-a-connection-for-another-repository
  (#'fixture/with-imported
   (fn [{:keys [parent conn room-id stamp] :as environment}]
     (repo/write! parent "local.md" (object/utf8 "Saved local work\n"))
     (repo/stage-all! parent)
     (repo/commit! parent {:message "Saved local work"})
     (d/transact conn [[:db/add [:room/id room-id] :room/github-source "different/repository"]])
     (repo/set-config! parent "remote.origin.url" "https://github.com/different/repository.git")
     (with-redefs [git/prepare-http! (fn [& _] (throw (AssertionError. "Cross-repository cursor reused")))]
       (is (thrown? clojure.lang.ExceptionInfo
                    (call environment :prepare {:client-id (random-uuid) :expected-target-stamp stamp})))))))

(deftest a-completed-send-is-recorded-after-permission-revocation-or-app-retirement
  (doseq [change [:revocation :retirement]]
    (with-ready
      (fn [{:keys [client ready creator room-id conn owner] :as environment}]
        (with-redefs [git/send-http!
                      (fn [_ _ prepared _]
                        (case change
                          :revocation (d/transact conn [[:db/add [:party/id creator] :actor/kind :agent]])
                          :retirement (is (not= ::timeout (deref (future (lifetime/fence! owner)) 5000 ::timeout))))
                        {:status :pushed :ref (:ref prepared) :old (:old prepared) :new (:new prepared)})]
          (let [result (call environment :apply {:client-id client :ready-id (parse-uuid (:ready-id ready))})
                row (journal/sync-in @conn (journal/request-id creator room-id client))]
            (is (= :pushed (:status result)))
            (is (= :pushed (:github.sync/state row)))
            (is (= (:git-oid ready) (:github.integration/oid (integrations/current-in @conn room-id))))))))))

(deftest a-new-owner-can-close-unsent-preparation-without-impersonating-its-creator
  (with-ready
    (fn [{:keys [client creator room-id conn w] :as environment}]
      (let [id (journal/request-id creator room-id client)]
        (d/transact conn [[:db/add [:room/id room-id] :room/owner [:party/id (:owner w)]]
                          [:db/add [:party/id creator] :actor/kind :agent]])
        (is (= :closed (:status (call (assoc environment :creator (:owner w)) :retire {:sync-id id}))))
        (is (= creator (:github.sync/by (journal/sync-in @conn id))))
        (is (nil? (journal/active-in @conn room-id)))))))

(deftest credential-rotation-or-revocation-after-ready-cannot-send
  (#'fixture/with-imported
   (fn [{:keys [parent conn room-id creator stamp]}]
     (d/transact conn (concat credentials/schema accounts/schema))
     (repo/write! parent "local.md" (object/utf8 "Saved outbound work\n"))
     (repo/stage-all! parent)
     (repo/commit! parent {:message "Saved outbound work"})
     (let [connection-id (:github.integration/connection-id (integrations/current-in @conn room-id))
           encrypted {:nonce "nonce" :ciphertext "ciphertext"} account-id (random-uuid) account-revision (random-uuid) sends (atom 0)
           client (str (random-uuid))
           invoke (fn [action request] ((service/prepare creator room-id action request)))]
       (d/transact conn (accounts/replace-tx @conn creator account-id nil account-revision "GitHub" encrypted))
       (d/transact conn (credentials/bind-tx @conn creator connection-id nil (random-uuid) account-id account-revision))
       (with-redefs [envelope/decrypt (fn [& _] {:version 1 :kind :github-account :by creator :account-id account-id
                                                 :token (apply str (repeat 24 "x"))})
                     git/prepare-http! (fn [_ _ _ head options]
                                         (assoc (git/prepare parent head options) :capabilities ["report-status"]))
                     git/send-http! (fn [& _] (swap! sends inc) (throw (AssertionError. "Changed credential sent")))]
         (let [ready (invoke :prepare-push {:client-id client :expected-target-stamp (str stamp)})
               request {:client-id client :ready-id (:ready-id ready)}
               row (accounts/current-in @conn creator account-id)]
           (is (= :ready (:status ready)))
           (d/transact conn (accounts/replace-tx @conn creator account-id row (random-uuid) "GitHub" encrypted))
           (is (thrown? clojure.lang.ExceptionInfo (invoke :apply request)))
           (let [row (accounts/current-in @conn creator account-id)]
             (d/transact conn (accounts/revoke-tx @conn creator account-id row (random-uuid))))
           (is (thrown? clojure.lang.ExceptionInfo (invoke :apply request)))
           (is (zero? @sends))
           (is (= :ready (:status (invoke :observe {:client-id client}))))
           (is (= :closed (:status (invoke :discard {:client-id client}))))))))))
