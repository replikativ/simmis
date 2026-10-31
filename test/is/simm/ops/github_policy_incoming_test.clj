(ns is.simm.ops.github-policy-incoming-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [dvergr.system.rooms :as rooms]
            [geschichte.repo :as repo]
            [geschichte.git.http :as http]
            [geschichte.git.object :as object]
            [geschichte.git.project :as project]
            [geschichte.query :as query]
            [is.simm.model.git-policy :as policy]
            [is.simm.model.github-sync :as journal]
            [is.simm.model.github-integrations :as integrations]
            [is.simm.ops.github-host :as host]
            [is.simm.ops.github-incoming :as incoming]
            [is.simm.ops.github-native :as native]
            [is.simm.ops.github-merge :as merge]
            [is.simm.ops.github-service :as service]
            [is.simm.ops.github-policy :as settings]
            [is.simm.ops.github-sync-test :as fixtures]))

(use-fixtures :each
  (fn [f]
    (with-redefs-fn {#'rooms/storage-gc-homes (atom #{})
                     #'rooms/storage-gc-admission (atom {:ever-admitted? false :prohibited-by nil})} f)))

(defn- call [environment action request]
  ((host/capture (:creator environment) (:room-id environment) action
                 #(incoming/in! (incoming/admitted-context
                                 (cond-> (assoc % :incoming-kind :incoming-policy)
                                   (:poll-policy environment) (assoc :poll-policy (:poll-policy environment))
                                   (:git-operation environment) (assoc :git-operation (:git-operation environment)))) action request))))

(deftest settings-use-the-inspected-target-connection-and-configuration-revision
  (#'fixtures/with-imported
   (fn [{:keys [conn room-id stamp creator]}]
     (d/transact conn (concat policy/schema journal/schema))
     (let [invoke (fn [operation request] ((settings/prepare creator room-id operation request)))
           read-request {:expected-target-stamp (str stamp)}
           initial (invoke :status read-request)
           request {:connection-id (:connection-id initial) :revision (:revision initial)
                    :settings {:enabled? true :interval-seconds 60 :review-mode :review-all}}
           saved (invoke :save request)]
       (is (false? (:enabled? initial)))
       (is (= :fast-forward (:review-mode initial)))
       (is (true? (:enabled? saved)))
       (is (= :review-all (:review-mode saved)))
       (is (not= (:revision initial) (:revision saved)))
       (is (thrown? clojure.lang.ExceptionInfo (invoke :save request)))
       (is (thrown? clojure.lang.ExceptionInfo
                    (invoke :save (assoc request :revision (:revision saved) :connection-id (str (random-uuid))))))
       (is (thrown? clojure.lang.ExceptionInfo
                    (invoke :status {:expected-target-stamp (str (random-uuid))})))
       (is (= saved (invoke :status read-request)))))))

(deftest proven-fast-forward-applies-the-imported-head-and-settles-the-control-cursor
  (#'fixtures/with-imported
   (fn [{:keys [parent conn room-id stamp creator] :as environment}]
     (d/transact conn (concat policy/schema journal/schema))
     (#'fixtures/c2! environment)
     (let [before (integrations/current-in @conn room-id) client (random-uuid)
           ready (call environment :prepare {:client-id client :expected-target-stamp stamp})
           id (journal/request-id creator room-id client)]
       (is (= :ready (:status ready)))
       (is (= :incoming-policy (:operation-kind ready)))
       (is (= :fast-forward (:github.sync/disposition (journal/sync-in @conn id))))
       (is (= before (integrations/current-in @conn room-id)))
       (is (nil? (repo/read parent "reports/new.md")))
       (let [applied (call environment :apply {:client-id client :ready-id (parse-uuid (:ready-id ready))})
             after (integrations/current-in @conn room-id)]
         (is (= :applied (:status applied)))
         (is (= "New upstream report\n" (String. ^bytes (repo/read parent "reports/new.md") "UTF-8")))
         (is (= (:github.integration/head after) (:geschichte.commit/id (repo/head-commit parent))))
         (is (not= before after))
         (is (= :closed (:github.sync/state (journal/sync-in @conn id))))
         (is (nil? (journal/active-in @conn room-id)))
         (is (nil? (:proposal-id applied))))))))

(defn- configure! [conn room by mode]
  (let [connection (:github.integration/connection-id (integrations/current-in @conn room))
        current (policy/effective @conn room connection)]
    (d/transact conn (policy/configure-tx @conn room connection (:git.policy/revision current) (random-uuid) by
                                          {:enabled? false :interval-seconds 60 :review-mode mode} (java.util.Date.)))))

(deftest a-new-review-policy-blocks-application-but-the-old-ready-remains-discardable
  (#'fixtures/with-imported
   (fn [{:keys [conn room-id creator stamp parent] :as environment}]
     (d/transact conn (concat policy/schema journal/schema))
     (#'fixtures/c2! environment)
     (let [client (random-uuid) ready (call environment :prepare {:client-id client :expected-target-stamp stamp})
           advances (atom 0)]
       (configure! conn room-id creator :review-all)
       (with-redefs [native/advance! (fn [& _] (swap! advances inc))]
         (is (thrown? clojure.lang.ExceptionInfo
                      (call environment :apply {:client-id client :ready-id (parse-uuid (:ready-id ready))}))))
       (is (zero? @advances))
       (is (nil? (repo/read parent "reports/new.md")))
       (is (= :closed (:status (call environment :discard {:client-id client}))))
       (is (nil? (journal/active-in @conn room-id)))))))

(deftest a-lost-applying-ack-never-authorizes-advancement-or-discard
  (#'fixtures/with-imported
   (fn [{:keys [conn room-id creator stamp] :as environment}]
     (d/transact conn (concat policy/schema journal/schema))
     (#'fixtures/c2! environment)
     (let [client (random-uuid) ready (call environment :prepare {:client-id client :expected-target-stamp stamp})
           id (journal/request-id creator room-id client) lost? (atom false) advances (atom 0)
           transact d/transact request {:client-id client :ready-id (parse-uuid (:ready-id ready))}]
       (with-redefs [native/advance! (fn [& _] (swap! advances inc))
                     d/transact (fn [& args]
                                  (let [result (apply transact args)]
                                    (when (and (= :applying (:github.sync/state (journal/sync-in (:db-after result) id)))
                                               (compare-and-set! lost? false true))
                                      (throw (ex-info "Lost Applying ACK" {})))
                                    result))]
         (is (thrown? clojure.lang.ExceptionInfo (call environment :apply request)))
         (is (= :applying (:github.sync/state (journal/sync-in @conn id))))
         (doseq [action [:apply :recover :discard]]
           (is (thrown? clojure.lang.ExceptionInfo
                        (call environment action (if (= :apply action) request {:client-id client})))))
         (is (zero? @advances)))))))

(deftest a-positive-advance-is-recorded-even-if-the-human-is-revoked-before-return
  (#'fixtures/with-imported
   (fn [{:keys [conn room-id creator stamp parent] :as environment}]
     (d/transact conn (concat policy/schema journal/schema))
     (#'fixtures/c2! environment)
     (let [client (random-uuid) ready (call environment :prepare {:client-id client :expected-target-stamp stamp})
           before (integrations/current-in @conn room-id) advance native/advance!]
       (with-redefs [native/advance! (fn [& args]
                                       (let [result (apply advance args)]
                                         (d/transact conn [[:db/add [:party/id creator] :actor/kind :agent]])
                                         result))]
         ;; Cleanup needs current authority; the admitted effect's fact doesn't.
         (is (thrown? clojure.lang.ExceptionInfo
                      (call environment :apply {:client-id client :ready-id (parse-uuid (:ready-id ready))}))))
       (is (= :applied (:github.sync/state (journal/sync-in @conn (journal/request-id creator room-id client)))))
       (is (not= before (integrations/current-in @conn room-id)))
       (is (= "New upstream report\n" (String. ^bytes (repo/read parent "reports/new.md") "UTF-8")))))))

(deftest review-all-files-a-proposal-and-an-open-pointer-suppresses-another-fetch
  (#'fixtures/with-imported
   (fn [{:keys [conn room-id creator stamp parent] :as environment}]
     (d/transact conn (concat policy/schema journal/schema))
     (#'fixtures/c2! environment)
     (configure! conn room-id creator :review-all)
     (let [cursor (integrations/current-in @conn room-id)
           invoke (fn [] ((service/prepare creator room-id :prepare-policy
                                           {:client-id (str (random-uuid)) :expected-target-stamp (str stamp)})))
           filed (invoke)]
       (is (= :filed (:status filed)))
       (is (some? (parse-uuid (:proposal-id filed))))
       (is (= cursor (integrations/current-in @conn room-id)))
       (is (nil? (repo/read parent "reports/new.md")))
       (with-redefs [http/fetch! (fn [& _] (throw (AssertionError. "An open proposal must suppress fetching")))]
         (is (= (:proposal-id filed) (:proposal-id (invoke)))))
       (is (nil? (journal/active-in @conn room-id)))))))

(deftest consecutive-automatic-updates-retain-a-usable-upstream-base
  (#'fixtures/with-imported
   (fn [{:keys [conn room-id creator stamp parent upstream graph] :as environment}]
     (d/transact conn (concat policy/schema journal/schema))
     (let [plan merge/plan-policy-fetched! advance native/advance!
           invoke (fn []
                    (let [planned (atom []) advanced (atom [])
                          result (with-redefs [merge/plan-policy-fetched!
                                               (fn [conn fetched options]
                                                 (swap! planned conj (:operation options))
                                                 (plan conn fetched options))
                                               native/advance!
                                               (fn [& args]
                                                 (swap! advanced conj (nth args 4))
                                                 (apply advance args))]
                                   ((service/prepare creator room-id :prepare-policy
                                                     {:client-id (str (random-uuid))
                                                      :expected-target-stamp (str stamp)})))]
                      (if (= :applied (:status result))
                        (do (is (= 1 (count @planned)))
                            (is (= 1 (count @advanced)))
                            (is (identical? (first @planned) (first @advanced))))
                        (is (empty? @advanced) "Unchanged remote never advances"))
                      result))]
       (#'fixtures/c2! environment)
       (is (= :applied (:status (invoke))))
       (repo/write! upstream "reports/third.md" (object/utf8 "Third report\n"))
       (repo/stage-all! upstream)
       (let [commit (repo/commit! upstream {:message "C3"})]
         (reset! graph (project/project upstream (:geschichte.commit/id commit))))
       (is (= :applied (:status (invoke))))
       (is (= "Third report\n" (String. ^bytes (repo/read parent "reports/third.md") "UTF-8")))
       (is (= :unchanged (:status (invoke))))
       (is (nil? (journal/active-in @conn room-id)))))))

(deftest disabling-polling-blocks-its-prepared-effect-but-not-discard
  (#'fixtures/with-imported
   (fn [{:keys [conn room-id creator stamp parent] :as environment}]
     (d/transact conn (concat policy/schema journal/schema))
     (#'fixtures/c2! environment)
     (let [connection (:github.integration/connection-id (integrations/current-in @conn room-id))
           default (policy/effective @conn room-id connection)]
       (d/transact conn (policy/configure-tx @conn room-id connection (:git.policy/revision default) (random-uuid) creator
                                             {:enabled? true :interval-seconds 60 :review-mode :fast-forward} (java.util.Date.)))
       (let [frozen (policy/effective @conn room-id connection) environment (assoc environment :poll-policy frozen)
             client (random-uuid) ready (call environment :prepare {:client-id client :expected-target-stamp stamp})
             advances (atom 0)]
         (configure! conn room-id creator :fast-forward)
         (is (= (policy/review-token frozen) (policy/review-token (policy/effective @conn room-id connection)))
             "Disabling polling preserves manual review tokens")
         (with-redefs [native/advance! (fn [& _] (swap! advances inc))]
           (is (thrown? clojure.lang.ExceptionInfo
                        (call environment :apply {:client-id client :ready-id (parse-uuid (:ready-id ready))}))))
         (is (zero? @advances))
         (is (nil? (repo/read parent "reports/new.md")))
         (is (= :closed (:status (call environment :discard {:client-id client})))))))))

(deftest changing-policy-after-filing-admission-refuses-native-publication-and-keeps-discard
  (#'fixtures/with-imported
   (fn [{:keys [conn room-id creator stamp parent] :as environment}]
     (d/transact conn (concat policy/schema journal/schema))
     (#'fixtures/c2! environment)
     (configure! conn room-id creator :review-all)
     (let [client (random-uuid) ready (call environment :prepare {:client-id client :expected-target-stamp stamp})
           id (journal/request-id creator room-id client) changed? (atom false) transact d/transact]
       (with-redefs [d/transact (fn [& args]
                                  (let [result (apply transact args)]
                                    (when (and (= :filing (:github.sync/state (journal/sync-in (:db-after result) id)))
                                               (compare-and-set! changed? false true))
                                      (configure! conn room-id creator :fast-forward))
                                    result))]
         (is (thrown? clojure.lang.ExceptionInfo
                      (call environment :apply {:client-id client :ready-id (parse-uuid (:ready-id ready))}))))
       (is @changed?)
       (is (= :held (:github.sync/state (journal/sync-in @conn id))))
       (is (= #{"refs/heads/main"} (set (keys (query/refs @parent)))))
       (is (= :closed (:status (call environment :discard {:client-id client}))))))))
