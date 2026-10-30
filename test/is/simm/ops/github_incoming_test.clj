(ns is.simm.ops.github-incoming-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [datahike.versioning :as versioning]
            [dvergr.system.rooms :as rooms]
            [geschichte.git.object :as object]
            [geschichte.repo :as repo]
            [geschichte.query :as query]
            [geschichte.workspace :as workspace]
            [is.simm.model.github-integrations :as integrations]
            [is.simm.model.github-sync :as journal]
            [is.simm.ops.github-host :as host]
            [is.simm.ops.github-decisions :as decisions]
            [is.simm.ops.github-incoming :as incoming]
            [is.simm.ops.github-sync-test :as fixtures]
            [is.simm.ops.github-native :as native]
            [is.simm.ops.github-sync-workspace :as private]
            [is.simm.ops.proposal-publication :as publication]
            [is.simm.ops.proposals :as proposals]))

(use-fixtures :each
  (fn [f]
    (with-redefs-fn {#'rooms/storage-gc-homes (atom #{})
                    #'rooms/storage-gc-admission (atom {:ever-admitted? false :prohibited-by nil})} f)))

(defn- call [environment action request]
  ((host/capture (:creator environment) (:room-id environment) action
                  #(incoming/in! % action request))))

(defn- decide [environment action request]
  ((host/capture (:creator environment) (:room-id environment) action
                  #(decisions/in! % action request))))

(deftest review-admission-pressure-is-not-an-unknown-settlement-outcome
  (let [party (random-uuid) room (random-uuid) request {:proposal-id (str (random-uuid))}]
    (doseq [error [(ex-info "HOST operation capacity exhausted" {:type :dvergr.runtime.preparation/capacity-exhausted})
                  (ex-info "Git permit busy" {:type :is.simm.ops.github-import/held :reason :busy})]]
      (let [calls (atom 0)]
        (with-redefs [host/capture (fn [& _] (fn [] (swap! calls inc) (throw error)))]
          (is (= {:status :busy} ((decisions/prepare party room :review request))))
          (is (= 1 @calls) "Read-only admission was not retried")
          (is (thrown? clojure.lang.ExceptionInfo
                       ((decisions/prepare party room :accept (assoc request :review-id (str (random-uuid)))))))
          (is (thrown? clojure.lang.ExceptionInfo ((decisions/prepare party room :dismiss request)))))))
    (doseq [error [(ex-info "Interrupted admission" {:type :dvergr.runtime.preparation/capacity-exhausted} (InterruptedException.))
                  (ex-info "Other failure" {:type :unrelated})]]
      (with-redefs [host/capture (fn [& _] (fn [] (throw error)))]
        (is (thrown? clojure.lang.ExceptionInfo ((decisions/prepare party room :review request))))))))

(deftest local-and-incoming-edits-file-one-frozen-proposal-without-advancing-the-cursor
  (#'fixtures/with-imported
    (fn [{:keys [parent conn room-id stamp creator] :as environment}]
      (repo/write! parent "local.md" (object/utf8 "Saved local knowledge\n"))
      (repo/stage-all! parent)
      (repo/commit! parent {:message "Local autosave"})
      (#'fixtures/c2! environment)
      (let [cursor (integrations/current-in @conn room-id)
            client (random-uuid)
            ready (call environment :prepare {:client-id client :expected-target-stamp stamp})]
        (is (= :ready (:status ready)))
        (is (= :incoming (:operation-kind ready)))
        (is (nil? (repo/read parent "reports/new.md")))
        (is (= cursor (integrations/current-in @conn room-id)))
        (let [filed (call environment :apply {:client-id client :ready-id (parse-uuid (:ready-id ready))})
              proposal (proposals/get-proposal (parse-uuid (:proposal-id filed)))
              fork (first (:proposal/forks proposal))
              row (journal/sync-in @conn (journal/request-id creator room-id client))]
          (is (= :filed (:status filed)))
          (is (= :open (:proposal/status proposal)))
          (is (:proposal/github-provenance proposal))
          (is (:proposal.fork/head-commit fork))
          (is (= :closed (:github.sync/state row)))
          (is (nil? (journal/active-in @conn room-id)))
          (is (= cursor (integrations/current-in @conn room-id)))
          (is (nil? (repo/read parent "reports/new.md")))
          (is (= "Saved local knowledge\n" (String. ^bytes (repo/read parent "local.md") "UTF-8")))
          (is (= filed (call environment :observe {:client-id client})))
          (let [again (call environment :prepare {:client-id (random-uuid) :expected-target-stamp stamp})]
            (is (= :filed (:status again)))
            (is (= (:proposal-id filed) (:proposal-id again)))
            (is (= 1 (count (filter #(= (:proposal/github-key proposal) (:proposal/github-key %))
                                    (proposals/list-proposals))))))
          (let [id (:proposal/id proposal)
                review (decide environment :review {:proposal-id id})]
            (is (= :ready (:status review)))
            ;; Even a non-file transaction invalidates the exact review token.
            (d/transact parent [[:db/add [:geschichte.repo/id (:geschichte.repo/id (query/repository @parent))]
                                 :geschichte.repo/name "unused"]])
            (is (thrown? clojure.lang.ExceptionInfo (decide environment :accept {:proposal-id id :review-id (:review-id review)})))
            (let [fresh (decide environment :review {:proposal-id id})]
              (is (= :accepted (:status (decide environment :accept {:proposal-id id :review-id (:review-id fresh)})))))
            (is (= "New upstream report\n" (String. ^bytes (repo/read parent "reports/new.md") "UTF-8")))
            (is (= "Saved local knowledge\n" (String. ^bytes (repo/read parent "local.md") "UTF-8")))
            (is (= :accepted (:proposal/status (proposals/get-proposal id))))
            (let [cursor (integrations/current-in @conn room-id)]
              (is (not= (:github.integration/head cursor) (:geschichte.commit/id (repo/head-commit parent)))
                  "Cursor names fetched upstream, not the merged local candidate")
              (is (:github.integration/sync cursor)))))))))

(deftest unchanged-upstream-closes-the-private-copy-without-a-proposal
  (#'fixtures/with-imported
    (fn [{:keys [conn room-id stamp] :as environment}]
      (let [cursor (integrations/current-in @conn room-id)
            result (call environment :prepare {:client-id (random-uuid) :expected-target-stamp stamp})]
        (is (= :closed (:status result)))
        (is (nil? (:proposal-id result)))
        (is (= cursor (integrations/current-in @conn room-id)))
        (is (nil? (journal/active-in @conn room-id)))))))

(deftest missing-base-is-an-explicit-not-started-response-before-preparation
  (#'fixtures/with-imported
    (fn [{:keys [conn room-id stamp remote-call]}]
      (d/transact conn [[:db/retractEntity [:github.integration/room room-id]]])
      (let [result (remote-call :prepare-incoming {:client-id (str (random-uuid)) :expected-target-stamp (str stamp)})]
        (is (= :not-started (:status result)))
        (is (= :integration-unverified (:reason result)))
        (is (nil? (journal/active-in @conn room-id)))))))

(deftest a-missing-filing-ack-cannot-be-replaced-by-journal-readback
  (#'fixtures/with-imported
    (fn [{:keys [stamp conn room-id] :as environment}]
      (#'fixtures/c2! environment)
      (let [client (random-uuid) ready (call environment :prepare {:client-id client :expected-target-stamp stamp})
            transact d/transact transition journal/transition-tx entered (atom false) native-calls (atom 0)]
        (with-redefs [journal/transition-tx (fn [& args]
                                            (let [tx (apply transition args)]
                                              (when (= :filing (nth args 2)) (reset! entered true)) tx))
                      d/transact (fn [& args]
                                   (let [before @entered report (apply transact args)]
                                     (when-not (and (not before) @entered) report)))
                      native/publish! (fn [& _] (swap! native-calls inc) (throw (AssertionError. "Publication dispatched")))]
          (is (thrown? clojure.lang.ExceptionInfo
                       (call environment :apply {:client-id client :ready-id (parse-uuid (:ready-id ready))}))))
        (is (zero? @native-calls))
        (is (= :filing (:github.sync/state (journal/active-in @conn room-id))))
        (is (= :unconfirmed (:status (call environment :observe {:client-id client}))))))))

(deftest recovering-positive-filing-retries-the-same-canonical-chat-card
  (#'fixtures/with-imported
    (fn [{:keys [stamp conn room-id] :as environment}]
      (#'fixtures/c2! environment)
      (let [client (random-uuid) ready (call environment :prepare {:client-id client :expected-target-stamp stamp})
            publish publication/publish-original! calls (atom 0)]
        (with-redefs [private/retire! (fn [& _] (throw (ex-info "Fixture cleanup unavailable" {})))]
          (is (thrown? clojure.lang.ExceptionInfo
                       (call environment :apply {:client-id client :ready-id (parse-uuid (:ready-id ready))}))))
        (is (= :closing (:github.sync/state (journal/active-in @conn room-id))))
        (let [recovered (with-redefs [publication/publish-original! (fn [& args] (swap! calls inc) (apply publish args))]
                          (call environment :recover {:client-id client}))]
          (is (= :filed (:status recovered)))
          (is (= 1 @calls))
          (is (= :published (:proposal/message-status (proposals/get-proposal (parse-uuid (:proposal-id recovered)))))))))))

(deftest recovery-retries-chat-publication-after-the-private-copy-is-already-closed
  (#'fixtures/with-imported
    (fn [{:keys [stamp conn room-id] :as environment}]
      (#'fixtures/c2! environment)
      (let [client (random-uuid) ready (call environment :prepare {:client-id client :expected-target-stamp stamp})]
        (with-redefs [publication/publish-original! (fn [& _] (throw (ex-info "Fixture interruption after closure" {})))]
          (is (thrown? clojure.lang.ExceptionInfo
                       (call environment :apply {:client-id client :ready-id (parse-uuid (:ready-id ready))}))))
        (is (nil? (journal/active-in @conn room-id)))
        (let [observed (call environment :observe {:client-id client})
              id (parse-uuid (:proposal-id observed))
              message (:proposal/message-id (proposals/get-proposal id))
              recovered (call environment :recover {:client-id client})
              proposal (proposals/get-proposal id)]
          (is (= :filed (:status recovered)))
          (is (= (:proposal-id observed) (:proposal-id recovered)))
          (is (= message (:proposal/message-id proposal)))
          (is (= :published (:proposal/message-status proposal))))))))

(deftest one-ui-sync-request-files-a-proposal-without-an-extra-confirmation
  (#'fixtures/with-imported
    (fn [{:keys [stamp remote-call parent] :as environment}]
      (#'fixtures/c2! environment)
      (let [result (remote-call :prepare-incoming {:client-id (str (random-uuid)) :expected-target-stamp (str stamp)})]
        (is (= :filed (:status result)))
        (is (= :open (:proposal/status (proposals/get-proposal (parse-uuid (:proposal-id result))))))
        (is (nil? (repo/read parent "reports/new.md")))))))

(deftest a-known-publication-refusal-releases-only-its-own-reservation-for-a-new-attempt
  (#'fixtures/with-imported
    (fn [{:keys [stamp conn room-id parent] :as environment}]
      (#'fixtures/c2! environment)
      (let [client (random-uuid) ready (call environment :prepare {:client-id client :expected-target-stamp stamp})
            merge-var #'workspace/merge-data merge-data @merge-var]
        (with-redefs-fn
          {merge-var (fn [target parents tx]
                       (d/transact parent [[:db/add [:geschichte.repo/id (:geschichte.repo/id (query/repository @parent))]
                                            :geschichte.repo/name "Concurrent metadata edit"]])
                       (merge-data target parents tx))}
          #(is (thrown? clojure.lang.ExceptionInfo
                        (call environment :apply {:client-id client :ready-id (parse-uuid (:ready-id ready))}))))
        (let [held (call environment :observe {:client-id client})]
          (is (= :held (:status held)))
          (is (= :target-changed (:reason held)))
          (is (:can-discard? held)))
        (is (= :closed (:status (call environment :discard {:client-id client}))))
        (is (nil? (journal/active-in @conn room-id)))
        (let [next-client (random-uuid) prepared (call environment :prepare {:client-id next-client :expected-target-stamp stamp})
              filed (call environment :apply {:client-id next-client :ready-id (parse-uuid (:ready-id prepared))})
              proposal (proposals/get-proposal (parse-uuid (:proposal-id filed)))]
          (is (= :filed (:status filed)))
          (is (nil? (:proposal/github-replaces proposal)) "A refused publication never created a Proposal to replace")
          (is (nil? (repo/read parent "reports/new.md"))))))))
