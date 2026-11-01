(ns is.simm.ops.owned-schedule-lifetime-test
  "Actual App tickets and Room preparation leases around fixed scheduling IO.
   All stores are disposable; no provider request or real timer is enabled."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.chat.persist :as persist]
            [is.simm.ops.work-test :as room-fixture]
            [is.simm.ops.owned-room-operation :as room-operation]
            [org.replikativ.spindel.engine.core :as ec]
            [dvergr.model.registry :as models]
            [dvergr.jobs :as jobs]
            [dvergr.discourse :as discourse]
            [dvergr.runtime.preparation :as preparation]
            [is.simm.model.workflow-schedules :as schedules]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.knowledge-inputs :as knowledge]
            [is.simm.ops.knowledge-filing :as filing]
            [is.simm.ops.knowledge-publish :as publication]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflow-runs-test :as saved]
            [is.simm.ops.owned-schedule-occurrences-test :as occurrences]
            [is.simm.ops.owned-schedule-admission-test :as fixture]
            [is.simm.ops.workflow-schedules :as operations]
            [is.simm.ops.workflows :as workflows]
            [is.simm.runtimes.operation-lifetime :as lifetime])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(deftest activation-source-read-keeps-its-original-app-and-room-through-physical-exit
  (doseq [retire [:app :room]]
    (@#'fixture/with-fixture
     true
     (fn [{:keys [conn w host room input app-owner]}]
       (let [request (input :workflow) snapshot workflows/snapshot-in
             entered (promise) release (CountDownLatch. 1) worker (atom nil) ticket (atom nil)]
         (try
           (with-redefs [workflows/snapshot-in
                         (fn [& args]
                           (reset! ticket lifetime/*ticket*)
                           (deliver entered true)
                           ;; Logical interruption cannot release a native store
                           ;; while its physical IO body still uses the handle.
                           (loop []
                             (when-not (try (.await release 100 TimeUnit/MILLISECONDS)
                                            (catch InterruptedException _ false))
                               (recur)))
                           (apply snapshot args))
                         models/get-model (fn [& _] (throw (AssertionError. "Retired source may not prepare models")))]
             (reset! worker (future (try (operations/create-owned! host (ops/party-principal (:owner w))
                                                                  (str (:room w)) request)
                                        :unexpected (catch Exception _ :held))))
             (is (= true (deref entered 15000 ::expired)))
             (let [leases (vec (vals (get @(:meta room) preparation/leases-key)))]
               (is (= 1 (count leases)))
               (is (lifetime/retained? @ticket))
               (case retire
                 :app (do (lifetime/fence! app-owner)
                          (is (= :physical-exit-unconfirmed
                                 (try (lifetime/join! app-owner 0) nil
                                      (catch clojure.lang.ExceptionInfo error (:reason (ex-data error)))))))
                 :room (doseq [lease leases]
                         (preparation/cancel! lease)
                         (is (= :dvergr.runtime.preparation/unconfirmed-drain
                                (try ((:join! lease) 0) nil
                                     (catch clojure.lang.ExceptionInfo error (:type (ex-data error))))))))
               (.countDown release)
               (is (= :held (deref @worker 15000 ::expired)))
               (is (not (lifetime/retained? @ticket)))
               (doseq [lease leases] (is (nil? ((:join! lease) 1000))))
               (is (empty? (get @(:meta room) preparation/leases-key)))
               (is (empty? (schedules/activations conn)))))
           (finally (.countDown release) (when @worker (deref @worker 15000 nil)))))))))

(defn- block-io! [entered release ticket]
  (reset! ticket lifetime/*ticket*)
  (deliver entered true)
  (loop []
    (when-not (try (.await ^CountDownLatch release 100 TimeUnit/MILLISECONDS)
                   (catch InterruptedException _ false))
      (recur))))

(defn- cancel-room-io! [room release worker ticket]
  (let [leases (vec (vals (get @(:meta room) preparation/leases-key)))]
    (is (= 1 (count leases)))
    (is (lifetime/retained? @ticket))
    (doseq [lease leases]
      (preparation/cancel! lease)
      (is (= :dvergr.runtime.preparation/unconfirmed-drain
             (try ((:join! lease) 0) nil
                  (catch clojure.lang.ExceptionInfo error (:type (ex-data error)))))))
    (.countDown ^CountDownLatch release)
    (is (contains? #{:returned :held} (deref worker 15000 ::expired)))
    (is (not (lifetime/retained? @ticket)))
    (doseq [lease leases] (is (nil? ((:join! lease) 1000))))
    (is (empty? (get @(:meta room) preparation/leases-key)))))

(deftest live-evidence-capture-is-joined-before-room-exit-and-never-prepares-a-provider
  (@#'occurrences/with-occurrence true saved/knowledge-source
   (fn [{:keys [conn host room due]}]
     (let [capture knowledge/capture entered (promise) release (CountDownLatch. 1)
           ticket (atom nil) worker (atom nil)]
       (try
         (with-redefs [knowledge/capture (fn [& args] (block-io! entered release ticket) (apply capture args))
                       work/prepare (fn [& _] (throw (AssertionError. "Cancelled capture may not prepare provider")))
                       jobs/start! (fn [& _] (throw (AssertionError. "Cancelled capture may not admit a Job")))]
           (reset! worker (future (try (operations/tick-owned! host due) :returned (catch Exception _ :held))))
           (is (= true (deref entered 15000 ::expired)))
           (cancel-room-io! room release @worker ticket)
           (is (empty? (requests/requests conn))))
         (finally (.countDown release) (when @worker (deref @worker 15000 nil))))))))

(defn- settled-request-only
  "Synthetic trusted journal producer. No Job, Run, wallet or Attempt starts;
   this fixture tests lifetime placement around completion IO, not execution."
  [conn _ principal room-id _ input prepared]
  (let [{:keys [request]} (requests/claim! conn {:party (work/authorize! conn principal room-id)
                                               :room room-id :client-id (:request-id input)
                                               :kind (:request-kind prepared) :intent (:basis prepared)
                                               :limits (update (:limits input) :settlement keyword)})
        id (:work.request/id request)]
    (requests/record-job! conn id (:work.request/claim request) (random-uuid))
    (requests/mark-returned! conn id)))

(deftest completion-observation-and-attempt-reads-have-physical-room-ownership
  (doseq [stage [:job :attempt]]
    (@#'occurrences/with-occurrence true saved/knowledge-source
     (fn [{:keys [conn host room due row]}]
       (with-redefs [work/submit-prepared! settled-request-only]
         (operations/tick-owned! host due))
       (let [activation (schedules/activation conn (:schedule.activation/id row))
             oid (:schedule.activation/active-occurrence activation)
             occurrence (schedules/occurrence conn oid)
             request (requests/get-request conn (:schedule.occurrence/request occurrence))
             entered (promise) release (CountDownLatch. 1) ticket (atom nil) worker (atom nil)
             completed {:id (:work.request/job request) :status :completed :settled? true :finished-at due}]
         (is (= :linked (:schedule.occurrence/state occurrence)))
         (try
           (with-redefs [jobs/status (fn [& _]
                                      (when (= :job stage) (block-io! entered release ticket))
                                      completed)
                         discourse/post! (fn [_ message]
                                           (when (= :message stage) (block-io! entered release ticket))
                                           message)
                         filing/candidate-outcome
                         (fn [& _]
                           (when (= :attempt stage) (block-io! entered release ticket))
                           ;; No synthetic success is allowed past this native
                           ;; reader seam. An unavailable Attempt stays held.
                           (throw (ex-info "Fixture native Attempt unavailable" {})))
                         work/prepare (fn [& _] (throw (AssertionError. "Completion may not prepare another provider")))]
             (reset! worker (future (try (operations/tick-owned! host due) :returned (catch Exception _ :held))))
             (is (= true (deref entered 15000 ::expired)))
             (cancel-room-io! room release @worker ticket)
             (let [current (schedules/activation conn (:schedule.activation/id row))]
               (is (= oid (:schedule.activation/active-occurrence current)))
               (is (nil? (:schedule.activation/last-processed-input current)))))
           (finally (.countDown release) (when @worker (deref @worker 15000 nil)))))))))

(deftest completion-subscriber-can-fork-before-notification-acknowledgement
  (@#'occurrences/with-occurrence true saved/knowledge-source
   (fn [{:keys [conn host room due]}]
     (with-redefs [work/submit-prepared! settled-request-only]
       (operations/tick-owned! host due))
     (let [observed (promise) mark! schedules/mark-announced!
           completed {:status :completed :settled? true :finished-at due}]
       (discourse/on-each-message
        room (fn [message]
               (when (= :workflow/completed (get-in message [:metadata :kind]))
                 (deliver observed
                          (future
                            (room-operation/with-app!
                             host
                             (fn []
                               (binding [ec/*execution-context* (:ctx room)]
                                 (try
                                   (let [child (discourse/fork-room room {:isolation :none :clone-participants? false})]
                                     (discourse/discard child)
                                     :forked)
                                   (catch Exception error (:type (ex-data error))))))))))))
       (with-redefs [jobs/status (constantly completed)
                     filing/candidate-outcome (fn [& _] (throw (ex-info "Fixture Attempt unavailable" {})))
                     schedules/mark-announced!
                     (fn [connection oid]
                       (is (empty? (get @(:meta room) preparation/leases-key)))
                       ;; Force reaction BEFORE the posting phase can finish.
                       ;; A source lease here would lose the fork reaction.
                       (let [worker (deref observed 10000 ::missing)]
                         (is (future? worker))
                         (when (future? worker) (is (= :forked (deref worker 10000 ::blocked)))))
                       (mark! connection oid))]
         (operations/tick-owned! host due))))))

(deftest native-post-storage-is-joined-before-room-close-without-a-preparation-lease
  (@#'room-fixture/with-room
   (fn [room]
     (let [message (discourse/message :fixture :_room-log "Native storage ownership" nil {:role :system})
           entered (promise) close-entered (promise) release (CountDownLatch. 1)
           persist! persist/persist-tx-result! poster (atom nil) closer (atom nil)]
       (try
         (with-redefs [persist/persist-tx-result!
                       (fn [connection tx options]
                         (when (= (:id message) (:msg-id options))
                           ;; This hook is called under the actual bus commit
                           ;; lock AFTER admission/enqueue, not before post!.
                           (deliver entered true)
                           (loop []
                             (when-not (try (.await release 100 TimeUnit/MILLISECONDS)
                                            (catch InterruptedException _ false))
                               (recur))))
                         (persist! connection tx options))]
           (reset! poster (future (binding [ec/*execution-context* (:ctx room)] (discourse/post! room message))))
           (is (= true (deref entered 15000 ::expired)))
           (is (empty? (get @(:meta room) preparation/leases-key)))
           (reset! closer (future (deliver close-entered true)
                                  (discourse/close-room! room {:close-context? false}) :closed))
           (is (= true (deref close-entered 5000 ::expired)))
           (is (= ::still-owned (deref @closer 50 ::still-owned)))
           (.countDown release)
           (is (= (:id message) (:id (deref @poster 15000 ::expired))))
           (is (= :closed (deref @closer 15000 ::expired)))
           (is (= 1 (d/q (quote [:find (count ?e) . :in $ ?id :where [?e :message/id ?id]])
                           @(get-in room [:store :conn]) (:id message)))))
         (finally (.countDown release)
                  (when @poster (deref @poster 15000 nil))
                  (when @closer (deref @closer 15000 nil))))))))

(deftest completion-observer-fencing-app-prevents-the-next-proposal-notification
  (@#'fixture/with-fixture
   true
   (fn [{:keys [host app-owner]}]
     (let [posted (atom [])]
       ;; Exercise the fixed two-post boundary, independently of certification.
       ;; The first native notification's observer retires this exact App.
       (with-redefs-fn
         {#'operations/owned-room-current! (fn [& _] true)
          #'operations/publish-completion-target!
          (fn [& _]
            (swap! posted conj :completion)
            (is (= :fenced
                   (deref (future (lifetime/fence! app-owner) :fenced) 5000 :timeout)))
            (is (not (lifetime/ready? app-owner))))
          #'publication/announce-filed-original! (fn [& _] (swap! posted conj :proposal))}
         #(is (thrown? Exception
                       (room-operation/with-app!
                        host
                        (fn []
                          (@#'operations/announce-owned-result!
                           host nil {} nil nil nil {:settled? true} nil nil {:kind :proposed}))))))
       (is (= [:completion] @posted))))))


(deftest certified-failure-reconciles-on-the-first-completion-pass-after-announcement
  (@#'occurrences/with-occurrence true saved/knowledge-source
   (fn [{:keys [conn host room due row]}]
     (with-redefs [work/submit-prepared! settled-request-only]
       (operations/tick-owned! host due))
     (let [activation (schedules/activation conn (:schedule.activation/id row))
           o (schedules/occurrence conn (:schedule.activation/active-occurrence activation))
           request (requests/get-request conn (:schedule.occurrence/request o))
           ;; Synthetic certified-outcome reader seam, not a fabricated native
           ;; Attempt. This test isolates scheduling order around the reader.
           outcome {:outcome :failed :candidate (random-uuid) :attempt-content (random-uuid)
                    :request-binding (dispositions/request-binding request)}
           events (atom []) fail! schedules/fail-certified!]
       (with-redefs [jobs/status (constantly {:status :completed :settled? true :finished-at due})
                     filing/candidate-outcome
                     (fn [& _]
                       (is (seq (get @(:meta room) preparation/leases-key)))
                       (swap! events conj :certified)
                       outcome)
                     schedules/fail-certified!
                     (fn [c a occurrence value]
                       (is (empty? (get @(:meta room) preparation/leases-key)))
                       (is (true? (:schedule.occurrence/announced? (schedules/occurrence c (:schedule.occurrence/id occurrence)))))
                       (swap! events conj :failed)
                       (fail! c a occurrence value))]
         (operations/tick-owned! host due)
         (let [after (schedules/activation conn (:schedule.activation/id row))]
           (is (= [:certified :failed] @events))
           (is (false? (:schedule.activation/enabled? after)))
           (is (= :execution-failed (:schedule.activation/reason after)))
           (is (nil? (:schedule.activation/active-occurrence after)))
           (is (nil? (:schedule.activation/last-processed-input after))))
         (operations/tick-owned! host due)
         (is (= [:certified :failed] @events)))))))

(deftest native-failure-holds-the-exact-submitted-occurrence
  (doseq [status [:failed :cancelled]]
    (@#'occurrences/with-occurrence true saved/knowledge-source
     (fn [{:keys [conn host due row]}]
       (with-redefs [work/submit-prepared! settled-request-only]
         (operations/tick-owned! host due))
       (let [aid (:schedule.activation/id row)
             before (schedules/activation conn aid)
             oid (:schedule.activation/active-occurrence before)]
         (with-redefs [jobs/status (constantly {:status status :settled? true :finished-at due})
                       filing/candidate-outcome
                       (fn [& _] (throw (ex-info "Native failure is not a candidate Attempt" {})))]
           (operations/tick-owned! host due)
           (let [after (schedules/activation conn aid)
                 occurrence (schedules/occurrence conn oid)]
             (is (false? (:schedule.activation/enabled? after)))
             (is (= oid (:schedule.activation/last-terminal-failure after)))
             (is (nil? (:schedule.activation/active-occurrence after)))
             (is (= status (:schedule.occurrence/failed-job-status occurrence)))
             (is (nil? (:schedule.occurrence/failed-candidate occurrence)))
             (is (true? (:schedule.occurrence/announced? occurrence)))
             (is (nil? (:schedule.activation/last-processed-input after)))
             (operations/tick-owned! host due)
             (is (= after (schedules/activation conn aid))))))))))
