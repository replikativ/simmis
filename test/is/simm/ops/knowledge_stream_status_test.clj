(ns is.simm.ops.knowledge-stream-status-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [geschichte.content :as content]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.ops.dvergr :as principals]
            [is.simm.ops.knowledge-program :as program]
            [is.simm.ops.knowledge-stream-status :as status]
            [is.simm.ops.owned-knowledge-batches-test :as fixture]
            [is.simm.ops.owned-knowledge-discovery :as discovery]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.workflows :as workflows]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [is.simm.runtimes.application-ops :as application]
            [malli.core :as m])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- request [data]
  (let [input (:batch-input data)]
    {:name "wiki" :expected-source (str (:expected-source input))
     :expected-bundle (str (:expected-bundle input))}))

(defn- observe [data party]
  (let [{:keys [host w app-owner]} data]
    (lifetime/with-operation! app-owner
      #(status/read-on-host! host (principals/party-principal party) (str (:room w)) (request data)))))

(deftest source-preconditions-do-not-accept-user-supplied-stream-identities
  (let [input {:name "wiki" :expected-source (str (random-uuid)) :expected-bundle (str (random-uuid))}]
    (is (m/validate status/request-schema input))
    (doseq [k [:author :by :room :stream :epoch :target]]
      (is (not (m/validate status/request-schema (assoc input k (random-uuid))))))))

(deftest undiscovered-is-not-empty-and-reading-does-not-discover-or-read-evidence
  (@#'fixture/with-fixture
    (fn [{:keys [conn w owned] :as data}]
      (let [before (vec (d/datoms @conn :eavt))
            forbidden (set (map :content (vals (select-keys (query/worktree @owned)
                                                            ["inbox/meeting.md" "inbox/second.md" "wiki/file.md"]))))
            read content/read-by-id]
        (with-redefs [content/read-by-id (fn [c id & args]
                                         (is (not (contains? forbidden id)))
                                         (apply read c id args))
                      discovery/prepare-batch! (fn [& _] (throw (AssertionError. "No discovery")))
                      discovery/refresh! (fn [& _] (throw (AssertionError. "No discovery")))
                      artifact/publish-value! (fn [& _] (throw (AssertionError. "No artifacts")))]
          (let [result (observe data (:owner w))]
            (is (= :not-discovered (:status result)))
            (is (not (contains? result :pending))))
          ;; Ordinary read membership suffices, and a viewer does not silently
          ;; select the owner's authored stream.
          (is (= :not-discovered (:status (observe data (:member w))))))
        (is (= before (vec (d/datoms @conn :eavt))))))))

(deftest exact-admission-stream-counts-pending-separately-from-claimed-items
  (@#'fixture/with-fixture
    (fn [{:keys [host w batch-input conn owned] :as data}]
      (let [p (principals/party-principal (:owner w)) handle (str (:room w))
            discovery (discovery/refresh! host p handle batch-input)
            initial (observe data (:owner w))]
        (is (= :observed (:status initial)))
        (is (= {:count 2 :complete? true :limit streams/max-changes} (:pending initial)))
        (is (= {:status :current} (:freshness initial)))
        (is (= {:status :none} (:batch initial)))
        (is (= :not-discovered (:status (observe data (:member w)))))
        (let [bound (reads/resolve-host-read! host p handle)
              descriptor (:value (program/describe (:owner w) (:room w) (:target bound)
                                                  (workflows/snapshot-in (:db bound) "wiki") "wiki"
                                                  (:expected-bundle batch-input) (:epoch batch-input)))]
          (is (= (:stream discovery) (:id descriptor)))
          (let [published @owned
                eid (:e (first (d/datoms published :avet :geschichte.work/path "inbox/meeting.md")))
                derived (:db-after (d/with published [[:db/add eid :geschichte.work/size 99]]))
                result (status/observe-in @conn derived descriptor)]
            (is (= (d/commit-id published) (d/commit-id derived)))
            (is (= {:status :unknown} (:freshness result)))
            (is (= 2 (get-in result [:pending :count])))))
        (let [batch (discovery/prepare-batch! host p handle batch-input)
              observed (observe data (:owner w))]
          (is (= :ready (:status batch)))
          (is (= 1 (get-in observed [:pending :count])))
          (is (= {:status :observed :phase :ready} (:batch observed))))
        (repo/write! owned "wiki/new-context.md" (.getBytes "Changed context" "UTF-8"))
        (repo/stage-all! owned) (repo/commit! owned {:message "New Memory basis"})
        (is (= {:status :changed} (:freshness (observe data (:owner w)))))))))

(deftest stale-source-is-refused-before-workflow-body-reads
  (@#'fixture/with-fixture
    (fn [{:keys [host w app-owner] :as data}]
      (with-redefs [workflows/snapshot-in (fn [& _] (throw (AssertionError. "No stale source body")))]
        (is (thrown? clojure.lang.ExceptionInfo
              (lifetime/with-operation! app-owner
                #(status/read-on-host! host (principals/party-principal (:owner w)) (str (:room w))
                                      (assoc (request data) :expected-source (str (random-uuid)))))))))))


(deftest damaged-active-phase-does-not-erase-known-pending-counts
  (@#'fixture/with-fixture
    (fn [{:keys [host w batch-input conn] :as data}]
      (let [ready (discovery/prepare-batch! host (principals/party-principal (:owner w)) (str (:room w)) batch-input)
            bid (:batch ready) original (batches/batch @conn bid)]
        (doseq [damage [{:knowledge.batch/selection-id (random-uuid)}
                       {:knowledge.batch/state :capturing}
                       {:knowledge.batch/state :capturing :knowledge.batch/scheduled-occurrence (random-uuid)}]]
          (d/transact conn [(assoc damage :db/id [:knowledge.batch/id bid])])
          (let [result (observe data (:owner w))]
            (is (= 1 (get-in result [:pending :count])))
            (is (= {:status :unavailable} (:batch result))))
          (d/transact conn [(assoc (select-keys original [:knowledge.batch/selection-id :knowledge.batch/state])
                                   :db/id [:knowledge.batch/id bid])]))))))

(deftest revocation-during-source-io-refuses-the-final-observation
  (@#'fixture/with-fixture
    (fn [{:keys [conn w] :as data}]
      (let [snapshot workflows/snapshot-in]
        (with-redefs [workflows/snapshot-in (fn [& args]
                                            (let [result (apply snapshot args)]
                                              (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties
                                                                [:party/id (:member w)]]])
                                              result))]
          (is (thrown? clojure.lang.ExceptionInfo (observe data (:member w)))))))))

(deftest original-app-fence-refuses-an-interrupt-absorbing-source-read-after-physical-exit
  (@#'fixture/with-fixture
    (fn [{:keys [app-owner w] :as data}]
      (let [snapshot workflows/snapshot-in entered (promise) absorbed (promise) outcome (promise)
            exited (promise) gate (CountDownLatch. 1) worker-thread (promise) ticket (promise)]
        (with-redefs [workflows/snapshot-in
                      (fn [& args]
                        (deliver worker-thread (Thread/currentThread))
                        (deliver ticket lifetime/*ticket*)
                        (deliver entered true)
                        (loop []
                          (when-not (try (.await gate 100 TimeUnit/MILLISECONDS)
                                         (catch InterruptedException _ (deliver absorbed true) false))
                            (recur)))
                        (apply snapshot args))]
          (let [worker (future
                         (try (deliver outcome (observe data (:owner w)))
                              (catch Throwable _ (deliver outcome :refused))
                              (finally (deliver exited true))))]
            (try
              (is (= true (deref entered 5000 false)))
              (lifetime/fence! app-owner)
              (.interrupt ^Thread @worker-thread)
              (is (= true (deref absorbed 5000 false)))
              (is (lifetime/retained? @ticket))
              (is (not (realized? outcome)))
              (is (not (realized? exited)))
              (.countDown gate)
              (is (= :refused (deref outcome 5000 :timeout)))
              (is (= true (deref exited 5000 false)))
              @worker
              (is (not (lifetime/retained? @ticket)))
              (finally (.countDown gate)))))))))


(deftest queued-observer-cannot-enter-a-replacement-application
  (@#'fixture/with-fixture
    (fn [{:keys [host app-owner w] :as data}]
      (let [replacement (doto (lifetime/create) lifetime/open!)
            installation (atom {:host host :owner app-owner})]
        (try
          (with-redefs-fn {#'application/installation installation}
            #(let [body (status/prepare-status (:owner w) (str (:room w)) (request data))]
               (reset! installation {:host host :owner replacement})
               (lifetime/fence! app-owner)
               (with-redefs [status/read-on-host! (fn [& _] (throw (AssertionError. "No replacement App adoption")))]
                 (is (thrown? clojure.lang.ExceptionInfo (body))))))
          (finally (lifetime/fence! replacement) (lifetime/join! replacement 5000)))))))
