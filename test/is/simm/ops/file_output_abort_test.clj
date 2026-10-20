(ns is.simm.ops.file-output-abort-test
  "Synthetic direct completion witnesses exercise ONLY the control journal.
   These tests are NOT evidence of native entry/closure or target publication."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.model.file-adoptions :as adoptions]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.file-output-dispatch-test :as fixture]
            [is.simm.ops.file-output-storage :as storage]
            [is.simm.ops.owned-workspace :as owned]))

(def ^:private completion {:completion :not-started :result nil :error nil})

(defn- abort! [{:keys [conn party room-id room claimed]} binding]
  (adoption/bound-not-started! conn party room-id room claimed binding completion))

(defn- with-claimed [f]
  (@#'fixture/with-claimed
   (fn [{:keys [claimed] :as state}]
     (let [binding {:target (get-in claimed [:proof :binding :target])}]
       ;; A fixed validation DOUBLE, not native authority evidence. The opt-in
       ;; native matrix separately proves the sealed original completion.
       (with-redefs [owned/validate! (fn [_ _ _ _ _ original]
                                       (is (identical? binding original)) binding)]
         (f state binding))))))

(defn- operation [{:keys [conn claimed]}]
  (adoptions/operation @conn (get-in claimed [:operation :id])))

(defn- database-state [conn]
  (let [db @conn]
    {:config (:config db) :max-tx (:max-tx db)
     :commit (:datahike/commit-id (meta db))}))

(defn- join! [^Thread thread]
  (loop [interrupted? false]
    (let [done? (try (.join thread) true (catch InterruptedException _ false))]
      (if done?
        (when interrupted? (.interrupt (Thread/currentThread)))
        (recur true)))))

(deftest original-claim-aborts-only-in-the-current-writer-with-original-ack
  (with-claimed
    (fn [{:keys [conn party room-id room claimed workspace] :as state} binding]
      (let [transact d/transact checked (atom []) before (database-state workspace)]
        (with-redefs [d/transact (fn [& args]
                                   (transact conn [{:room/id room-id :room/name "Abort writer marker"}])
                                   (apply transact args))
                      owned/validate! (fn [db principal action rid canonical original]
                                        (swap! checked conj (:room/name (d/entity db [:room/id room-id])))
                                        (is (= [party :merge room-id] [principal action rid]))
                                        (is (identical? room canonical))
                                        (is (identical? binding original)) binding)
                      storage/read-file-in (fn [& _] (throw (AssertionError. "Abort read retained bodies")))]
          (is (= {:status :aborted :operation (get-in claimed [:operation :id]) :reason :native-not-started}
                 (abort! state binding))))
        (is (= ["Abort writer marker"] @checked))
        (is (= :aborted (:state (operation state))))
        (is (= :native-not-started (:reason (operation state))))
        (is (= (dissoc (operation state) :reason :updated-at :state)
               ;; Compare immutable fields, not the intentional transition.
               (dissoc (:operation claimed) :updated-at :state)))
        (is (= :open (:proposal/status (d/entity @conn [:proposal/id (:proposal (:operation claimed))]))))
        (is (= (:proof claimed) (adoption/snapshot @conn party room-id (:proposal (:operation claimed)))))
        (is (thrown? Exception (abort! state binding)))
        (is (= before (database-state workspace)))))))

(deftest current-permission-binding-and-complete-proof-cannot-be-skipped
  (doseq [mode [:permission :binding :target :proof :applying]]
    (with-claimed
      (fn [{:keys [conn room-id claimed] :as state} binding]
        (case mode
          :permission (d/transact conn [[:db/retract [:room/id room-id] :room/owner]])
          :proof (d/transact conn [[:db/add (get-in claimed [:proof :fork :db/id])
                                    :proposal.fork/branch "Different branch"]])
          :applying (d/transact conn
                                (adoptions/transition-tx-data @conn (get-in claimed [:proof :fork :db/id])
                                                              (:operation claimed) :applying {} (java.util.Date.)))
          nil)
        (let [before (operation state)]
          (with-redefs [owned/validate! (fn [& _]
                                          (case mode
                                            :binding (throw (ex-info "Original stamp/config replaced" {}))
                                            :target (assoc-in binding [:target :store] (random-uuid))
                                            binding))]
            (is (thrown? Exception (abort! state binding))))
          (is (= before (operation state))))))))

(deftest both-original-report-snapshots-are-required-without-readback-success
  (doseq [mode [:missing-before :missing-after :stale-before :stale-after :lost]]
    (with-claimed
      (fn [{:keys [conn claimed] :as state} binding]
        (let [transact d/transact calls (atom 0)
              failure (with-redefs [d/transact
                                    (fn [& args]
                                      (swap! calls inc)
                                      (let [report (apply transact args)]
                                        (case mode
                                          :missing-before (dissoc report :db-before)
                                          :missing-after (dissoc report :db-after)
                                          :stale-before (assoc report :db-before (:db-after report))
                                          :stale-after (assoc report :db-after (:db-before report))
                                          :lost (throw (ex-info "Lost original abort ACK" {})))))]
                        (try (abort! state binding) nil (catch clojure.lang.ExceptionInfo error error)))]
          (is (= ::adoption/abort-unacknowledged (:type (ex-data failure))))
          (is (= (get-in claimed [:operation :id]) (:operation (ex-data failure))))
          (is (not (contains? (ex-data failure) :token)))
          (is (= 1 @calls))
          ;; Diagnostic inspection ONLY; the implementation never turns this
          ;; durable row into returned cleanup or replay ownership.
          (is (= :aborted (:state (operation state)))))))))

(deftest interruption-before-or-after-commit-preserves-flag-and-unknown-ack
  (doseq [committed? [false true]]
    (with-claimed
      (fn [{:keys [claimed] :as state} binding]
        (let [transact d/transact answer (promise)
              worker (Thread.
                      ^Runnable
                      (fn []
                        (with-redefs [d/transact (fn [& args]
                                                   (when committed? (apply transact args))
                                                   (throw (ex-info "Wrapped interruption" {}
                                                                   (InterruptedException. "Actual cause"))))]
                          (try (abort! state binding) (deliver answer :unexpected)
                               (catch clojure.lang.ExceptionInfo error
                                 (deliver answer [(:type (ex-data error))
                                                  (.isInterrupted (Thread/currentThread))]))))))]
          (.start worker)
          (try (join! worker)
               (is (= [::adoption/abort-unacknowledged true]
                      (deref answer 1000 ::missing-after-physical-exit)))
               (is (= (if committed? :aborted :claimed) (:state (operation state))))
               (finally (join! worker))))))))

(deftest invalid-or-preinterrupted-call-never-submits-cleanup
  (with-claimed
    (fn [{:keys [conn party room-id room claimed] :as state} binding]
      (with-redefs [d/transact (fn [& _] (throw (AssertionError. "Invalid cleanup submitted")))]
        (doseq [bad [{:completion :pending :result nil :error nil}
                     {:completion :not-started :error nil}
                     (assoc completion :error {:interrupted? true})]]
          (is (thrown? Exception
                       (adoption/bound-not-started! conn party room-id room claimed binding bad))))
        (let [answer (promise) worker (Thread. ^Runnable
                                       (fn []
                                         (.interrupt (Thread/currentThread))
                                         (try (abort! state binding)
                                              (catch Exception _
                                                (deliver answer (.isInterrupted (Thread/currentThread)))))))]
          (.start worker)
          (try (join! worker)
               (is (true? (deref answer 1000 ::missing-after-physical-exit)))
               (finally (join! worker)))))
      (is (= :claimed (:state (operation state)))))))

(deftest token-links-and-concurrent-applying-cannot-share-cleanup-ownership
  (doseq [mode [:token :unlinked :shared]]
    (with-claimed
      (fn [{:keys [conn claimed] :as state} binding]
        (let [id (get-in claimed [:operation :id]) fork-eid (get-in claimed [:proof :fork :db/id])]
          (case mode
            :token (d/transact conn [[:db/add [:file.adoption/id id] :file.adoption/token (random-uuid)]])
            :unlinked (d/transact conn [[:db/retract fork-eid :proposal.fork/file-adoption]])
            :shared (d/transact conn [{:proposal.fork/scope (random-uuid)
                                       :proposal.fork/system-type :file-output
                                       :proposal.fork/file-adoption [:file.adoption/id id]}]))
          (is (thrown? Exception (abort! state binding)))
          ;; Deliberately corrupted token/ownership is NOT a valid operation
          ;; for the strict reader. Inspect only the raw diagnostic state.
          (is (= :claimed (:file.adoption/state (d/entity @conn [:file.adoption/id id]))))))))
  (with-claimed
    (fn [{:keys [conn claimed] :as state} binding]
      (let [start (java.util.concurrent.CountDownLatch. 1)
            results (atom [])
            run! (fn [kind f]
                   (Thread. ^Runnable
                    (fn [] (.await start)
                      (swap! results conj
                             [kind (try (f) :ack (catch Exception _ :refused))]))))
            aborter (run! :abort #(abort! state binding))
            dispatcher (run! :apply #(d/transact conn
                                                 [[:db.fn/call
                                                   (fn [db]
                                                     (adoptions/transition-tx-data db
                                                                                   (get-in claimed [:proof :fork :db/id])
                                                                                   (:operation claimed) :applying {} (java.util.Date.)))]]))]
        (.start aborter) (.start dispatcher)
        (try (.countDown start) (join! aborter) (join! dispatcher)
             (is (= 1 (count (filter #(= :ack (second %)) @results))))
             (is (#{:aborted :applying} (:state (operation state))))
             (finally (.countDown start) (join! aborter) (join! dispatcher)))))))
