(ns is.simm.ops.file-output-controller-test
  "Mocked fixed-host orchestration ONLY. No Room, DB transaction or native
   publication is exercised; real ownership requires separate integration tests."
  (:require [clojure.test :refer [deftest is]]
            [is.simm.model.file-adoptions :as adoptions]
            [is.simm.model.system-db :as sdb]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.file-output-controller :as controller]
            [is.simm.ops.file-output-publication-test :as fixture]
            [is.simm.ops.owned-workspace :as owned]))

(defn- with-host [options f]
  (let [{:keys [operation plan candidate receipt]} (@#'fixture/fixture)
        conn (atom {}) room (Object.) party (:actor operation) rid (:room operation)
        system (Object.) binding {:system system :target (:target operation)}
        proof {:binding (select-keys operation [:proposal :component :manifest :room :actor :filing :disposition :target])
               :fork {:db/id 1} :proposal {:db/id 2} :retained {:manifest (:manifest plan)}}
        prepared {:proof proof :plan plan}
        claimed (assoc prepared :operation operation)
        journal (atom (:existing options)) calls (atom [])
        note! #(swap! calls conj %)
        write! (fn [actual-room actual-system files opts]
                 (note! :native-write)
                 (is (identical? room actual-room))
                 (is (identical? system actual-system))
                 (is (= #{"/wiki/new.md" "/wiki/old.md"} (set (keys files))))
                 (is (= #{:expected-worktree :file-modes :publication} (set (keys opts))))
                 (let [authorize! (get-in opts [:publication :authorize!])
                       before! (get-in opts [:publication :before-dispatch!])]
                   (is (= (get-in opts [:publication :identity :publication/id]) (:id operation)))
                   (when-not (#{:skip :not-started :not-started-forged-positive} (:mode options)) (authorize!))
                   (when-not (#{:skip :not-started :not-started-forged-positive :no-callback :forged-error-before} (:mode options))
                     (before! candidate))
                   (when (= :duplicate-callback (:mode options)) (before! candidate))
                   (when (#{:forged-error-before :forged-error-after} (:mode options))
                     (throw (ex-info "Interceptor's untrusted publication claim" {:publication receipt})))
                   (when (:native-interrupt? options) (.interrupt (Thread/currentThread)))
                   (let [result {:applied? (not= :skip (:mode options))
                                 :publication (if (= :wrong-receipt (:mode options))
                                                (assoc receipt :publication/actor (random-uuid)) receipt)}]
                     (if (= :malformed-completion (:mode options)) result
                         {:completion (cond (#{:not-started :not-started-forged-positive :impossible-not-started-after-applying} (:mode options)) :not-started
                                            (#{:skip :pending-receipt} (:mode options)) :pending
                                            :else :returned)
                          :result (when-not (= :not-started (:mode options)) result)
                          :error (when (= :returned-error (:mode options)) (ex-info "After native positive" {}))}))))
        run! #(controller/adopt! conn party rid room (:proposal operation))]
    (with-redefs-fn
      {#'sdb/get-conn (constantly conn)
       #'owned/resolve! (fn [& _] (note! :resolve) binding)
       #'owned/validate! (fn [& _]
                           (note! :validate-binding)
                           (when (:revoked? options) (throw (ex-info "Revoked" {})))
                           binding)
       #'adoption/snapshot (fn [& _] (note! :snapshot) proof)
       #'adoptions/current-operation (fn [& _] @journal)
       #'adoptions/linked-operation! (fn [_ _ expected] (is (= expected @journal)) expected)
       #'adoption/prepare (fn [& _] (note! :prepare) prepared)
       #'adoption/claim! (fn [& _]
                           (note! :claim) (reset! journal operation)
                           (when (:lost-claim? options)
                             (throw (ex-info "Claim was not acknowledged"
                                             {:type :is.simm.ops.file-output-adoption/claim-unacknowledged
                                              :operation (:id operation)})))
                           claimed)
       #'adoption/bound-applying!
       (fn [actual-conn actual-party actual-rid actual-room actual-claimed actual-candidate actual-binding]
         (note! :applying)
         (is (identical? conn actual-conn)) (is (identical? room actual-room))
         (is (= [party rid claimed candidate binding]
                [actual-party actual-rid actual-claimed actual-candidate actual-binding]))
         (let [ack (assoc claimed :operation (assoc operation :state :applying))]
           (reset! journal (:operation ack))
           (when (:lost-applying? options) (throw (ex-info "Lost applying ACK" {})))
           ack))
       #'adoption/finalize! (fn [_ actual-applying actual-receipt]
                              (note! :finalize)
                              (is (= receipt actual-receipt))
                              (is (= :applying (get-in actual-applying [:operation :state])))
                              (reset! journal (assoc operation :state :accepted))
                              (when (:lost-finalization? options) (throw (ex-info "Lost accepted ACK" {})))
                              {:status :accepted :operation (:id operation)})
       #'adoption/bound-not-started!
       (fn [actual-conn actual-party actual-rid actual-room actual-claimed actual-binding completion]
         (note! :abort)
         (is (identical? conn actual-conn)) (is (identical? room actual-room))
         (is (= [party rid claimed binding] [actual-party actual-rid actual-claimed actual-binding]))
         (is (= :not-started (:completion completion)))
         (when-not (= operation @journal) (throw (ex-info "Exact claimed CAS refused" {})))
         (reset! journal (assoc operation :state :aborted :reason :native-not-started))
         (when (:lost-abort? options) (throw (ex-info "Lost aborted ACK" {})))
         {:status :aborted :operation (:id operation) :reason :native-not-started})
       #'controller/native-writer (fn [] (note! :feature-check) (when-not (:unsupported? options) write!))}
      #(f {:run! run! :calls calls :journal journal :operation operation :claimed claimed}))))

(defn- error [run!]
  (try (run!) nil (catch clojure.lang.ExceptionInfo e e)))

(deftest original-normal-native-result-is-the-only-finalization-path
  (with-host {}
    (fn [{:keys [run! calls journal operation]}]
      (is (= {:status :accepted :operation (:id operation)} (run!)))
      (is (= :accepted (:state @journal)))
      (is (= 1 (count (filter #{:claim} @calls)) (count (filter #{:native-write} @calls))
             (count (filter #{:applying} @calls)) (count (filter #{:finalize} @calls)))))))

(deftest existing-ownership-is-pending-not-a-new-invocation-capability
  (doseq [state [:claimed :applying :accepted]]
    (with-host {:existing {:id (random-uuid) :state state}}
      (fn [{:keys [run! calls journal]}]
        (is (= {:status :pending :operation (:id @journal) :recovery :unavailable} (run!)))
        (is (= [:resolve :snapshot] @calls))))))

(deftest unavailable-native-capability-never-falls-back-or-claims
  (with-host {:unsupported? true}
    (fn [{:keys [run! calls journal]}]
      (is (= :explicit-writer-unavailable (:reason (ex-data (error run!)))))
      (is (nil? @journal))
      (is (= [:resolve :snapshot :feature-check] @calls)))))

(deftest lost-claim-ack-never-reconstructs-a-publisher-from-readback
  (with-host {:lost-claim? true}
    (fn [{:keys [run! calls journal operation]}]
      (is (= :is.simm.ops.file-output-adoption/claim-unacknowledged (:type (ex-data (error run!)))))
      (is (= :claimed (:state @journal)))
      (is (not-any? #{:native-write :applying :finalize} @calls))
      (reset! calls [])
      (is (= {:status :pending :operation (:id operation) :recovery :unavailable} (run!)))
      (is (= [:resolve :snapshot] @calls)))))

(deftest skipped-missing-duplicate-or-forged-outcomes-never-finalize-or-replay
  (doseq [mode [:skip :no-callback :duplicate-callback :forged-error-before :forged-error-after :wrong-receipt
                :pending-receipt :malformed-completion]]
    (with-host {:mode mode}
      (fn [{:keys [run! calls operation]}]
        (let [failure (error run!)]
          (is (= ::controller/pending (:type (ex-data failure))))
          (is (= (:id operation) (:operation (ex-data failure))))
          (is (= #{:type :status :operation :recovery} (set (keys (ex-data failure)))))
          (is (zero? (count (filter #{:finalize} @calls))))
          (is (zero? (count (filter #{:abort} @calls))))
          (is (= 1 (count (filter #{:native-write} @calls))))
          (is (<= (count (filter #{:applying} @calls)) 1)))))))

(deftest direct-sealed-not-started-can-only-release-the-original-claimed-operation
  (doseq [mode [:not-started :not-started-forged-positive]]
    (with-host {:mode mode}
      (fn [{:keys [run! calls operation journal]}]
        (is (= {:status :aborted :operation (:id operation) :reason :native-not-started} (run!)))
        (is (= :aborted (:state @journal)))
        (is (= 1 (count (filter #{:native-write} @calls)) (count (filter #{:abort} @calls))))
        (is (not-any? #{:applying :finalize} @calls))))))

(deftest private-positive-completion-wins-over-a-later-diagnostic-error
  (with-host {:mode :returned-error}
    (fn [{:keys [run! calls journal]}]
      (is (= :accepted (:status (run!))))
      (is (= :accepted (:state @journal)))
      (is (= 1 (count (filter #{:finalize} @calls))))
      (is (not-any? #{:abort} @calls)))))

(deftest lost-abort-ack-stays-pending-without-native-or-transaction-retry
  (with-host {:mode :not-started :lost-abort? true}
    (fn [{:keys [run! calls journal operation]}]
      (is (= ::controller/pending (:type (ex-data (error run!)))))
      (is (= :aborted (:state @journal)) "Diagnostic truth is not returned ACK ownership")
      (is (= 1 (count (filter #{:native-write} @calls)) (count (filter #{:abort} @calls))))
      (is (not-any? #{:applying :finalize} @calls)))))

(deftest interrupted-not-started-preserves-the-owner-thread-flag-without-cleanup
  (with-host {:mode :not-started :native-interrupt? true}
    (fn [{:keys [run! calls journal]}]
      (let [result (promise)
            worker (Thread. ^Runnable
                    (fn [] (let [failure (error run!)]
                             (deliver result [(:type (ex-data failure))
                                              (.isInterrupted (Thread/currentThread))]))))]
        (.start worker)
        (try (.join worker)
             (is (= [::controller/pending true] (deref result 1000 ::missing-after-physical-exit)))
             (is (= :claimed (:state @journal)))
             (is (not-any? #{:abort :applying :finalize} @calls))
             (finally (.join worker)))))))

(deftest causal-interruption-inspection-is-cycle-safe-and-does-not-read-error-data
  (let [a (Exception. "a") b (Exception. "b")]
    (.initCause a b) (.initCause b a)
    (is (false? (adoption/causal-interruption? a))))
  (is (adoption/causal-interruption? (ex-info "Wrapped" {} (InterruptedException. "Real cause"))))
  (is (false? (adoption/causal-interruption? (ex-info "Fake" {:interrupted? true})))))

(deftest even-an-impossible-not-started-envelope-cannot-abort-an-applying-ack
  (with-host {:mode :impossible-not-started-after-applying}
    (fn [{:keys [run! calls journal]}]
      (is (= ::controller/pending (:type (ex-data (error run!)))))
      (is (= :applying (:state @journal)))
      (is (not-any? #{:abort :finalize} @calls)))))

(deftest current-binding-refusal-does-not-enter-applying
  (with-host {:revoked? true}
    (fn [{:keys [run! calls journal]}]
      (is (= ::controller/pending (:type (ex-data (error run!)))))
      (is (= :claimed (:state @journal)))
      (is (not-any? #{:applying :finalize} @calls)))))

(deftest lost-transition-acks-stay-pending-without-readback-success
  (doseq [options [{:lost-applying? true} {:lost-finalization? true}]]
    (with-host options
      (fn [{:keys [run! calls journal]}]
        (is (= ::controller/pending (:type (ex-data (error run!)))))
        (is (= (if (:lost-finalization? options) :accepted :applying) (:state @journal)))
        (is (= 1 (count (filter #{:native-write} @calls))))
        (is (= (if (:lost-finalization? options) 1 0) (count (filter #{:finalize} @calls))))))))
