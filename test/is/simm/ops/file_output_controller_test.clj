(ns is.simm.ops.file-output-controller-test
  "Mocked fixed-host orchestration ONLY. No Room, DB transaction or native
   publication is exercised; real ownership requires separate integration tests."
  (:require [clojure.test :refer [deftest is]]
            [dvergr.runtime.preparation :as preparation]
            [is.simm.model.file-adoptions :as adoptions]
            [is.simm.model.system-db :as sdb]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.file-output-controller :as controller]
            [is.simm.ops.file-output-publication-test :as fixture]
            [is.simm.ops.file-output-validation :as validation]
            [is.simm.ops.owned-workspace :as owned]))

(defn- with-host [options f]
  (let [{:keys [operation plan candidate receipt]} (@#'fixture/fixture)
        conn (atom {}) room (Object.) party (:actor operation) rid (:room operation)
        system (Object.) binding {:system system :target (:target operation) :db {:source :snapshot}}
        witness {:manifest (get-in plan [:manifest :id]) :target (:target operation)}
        witness-ref (atom witness)
        proof {:binding (select-keys operation [:proposal :component :manifest :room :actor :filing :disposition :target])
               :fork {:db/id 1} :proposal {:db/id 2} :retained {:manifest (:manifest plan)}}
        prepared {:proof proof :plan plan}
        claimed (assoc prepared :operation operation)
        journal (atom (:existing options)) calls (atom [])
        lease (Object.) held (atom false)
        note! #(swap! calls conj %)
        write! (fn [actual-room actual-system files opts]
                 (is @held "native dispatch remains inside Room resource ownership")
                 (note! :native-write)
                 (is (identical? room actual-room))
                 (is (identical? system actual-system))
                 (is (= #{"/wiki/new.md" "/wiki/old.md"} (set (keys files))))
                 (is (= #{:expected-worktree :file-modes :publication} (set (keys opts))))
                 (let [authorize! (get-in opts [:publication :authorize!])
                       validate! (get-in opts [:publication :validate-basis!])
                       before! (get-in opts [:publication :before-dispatch!])]
                   (is (= (get-in opts [:publication :identity :publication/id]) (:id operation)))
                   (when-not (#{:skip :not-started :not-started-forged-positive} (:mode options)) (authorize!))
                   (when-not (#{:skip :not-started :not-started-forged-positive} (:mode options))
                     ;; Evaluate OUTSIDE is: clojure.test catches exceptions,
                     ;; which must escape this native stand-in and stop dispatch.
                     (let [validated (validate! {:source :prepared-after})]
                       (is (true? validated))))
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
       #'preparation/with-host-operation! (fn [actual-room f]
                                            (is (identical? room actual-room))
                                            (reset! held true)
                                            (try (f lease) (finally (reset! held false))))
       #'preparation/with-host-current! (fn [actual-lease f]
                                          (is @held)
                                          (is (identical? lease actual-lease))
                                          (f))
       #'owned/resolve! (fn [& _] (note! :resolve) binding)
       #'owned/validate! (fn [& _]
                           (note! :validate-binding)
                           (when (:revoked? options) (throw (ex-info "Revoked" {})))
                           binding)
       #'validation/prepare-in (fn [db actual-plan]
                                 (note! :canonical-preflight)
                                 (is (= (:db binding) db))
                                 (is (= plan actual-plan))
                                 (when (:invalid-canonical? options) (throw (ex-info "Invalid canonical output" {})))
                                 (cond-> @witness-ref
                                   (:wrong-witness? options) (assoc :manifest (random-uuid))))
       #'validation/assert-after! (fn [db actual-witness]
                                    (note! :canonical-after)
                                    (is (= {:source :prepared-after} db))
                                    (is (= @witness-ref actual-witness))
                                    (when (= :guard-refused (:mode options))
                                      (throw (ex-info "Prepared source changed" {})))
                                    true)
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
      #(f {:run! run! :calls calls :journal journal :operation operation :claimed claimed
           :witness witness-ref
           :preview! (fn [] (controller/preview-reviewed! conn party rid room (:proposal operation)))
           :adopt-reviewed! (fn [revision]
                              (controller/adopt-reviewed! conn party rid room (:proposal operation) revision))}))))

(defn- error [run!]
  (try (run!) nil (catch clojure.lang.ExceptionInfo e e)))

(deftest reviewed-preview-is-read-only-and-acceptance-recomputes-before-claim
  (with-host {}
    (fn [{:keys [preview! adopt-reviewed! calls journal]}]
      (let [preview (preview!) again (preview!)]
        (is (= :reviewed (:status preview)))
        (is (uuid? (:revision preview)))
        (is (= preview again))
        (is (nil? @journal))
        (is (not-any? #{:claim :native-write :applying :finalize} @calls))
        (reset! calls [])
        (is (= :accepted (:status (adopt-reviewed! (:revision preview)))))
        (is (= 1 (count (filter #{:canonical-preflight} @calls))))
        (is (< (.indexOf @calls :canonical-preflight) (.indexOf @calls :claim)))))))

(deftest missing-or-changed-review-cannot-claim
  (with-host {}
    (fn [{:keys [preview! adopt-reviewed! calls journal witness]}]
      (doseq [invalid [nil false "client-supplied-string"]]
        (is (= :invalid-review-revision (:reason (ex-data (error #(adopt-reviewed! invalid)))))))
      (is (empty? @calls))
      (let [preview (preview!)]
        (swap! witness assoc :selection {:files {"wiki/other.md" {:content (random-uuid)}}})
        (reset! calls [])
        (is (= :review-changed (:reason (ex-data (error #(adopt-reviewed! (:revision preview)))))))
        (is (nil? @journal))
        (is (not-any? #{:claim :native-write :applying :finalize} @calls))
        (is (not= (:revision preview) (:revision (preview!))))))))

(deftest preview-of-existing-ownership-never-reads-bodies-or-republishes
  (with-host {:existing {:id (random-uuid) :state :claimed}}
    (fn [{:keys [preview! calls]}]
      (is (= :pending (:status (preview!))))
      (is (= [:resolve :snapshot] @calls)))))

(deftest review-revision-covers-semantic-witness-with-order-independent-scopes
  (let [prepared {:proof {:binding {:proposal (random-uuid) :component (random-uuid)}}}
        a {:revision (random-uuid) :declarations {:a true}}
        b {:revision (random-uuid) :declarations {:b false}}
        witness {:profile {:version 1} :preservation {:version 1}
                 :manifest (random-uuid) :target {:ref "refs/heads/main"}
                 :selection {:files {}} :typed-scopes [a b]
                 :constraint-dependencies {:external-targets #{}} :after {}}
        revision #(@#'controller/review-revision prepared %)]
    (is (= (revision witness) (revision (assoc witness :typed-scopes [b a]))))
    (doseq [changed [(assoc witness :profile {:version 2})
                     (assoc witness :preservation {:version 2})
                     (assoc witness :manifest (random-uuid))
                     (assoc witness :target {:ref "refs/heads/other"})
                     (assoc witness :selection {:files {"wiki/a.md" {:content (random-uuid)}}})
                     (assoc witness :typed-scopes [(assoc a :declarations {:a false}) b])
                     (assoc witness :constraint-dependencies {:external-targets #{(random-uuid)}})
                     (assoc witness :after {"wiki/new.md" {:content (random-uuid)}})]]
      (is (not= (revision witness) (revision changed))))))

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

(deftest canonical-preflight-and-witness-binding-refuse-before-claim
  (doseq [options [{:invalid-canonical? true} {:wrong-witness? true}]]
    (with-host options
      (fn [{:keys [run! calls journal]}]
        (is (some? (error run!)))
        (is (nil? @journal))
        (is (= 1 (count (filter #{:canonical-preflight} @calls))))
        (is (not-any? #{:claim :native-write :applying :finalize} @calls))))))

(deftest changed-prepared-source-stays-pending-without-applying-or-replaying
  (with-host {:mode :guard-refused}
    (fn [{:keys [run! calls journal]}]
      (is (= ::controller/pending (:type (ex-data (error run!)))))
      (is (= :claimed (:state @journal)))
      (is (= 1 (count (filter #{:canonical-preflight} @calls))
             (count (filter #{:canonical-after} @calls))))
      (is (not-any? #{:applying :abort :finalize} @calls))
      (reset! calls [])
      (is (= :pending (:status (run!))))
      (is (= [:resolve :snapshot] @calls)))))

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
