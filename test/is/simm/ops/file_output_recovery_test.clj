(ns is.simm.ops.file-output-recovery-test
  "Recovery CONTROL tests with synthesized target receipt rows, NOT proof of
   native publication. Owned binding is an explicit orchestration seam; actual
   native descriptor/alias/default checks and publication have separate tests."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [dvergr.rooms.files :as native-files]
            [dvergr.rooms.publication :as scoped]
            [geschichte.repo :as repo]
            [geschichte.query :as query]
            [is.simm.model.file-adoptions :as adoptions]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.file-output-dispatch-test :as fixture]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.ops.proposals :as proposals]))

(defn- with-recovery [phase f]
  (@#'fixture/with-claimed
   (fn [{:keys [conn party room-id room claimed workspace] :as state}]
     (let [candidate (@#'fixture/candidate claimed)
           applying (when (= :applying phase)
                      (adoption/applying! conn party room-id room claimed candidate))
           winning (update candidate :publication/entries
                           #(mapv (fn [entry]
                                    (assoc entry :publication.entry/content
                                           {:geschichte.content/id (second (:publication.entry/content entry))})) %))
           system (Object.)
           change (atom identity)
           current-binding (fn [] (@change {:room room :system system :conn workspace :db @workspace
                                            :target (assoc (get-in claimed [:proof :binding :target])
                                                           :ref (query/current-ref @workspace))
                                            :workspace-config {:config (:config @workspace)
                                                               :workspace-branch (get-in @workspace [:config :branch])}}))]
       (with-redefs [owned/resolve!
                     (fn [_ _ _ rid canonical]
                       (when-not (and (= room-id rid) (identical? room canonical))
                         (throw (ex-info "Noncanonical test Room" {})))
                       (current-binding))]
         (f (assoc state :applying applying :winning winning :change-binding change)))))))

(defn- publish-synthetic! [{:keys [workspace winning]}]
  (scoped/install! workspace)
  (d/transact workspace [winning]))

(defn- recover! [{:keys [conn party room-id room claimed]}]
  (adoption/recover! conn party room-id room (get-in claimed [:operation :proposal])))

(defn- journal [{:keys [conn claimed]}]
  (adoptions/operation @conn (get-in claimed [:operation :id])))

(defn- join-completely! [join!]
  ;; Diagnostic timeouts below are NOT physical ACK. Retain this fixture's
  ;; stores/context until every started test worker has actually returned.
  (let [interrupted? (volatile! false)]
    (try
      (loop []
        (when-not (try (join!) true
                       (catch InterruptedException _ (vreset! interrupted? true) false))
          (recur)))
      (finally (when @interrupted? (.interrupt (Thread/currentThread)))))))

(defn- new-owner! [{:keys [conn room-id]}]
  (let [party (random-uuid)]
    (d/transact conn [{:party/id party}
                      [:db/add [:room/id room-id] :room/owner [:party/id party]]
                      [:db/add [:room/id room-id] :room/parties [:party/id party]]])
    party))

(deftest absence-without-receipt-schema-is-observational-not-publication-authority
  (doseq [phase [:claimed :applying]]
    (with-recovery phase
      (fn [{:keys [conn workspace claimed] :as state}]
        (is (empty? (d/datoms @workspace :avet :db/ident :publication/id)))
        (let [control @conn target @workspace calls (atom 0)]
          (with-redefs [d/transact (fn [& _] (swap! calls inc))
                        scoped/recover (fn [& _] (swap! calls inc))
                        native-files/write-to! (fn [& _] (swap! calls inc))
                        artifact/get-value (fn [& _] (swap! calls inc))]
            (is (= {:status :pending :proposal (get-in claimed [:operation :proposal])
                    :operation-id (get-in claimed [:operation :id]) :observed-state phase
                    :reason :receipt-not-observed}
                   (recover! state))))
          (is (zero? @calls))
          (is (= control @conn))
          (is (= target @workspace)))))))

(deftest positive-recovery-preserves-original-actor-and-historical-ref
  (with-recovery :applying
    (fn [{:keys [conn workspace claimed winning] :as state}]
      (publish-synthetic! state)
      ;; Real later worktree/HEAD/ref changes, but synthetic receipt content.
      (repo/write! workspace "human.md" (.getBytes "Later human work" "UTF-8"))
      (repo/stage-all! workspace)
      (repo/commit! workspace {:message "Later human commit"})
      (repo/branch! workspace "later-checkout")
      (repo/checkout! workspace "later-checkout")
      (is (not= (:publication/ref winning) (query/current-ref @workspace)))
      (let [reconciler (new-owner! state) state (assoc state :party reconciler)
            target @workspace effects (atom 0)
            result (with-redefs [native-files/write-to! (fn [& _] (swap! effects inc))
                                 scoped/publish! (fn [& _] (swap! effects inc))
                                 artifact/get-value (fn [& _] (swap! effects inc))]
                     (recover! state))
            operation (journal state)
            p (proposals/get-proposal (get-in claimed [:operation :proposal]))
            fork (first (:proposal/forks p))]
        (is (= :accepted (:status result) (:state operation) (:proposal/status p) (:proposal.fork/status fork)))
        (is (false? (:already-accepted? result)))
        (is (= (:publication/commit winning) (:commit operation)))
        (is (= (get-in claimed [:operation :actor]) (:actor operation)
               (:proposal/resolved-by p) (:proposal/resolved-by fork)))
        (is (= reconciler (:reconciled-by operation)))
        (is (= (:updated-at operation) (:proposal/resolved-at p) (:proposal/resolved-at fork)))
        (is (not (contains? result :token)))
        (is (zero? @effects))
        (is (= target @workspace))
        (let [other (new-owner! state) repeated (recover! (assoc state :party other))]
          (is (true? (:already-accepted? repeated)))
          (is (= operation (journal state)))
          (is (= target @workspace)))))))

(deftest current-authority-is-required-before-lookup-and-again-in-the-writer
  (doseq [when-revoked [:before :after-lookup]]
    (with-recovery :applying
      (fn [{:keys [conn room-id workspace] :as state}]
        (publish-synthetic! state)
        (let [revoke! #(d/transact conn [[:db/retract [:room/id room-id] :room/owner]
                                         [:db/retract [:room/id room-id] :room/parties]])
              lookup scoped/recover calls (atom 0) target @workspace]
          (when (= :before when-revoked) (revoke!))
          (with-redefs [scoped/recover (fn [& args]
                                         (swap! calls inc)
                                         (let [value (apply lookup args)]
                                           (when (= :after-lookup when-revoked) (revoke!))
                                           value))]
            (is (thrown? clojure.lang.ExceptionInfo (recover! state))))
          (is (= (if (= :before when-revoked) 0 1) @calls))
          (is (= :applying (:state (journal state))))
          (is (= target @workspace)))))))

(deftest positive-noop-and-original-finalization-ack-loss-remain-audit-facts
  (doseq [mode [:noop :original-finalization-ack-loss]]
    (with-recovery :applying
      (fn [{:keys [conn applying winning workspace] :as state}]
        (let [state (cond-> state
                      (= :noop mode) (assoc :winning (-> winning
                                                         (assoc :publication/noop? true)
                                                         (dissoc :publication/commit))))]
          (publish-synthetic! state)
          (when (= :original-finalization-ack-loss mode)
            (let [transact d/transact]
              (with-redefs [d/transact (fn [& args]
                                         (apply transact args)
                                         (throw (ex-info "Lost original finalization ACK" {})))]
                (is (thrown? clojure.lang.ExceptionInfo
                             (adoption/finalize! conn applying (:winning state)))))))
          (let [target @workspace result (recover! state) operation (journal state)]
            (is (= :accepted (:status result)))
            (is (= (= :original-finalization-ack-loss mode) (:already-accepted? result)))
            (is (= (= :noop mode) (:noop? operation)))
            (is (= (= :original-finalization-ack-loss mode) (contains? operation :commit)))
            (when (= :original-finalization-ack-loss mode)
              (is (not (contains? operation :reconciled-by))))
            (is (= target @workspace))))))))

(deftest current-attempt-target-handles-and-config-are-rechecked-without-freezing-ref
  (with-recovery :applying
    (fn [{:keys [conn claimed workspace change-binding] :as state}]
      (publish-synthetic! state)
      (let [lookup scoped/recover target @workspace operation (:operation (:applying state))
            fork (get-in claimed [:proof :fork :db/id])]
        (is (= operation (adoptions/linked-operation! @conn fork operation)))
        (doseq [change [:default :config :system :conn :room]]
          (reset! change-binding identity)
          (try
            (let [before @conn]
              (with-redefs [scoped/recover
                            (fn [& args]
                              (let [value (apply lookup args)]
                                (reset! change-binding
                                        (case change
                                          :default #(assoc-in % [:target :registry-system] (random-uuid))
                                          :config #(assoc-in % [:workspace-config :config :fixture-revision] 2)
                                          :system #(assoc % :system (Object.))
                                          :conn #(assoc % :conn (atom @workspace))
                                          :room #(assoc % :room {})))
                                value))]
                (is (thrown? clojure.lang.ExceptionInfo (recover! state))))
              (is (= before @conn)))
            (finally (reset! change-binding identity)))
          (is (= operation (adoptions/linked-operation! @conn fork operation)))
          (is (= target @workspace)))))))

(deftest claimed-receipt-is-inconsistent-only-if-the-current-writer-is-still-claimed
  (doseq [progress? [false true]]
    (with-recovery :claimed
      (fn [{:keys [conn party room-id room claimed workspace] :as state}]
        (publish-synthetic! state)
        (let [lookup scoped/recover target @workspace]
          (with-redefs [scoped/recover
                        (fn [& args]
                          (let [value (apply lookup args)]
                            (when progress?
                              (adoption/applying! conn party room-id room claimed (@#'fixture/candidate claimed)))
                            value))]
            (if progress?
              (is (= :accepted (:status (recover! state))))
              (is (thrown? clojure.lang.ExceptionInfo (recover! state)))))
          (is (= (if progress? :accepted :claimed) (:state (journal state))))
          (is (= target @workspace)))))))

(deftest changed-immutable-proof-or-token-refuses-after-positive-observation
  (with-recovery :applying
    (fn [{:keys [conn claimed workspace] :as state}]
      (publish-synthetic! state)
      (let [lookup scoped/recover target @workspace
            operation (:operation (:applying state))
            scope (:component operation) fork (get-in claimed [:proof :fork :db/id])
            manifest-eid (:e (first (d/datoms @conn :avet :file-output/id scope)))
            entry (:v (first (d/datoms @conn :eavt manifest-eid :file-output/entries)))
            body-ref (:file-output.entry/body-ref (d/pull @conn [:file-output.entry/body-ref] entry))
            valid! (fn []
                     (is (= (:proof claimed)
                            (adoption/snapshot @conn (:actor operation) (:room operation) (:proposal operation))))
                     (is (= operation (adoptions/linked-operation! @conn fork operation))))]
        (valid!)
        ;; Refusals leave the journal applying. Restore only scalar facts;
        ;; the original publisher's transition is never reversed or replayed.
        (doseq [[corrupt restore]
                [[[[:db/add entry :file-output.entry/body-ref (random-uuid)]]
                  [[:db/add entry :file-output.entry/body-ref body-ref]]]
                 [[[:db/add [:file.adoption/id (:id operation)] :file.adoption/token (random-uuid)]]
                  [[:db/add [:file.adoption/id (:id operation)] :file.adoption/token (:token operation)]]]
                 [[{:proposal/id (random-uuid) :proposal/room (:room operation) :proposal/forks [fork]}]
                  nil]]]
          (let [observed (volatile! nil) lookups (atom 0)]
           (try
            (with-redefs [scoped/recover
                          (fn [& args]
                            (let [value (apply lookup args)]
                              (swap! lookups inc)
                              (d/transact conn corrupt)
                              (vreset! observed @conn)
                              value))]
              (is (thrown? clojure.lang.ExceptionInfo (recover! state))))
            (is (= 1 @lookups))
            (is (= @observed @conn) "Refusal must not write beyond the injected corruption")
            (is (= :applying (:file.adoption/state
                              (d/pull @conn [:file.adoption/state] [:file.adoption/id (:id operation)]))))
            (is (= :open (:proposal/status (proposals/get-proposal (:proposal operation)))))
            (is (= target @workspace))
            (finally (when restore (d/transact conn restore)))))
          ;; Shared component ownership is deliberately last and not undone.
          (when restore (valid!)))))))

(deftest unknown-recovery-acks-never-read-back-original-ownership
  (doseq [mode [:missing-before :missing-after :stale :lost]]
    (with-recovery :applying
      (fn [{:keys [conn workspace] :as state}]
        (publish-synthetic! state)
        (let [transact d/transact target @workspace
              error (with-redefs [d/transact
                                  (fn [& args]
                                    (let [report (apply transact args)]
                                      (case mode
                                        :missing-before (dissoc report :db-before)
                                        :missing-after (dissoc report :db-after)
                                        :stale (assoc report :db-after (:db-before report))
                                        :lost (throw (ex-info "Lost recovery ACK" {})))))]
                      (try (recover! state) nil (catch clojure.lang.ExceptionInfo error error)))]
          (is (= ::adoption/recovery-unacknowledged (:type (ex-data error))))
          (is (not (contains? (ex-data error) :token)))
          (is (= :accepted (:state (journal state))))
          ;; A NEW currently authorized observation may verify the accepted
          ;; fact; it cannot recover the prior caller's dispatch ownership.
          (is (true? (:already-accepted? (recover! state))))
          (is (= target @workspace)))))))

(deftest competing-positive-reconcilers-have-one-transition-and-one-no-op
  (with-recovery :applying
    (fn [{:keys [workspace] :as state}]
      (publish-synthetic! state)
      (let [lookup scoped/recover entered (java.util.concurrent.CountDownLatch. 2)
            exited (java.util.concurrent.CountDownLatch. 2)
            gate (promise) target @workspace]
        (with-redefs [scoped/recover (fn [& args]
                                       (let [value (apply lookup args)]
                                         (.countDown entered)
                                         @gate
                                         value))]
          (let [run #(try (recover! state) (finally (.countDown exited)))
                a (future (run)) b (future (run))]
            (try
              (is (.await entered 10000 java.util.concurrent.TimeUnit/MILLISECONDS))
              (deliver gate true)
              (let [answers [(deref a 10000 ::timeout) (deref b 10000 ::timeout)]]
                (is (= #{false true} (set (map :already-accepted? answers))))
                (is (every? #(= :accepted (:status %)) answers)))
              (finally
                (deliver gate true)
                (join-completely! #(.await exited))))))
        (is (= :accepted (:state (journal state))))
        (is (= target @workspace))))))

(deftest terminal-read-is-separate-and-corrupt-or-missing-evidence-cannot-reopen
  (with-recovery :applying
    (fn [{:keys [conn claimed workspace] :as state}]
      (publish-synthetic! state)
      (recover! state)
      (let [fork (get-in claimed [:proof :fork :db/id]) operation (journal state)]
        (is (= operation (adoptions/accepted-operation @conn fork)))
        (is (thrown? clojure.lang.ExceptionInfo (adoptions/current-operation @conn fork)))
        (is (thrown? clojure.lang.ExceptionInfo (adoptions/linked-operation! @conn fork operation)))
        (d/transact workspace [[:db/retractEntity [:publication/id (:id operation)]]])
        (let [before @conn target @workspace]
          (is (thrown? clojure.lang.ExceptionInfo (recover! state)))
          (is (= before @conn))
          (is (= target @workspace)))
        ;; An extra unretired history entry is corruption even for the narrow
        ;; accepted reader, not a reason to loosen the open-only guard.
        (let [other (adoptions/make (select-keys operation [:proposal :component :manifest :room :actor
                                                            :filing :disposition :target])
                                    (random-uuid) (java.util.Date.))]
          (d/transact conn [(@#'adoptions/entity other)])
          (is (thrown? clojure.lang.ExceptionInfo (adoptions/accepted-operation @conn fork))))))))

(deftest wrong-intent-and-partial-terminal-projection-refuse-without-repair
  (doseq [mode [:wrong-intent :partial-terminal]]
    (with-recovery :applying
      (fn [{:keys [conn claimed workspace] :as state}]
        (publish-synthetic! state)
        (if (= :wrong-intent mode)
          (d/transact workspace [[:db/add [:publication/id (get-in claimed [:operation :id])]
                                  :publication/intent (random-uuid)]])
          (do (recover! state)
              (d/transact conn [[:db/add [:proposal/id (get-in claimed [:operation :proposal])]
                                 :proposal/status :open]])))
        (let [before @conn target @workspace]
          (is (thrown? clojure.lang.ExceptionInfo (recover! state)))
          (is (= before @conn))
          (is (= target @workspace)))))))

(deftest interrupted-recovery-retains-interrupt-without-readback-success
  (doseq [committed? [false true]]
    (with-recovery :applying
      (fn [{:keys [conn workspace] :as state}]
        (publish-synthetic! state)
        (let [transact d/transact result (promise) target @workspace
              worker (Thread.
                      ^Runnable
                      (fn []
                        (with-redefs [d/transact (fn [& args]
                                                   (when committed? (apply transact args))
                                                   (throw (InterruptedException. "Injected recovery interruption")))]
                          (try (recover! state) (deliver result ::unexpected)
                               (catch clojure.lang.ExceptionInfo error
                                 (deliver result {:type (:type (ex-data error))
                                                  :interrupted? (.isInterrupted (Thread/currentThread))}))))))]
          (.start worker)
          (try
            (.join worker 10000)
            (is (not (.isAlive worker)))
            (let [answer (deref result 1000 ::timeout)]
              (is (= ::adoption/recovery-unacknowledged (:type answer)))
              (is (true? (:interrupted? answer))))
            (finally (join-completely! #(.join worker))))
          (is (= (if committed? :accepted :applying) (:state (journal state))))
          (is (= target @workspace)))))))
