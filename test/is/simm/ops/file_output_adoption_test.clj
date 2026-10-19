(ns is.simm.ops.file-output-adoption-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.agent.episode :as episode]
            [dvergr.artifact :as artifact]
            [is.simm.documents.markdown :as markdown]
            [is.simm.model.file-adoptions :as adoptions]
            [is.simm.model.access :as access]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.file-output-storage :as storage]
            [is.simm.ops.knowledge-filing :as filing]
            [is.simm.ops.knowledge-outputs :as outputs]
            [is.simm.ops.knowledge-publish-test :as fixture]
            [is.simm.ops.proposals :as proposals]))

(defn- with-filed [f]
  (@#'fixture/with-fixture
   (fn [state]
     (let [receipt (@#'fixture/file! state)]
       (f (assoc state :receipt receipt :proposal (:proposal receipt) :manifest (:manifest receipt)))))))

(defn- prepare [{:keys [conn party room-id room proposal]}]
  (adoption/prepare conn party room-id room proposal))

(defn- with-prepared-claim [f]
  (with-filed
    (fn [{:keys [conn] :as state}]
      ;; Not installed by the product yet: only this private memory fixture.
      (d/transact conn adoptions/schema)
      (f (assoc state :prepared (prepare state))))))

(defn- claim! [{:keys [conn party room-id room prepared]}]
  (adoption/claim! conn party room-id room prepared))

(deftest withdrawal-and-revision-permissions-do-not-require-merge
  (with-filed
    (fn [{:keys [conn party room-id proposal manifest]}]
      (let [member (random-uuid) outsider (random-uuid)
            branch (:proposal.fork/branch
                    (first (:proposal/forks (proposals/get-proposal proposal))))
            snapshot #(adoption/decision-snapshot @conn % room-id proposal %2)]
        (d/transact conn [{:party/id member}
                          [:db/add [:room/id room-id] :room/parties [:party/id member]]
                          ;; Author remains a member but loses merge authority.
                          [:db/retract [:room/id room-id] :room/owner]])
        (is (false? (access/can? @conn party :merge {:room room-id})))
        ;; No adoption schema has been installed in this legacy fixture.
        (is (= proposal (get-in (snapshot party {:verb :dismiss-proposal}) [:binding :proposal])))
        (is (= manifest (get-in (snapshot party {:verb :dismiss-fork :scope manifest :branch branch})
                                [:binding :component])))
        (is (thrown? Exception (snapshot member {:verb :dismiss-proposal})))
        (is (thrown? Exception (snapshot member {:verb :dismiss-fork :scope manifest :branch branch})))
        (is (= member (get-in (snapshot member {:verb :request-changes}) [:binding :actor])))
        (is (thrown? Exception (snapshot outsider {:verb :request-changes})))
        (is (thrown? Exception (snapshot nil {:verb :dismiss-proposal})))
        (is (thrown? Exception (snapshot party {:verb :dismiss-proposal :force? true})))
        (is (thrown? Exception (adoption/snapshot @conn party room-id proposal)))))))

(deftest decisions-need-no-target-or-bodies-but-refuse-an-active-publication
  (with-prepared-claim
    (fn [{:keys [conn party room-id proposal prepared] :as state}]
      (let [snapshot #(adoption/decision-snapshot @conn party room-id proposal {:verb %})
            fork-eid (get-in prepared [:proof :fork :db/id])]
        (with-redefs [filing/current-target (fn [& _] (throw (AssertionError. "Target lookup during withdrawal")))
                      storage/read-file-in (fn [& _] (throw (AssertionError. "Body IO during withdrawal")))]
          (is (map? (snapshot :dismiss-proposal)))
          (is (map? (snapshot :request-changes))))
        (let [op (:operation (claim! state))]
          (with-redefs [filing/current-target (fn [& _] (throw (AssertionError. "Target lookup during withdrawal")))
                        storage/read-file-in (fn [& _] (throw (AssertionError. "Body IO during withdrawal")))]
            (is (= op (adoptions/current-operation @conn fork-eid)))
            (is (thrown? Exception (snapshot :dismiss-proposal)))
            (is (thrown? Exception (snapshot :request-changes)))))))))

(deftest journal-outcomes-and-corrupt-links-fence-decisions
  (doseq [state [:applying :accepted :aborted :partial :shared :foreign :orphaned]]
    (with-prepared-claim
      (fn [{:keys [conn party room-id proposal prepared] :as fixture}]
        (let [op (:operation (claim! fixture))
              eid (get-in prepared [:proof :fork :db/id])
              decide #(adoption/decision-snapshot @conn party room-id proposal {:verb :dismiss-proposal})
              transition! (fn [expected next-state fields]
                            (d/transact conn
                                        (adoptions/transition-tx-data @conn eid expected next-state fields
                                                                      (java.util.Date.))))]
          (case state
            :applying (transition! op :applying {})
            :accepted (do (transition! op :applying {})
                          (let [commit (random-uuid)]
                            (transition! (adoptions/operation @conn (:id op)) :accepted
                                         {:noop? false :commit commit :head commit})))
            :aborted (transition! op :aborted {:reason :witnessed-predispatch-failure})
            :partial (d/transact conn [[:db/retract [:file.adoption/id (:id op)] :file.adoption/intent]])
            :shared (d/transact conn [{:proposal.fork/scope (random-uuid)
                                       :proposal.fork/system-type :file-output
                                       :proposal.fork/file-adoption [:file.adoption/id (:id op)]}])
            :foreign (let [component (random-uuid)
                           other (adoptions/make (assoc (:binding (:proof prepared))
                                                        :component component :manifest component)
                                                 (random-uuid) (java.util.Date.))]
                       (d/transact conn [{:db/id -1 :proposal.fork/scope component
                                          :proposal.fork/system-type :file-output}])
                       (let [other-eid (d/q '[:find ?e . :in $ ?scope
                                              :where [?e :proposal.fork/scope ?scope]] @conn component)]
                         (d/transact conn (adoptions/claim-tx-data @conn other-eid other))
                         (d/transact conn [[:db/retract other-eid :proposal.fork/file-adoption]
                                           [:db/add eid :proposal.fork/file-adoption
                                            [:file.adoption/id (:id other)]]])))
            :orphaned (d/transact conn [[:db/retract eid :proposal.fork/file-adoption]]))
          (if (= :aborted state)
            (do (is (map? (decide)))
                (let [member (random-uuid)]
                  (d/transact conn [{:party/id member}
                                    [:db/add [:room/id room-id] :room/parties [:party/id member]]])
                  (is (= member (get-in (adoption/decision-snapshot @conn member room-id proposal
                                                                    {:verb :request-changes})
                                        [:binding :actor])))))
            (is (thrown? Exception (decide)))))))))

(deftest preflight-uses-retained-evidence-not-an-expired-attempt-or-live-file
  (with-filed
    (fn [{:keys [conn workspace party proposal manifest receipt] :as state}]
      (let [control-before @conn files-before @workspace
            result (with-redefs [episode/attempt (fn [& _] (throw (AssertionError. "Attempt expired")))]
                     (prepare state))
            proof (:proof result)]
        (is (= control-before @conn))
        (is (= files-before @workspace))
        (is (= proposal (get-in proof [:binding :proposal])))
        (is (= manifest (get-in proof [:binding :component])))
        (is (= party (get-in proof [:binding :actor])))
        (is (= (:id receipt) (get-in proof [:binding :disposition])))
        (is (= (:target (:basis state)) (get-in proof [:binding :target])))
        (is (every? uuid? (vals (get-in proof [:retained :body-refs]))))
        (is (= "Historical before\r\n"
               (markdown/decode (outputs/bytes! (get-in result [:plan :bodies "wiki/existing.md" :before])))))
        (is (= "Proposed after\r\n"
               (markdown/decode (outputs/bytes! (get-in result [:plan :bodies "wiki/existing.md" :after])))))
        ;; No schema installation, claim, status change or target publication.
        (is (= :open (:proposal/status (proposals/get-proposal proposal))))))))

(deftest authorization-and-target-refusal-precede-every-body-read
  (with-filed
    (fn [{:keys [conn room-id basis] :as state}]
      (let [reads (atom 0) original storage/read-file-in]
        (with-redefs [storage/read-file-in (fn [& args] (swap! reads inc) (apply original args))]
          (is (thrown? Exception (prepare (assoc state :party (random-uuid)))))
          (is (thrown? Exception (prepare (assoc state :party nil))))
          (is (thrown? Exception (prepare (assoc state :room-id (random-uuid)))))
          (is (thrown? Exception (prepare (assoc state :conn (atom @conn)))))
          (with-redefs [filing/current-target (fn [& _] (assoc (:target basis) :store (random-uuid)))]
            (is (thrown? Exception (prepare state))))
          (is (zero? @reads))
          ;; Room membership alone does not grant merge after owner revocation.
          (d/transact conn [[:db/retract [:room/id room-id] :room/owner]])
          (is (thrown? Exception (prepare state)))
          (is (zero? @reads)))))))

(deftest mixed-review-resolved-and-partial-filing-refuse-without-reading-bodies
  (doseq [alter! [(fn [{:keys [conn proposal]}]
                    (d/transact conn [{:db/id [:proposal/id proposal]
                                       :proposal/forks [{:proposal.fork/scope (random-uuid)
                                                         :proposal.fork/branch "other"
                                                         :proposal.fork/system-type :kb}]}]))
                  (fn [{:keys [conn proposal]}]
                    (d/transact conn [[:db/add [:proposal/id proposal] :proposal/review (random-uuid)]]))
                  (fn [{:keys [conn proposal]}]
                    (d/transact conn [[:db/add [:proposal/id proposal] :proposal/status :dismissed]]))
                  (fn [{:keys [conn request]}]
                    (d/transact conn [[:db/retract [:work.request/id request] :knowledge.disposition/output]]))
                  (fn [{:keys [conn proposal]}]
                    (d/transact conn [[:db/add [:proposal/id proposal] :proposal/run (random-uuid)]]))]]
    (with-filed
      (fn [state]
        (alter! state)
        (with-redefs [storage/read-file-in (fn [& _] (throw (AssertionError. "Body read before refusal")))]
          (is (thrown? Exception (prepare state))))))))

(deftest missing-or-corrupt-retained-bytes-fail-before-any-adoption
  (doseq [value [nil {:before nil :after nil}]]
    (with-filed
      (fn [{:keys [conn proposal workspace] :as state}]
        (let [before @conn target @workspace]
          (with-redefs [artifact/get-value (constantly value)]
            (is (thrown? Exception (prepare state))))
          (is (= before @conn))
          (is (= target @workspace))
          (is (= :open (:proposal/status (proposals/get-proposal proposal)))))))))

(deftest exact-pair-reads-use-the-supplied-snapshot-not-a-later-body-reference
  (with-filed
    (fn [{:keys [conn manifest]}]
      (let [db @conn
            retained (storage/retained-in db manifest)
            entry-eid (:v (first (d/datoms db :eavt
                                           (:e (first (d/datoms db :avet :file-output/id manifest)))
                                           :file-output/entries)))
            original-ref (get-in retained [:body-refs "wiki/existing.md"])
            seen (atom nil)
            original artifact/get-value]
        (d/transact conn [[:db/add entry-eid :file-output.entry/body-ref (random-uuid)]])
        (with-redefs [artifact/get-value (fn [store ref] (reset! seen ref) (original store ref))]
          (is (map? (storage/read-file-in conn db manifest "wiki/existing.md")))
          (is (= original-ref @seen))
          ;; New snapshot names a missing blob and must refuse, never fall back.
          (is (thrown? Exception (storage/read-file conn manifest "wiki/existing.md"))))))))

(deftest acknowledged-claim-is-one-internal-owner-not-a-publication
  (with-prepared-claim
    (fn [{:keys [conn workspace proposal prepared] :as state}]
      (let [files-before @workspace
            result (claim! state)
            operation (:operation result)]
        (is (= :claimed (:state operation)))
        (is (= (:binding (:proof prepared))
               (select-keys operation [:proposal :component :manifest :room :actor :filing :disposition :target])))
        (is (= operation (adoptions/operation @conn (:id operation))))
        (is (= prepared (dissoc result :operation)))
        (is (thrown? Exception (claim! state)))
        (is (= 1 (count (d/q '[:find ?e :where [?e :file.adoption/id _]] @conn))))
        (is (= files-before @workspace))
        (is (= :open (:proposal/status (proposals/get-proposal proposal))))))))

(deftest concurrent-host-claims-have-one-acknowledged-owner
  (with-prepared-claim
    (fn [{:keys [conn workspace] :as state}]
      (let [start (promise) files-before @workspace
            workers (mapv (fn [_]
                            (future @start
                                    (try (:operation (claim! state))
                                         (catch clojure.lang.ExceptionInfo error
                                           (:type (ex-data error)))))) (range 4))]
        (deliver start true)
        (let [results (mapv #(deref % 10000 ::timeout) workers)
              owners (filterv map? results)]
          (is (not-any? #{::timeout} results))
          (is (= 1 (count owners)))
          (is (= 3 (count (filter #{::adoption/claim-unacknowledged} results))))
          (is (= (first owners) (adoptions/operation @conn (:id (first owners)))))
          (is (= files-before @workspace)))))))

(deftest writer-rechecks-frozen-proof-authority-and-target-without-body-reads
  (doseq [alter! [(fn [state] (assoc-in state [:prepared :proof :proposal :proposal/run] (random-uuid)))
                  (fn [{:keys [conn room-id] :as state}]
                    (d/transact conn [[:db/retract [:room/id room-id] :room/owner]]) state)
                  (fn [{:keys [conn] :as state}]
                    ;; The bounded fork authority projection omits entries.
                    (let [manifest-eid (:e (first (d/datoms @conn :avet :file-output/id (:manifest state))))
                          eid (:v (first (d/datoms @conn :eavt manifest-eid :file-output/entries)))]
                      (d/transact conn [[:db/add eid :file-output.entry/body-ref (random-uuid)]]))
                    state)]]
    (with-prepared-claim
      (fn [{:keys [conn workspace] :as initial}]
        (let [state (alter! initial) target @workspace reads (atom 0)]
          (with-redefs [storage/read-file-in (fn [& _] (swap! reads inc) (throw (AssertionError. "Artifact IO in writer")))]
            (is (thrown? Exception (claim! state))))
          (is (zero? @reads))
          (is (empty? (d/q '[:find ?e :where [?e :file.adoption/id _]] @conn)))
          (is (= target @workspace))))))
  (with-prepared-claim
    (fn [{:keys [conn basis] :as state}]
      (with-redefs [filing/current-target (fn [& _] (assoc (:target basis) :store (random-uuid)))]
        (is (thrown? Exception (claim! state))))
      (is (empty? (d/q '[:find ?e :where [?e :file.adoption/id _]] @conn))))))

(deftest lost-claim-ack-never-recovers-publisher-ownership-from-readback
  (with-prepared-claim
    (fn [{:keys [conn workspace] :as state}]
      (let [original d/transact target @workspace readbacks (atom 0)
            error (with-redefs [d/transact (fn [& args]
                                             (apply original args)
                                             (throw (ex-info "Injected lost ACK after commit" {})))
                                adoptions/linked-operation! (fn [& _]
                                                              (swap! readbacks inc)
                                                              (throw (AssertionError. "Ownership readback after lost ACK")))]
                    (try (claim! state) nil
                         (catch clojure.lang.ExceptionInfo error error)))
            id (:operation (ex-data error))]
        (is (= ::adoption/claim-unacknowledged (:type (ex-data error))))
        (is (zero? @readbacks))
        (is (uuid? id))
        (is (not (contains? (ex-data error) :token)))
        (is (= :claimed (:state (adoptions/operation @conn id))))
        (is (thrown? Exception (claim! state)))
        (is (= 1 (count (d/q '[:find ?e :where [?e :file.adoption/id _]] @conn))))
        (is (= target @workspace))))))

(deftest interrupted-claim-keeps-interrupt-and-does-not-return-ownership
  (with-prepared-claim
    (fn [state]
      (let [result (promise)
            worker (Thread.
                    ^Runnable
                    (fn []
                      (with-redefs [d/transact (fn [& _] (throw (InterruptedException. "Injected claim interruption")))]
                        (try (claim! state) (deliver result ::unexpected)
                             (catch clojure.lang.ExceptionInfo error
                               (deliver result {:type (:type (ex-data error))
                                                :operation (:operation (ex-data error))
                                                :interrupted? (.isInterrupted (Thread/currentThread))}))))))]
        (.start worker)
        (.join worker 10000)
        (is (not (.isAlive worker)))
        (let [answer (deref result 1000 ::timeout)]
          (is (= ::adoption/claim-unacknowledged (:type answer)))
          (is (uuid? (:operation answer)))
          (is (true? (:interrupted? answer))))))))

(deftest missing-or-stale-ack-report-cannot-be-replaced-by-live-readback
  (doseq [mode [:missing :stale]]
    (with-prepared-claim
      (fn [{:keys [conn workspace] :as state}]
        (let [original d/transact target @workspace
              error (with-redefs [d/transact
                                  (fn [& args]
                                    (let [report (apply original args)]
                                      (case mode
                                        :missing (dissoc report :db-after)
                                        :stale (assoc report :db-after (:db-before report)))))]
                      (try (claim! state) nil
                           (catch clojure.lang.ExceptionInfo error error)))
              id (:operation (ex-data error))]
          (is (= ::adoption/claim-unacknowledged (:type (ex-data error))))
          (is (uuid? id))
          (is (= :claimed (:state (adoptions/operation @conn id))))
          (is (thrown? Exception (claim! state)))
          (is (= 1 (count (d/q '[:find ?e :where [?e :file.adoption/id _]] @conn))))
          (is (= target @workspace)))))))
