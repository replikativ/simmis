(ns is.simm.ops.file-output-decisions-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.agents.room-agents :as agents]
            [is.simm.model.access :as access]
            [is.simm.model.file-adoptions :as adoptions]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.file-output-storage :as storage]
            [is.simm.ops.knowledge-filing :as filing]
            [is.simm.ops.knowledge-publish-test :as fixture]
            [is.simm.ops.proposals :as proposals]
            [is.simm.runtimes.branching :as branching]))

(defn- with-filed [f]
  (@#'fixture/with-fixture
   (fn [state]
     (let [receipt (@#'fixture/file! state)
           proposal (:proposal receipt)
           p (proposals/get-proposal proposal)]
       (f (assoc state :proposal proposal :p p :fork (first (:proposal/forks p))))))))

(defn- member! [conn room-id party]
  (d/transact conn [{:party/id party}
                    [:db/add [:room/id room-id] :room/parties [:party/id party]]]))

(defn- withdraw! [{:keys [proposal fork party]} component?]
  (if component?
    (proposals/dismiss-fork! proposal (:proposal.fork/scope fork) (:proposal.fork/branch fork) :by party)
    (proposals/dismiss-proposal! proposal :by party)))

(defn- with-backend-counts [f]
  (let [calls (atom [])]
    (with-redefs [branching/land! (fn [& _] (swap! calls conj :land))
                  branching/drop-branch! (fn [& _] (swap! calls conj :drop))
                  agents/reopen-fork! (fn [& _] (swap! calls conj :reopen))]
      (f))
    (is (empty? @calls) "retained decisions never use branch or overlay backends")))

(deftest authors-can-withdraw-without-merge-or-a-live-target
  (doseq [component? [false true]]
    (with-filed
      (fn [{:keys [conn room-id party proposal workspace] :as state}]
        (member! conn room-id party)
        (d/transact conn [[:db/retract [:room/id room-id] :room/owner [:party/id party]]])
        (is (access/can? @conn party :write {:room room-id}))
        (is (not (access/can? @conn party :merge {:room room-id})))
        (let [target @workspace io (atom 0)]
          (with-backend-counts
            #(with-redefs [filing/current-target (fn [& _] (swap! io inc))
                           storage/read-file-in (fn [& _] (swap! io inc))]
               (is (= :dismissed (:status (withdraw! state component?))))))
          (is (zero? @io))
          (is (= target @workspace))
          (let [p (proposals/get-proposal proposal)]
            (is (= :dismissed (:proposal/status p)))
            (is (= :dismissed (:proposal.fork/status (first (:proposal/forks p)))))
            (is (= party (:proposal/resolved-by p)))))))))

(deftest room-writers-can-request-a-new-revision-but-not-withdraw-another-authors-file
  (with-filed
    (fn [{:keys [conn room-id proposal] :as state}]
      (let [commenter (random-uuid)]
        (member! conn room-id commenter)
        (is (not (access/can? @conn commenter :merge {:room room-id})))
        (with-backend-counts
          #(let [result (proposals/request-changes! proposal {:body "Please revise" :by commenter})]
             (is (uuid? (:comment result)))
             (is (empty? (:reopened result)))
             (is (= 1 (count (:not-reopened result))))))
        (is (thrown? Exception (withdraw! (assoc state :party commenter) false)))
        (is (thrown? Exception (withdraw! (assoc state :party commenter) true)))
        (is (thrown? Exception (withdraw! (assoc state :party nil) false)))
        (is (thrown? Exception (proposals/request-changes! proposal {:body "Anonymous" :by nil})))
        (is (= :open (:proposal/status (proposals/get-proposal proposal))))))))

(deftest pending-journals-fence-decisions-without-blocking-ordinary-audit-comments
  (doseq [state [:claimed :applying]]
    (with-filed
      (fn [{:keys [conn party room-id room proposal fork p]}]
        (d/transact conn adoptions/schema)
        (let [prepared (adoption/prepare conn party room-id room proposal)
              owned (adoption/claim! conn party room-id room prepared)
              operation (:operation owned)]
          (when (= :applying state)
            (d/transact conn [[:db.fn/call adoptions/transition-tx-data
                               (:db/id fork) operation :applying {} (java.util.Date.)]]))
          (with-backend-counts
            #(do
               (is (thrown? Exception (proposals/dismiss-proposal! proposal :by party)))
               (is (thrown? Exception (proposals/dismiss-fork! proposal (:proposal.fork/scope fork)
                                                               (:proposal.fork/branch fork) :by party)))
               (is (thrown? Exception (proposals/request-changes! proposal {:body "Revise" :by party})))
               (is (thrown? Exception (proposals/accept-proposal! proposal :by party :force? true)))
               (is (thrown? Exception (proposals/accept-fork! proposal (:proposal.fork/scope fork)
                                                              (:proposal.fork/branch fork) :by party :force? true)))))
          ;; A stale supplied map cannot bypass the writer-side actual DB check.
          (is (thrown? Exception (proposals/record-decision! (assoc p :proposal/forks []) :accepted nil party)))
          (is (thrown? Exception (proposals/reconcile-status! proposal)))
          (is (uuid? (proposals/add-comment! proposal {:body "Audit while pending" :author party})))
          (is (= state (:state (adoptions/operation @conn (:id operation)))))
          (is (= :open (:proposal/status (proposals/get-proposal proposal)))))))))

(deftest a-completed-withdrawal-prevents-a-stale-prepared-claim
  (with-filed
    (fn [{:keys [conn party room-id room proposal]}]
      (d/transact conn adoptions/schema)
      (let [prepared (adoption/prepare conn party room-id room proposal)]
        (proposals/dismiss-proposal! proposal :by party)
        (is (thrown? Exception (adoption/claim! conn party room-id room prepared)))
        (is (empty? (d/q '[:find ?e :where [?e :file.adoption/id _]] @conn)))))))

(deftest revision-comments-do-not-create-a-lasting-approval-hold
  (with-filed
    (fn [{:keys [conn party room-id room proposal]}]
      (d/transact conn adoptions/schema)
      (let [prepared (adoption/prepare conn party room-id room proposal)]
        (proposals/request-changes! proposal {:body "An audit revision request" :by party})
        (is (= :claimed (:state (:operation (adoption/claim! conn party room-id room prepared)))))))))

(deftest competing-withdrawal-and-claim-have-one-serialized-winner
  (doseq [component? [false true]]
    (with-filed
      (fn [{:keys [conn party room-id room proposal] :as state}]
        (d/transact conn adoptions/schema)
        (let [prepared (adoption/prepare conn party room-id room proposal)
              gate (promise)
              claimant (future @gate (try {:claim (:operation (adoption/claim! conn party room-id room prepared))}
                                          (catch Exception _ {:refused :claim})))
              withdrawer (future @gate (try {:decision (withdraw! state component?)}
                                            (catch Exception _ {:refused :decision})))]
          (deliver gate true)
          (let [claimed (deref claimant 10000 ::timeout)
                withdrawn (deref withdrawer 10000 ::timeout)
                p (proposals/get-proposal proposal)
                count-journals (count (d/q '[:find ?e :where [?e :file.adoption/id _]] @conn))]
            (is (not= ::timeout claimed))
            (is (not= ::timeout withdrawn))
            (if (:claim claimed)
              (do (is (= :decision (:refused withdrawn)))
                  (is (= 1 count-journals))
                  (is (= :open (:proposal/status p)))
                  (is (nil? (:proposal.fork/status (first (:proposal/forks p))))))
              (do (is (= :claim (:refused claimed)))
                  (is (= :dismissed (get-in withdrawn [:decision :status])))
                  (is (zero? count-journals))
                  (is (= :dismissed (:proposal/status p)))))))))))

(deftest current-writer-permission-not-a-prior-routing-snapshot-controls-the-decision
  (with-filed
    (fn [{:keys [conn party room-id proposal]}]
      (let [transact d/transact first? (atom true)]
        (with-redefs [d/transact
                      (fn [& args]
                        (when (compare-and-set! first? true false)
                          (transact conn [[:db/retract [:room/id room-id] :room/owner [:party/id party]]
                                          [:db/retract [:room/id room-id] :room/parties [:party/id party]]]))
                        (apply transact args))]
          (is (thrown? Exception (proposals/request-changes! proposal {:body "Revoked before writer" :by party}))))
        (let [p (proposals/get-proposal proposal)]
          (is (= :open (:proposal/status p)))
          (is (empty? (:proposal/comments p))))))))

(deftest a-witnessed-aborted-claim-allows-withdrawal
  (with-filed
    (fn [{:keys [conn party room-id room proposal fork]}]
      (d/transact conn adoptions/schema)
      (let [prepared (adoption/prepare conn party room-id room proposal)
            operation (:operation (adoption/claim! conn party room-id room prepared))]
        (d/transact conn [[:db.fn/call adoptions/transition-tx-data (:db/id fork) operation
                           :aborted {:reason :witnessed-predispatch-failure} (java.util.Date.)]])
        (is (= :dismissed (:status (proposals/dismiss-proposal! proposal :by party))))))))

(deftest mixed-or-escalated-file-proposals-refuse-before-any-side-effect
  (doseq [mixed? [false true]]
    (with-filed
      (fn [{:keys [conn proposal fork party]}]
        (if mixed?
          (d/transact conn [{:db/id [:proposal/id proposal]
                             :proposal/forks [{:proposal.fork/scope (random-uuid)
                                               :proposal.fork/branch "ordinary" :proposal.fork/system-type :kb}]}])
          (d/transact conn [[:db/add [:proposal/id proposal] :proposal/review (random-uuid)]]))
        (let [before @conn ordinary (first (filter #(= :kb (:proposal.fork/system-type %))
                                                   (:proposal/forks (proposals/get-proposal proposal))))]
          (with-backend-counts
            #(do
               (is (thrown? Exception (proposals/accept-proposal! proposal :by party :force? true)))
               (is (thrown? Exception (proposals/accept-fork! proposal
                                                              (:proposal.fork/scope (or ordinary fork))
                                                              (:proposal.fork/branch (or ordinary fork)) :by party)))
               (is (thrown? Exception (proposals/dismiss-proposal! proposal :by party)))
               (is (thrown? Exception (proposals/dismiss-fork! proposal (:proposal.fork/scope fork)
                                                               (:proposal.fork/branch fork) :by party)))
               (is (thrown? Exception (proposals/request-changes! proposal {:body "No prefix mutation" :by party})))))
          (is (= before @conn)))))))

(deftest direct-retained-reopen-never-even-looks-up-an-agent-overlay
  (let [calls (atom 0)]
    (with-redefs-fn {#'agents/private-overlay (fn [& _] (swap! calls inc) (atom nil))}
      #(is (false? (agents/reopen-fork! (random-uuid) (random-uuid) (random-uuid)
                                        :files-retained :file-output "Cannot reopen evidence"))))
    (is (zero? @calls))))

(deftest missing-or-stale-acknowledged-reports-never-report-decision-success
  (doseq [comment? [false true] missing? [false true]]
    (with-filed
      (fn [{:keys [conn proposal party] :as state}]
        (let [before @conn transact d/transact]
          (with-redefs [d/transact (fn [& args]
                                     (apply transact args)
                                     (if missing? {} {:db-after before}))]
            (is (thrown? Exception
                         (if comment?
                           (proposals/request-changes! proposal {:body "Committed but unacknowledged" :by party})
                           (withdraw! state false)))))
          ;; The underlying decision happened, but its absent/stale ACK cannot
          ;; be replaced with this later read to return success to the caller.
          (let [p (proposals/get-proposal proposal)]
            (if comment?
              (is (= 1 (count (:proposal/comments p))))
              (is (= :dismissed (:proposal/status p))))))))))
