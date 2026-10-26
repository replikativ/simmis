(ns is.simm.ops.file-output-adoption
  "INTERNAL fixed control writers prepare, claim, apply and finalize joint
   adoption facts. No endpoint, schema installation, target publication or
   execution. Returned data is NOT a capability or substitute for original ACKs.
   Admission/dispatch repeats current authority/target checks; finalization
   only records an original publisher's
   observed fact. Every transition compares the exact frozen storage proof."
  (:require [datahike.api :as d]
            [dvergr.rooms.publication :as scoped-publication]
            [is.simm.model.access :as access]
            [is.simm.model.file-adoptions :as adoptions]
            [is.simm.model.file-outputs :as manifests]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.knowledge-processing :as processing]
            [is.simm.ops.knowledge-batch-resolution :as batch-resolution]
            [is.simm.model.knowledge-publication :as publication]
            [is.simm.model.system-db :as sdb]
            [is.simm.ops.file-output-plan :as plans]
            [is.simm.ops.file-output-publication :as native-publication]
            [is.simm.ops.file-output-storage :as storage]
            [is.simm.ops.knowledge-filing :as filing]
            [is.simm.ops.owned-workspace :as owned-workspace]
            [org.replikativ.spindel.engine.impl.simple :as simple]
            [malli.core :as m]))

(defn- refuse! [reason]
  (throw (ex-info "Retained file adoption is unavailable"
                  {:type ::unavailable :reason reason})))

(defn- host-connection! [conn]
  (when (simple/on-drain-thread?) (refuse! :requires-host-worker))
  (when-not (and conn (identical? conn (sdb/get-conn))) (refuse! :noncanonical-connection)))

(defn- filing-rows!
  "Bounded structural header. State-specific callers keep their own CLOSED
   admission rules; neither this reader nor metadata gives any authority."
  [db room-id proposal-id]
  (let [p (d/pull db [:db/id :proposal/id :proposal/room :proposal/status
                      :proposal/author :proposal/run :proposal/filing-hash :proposal/review]
                  [:proposal/id proposal-id])]
    (when-not (and (= proposal-id (:proposal/id p)) (= room-id (:proposal/room p))
                   (nil? (:proposal/review p)))
      (refuse! :unsupported-proposal))
    (let [links (vec (take 2 (d/datoms db :eavt (:db/id p) :proposal/forks)))]
      (when-not (= 1 (count links)) (refuse! :unsupported-component-set))
      (let [fork (d/pull db [:db/id :proposal.fork/scope :proposal.fork/authority-scope
                             :proposal.fork/branch :proposal.fork/status :proposal.fork/system-type
                             {:proposal.fork/file-output [:file-output/id :file-output/room]}]
                         (:v (first links)))]
        (when-not (and (= :file-output (:proposal.fork/system-type fork))
                       (= room-id (:proposal.fork/authority-scope fork))
                       (string? (:proposal.fork/branch fork))
                       (re-matches #"[A-Za-z0-9._-]{1,128}" (:proposal.fork/branch fork)))
          (refuse! :unsupported-component))
        {:proposal p :fork fork}))))

(defn- filing-proof!
  "Canonical immutable provenance, after the caller's exact authority/state
   checks. Original publisher finalization does not admit fresh work."
  [db party room-id proposal-id {:keys [proposal fork]}]
  (let [p proposal scope (:proposal.fork/scope fork)
        owner (manifests/owner db scope)]
    (when-not (= (select-keys owner [:proposal :room :fork-eid :proposal-eid])
                 {:proposal proposal-id :room room-id :fork-eid (:db/id fork) :proposal-eid (:db/id p)})
      (refuse! :invalid-owner))
    (let [{:keys [manifest] :as retained} (storage/retained-in db scope)
          request (d/pull db '[*] [:work.request/id (:request manifest)])
          disposition (dispositions/disposition request)
          binding {:proposal proposal-id :component scope :manifest (:id manifest)
                   :room room-id :actor party :filing (:proposal/filing-hash p)
                   :disposition (:id disposition) :target (:target manifest)}]
      (when-not (and (= :proposed (:kind disposition))
                     (= proposal-id (:proposal disposition))
                     (= scope (:manifest disposition))
                     (= room-id (:room manifest) (:work.request/room request))
                     (= (:proposal/author p) (:work.request/by request))
                     (= (:proposal/run p) (:candidate disposition) (:attempt manifest))
                     (= (:request manifest) (:request disposition))
                     (= (:basis manifest) (:basis disposition))
                     (= (:output manifest) (:output disposition))
                     (seq (:entries manifest))
                     (publication/host-target? (:target manifest))
                     (m/validate adoptions/binding-schema binding))
        (refuse! :invalid-filing))
      {:binding binding :proposal p :fork fork :retained retained :disposition disposition})))

(defn- filing-snapshot
  "OPEN-only canonical filing reader. Preserve existing claim/decision/dispatch
   admission; terminal recovery uses its own separately authorized state gate."
  [db party room-id proposal-id]
  ;; Preserve the early closed-Proposal refusal before component expansion.
  (when-not (= :open (:proposal/status (d/pull db [:proposal/status] [:proposal/id proposal-id])))
    (refuse! :unsupported-proposal))
  (let [{:keys [fork] :as rows} (filing-rows! db room-id proposal-id)]
    (when-not (nil? (:proposal.fork/status fork)) (refuse! :unsupported-component))
    (filing-proof! db party room-id proposal-id rows)))

(defn snapshot
  "INTERNAL merge-authorized control-DB read. Only one ordinary open file
   component is supported. No bodies are loaded and no ownership is granted.
   Repeat against the serialized writer DB before any journal transition."
  [db party room-id proposal-id]
  (when-not (and (every? uuid? [party room-id proposal-id])
                 (access/can? db party :read {:room room-id})
                 (access/can? db party :write {:proposal proposal-id})
                 (access/can? db party :merge {:room room-id}))
    (refuse! :denied))
  (let [proof (filing-snapshot db party room-id proposal-id)]
    (when-not (access/may-merge-proposal-fork? db party (:fork proof))
      (refuse! :denied))
    proof))

(defn decision-snapshot
  "INTERNAL pure writer guard for retained-file withdrawal/revision requests.
   Preserve each existing public permission resource, including author
   withdrawal without merge authority. No live target lookup, body IO, branch
   mutation or journal ownership recovery. Compose the decision datoms in the
   SAME serialized transaction that calls this function."
  [db party room-id proposal-id {:keys [verb scope branch] :as action}]
  (let [resource (case verb
                   :dismiss-proposal (when (= #{:verb} (set (keys action)))
                                       {:proposal-forks proposal-id})
                   :dismiss-fork (when (and (= #{:verb :scope :branch} (set (keys action)))
                                            (uuid? scope) (or (string? branch) (keyword? branch)))
                                   {:proposal-fork {:proposal proposal-id :scope scope :branch branch}})
                   :request-changes (when (and (every? #{:verb :branch} (keys action))
                                               (or (nil? branch) (string? branch) (keyword? branch)))
                                      {:proposal proposal-id})
                   nil)]
    (when-not (and resource (every? uuid? [party room-id proposal-id])
                   (access/can? db party :write resource))
      (refuse! :denied))
    (let [{:keys [binding fork] :as proof} (filing-snapshot db party room-id proposal-id)
          operation (adoptions/current-operation db (:db/id fork))]
      (when (or (and (= :dismiss-fork verb) (not= scope (:proposal.fork/scope fork)))
                (and branch (not= (name branch) (:proposal.fork/branch fork))))
        (refuse! :unsupported-component))
      (when (and operation
                 (or (not= :aborted (:state operation))
                     (not= (dissoc binding :actor)
                           (select-keys operation (remove #{:actor} (keys binding))))))
        (refuse! :publication-pending))
      proof)))

(defn prepare
  "Host-only read preflight for a NEW adoption. Canonical Room/default target
   verification precedes the first body fetch. Freeze all exact body refs from
   one DB; validate every bounded pair before returning. Concurrent deletion/GC
   refuses, never repairs/reexecutes. Retained evidence outlives the Attempt.
   Party is captured by the trusted authenticated host, never a client field.
   No caller-supplied bytes, manifest, descriptor or claim token is accepted."
  [conn party room-id room proposal-id]
  (host-connection! conn)
  (let [db @conn
        {:keys [binding retained] :as proof} (snapshot db party room-id proposal-id)
        manifest (:manifest retained)]
    (when-not (= (:target binding) (filing/current-target db room-id room))
      (refuse! :target-changed))
    (let [bodies (into (sorted-map)
                       (for [path (keys (:entries manifest))]
                         [path (storage/read-file-in conn db (:id manifest) path)]))]
      (plans/validate-plan! {:manifest manifest :proposal proposal-id :bodies bodies})
      {:proof proof :plan {:manifest manifest :proposal proposal-id :bodies bodies}})))

(defn- prepared! [{:keys [proof plan] :as prepared} party room-id]
  (when-not (and (= #{:proof :plan} (set (keys prepared)))
                 (= party (get-in proof [:binding :actor]))
                 (= room-id (get-in proof [:binding :room]))
                 (= (:manifest plan) (get-in proof [:retained :manifest]))
                 (= (:proposal plan) (get-in proof [:binding :proposal])))
    (refuse! :invalid-preflight))
  (plans/validate-plan! plan)
  prepared)

(defn- claim-tx
  "Fixed writer composition: no artifact IO, hydration, Room lock, nested
   transaction or caller-supplied transaction callback. The preflight is data,
   not authority. Check its COMPLETE proof against this serialized DB value."
  [db party room-id room proof operation]
  (let [current (snapshot db party room-id (get-in proof [:binding :proposal]))]
    (when-not (= proof current) (refuse! :preflight-changed))
    (when-not (= (get-in proof [:binding :target]) (filing/current-target db room-id room))
      (refuse! :target-changed))
    (adoptions/claim-tx-data db (get-in proof [:fork :db/id]) operation)))

(defn claim!
  "INTERNAL trusted-host storage boundary, NOT an endpoint or publication.
   Takes only the host's prepared value; never accept this map from a client.
   Schema must already be installed by the owning host. An optional HOST review
   revision is recorded as immutable version-2 intent data, not authority.

   Fresh invocation token, one claim transaction, NO retries. Only its original
   acknowledged report confers permission to continue towards dispatch. Any
   error/lost ACK leaves ownership unacknowledged: expose only operation ID,
   never infer ownership from a live readback. Applying and all competing
   decision paths must be wired before this is used by a public accept action."
  ([conn party room-id room prepared] (claim! conn party room-id room prepared nil))
  ([conn party room-id room prepared review-revision]
   (host-connection! conn)
   (prepared! prepared party room-id)
   (let [proof (:proof prepared)
         operation (adoptions/make (:binding proof) (random-uuid) (java.util.Date.) review-revision)]
     (try
       (let [report (d/transact conn [[:db.fn/call
                                       (fn [db] (claim-tx db party room-id room proof operation))]])]
         (when-not (:db-after report) (refuse! :claim-report-unavailable))
        ;; Only the immutable result of THIS acknowledged native invocation.
        ;; Do not replace it with @conn, even if the same token is readable.
         (adoptions/linked-operation! (:db-after report) (get-in proof [:fork :db/id]) operation)
         (assoc prepared :operation operation))
       (catch Throwable error
         (when (instance? InterruptedException error) (.interrupt (Thread/currentThread)))
         (throw (ex-info "File adoption claim was not acknowledged; do not publish"
                         {:type ::claim-unacknowledged :operation (:id operation)} error)))))))

(defn- applying-with-binding!
  "INTERNAL original-publisher callback, not a route or recovery operation.
   Only call with this native invocation's acknowledged claim and exact native
   pre-dispatch candidate. Recheck the full proof, current authority/target and
   exact latest token in the SAME writer transaction. No target IO occurs.

   Only THIS transition's acknowledged immutable report permits the caller to
   return from before-dispatch and let the native writer start. Missing/lost
   ACK throws and never recovers ownership from readback. It can leave applying
   without a publication: absence of a receipt must never authorize replay."
  [conn party room-id room claimed candidate owned-binding]
  (host-connection! conn)
  (when-not (= #{:proof :plan :operation} (set (keys claimed)))
    (refuse! :invalid-claim))
  (prepared! (dissoc claimed :operation) party room-id)
  (let [{:keys [proof plan operation]} claimed
        _ (when-not (and (= :claimed (:state operation))
                         (= (:binding proof)
                            (select-keys operation (keys (:binding proof)))))
            (refuse! :invalid-claim))
        _ (native-publication/validate-candidate! operation (:manifest plan) candidate)
        at (java.util.Date.)
        expected (adoptions/validate! (assoc operation :state :applying :updated-at at))]
    (try
      (let [report (d/transact
                    conn
                    [[:db.fn/call
                      (fn [db]
                        (when-not (= proof (snapshot db party room-id (get-in proof [:binding :proposal])))
                          (refuse! :preflight-changed))
                        (when-not (= (get-in proof [:binding :target]) (filing/current-target db room-id room))
                          (refuse! :target-changed))
                        ;; Fixed host binding, not a caller-supplied transaction
                        ;; callback. Use THIS writer snapshot for ownership and
                        ;; authority, retaining the original live System/conn.
                        (when owned-binding
                          (let [current (owned-workspace/validate! db party :merge room-id room owned-binding)]
                            (when-not (= (get-in proof [:binding :target]) (:target current))
                              (refuse! :target-changed))))
                        (adoptions/transition-tx-data db (get-in proof [:fork :db/id]) operation
                                                      :applying {} at))]])]
        (when-not (:db-after report) (refuse! :applying-report-unavailable))
        (adoptions/linked-operation! (:db-after report) (get-in proof [:fork :db/id]) expected)
        (assoc claimed :operation expected))
      (catch Throwable error
        (when (instance? InterruptedException error) (.interrupt (Thread/currentThread)))
        (throw (ex-info "File adoption dispatch transition was not acknowledged; do not publish"
                        {:type ::applying-unacknowledged :operation (:id operation)} error))))))

(defn applying!
  "Internal journal-only transition. Native publication controllers must use
   bound-applying! instead, so the original host binding is rechecked in the
   serialized admission. Both paths require their own original immutable ACK."
  [conn party room-id room claimed candidate]
  (applying-with-binding! conn party room-id room claimed candidate nil))

(defn bound-applying!
  "INTERNAL fixed native dispatch gate. Revalidate the original trusted-host
   owned binding in the SAME transaction as proof, authority and latest-token
   checks. The binding is neither persisted nor accepted from a client; no
   arbitrary callback, target mutation, artifact-body IO or lifetime lease is
   introduced."
  [conn party room-id room claimed candidate owned-binding]
  (when-not (map? owned-binding) (refuse! :missing-owned-binding))
  (applying-with-binding! conn party room-id room claimed candidate owned-binding))

(defn causal-interruption?
  "INTERNAL diagnostic predicate. Inspect actual Throwable causes cycle-safely;
   exception data is never native completion or journal-transition authority."
  [error]
  (let [seen (java.util.IdentityHashMap.)]
    (loop [cause error]
      (cond
        (not (instance? Throwable cause)) false
        (.containsKey seen cause) false
        (instance? InterruptedException cause) true
        :else (do (.put seen cause true) (recur (.getCause ^Throwable cause)))))))

(defn bound-not-started!
  "INTERNAL original publisher cleanup, never an endpoint/recovery operation.
   The controller supplies ONLY its direct normal fixed-native completion and
   ORIGINAL acknowledged claim/binding. This map validation cannot authenticate
   provenance: neither clients nor stored/Throwable maps supply a witness.

   Strict current authority, original target/stamp and complete filing proof
   recheck in the SAME writer as exact claimed->aborted. Applying NEVER aborts.
   Only the ORIGINAL before/after report ACK establishes returned success; lost
   ACK remains pending even if an abort committed. No readback, dispatch, body IO,
   target mutation or automatic retry. Interrupted callers do not submit cleanup."
  [conn party room-id room claimed owned-binding completion]
  (host-connection! conn)
  (when (.isInterrupted (Thread/currentThread)) (refuse! :interrupted-cleanup))
  (when-not (and (= #{:proof :plan :operation} (set (keys claimed)))
                 (map? owned-binding) (map? completion)
                 (= #{:completion :result :error} (set (keys completion)))
                 (= :not-started (:completion completion))
                 (or (nil? (:error completion)) (instance? Throwable (:error completion))))
    (refuse! :invalid-not-started))
  (prepared! (dissoc claimed :operation) party room-id)
  (let [{:keys [proof operation]} claimed
        _ (adoptions/validate! operation)
        _ (when-not (and (= :claimed (:state operation))
                         (= (:binding proof) (select-keys operation (keys (:binding proof)))))
            (refuse! :invalid-claim))
        at (java.util.Date.)
        expected (adoptions/validate! (assoc operation :state :aborted
                                             :reason :native-not-started :updated-at at))
        fork-eid (get-in proof [:fork :db/id])]
    (try
      (let [report (d/transact
                    conn
                    [[:db.fn/call
                      (fn [db]
                        (when-not (identical? conn (sdb/get-conn)) (refuse! :noncanonical-connection))
                        (when-not (= proof (snapshot db party room-id (:proposal operation)))
                          (refuse! :preflight-changed))
                        (let [current (owned-workspace/validate! db party :merge room-id room owned-binding)]
                          (when-not (= (get-in proof [:binding :target]) (:target current))
                            (refuse! :target-changed)))
                        (adoptions/transition-tx-data db fork-eid operation :aborted
                                                      {:reason :native-not-started} at))]])]
        (when-not (and (:db-before report) (:db-after report)) (refuse! :abort-report-unavailable))
        ;; Never substitute @conn. Both snapshots must describe THIS exact
        ;; transition, the unique latest owner, and the same still-open filing.
        (doseq [[db value] [[(:db-before report) operation] [(:db-after report) expected]]]
          (when-not (= proof (snapshot db party room-id (:proposal operation)))
            (refuse! :abort-report-mismatch))
          (adoptions/linked-operation! db fork-eid value))
        {:status :aborted :operation (:id operation) :reason :native-not-started})
      (catch Throwable error
        (when (causal-interruption? error) (.interrupt (Thread/currentThread)))
        (throw (ex-info "File adoption cleanup was not acknowledged; do not replay publication"
                        {:type ::abort-unacknowledged :operation (:id operation)} error))))))

(defn- terminal-facts!
  "Terminal integrity only, not dispatch ownership. The host separately owns
   current authorization or its original native invocation's ACK obligation."
  [db proof expected at]
  (when-not db (refuse! :finalization-report-unavailable))
  (try
    (let [facts (adoptions/terminal-facts! db expected)]
      (when-not (and (= at (:updated-at expected))
                     (= (get-in proof [:fork :db/id]) (:fork-eid facts)))
        (refuse! :finalization-report-mismatch))
      (dissoc facts :fork-eid))
    (catch Exception _ (refuse! :finalization-report-mismatch))))

(defn- finalized-report!
  "Original immutable ACK only. Do not substitute a live ownership readback."
  [db proof expected at & [batch]]
  (let [facts (terminal-facts! db proof expected at)]
    (when batch
      (when-not (empty? (processing/acceptance-tx db (:expected batch) (:value batch) expected))
        (refuse! :batch-finalization-report-mismatch)))
    facts))

(defn finalize!
  "INTERNAL original publisher's finalization, NOT an endpoint or recovery API.
   The owning host supplies its ORIGINAL acknowledged applying value and the
   already observed scoped WINNING receipt from the canonical native writer.
   Pure receipt validity alone is not evidence that publication occurred.

   Record that observed fact even if permission/default selection changed
   AFTER publication. No new authorization, target lookup, body IO or dispatch
   occurs here. Canonical filing integrity and exact latest ownership are still
   checked in the SAME transaction as all three terminal records. Corruption
   leaves the outcome unresolved; it never permits abort, restore or replay.

   Only the original report establishes returned success. A lost ACK can hide
   durable acceptance; never infer original ownership from a live readback."
  [conn applying receipt]
  (host-connection! conn)
  (when-not (= #{:proof :plan :operation} (set (keys applying)))
    (refuse! :invalid-applying))
  (let [{:keys [proof plan operation]} applying
        {:keys [proposal room actor]} operation]
    (adoptions/validate! operation)
    (when-not (and (= :applying (:state operation))
                   (= (:binding proof) (select-keys operation (keys (:binding proof))))
                   (= (:manifest plan) (get-in proof [:retained :manifest]))
                   (= (:proposal plan) proposal))
      (refuse! :invalid-applying))
    (native-publication/validate-receipt! operation (:manifest plan) receipt)
    (let [at (java.util.Date.)
          outcome (into {} (for [key [:commit :head :noop?]
                                 :let [attr (keyword "publication" (name key))]
                                 :when (contains? receipt attr)]
                             [key (get receipt attr)]))
          expected (adoptions/validate! (merge operation outcome {:state :accepted :updated-at at}))
          batch (batch-resolution/retained! conn (:disposition proof))]
      (try
        (let [report (d/transact
                      conn
                      [[:db.fn/call
                        (fn [db]
                          ;; This records an admitted operation's observed fact,
                          ;; not a fresh merge or a permission-bypass option.
                          (when-not (= proof (filing-snapshot db actor room proposal))
                            (refuse! :preflight-changed))
                          (into (adoptions/transition-tx-data db (get-in proof [:fork :db/id])
                                                              operation :accepted outcome at)
                                (concat [{:db/id (get-in proof [:fork :db/id])
                                          :proposal.fork/status :accepted
                                          :proposal/resolved-by actor :proposal/resolved-at at}
                                         {:db/id (get-in proof [:proposal :db/id])
                                          :proposal/status :accepted
                                          :proposal/resolved-by actor :proposal/resolved-at at}]
                                        (when batch
                                          (processing/acceptance-tx db (:expected batch) (:value batch) expected)))))]])]
          (finalized-report! (:db-after report) proof expected at batch))
        (catch Throwable error
          (when (instance? InterruptedException error) (.interrupt (Thread/currentThread)))
          (throw (ex-info "File adoption finalization was not acknowledged; durable outcome may already be accepted"
                          {:type ::finalization-unacknowledged :operation (:id operation)} error)))))))

(defn reconciliation-snapshot
  "HOST metadata-only current authority and coherent joint recovery state.
   Supports accepted history as well as uncertain open publication. Authorize
   before Room hydration; returned proof is NOT publisher ownership, a receipt
   or native target capability. Never expose it to clients or SCI."
  [db party room-id proposal-id]
  (when-not (and (every? uuid? [party room-id proposal-id])
                 (access/can? db party :read {:room room-id})
                 (access/can? db party :read {:proposal proposal-id})
                 (access/can? db party :write {:proposal proposal-id})
                 (access/can? db party :merge {:room room-id}))
    (refuse! :denied))
  (let [{:keys [proposal fork] :as rows} (filing-rows! db room-id proposal-id)
        _ (when-not (access/may-merge-proposal-fork? db party fork) (refuse! :denied))
        state [(:proposal/status proposal) (:proposal.fork/status fork)]
        operation (cond
                    (= [:open nil] state) (adoptions/current-operation db (:db/id fork))
                    (= [:accepted :accepted] state) (adoptions/accepted-operation db (:db/id fork))
                    :else (refuse! :inconsistent-terminal-state))]
    (when-not (#{:claimed :applying :accepted} (:state operation)) (refuse! :not-recoverable))
    (when-not (= (= [:accepted :accepted] state) (= :accepted (:state operation)))
      (refuse! :inconsistent-terminal-state))
    (let [proof (filing-proof! db (:actor operation) room-id proposal-id rows)]
      (when-not (= (:binding proof) (select-keys operation (keys (:binding proof))))
        (refuse! :journal-binding-changed))
      (when (= :accepted (:state operation))
        (terminal-facts! db proof operation (:updated-at operation)))
      {:proof proof :operation operation})))

(defn- immutable-proof [proof]
  (-> proof
      (update :proposal dissoc :proposal/status)
      (update :fork dissoc :proposal.fork/status)))

(defn- same-operation! [before after]
  (let [immutable #(dissoc % :state :updated-at :commit :head :noop? :reconciled-by)
        forward {:claimed #{:claimed :applying :accepted}
                 :applying #{:applying :accepted} :accepted #{:accepted}}]
    (when-not (and (= (immutable before) (immutable after))
                   (contains? (get forward (:state before)) (:state after))
                   (or (= before after)
                       (and (not= (:state before) (:state after))
                            (not (.before ^java.util.Date (:updated-at after) (:updated-at before))))))
      (refuse! :recovery-operation-changed))))

(defn- recovery-binding!
  [db party room-id room operation original]
  ;; Strengthened resolve! calls the exact native workspace-for-system! guard,
  ;; including all-type connection aliases and coherent canonical config.
  (let [current (owned-workspace/resolve! db party :merge room-id room)]
    (when-not (and (map? (:workspace-config current))
                   (= (dissoc (:target operation) :ref) (dissoc (:target current) :ref))
                   (or (nil? original)
                       (and (identical? (:room original) (:room current))
                            (identical? (:system original) (:system current))
                            (identical? (:conn original) (:conn current))
                            (= (:workspace-config original) (:workspace-config current)))))
      (refuse! :target-changed))
    current))

(defn- receipt-outcome [receipt]
  (into {} (for [key [:commit :head :noop?]
                 :let [attr (keyword "publication" (name key))]
                 :when (contains? receipt attr)]
             [key (get receipt attr)])))

(defn- recovery-tx
  [db party room-id room observation binding receipt at batch]
  (let [{:keys [proof operation]} (reconciliation-snapshot db party room-id (get-in observation [:operation :proposal]))]
    (same-operation! (:operation observation) operation)
    (when-not (= (immutable-proof (:proof observation)) (immutable-proof proof))
      (refuse! :preflight-changed))
    (recovery-binding! db party room-id room operation binding)
    (native-publication/validate-receipt! operation (get-in proof [:retained :manifest]) receipt)
    (case (:state operation)
      :claimed (refuse! :receipt-without-dispatch-admission)
      :accepted (if batch (processing/acceptance-tx db (:expected batch) (:value batch) operation) [])
      :applying
      (let [outcome (assoc (receipt-outcome receipt) :reconciled-by party)
            accepted (adoptions/validate! (merge operation outcome {:state :accepted :updated-at at}))]
        (into (adoptions/transition-tx-data db (get-in proof [:fork :db/id]) operation :accepted outcome at)
              (concat [{:db/id (get-in proof [:fork :db/id]) :proposal.fork/status :accepted
                        :proposal/resolved-by (:actor operation) :proposal/resolved-at at}
                       {:db/id (get-in proof [:proposal :db/id]) :proposal/status :accepted
                        :proposal/resolved-by (:actor operation) :proposal/resolved-at at}]
                      (when batch
                        (processing/acceptance-tx db (:expected batch) (:value batch) accepted))))))))

(defn recover!
  "INTERNAL CURRENT-authorized positive receipt observation, not dispatch or a
   replacement for the original publisher's ACK. Derive the operation from
   canonical control records; never accept a receipt/token/descriptor as input.
   Current owned/native target must match every original target field EXCEPT
   logical ref. Look up ORIGINAL ref/intent in one immutable target DB. No
   schema installation, bodies, worktree/HEAD reads, target write or task replay.

   Positive applying -> joint acceptance records ORIGINAL actor plus separate
   current reconciled-by. Accepted positive repeat preserves all audit facts.
   Claimed plus positive is inconsistent; absence is ONLY an observation, never
   permission to take ownership, abort, retry publication or release a claim."
  [conn party room-id room proposal-id]
  (host-connection! conn)
  (let [db @conn
        {:keys [proof operation] :as observation} (reconciliation-snapshot db party room-id proposal-id)
        binding (recovery-binding! db party room-id room operation nil)
        target-db (:db binding)
        identity (native-publication/identity operation (get-in proof [:retained :manifest]))
        ;; A pristine repository may have no receipt schema yet. Do not install
        ;; it or pull an unknown attribute merely to establish absence.
        receipt (when (seq (d/datoms target-db :avet :db/ident :publication/id))
                  (scoped-publication/recover target-db identity))]
    (if-not receipt
      (if (= :accepted (:state operation))
        (refuse! :accepted-receipt-unavailable)
        {:status :pending :proposal proposal-id :operation-id (:id operation)
         :observed-state (:state operation) :reason :receipt-not-observed})
      (do
        (native-publication/validate-receipt! operation (get-in proof [:retained :manifest]) receipt)
        (let [at (java.util.Date.)
              batch (batch-resolution/retained! conn (:disposition proof))]
          (try
            (let [report (d/transact conn [[:db.fn/call
                                            (fn [writer-db]
                                              (recovery-tx writer-db party room-id room observation binding receipt at batch))]])]
              (when-not (and (:db-before report) (:db-after report)) (refuse! :recovery-report-unavailable))
              (let [before (reconciliation-snapshot (:db-before report) party room-id proposal-id)
                    _ (same-operation! operation (:operation before))
                    _ (when-not (= (immutable-proof proof) (immutable-proof (:proof before)))
                        (refuse! :recovery-report-mismatch))
                    previous (:operation before)
                    expected (case (:state previous)
                               :accepted previous
                               :applying (adoptions/validate!
                                          (merge previous (receipt-outcome receipt)
                                                 {:state :accepted :updated-at at :reconciled-by party}))
                               (refuse! :recovery-report-mismatch))
                    _ (adoptions/accepted-operation (:db-after report) (get-in proof [:fork :db/id]))]
                (finalized-report! (:db-after report) proof expected (:updated-at expected) batch)
                {:status :accepted :proposal proposal-id :operation-id (:id expected)
                 :actor (:actor expected) :already-accepted? (= :accepted (:state previous))
                 :recovered? true :outcome (select-keys expected [:commit :head :noop? :reconciled-by])}))
            (catch Throwable error
              (when (instance? InterruptedException error) (.interrupt (Thread/currentThread)))
              (throw (ex-info "File adoption recovery was not acknowledged; durable outcome may already be accepted"
                              {:type ::recovery-unacknowledged :operation (:id operation)} error)))))))))
