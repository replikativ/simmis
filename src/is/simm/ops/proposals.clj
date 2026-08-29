(ns is.simm.ops.proposals
  "Proposal (ForkSet) lifecycle — doc/proposals-and-time-travel.md §6.

   A proposal is the only first-class JOINT multi-system state: a named
   {scope → branch} map + metadata in the SYSTEM DB. Accept merges each
   fork into its trunk (yggdrasil 3-way — retractions propagate since
   yggdrasil 829c108) after surfacing conflict warnings; dismiss discards
   the branches. Accept-time is the future site of a yggdrasil workspace
   JOINT commit (a writer-consistent 'solid detent' on the global
   timeline)."
  (:require [is.simm.model.system-db :as sdb]
            [is.simm.model.access :as access]
            [is.simm.model.forkset :as fs]
            [is.simm.runtimes.branching :as branching]
            [is.simm.runtimes.context :as ctx]
            [clojure.string :as str]
            [datahike.api :as d]
            [taoensso.telemere :as log]))

(defn- conn [] (sdb/get-conn))

(defn file-proposal!
  "Record an open proposal. `forks` = [{:scope uuid :branch str
   :base-commit str :system-type kw :author uuid} …]. Returns the proposal id.

   `:author` on a FORK is the party who wrote it, which on a campaign is not
   `author` (the filer) — see `:proposal.fork/author`.

   `intent` (default `:change`) says what KIND of proposed future this is —
   see `is.simm.model.forkset`. It decides which view the row appears in, so a
   caller filing a budget rather than a patch must say so; nothing about the
   fork itself distinguishes them."
  [{:keys [id title summary author room run adoption forks intent]}]
  {:pre [(string? title) (seq forks)
         (or (nil? intent) (contains? fs/intents intent))]}
  (let [id (or id (random-uuid))]
    (d/transact (conn)
      [(cond-> {:proposal/id id
                :proposal/title title
                :proposal/status :open
                :proposal/created-at (java.util.Date.)
                :proposal/forks (mapv (fn [{:keys [scope authority-scope branch
                                                   base-commit system-type
                                                   world-system-id settlement-id
                                                   settlement-state descriptor]
                                            fork-author :author}]
                                        (cond-> {:proposal.fork/scope scope
                                                 :proposal.fork/branch (name branch)}
                                          base-commit (assoc :proposal.fork/base-commit (str base-commit))
                                          authority-scope (assoc :proposal.fork/authority-scope
                                                                 authority-scope)
                                          system-type (assoc :proposal.fork/system-type system-type)
                                          world-system-id (assoc :proposal.fork/world-system-id
                                                                 (str world-system-id))
                                          settlement-id (assoc :proposal.fork/settlement-id settlement-id)
                                          settlement-state (assoc :proposal.fork/settlement-state
                                                                  settlement-state)
                                          descriptor (assoc :proposal.fork/descriptor (pr-str descriptor))
                                          fork-author (assoc :proposal.fork/author fork-author)))
                                      forks)}
         summary (assoc :proposal/summary summary)
         author  (assoc :proposal/author author)
         room    (assoc :proposal/room room)
         run     (assoc :proposal/run run)
         adoption (assoc :proposal/adoption [:world-adoption/id adoption])
         ;; only when non-default: absent already MEANS :change, and writing
         ;; the default would make old and new rows differ for no reason
         (and intent (not= fs/default-intent intent))
         (assoc :proposal/intent intent))])
    (log/log! {:level :info :id ::filed :data {:proposal id :title title
                                               :forks (count forks)}})
    id))

(defn get-proposal [id]
  (when-let [e (d/q '[:find ?e . :in $ ?id :where [?e :proposal/id ?id]]
                    @(conn) id)]
    (d/pull @(conn) '[* {:proposal/forks [*]}] e)))

(defn list-proposals
  "Proposals, newest first. `status` filters (nil = all)."
  [& {:keys [status]}]
  (->> (d/q '[:find [(pull ?e [* {:proposal/forks [*]}]) ...]
              :where [?e :proposal/id _]] @(conn))
       (filter #(or (nil? status) (= status (:proposal/status %))))
       (sort-by :proposal/created-at #(compare %2 %1))
       vec))

(defn- proposal-visible?
  "May `party` see proposal `p` at all? Extracted so `visible-proposals` and
   `visible-status` cannot drift — this predicate has already been duplicated
   once (see `visible-proposals`) and a second copy is how the list and the
   single-id lookup would start disagreeing about who may see what."
  [party p]
  (or (nil? (:proposal/room p))
      (access/can? party :read {:room (:proposal/room p)})))

(defn visible-proposals
  "Proposals `party` may see — THE one place that decides.

   `list-proposals` is unfiltered by design (callers that already hold a
   principal-free context need it), and that is exactly how every authenticated
   party came to receive every proposal in the workspace: titles, the LLM-written
   `:summary` describing someone else's change, the author, and each fork's
   `:scope` — the store uuid of a KB they cannot read. Measured on this
   workspace: 10 proposals sent, 2 authorized.

   A room-LESS proposal is visible to everyone here. That is the historical
   behaviour and it is preserved deliberately rather than tightened blind: the
   same rows are already unactionable, because `access/proposal-room-eid`
   resolves nil for them and every write RPC therefore denies. Making them
   author-only is the right end state and belongs with a decision about whether
   `:proposal/room` should be required at all — see doc/archive/navigation-redesign-plan.md.

   The two aggregates each carried their own copy of this predicate; they now
   call this."
  [party & {:keys [status]}]
  (->> (list-proposals :status status)
       (filterv #(proposal-visible? party %))))

(defn visible-status
  "What `party` may be TOLD about proposal `id`: `:open` | `:accepted` |
   `:dismissed` when they may see it, and `nil` otherwise.

   `nil` deliberately conflates \"no such proposal\" with \"not yours to see\",
   and callers must not un-conflate it. The client only learns that a proposal
   is absent from its world — never that one exists which it may not have.

   This exists because ABSENCE HAS TWO CAUSES and the view was asserting the
   wrong one. `list-proposals!` is filtered by `visible-proposals`, so an id
   missing from the \"open\" list means either resolved OR invisible; the card
   said \"it has been accepted or dismissed\", which for a shared link is both
   false and a disclosure that someone else's proposal exists."
  [party id]
  (let [p (get-proposal id)]
    (when (and p (proposal-visible? party p))
      (or (:proposal/status p) :open))))

(defn with-merge-authority
  "Annotate every fork with `:proposal.fork/may-merge?` — whether `party` holds
   `:merge` on THAT fork's own scope.

   This is the read-side twin of `authorize-forks!`, and it exists so the card
   can say which patches a reviewer may land BEFORE they press a button that
   would refuse. Since B1, `:merge` is a distinct verb and membership no longer
   implies it, so on real data the answer genuinely varies per fork: a reviewer
   can hold write on a KB shared into their room and still not be allowed to
   land anything onto its trunk.

   ONE db snapshot for the whole pass, for the same reason `authorize-forks!`
   takes one: a card that showed patch A landable and patch B not, decided
   against two different states of the grant table, would be reporting a
   situation that never existed.

   It is an AFFORDANCE, not the gate. `authorize-forks!` still re-checks at
   decision time — this only decides what to draw.

   Takes the db explicitly for the same reason `can?` does (B0): the decision is
   a pure function of a system-DB value, and keeping the ambient read out of it
   is what makes it testable without standing up a server."
  ([party proposals]
   (with-merge-authority (some-> (sdb/get-conn) deref) party proposals))
  ([db party proposals]
   (mapv (fn [p]
           (if-let [forks (seq (:proposal/forks p))]
             (assoc p :proposal/forks
                    (mapv (fn [f]
                            (assoc f :proposal.fork/may-merge?
                                   (boolean
                                    (and party
                                         (access/can? db party :merge
                                                      (or (:proposal.fork/authority-scope f)
                                                          (:proposal.fork/scope f)))))))
                          forks))
             p))
         proposals)))

(defn with-capability-availability
  "Annotate adopted-world components with process-local capability availability.
   Ordinary branch-backed forks are durable/reopenable and remain unannotated."
  [proposals]
  (let [live? (requiring-resolve 'is.simm.ops.run-world-proposals/live-scope?)]
    (mapv (fn [p]
            (update p :proposal/forks
                    (fn [forks]
                      (mapv (fn [f]
                              (if (= :world (:proposal.fork/system-type f))
                                (assoc f :proposal.fork/capability-live?
                                       (live? (:proposal.fork/scope f)))
                                f))
                            forks))))
          proposals)))

(defn- fork-branch-kw [fork] (keyword (:proposal.fork/branch fork)))

(defn fork-open?
  "True while a fork has not been individually resolved. Absence is the open
   state (see `:proposal.fork/status`), so this is the only place that knows it."
  [fork]
  (nil? (:proposal.fork/status fork)))

(defn open-forks
  "The forks still in play. EVERY branch-touching path goes through this rather
   than `:proposal/forks`: a dismissed fork's branch has been deleted, so
   `branch-head-id`, `kb-conflicts` and the semantic diff all fail on it. The
   row survives to be shown as a refusal; the branch behind it does not."
  [p]
  (filterv fork-open? (:proposal/forks p)))

(defn find-fork
  "The fork row addressed by `scope` + `branch` — the pair is unique within a
   proposal, and it is what the client already holds from the diff, so no
   positional index has to stay in step between the two."
  [p scope branch]
  (first (filter #(and (= scope (:proposal.fork/scope %))
                       (= (name branch) (:proposal.fork/branch %)))
                 (:proposal/forks p))))

;; ---------------------------------------------------------------------------
;; Which backend settles a fork.
;;
;; A fork's `:proposal.fork/system-type` names it, and `branching`'s neutral API
;; makes the choice ONCE — this file never asks which backend it is holding.
;; That is deliberate: the failure mode of forgetting a dispatch is silent (a
;; repo scope asked a KB verb answers nil, and nil is also what "no such branch"
;; looks like, so a cache key collapses and a tier reads an empty delta as
;; trivial), so the fewer places that can forget, the better.
;; ---------------------------------------------------------------------------

(defn- fork-type [fork] (:proposal.fork/system-type fork))

(defn- fork-head-id [fork branch]
  (branching/head-id (:proposal.fork/scope fork) (fork-type fork) branch))

(defn- fork-trunk [fork]
  (branching/trunk-of (:proposal.fork/scope fork) (fork-type fork)))

(defn- fork-delta
  "The fork's change set against its trunk, fed to dvergr's classifier — which
   reads only the summary counts, so `:patch? false` keeps tiering from reading
   every changed blob on both sides of a code fork."
  [fork]
  (branching/delta (:proposal.fork/scope fork) (fork-type fork)
                   (fork-branch-kw fork) {:patch? false}))

(defn- fork-conflict-seq [fork]
  (branching/conflicts-with-trunk (:proposal.fork/scope fork) (fork-type fork)
                                  (fork-branch-kw fork)))

(defn fork-heads
  "The current head commit of each fork branch, plus of each fork's TRUNK.

   Both halves are the cache key for anything derived from a comparison between
   them. Trunk belongs in the key because mergeability is not a property of the
   branch alone: trunk advancing can turn a mergeable ForkSet into a conflicted
   one, and a key that ignored it would keep serving `:reviewable` for something
   that no longer merges.

   Dismissed forks drop out here, which re-keys every cache built on this: a
   refusal changes what Accept will do, and the conflict it contributed must
   stop being reported the moment its branch is gone."
  [p]
  (mapv (fn [f]
          [(fork-head-id f (fork-branch-kw f))
           (fork-head-id f (fork-trunk f))])
        (open-forks p)))

(defonce ^:private review-cache
  ;; {[proposal-id fork+trunk-heads] → review}. `conflicts` costs ~4.7 s on a
  ;; 6 000-datom KB (measured 2026-07-26: common-ancestor 1.5 s, the 3-way
  ;; compare the rest) against ~0.2 s for the diff beside it — so one card was
  ;; seconds of server time and a list of N was N × that.
  ;;
  ;; Keyed on heads rather than time, so this is exact rather than stale: any
  ;; move on either side changes the key. Nothing is invalidated by hand.
  (atom {}))

(defonce ^:private summary-cache
  ;; {[proposal-id fork-head-ids] → summary}. Keyed on the branch HEADS, so a
  ;; branch that advances misses the cache rather than serving prose that
  ;; describes a state the reviewer is no longer looking at. Process-local: a
  ;; restart re-earns it, which costs one LLM call per proposal actually opened.
  (atom {}))

(defn proposal-summary
  "Hierarchical summary of a proposal's diff, cached by the ForkSet's head
   commits. Returns nil when summarization fails — the raw diff is always the
   source of truth and renders regardless."
  [p diffs]
  (ctx/with-server-context
    (let [heads (mapv (fn [f] (fork-head-id f (fork-branch-kw f))) (open-forks p))
          k [(:proposal/id p) heads]]
      (if (contains? @summary-cache k)
        (get @summary-cache k)
        (let [s ((requiring-resolve 'is.simm.ops.diff-summary/summarize-diff)
                 {:title (:proposal/title p)
                  :rationale (:proposal/summary p)
                  :diffs diffs})]
          ;; cache negatives too — a proposal whose summary fails should not
          ;; re-attempt (and re-bill) an LLM call on every card render
          (swap! summary-cache assoc k s)
          s)))))

(defn- compute-proposal-review
  "The uncached body of `proposal-review`."
  [p]
  (ctx/with-server-context
    (let [classify (requiring-resolve 'dvergr.rooms.forks/classify)
          entries (doall
                   (for [f (open-forks p)
                         :let [scope (:proposal.fork/scope f)
                               b (fork-branch-kw f)]]
                     {:system (str scope)
                      :delta (fork-delta f)
                      :conflicts (mapv #(assoc % :scope (str scope) :branch (name b))
                                       (fork-conflict-seq f))}))
          conflicts (vec (mapcat :conflicts entries))]
      {:tier (classify (into {} (map (juxt :system :delta)) entries) conflicts)
       :conflicts conflicts})))

(defn proposal-review
  "Tier + conflicts for a proposal, classified by DVERGR's fork classifier so
   simmis, the dvergr TUI and any agent reviewer agree on what \"trivial\"
   means rather than each inventing a threshold.

   `dvergr.rooms.forks/classify` already takes a plain {system → delta} map
   and a conflicts seq, so no dvergr change is needed to share it — only the
   Room-shaped wrappers around it are unusable here.

   Returns {:tier :trivial|:reviewable|:conflict :conflicts [...]}. Computing
   conflicts here means the reviewer sees a conflict BEFORE pressing Accept,
   instead of discovering it in the response."
  [p]
  (ctx/with-server-context
    (let [k [(:proposal/id p) (fork-heads p)]]
      (if (contains? @review-cache k)
        (get @review-cache k)
        (let [v (compute-proposal-review p)]
          (swap! review-cache assoc k v)
          v)))))


(defn- resolution-facts
  "The `who`/`why` half of a resolution, omitted when not supplied. Recorded
   at decision time because it cannot be reconstructed later — see
   `:proposal/resolution-note`."
  [note by]
  (cond-> {}
    (not (str/blank? note)) (assoc :proposal/resolution-note (str/trim note))
    by (assoc :proposal/resolved-by by)))

(defn- fork-resolution-tx
  "The datoms that settle ONE fork. The `:proposal/resolution-*` attributes ride
   on the fork entity too — see `:proposal.fork/status` for why there is no
   parallel `:proposal.fork/resolution-*` pair."
  [fork status note by]
  (merge {:db/id (:db/id fork)
          :proposal.fork/status status
          :proposal/resolved-at (java.util.Date.)}
         (resolution-facts note by)))

(defn- discard-fork-branch!
  "Delete a fork's branch, surviving a failure. One undiscardable fork must not
   strand the rest of the ForkSet, but a swallowed failure leaks the branch and
   its snapshots with nothing left pointing at them — so it is logged loudly."
  [proposal-id fork]
  (let [scope (:proposal.fork/scope fork)
        b (fork-branch-kw fork)]
    ;; Adopted worlds are affine capabilities with their own durable settlement
    ;; frontier. Swallowing that failure and marking the row dismissed would be
    ;; a false audit record, so this backend must fail the decision visibly.
    (if (= :world (fork-type fork))
      (branching/drop-branch! scope (fork-type fork) b)
      (try (branching/drop-branch! scope (fork-type fork) b)
           (catch Exception e
             (log/log! {:level :warn :id ::fork-discard-failed
                        :msg "Fork branch not discarded — it is now orphaned"
                        :data {:proposal proposal-id
                               :scope (str scope)
                               :branch b
                               :system-type (:proposal.fork/system-type fork)
                               :error (.getMessage e)}}))))))

(defn- fork-conflicts
  "`fork-conflict-seq` tagged with which fork each conflict came from — a
   reviewer of a four-fork ForkSet needs to know WHICH contribution conflicts,
   since three of them may be landable as they stand."
  [fork]
  (let [scope (:proposal.fork/scope fork)
        b (fork-branch-kw fork)]
    (for [c (fork-conflict-seq fork)]
      (assoc c :scope scope :branch b))))

(defn- land-fork!
  "Merge one fork into its trunk and drop the now-redundant branch."
  [fork]
  (let [scope (:proposal.fork/scope fork)
        t (fork-type fork)
        b (fork-branch-kw fork)]
    ;; A code fork whose only clash is that it TOUCHED the same file trunk did
    ;; just lands: geschichte reconciles those while planning the merge, so
    ;; nothing has to be resolved here first. simmis used to do that pass
    ;; itself, before the content merge went upstream (geschichte 0.1.14).
    (branching/land! scope t b)
    (branching/drop-branch! scope t b)))

(defn- authorize-forks!
  "Landing puts a fork onto TRUNK, so each fork's scope must permit `:merge` —
   not `:write`. Writing a fork is the generous half of the fork/review model and
   every contributor has it; deciding that a fork becomes the trunk is the
   irreversible half and is granted per scope, exactly as Forgejo's
   branch-protection merge whitelist is separate from write access to the repo.

   Two layers, and both are needed. The RPC policy gates these calls on
   `{:proposal id}`, which resolves to the proposal's ROOM — that decides who may
   SEE and act on a proposal at all. This function decides who may LAND each
   patch, per scope. Without it a room member holding a mere `:read-write` grant
   on a KB could merge a fork into that KB's trunk on the strength of room
   membership alone.

   ONE db value for the whole decision. The rule is `∀ patch ∈ selected :
   can? by :merge patch.scope` — a single quantified question, so it must be
   asked of a single snapshot. Reading the ambient connection per fork would let
   a grant revoked mid-loop authorize the forks checked before it and refuse the
   ones after, landing part of a ForkSet under authority that no longer holds."
  [proposal-id by forks]
  ;; A nil `by` SKIPS this check, and that is deliberate rather than a hole —
  ;; but it used to be silent, which is what made it look like one.
  ;;
  ;; This is not the authentication boundary. `access/authorize-remote` is:
  ;; deny-by-default over `rpc-policy`, consulted for every network-inbound
  ;; call before its handler runs. Both production callers
  ;; (`proposals_remote/accept-proposal!` and `/accept-fork!`) resolve `:by`
  ;; from `authenticated-party-id`, so a nil here means an INTERNAL caller with
  ;; no principal — a REPL session, or a test exercising merge mechanics on
  ;; bare scopes that have no party, room or grant to authorize against.
  ;;
  ;; Refusing outright was tried and is wrong: it fails the fork-merge tests,
  ;; which are about whether a branch lands correctly and would have to grow a
  ;; whole authorization fixture to say so. Logging it means a future
  ;; production caller that forgets `:by` leaves a trace instead of merging
  ;; quietly.
  (when-not by
    (log/log! {:level :warn :id ::merge-authorization-skipped
               :data {:proposal proposal-id :forks (count forks)}}
              "landing forks with no acting party — authorization not checked"))
  (when by
   (let [db (some-> (sdb/get-conn) deref)
        refused (vec (for [f forks
                           :let [scope (or (:proposal.fork/authority-scope f)
                                           (:proposal.fork/scope f))]
                           :when (not (access/can? db by :merge scope))]
                       (str scope)))]
    (when (seq refused)
      (throw (ex-info "not authorized to write every fork's trunk"
                      {:error :not-authorized :proposal proposal-id
                       :scopes refused}))))))

(defn- settle-proposal!
  "Flip the proposal once no fork is left open, and report what it became.

   `:dismissed` when every fork was refused: a ForkSet whose every contribution
   was turned down is not an acceptance of nothing, and the Feed reads this
   status as the decision that was made."
  [id note by]
  (let [forks (:proposal/forks (get-proposal id))]
    (when (not-any? fork-open? forks)
      (let [status (if (some #(= :accepted (:proposal.fork/status %)) forks)
                     :accepted :dismissed)]
        (d/transact (conn) [(merge {:proposal/id id
                                    :proposal/status status
                                    :proposal/resolved-at (java.util.Date.)}
                                   (resolution-facts note by))])
        ;; Ordinary ForkSets have no live adoption and this is a no-op. An
        ;; adopted Run world releases its structural Dvergr ancestry only after
        ;; every per-component terminal record is durable.
        ((requiring-resolve
          'is.simm.ops.run-world-proposals/release-proposal!) id)
        status))))

(defn reconcile-status!
  "Close a Proposal whose component statuses are already durably terminal.
   Used by recovery backends after replaying an ambiguous terminal commit."
  [id]
  (settle-proposal! id nil nil))

(defn- open-proposal!
  "The proposal `id` names, refusing anything already resolved."
  [id]
  (let [p (get-proposal id)]
    (when-not p (throw (ex-info "unknown proposal" {:id id})))
    (when-not (= :open (:proposal/status p))
      (throw (ex-info "proposal not open" {:id id :status (:proposal/status p)})))
    p))

(defn- open-fork!
  "The still-open fork `scope`+`branch` names within `p`."
  [p scope branch]
  (let [f (find-fork p scope branch)]
    (when-not f
      (throw (ex-info "unknown fork" {:error :unknown-fork
                                      :proposal (:proposal/id p)
                                      :scope (str scope) :branch (str branch)})))
    (when-not (fork-open? f)
      (throw (ex-info "fork already resolved"
                      {:error :fork-resolved :proposal (:proposal/id p)
                       :status (:proposal.fork/status f)})))
    f))

;; ---------------------------------------------------------------------------
;; The review conversation
;; ---------------------------------------------------------------------------

(defn add-comment!
  "Record one comment on an open proposal. Returns the comment id.

   `fork-branch` scopes it to one contribution; absent means the ForkSet as a
   whole. `kind` is `:comment`, or `:changes-requested` when the reviewer is
   asking for a revision rather than remarking — see `request-changes!`, which
   is the only thing that should pass it, because the flag is what tells a later
   reader that this proposal went round twice."
  [id {:keys [body author fork-branch kind]}]
  {:pre [(string? body)]}
  (when (str/blank? body)
    (throw (ex-info "a comment needs something in it" {:error :empty-comment})))
  (let [p (get-proposal id)
        cid (random-uuid)]
    (when-not p (throw (ex-info "unknown proposal" {:id id})))
    (d/transact (conn)
      [{:proposal/id id
        :proposal/comments
        [(cond-> {:proposal.comment/id cid
                  :proposal.comment/body (str/trim body)
                  :proposal.comment/at (java.util.Date.)
                  :proposal.comment/kind (or kind :comment)}
           author (assoc :proposal.comment/author author)
           fork-branch (assoc :proposal.comment/fork-branch (name fork-branch)))]}])
    (log/log! {:level :info :id ::commented
               :data {:proposal id :kind (or kind :comment)
                      :fork (some-> fork-branch name)}})
    cid))

(defn request-changes!
  "Ask the contributor to revise, instead of accepting or refusing.

   The third verb a review needs and the one that was missing: without it a
   reviewer holding a nearly-right change had to either take it as it stood or
   throw it away, and for an agent-authored change both are wasteful — the agent
   can revise, and asking costs less than either.

   Records the comment and REOPENS the fork's author to write onto the branch
   the fork already has. Nothing is merged, nothing is discarded, and the
   proposal stays open: this is a request, not a decision.

   Returns `{:comment id :reopened [{:scope :branch :author}…]}`. The reopened
   list can be empty — the author may be a human, or an agent this process no
   longer holds an overlay for — and that is not a failure. The comment is the
   durable part; reopening is a convenience for a live agent."
  [id {:keys [body by fork-branch]}]
  (let [p (open-proposal! id)
        forks (cond->> (open-forks p)
                fork-branch (filter #(= (name fork-branch)
                                        (:proposal.fork/branch %))))]
    (when (and fork-branch (empty? forks))
      (throw (ex-info "unknown fork" {:error :unknown-fork :proposal id
                                      :branch (name fork-branch)})))
    (let [cid (add-comment! id {:body body :author by :fork-branch fork-branch
                                :kind :changes-requested})
          reopen! (requiring-resolve 'is.simm.agents.room-agents/reopen-fork!)
          ;; `reopen-fork!` can decline: the author may have opened a proposal
          ;; of its own since filing, and its overlay is a single slot. Both
          ;; halves are returned — a request for changes that reached nobody
          ;; is a comment on a filed proposal and nothing more, and the
          ;; reviewer has to be told which it was.
          outcome (group-by
                   (fn [f]
                     (if (and (:proposal.fork/author f)
                              (reopen! (:proposal/room p)
                                       (:proposal.fork/author f)
                                       (:proposal.fork/scope f)
                                       (fork-branch-kw f)
                                       (:proposal.fork/system-type f)
                                       (:proposal/title p)))
                       :reopened :not-reopened))
                   forks)
          describe (fn [f] {:scope (:proposal.fork/scope f)
                            :branch (:proposal.fork/branch f)
                            :author (:proposal.fork/author f)})
          reopened (mapv describe (:reopened outcome))
          not-reopened (mapv describe (:not-reopened outcome))]
      (log/log! {:level (if (seq not-reopened) :warn :info)
                 :id ::changes-requested
                 :data {:proposal id :forks (count forks)
                        :reopened (count reopened)
                        :not-reopened (count not-reopened)}})
      {:comment cid :reopened reopened :not-reopened not-reopened})))

(defn accept-proposal!
  "Merge every STILL-OPEN fork into its trunk. First call returns
   {:warnings […]} when conflicts exist and `force?` is not set —
   callers surface them and retry with :force? true. On merge: discard
   branches, flip status. Returns {:status :accepted|:dismissed} or {:warnings …}.

   Forks already dismissed individually are skipped in both the conflict
   pre-check and the merge loop: their branches no longer exist, and a conflict
   reported by a contribution the reviewer already refused would block the three
   they are trying to land. When every fork was dismissed there is nothing left
   to merge, and `settle-proposal!` records that as a dismissal rather than an
   acceptance of nothing.

   `:note` / `:by` record who decided and why (both optional)."
  [id & {:keys [force? note by]}]
  (let [p (open-proposal! id)
        forks (open-forks p)]
    (authorize-forks! id by forks)
    (ctx/with-server-context
      (let [warnings (vec (mapcat fork-conflicts forks))]
        (if (and (seq warnings) (not force?))
          {:warnings warnings}
          ;; ONE FORK AT A TIME, each recording its own outcome before the next
          ;; is attempted. Two things forced this shape, and both are only
          ;; reachable on a backend whose merge can actually fail — repos, since
          ;; the datahike adapter reports no conflicts and never refuses:
          ;;
          ;; 1. The pre-check above compares every fork against TRUNK, and two
          ;;    forks off one base each merge into trunk cleanly. They conflict
          ;;    only with EACH OTHER, which becomes visible the moment the first
          ;;    lands. So a mutually-conflicting ForkSet passes the gate with no
          ;;    warnings at all — the gate cannot be made to catch this, because
          ;;    before anything lands there is nothing to catch.
          ;;
          ;; 2. Recording every status in one transaction AFTER the loop meant a
          ;;    throw on the second fork rolled back the record of the first —
          ;;    which had already merged and had its branch deleted. The
          ;;    proposal was then permanently wedged: both rows still `:open`,
          ;;    and every retry threw "Unknown Geschichte merge source" on a
          ;;    branch that no longer exists.
          ;;
          ;; Landing is not atomic across forks and cannot be (each merge is its
          ;; own commit on its own trunk), so the RECORD must track it fork by
          ;; fork. A partial landing is then a truthful state a reviewer can act
          ;; on — the landed forks read `:accepted`, the rest stay open — rather
          ;; than a lie that cannot be retried.
          (let [outcomes
                (doall
                 (for [f forks]
                   (try
                     (land-fork! f)
                     (d/transact (conn) [(fork-resolution-tx f :accepted note by)])
                     {:fork f :landed? true}
                     (catch Exception e
                       (log/log! {:level :warn :id ::fork-land-failed
                                  :msg "Fork did not land — it stays open and retryable"
                                  :data {:proposal id
                                         :scope (str (:proposal.fork/scope f))
                                         :branch (fork-branch-kw f)
                                         :error (.getMessage e)}})
                       {:fork f :landed? false :error (.getMessage e)}))))
                landed (filterv :landed? outcomes)
                failed (filterv (complement :landed?) outcomes)
                status (settle-proposal! id note by)]
            (log/log! {:level :info :id ::accepted
                       :data {:proposal id :forced (boolean force?)
                              :overridden-warnings (count warnings)
                              :landed (count landed) :failed (count failed)
                              :status status
                              :noted? (not (str/blank? note))}})
            (cond-> {:status status}
              ;; surfaced, not swallowed: the reviewer pressed Accept on N forks
              ;; and fewer landed. Naming which ones and why is the difference
              ;; between a retryable state and a silent partial merge.
              (seq failed)
              (assoc :failed (mapv (fn [{:keys [fork error]}]
                                     {:scope (:proposal.fork/scope fork)
                                      :branch (name (fork-branch-kw fork))
                                      :error error})
                                   failed)))))))))

(defn dismiss-proposal!
  "Discard every remaining fork branch; flip status. `:note` / `:by` record who
   decided and why — a REFUSAL and its reason is the more valuable training
   signal of the two, since an acceptance is often just silence.

   Uses `open-proposal!` like every other decision verb. It used to use
   `get-proposal`, so dismissing an already-ACCEPTED proposal succeeded: it
   flipped `:proposal/status` to `:dismissed` and overwrote
   `:proposal/resolved-at`, rewriting the record of a merge that had already
   landed on trunk."
  [id & {:keys [note by]}]
  (let [p (open-proposal! id)]
    (let [forks (open-forks p)]
      (ctx/with-server-context
        (doseq [f forks] (discard-fork-branch! id f)))
      (d/transact (conn) (conj (mapv #(fork-resolution-tx % :dismissed note by) forks)
                               (merge {:proposal/id id
                                       :proposal/status :dismissed
                                       :proposal/resolved-at (java.util.Date.)}
                                      (resolution-facts note by))))
      (log/log! {:level :info :id ::dismissed
                 :data {:proposal id :forks (count forks)
                        :noted? (not (str/blank? note))}})
      {:status :dismissed})))

;; ---------------------------------------------------------------------------
;; Per-fork resolution — one contribution at a time
;;
;; A proposal carrying four agents' work is refused wholesale today, so a single
;; unacceptable fork (a customer notice that admits legal liability) costs the
;; other three their landing. These two settle ONE fork and leave the ForkSet
;; open for the rest; when the last one is settled, `settle-proposal!` closes it.
;; ---------------------------------------------------------------------------

(defn dismiss-fork!
  "Refuse ONE fork: discard its branch, mark the fork dismissed, keep the note.
   The rest of the ForkSet stays open and lands normally.

   Gated exactly like `dismiss-proposal!` — the RPC's room check and nothing
   more. Discarding is not landing, so the fork-scope :write requirement that
   `accept-*` carries does not apply here: refusing a contribution to your own
   room's proposal must not require write access to a KB you were never granted.

   Returns {:status :dismissed :proposal <status-or-nil>}, the second being what
   the proposal became if this was the last open fork."
  [id scope branch & {:keys [note by]}]
  (let [p (open-proposal! id)
        f (open-fork! p scope branch)]
    (ctx/with-server-context (discard-fork-branch! id f))
    (d/transact (conn) [(fork-resolution-tx f :dismissed note by)])
    (let [status (settle-proposal! id note by)]
      (log/log! {:level :info :id ::fork-dismissed
                 :data {:proposal id :scope (str scope) :branch (str branch)
                        :proposal-status status
                        :noted? (not (str/blank? note))}})
      {:status :dismissed :proposal status})))

(defn accept-fork!
  "Land ONE fork on its own trunk, leaving the rest of the ForkSet open.

   Same two-step conflict handling as `accept-proposal!`, narrowed to this
   fork's branch: conflicts elsewhere in the ForkSet are somebody else's
   problem and must not block a contribution that merges cleanly.

   Returns {:status :accepted :proposal <status-or-nil>} or {:warnings …}."
  [id scope branch & {:keys [force? note by]}]
  (let [p (open-proposal! id)
        f (open-fork! p scope branch)]
    (authorize-forks! id by [f])
    (ctx/with-server-context
      (let [warnings (vec (fork-conflicts f))]
        (if (and (seq warnings) (not force?))
          {:warnings warnings}
          (do
            ;; RECORD THE LAND EVEN IF SOMETHING AFTER IT THROWS. `land-fork!`
            ;; merges onto trunk and then deletes the branch; once that has
            ;; happened the fork is landed whatever else fails. Leaving the row
            ;; `:open` is what wedges a proposal — `accept-proposal!`'s own
            ;; comment records the incident: a throw on the second fork rolled
            ;; back the record of the first, which had already merged and had
            ;; its branch deleted, and every retry then threw "Unknown
            ;; Geschichte merge source" on a branch that no longer existed. The
            ;; multi-fork path was hardened; this single-fork path was not.
            (land-fork! f)
            (try
              (d/transact (conn) [(fork-resolution-tx f :accepted note by)])
              (catch Throwable e
                (log/log! {:level :error :id ::fork-resolution-not-recorded
                           :error e
                           :data {:proposal id :scope (str scope) :branch (str branch)}}
                          "fork LANDED but its resolution was not recorded")
                (throw e)))
            (let [status (settle-proposal! id note by)]
              (log/log! {:level :info :id ::fork-accepted
                         :data {:proposal id :scope (str scope) :branch (str branch)
                                :forced (boolean force?)
                                :overridden-warnings (count warnings)
                                :proposal-status status
                                :noted? (not (str/blank? note))}})
              {:status :accepted :proposal status})))))))
