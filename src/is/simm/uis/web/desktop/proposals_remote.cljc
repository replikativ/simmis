(ns is.simm.uis.web.desktop.proposals-remote
  "Spin-remotes for the proposals inbox (S2). Thin pass-throughs over
   is.simm.ops.proposals + ops.semantic-diff; all UUIDs cross the wire as
   strings. Diffs are computed server-side (semantic ops only; full datoms
   never leave the server)."
  (:require [clojure.string :as str]
            [org.replikativ.spindel.distributed.macros :refer [defn-spin-remote]
             :include-macros true]
            [org.replikativ.spindel.distributed.core :as dist]
            [is.simm.model.forkset :as fs]
            #?(:clj [is.simm.ops.proposals :as props])
            #?(:clj [is.simm.ops.semantic-diff :as sdiff])
            #?(:clj [is.simm.ops.checks :as checks])
            #?(:clj [is.simm.runtimes.branching :as branching])
            #?(:clj [is.simm.model.access :as access])
            #?(:clj [is.simm.model.parties :as parties])
            #?(:clj [is.simm.runtimes.context :as ctx])))

#?(:clj
   (defn- fork-author-name
     "Display name for a fork's author, resolved server-side — the client has no
      party directory, and a raw uuid on a review card tells a reviewer nothing.
      nil for forks filed before `:proposal.fork/author` existed."
     [f]
     (when-let [a (:proposal.fork/author f)]
       (let [p (parties/get-party a)]
         (or (:party/display-name p) (:party/handle p) (str a))))))

#?(:clj
   (defn- comment-author-name
     "Display name for a comment's author, resolved server-side for the same
      reason a fork's is: the client has no party directory, and a raw uuid over
      a review remark tells the reader nothing about who is asking."
     [c]
     (when-let [a (:proposal.comment/author c)]
       (let [p (parties/get-party a)]
         (or (:party/display-name p) (:party/handle p) (str a))))))

(defn-spin-remote list-proposals!
  [server-id status-kw]
  (spin-remote server-id [status-kw]
    (let [st (identity status-kw)]
      #?(:clj (ctx/with-server-context
                ;; `visible-proposals`, not `list-proposals`: policed as
                ;; :authenticated, this endpoint was sending EVERY proposal in
                ;; the workspace to every party — titles, the LLM summary of
                ;; someone else's change, and each fork's KB store uuid.
                (->> (let [party (access/authenticated-party-id)]
                       ;; who may LAND each patch, resolved server-side — the
                       ;; client has no grant table and must not guess
                       (props/with-merge-authority
                         party
                         (props/visible-proposals party :status (when st (keyword st)))))
                     props/with-capability-availability
                     ;; stringify uuids for the wire; keep forks compact
                     (mapv (fn [p]
                             {:id (str (:proposal/id p))
                              :title (:proposal/title p)
                              :summary (:proposal/summary p)
                              :status (:proposal/status p)
                              ;; normalised here rather than left absent, so the
                              ;; client never has to know that a missing intent
                              ;; means :change
                              :intent (or (:proposal/intent p) fs/default-intent)
                              :created-at (:proposal/created-at p)
                              :author (some-> (:proposal/author p) str)
                              :room (some-> (:proposal/room p) str)
                              ;; oldest first — a conversation reads forward
                              :comments (->> (:proposal/comments p)
                                             (sort-by :proposal.comment/at)
                                             (mapv (fn [c]
                                                     {:id (str (:proposal.comment/id c))
                                                      :body (:proposal.comment/body c)
                                                      :at (:proposal.comment/at c)
                                                      :kind (or (:proposal.comment/kind c) :comment)
                                                      :fork-branch (:proposal.comment/fork-branch c)
                                                      :author-name (comment-author-name c)})))
                              :forks (mapv (fn [f]
                                             {:scope (str (:proposal.fork/scope f))
                                              :branch (:proposal.fork/branch f)
                                              :system-type (:proposal.fork/system-type f)
                                              :author-name (fork-author-name f)
                                              ;; may THIS reviewer land THIS
                                              ;; patch — see with-merge-authority
                                              :may-merge? (:proposal.fork/may-merge? f)
                                              :capability-live?
                                              (:proposal.fork/capability-live? f)
                                              ;; nil ⇒ open; the client shows a
                                              ;; resolved fork struck through
                                              ;; rather than dropping it
                                              :status (:proposal.fork/status f)})
                                           (:proposal/forks p))}))))
         :cljs nil))))

(defn-spin-remote proposal-status!
  [server-id proposal-id-str]
  (spin-remote server-id [proposal-id-str]
    (let [s (identity proposal-id-str)]
      #?(:clj (ctx/with-server-context
                ;; `:open` | `:accepted` | `:dismissed` when this party may see
                ;; it, `{:status nil}` otherwise — and nil means BOTH "no such
                ;; proposal" and "not yours to see", on purpose. The client is
                ;; told only that it is absent from its world, never that one
                ;; exists which it may not have.
                {:status (props/visible-status (access/authenticated-party-id)
                                               (java.util.UUID/fromString s))})
         :cljs nil))))

(defn-spin-remote proposal-diff!
  [server-id proposal-id-str]
  (spin-remote server-id [proposal-id-str]
    (let [s (identity proposal-id-str)]
      #?(:clj (ctx/with-server-context
                (when-let [p (props/get-proposal (java.util.UUID/fromString s))]
                  (let [open (props/open-forks p)
                        review (props/proposal-review p)
                        ;; diffed OPEN forks only: a dismissed fork's branch has
                        ;; been deleted, and `semantic-diff` reads it — so the
                        ;; whole card would fail on one refusal. The row is still
                        ;; sent below, carrying its status and no diff, because
                        ;; the refusal is the record; the content behind it is
                        ;; genuinely gone.
                        diffs (sdiff/proposal-diff (assoc p :proposal/forks open))
                        by-key (zipmap (map (juxt (comp str :proposal.fork/scope)
                                                  :proposal.fork/branch)
                                            open)
                                       diffs)]
                    ;; one entry per fork; uuids → strings for transit. Tier,
                    ;; conflicts and summary ride along so the card needs no
                    ;; second round trip to be fully useful.
                    {:tier (:tier review)
                     :summary (props/proposal-summary p diffs)
                     ;; Conflicts with the TEXT of each side, per code fork.
                     ;; The raw descriptors carry content ids, which tell a
                     ;; reviewer that something clashes and nothing about what —
                     ;; leaving refuse as the only informed move.
                     :conflicts (vec (mapcat
                                      (fn [f]
                                        (when (and (= :repo (:proposal.fork/system-type f))
                                                   (nil? (:proposal.fork/status f)))
                                          (map #(assoc % :branch (:proposal.fork/branch f)
                                                       :scope (str (:proposal.fork/scope f)))
                                               (branching/repo-conflict-details
                                                (:proposal.fork/scope f)
                                                (keyword (:proposal.fork/branch f))))))
                                      (:proposal/forks p)))
                     ;; datom-backed forks still report only counts (the
                     ;; datahike adapter reports no conflicts at all), so their
                     ;; descriptors ride separately rather than being dressed up
                     ;; as something a reviewer can read
                     :conflict-counts (count (:conflicts review))
                     ;; every fork, in filing order, each addressable by
                     ;; scope+branch — that pair is what the per-fork Accept /
                     ;; Dismiss endpoints take, so no positional index has to be
                     ;; kept in step between client and server
                     :forks (mapv (fn [f]
                                    (let [scope (str (:proposal.fork/scope f))
                                          branch (:proposal.fork/branch f)]
                                      (-> (get by-key [scope branch])
                                          (assoc :scope scope
                                                 :branch branch
                                                 :system-type (or (:proposal.fork/system-type f) :kb)
                                                 :author-name (fork-author-name f)
                                                 :status (:proposal.fork/status f))
                                          (update :pages
                                                  (fn [ps]
                                                    (mapv #(update % :page-uuid str) ps))))))
                                  (:proposal/forks p))})))
         :cljs nil))))

(defn-spin-remote accept-proposal!
  [server-id proposal-id-str force? note]
  (spin-remote server-id [proposal-id-str force? note]
    (let [s (identity proposal-id-str) f (identity force?) n (identity note)]
      ;; `by` comes from the JWT principal, never from the client — a decision
      ;; corpus is only worth training on if its attribution is trustworthy.
      #?(:clj (props/accept-proposal! (java.util.UUID/fromString s)
                                      :force? (boolean f)
                                      :note n
                                      :by (access/authenticated-party-id))
         :cljs nil))))

;; Test results per CODE fork. Its own round trip, not part of `proposal-diff!`,
;; because a suite takes as long as it takes and a card must render before it
;; finishes — the same reason the diff is not part of the list. (No docstring:
;; `defn-spin-remote` requires the args vector to follow the name.)
(defn-spin-remote proposal-checks!
  [server-id proposal-id-str]
  (spin-remote server-id [proposal-id-str]
    (let [s (identity proposal-id-str)]
      #?(:clj (ctx/with-server-context
                (when-let [p (props/get-proposal (java.util.UUID/fromString s))]
                  (vec (keep (fn [f]
                               (when (= :repo (:proposal.fork/system-type f))
                                 (some-> (checks/fork-checks
                                          (:proposal.fork/scope f)
                                          :repo
                                          (keyword (:proposal.fork/branch f)))
                                         (assoc :scope (str (:proposal.fork/scope f))
                                                :branch (:proposal.fork/branch f)))))
                             (props/open-forks p)))))
         :cljs nil))))

(defn-spin-remote comment-on-proposal!
  [server-id proposal-id-str body fork-branch]
  (spin-remote server-id [proposal-id-str body fork-branch]
    (let [s (identity proposal-id-str) b (identity body) fb (identity fork-branch)]
      ;; the author is the JWT principal, never the client's word for it — a
      ;; review conversation whose attribution can be set by the sender is not
      ;; a record of who said what
      #?(:clj (str (props/add-comment! (java.util.UUID/fromString s)
                                       {:body b
                                        :author (access/authenticated-party-id)
                                        :fork-branch (when-not (str/blank? fb) fb)}))
         :cljs nil))))

(defn-spin-remote request-changes!
  [server-id proposal-id-str body fork-branch]
  (spin-remote server-id [proposal-id-str body fork-branch]
    (let [s (identity proposal-id-str) b (identity body) fb (identity fork-branch)]
      #?(:clj (ctx/with-server-context
                ;; `reopen-fork!` reaches the room's overlay registry, and
                ;; `head-id` resolves a system — both want a bound context
                (let [r (props/request-changes! (java.util.UUID/fromString s)
                                                {:body b
                                                 :by (access/authenticated-party-id)
                                                 :fork-branch (when-not (str/blank? fb) fb)})]
                  (let [strs #(mapv (fn [x] (-> x
                                                 (update :scope str)
                                                 (update :author str)))
                                    %)]
                    (-> r
                        (update :comment str)
                        (update :reopened strs)
                        (update :not-reopened strs)))))
         :cljs nil))))

(defn-spin-remote dismiss-proposal!
  [server-id proposal-id-str note]
  (spin-remote server-id [proposal-id-str note]
    (let [s (identity proposal-id-str) n (identity note)]
      #?(:clj (props/dismiss-proposal! (java.util.UUID/fromString s)
                                       :note n
                                       :by (access/authenticated-party-id))
         :cljs nil))))

;; Per-fork decisions. Both are policed exactly like their whole-proposal
;; counterparts (`{:proposal id}` → the room, in access/rpc-policy): the room
;; decides who may SEE a ForkSet, and for the accept path `ops.proposals`
;; additionally requires :write on THAT fork's scope, because landing one fork
;; writes to one trunk. Refusing a fork writes to no trunk, so it carries the
;; room check alone — the same shape as `dismiss-proposal!`.

(defn-spin-remote accept-fork!
  [server-id proposal-id-str scope-str branch force? note]
  (spin-remote server-id [proposal-id-str scope-str branch force? note]
    (let [s (identity proposal-id-str) sc (identity scope-str)
          b (identity branch) f (identity force?) n (identity note)]
      #?(:clj (props/accept-fork! (java.util.UUID/fromString s)
                                  (java.util.UUID/fromString sc)
                                  (keyword b)
                                  :force? (boolean f)
                                  :note n
                                  ;; `by` from the JWT principal, never the
                                  ;; client — see accept-proposal! above
                                  :by (access/authenticated-party-id))
         :cljs nil))))

(defn-spin-remote dismiss-fork!
  [server-id proposal-id-str scope-str branch note]
  (spin-remote server-id [proposal-id-str scope-str branch note]
    (let [s (identity proposal-id-str) sc (identity scope-str)
          b (identity branch) n (identity note)]
      #?(:clj (props/dismiss-fork! (java.util.UUID/fromString s)
                                   (java.util.UUID/fromString sc)
                                   (keyword b)
                                   :note n
                                   :by (access/authenticated-party-id))
         :cljs nil))))
