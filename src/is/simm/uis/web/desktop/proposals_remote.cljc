(ns is.simm.uis.web.desktop.proposals-remote
  "Spin-remotes for the proposals inbox (S2). Thin pass-throughs over
   is.simm.ops.proposals + ops.semantic-diff; all UUIDs cross the wire as
   strings. Diffs are computed server-side (semantic ops only; full datoms
   never leave the server)."
  (:refer-clojure :exclude [await])
  (:require [clojure.string :as str]
            [org.replikativ.spindel.effects.await :refer [await]]
            [org.replikativ.spindel.distributed.macros :refer [defn-spin-remote]
             :include-macros true]
            [org.replikativ.spindel.distributed.core :as dist]
            [is.simm.model.forkset :as fs]
            #?(:clj [org.replikativ.spindel.blocking :as blocking])
            #?(:clj [is.simm.ops.proposals :as props])
            #?(:clj [is.simm.ops.owned-reads :as owned-reads])
            #?(:clj [org.replikativ.spindel.engine.core :as ec])
            #?(:clj [is.simm.ops.semantic-diff :as sdiff])
            #?(:clj [is.simm.ops.file-output-review :as file-review])
            #?(:clj [is.simm.ops.proposal-evidence :as evidence])
            #?(:clj [is.simm.ops.file-output-service :as file-service])
            #?(:clj [is.simm.ops.retained-knowledge-sources :as retained-sources])
            #?(:clj [is.simm.ops.file-output-adoption :as adoption])
            #?(:clj [is.simm.runtimes.application-ops :as application])
            #?(:clj [is.simm.ops.checks :as checks])
            #?(:clj [is.simm.runtimes.branching :as branching])
            #?(:clj [is.simm.model.access :as access])
            #?(:clj [is.simm.model.parties :as parties])
            #?(:clj [is.simm.runtimes.context :as ctx])
            #?(:clj [is.simm.workflows.review :as wreview])))

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

#?(:clj
   (defn- review-view
     "What a card shows of an escalated review: the reviewer's verdict, reasons
      and concerns, every check with its evidence, whether this is a spot check
      of an approval, and whether the world changed since it was reviewed (then
      Accept will refuse). Checks and evidence travel as printed EDN — they are
      open-ended data, shown, never interpreted, by the client."
     [p]
     (when (:proposal/review p)
       (let [rv (wreview/review-record p)
             f (first (:proposal/forks p))
             live? (and (nil? (:proposal.fork/status f))
                        (wreview/world-live? (:proposal.fork/scope f)))
             now (when live? (wreview/world-head-id (:proposal.fork/scope f) nil))]
         {:id (str (:proposal/review p))
          :spot-check? (boolean (:proposal/review-spot-check? p))
          :verdict (get-in rv [:review/verdict :review/verdict])
          :reviewer-reasons (vec (get-in rv [:review/verdict :review/reasons]))
          :concerns (vec (get-in rv [:review/verdict :review/concerns]))
          :reasons (vec (:review/reasons rv))
          :checks (mapv (fn [{:keys [check pass? evidence]}]
                          {:check (pr-str check) :pass? (boolean pass?)
                           :evidence (when (seq evidence) (pr-str evidence))})
                        (:review/checks rv))
          :workflow (:proposal/workflow-id p)
          :changed? (boolean (and now (not= now (str "state:" (:proposal/review-state p)))))}))))

;; A room's labels: what its people decided on reviewed work, newest first.
;; Append-only — there is no RPC that changes or removes one.
(defn-spin-remote list-labels!
  [server-id room-id-str]
  (spin-remote server-id [room-id-str]
               (let [rid (identity room-id-str)]
                 #?(:clj (ctx/with-server-context
                           (wreview/labels-for-wire (java.util.UUID/fromString rid)))
                    :cljs nil))))

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
                              ;; an escalated review's notes; nil otherwise
                                         :review (review-view p)
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
                                                         :settlement-supported?
                                                         (:proposal.fork/settlement-supported? f)
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
                             (let [party (access/authenticated-party-id)
                                   open (filterv #(or (props/fork-open? %)
                                                      (= :file-output (:proposal.fork/system-type %))
                                                      (:proposal.fork/head-commit %))
                                                 (:proposal/forks p))
                                   review (props/proposal-review p)
                        ;; Live branches are diffed only while open. Retained
                        ;; file components remain inspectable after dismissal:
                        ;; they own immutable evidence, not a discarded branch.
                                   diffs (sdiff/proposal-diff (assoc p :proposal/forks open) :principal party)
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
                                                            :status (:proposal.fork/status f)
                                                            :head-commit (:proposal.fork/head-commit f))
                                                     (update :pages
                                                             (fn [ps]
                                                               (mapv #(update % :page-uuid str) ps))))))
                                             (:proposal/forks p))})))
                    :cljs nil))))

(defn-spin-remote proposal-file!
  [server-id proposal-id-str scope-str path]
  (spin-remote server-id [proposal-id-str scope-str path]
               #?(:clj (let [party (access/authenticated-party-id)]
              ;; Capture authentication at server entry, never from client data
              ;; and never by rereading dynamic bindings in a later continuation.
                         (await (blocking/blocking
                                 #(ctx/with-server-context
                                    (file-review/read-file party (java.util.UUID/fromString proposal-id-str)
                                                           (java.util.UUID/fromString scope-str) path)))))
                  :cljs nil)))

#?(:clj
   (defn- wire-uuid [value]
     (when (and (string? value)
                (re-matches #"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}" value))
       (parse-uuid value))))

#?(:clj
   (defn prepare-source-operation
     "Capture the original App before this fixed retained read is queued."
     [party operation proposal component options]
     (when-not (uuid? party)
       (throw (ex-info "Authentication required" {:type :authentication-required})))
     (when-not (#{:list :read :audit} operation)
       (throw (ex-info "Unknown retained source operation" {:type :validation-error})))
     (let [unavailable (fn [error]
                         (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
                         {:status :unavailable})]
       (try
         (let [invoke (application/capture-operation)
               proposal (wire-uuid proposal) component (wire-uuid component)]
           (fn []
             (try
               (invoke (fn [host]
                         (case operation
                           :audit (or (evidence/audit host party proposal)
                                      (file-review/audit party proposal))
                           :list (retained-sources/list! host party proposal component)
                           :read (retained-sources/read! host party proposal component options))))
               (catch Throwable error (unavailable error)))))
         (catch Throwable error
           (let [result (unavailable error)] (constantly result)))))))

(defn-spin-remote proposal-retained-audit!
  [server-id proposal-id-str]
  (spin-remote server-id [proposal-id-str]
               #?(:clj (let [body (prepare-source-operation (access/authenticated-party-id) :audit
                                                            proposal-id-str nil nil)]
                         (await (blocking/blocking body)))
                  :cljs nil)))

(defn-spin-remote list-proposal-sources!
  [server-id proposal-id-str component-id-str]
  (spin-remote server-id [proposal-id-str component-id-str]
               #?(:clj (let [body (prepare-source-operation (access/authenticated-party-id) :list
                                                            proposal-id-str component-id-str nil)]
                         (await (blocking/blocking body)))
                  :cljs nil)))

(defn-spin-remote read-proposal-source!
  [server-id proposal-id-str component-id-str options]
  (spin-remote server-id [proposal-id-str component-id-str options]
               #?(:clj (let [body (prepare-source-operation (access/authenticated-party-id) :read
                                                            proposal-id-str component-id-str options)]
                         (await (blocking/blocking body)))
                  :cljs nil)))

(defn-spin-remote preview-file-proposal!
  [server-id proposal-id-str]
  (spin-remote server-id [proposal-id-str]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking
                                 #(file-service/preview! party (wire-uuid proposal-id-str)))))
                  :cljs nil)))

(defn-spin-remote adopt-reviewed-files!
  [server-id proposal-id-str expected-review-str]
  (spin-remote server-id [proposal-id-str expected-review-str]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking
                                 #(file-service/adopt! party (wire-uuid proposal-id-str)
                                                       (wire-uuid expected-review-str)))))
                  :cljs nil)))

(defn-spin-remote reconcile-file-proposal!
  [server-id proposal-id-str]
  (spin-remote server-id [proposal-id-str]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking
                                 #(file-service/reconcile! party (wire-uuid proposal-id-str)))))
                  :cljs nil)))

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
               #?(:clj (ctx/with-server-context
                         (when-let [p (props/get-proposal (java.util.UUID/fromString proposal-id-str))]
                           (vec (keep (fn [f]
                                        (when (= :repo (:proposal.fork/system-type f))
                                          (let [stored (:proposal.fork/checks f)
                                                result (if stored
                                                         (into {} (map (fn [[k v]] [(keyword (name k)) v]))
                                                               (dissoc stored :db/id))
                                                         (when (props/fork-open? f)
                                                           (checks/fork-checks (:proposal.fork/scope f) :repo
                                                                               (keyword (:proposal.fork/branch f))
                                                                               (:proposal.fork/head-commit f))))]
                                            (when result (assoc result :scope (str (:proposal.fork/scope f))
                                                                :branch (:proposal.fork/branch f))))))
                                      (:proposal/forks p)))))
                  :cljs nil)))

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

#?(:clj
   (defn prepare-decision-operation
     "Capture authenticated identity and the exact original App before queuing.
      This HOST-only thunk is not room state, a client capability or a callback
      supplied by the user. The worker retains its original control connection."
     [party verb proposal {:keys [scope branch note body]}]
     (when-not (uuid? party)
       (throw (ex-info "Authentication required" {:type :authentication-required})))
     (let [id (wire-uuid proposal) scope-id (when scope (wire-uuid scope))
           branch (if (and (string? branch) (str/blank? branch)) nil branch)]
       (when-not (and id (#{:dismiss-proposal :dismiss-fork :request-changes} verb)
                      (or (nil? note) (and (string? note) (<= (count note) 16000)))
                      (or (nil? branch) (and (string? branch) (<= (count branch) 512)))
                      (or (not= :dismiss-fork verb) (and scope-id branch))
                      (or (not= :request-changes verb) (and (string? body) (<= (count body) 16000))))
         (throw (ex-info "Invalid proposal decision" {:type :validation-error})))
       (let [invoke (application/capture-operation)
             action (case verb
                      :dismiss-proposal {:verb verb}
                      :dismiss-fork {:verb verb :scope scope-id :branch branch}
                      :request-changes (cond-> {:verb verb} branch (assoc :branch branch)))]
         (fn []
           (invoke
            (fn [host]
              (binding [ec/*execution-context* (:execution-ctx host)]
                (owned-reads/current-host-control-db! host)
                (props/decide-in! (get-in host [:is.simm.ops.owned-reads/owner :control-conn])
                                 id party action {:note note :body body})))))))))

(defn-spin-remote request-changes!
  [server-id proposal-id-str body fork-branch]
  (spin-remote server-id [proposal-id-str body fork-branch]
               #?(:clj
                  (let [operation (prepare-decision-operation (access/authenticated-party-id) :request-changes
                                                              proposal-id-str {:body body :branch fork-branch})
                        r (await (blocking/blocking operation))
                        strs #(mapv (fn [x] (-> x (update :scope str) (update :author str))) %)]
                    (-> r (update :comment str) (update :reopened strs) (update :not-reopened strs)))
                  :cljs nil)))

(defn-spin-remote dismiss-proposal!
  [server-id proposal-id-str note]
  (spin-remote server-id [proposal-id-str note]
               #?(:clj (let [operation (prepare-decision-operation (access/authenticated-party-id) :dismiss-proposal
                                                                   proposal-id-str {:note note})]
                         (await (blocking/blocking operation)))
                  :cljs nil)))

;; Per-fork decisions. The room decides who may SEE a ForkSet (`{:proposal id}`
;; in access/rpc-policy). Accepting additionally needs :merge on THAT fork's
;; scope (`ops.proposals`), because landing one fork writes to one trunk.
;; Dismissing deletes the fork's branch, so it needs control of the fork: its
;; author or a decider (`{:proposal-fork …}`; `dismiss-proposal!` the same for
;; every open fork, or being the proposal's filer).

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
               #?(:clj (let [operation (prepare-decision-operation (access/authenticated-party-id) :dismiss-fork
                                                                   proposal-id-str {:scope scope-str :branch branch :note note})]
                         (await (blocking/blocking operation)))
                  :cljs nil)))
