(ns is.simm.workflows.review
  "Review escalations become proposals, and people's decisions become labels.

   dvergr's review stage (`dvergr.agent.review/review!`) decides a retained
   world: deterministic checks, an optional reviewer, then merge (pinned to the
   state it reviewed), escalate to a person, or discard. It records one
   immutable Review per decision in the room's store. This namespace is the
   simmis half (doc/proposals-and-time-travel.md, \"Review escalations\"):

   - `review-run!` runs a review on a worker, merging or discarding through
     `is.simm.ops.dvergr/invoke` AS the reviewer's party, so simmis's
     authorizer decides whether the reviewer may land anything.
   - An ESCALATED Review becomes a proposal (`file-escalation!`): one per
     Review, its notes as the first comment, linked to the Review's id, state
     token and policy. The trigger is the Review RECORD — a listener on the
     room store's transactions (`ensure-listener!`) — not dvergr's escalation
     message, which is posted without `:type` and so never reaches a room's
     message listeners (dvergr 0.1.286). The record is the authoritative half
     anyway, and a boot sweep (`sweep!`) files whatever a crash left behind.
   - Deciding such a proposal (`decide!`) merges with `:expect-state` = the
     reviewed token, through dvergr as the deciding party, or discards it; and
     every decision records a label in the ROOM's store (`labels`).

   A spot-checked review (the reviewer approved, and the review was sampled
   for a person) escalates in dvergr rather than merging, so it is the same
   kind of proposal, marked `:proposal/review-spot-check?`. Deciding it also
   records a `:reviewer` label: agreeing with the approval or not."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [datahike.api :as d]
            [dvergr.agent.review :as review]
            [dvergr.room.registry :as rreg]
            [dvergr.room.store :as rstore]
            [dvergr.rooms.forks :as forks]
            [is.simm.agents.room-agents :as room-agents]
            [is.simm.model.access :as access]
            [is.simm.model.parties :as parties]
            [is.simm.model.room-databases :as room-dbs]
            [is.simm.model.system-db :as sdb]
            [is.simm.ops.dvergr :as dvergr]
            [is.simm.ops.proposals :as proposals]
            [is.simm.runtimes.context :as ctx]
            [org.replikativ.spindel.engine.core :as ec]
            [taoensso.telemere :as log])
  (:import [java.util.concurrent ExecutorService Executors ThreadFactory]))

;; =============================================================================
;; Workers
;; =============================================================================

(defn- worker
  "One daemon thread. A review blocks (diffs, checks, a merge) and must never
   run on a render, RPC or room-bus thread; filings are serialized so two
   sightings of the same Review cannot race to file it twice."
  [thread-name]
  (Executors/newSingleThreadExecutor
   (reify ThreadFactory
     (newThread [_ r]
       (doto (Thread. ^Runnable r ^String thread-name)
         (.setDaemon true))))))

(defonce ^:private ^ExecutorService review-worker (worker "simmis-review"))
(defonce ^:private ^ExecutorService escalation-worker (worker "simmis-review-escalations"))

(defn- submit
  "Run `f` on `ex` in the server's execution context: a worker thread has no
   binding of its own, and live rooms are registered there."
  ^java.util.concurrent.Future [^ExecutorService ex f]
  (.submit ex ^Callable (fn [] (ctx/with-server-context (f)))))

(defn await-escalations!
  "Block until every escalation seen so far is filed. The store listener queues
   the filing before the Review's transaction returns (datahike notifies its
   listeners before delivering the commit), so after `review!` returns this is
   a barrier, not a guess."
  []
  @(submit escalation-worker (constantly nil)))

;; =============================================================================
;; Rooms and worlds
;; =============================================================================

(defn- conn [] (sdb/get-conn))

(defn- room-arg
  "A dvergr room id as ops and Review records spell it: no colon."
  [id]
  (if (keyword? id) (subs (str id) 1) (str id)))

(defn- simmis-room-of
  "The simmis `:room/id` of the live dvergr room `room`, by its slug."
  [room]
  (when-let [db (some-> (conn) deref)]
    (d/q '[:find ?id . :in $ ?slug :where [?r :room/slug ?slug] [?r :room/id ?id]]
         db (rstore/room-id->slug (:id room)))))

(defn- live-world
  "The live fork `world` (an id string) of `room`, or nil."
  [room world]
  (when (and room world)
    (binding [ec/*execution-context* (:ctx room)]
      (rreg/lookup (keyword world)))))

(defn escalation-proposal-id
  "The proposal an escalated Review becomes: a pure function of the Review id,
   so a retried filing names the same proposal."
  [review-id]
  (java.util.UUID/nameUUIDFromBytes
   (.getBytes (str "simmis:review-proposal:" review-id) "UTF-8")))

(defn- notes-comment-id [review-id]
  (java.util.UUID/nameUUIDFromBytes
   (.getBytes (str "simmis:review-notes:" review-id) "UTF-8")))

(defn proposal-for-review
  "The proposal filed for Review `review-id`, or nil."
  [review-id]
  (when review-id
    (when-let [e (d/q '[:find ?p . :in $ ?r :where [?p :proposal/review ?r]]
                      @(conn) review-id)]
      (d/pull @(conn) '[* {:proposal/forks [*]}] e))))

(defn- world-of-scope
  "`{:proposal :room :world}` for a review proposal's fork scope (the Review
   id); `:world` nil when the world is not live in this process."
  [scope]
  (when-let [p (proposal-for-review scope)]
    (let [room (room-agents/live-room (:proposal/room p))
          world (some-> p :proposal/forks first :proposal.fork/branch)]
      {:proposal p :room room :world (live-world room world)})))

;; =============================================================================
;; The branching backend's read half (`branching` system-type :review-world)
;; =============================================================================

(defn- world-state [world] (forks/state-token world))

(defn world-live?
  "Whether this process holds the reviewed world of `scope` (a Review id) in a
   state it can still read. A world can stay registered with its stores
   released — a merge that failed half-way (dvergr 0.1.286) — and then nothing
   can merge or discard it; it is reported, not offered."
  [scope]
  (if-let [w (:world (world-of-scope scope))]
    (try (some? (world-state w))
         (catch Throwable t
           (log/log! {:level :warn :id ::world-unreadable
                      :data {:review scope :world (:id w) :error (ex-message t)}})
           false))
    false))

(defn world-head-id
  "A cache key for the world: its CURRENT state token (so a world that changed
   since review re-keys every derived view), or a fixed trunk name."
  [scope branch]
  (if (= branch :world/trunk)
    (str "trunk:" scope)
    (when-let [w (:world (world-of-scope scope))]
      (str "state:" (world-state w)))))

(defn- changed-files
  "Every path the world changed, committed or not, and the other systems it
   changed — what dvergr's checks look at (`dvergr.agent.review`)."
  [world]
  (let [{:keys [diff]} (forks/review world)
        file-deltas (filter (fn [[_ delta]] (contains? delta :files)) diff)
        others (for [[sid delta] diff
                     :when (and (not (contains? delta :files))
                                (or (:error delta)
                                    (some #(and (number? %) (pos? %)) (vals (:summary delta)))
                                    (seq (:intents delta))))]
                 ;; a diff that failed is a change (dvergr's checks say so too),
                 ;; and the reviewer of the card needs to know which
                 (str sid (when (:error delta) (str " (its diff failed: " (:error delta) ")"))))]
    {:files (vec (sort (distinct (concat (for [[_ delta] file-deltas
                                               {:keys [path]} (:files delta)
                                               p (str/split (str path) #"\t")]
                                           p)
                                         (map :path (forks/uncommitted world))))))
     :other-systems (vec (sort others))}))

(defn world-delta
  "The world's change set in the shape the classifier reads (`:files`, like a
   repository diff, uncommitted files included — a merge commits them)."
  [scope]
  (when-let [w (:world (world-of-scope scope))]
    (let [{:keys [files other-systems]} (changed-files w)]
      {:files (mapv (fn [p] {:path p}) files)
       :other-systems other-systems
       :summary {:files-changed (count files)}})))

(defn world-conflicts [scope]
  (if-let [w (:world (world-of-scope scope))]
    (vec (forks/fork-conflicts w))
    []))

;; =============================================================================
;; Escalation → proposal
;; =============================================================================

(defonce ^:private contexts
  ;; world id → {:reviewer-party :workflow}, set by `review-run!` while its
  ;; review runs, so a filing that the store listener triggers knows who
  ;; reviewed. A review dvergr starts on its own has none, and files without.
  (atom {}))

(defn- bullet-lines [xs] (str/join "\n" (map #(str "- " %) xs)))

(defn- notes
  "The reviewer's notes, as the proposal's first comment."
  [{:review/keys [reasons verdict checks spot-check?]}]
  (let [failed (remove :pass? checks)]
    (str/join
     "\n\n"
     (remove str/blank?
             [(cond
                spot-check? "The reviewer approved this; it was sampled for a person to check."
                verdict (str "The reviewer's verdict: " (name (:review/verdict verdict)) ".")
                :else "No reviewer was asked: a check failed.")
              (when (seq (:review/reasons verdict))
                (str "Reasons:\n" (bullet-lines (:review/reasons verdict))))
              (when (seq (:review/concerns verdict))
                (str "Concerns:\n" (bullet-lines (:review/concerns verdict))))
              (when (seq failed)
                (str "Failed checks:\n"
                     (bullet-lines (map #(str (pr-str (:check %)) " — " (pr-str (:evidence %)))
                                        failed))))
              (when (and (empty? (:review/reasons verdict)) (seq reasons))
                (str "Why it needs a person:\n" (bullet-lines reasons)))]))))

(defn- record-edn
  "What the card shows of a Review, copied: a Review is immutable."
  [rv]
  (pr-str (select-keys rv [:review/id :review/world :review/decision :review/checks
                           :review/verdict :review/reasons :review/spot-check?
                           :review/run-id :review/reviewed-at :review/policy])))

(defn file-escalation!
  "File escalated Review `rv` of simmis room `room-id` as a proposal, once.
   Returns the proposal id, or nil when `rv` is not an escalation or its world
   is no longer live (decided elsewhere, or gone with a restart).

   Idempotent per Review: a second sighting finds the proposal by
   `:proposal/review` and files nothing. Runs on the escalation worker
   (`submit-filing!`); the filing itself is one transaction."
  [room-id rv & {:keys [reviewer-party workflow]}]
  (let [rid (:review/id rv)
        world (:review/world rv)]
    (cond
      (not= :escalated (:review/decision rv)) nil

      (proposal-for-review rid) (:proposal/id (proposal-for-review rid))

      (nil? (live-world (room-agents/live-room room-id) world))
      (do (log/log! {:level :info :id ::escalation-world-gone
                     :data {:room room-id :review rid :world world}})
          nil)

      :else
      (let [pid (escalation-proposal-id rid)
            spot? (boolean (:review/spot-check? rv))
            ;; whose `:merge` decides it: the room's store scope, which `can?`
            ;; answers as the room's owner — the same scope every proposal
            ;; affordance and policy row (`with-merge-authority`,
            ;; `{:proposal-forks …}`) asks about
            authority (or (room-dbs/get-room-db-scope room-id)
                          (throw (ex-info "The room has no store scope" {:room room-id})))]
        (try
          (proposals/file-proposal!
           {:id pid
            :title (str (if spot? "Spot check: " "Review: ")
                        (or (:id workflow) "a workflow's work") " needs a person")
            :summary (str/join "; " (:review/reasons rv))
            :author reviewer-party
            :room room-id
            :run (:review/run-id rv)
            ;; no fork author: the reviewer filed it but did not write it,
            ;; and a fork's author may withdraw it
            :forks [{:scope rid :authority-scope authority :branch world
                     :system-type :review-world}]
            :attrs (cond-> {:proposal/review rid
                            :proposal/review-state (:review/state-token rv)
                            :proposal/review-policy (:review/policy rv)
                            :proposal/review-spot-check? spot?
                            :proposal/review-record (record-edn rv)}
                     (:id workflow) (assoc :proposal/workflow-id (str (:id workflow)))
                     (:version workflow) (assoc :proposal/workflow-version
                                                (str (:version workflow))))
            :comments [(cond-> {:proposal.comment/id (notes-comment-id rid)
                                :proposal.comment/body (notes rv)
                                :proposal.comment/at (:review/reviewed-at rv)
                                :proposal.comment/kind :comment}
                         reviewer-party (assoc :proposal.comment/author reviewer-party))]})
          (catch clojure.lang.ExceptionInfo e
            ;; another filing of the same Review won with other context
            (when-not (= :proposal/filing-conflict (:type (ex-data e)))
              (throw e))))
        (log/log! {:level :info :id ::escalation-filed
                   :data {:room room-id :review rid :proposal pid :spot-check? spot?}})
        pid))))

(defn- submit-filing!
  "File `rv` on the escalation worker; a future of the proposal id."
  [room-id rv]
  (submit escalation-worker
          (fn []
            (try
              (let [ctx (get @contexts (:review/world rv))]
                (file-escalation! room-id rv
                                  :reviewer-party (:reviewer-party ctx)
                                  :workflow (:workflow ctx)))
              (catch Throwable t
                (log/log! {:level :error :id ::escalation-filing-failed :error t
                           :data {:room room-id :review (:review/id rv)}})
                (throw t))))))

(defn- escalated-reviews
  "The Reviews that `tx-data` recorded as escalated, read from `db-after`."
  [tx-data db-after]
  (for [e (distinct (keep (fn [datom]
                            (when (and (:added datom)
                                       (= :review/decision (:a datom))
                                       (= :escalated (:v datom)))
                              (:e datom)))
                          tx-data))
        :let [payload (d/q '[:find ?p . :in $ ?e :where [?e :review/payload-edn ?p]]
                           db-after e)]
        :when payload]
    (edn/read-string payload)))

(defonce ^:private listening
  ;; #{[room-uuid conn-identity]} — one listener per room store per process
  (atom #{}))

(defn ensure-listener!
  "Listen to `room`'s store (the live dvergr room of simmis room `room-id`) for
   escalated Reviews and file each as a proposal. Idempotent. The callback only
   queues the filing: it runs on datahike's commit path."
  [room room-id]
  (when-let [c (some-> room :store :conn)]
    (let [k [room-id (System/identityHashCode c)]]
      (when-not (contains? @listening k)
        (swap! listening conj k)
        (d/listen c ::escalations
                  (fn [{:keys [tx-data db-after]}]
                    (doseq [rv (escalated-reviews tx-data db-after)]
                      (submit-filing! room-id rv))))
        (log/log! {:level :debug :id ::listening :data {:room room-id}})))))

(defn- arm-room!
  "Arm a registered dvergr room, if it is a simmis room (forks are not)."
  [room]
  (when-not (:parent-id room)
    (when-let [room-id (simmis-room-of room)]
      (ensure-listener! room room-id)
      room-id)))

(defn install!
  "Arm every simmis room as dvergr registers it. Call before hydration."
  []
  (rreg/add-register-hook! ::escalations
                           (fn [room]
                             (try (arm-room! room)
                                  (catch Throwable t
                                    (log/log! {:level :warn :id ::arm-failed
                                               :data {:room (:id room) :error (ex-message t)}})))))
  nil)

(defn sweep!
  "Arm the live rooms and file every escalated Review that has no proposal yet
   and whose world is still live — what a crash between a Review and its
   filing left behind. Call after hydration. Returns the number queued."
  []
  (let [queued
        (ctx/with-server-context
          (doall
           (for [room (rreg/list-rooms)
                 :let [room-id (try (arm-room! room) (catch Throwable _ nil))]
                 :when room-id
                 rv (review/reviews room {:decision :escalated})
                 :when (and (nil? (proposal-for-review (:review/id rv)))
                            (live-world room (:review/world rv)))]
             (submit-filing! room-id rv))))]
    (doseq [f queued] (try @f (catch Throwable _ nil)))
    (log/log! {:level :info :id ::swept :data {:filed (count queued)}})
    (count queued)))

;; =============================================================================
;; Running a review
;; =============================================================================

(defn review-run!
  "Review the retained world `world` (a fork id) of simmis room `room-id` under
   `spec`, ON THE REVIEW WORKER. Returns a future of `{:review <the stored
   Review> :proposal-id <the escalation's proposal, or nil>}`.

   `reviewer` is the reviewer function dvergr asks (`dvergr.agent.review`);
   `reviewer-party` is the party it reviews as: a merge or discard goes
   through dvergr's ops AS that party, so simmis's authorizer decides whether
   it may (a room's merge is its owner's — until delegation exists, a review
   that runs as anyone else escalates with the refusal as its reason).
   `workflow` (`{:id :version}`) is carried onto the proposal and its labels.

   Nothing calls this on its own yet: starting reviews when a workflow settles
   is dvergr's."
  [room-id world spec & {:keys [run-id reviewer reviewer-party workflow now]}]
  (when-not reviewer-party
    (throw (ex-info "A review merges as a party; none was given" {:room room-id})))
  (submit
   review-worker
   (fn []
     (let [room (or (room-agents/live-room room-id)
                    (throw (ex-info "Room is not live" {:room room-id})))
           world (room-arg world)
           principal (dvergr/party-principal reviewer-party)]
       (ensure-listener! room room-id)
       (swap! contexts assoc world {:reviewer-party reviewer-party :workflow workflow})
       (try
         (let [rv (review/review! room (keyword world) spec
                                  :run-id run-id :reviewer reviewer :now now
                                  :invoke (fn [op args] (dvergr/invoke (:ctx room) principal op args)))
               pid (when (= :escalated (:review/decision rv))
                     ;; the listener queued it already; this files it if it
                     ;; did not (and is a no-op if it did)
                     @(submit-filing! room-id rv))]
           (log/log! {:level :info :id ::reviewed
                      :data {:room room-id :world world :review (:review/id rv)
                             :decision (:review/decision rv) :proposal pid}})
           {:review rv :proposal-id pid})
         (finally (swap! contexts dissoc world)))))))

;; =============================================================================
;; Labels — the room's record of what people decided
;; =============================================================================

(defn- room-conn
  "The room's store, or nil when it is not hydrated in this process."
  [room-id]
  (some-> (room-dbs/get-room-db-scope room-id) room-dbs/connect-room-database))

(defn- label->entity
  [{:label/keys [id subject about review policy reviewer-verdict workflow verdict
                 edit by reasons at]}]
  (let [[kind sid] subject]
    (cond-> {:entity/uuid id
             :entity/created-at at
             :S.Label/subject-kind kind
             :S.Label/subject-id sid
             :S.Label/about about
             :S.Label/verdict verdict
             :S.Label/by by
             :S.Label/at at}
      review (assoc :S.Label/review review)
      policy (assoc :S.Label/policy policy)
      reviewer-verdict (assoc :S.Label/reviewer-verdict reviewer-verdict)
      (:id workflow) (assoc :S.Label/workflow-id (str (:id workflow)))
      (:version workflow) (assoc :S.Label/workflow-version (str (:version workflow)))
      (not (str/blank? edit)) (assoc :S.Label/edit edit)
      (not (str/blank? reasons)) (assoc :S.Label/reasons (str/trim reasons)))))

(defn- entity->label [e]
  (cond-> {:label/id (:entity/uuid e)
           :label/subject [(:S.Label/subject-kind e) (:S.Label/subject-id e)]
           :label/about (:S.Label/about e)
           :label/verdict (:S.Label/verdict e)
           :label/by (:S.Label/by e)
           :label/at (:S.Label/at e)}
    (:S.Label/review e) (assoc :label/review (:S.Label/review e))
    (:S.Label/policy e) (assoc :label/policy (:S.Label/policy e))
    (:S.Label/reviewer-verdict e) (assoc :label/reviewer-verdict (:S.Label/reviewer-verdict e))
    (:S.Label/workflow-id e) (assoc :label/workflow
                                    (cond-> {:id (:S.Label/workflow-id e)}
                                      (:S.Label/workflow-version e)
                                      (assoc :version (:S.Label/workflow-version e))))
    (:S.Label/edit e) (assoc :label/edit (:S.Label/edit e))
    (:S.Label/reasons e) (assoc :label/reasons (:S.Label/reasons e))))

(def verdicts #{:accept :reject :edit})
(def abouts #{:workflow :reviewer})

(defn- record-labels!
  "Append `labels` to room `room-id`'s store, in one transaction. There is no
   update and no delete: a label is what someone decided, when they decided it."
  [room-id labels]
  (doseq [l labels]
    (when-not (and (contains? verdicts (:label/verdict l))
                   (contains? abouts (:label/about l))
                   (uuid? (:label/by l)))
      (throw (ex-info "Not a label" {:label l}))))
  (d/transact (or (room-conn room-id)
                  (throw (ex-info "The room's store is not available" {:room room-id})))
              (mapv label->entity labels))
  (mapv :label/id labels))

(defn labels
  "Room `room-id`'s labels, newest first, as `:label/*` maps — or nil when the
   room's store is not hydrated in this process (unknown, which is not none).
   Filters: `:subject-id` (a proposal or run id), `:about`, `:review`."
  [room-id & {:keys [subject-id about review]}]
  (when-let [c (room-conn room-id)]
    (->> (d/q '[:find [(pull ?l [*]) ...] :where [?l :S.Label/verdict _]] @c)
         (map entity->label)
         (filter #(or (nil? subject-id) (= subject-id (second (:label/subject %)))))
         (filter #(or (nil? about) (= about (:label/about %))))
         (filter #(or (nil? review) (= review (:label/review %))))
         (sort-by (juxt :label/at (comp str :label/id)) #(compare %2 %1))
         vec)))

(defn labels-for-wire
  "`labels` as a client renders them: uuids as strings, the deciding party's
   name resolved here (the client has no party directory). nil as `labels`."
  [room-id]
  (some->> (labels room-id)
           (mapv (fn [{:label/keys [id subject about verdict by reasons at review workflow
                                    reviewer-verdict]}]
                   {:id (str id)
                    :subject [(first subject) (str (second subject))]
                    :about about :verdict verdict
                    :by (str by)
                    :by-name (let [p (parties/get-party by)]
                               (or (:party/display-name p) (:party/handle p) (str by)))
                    :reasons reasons :at at
                    :review (some-> review str)
                    :workflow workflow
                    :reviewer-verdict reviewer-verdict}))))

;; =============================================================================
;; Deciding an escalated proposal
;; =============================================================================

(defn- changed-since-review?
  [e]
  (let [{:keys [type]} (ex-data e)
        msg (str (ex-message e))]
    (or (= :dvergr.ops/fork-changed type)
        (= :dvergr.rooms.forks/fork-changed type)
        (str/includes? msg "changed since it was reviewed"))))

(defn review-record
  "The Review a review's proposal was filed for, as copied onto it."
  [p]
  (some-> (:proposal/review-record p) edn/read-string))

(defn decide!
  "A person's decision on an escalated review's proposal `proposal-id`:
   `:accept` merges the world with `:expect-state` = the token it was reviewed
   at — a world that changed since is refused, and stays open to be refused or
   reviewed again — and `:reject` discards it. Both go through dvergr's ops as
   `by`, who must hold `:merge` on the proposal's room (its owner): the same
   rule `ops.proposals` applies to landing anything, asked twice (here, and by
   dvergr's authorizer for the op itself).

   Records the decision on the proposal and the label(s) in the room's store.
   `note` becomes the label's reasons. Returns `{:status :labels}`."
  [proposal-id verdict & {:keys [note by]}]
  {:pre [(#{:accept :reject} verdict)]}
  (ctx/with-server-context
    (let [p (proposals/get-proposal proposal-id)
          _ (when-not p (throw (ex-info "unknown proposal" {:id proposal-id})))
          _ (when-not (= :open (:proposal/status p))
              (throw (ex-info "proposal not open" {:id proposal-id :status (:proposal/status p)})))
          _ (when-not (:proposal/review p)
              (throw (ex-info "not a review's proposal" {:id proposal-id})))
          _ (when-not by
              (throw (ex-info "a decision needs the party who takes it"
                              {:error :not-authorized :proposal proposal-id})))
          room-id (:proposal/room p)
          f (first (proposals/open-forks p))
          _ (when-not (and (access/can? by :merge {:room room-id})
                           (access/can? by :merge (:proposal.fork/authority-scope f)))
              (throw (ex-info "only someone who may land onto this room decides its reviews"
                              {:error :not-authorized :proposal proposal-id})))
          room (or (room-agents/live-room room-id)
                   (throw (ex-info "Room is not live" {:room room-id})))
          world (:proposal.fork/branch f)
          _ (when-not (live-world room world)
              (throw (ex-info "The reviewed world is not live in this process"
                              {:error :world-unavailable :proposal proposal-id :world world})))
          principal (dvergr/party-principal by)
          token (:proposal/review-state p)]
      (case verdict
        :accept
        (try
          (dvergr/invoke (:ctx room) principal :room/merge {:room world :expect-state token})
          (catch clojure.lang.ExceptionInfo e
            (if (changed-since-review? e)
              (throw (ex-info (str "The world changed after it was reviewed, so it was not merged."
                                   " Reject it, or have it reviewed again.")
                              {:error :world-changed :proposal proposal-id
                               :reviewed token}))
              (throw e))))
        :reject
        (dvergr/invoke (:ctx room) principal :room/discard {:room world}))
      (let [status (proposals/record-decision! p (if (= :accept verdict) :accepted :dismissed)
                                               note by)
            rv (review-record p)
            now (java.util.Date.)
            base {:label/subject [:proposal proposal-id]
                  :label/review (:proposal/review p)
                  :label/policy (:proposal/review-policy p)
                  :label/verdict verdict
                  :label/by by
                  :label/reasons note
                  :label/at now}
            base (cond-> base
                   (get-in rv [:review/verdict :review/verdict])
                   (assoc :label/reviewer-verdict (get-in rv [:review/verdict :review/verdict]))
                   (:proposal/workflow-id p)
                   (assoc :label/workflow (cond-> {:id (:proposal/workflow-id p)}
                                            (:proposal/workflow-version p)
                                            (assoc :version (:proposal/workflow-version p)))))
            ls (cond-> [(assoc base :label/id (random-uuid) :label/about :workflow)]
                 ;; the reviewer approved: accepting agrees with it, rejecting does not
                 (:proposal/review-spot-check? p)
                 (conj (assoc base :label/id (random-uuid) :label/about :reviewer)))
            ids (try (record-labels! room-id ls)
                     (catch Throwable t
                       ;; the decision stands — it is merged or discarded — so the
                       ;; missing label must be loud, not a second failure mode
                       (log/log! {:level :error :id ::label-not-recorded :error t
                                  :data {:proposal proposal-id :verdict verdict}})
                       (throw (ex-info "Decided, but the label was not recorded"
                                       {:error :label-not-recorded :proposal proposal-id
                                        :status status}
                                       t))))]
        (log/log! {:level :info :id ::decided
                   :data {:proposal proposal-id :review (:proposal/review p)
                          :verdict verdict :status status :labels (count ids)}})
        {:status status :labels ids}))))
