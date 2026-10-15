(ns is.simm.ops.tasks
  "Tasks as a VIEW over sources, not a table.

   Three things in this system are legitimately \"something to do\", and they
   live in three places for good reasons (doc/archive/navigation-redesign-plan.md,
   \"Resolved: S/Task vs dvergr :task/*\"):

     :page      an `S/Task` page in a KB — the durable work item, part of the
                wiki graph, forkable, proposable.
     :forkset   a landable ForkSet — a proposed change that is ready to merge
                IS a thing to do, and the user asked for exactly that: \"a
                proposal that is ready is a task\".
     :dispatch  a dvergr `:task/*` row — one actor asking another to do
                something. dvergr owns it; this namespace only reads it.

   Each source gets a small adapter to one shape, and the aggregate is a
   fan-out at read time. There is deliberately NO persisted index: KB pages are
   written by the client through konserve-sync replicas, which a server-side
   listener does not observe as tx reports, so an index would silently miss half
   the writes. A `can?`-filtered query per visible KB has no derived state to go
   wrong, and the KB count is small. Revisit under measurement, rebuild-first.

   Visibility is `access/can?` per KB, the one authorization seam — a global
   task list is exactly where a task from an invisible project would leak."
  (:require [is.simm.model.access :as access]
            [is.simm.model.forkset :as fs]
            [is.simm.model.system-db :as sdb]
            [is.simm.ops.proposals :as props]
            [is.simm.runtimes.branching :as branching]
            [is.simm.runtimes.context :as ctx]
            [clojure.string :as str]
            [datahike.api :as d]
            [taoensso.telemere :as log]))

;; =============================================================================
;; Vocabulary
;; =============================================================================

(def statuses
  "The status vocabulary, in workflow order. Stored as STRINGS on the page (see
   `seed/morphism-Task-status`) and normalised on write here, so the database
   stays permissive while the UI has a closed set to render."
  ["open" "in-progress" "blocked" "done"])

(def default-status "open")

(defn normalize-status
  "An unknown status is kept, not coerced. An agent writing
   \"waiting-on-tenant\" has said something true, and silently rewriting it to
   \"open\" would lose it; the UI groups anything unrecognised with `open`."
  [s]
  (let [s (some-> s name str/trim str/lower-case)]
    (if (str/blank? s) default-status s)))

(defn open?
  "Everything that is not done. `blocked` is still open — it is a task you have,
   not a task you finished."
  [status]
  (not= "done" (normalize-status status)))

;; =============================================================================
;; Source: S/Task pages in a KB
;; =============================================================================

(def ^:private task-page-q
  ;; `:instance/of-role` is cardinality-many, so a task page holds BOTH S/Page
  ;; and S/Task. Matching on S/Task alone is what makes this a type query rather
  ;; than a page scan.
  '[:find [(pull ?e [:entity/uuid :entity/name :entity/updated-at
                     :S.Page/title :S.Page/archived
                     :S.Task/status :S.Task/priority :S.Task/due
                     :S.Task/done-at :S.Task/assignee :S.Task/forkset]) ...]
    :where [?e :instance/of-role [:entity/name "S/Task"]]])

(defn- page->task [scope kb-name p]
  ;; scope-qualified for the same reason as the feed's ids: a page is
  ;; (store, uuid), and this :id is a render key downstream.
  {:id (str "page:" scope "/" (:entity/uuid p))
   :source :page
   :title (or (:S.Page/title p) (:entity/name p) "Untitled task")
   :status (normalize-status (:S.Task/status p))
   :priority (:S.Task/priority p)
   :due (:S.Task/due p)
   :done-at (:S.Task/done-at p)
   :assignee (:S.Task/assignee p)
   :forkset (:S.Task/forkset p)
   :scope scope
   :kb-name kb-name
   :page (:entity/uuid p)
   :ref {:kind :page :scope (str scope) :page (str (:entity/uuid p))
         :title (or (:S.Page/title p) (:entity/name p))}
   :updated-at (:entity/updated-at p)})

(defn kb-tasks
  "Every S/Task page in one KB store. Returns [] when the store declares no
   S/Task type yet (a store `store/install!` has not reached — datahike
   rejects the unresolvable lookup ref rather than returning nothing, and that
   is a schema-install gap worth logging, not a normal empty result)."
  [scope kb-name]
  (ctx/with-server-context
    (if-let [conn (branching/get-kb-conn scope)]
      (try
        (->> (d/q task-page-q @conn)
             (remove :S.Page/archived)
             (mapv #(page->task scope kb-name %)))
        (catch Exception e
          (log/log! {:level :warn :id ::kb-task-query-failed
                     :msg "Could not read tasks from a KB — S/Task type missing?"
                     :data {:scope (str scope) :kb kb-name :error (.getMessage e)}})
          []))
      (do (log/log! {:level :warn :id ::kb-conn-missing
                     :msg "No connection for a KB the party can read"
                     :data {:scope (str scope) :kb kb-name}})
          []))))

;; =============================================================================
;; Source: landable ForkSets
;; =============================================================================

(defn- forkset->task [p tier]
  {:id (str "forkset:" (:proposal/id p))
   :source :forkset
   :title (:proposal/title p)
   ;; A ForkSet has no status of its own beyond open/resolved: it is ready, and
   ;; "doing" it means accepting it. Mapping it to `open` keeps one vocabulary.
   :status default-status
   :priority nil
   :due nil
   :assignee (:proposal/author p)
   :forkset (:proposal/id p)
   :room (:proposal/room p)
   :tier tier
   :auto-mergeable? (fs/auto-mergeable? (:proposal/intent p) tier)
   ;; A ForkSet is reviewed where proposals are reviewed; the ref keeps the row
   ;; from being a dead end until that review moves inline.
   :ref {:kind :proposal :id (str (:proposal/id p)) :title (:proposal/title p)}
   :updated-at (:proposal/created-at p)})

(defn forkset-tasks
  "Open ForkSets that route to Tasks — i.e. the ones that MERGE. A conflicted
   one is a Future and deliberately absent here; see `fs/destination`.

   Tier costs a 3-way compare per fork, cached on branch+trunk heads
   (`props/proposal-review`), so this is one cache lookup per open proposal
   after the first."
  [party]
  (ctx/with-server-context
    (->> (props/visible-proposals party :status :open)
         (keep (fn [p]
                 (let [tier (:tier (props/proposal-review p))]
                   (when (= :tasks (fs/destination (:proposal/intent p) tier))
                     (forkset->task p tier)))))
         vec)))

;; =============================================================================
;; Source: dvergr dispatch rows
;; =============================================================================

(defn- dispatch->task [t]
  {:id (str "dispatch:" (:id t))
   :source :dispatch
   ;; A dispatch has no title, only content — the request itself. First line,
   ;; so a long ask still reads as one row.
   :title (or (some-> (:content t) (str/split-lines) first) "Request")
   :status (case (:status t)
             :completed "done"
             :accepted "in-progress"
             :ignored "done"
             "open")
   :priority nil
   :due nil
   :assignee nil
   :room (:room-id t)
   :ref (when (:room-id t) {:kind :room :room (str (:room-id t))})
   :skill (:skill t)
   :dispatch-id (:id t)
   :updated-at (:created-at t)})

(defn dispatch-tasks
  "dvergr dispatch rows addressed to this party. Read-only: `dvergr.orchestration.tasks`
   owns the lifecycle, and duplicating accept/complete here would give one row
   two writers."
  [party]
  (ctx/with-server-context
    (let [actor-id (keyword "party" (str party))]
      (try
        (let [list-fn (requiring-resolve 'dvergr.orchestration.tasks/list-tasks)]
          (->> (list-fn (sdb/get-conn))
               (filter #(= actor-id (:actor-id %)))
               (mapv dispatch->task)))
        (catch Exception e
          (log/log! {:level :warn :id ::dispatch-read-failed
                     :msg "Could not read dvergr dispatch tasks"
                     :data {:party (str party) :error (.getMessage e)}})
          [])))))

;; =============================================================================
;; The aggregate
;; =============================================================================

(defn- sort-key
  "Due date first (soonest first, undated last), then priority, then recency.
   A task list whose order is not the order you would work in is a list people
   stop reading."
  [{:keys [due priority updated-at]}]
  [(if due (.getTime ^java.util.Date due) Long/MAX_VALUE)
   (case (some-> priority str/lower-case)
     "critical" 0 "high" 1 "medium" 2 "low" 3 4)
   (- (if updated-at (.getTime ^java.util.Date updated-at) 0))])

(defn visible-kbs
  "Every KB this party may READ, through any relation `can?` recognises —
   ownership, an explicit share, or a room grant. Enumerating and filtering
   rather than querying ownership directly is what makes a granted KB's tasks
   visible without a second visibility rule to keep in sync."
  [party]
  (when-let [conn (sdb/get-conn)]
    (->> (d/q '[:find [(pull ?e [:kb/id :kb/name :kb/db-scope]) ...]
                :where [?e :kb/id _]]
              @conn)
         (filter #(access/can? party :read {:kb (:kb/id %)}))
         vec)))

(defn list-tasks
  "Every task this party can see, from all sources, in work order.

   `:include-done?` (default false) — done tasks are history, and a list that
   shows them by default stops being a list of what to do.
   `:sources` — subset of #{:page :forkset :dispatch} to read (default all).
   `:assignee` — a party uuid, or `:me` for this party. Note an UNASSIGNED task
   is included by `:me`: nobody having picked it up is not the same as it not
   being yours, and hiding it is how work disappears."
  [party & {:keys [include-done? sources assignee]
            :or {sources #{:page :forkset :dispatch}}}]
  (when-not party (throw (ex-info "tasks require an authenticated party" {})))
  (let [wanted (fn [k] (contains? sources k))
        page-tasks (when (wanted :page)
                     (mapcat (fn [{:kb/keys [db-scope name]}] (kb-tasks db-scope name))
                             (visible-kbs party)))
        all (concat page-tasks
                    (when (wanted :forkset) (forkset-tasks party))
                    (when (wanted :dispatch) (dispatch-tasks party)))
        who (if (= :me assignee) party assignee)]
    (->> all
         (filter #(or include-done? (open? (:status %))))
         (filter #(or (nil? who) (nil? (:assignee %)) (= who (:assignee %))))
         (sort-by sort-key)
         vec)))

;; =============================================================================
;; Writing S/Task pages
;; =============================================================================

(defn create-task!
  "Create an S/Task page in `scope`. Returns the page uuid.

   The page is tagged S/Page AND S/Task: it is a real wiki page that happens to
   be a task, so every page affordance (blocks, links, backlinks, fulltext, the
   renderer) applies without a second code path."
  [scope {:keys [title status priority due assignee forkset]}]
  {:pre [(string? title) (not (str/blank? title))]}
  (ctx/with-server-context
    (let [conn (or (branching/get-kb-conn scope)
                   (throw (ex-info "no connection for scope" {:scope scope})))
          id (random-uuid)
          now (java.util.Date.)]
      (d/transact conn
        [(cond-> {:entity/uuid id
                  :entity/name title
                  :entity/created-at now
                  :entity/updated-at now
                  :instance/of-role [[:entity/name "S/Page"] [:entity/name "S/Task"]]
                  :S.Page/title title
                  :S.Page/archived false
                  :S.Task/status (normalize-status status)}
           priority (assoc :S.Task/priority priority)
           due      (assoc :S.Task/due due)
           assignee (assoc :S.Task/assignee assignee)
           forkset  (assoc :S.Task/forkset forkset))])
      (log/log! {:level :info :id ::task-created
                 :data {:scope (str scope) :page (str id) :title title}})
      id)))

(defn update-task!
  "Set task properties on an existing S/Task page. Only the keys supplied are
   written. Setting status to \"done\" stamps `:S.Task/done-at`; moving it back
   off done retracts the stamp, so a reopened task does not read as finished."
  [scope page-uuid {:keys [title status priority due assignee] :as attrs}]
  (ctx/with-server-context
    (let [conn (or (branching/get-kb-conn scope)
                   (throw (ex-info "no connection for scope" {:scope scope})))
          ;; `some?` first: a nil `:in` binding is UNBOUND to datahike, and the
          ;; bare query would pick an arbitrary entity to write the task onto
          eid (or (when (some? page-uuid)
                    (d/q '[:find ?e . :in $ ?u :where [?e :entity/uuid ?u]] @conn page-uuid))
                  (throw (ex-info "no such page" {:scope scope :page page-uuid})))
          status (when (contains? attrs :status) (normalize-status status))
          now (java.util.Date.)
          base (cond-> {:db/id eid :entity/updated-at now}
                 title    (assoc :S.Page/title title :entity/name title)
                 status   (assoc :S.Task/status status)
                 priority (assoc :S.Task/priority priority)
                 due      (assoc :S.Task/due due)
                 assignee (assoc :S.Task/assignee assignee)
                 (= "done" status) (assoc :S.Task/done-at now))
          retract (when (and status (not= "done" status)
                             (d/q '[:find ?d . :in $ ?e :where [?e :S.Task/done-at ?d]]
                                  @conn eid))
                    [[:db/retract eid :S.Task/done-at]])]
      (d/transact conn (into [base] retract))
      page-uuid)))
