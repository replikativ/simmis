(ns is.simm.uis.web.desktop.timeline-source
  "Where the timeline rail's past comes from: the client's OWN replicas.

   The obvious source is the commit graph, and it is not available here. A
   commit record is stored under its commit-id, and datahike's konserve-sync
   walk-fn does not emit that key — it ships index nodes, the branch list, and
   each branch's head. So a freshly connected client holds ZERO historical
   commit records, and only sees commits made while its tab is open. On top of
   that yggdrasil's `Graphable` protocol is synchronous (`k/get … :sync? true`)
   against a client store whose IndexedDB backend asserts on sync ops, and
   yggdrasil is not in the CLJS bundle at all. Reaching it would mean changes to
   two upstream repos and a new browser dependency.

   None of which is needed, because the replicas are `:keep-history? true` and
   datahike commits 1:1 with transactions: every transaction's `:db/txInstant`
   datom is already in the local index. Querying THAT gives the same timeline,
   locally, live, with no round trip — and it updates as the replica syncs
   rather than needing an invalidation, which is what made the old RPC version
   read `No history loaded yet` and then never change.

   Content transactions only. Installing a store's schema and seed costs a few
   hundred transactions in a two-minute burst (measured: 228 of one KB's 476),
   and a history in which half the entries are `installed the schema` is worse
   than no history — it buries the writes a person actually made and makes
   scrubbing to `this morning` land in boot noise."
  (:require [datahike.api :as d]
            [org.replikativ.spindel.engine.core :as rtc]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.db-signal :as db-sig])
  (:require-macros [org.replikativ.spindel.signal :refer [signal]]))

;; --- the clock --------------------------------------------------------------

(def now-tick
  "Signal carrying the current time, so the rail's `now` is actually now.

   The axis positions everything RELATIVE to the present — a commit's distance
   from the right edge is its age, and schedules sit right of now by how long
   until they fire. Without a clock those positions are computed once and then
   quietly rot: leave the tab open and `in 20 minutes` keeps claiming twenty
   minutes forever.

   This is a timer, not a `setTimeout` papering over a race (which this codebase
   forbids, rightly). There is no event to await here — the passage of time is
   the input, and a clock is how you read it. A minute is finer than the axis
   can render at any realistic span."
  (signal runtime (js/Date.)))

(defonce ^:private clock
  (js/setInterval (fn []
                    (binding [rtc/*execution-context* runtime]
                      (reset! now-tick (js/Date.))))
                  60000))

(def ^:private content-tx-q
  "Transactions that touched a block's text or a page's title.

   Deliberately narrow, and it under-reports: a transaction that only reordered
   blocks or only set a property does not appear. That is the right direction to
   be wrong in for a rail whose job is `when did this change` — a missing dot
   costs a scrub target, an extra dot for every schema assertion costs the whole
   view."
  '[:find ?tx ?inst
    :where
    (or [_ :block/content _ ?tx]
        [_ :S.Page/title _ ?tx])
    [?tx :db/txInstant ?inst]])

(defn scope-commits
  "One replica's content transactions as rail rows, newest first.

   Returns [] rather than throwing on a db that cannot answer — a KB still
   materialising its indices is a normal state during boot, not an error, and
   the rail simply has fewer dots until it settles."
  [db scope-str scope-name]
  (if (nil? db)
    []
    (try
      (->> (d/q content-tx-q db)
           (mapv (fn [[tx inst]]
                   {:id (str scope-str ":" tx)
                    :tx tx
                    :ts inst
                    :ms (.getTime inst)
                    :scope scope-str
                    :scope-name scope-name}))
           (sort-by :ms >)
           vec)
      (catch :default e
        (js/console.warn "[timeline] scope query failed" scope-str e)
        []))))

;; --- what changed between a cut and now ------------------------------------

(def ^:private changed-blocks-q
  '[:find ?b ?inst ?tx
    :in $ ?cut
    :where
    [?b :block/content _ ?tx]
    [?tx :db/txInstant ?inst]
    [(> ?inst ?cut)]])

(def ^:private tx-authors-q
  "tx → the party uuid (a string) that caused it, for the transactions that say.

   A separate query joined in Clojure rather than a `get-else` inside
   `changed-blocks-q`, because `:tx/author` is OPTIONAL — nothing
   written before the attribute existed has one, and no interactive write path
   sets it yet — so joining it as a pattern would silently drop every row it
   cannot answer, turning `who changed this` into `this did not change`. One
   query per replica, the same shape as `page-resolver`."
  '[:find ?tx ?author
    :where [?tx :tx/author ?author]])

(def ^:private page-kinds-q
  "page eid → `:S.Page/kind`, for the pages that have one.

   A separate query rather than a `get-else` in the walk, for the same reason
   `tx-authors-q` is separate: the attribute is OPTIONAL — an ordinary wiki page
   has no kind — and joining it as a pattern would silently drop every page that
   is not a record."
  '[:find ?e ?kind :where [?e :S.Page/kind ?kind]])

(defn- page-resolver
  "block eid → page title, following `:block/parent` upward. nil for a block
   that belongs to no page, and nil for one whose page is workspace BOOTSTRAP.

   One hop is not enough and the shortfall is silent: of one KB's 147 blocks,
   127 sit directly under their page and 20 do not. Building the parent map once
   and walking it costs 12 ms for the whole KB, which is cheaper than being
   wrong about 14% of an AUDIT.

   BOOTSTRAP pages (`:S.Page/kind :bootstrap` — `model.seed/bootstrap-page-uuids`)
   are excluded because they are not anybody's edit. Every simmis store is
   installed with `SKILL` and `Getting Started` in it, so a wiki created this
   afternoon reported 21 blocks of change against a cut this morning: the audit
   panel accounting for the act of provisioning rather than for the work. That
   is the same reason the pathless category-S blocks below are dropped.

   The test is the PAGE's kind, from the store, not its title. A title filter
   would hide a page a person happened to call `SKILL`, and would miss the
   bootstrap the moment a store is seeded in another language."
  [db]
  (let [parents (into {} (d/q '[:find ?b ?p :where [?b :block/parent ?p]] db))
        kinds (into {} (d/q page-kinds-q db))
        pages (into {} (d/q '[:find ?e ?t :where [?e :S.Page/title ?t]] db))]
    (fn [b]
      (loop [e b, hops 0]
        (cond
          (nil? e) nil
          (contains? pages e) (when-not (= :bootstrap (get kinds e)) (get pages e))
          (> hops 32) nil
          :else (recur (get parents e) (inc hops)))))))

(defn changes-since
  "What changed between `cut-ms` and now, grouped by page, newest first.

   `[{:key :scope-name :page :n :last-ms :ts :author}]`. This is the AUDIT half
   of the Timelines view — the same question the proposal diff answers for a
   fork (`how does this state differ from the present?`) asked in the other
   direction.

   `:author` is the party uuid string off the NEWEST transaction in the group,
   the same one `:ts` comes from, or nil where the writer left no `:tx/author`.
   A page a person and an agent both edited therefore reads as whoever touched
   it last, which is the honest summary of a row that already collapses n edits
   into one line — the per-edit attribution is the page's own history, not this.

   Blocks that resolve to no page are dropped, and that is deliberate rather
   than lossy: they belong to a bootstrap page (see `page-resolver`), which is
   the store's own installation and not anybody's edit. Dropping real content
   would be a lie in a view whose whole job is accounting for change.

   This also used to catch `:fmap/*` UI-functor scaffolding — empty blocks the
   seed transacted into every store to feed a resolver that never once ran. The
   functor is gone; a store built after that has none. A store built BEFORE it
   still carries them, and they still land here."
  [scope-names cut-ms]
  (binding [rtc/*execution-context* runtime]
    (let [cut (js/Date. cut-ms)]
      (->> (keys @db-sig/kb-heads)
           (mapcat
            (fn [s]
              (if-let [db (some-> (db-sig/kb-db-signal s) deref)]
                (try
                  (let [->page (page-resolver db)
                        authors (into {} (d/q tx-authors-q db))]
                    (->> (d/q changed-blocks-q db cut)
                         (keep (fn [[b inst tx]]
                                 (when-let [t (->page b)]
                                   ;; Keep the block eid: the row shows a COUNT,
                                   ;; but expanding it has to diff the actual
                                   ;; blocks, and re-deriving them later would
                                   ;; mean running this query twice.
                                   {:page t :block b :ms (.getTime inst) :ts inst
                                    :author (get authors tx)
                                    :scope s :scope-name (get scope-names s s)})))
                         (group-by (juxt :scope :page))
                         (mapv (fn [[[scope page] hits]]
                                 (let [newest (apply max (map :ms hits))
                                       last-hit (first (filter #(= newest (:ms %)) hits))]
                                   {:key (str scope "/" page)
                                    :page page
                                    :scope scope
                                    :blocks (mapv :block hits)
                                    :scope-name (:scope-name (first hits))
                                    :n (count hits)
                                    :last-ms newest
                                    :ts (:ts last-hit)
                                    :author (:author last-hit)})))))
                  (catch :default e
                    (js/console.warn "[timeline] changes-since failed" s e)
                    []))
                [])))
           (sort-by :last-ms >)
           vec))))

(defn page-block-ops
  "Block-level ops for ONE changed page, for the audit panel's expansion:
   `[{:op :block/add|:block/edit|:block/remove :before {:content …} :after {…}}]`.

   The row above it says `3 blocks`; this says WHAT those blocks now say and
   what they said at the cut. Same shape the proposal card renders
   (`vc/block-op-view`), so a change reads identically whichever direction you
   approached it from.

   Both sides come from the client's own replica — the live db for `after`,
   `(d/as-of db cut)` for `before` — so this costs no round trip. A block with
   no `before` was added since the cut; one with no `after` was removed.
   Ordering follows the live db's `:block/order` where it has one, so the diff
   reads down the page rather than in query order.

   Returns nil (not an empty vector) when the scope has no replica or the
   as-of view cannot be taken, so the caller can say `could not reconstruct`
   rather than silently showing `no changes`."
  [scope block-eids cut-ms]
  (binding [rtc/*execution-context* runtime]
    (when-let [db (some-> (db-sig/kb-db-signal scope) deref)]
      (try
        (let [as-of (d/as-of db (js/Date. cut-ms))
              pull-block (fn [d e]
                           (try
                             (let [ent (d/entity d e)]
                               (when-let [c (:block/content ent)]
                                 {:content c :order (:block/order ent)}))
                             (catch :default _ nil)))]
          (->> block-eids
               distinct
               (keep (fn [e]
                       (let [before (pull-block as-of e)
                             after  (pull-block db e)]
                         (cond
                           (and before after (not= (:content before) (:content after)))
                           {:op :block/edit :before before :after after :order (:order after)}
                           (and after (not before))
                           {:op :block/add :after after :order (:order after)}
                           (and before (not after))
                           {:op :block/remove :before before :order (:order before)}
                           ;; identical content: the block was touched by a
                           ;; transaction that did not change what it says
                           ;; (a re-assert). Nothing to show.
                           :else nil))))
               (sort-by :order)
               vec))
        (catch :default e
          ;; as-of before the store was installed throws on lookup refs
          ;; (roadmap #33) — report it rather than rendering an empty diff.
          (js/console.warn "[timeline] page-block-ops failed" (str scope) e)
          nil)))))

(defn all-commits
  "Every connected replica's content transactions, merged, newest first.

   `scope-names` maps scope-str → display name; a scope missing from it renders
   under its raw uuid rather than being dropped, so an unnamed system is visibly
   present instead of silently absent.

   Reads the per-KB signals by plain DEREF, not `track`. The caller must already
   be tracking `db-sig/kb-heads`, which is written after every per-KB signal
   update — see that signal's docstring for why the pair is the way to observe N
   replicas from one spin."
  [scope-names]
  (binding [rtc/*execution-context* runtime]
    (let [scopes (keys @db-sig/kb-heads)]
      (->> scopes
           (mapcat (fn [s]
                     (let [db (some-> (db-sig/kb-db-signal s) deref)]
                       (scope-commits db s (get scope-names s s)))))
           (sort-by :ms >)
           vec))))
