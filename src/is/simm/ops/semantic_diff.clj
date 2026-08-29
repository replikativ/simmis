(ns is.simm.ops.semantic-diff
  "Lift yggdrasil/datahike datom diffs to DOMAIN ops for content-native
   rendering (doc/proposals-and-time-travel.md §7): KB changes as
   page-anchored block ops, drives as file ops, books as double-entry
   transactions, everything else as datom counts. A proposal is only reviewable
   to the extent its forks render as the artifact they are — a fork that shows
   `{:entities-touched 14}` is a number a human cannot accept or refuse.
   Server-side only — full datoms never cross the wire; the
   returned shape is transit-safe (maps/strings/uuids/insts/keywords).

   Direction: `(diff base branch)` = what the BRANCH changed since it forked.
   `:added` eids resolve in the branch db, `:removed` eids in the db the LEFT
   side came from — the base when one resolves, trunk otherwise. Falling back
   to trunk still finds the right ENTITY (base and trunk share one lineage, and
   datahike does not reuse eids) but reads it at the wrong TIME, so a block the
   branch deleted and trunk then edited shows trunk's newer text as `before`.
   That is a different failure from the eid COLLISION below, which is between
   two branches; both end in one entity's content shown as another's.

   Against trunk's CURRENT head instead of the fork point, everything trunk did
   while the proposal sat open reads as the branch having removed it, so an
   untouched proposal grows phantom deletions the longer it waits for review.

   IDENTITY, NOT EIDS. A datom's `e` is only meaningful in the db it came
   from: branch and trunk keep allocating entity ids from the same counter
   after they diverge, so the same number denotes DIFFERENT entities on each
   side. Comparing raw eids across the two dbs therefore fuses unrelated
   entities — measured 2026-07-25: eid 1212 was the page \"Onboarding
   Checklist\" on the branch and an unrelated block on trunk, which turned a
   `:page/create` into a `:page/update` and would have rendered one entity's
   content as another's before/after. Every eid is resolved to its entity's
   durable identity (`:entity/uuid`, or `:fs.node/id` on drives) before
   anything is compared, and back to a per-db eid only to read it."
  (:require [is.simm.runtimes.branching :as branching]
            [yggdrasil.protocols :as yp]
            [datahike.api :as d]
            [clojure.string :as str]))

(def ^:private identity-attrs
  "Durable, cross-branch identity per system: KB entities carry
   `:entity/uuid`, drive nodes `:fs.node/id` (`:db.unique/identity`)."
  [:entity/uuid :fs.node/id])

(defn- identity-attrs-for
  "`identity-attrs` narrowed to those this store actually declares. Pulling an
   undeclared attribute does not return nil — datahike REJECTS it — so a KB
   store (no `:fs.node/id`) would throw on the drive attr and vice versa."
  [db]
  (filterv #(contains? (:schema db) %) identity-attrs))

(defn- identity-index
  "{identity → eid} for `eids` as they exist in `db`. Entities with no durable
   identity (schema datoms, datahike internals) are dropped — they are never
   pages, blocks or files. One pull per entity, so this costs a linear pass
   rather than a query each."
  [db eids]
  (let [attrs (identity-attrs-for db)
        pattern (vec attrs)]
    (if (empty? attrs)
      {}
      (persistent!
       (reduce (fn [acc e]
                 (let [ent (d/pull db pattern e)]
                   (if-let [id (some #(get ent %) attrs)] (assoc! acc id e) acc)))
               (transient {}) eids)))))

(defn- anchor-page
  "Walk :block/parent upward to the containing page; returns
   {:page-uuid u :title t} or nil."
  [db eid]
  (loop [e eid, hops 0]
    (when (and e (< hops 32))
      (let [ent (d/pull db [:entity/uuid :S.Page/title
                            {:block/parent [:db/id]}] e)]
        (if-let [t (:S.Page/title ent)]
          {:page-uuid (:entity/uuid ent) :title t}
          (recur (get-in ent [:block/parent :db/id]) (inc hops)))))))

(defn- entity-kind [db eid]
  (let [ent (d/entity db eid)]
    (cond
      (:S.Page/title ent)  :page
      (:block/parent ent)  :block
      (:fs.node/id ent)    :file
      (:message/id ent)    :message
      :else                :other)))

(defn- block-view [db eid]
  (let [ent (d/pull db [:entity/uuid :block/content :block/order] eid)]
    {:block-uuid (:entity/uuid ent)
     :order (:block/order ent)
     :content (:block/content ent)}))

(defn- file-view [db eid]
  (let [ent (d/pull db [:fs.node/id :fs.node/name :fs.node/kind
                        :fs.node/mime :fs.node/size] eid)]
    {:node-id (:fs.node/id ent) :name (:fs.node/name ent)
     :kind (:fs.node/kind ent) :mime (:fs.node/mime ent)
     :size (:fs.node/size ent)}))

(defn semantic-kb-diff
  "Page-grouped block/page ops for what `branch` changes on `scope`.
   Returns {:pages [{:page-uuid :title :ops [...]}] :counts … :base … :baseless? …}.

   Diffed against the MERGE BASE — the trunk head at fork time — not against
   trunk's current head. Against the head, anything trunk did while the proposal
   sat open reads as the BRANCH having removed it, so an untouched proposal
   sprouts phantom deletions the longer it waits. `base-commit` is recorded when
   the branch is minted; `common-ancestor` is the fallback for proposals filed
   before that, and when neither resolves we say so (`:baseless?`) rather than
   quietly showing a trunk-head diff as if it were a real one."
  [scope branch & {:keys [base-commit]}]
  (let [base (or base-commit (branching/merge-base-id scope branch))
        base-diff (branching/kb-diff-from scope base branch)
        diff (or base-diff (branching/kb-diff scope :db branch))
        baseless? (nil? base-diff)
        bdb  @(branching/get-kb-conn-on-branch scope branch)
        ;; The `:removed` side belongs to whichever db the diff's LEFT side was:
        ;; the base when we have one, trunk otherwise. Reading base-era eids out
        ;; of the TRUNK db finds the right entity — datahike does not reuse eids
        ;; — but at the wrong TIME: if trunk edited a block the branch deleted,
        ;; `before` renders trunk's newer text as the words being removed.
        tdb  (or (when-not baseless? (branching/kb-db-at-commit scope base))
                 @(branching/get-kb-conn scope))
        ;; datoms are [:db/add e a v] — entity is SECOND. Each side's eids are
        ;; only resolvable in the db they came from (see the ns docstring), so
        ;; both are immediately mapped to durable identities.
        b-idx (identity-index bdb (map second (:added diff)))
        t-idx (identity-index tdb (map second (:removed diff)))
        added-ids   (set (keys b-idx))     ; identities the branch asserted on
        removed-ids (set (keys t-idx))     ; identities whose trunk datoms the branch dropped
        touched (into added-ids removed-ids)
        ops
        (for [id touched
              :let [be (get b-idx id)      ; this entity's eid ON THE BRANCH
                    te (get t-idx id)      ; …and ON TRUNK — different numbers
                    in-branch? (some? be)
                    in-trunk?  (some? te)
                    kind (if in-branch? (entity-kind bdb be) (entity-kind tdb te))]
              :when (#{:page :block} kind)
              :let [op
                    (case kind
                      :block
                      (cond
                        (and in-branch? in-trunk?)
                        {:op :block/edit
                         :before (block-view tdb te)
                         :after  (block-view bdb be)
                         :anchor (anchor-page bdb be)}
                        in-branch?
                        {:op :block/add :after (block-view bdb be)
                         :anchor (anchor-page bdb be)}
                        :else
                        {:op :block/remove :before (block-view tdb te)
                         :anchor (anchor-page tdb te)})
                      :page
                      (cond
                        (and in-branch? (not in-trunk?))
                        {:op :page/create
                         :anchor {:page-uuid (:entity/uuid (d/entity bdb be))
                                  :title (:S.Page/title (d/entity bdb be))}}
                        (and in-trunk? (not in-branch?))
                        {:op :page/remove
                         :anchor {:page-uuid (:entity/uuid (d/entity tdb te))
                                  :title (:S.Page/title (d/entity tdb te))}}
                        :else
                        {:op :page/update
                         :anchor {:page-uuid (:entity/uuid (d/entity bdb be))
                                  :title (:S.Page/title (d/entity bdb be))}}))]
              :when (:anchor op)]
          op)
        pages (->> ops
                   (group-by :anchor)
                   (mapv (fn [[anchor os]]
                           (assoc anchor :ops (mapv #(dissoc % :anchor) os)))))]
    {:scope scope :system-type :kb
     :pages pages
     :counts (:summary diff)
     :base (some-> base str)
     ;; surfaced so the card can say the diff is against trunk's moving head
     ;; rather than the fork point — a reviewer should know when the comparison
     ;; is the weaker one
     :baseless? baseless?}))

(defn semantic-fs-diff
  "File ops for what `branch` changes on a drive `scope`, against the fork point
   for the same reason as the KB diff — otherwise a file trunk added while the
   proposal was open reads as the branch having deleted it."
  [scope branch & {:keys [base-commit]}]
  (let [base (or base-commit (branching/merge-base-id scope branch))
        base-diff (branching/kb-diff-from scope base branch)
        diff (or base-diff (branching/kb-diff scope :db branch))
        baseless? (nil? base-diff)
        bdb  @(branching/get-kb-conn-on-branch scope branch)
        tdb  (or (when-not baseless? (branching/kb-db-at-commit scope base))
                 @(branching/get-kb-conn scope))
        ;; identity-keyed for the same reason as the KB diff — a drive node's
        ;; eid diverges across branches, `:fs.node/id` does not.
        b-idx (identity-index bdb (map second (:added diff)))
        t-idx (identity-index tdb (map second (:removed diff)))
        files
        (for [id (into (set (keys b-idx)) (keys t-idx))
              :let [be (get b-idx id) te (get t-idx id)
                    kind (if be (entity-kind bdb be) (entity-kind tdb te))]
              :when (= :file kind)]
          (cond
            (and be te) {:op :file/modify
                         :before (file-view tdb te)
                         :after (file-view bdb be)}
            be          {:op :file/add :after (file-view bdb be)}
            :else       {:op :file/remove :before (file-view tdb te)}))]
    {:scope scope :system-type :fs
     :files (vec files)
     :counts (:summary diff)
     :base (some-> base str)
     :baseless? baseless?}))

;; =============================================================================
;; :book — proposed postings as double entry
;; =============================================================================

(defn- posting-attr? [a]
  (and (keyword? a) (= "kontor.posting" (namespace a))))

(defn- transaction-attr? [a]
  (and (keyword? a) (= "kontor.transaction" (namespace a))))

(defn- exact-amount
  "`:kontor.posting/amount` as an EXACT decimal string, padded to the
   commodity's declared precision.

   It is a `:db.type/bigdec`, and it must survive both transit to the browser
   and being read by a human deciding whether to accept money movements. Going
   through a double would round (0.1 + 0.2 is the canonical case, and a cent is
   a real error in a ledger), and a raw BigDecimal is not a transit-safe value —
   the whole diff shape is deliberately maps/strings/uuids/insts/keywords.
   `toPlainString` also avoids scientific notation, which `str` will produce for
   a large or heavily-scaled decimal.

   kontor stores whatever SCALE the poster supplied — `:amount 1250` is stored
   as `1250`, `:amount 12.50M` as `12.50` — so a column of raw values mixes
   `1250` with `12.50`. Padding to `:kontor.commodity/precision` is exact by
   construction because it is applied ONLY when it adds trailing zeros: a value
   already carrying more digits than the commodity declares is printed as
   stored, never rounded to fit the display."
  [amount precision]
  (when (some? amount)
    (let [d (bigdec amount)]
      (.toPlainString
       ^java.math.BigDecimal
       (if (and (nat-int? precision) (<= (.scale d) (int precision)))
         (.setScale d (int precision) java.math.RoundingMode/UNNECESSARY)
         d)))))

(defn- book-entry-view
  "One kontor transaction as human-readable double entry.

   `?p` is BOUND in the query even though it is unused in the result: `:find`
   has SET semantics, so two legs with the same account, amount and commodity —
   an ordinary split — would collapse into one row and the entry would render as
   not balancing. kontor's own reader binds it for the same reason."
  [db tx-eid]
  (let [t (d/pull db [:kontor.transaction/narration
                      :kontor.transaction/effective-date
                      :kontor.transaction/state
                      {:kontor.transaction/journal [:kontor.journal/code]}]
                  tx-eid)
        rows (d/q '[:find ?p ?path ?amt ?sym ?prec :in $ ?tx :where
                    [?p :kontor.posting/transaction ?tx]
                    [?p :kontor.posting/account ?a] [?a :kontor.account/path ?path]
                    [?p :kontor.posting/amount ?amt]
                    [?p :kontor.posting/commodity ?c] [?c :kontor.commodity/symbol ?sym]
                    ;; `get-else`, not a plain clause: precision is optional and
                    ;; a missing one must not drop the posting from the entry
                    [(get-else $ ?c :kontor.commodity/precision -1) ?prec]]
                  db tx-eid)
        lines (->> rows
                   (sort-by first)                      ; stable order: posting eid
                   (mapv (fn [[_ path amt sym prec]]
                           ;; kontor's sign convention: positive = debit,
                           ;; negative = credit. The side carries the sign so
                           ;; the column shows a plain positive amount, which is
                           ;; how a ledger is read. Negated with `-` rather than
                           ;; `abs`, which is not defined over BigDecimal.
                           (let [d (bigdec amt)]
                             {:account path
                              :side (if (neg? d) :credit :debit)
                              :amount (exact-amount (if (neg? d) (- d) d) prec)
                              :commodity sym}))))
        ;; Per commodity, because a transaction may carry more than one and each
        ;; must sum to zero on its own (kontor.governance/balance-violations).
        sums (reduce (fn [m [_ _ amt sym _]] (update m sym (fnil + 0M) (bigdec amt)))
                     {} rows)]
    {:narration (:kontor.transaction/narration t)
     :effective-date (:kontor.transaction/effective-date t)
     :journal (get-in t [:kontor.transaction/journal :kontor.journal/code])
     :state (:kontor.transaction/state t)
     :lines lines
     :balanced? (every? zero? (vals sums))}))

(defn semantic-book-diff
  "Postings a book fork proposes, grouped into the transactions they belong to.

   NOT identity-keyed like the KB and drive diffs, and it cannot be: kontor
   transactions and postings carry no `:db.unique/identity` attribute (accounts,
   journals and commodities do — `:kontor.account/path` and friends — which is
   why merging a fork resolves its refs to the existing chart instead of
   duplicating it). So entities are read out of the BRANCH db by their branch
   eids, which is sound here because the branch's eids are the only ones
   involved.

   ADDITIONS ONLY, and that is not a simplification. `kontor.governance` rejects
   any retraction of a row that was `:kontor.posting/posted-at` in `db-before`,
   in the writer, mandatorily — so a fork CANNOT delete a posted entry. What it
   proposes is always new entries; a correction is a reversing entry, which is
   itself new postings. The removed count is still reported in `:counts`."
  [scope branch & {:keys [base-commit]}]
  (let [base (or base-commit (branching/merge-base-id scope branch))
        base-diff (branching/kb-diff-from scope base branch)
        diff (or base-diff (branching/kb-diff scope :db branch))
        baseless? (nil? base-diff)
        bdb @(branching/get-kb-conn-on-branch scope branch)
        ;; datoms are [:db/add e a v] — entity SECOND, attribute THIRD.
        ;; A posting reaches its transaction through the ref; a transaction
        ;; whose only new datoms are its own (narration edited, no new legs)
        ;; is picked up directly.
        tx-eids (into #{}
                      (keep (fn [d]
                              (let [e (nth d 1) a (nth d 2)]
                                (cond
                                  (posting-attr? a)
                                  (d/q '[:find ?t . :in $ ?p :where
                                         [?p :kontor.posting/transaction ?t]]
                                       bdb e)
                                  (transaction-attr? a) e
                                  :else nil))))
                      (:added diff))
        entries (mapv #(book-entry-view bdb %) (sort tx-eids))]
    {:scope scope :system-type :book
     :entries entries
     ;; the reviewer's headline question about a book change, answered before
     ;; they open it: kontor re-validates on merge, so an unbalanced entry could
     ;; not have been committed to the branch either — this says so rather than
     ;; asking them to add up the columns
     :balanced? (every? :balanced? entries)
     :counts (:summary diff)
     :base (some-> base str)
     :baseless? baseless?}))

;; =============================================================================
;; :repo — proposed code as a unified patch
;; =============================================================================

(def ^:private max-patch-chars
  "Ceiling on the unified patch a proposal card carries to the browser.

   The diff is rendered server-side and shipped whole, so a fork that rewrites a
   vendored directory would otherwise push megabytes of text through the
   websocket into a review card nobody scrolls. The `:files` list and `:stat` are
   never truncated — a reviewer still sees EVERY path that changed and its line
   counts, and `:patch-truncated?` says the body was cut, so this degrades to a
   smaller review rather than a quietly incomplete one."
  200000)

(defn semantic-repo-diff
  "What a code fork changes, as the file list + unified patch geschichte itself
   produces.

   Unlike the datom-backed diffs above there is no identity-vs-eid hazard here:
   a repository is keyed by PATH, and a path means the same thing on both
   branches by construction. The fork point still matters for the same reason —
   diffing against trunk's current head would report everything trunk committed
   while the proposal sat open as deletions by the branch."
  [scope branch & {:keys [base-commit]}]
  (let [trunk (branching/repo-trunk scope)
        base (or base-commit
                 (some-> (branching/get-repo-system scope)
                         (yp/common-ancestor branch trunk)))
        baseless? (nil? base)
        ;; With no resolvable fork point, diff from trunk and SAY so, rather
        ;; than silently showing trunk's own advance as the fork's deletions.
        diff (branching/repo-diff scope (or base trunk) branch)
        patch (or (:patch diff) "")
        long? (> (count patch) max-patch-chars)]
    {:scope scope :system-type :repo
     :files (vec (:files diff))
     :stat (:stat diff)
     :patch (if long? (subs patch 0 max-patch-chars) patch)
     :patch-truncated? long?
     :counts (:summary diff)
     :base (some-> base str)
     :baseless? baseless?}))

(defn semantic-diff
  "Dispatch on system-type keyword (:kb :fs :book :repo; :room and unknown fall
   back to counts). `base-commit` is the fork point when the caller knows it."
  [scope branch system-type & {:keys [base-commit]}]
  (case system-type
    :kb (semantic-kb-diff scope branch :base-commit base-commit)
    :fs (semantic-fs-diff scope branch :base-commit base-commit)
    :book (semantic-book-diff scope branch :base-commit base-commit)
    :repo (semantic-repo-diff scope branch :base-commit base-commit)
    :world (let [delta (branching/delta scope :world branch {:patch? false})]
             (cond-> {:scope scope :system-type :world
                      :counts (:summary delta)}
               (:files delta) (assoc :files (vec (:files delta))
                                     :stat (:stat delta))))
    {:scope scope :system-type system-type
     :counts (:summary (branching/kb-diff scope :db branch))}))

(defn proposal-diff
  "Semantic diff for every fork of a proposal (ops/proposals row), each against
   the fork point recorded on its `:proposal.fork/base-commit`."
  [proposal]
  (mapv (fn [f]
          (semantic-diff (:proposal.fork/scope f)
                         (keyword (:proposal.fork/branch f))
                         (or (:proposal.fork/system-type f) :kb)
                         :base-commit (:proposal.fork/base-commit f)))
        (:proposal/forks proposal)))
