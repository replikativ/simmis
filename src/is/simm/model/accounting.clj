(ns is.simm.model.accounting
  "Reflect the kontor accounting kernel into a room's category S, and govern the
   room database so book invariants hold in the writer.

   `ensure-accounting-schema!` is the accounting sibling of `seed/ensure-seed-data!`:
   enabling accounting on a KB/room database (1) installs kontor's kernel schema
   (`:kontor.*`), (2) projects kontor's core types into category S as simmis
   Objects whose properties are backed DIRECTLY by the real `:kontor.*` attributes
   (namespace-preserving projection — no parallel `:S.*` storage, zero query-time
   indirection), and (3) registers `kontor.governance/validate-report` as the
   store's transaction predicate so every book-affecting write is validated
   post-resolution in the writer. Non-accounting wiki/property writes pass through
   untouched (the governor is content-gated on postings/sealing/invariants).

   Idempotent — safe on every boot / KB connect (projection skips existing types
   and morphisms; kontor schema install and `govern!` are idempotent)."
  (:require [datahike.api :as d]
            [dvergr.agent.attempt.governance :as attempt-governance]
            [is.simm.model.katzen-projection :as kp]
            [kontor.core :as kcore]
            [kontor.governance :as gov]
            [taoensso.telemere :as log]))

;; kontor attribute-namespace → the simmis type (Object) it surfaces as. Person
;; identifies onto simmis's existing S/Person (facet composition — kontor person
;; attrs become additional properties on the party you already unified).
(def ^:private ledger-objects
  {"kontor.commodity"   :Commodity
   "kontor.account"     :Account
   "kontor.journal"     :Journal
   "kontor.transaction" :Transaction
   "kontor.posting"     :Posting})

(defn- vt->attr-type
  "A kontor attribute's datahike value type → the katzen attr-type S records.

   Two entries used to lie, and category S repeated the lie to anything that
   read it:

     :db.type/bigdec → :Number   — every money column described as a Long.
     :db.type/ref    → :Identity — an Identity is a STRING attr-type, so every
                                   kontor reference (account, transaction,
                                   ledger, commodity, partner, lot, entity)
                                   was described as a string, collapsing the
                                   Hom/Attr distinction katzen's formalism is
                                   built on — inside a projection whose
                                   docstring claims to be LOSSLESS on the
                                   categorically load-bearing facts.

   Neither corrupted data: `crud/create-morphism!` sees the attribute already
   installed and reuses it, discarding the derived schema. They corrupted the
   METADATA — and `views/types.cljc` routes an editor from exactly that
   metadata, so the day anything carries `:instance/of-role \"S/Posting\"`,
   money gets a number box and a reference gets a text box.

   `:Ref` is deliberately not a Hom. A Hom needs a target object, and
   datahike's schema does not record what a `:db.type/ref` points AT — so the
   honest answer is \"a reference, target unknown\", which at least renders as
   a relation rather than as text."
  [vt]
  (get {:db.type/bigdec  :BigDec
        :db.type/bigint  :BigInt
        :db.type/symbol  :Symbol
        :db.type/instant :Instant
        :db.type/string  :String
        :db.type/keyword :Keyword
        :db.type/long    :Long
        :db.type/double  :Double
        :db.type/boolean :Boolean
        :db.type/uuid    :UUID
        :db.type/ref     :Ref}
       vt :String))

(defn- reflect-ns->schema
  "Build a katzen schema-map (one Attr per kontor attribute) for `obj` from
   kontor's INSTALLED schema under `attr-ns`."
  [db attr-ns obj]
  (let [sch   (d/schema db)
        attrs (sort (filter #(= attr-ns (namespace %)) (keys sch)))]
    {:objects [obj]
     :attrs (vec (for [a attrs :let [vt (get-in sch [a :db/valueType])
                                     card (get-in sch [a :db/cardinality])]]
                   {:name (keyword (name a)) :dom obj :codom (vt->attr-type vt)
                    :cardinality (if (= card :db.cardinality/many) :many :one)}))}))

(defn accounting-installed?
  "Has Kontor's full accounting kernel been installed on this database?

   `:kontor.fx-rate/rate` is outside `kontor.schema/resource`, the deliberately
   small posting spine Dvergr installs for conserved Run authority. The former
   `:kontor.posting/amount` sentinel belongs to both schemas, so a fresh Room's
   resource kernel was mistaken for a complete book: category-S projections,
   the starter chart, and the default commodity were silently skipped."
  [conn]
  (some? (d/q '[:find ?e . :where [?e :db/ident :kontor.fx-rate/rate]] @conn)))

(def starter-book
  "The minimum that makes a book USABLE rather than merely installed.

   `kontor/install-schema!` sets up the kernel — schema, invariants, a primary
   ledger — but seeds no commodity and no journals, so every posting verb fails
   on an empty book. An agent handed `kontor/entry!` and friends could describe
   the ledger in detail and post nothing to it.

   `kontor.book`'s verbs resolve a journal BY TYPE, so ONE journal per type is
   what makes each verb reachable: sell! -> :sale, buy! -> :purchase,
   receive!/pay! -> :cash, transfer!/adjust! -> :general.

   Exactly one `:cash` journal, not the textbook CR/CD receipts-and-disbursements
   pair. Seeding both made `receive-payment!` and `pay-bill!` fail outright on
   kontor 0.1.9 — `2 journals of type :cash — ambiguous; pass :journal
   explicitly` — so the split this seed used to claim made every verb reachable
   in fact made two of them unusable. A book that wants the split adds the
   second journal and names it explicitly; the starter's job is that the verbs
   work.

   USD is a starting default, not a claim about the tenant's currency — a book
   that needs another commodity adds it; this one exists so the verbs work.

   ACCOUNTS are the half this was missing, and without them the claim above was
   still false. `kontor.book/entry!` refuses an account ref that does not
   resolve — deliberately (ADR-124: an unresolved account slot was read as a
   tempid and the money went nowhere), and kontor ships no default chart. So a
   book with journals and no accounts still rejects every posting, which is
   exactly what three seeded rooms did: zero accounts, zero entries.

   Six accounts, one per type plus the two settlement sides, in PTA notation.
   Deliberately minimal — a real chart is a tenant's decision and a fifty-line
   default would be a claim about their business. These are the slots the verbs
   name: sell! wants a receivable and an income account, buy! a payable and an
   expense one, receive!/pay! a bank account."
  [{:kontor.commodity/symbol "USD"
    :kontor.commodity/name "US Dollar"
    :kontor.commodity/precision 2}
   {:kontor.journal/code "SAL" :kontor.journal/type :sale}
   {:kontor.journal/code "PUR" :kontor.journal/type :purchase}
   {:kontor.journal/code "CSH" :kontor.journal/type :cash}
   {:kontor.journal/code "GEN" :kontor.journal/type :general}
   {:kontor.account/path "Assets:Bank"
    :kontor.account/name "Bank" :kontor.account/type :asset}
   {:kontor.account/path "Assets:Receivable"
    :kontor.account/name "Accounts receivable" :kontor.account/type :asset}
   {:kontor.account/path "Liabilities:Payable"
    :kontor.account/name "Accounts payable" :kontor.account/type :liability}
   {:kontor.account/path "Equity:Opening"
    :kontor.account/name "Opening balances" :kontor.account/type :equity}
   {:kontor.account/path "Income:Sales"
    :kontor.account/name "Sales" :kontor.account/type :income}
   {:kontor.account/path "Expenses:General"
    :kontor.account/name "General expenses" :kontor.account/type :expense}])

(defn install-accounting-schema!
  "Install kontor accounting on `conn`'s database: the kernel schema, the core
   ledger types reflected into category S (namespace-preserving), and kontor's
   person facets folded onto S/Person. Does NOT govern — see `govern-store!`.

   Expensive: `kcore/install-schema!` runs eleven sub-installers plus seed
   transactions unconditionally, measured at ~8s on a fresh crypto-hashed file
   store and ~3.8s even when everything is already present. Call it once per
   database, not once per connect."
  [conn]
  ;; 1. kontor kernel schema (:kontor.*)
  (kcore/install-schema! conn)
  ;; 2. reflect the core ledger types as new S Objects backed by :kontor.*
  (doseq [[attr-ns obj] ledger-objects]
    (kp/project-schema! conn (reflect-ns->schema @conn attr-ns obj)
                        {:storage-ns {obj attr-ns}}))
  ;; 3. fold kontor's person facets onto the EXISTING S/Person (if present)
  (when (d/q '[:find ?e . :where [?e :entity/name "S/Person"]] @conn)
    (kp/project-schema! conn (reflect-ns->schema @conn "kontor.person" :KontorPerson)
                        {:storage-ns {:KontorPerson "kontor.person"}
                         :identify {:KontorPerson "S/Person"}}))
  ;; 4. make the book usable, not just installed — see `starter-book`
  (d/transact conn starter-book)
  conn)

(defn govern-store!
  "Register kontor's writer-side book governance on `conn`'s store.

   Must run on EVERY connect: `kontor.governance/govern!` registers a datahike
   transaction predicate keyed by store-id in PROCESS-LOCAL state, so a restart
   (or any new process touching this store) leaves the store ungoverned until
   this runs. Cheap — a registration, not a transaction."
  [conn]
  (gov/govern! conn)
  ;; Datahike currently has one mandatory predicate slot per store. Kontor's
  ;; idempotent registration therefore replaces Dvergr's composed Attempt
  ;; predicate when Simmis extends an already-provisioned Room. Recompose it
  ;; last: certified Attempt immutability and accounting/resource invariants
  ;; must both survive regardless of subsystem installation order.
  (attempt-governance/govern! conn)
  conn)

(defn ensure-book-usable!
  "Give an already-installed book whatever `starter-book` has since gained.

   Narrower than a general seed migration on purpose. The note on
   `ensure-accounting-schema!` is right that re-running installers per connect
   is no substitute for a schema version marker — but these are two indexed
   queries, and the alternative is that a book created before the starter grew
   silently rejects postings forever.

   ACCOUNTS, guarded on absence of a NON-RESOURCE chart rather than on each
   path, so a tenant who deleted or renamed the starter chart does not get it
   resurrected. Dvergr's resource kernel creates source, sink, and wallet rows
   with ordinary `:kontor.account/path` values; counting those as a tenant chart
   skipped every starter account and left all posting verbs unusable.

   JOURNALS, guarded per TYPE, because that is how `kontor.book` resolves them:
   `receive!`/`pay!` want a `:cash` journal and fail with `Nothing found for
   entity id [:kontor.journal/code \"CSH\"]` when the book has none. Measured
   2026-07-27 seeding Tröskel's book: four entries posted and the two cash ones
   did not, because the room predated the `:cash` journal being added to
   `starter-book` and the accounts-only migration never backfilled it. Per type
   rather than per code so a tenant's own `RCT` journal counts as their cash
   journal and does not get a duplicate alongside it — which would reintroduce
   the ambiguity the block below exists to clear."
  [conn]
  (when (accounting-installed? conn)
    (when (nil? (d/q '[:find ?a . :where
                        [?a :kontor.account/path _]
                        (not [?a :kontor.resource-account/id _])]
                      @conn))
      (let [accounts (filterv :kontor.account/path starter-book)]
        (d/transact conn accounts)
        (log/log! {:level :info :id ::starter-accounts-seeded
                   :msg "Seeded the starter chart into an existing book"
                   :data {:accounts (count accounts)}})))
    (let [have (set (d/q '[:find [?t ...] :where [?j :kontor.journal/type ?t]] @conn))
          missing (filterv (fn [{t :kontor.journal/type}]
                             (and t (not (contains? have t))))
                           starter-book)]
      (when (seq missing)
        (d/transact conn missing)
        (log/log! {:level :info :id ::starter-journals-seeded
                   :msg "Backfilled starter journals whose type the book was missing"
                   :data {:types (mapv :kontor.journal/type missing)}})))
    ;; Books seeded with the CR/CD pair cannot take a payment at all — the cash
    ;; verbs resolve by type and refuse an ambiguous match. Drop the extras,
    ;; but ONLY ones no transaction references: a journal with history is part
    ;; of the record, and the fix for that book is to name the journal
    ;; explicitly rather than to rewrite its past.
    (let [cash (vec (d/q '[:find [?j ...] :where [?j :kontor.journal/type :cash]] @conn))]
      (when (> (count cash) 1)
        (let [unused (filterv (fn [j]
                                (nil? (d/q '[:find ?t . :in $ ?j
                                             :where [?t :kontor.transaction/journal ?j]]
                                           @conn j)))
                              cash)
              ;; keep at least one
              droppable (if (= (count unused) (count cash)) (rest unused) unused)]
          (when (seq droppable)
            (d/transact conn (mapv (fn [j] [:db/retractEntity j]) droppable))
            (log/log! {:level :info :id ::ambiguous-cash-journal-dropped
                       :msg "Removed unused duplicate :cash journals so the cash verbs resolve"
                       :data {:dropped (count droppable)}}))))))
  conn)

(defn ensure-accounting-schema!
  "Enable kontor accounting on `conn`'s database. Installs the schema when it is
   absent, and governs the store every time.

   The split matters on the room-hydration path. Governance is process-local so
   it has to be re-registered on every connect, but the schema install is a
   multi-second write-heavy operation that only needs to happen once per
   database. Doing both on every connect cost ~4s per room per boot for work
   that was already done.

   NOTE: an existing database does not pick up NEW kontor seeds after a kontor
   upgrade, because the sentinel only asks whether the kernel schema is present
   at all. Migrating an installed book across kontor versions wants a schema
   version marker in kontor itself; re-running every installer on every connect
   is not a substitute for one."
  [conn]
  (when-not (accounting-installed? conn)
    (install-accounting-schema! conn))
  (ensure-book-usable! conn)
  (govern-store! conn)
  conn)
