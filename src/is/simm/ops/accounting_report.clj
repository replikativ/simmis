(ns is.simm.ops.accounting-report
  "The workspace's financial position, assembled from the per-book query.

   Every simmis store carries the kontor kernel (`model.store/install!` —
   that is the point of one store kind), so \"the books\" is not one ledger but
   one per team. This namespace answers the GLOBAL question by running the same
   per-book report over each book the caller can read and labelling the rows,
   never by maintaining a second aggregate.

   That rule is the whole reason a global Accounting view and a per-team cost
   figure are not double work: they are the same query at two scopes. A
   parallel roll-up would be a second source of truth for money, which is the
   worst possible place to have one."
  (:require [is.simm.model.access :as access]
            [is.simm.model.rooms :as rooms]
            [is.simm.model.room-databases :as room-dbs]
            [is.simm.model.system-db :as sdb]
            [is.simm.runtimes.context :as ctx]
            [datahike.api :as d]
            [taoensso.telemere :as log]))

(defn- book-conn
  "The governed content-db conn for a room, or nil when it has none."
  [room-id]
  (when-let [sys (and (some? room-id) (sdb/get-conn))]
    (when-let [scope (d/q '[:find ?s . :in $ ?rid
                            :where [?r :room/id ?rid] [?r :room/content-db-scope ?s]]
                          @sys room-id)]
      (room-dbs/connect-room-database scope))))

(defn- account-rows
  "One row per account with a non-zero balance: {:path :commodity :amount}.

   `trial-balance` takes the CONN, not a db value: the whole report is one
   snapshot resolved internally, so every account is read at the same
   `:as-of-tx` and a write landing mid-report cannot produce a trial balance
   that fails to balance for no visible reason.

   It returns `{account-eid {commodity-eid Money}}`, so the eids are resolved
   here. kontor has a display-keyed `trial-balance-readable` that does exactly
   this, but it is not in the released 0.1.3 — swap to it when kontor ships it,
   rather than growing this. Resolving names against a later `d/db` than the
   report's snapshot is safe for the two attributes read: an account's path and
   a commodity's symbol are its identity, not its balance."
  [conn]
  (let [trial (requiring-resolve 'kontor.reporting.trial/trial-balance)
        balances (trial conn {})
        db (d/db conn)]
    (->> balances
         (mapcat (fn [[account-eid by-commodity]]
                   (let [path (:kontor.account/path
                               (d/pull db [:kontor.account/path] account-eid))]
                     (for [[commodity-eid amount] by-commodity]
                       {:path (str path)
                        :commodity (or (:kontor.commodity/symbol
                                        (d/pull db [:kontor.commodity/symbol] commodity-eid))
                                       "")
                        ;; Money is a RECORD: `str` gives
                        ;; "kontor.money.Money@dce06ac3", which is what the
                        ;; Accounting view displayed where a figure belongs.
                        ;; kontor's `money->str` appends the commodity, but
                        ;; `trial-balance` (unlike `-readable`) carries it as an
                        ;; EID, so that rendered "7350 1227". The BigDecimal
                        ;; keeps the stored scale — 2400.00 stays "2400.00" —
                        ;; and the symbol is resolved into its own column.
                        :amount (str (:amount amount))}))))
         (sort-by (juxt :path :commodity))
         vec)))

(defn- orphan-postings
  "Postings whose account entity has no `:kontor.account/path`.

   These are MONEY THAT IS OFF THE BOOK. No released kontor has ADR-124's
   `assert-refs-resolve!`, so an account ref that fails to resolve is written as
   a new empty entity instead of being refused — and `trial-balance` then never
   sees those postings at all, because it enumerates accounts BY path. The
   amounts are simply gone from every report.

   So this is counted separately and shown. The alternative — which this
   namespace did until a real posting exercised it — is to filter pathless
   accounts out of the rows, which hides the discrepancy in the one view whose
   job is to be trustworthy about money."
  [conn]
  (count (d/q '[:find [?p ...]
                :where [?p :kontor.posting/account ?a]
                [(missing? $ ?a :kontor.account/path)]]
              @conn)))

(defn workspace-position
  "Every book this party can read, with its accounts. Returns
   [{:room uuid :room-name str :accounts [...] :error str?}].

   A book that fails to report keeps its row and carries the reason: a team
   silently missing from a financial summary is worse than a visible failure,
   because the total still looks like a total."
  [party]
  (when-not party (throw (ex-info "accounting requires an authenticated party" {})))
  (ctx/with-server-context
    (->> (rooms/get-party-rooms party)
         (filter #(access/can? party :read {:room (:room/id %)}))
         (mapv (fn [{:room/keys [id name]}]
                 (let [base {:room id :room-name name
                             ;; the heading opens this team's book: its
                             ;; statements, journal and ledgers
                             :ref {:kind :book :room (str id) :title name}}]
                   (try
                     (if-let [conn (book-conn id)]
                       (cond-> (assoc base :accounts (account-rows conn))
                         (pos? (orphan-postings conn))
                         (assoc :orphans (orphan-postings conn)))
                       (assoc base :accounts [] :error "no content database"))
                     (catch Exception e
                       (log/log! {:level :warn :id ::book-report-failed
                                  :msg "A book could not be reported — row kept with its reason"
                                  :data {:room (str id) :error (.getMessage e)}})
                       (assoc base :accounts [] :error (.getMessage e)))))))
         vec)))
