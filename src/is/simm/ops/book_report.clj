(ns is.simm.ops.book-report
  "One team's book, reported: balance sheet, income statement, trial balance,
   journal, and an account's ledger.

   Every figure comes from kontor's own reporting — `trial-balance` for
   balances, `running-balance` for a ledger — the same query
   `ops.accounting-report` runs for the workspace index, at a finer grain. The
   statements are arrangements of those balances, never a second computation
   of them: a balance sheet that disagrees with the trial balance beside it is
   the failure a report like this exists to rule out.

   The room books use a PATH chart typed by account type (Assets:Bank :asset,
   …), not kontor's code-based national charts, so the statements group by
   `:kontor.account/type` rather than by `financial-statements` line codes,
   which select accounts by code prefix and would match nothing here.

   Signs. kontor stores debits positive and credits negative. Statements show
   each section in its NATURAL sign — liabilities, equity and income are
   credit-normal, so they are negated for display — and the trial balance
   shows the raw split into debit and credit columns.

   Time. `:through` is the reporting date (inclusive, whole day, UTC — the
   book's effective dates are UTC midnights); `:from` opens the income
   statement's period. `:as-of-tx` is the workspace time cut: the book as it
   was RECORDED at that instant, so the timeline slider rewinds these reports
   with everything else."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [datahike.api :as d]
            [is.simm.model.access :as access]
            [is.simm.model.accounting-sandbox :as acct-sandbox]
            [is.simm.model.parties :as parties]
            [is.simm.model.rooms :as rooms]
            [is.simm.runtimes.context :as ctx]))

;; =============================================================================
;; Time
;; =============================================================================

(def ^:private day-ms (* 24 60 60 1000))

(defn- end-of-day
  "The last millisecond of `d`'s UTC day — `:through` is inclusive, and an
   entry effective ON the reporting date belongs in it."
  ^java.util.Date [^java.util.Date d]
  (java.util.Date. (dec (+ (* day-ms (quot (.getTime d) day-ms)) day-ms))))

(defn- just-before
  "The instant before `d`'s UTC day starts: the opening balance of a period
   that begins on `d`."
  ^java.util.Date [^java.util.Date d]
  (java.util.Date. (dec (* day-ms (quot (.getTime d) day-ms)))))

(defn- ms [^java.util.Date d] (some-> d .getTime))

;; =============================================================================
;; Balances — kontor's trial balance, resolved to rows
;; =============================================================================

(defn- balance-rows
  "kontor's trial balance at `as-of-valid` / `as-of-tx` as
   `[{:path :name :type :commodity :amount BigDecimal}]`, non-zero rows only.
   `trial-balance` returns eids; account and commodity identity is resolved
   here against the same cut."
  [conn as-of-valid as-of-tx]
  (let [trial (requiring-resolve 'kontor.reporting.trial/trial-balance)
        balances (trial conn (cond-> {}
                               as-of-valid (assoc :as-of-valid as-of-valid)
                               as-of-tx (assoc :as-of-tx as-of-tx)))
        db (cond-> (d/db conn) as-of-tx (d/as-of as-of-tx))]
    (->> balances
         (mapcat (fn [[account-eid by-commodity]]
                   (let [{:kontor.account/keys [path name type]}
                         (d/pull db [:kontor.account/path :kontor.account/name
                                     :kontor.account/type] account-eid)]
                     (for [[commodity-eid money] by-commodity]
                       {:path (str path)
                        :name name
                        :type type
                        :commodity (or (:kontor.commodity/symbol
                                        (d/pull db [:kontor.commodity/symbol] commodity-eid))
                                       "")
                        :amount (:amount money)}))))
         (remove #(zero? (.signum ^BigDecimal (:amount %))))
         (sort-by (juxt :commodity :path))
         vec)))

;; =============================================================================
;; Statements — pure arrangements of balance rows
;; =============================================================================

(def ^:private credit-normal #{:liability :equity :income})

(defn- natural
  "A balance in its type's natural sign: credit-normal types negated."
  [{:keys [type amount]}]
  (if (credit-normal type) (.negate ^BigDecimal amount) amount))

(defn- total [rows] (reduce (fn [^BigDecimal a ^BigDecimal b] (.add a b)) 0M rows))

(defn- section [rows type]
  (let [lines (->> rows
                   (filter #(= type (:type %)))
                   (mapv #(assoc % :amount (natural %))))]
    {:type type :lines lines :total (total (map :amount lines))}))

(defn balance-sheet
  "Assets against liabilities and equity, per commodity, from balance rows at
   the reporting date. Income and expense accounts are not closed into equity
   in these books, so their net — the result not yet closed — is shown as its
   own equity line; without it a sheet of correct balances would not balance.
   `:balanced?` is checked, not assumed."
  [rows]
  (->> (group-by :commodity rows)
       (sort-by key)
       (mapv (fn [[commodity rows]]
               (let [assets (section rows :asset)
                     liabilities (section rows :liability)
                     equity (section rows :equity)
                     result (.negate ^BigDecimal
                                     (total (map :amount (filter #(#{:income :expense} (:type %))
                                                                 rows))))
                     equity (cond-> equity
                              (not (zero? (.signum ^BigDecimal result)))
                              (-> (update :lines conj {:path "Result not yet closed"
                                                       :type :result
                                                       :amount result})
                                  (update :total #(.add ^BigDecimal % result))))
                     claims (.add ^BigDecimal (:total liabilities) ^BigDecimal (:total equity))]
                 {:commodity commodity
                  :assets assets
                  :liabilities liabilities
                  :equity equity
                  :liabilities-and-equity claims
                  :balanced? (zero? (.compareTo ^BigDecimal (:total assets) claims))})))))

(defn- movement
  "Per [path commodity], closing minus opening — what happened IN the period."
  [opening closing]
  (let [k (juxt :path :commodity)
        open (into {} (map (juxt k :amount)) opening)]
    (->> closing
         (concat (map #(assoc % :amount 0M) (remove (comp (set (map k closing)) k) opening)))
         (map (fn [r] (assoc r :amount (.subtract ^BigDecimal (:amount r)
                                                  ^BigDecimal (get open (k r) 0M)))))
         (remove #(zero? (.signum ^BigDecimal (:amount %)))))))

(defn income-statement
  "Income less expenses over the period, per commodity: the movement of every
   income and expense account between the opening and closing balance rows."
  [opening closing]
  (let [pl? #(#{:income :expense} (:type %))]
    (->> (movement (filter pl? opening) (filter pl? closing))
         (group-by :commodity)
         (sort-by key)
         (mapv (fn [[commodity rows]]
                 (let [income (section rows :income)
                       expenses (section rows :expense)]
                   {:commodity commodity
                    :income income
                    :expenses expenses
                    :net (.subtract ^BigDecimal (:total income) ^BigDecimal (:total expenses))}))))))

(defn trial-balance
  "Every account with a balance, split into debit and credit columns, with the
   per-commodity totals and whether they agree."
  [rows]
  (->> (group-by :commodity rows)
       (sort-by key)
       (mapv (fn [[commodity rows]]
               (let [lines (mapv (fn [{:keys [^BigDecimal amount] :as r}]
                                   (assoc r
                                          :debit (when (pos? (.signum amount)) amount)
                                          :credit (when (neg? (.signum amount)) (.negate amount))))
                                 rows)
                     debits (total (keep :debit lines))
                     credits (total (keep :credit lines))]
                 {:commodity commodity :lines lines
                  :debits debits :credits credits
                  :balanced? (zero? (.compareTo ^BigDecimal debits credits))})))))

;; =============================================================================
;; Journal — transactions, with the agent run that posted each
;; =============================================================================

(defn- booking-calls
  "The `clojure_eval` calls in this room that called a `kontor/` verb, as
   `{:run-id :start :end}`. Agents post through the sandbox, and dvergr records
   every call — run, code, start and end — in the SAME store as the book."
  [db]
  (->> (d/q '[:find ?run ?start ?end ?input
              :where
              [?c :tool-call/name "clojure_eval"]
              [?c :tool-call/run-id ?run]
              [?c :tool-call/started-at ?start]
              [?c :tool-call/ended-at ?end]
              [?c :tool-call/input ?input]]
            db)
       (filter (fn [[_ _ _ input]] (str/includes? (str input) "kontor/")))
       (mapv (fn [[run start end _]] {:run-id run :start start :end end}))))

(defn posted-by
  "The run that posted a transaction sealed at `posted-at`: the booking call
   whose execution CONTAINS that instant. kontor's posting API carries no
   transaction metadata, so authorship is not written with the entry; it is
   recovered from the call record. Exactly one containing call names the run;
   none (a human or a script posted it) or several (concurrent booking calls)
   name nobody rather than guessing."
  [calls ^java.util.Date posted-at]
  (when posted-at
    (let [t (.getTime posted-at)
          hits (filter (fn [{:keys [^java.util.Date start ^java.util.Date end]}]
                         (<= (.getTime start) t (.getTime end)))
                       calls)]
      (when (= 1 (count hits))
        (:run-id (first hits))))))

(defn- actor-name [db run-id]
  (when-let [actor (d/q '[:find ?a . :in $ ?r :where [?e :run/id ?r] [?e :run/actor ?a]]
                        db run-id)]
    (or (some-> (parties/actor-id->party-id actor) parties/get-party :party/display-name)
        (name actor))))

(def ^:private journal-limit 500)

(defn- journal
  "Transactions effective in [from, through], newest first, each with its
   postings and the run that posted it. Every state is listed, with the
   state: a draft that a statement leaves out should still be visible
   somewhere."
  [db from through]
  (let [calls (booking-calls db)
        in-period? (fn [^java.util.Date ed]
                     (and (or (nil? from) (>= (.getTime ed) (.getTime (just-before from))))
                          (<= (.getTime ed) (.getTime (end-of-day through)))))
        txs (->> (d/q '[:find ?t ?ed :where [?t :kontor.transaction/effective-date ?ed]] db)
                 (filter (comp in-period? second))
                 (sort-by (fn [[t ed]] [(- (.getTime ^java.util.Date ed)) (- t)])))]
    {:truncated? (> (count txs) journal-limit)
     :entries
     (->> (take journal-limit txs)
          (mapv (fn [[t _]]
                  (let [e (d/pull db [:kontor.transaction/effective-date
                                      :kontor.transaction/posted-at
                                      :kontor.transaction/narration
                                      :kontor.transaction/state
                                      {:kontor.transaction/journal [:kontor.journal/code]}
                                      {:kontor.posting/_transaction
                                       [:kontor.posting/amount
                                        {:kontor.posting/account [:kontor.account/path]}
                                        {:kontor.posting/commodity [:kontor.commodity/symbol]}]}]
                                  t)
                        run (posted-by calls (:kontor.transaction/posted-at e))]
                    {:id t
                     :date (ms (:kontor.transaction/effective-date e))
                     :posted-at (ms (:kontor.transaction/posted-at e))
                     :narration (:kontor.transaction/narration e)
                     :state (:kontor.transaction/state e)
                     :journal (get-in e [:kontor.transaction/journal :kontor.journal/code])
                     :postings (->> (:kontor.posting/_transaction e)
                                    (mapv (fn [p]
                                            {:path (get-in p [:kontor.posting/account :kontor.account/path])
                                             :commodity (get-in p [:kontor.posting/commodity :kontor.commodity/symbol])
                                             :amount (:kontor.posting/amount p)}))
                                    ;; debits first, the way an entry is read
                                    (sort-by (juxt #(- (.signum ^BigDecimal (:amount %))) :path))
                                    vec)
                     :run (when run {:id (str run) :actor (actor-name db run)})}))))}))

;; =============================================================================
;; Wire shape
;; =============================================================================

(defn- wire
  "BigDecimals as their digit strings (the view formats them — `desktop.money`),
   so the scale the book stored survives the trip."
  [x]
  (walk/postwalk #(if (decimal? %) (.toPlainString ^BigDecimal %) %) x))

;; =============================================================================
;; Entry points
;; =============================================================================

(defn- authorized-conn [party room-id]
  (when-not party (throw (ex-info "accounting requires an authenticated party" {})))
  (when-not (access/can? party :read {:room room-id})
    (throw (ex-info "not authorized to read this book" {:error :not-authorized})))
  (or (acct-sandbox/room-book-conn room-id)
      (throw (ex-info "this team has no book" {:error :no-book}))))

(defn book-report
  "Every report over one team's book for the period `from`–`through`
   (`from` nil = since the first entry; `through` defaults to today), at time
   cut `as-of-tx` (nil = now)."
  [party room-id {:keys [from through as-of-tx]}]
  (ctx/with-server-context
    (let [conn (authorized-conn party room-id)
          through (or through (java.util.Date.))
          closing (balance-rows conn (end-of-day through) as-of-tx)
          opening (if from (balance-rows conn (just-before from) as-of-tx) [])
          db (cond-> (d/db conn) as-of-tx (d/as-of as-of-tx))]
      (wire
       {:room (str room-id)
        :room-name (:room/name (rooms/get-room room-id))
        :from (ms from) :through (ms through) :as-of-tx (ms as-of-tx)
        :balance-sheet (balance-sheet closing)
        :income-statement (income-statement opening closing)
        :trial-balance (trial-balance closing)
        :journal (journal db from through)
        :accounts (->> (d/q '[:find ?p ?t :where [?a :kontor.account/path ?p]
                              [?a :kontor.account/type ?t]] db)
                       (map (fn [[p t]] {:path p :type t}))
                       (sort-by :path)
                       vec)}))))

(defn account-ledger
  "One account's postings up to `through`, each with the running balance —
   kontor's `running-balance`, per commodity."
  [party room-id path {:keys [through as-of-tx]}]
  (ctx/with-server-context
    (let [conn (authorized-conn party room-id)
          db (cond-> (d/db conn) as-of-tx (d/as-of as-of-tx))
          eid (or (d/q '[:find ?a . :in $ ?p :where [?a :kontor.account/path ?p]] db path)
                  (throw (ex-info "no such account" {:error :no-account :path path})))
          running (requiring-resolve 'kontor.reporting.ledger/running-balance)
          sym (fn [c] (:kontor.commodity/symbol (d/pull db [:kontor.commodity/symbol] c)))]
      (wire
       {:path path
        :rows (->> (running conn eid (cond-> {:as-of-valid (end-of-day (or through (java.util.Date.)))}
                                       as-of-tx (assoc :as-of-tx as-of-tx)))
                   (mapv (fn [{:keys [transaction valid-from amount running narration commodity]}]
                           {:transaction transaction
                            :date (ms valid-from)
                            :narration narration
                            :commodity (sym commodity)
                            :amount (:amount amount)
                            :running (:amount running)})))}))))
