(ns is.simm.uis.web.desktop.views.book
  "One team's book: balance sheet, income statement, trial balance, journal,
   and an account's ledger, over a chosen period and the workspace time cut.

   Every figure is the server's (`is.simm.ops.book-report`, kontor's own
   trial balance and running balance); this namespace arranges and formats.
   Opened from the team's heading on the Accounting perspective."
  (:require [clojure.string :as str]
            [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.book-period :as period]
            [is.simm.uis.web.desktop.money :as money]
            [is.simm.uis.web.desktop.views.core :as vc]
            #?(:cljs [is.simm.uis.web.desktop.signals :as sig])
            #?(:cljs [clojure.core.async :refer [go <! put! promise-chan] :include-macros true])
            #?(:cljs [org.replikativ.spindel.engine.core :as rtc])
            #?(:cljs [is.simm.uis.web.desktop.runtime :refer [runtime]])
            #?(:cljs [is.simm.uis.web.desktop.accounting-remote :as ar])
            #?(:cljs [is.simm.uis.web.desktop.refs :as refs])
            #?(:cljs [is.simm.uis.web.desktop.remote :as rem])
            #?(:cljs [is.simm.runtimes.web :as web]))
  #?(:cljs (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]]
                            [org.replikativ.spindel.incremental.interval]))
  #?(:cljs (:require [org.replikativ.spindel.incremental.interval :as iv]
                     [org.replikativ.spindel.effects.track :refer [track]])))

(def reports
  [[:balance-sheet "Balance sheet"]
   [:income-statement "Income statement"]
   [:journal "Journal"]
   [:ledger "Ledger"]
   [:trial-balance "Trial balance"]])

;; =============================================================================
;; Loading
;; =============================================================================

#?(:cljs
   (defn- remote-result
     "Run spin-remote `s` and deliver `{:ok r}` / `{:err e}` on a channel."
     [s]
     (let [ch (promise-chan)]
       (binding [rtc/*execution-context* runtime]
         (s (fn [r] (put! ch {:ok r})) (fn [e] (put! ch {:err e}))))
       ch)))

#?(:cljs
   (defn- error-text
     "A readable reason: the server's short message and correlation id
      (`remote/error-text`)."
     [err]
     (if (map? err)
       (or (some-> (:data err) :error name) (:cause err) (str err))
       (rem/error-text err))))

#?(:cljs
   (defonce ^:private requested
     ;; key -> opts last asked for, so a render while that fetch is out asks
     ;; for nothing new
     (atom {})))

#?(:cljs
   (defn load-book!
     "Fetch `room-id`'s reports for `opts` into `sig/books`."
     [room-id opts]
     (swap! requested assoc room-id opts)
     (go
       (let [{:keys [ok err]} (<! (remote-result (ar/load-book! web/server-id room-id opts)))]
         (binding [rtc/*execution-context* runtime]
           (swap! sig/books assoc room-id
                  (if err {:error (error-text err) :opts opts} {:data ok :opts opts})))))))

#?(:cljs
   (defn load-ledger!
     "Fetch one account's ledger into `sig/book-ledgers`."
     [room-id path opts]
     (swap! requested assoc [room-id path] opts)
     (go
       (let [{:keys [ok err]} (<! (remote-result
                                   (ar/load-account-ledger! web/server-id room-id path opts)))]
         (binding [rtc/*execution-context* runtime]
           (swap! sig/book-ledgers assoc [room-id path]
                  (if err {:error (error-text err) :opts opts} {:data ok :opts opts})))))))

#?(:cljs
   (defn reload-all!
     "Load every open book again — a posting landed somewhere. The opts each
      was last asked for still apply."
     []
     (doseq [[k opts] @requested]
       (if (vector? k)
         (load-ledger! (first k) (second k) opts)
         (load-book! k opts)))))

#?(:cljs
   (defn- set-ui! [room-id k v]
     (binding [rtc/*execution-context* runtime]
       (swap! sig/book-ui assoc-in [room-id k] v))))

;; =============================================================================
;; Pieces
;; =============================================================================

(defn- fig
  "A figure from the server, formatted (`desktop.money`); blank for nil."
  [s]
  (el/span {:class "book-amount"} (when s (money/format-amount s))))

(defn- account-link
  "An account path that opens its ledger."
  [on-account path label]
  (el/span {:class (str "book-account" (when on-account " book-account--link"))
            :on-click (fn [_] (when on-account (on-account path)))}
    (or label path)))

(defn- badge [ok? ok-text bad-text]
  (el/span {:class (str "book-badge " (if ok? "book-badge--ok" "book-badge--bad"))}
    (if ok? ok-text bad-text)))

(defn- section-block
  "A statement section: heading, one line per account, total."
  [title {:keys [lines total]} on-account]
  (el/div {:class "book-section"}
    (el/div {:class "book-section-head"} title)
    (if (empty? lines)
      (el/div {:class "book-line book-line--none"} (el/span {} "None"))
      (for [{:keys [path name amount type]} lines]
        (el/div {:key path :class "book-line"}
          (if (= :result type)
            (el/span {:class "book-account book-account--derived"
                      :title "Income less expenses not yet closed into equity"}
              path)
            (account-link on-account path (if (and name (not= name path))
                                            (str path " · " name)
                                            path)))
          (fig amount))))
    (el/div {:class "book-line book-line--total"}
      (el/span {} (str "Total " (str/lower-case title)))
      (fig total))))

(defn- per-commodity
  "Render one block per commodity, headed by the commodity when there are
   several."
  [items render]
  (el/div {:class "book-statements"}
    (for [{:keys [commodity] :as item} items]
      (el/div {:key (str commodity) :class "book-statement"}
        (when (> (count items) 1)
          (el/div {:class "book-commodity"} commodity))
        (render item)))))

(defn- empty-note [text]
  (el/div {:class "acct-empty"} text))

;; =============================================================================
;; Reports
;; =============================================================================

(defn- balance-sheet-view [items on-account]
  (if (empty? items)
    (empty-note "No balances at this date.")
    (per-commodity
     items
     (fn [{:keys [commodity assets liabilities equity liabilities-and-equity balanced?]}]
       (el/div {}
         (el/div {:class "book-statement-meta"}
           (badge balanced? "Assets = liabilities + equity" "Does not balance")
           (el/span {:class "book-commodity-note"} commodity))
         (el/div {:class "book-columns"}
           (el/div {:class "book-column"}
             (section-block "Assets" assets on-account))
           (el/div {:class "book-column"}
             (section-block "Liabilities" liabilities on-account)
             (section-block "Equity" equity on-account)
             (el/div {:class "book-line book-line--grand"}
               (el/span {} "Total liabilities and equity")
               (fig liabilities-and-equity)))))))))

(defn- income-statement-view [items on-account]
  (if (empty? items)
    (empty-note "No income or expenses in this period.")
    (per-commodity
     items
     (fn [{:keys [commodity income expenses net]}]
       (el/div {:class "book-narrow"}
         (el/div {:class "book-statement-meta"}
           (el/span {:class "book-commodity-note"} commodity))
         (section-block "Income" income on-account)
         (section-block "Expenses" expenses on-account)
         (el/div {:class "book-line book-line--grand"}
           (el/span {} (if (str/starts-with? (str net) "-") "Net loss" "Net result"))
           (fig net)))))))

(defn- trial-balance-view [items on-account]
  (if (empty? items)
    (empty-note "No balances at this date.")
    (per-commodity
     items
     (fn [{:keys [commodity lines debits credits balanced?]}]
       (el/div {:class "book-narrow"}
         (el/div {:class "book-statement-meta"}
           (badge balanced? "Debits = credits" "Debits and credits differ")
           (el/span {:class "book-commodity-note"} commodity))
         (el/table {:class "book-table"}
           (el/thead {}
             (el/tr {}
               (el/th {} "Account")
               (el/th {:class "num"} "Debit")
               (el/th {:class "num"} "Credit")))
           (el/tbody {}
             (for [{:keys [path debit credit]} lines]
               (el/tr {:key path}
                 (el/td {} (account-link on-account path nil))
                 (el/td {:class "num"} (fig debit))
                 (el/td {:class "num"} (fig credit))))
             (el/tr {:class "book-table-total"}
               (el/td {} "Total")
               (el/td {:class "num"} (fig debits))
               (el/td {:class "num"} (fig credits))))))))))

(defn- journal-view [{:keys [entries truncated?]} on-account on-run]
  (if (empty? entries)
    (empty-note "No entries in this period.")
    (el/div {:class "book-journal"}
      (when truncated?
        (el/div {:class "book-note"} "Showing the 500 most recent entries in this period."))
      (for [{:keys [id date narration state journal postings run]} entries]
        (el/div {:key (str id) :class "book-entry"}
          (el/div {:class "book-entry-head"}
            (el/span {:class "book-entry-date"} (period/format-day date))
            (el/span {:class "book-entry-narration"} (or narration "—"))
            (when journal (el/span {:class "book-chip"} journal))
            (when (not= :posted state)
              (el/span {:class "book-chip book-chip--state"} (name state)))
            (if run
              (el/span {:class "book-entry-by book-account--link"
                        :title "Open the agent run that posted this entry"
                        :on-click (fn [e] (when on-run (on-run e run)))}
                (vc/icon "orbit")
                (str "Posted by " (or (:actor run) "an agent")))
              (el/span {:class "book-entry-by book-entry-by--unknown"
                        :title "No single agent call was running when this entry was sealed"}
                "Posted outside an agent run")))
          (el/table {:class "book-table book-table--entry"}
            (el/tbody {}
              (for [[i {:keys [path amount commodity]}] (map-indexed vector postings)]
                (let [credit? (str/starts-with? (str amount) "-")]
                  (el/tr {:key (str i)}
                    (el/td {:class (when credit? "book-credit-account")}
                      (account-link on-account path nil))
                    (el/td {:class "num"} (when-not credit? (money/format-amount amount)))
                    (el/td {:class "num"} (when credit? (money/format-amount (subs (str amount) 1))))
                    (el/td {:class "book-commodity-cell"} commodity)))))))))))

(defn- ledger-view [accounts account ledger on-account]
  (el/div {:class "book-narrow"}
    (el/div {:class "book-ledger-pick"}
      (el/select {:class "property-select"
                  :value (or account "")
                  :on-change (fn [e] #?(:cljs (let [v (.. e -target -value)]
                                                (when (seq v) (on-account v)))
                                        :clj nil))}
        (el/option {:value ""} "Choose an account…")
        (for [{:keys [path type]} accounts]
          (el/option {:key path :value path} (str path "  (" (name type) ")")))))
    (let [{:keys [data error]} ledger]
      (cond
        (nil? account) (empty-note "Pick an account, or click any account name in another report.")
        error (el/div {:class "proposal-error"} (str "Couldn't load the ledger — " error))
        (nil? data) (empty-note "Loading…")
        (empty? (:rows data)) (empty-note "No postings on this account up to this date.")
        :else
        (el/table {:class "book-table"}
          (el/thead {}
            (el/tr {}
              (el/th {} "Date")
              (el/th {} "Entry")
              (el/th {:class "num"} "Amount")
              (el/th {:class "num"} "Balance")
              (el/th {} "")))
          (el/tbody {}
            (for [[i {:keys [date narration amount running commodity]}] (map-indexed vector (:rows data))]
              (el/tr {:key (str i)}
                (el/td {:class "book-date-cell"} (period/format-day date))
                (el/td {} (or narration "—"))
                (el/td {:class "num"} (fig amount))
                (el/td {:class "num"} (fig running))
                (el/td {:class "book-commodity-cell"} commodity)))))))))

;; =============================================================================
;; The tab
;; =============================================================================

(defn view
  "Pure render. `book` is `{:data :error}` as loaded, `ui` the tab's reading
   position, `opts` the period + cut the tab is asking for."
  [{:keys [room-name book ledger ui opts cut? on-ui on-account on-run on-back]}]
  (let [{:keys [data error]} book
        report (or (:report ui) :balance-sheet)
        preset (or (:preset ui) period/default-preset)
        fresh? (= opts (:opts book))]
    (el/div {:class "run-history book-view"}
      (el/header {:class "run-history-header"}
        (el/button {:class "run-history-back" :title "Back to Accounting"
                    :on-click (fn [_] (when on-back (on-back)))}
          (vc/icon "arrow-left"))
        (el/div {:class "run-history-heading"}
          (el/div {:class "run-history-kicker"} (vc/icon "wallet") (or room-name "Team"))
          (el/h2 {} "Book")
          (el/p {}
            (str (if (:from opts)
                   (str (period/format-day (:from opts)) " – ")
                   "Since the first entry – ")
                 (period/format-day (:through opts))
                 (when cut? " · as recorded at the timeline cut")))))
      (el/div {:class "book-toolbar"}
        (el/div {:class "book-tabs"}
          (for [[k label] reports]
            (el/button {:key (name k)
                        :class (str "book-tab" (when (= k report) " book-tab--active"))
                        :on-click (fn [_] (on-ui :report k))}
              label)))
        (el/div {:class "book-tabs book-periods"}
          (for [[k label] period/presets]
            (el/button {:key (name k)
                        :class (str "book-tab" (when (= k preset) " book-tab--active"))
                        :on-click (fn [_] (on-ui :preset k))}
              label))))
      (el/main {:class "run-history-body book-body"}
        (cond
          error (el/div {:class "proposal-error"} (str "Couldn't load the book — " error))
          (or (nil? data) (not fresh?)) (empty-note "Loading…")
          :else
          (case report
            :income-statement (income-statement-view (:income-statement data) on-account)
            :journal (journal-view (:journal data) on-account on-run)
            :ledger (ledger-view (:accounts data) (:account ui) ledger on-account)
            :trial-balance (trial-balance-view (:trial-balance data) on-account)
            (balance-sheet-view (:balance-sheet data) on-account)))))))

(defn book-tab
  "The Book tab of a team. Loads for the selected period and the workspace
   time cut, and again whenever either changes."
  [{:keys [room-id room-name db-scope]}]
  #?(:cljs
     (spin
       (let [books (iv/get-new (track sig/books))
             ledgers (iv/get-new (track sig/book-ledgers))
             ui-all (iv/get-new (track sig/book-ui))
             gref (iv/get-new (track sig/global-ref))
             ui (get ui-all room-id)
             cut-ms (some-> (:as-of gref) (.getTime))
             {:keys [from through]} (period/period (or (:preset ui) period/default-preset)
                                                   (or cut-ms (js/Date.now)))
             ;; `through` is a whole DAY (`period/period`), so opts stay equal
             ;; between renders and match what was loaded
             opts (cond-> {:through through}
                    from (assoc :from from)
                    cut-ms (assoc :as-of-tx cut-ms))
             account (:account ui)
             _ (when (not= opts (get @requested room-id))
                 (load-book! room-id opts))
             _ (when (and account (= :ledger (:report ui))
                          (not= opts (get @requested [room-id account])))
                 (load-ledger! room-id account opts))]
         (view {:room-name room-name
                :book (get books room-id)
                :ledger (when account
                          (let [l (get ledgers [room-id account])]
                            (when (= opts (:opts l)) l)))
                :ui ui
                :opts opts
                :cut? (some? cut-ms)
                :on-ui (fn [k v] (set-ui! room-id k v))
                :on-account (fn [path]
                              (binding [rtc/*execution-context* runtime]
                                (swap! sig/book-ui update room-id assoc
                                       :report :ledger :account path)))
                :on-run (fn [e run]
                          ;; beside the book, not over it: the reader is
                          ;; checking one entry and comes back to the rest
                          (let [new-column? (or (.-metaKey e) (.-ctrlKey e))]
                            (refs/open! {:kind :run :room room-id :room-name room-name
                                         :run (:id run)
                                         :title (str "Run · " (or (:actor run) "agent"))}
                                        {:new-column? new-column?
                                         :new-tab? (not new-column?)})))
                :on-back (fn [] (sig/open-or-activate-tab! :accounting nil {:title "Accounting"}))})))
     :clj (view {:room-name room-name :ui {} :opts {} :on-ui (fn [_ _])})))
