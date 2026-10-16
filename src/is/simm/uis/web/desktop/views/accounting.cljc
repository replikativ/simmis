(ns is.simm.uis.web.desktop.views.accounting
  "Accounting — the workspace's position, one book per team.

   Every simmis store carries the kontor kernel, so there is no single ledger:
   each team keeps its own book and this view is the index over them. The
   figures come from the SAME per-book trial balance a team page would show,
   never a roll-up maintained alongside — a second source of truth for money is
   the worst place to have one (see `is.simm.ops.accounting-report`).

   A book that cannot be read keeps its row and states why. A team missing from
   a financial summary is worse than a visible failure, because the total still
   looks like a total."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.money :as money]
            [is.simm.uis.web.desktop.signals :as sig]
            #?(:cljs [clojure.core.async :refer [go <! put! promise-chan] :include-macros true])
            #?(:cljs [org.replikativ.spindel.engine.core :as rtc])
            #?(:cljs [is.simm.uis.web.desktop.runtime :refer [runtime]])
            #?(:cljs [is.simm.uis.web.desktop.accounting-remote :as ar])
            #?(:cljs [is.simm.uis.web.desktop.refs :as refs])
            #?(:cljs [is.simm.uis.web.desktop.aggregate :as agg])
            #?(:cljs [is.simm.uis.web.desktop.remote :as rem])
            #?(:cljs [is.simm.runtimes.web :as web]))
  #?(:cljs (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]]
                            [org.replikativ.spindel.incremental.interval]))
  #?(:cljs (:require [org.replikativ.spindel.incremental.interval :as iv]
                     [org.replikativ.spindel.effects.track :refer [track]])))

#?(:cljs (defonce ^:private gate (agg/gate)))

#?(:cljs
   (defn load-position!
     "Fetch every readable book's position, coalesced by the aggregate gate."
     []
     (agg/run!
      gate
      (fn [done]
        (go
          (let [ch (promise-chan)]
            (binding [rtc/*execution-context* runtime]
              (let [s (ar/load-position! web/server-id)]
                (s (fn [r] (put! ch {:ok r})) (fn [e] (put! ch {:err e})))))
            (let [{:keys [ok err]} (<! ch)]
              (binding [rtc/*execution-context* runtime]
                (reset! sig/accounting-data
                        (if err
                          {:books [] :error (or (rem/error-text err)
                                                (ex-message err) (str err))}
                          {:books (vec ok) :loaded-at (js/Date.now)})))
              (done))))))))

#?(:cljs
   (defn- book-card [{:keys [room room-name accounts error] :as book}]
     (el/div {:key (str room) :class "acct-book"}
       (el/div {:class (str "acct-book-head" (when (:ref book) " acct-book-head--linked"))
                :on-click (fn [e]
                            (when-let [r (:ref book)]
                              (refs/open! r {:new-column? (or (.-metaKey e) (.-ctrlKey e))})))}
         (or room-name "Untitled team"))
       (when-let [n (:orphans book)]
         ;; Money the trial balance cannot see. Loud on purpose — see
         ;; `ops.accounting-report/orphan-postings`.
         (el/div {:class "proposal-error"}
           (str n " posting" (when (> n 1) "s") " reference an account with no path,"
                " so their amounts appear in NO balance below. Fix the account"
                " reference and re-post.")))
       (cond
         error (el/div {:class "proposal-error"}
                 (str "This book could not be read — " error))
         (empty? accounts)
         (el/div {:class "acct-empty"}
           "No postings yet. The ledger is set up — an agent with `kontor/entry!`
            or its verbs can post to it.")
         :else
         (el/div {:class "acct-rows"}
           (for [[i a] (map-indexed vector accounts)]
             (el/div {:key (str i) :class "acct-row"}
               (el/span {:class "acct-path"} (:path a))
               ;; A figure, not the BigDecimal's digit string: `885300 SEK`
               ;; where the number is 885 300.00 SEK invites the reader to be
               ;; wrong by an order of magnitude. See `desktop.money`.
               (el/span {:class "acct-amount"} (money/format-amount (:amount a)))
               (el/span {:class "acct-commodity"} (:commodity a)))))))))

(defn accounting-view []
  #?(:cljs
     (spin
       (let [data (iv/get-new (track sig/accounting-data))
             _ (when (nil? data) (load-position!))
             {:keys [books error]} data]
         (el/div {:class "acct-view"}
           ;; No Refresh button — the room-store book listener nudges this.
           (el/div {:class "acct-header"}
             (el/h2 {} "Accounting"))
           (cond
             error (el/div {:class "proposal-error"} (str "Couldn't load — " error))
             (nil? data) (el/div {:class "acct-empty"} "Loading…")
             (empty? books) (el/div {:class "acct-empty"} "No books you can read.")
             :else (el/div {:class "acct-books"}
                     (for [b books] (book-card b)))))))
     :clj nil))
