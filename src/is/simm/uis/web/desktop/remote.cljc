(ns is.simm.uis.web.desktop.remote
  "The one place a remote call's outcome is routed.

   Two mechanisms reach the server, and BOTH default to silence:

   - `distributed-scope/invoke-remote` returns a channel that yields either
     the result or an `ex-info`. A caller that fires the call and drops the
     channel — `(go (remote/write! …))` — sends the request and never learns
     whether it landed.
   - `defn-spin-remote` returns a spin invoked with a success and a failure
     continuation. A caller that passes only the success one silently swallows
     the failure.

   Both shapes are wrapped here so the DEFAULT is to tell the user, and
   silence is something a call site has to ask for. `:silent? true` is for
   calls whose failure genuinely does not matter to the person at the
   keyboard (a speculative prefetch, a best-effort presence ping); anything
   the user typed is not one of those.

   `spin!` also carries the `*execution-context*` binding that spin-remote
   needs on both the invoke and the callback — omitting it makes the call a
   silent no-op, which is the same failure mode by another route."
  (:require [clojure.string :as str]
            #?(:clj [clojure.edn :as edn] :cljs [cljs.reader :as edn])
            #?@(:cljs [[cljs.core.async :refer [promise-chan put! <!]]
                       [is.simm.uis.web.desktop.signals :as sig]
                       [org.replikativ.spindel.engine.core :as rtc]
                       [is.simm.uis.web.desktop.runtime :refer [runtime]]]))
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go]])))

#?(:clj
   (defn wire-instant
     "A Datahike instant as it crosses to the client: ISO-8601 UTC
      (\"2026-10-16T09:12:03.120Z\"), which `js/Date.` parses everywhere.
      NOT `(str date)` — `java.util.Date/toString` is \"Fri Oct 16 09:12:03
      CEST 2026\", a zone abbreviation browsers cannot parse, so the admin
      dashboard showed \"Invalid Date\". nil stays nil."
     [d]
     (when d (str (.toInstant ^java.util.Date d)))))

#?(:cljs
   (defn failure?
     "`invoke-remote` reports a server-side failure by yielding the error as an
      ordinary channel VALUE rather than throwing, so a caller that does not
      look at what it took cannot tell success from failure."
     [v]
     (instance? js/Error v)))

(defn error-info
  "The server's account of a failed remote call, as a map, or nil when `err`
   carries none.

   `invoke-remote` puts what the server sent under `:error` in ex-data: the
   server's `pr-str` of its error. The server reduces every failure to
   `{:error \"<short message>\" :type <kw, when the client may see it>
   :correlation-id \"<uuid>\"}` before that `pr-str`
   (`is.simm.runtimes.rpc-errors`), so the string reads back as that map. The
   correlation id is what to quote in a report: the server logged the full
   exception under it."
  [err]
  (let [e (:error (ex-data err))]
    (cond
      (map? e) e
      (and (string? e) (str/starts-with? e "{"))
      (let [m (try (edn/read-string e)
                   (catch #?(:clj Exception :cljs :default) _ nil))]
        (when (map? m) m)))))

(defn error-text
  "Short, human-readable form of a remote failure: the server's message, and
   the start of its correlation id to quote when reporting it."
  [err]
  (if-let [{:keys [error correlation-id]} (error-info err)]
    (str error (when (seq correlation-id)
                 (str " (ref " (subs correlation-id 0 (min 8 (count correlation-id))) ")")))
    (let [e (:error (ex-data err))]
      (or (some-> e str not-empty)
          (ex-message err)
          (str err)))))

#?(:cljs
   (defn- report!
     [err {:keys [message silent? on-error]}]
     (cond
       on-error (on-error err)
       silent? (js/console.warn "[remote] silenced failure:" (error-text err))
       :else (sig/show-error! (or message "That did not reach the server.")
                              (error-text err)
                              :remote))))

#?(:cljs
   (defn report-error!
     "Report a failure that did NOT arrive through `invoke!` or `spin!` — a
      rejected `fetch`, a FileSystem API error — on the same channel as one
      that did, so a user-visible operation fails visibly however it broke."
     ([err] (report-error! nil err))
     ([message err] (report! err {:message message}))))

#?(:cljs
   (defn invoke!
     "Take the outcome of a channel-returning remote call and route it.

      Returns a channel yielding the result, or `nil` when the call failed —
      the failure having already been reported. Callers that only need the
      request sent can ignore the returned channel; the reporting happens
      regardless, which is the whole point of the wrapper.

      Options: `:message` (what the user sees), `:silent?`, `:on-error`."
     ([ch] (invoke! ch nil))
     ([ch opts]
      (go (let [r (<! ch)]
            (if (failure? r)
              (do (report! r opts) nil)
              r))))))

#?(:cljs
   (defn spin!
     "Invoke a spin-remote and hand its outcome to `cb` as `(cb ok err)`.

      Both the invoke and the callback run with `*execution-context*` bound —
      from a DOM or load callback the binding is not in place, and without it
      the spin silently no-ops.

      With no `cb`, a failure is reported through `report!` and the result is
      dropped; that is the form for a fire-and-forget write that still has to
      speak up when it does not land."
     ([make-spin] (spin! make-spin nil nil))
     ([make-spin cb] (spin! make-spin cb nil))
     ([make-spin cb opts]
      (go (let [ch (promise-chan)]
            (binding [rtc/*execution-context* runtime]
              (let [s (make-spin)]
                (s (fn [r] (put! ch {:ok r :done true}))
                   (fn [e] (put! ch {:err (or e (ex-info "remote call failed" {}))
                                     :done true})))))
            (let [{:keys [ok err]} (<! ch)]
              (binding [rtc/*execution-context* runtime]
                (cond
                  cb (cb ok err)
                  err (report! err opts)
                  :else ok))))))))
