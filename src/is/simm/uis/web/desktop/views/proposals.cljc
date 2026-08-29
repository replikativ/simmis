(ns is.simm.uis.web.desktop.views.proposals
  "Proposals inbox (S2). Agent-filed structural changes as review cards:
   AI summary + per-system CONTENT-NATIVE diffs (KB block ops rendered as
   static blocks — NOT TipTap — anchored under their page titles with
   add/remove rails and before/after stacks) + Accept / Dismiss.
   doc/proposals-and-time-travel.md §S2."
  (:require [clojure.string :as str]
            [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.views.core :as vc]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.model.forkset :as fs]
            #?(:cljs [org.replikativ.spindel.engine.core :as rtc])
            #?(:cljs [is.simm.uis.web.desktop.runtime :refer [runtime]])
            #?(:cljs [is.simm.uis.web.desktop.remote :as rem])
            #?(:cljs [is.simm.uis.web.desktop.proposals-remote :as pr])
            #?(:cljs [is.simm.uis.web.desktop.aggregate :as agg])
            #?(:cljs [is.simm.runtimes.web :as web]))
  #?(:cljs (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]]
                            [org.replikativ.spindel.incremental.interval]
                            [org.replikativ.spindel.dom.foreach :refer [ifor-each]]))
  #?(:cljs (:require [org.replikativ.spindel.incremental.interval :as iv]
                     [org.replikativ.spindel.effects.track :refer [track]])))

;; =============================================================================
;; Loading + actions
;; =============================================================================

#?(:cljs (defonce ^:private gate (agg/gate)))

#?(:cljs
   (defn- update-proposal!
     "Apply `f` to the proposal with `pid` THROUGH the signal — a plain atom
      wouldn't re-render the spin."
     [pid f]
     (binding [rtc/*execution-context* runtime]
       (swap! sig/proposals-data update :proposals
              (fn [ps] (mapv #(if (= (:id %) pid) (f %) %) ps))))))

#?(:cljs (declare load-checks-into-signal!))

#?(:cljs
   (defn- load-diff-into-signal! [pid]
     ;; A failure has to land on the card. Silently ignoring it left `:diffs`
     ;; nil forever, so a DENIED diff rendered as a permanent "Loading changes…"
     ;; — indistinguishable from a slow one, and invisible in the console.
     (rem/spin!
      #(pr/proposal-diff! web/server-id pid)
      (fn [ok err]
        (update-proposal! pid
                          (if err
                            #(assoc % :diff-error (rem/error-text err))
                            #(-> %
                                 (assoc :diffs (vec (:forks ok))
                                        :tier (:tier ok)
                                        :ai-summary (:summary ok)
                                        :conflicts (vec (:conflicts ok)))
                                 (dissoc :diff-error))))
        ;; only once the diff says there IS a code fork — a check request for a
        ;; wiki-only proposal would open a repository workspace to discover
        ;; there is nothing to run
        (when (and (nil? err)
                   (some #(= :repo (:system-type %)) (:forks ok)))
          (load-checks-into-signal! pid))))))

#?(:cljs
   (defn- load-checks-into-signal!
     "Test results per code fork, fetched AFTER the diff and separately from it.

      A suite takes as long as it takes; folding it into the diff would hold the
      whole card behind it. A failure to fetch is left silent here — unlike the
      diff, an absent check is a legible state (the card simply shows no test
      result), so surfacing an error for it would put a red line on a card whose
      change is fine."
     [pid]
     (rem/spin!
      #(pr/proposal-checks! web/server-id pid)
      (fn [ok _err]
        (when ok (update-proposal! pid #(assoc % :checks (vec ok))))))))

;; --- focus verification ------------------------------------------------------
;;
;; A focused card asks for ONE proposal; the signal holds a cached list of the
;; OPEN ones. When the id is not in that list there are two possible worlds —
;; the proposal was resolved, or the list predates it — and the view must not
;; pick the alarming one by default. So an unmatched id is REQUESTED here, and
;; only a fetch that went out after the request can retire it. Plain atoms: this
;; is bookkeeping for a one-shot request, not reactive state — the re-render
;; comes from `proposals-data` when that fetch lands.

#?(:cljs (defonce ^:private focus-pending (atom #{})))   ;; asked, not yet answered
#?(:cljs (defonce ^:private focus-answered (atom #{})))  ;; a fresh list has spoken

#?(:cljs
   (defn- focus-answered?
     "True once a list fetched AFTER `focus-id` was requested has come back —
      i.e. once its absence is a fact about the workspace rather than about the
      cache."
     [focus-id]
     (contains? @focus-answered focus-id)))

#?(:cljs
   (defn load-proposals!
     "Fetch the open proposals, coalesced by the aggregate gate.

      A refusal now lands in the signal instead of only the console: leaving it
      nil rendered a permanent \"Loading…\" with no way back — the same failure
      this file's own `accept!` comment warns about, one function above it."
     []
     (agg/run!
      gate
      (fn [done]
        ;; Snapshot at fetch START, not at completion: these are exactly the ids
        ;; this round trip can speak for. An id requested WHILE it is in flight
        ;; is not in the snapshot, and the gate's pending bit runs another pass
        ;; for it rather than letting this answer stand in for one it predates.
        (let [answering @focus-pending]
          (rem/spin!
           #(pr/list-proposals! web/server-id "open")
           (fn [ok err]
             (binding [rtc/*execution-context* runtime]
               ;; retire the questions BEFORE the signal write — that write is
               ;; what re-renders, and it must not land on a stale "still
               ;; asking" flag. A FAILED fetch retires them too: the error
               ;; branch of the view outranks the resolution message anyway,
               ;; and leaving them open would re-ask on every render.
               (when (seq answering)
                 (swap! focus-answered into answering)
                 (swap! focus-pending #(reduce disj % answering)))
               (if err
                 (do (js/console.error "[proposals] load error:" err)
                     (reset! sig/proposals-data
                             {:proposals [] :error (rem/error-text err)}))
                 (do (reset! sig/proposals-data
                             {:proposals (vec ok) :loaded-at (js/Date.now)})
                     (doseq [p ok] (load-diff-into-signal! (:id p))))))
             (done))))))))

;; Why an id is absent, once the list has spoken: :accepted | :dismissed |
;; :unavailable. Absence has TWO causes and the view used to assert the wrong
;; one — see `ops.proposals/visible-status`.
#?(:cljs (defonce ^:private focus-outcome (atom {})))

#?(:cljs
   (defn- ask-focus-outcome!
     "Ask the server what it is willing to say about `focus-id`. The answer is
      `:accepted`/`:dismissed` when this party may see the proposal, and
      `:unavailable` when it may not — a deliberately ambiguous answer that does
      not distinguish \"no such proposal\" from \"not yours\", so a shared link
      cannot be used to probe for proposals in rooms you are not in."
     [focus-id]
     (rem/spin!
      #(pr/proposal-status! web/server-id focus-id)
      (fn [ok _err]
        (binding [rtc/*execution-context* runtime]
          (swap! focus-outcome assoc focus-id
                 (case (:status ok)
                   :accepted :accepted
                   :dismissed :dismissed
                   :unavailable))
          ;; nudge the signal so the view re-renders with the answer
          (swap! sig/proposals-data update :outcome-tick (fnil inc 0)))))))

#?(:cljs
   (defn- recheck-focus!
     "Ask the server once about a focus id the cached list does not contain.
      Idempotent per id; the answer arrives as `focus-answered?`."
     [focus-id]
     (when (and focus-id
                (not (contains? @focus-answered focus-id))
                (not (contains? @focus-pending focus-id)))
       (swap! focus-pending conj focus-id)
       (load-proposals!)
       ;; asked in parallel with the list: the list says WHETHER it is absent,
       ;; this says WHY, and the view needs both before it can claim anything
       (ask-focus-outcome! focus-id))))

#?(:cljs
   (defn- note-for
     "The reviewer's typed reason, read straight off the DOM at click time.
      Deliberately NOT signal-backed: keystroke-per-render on a textarea buys
      nothing here, and routing it through the signal would re-render the whole
      card (and its diff) on every character."
     [pid]
     (some-> (.querySelector js/document
                             (str "[data-note-for='" pid "']"))
             .-value)))

#?(:cljs
   (defn- accept! [pid force?]
     (update-proposal! pid #(-> % (assoc :busy? true) (dissoc :action-error)))
     (rem/spin!
      #(pr/accept-proposal! web/server-id pid force? (note-for pid))
      (fn [ok err]
        (cond
          ;; a refused or broken accept must SAY so on the card — this one
          ;; failed authorization for weeks while the button looked inert
          err (do (js/console.error "[proposals] accept error:" err)
                  (update-proposal! pid #(-> % (dissoc :busy?)
                                             (assoc :action-error (rem/error-text err)))))
          (:warnings ok)
          (update-proposal! pid
                            #(-> % (dissoc :busy?)
                                 ;; ARMS the next press. Without this the
                                 ;; button below kept sending force? false, so
                                 ;; "Accept again" repeated this same warning
                                 ;; forever and the force path was unreachable
                                 ;; from the UI — the instruction could not be
                                 ;; followed. Deliberately NOT cleared by the
                                 ;; `dissoc :action-error` at the top of
                                 ;; `accept!`: it has to survive precisely the
                                 ;; press it is arming. `load-proposals!` drops
                                 ;; it on a successful decision, which re-arms
                                 ;; the warning for the next conflicted one.
                                 (assoc :accept-warned? true)
                                 (assoc :action-error
                                        ;; "force" only overrides the WARNING
                                        ;; GATE — it is not a merge strategy.
                                        ;; A datom fork then merges anyway; a
                                        ;; code fork's merge still refuses,
                                        ;; because geschichte will not invent a
                                        ;; resolution. Promising a force-merge
                                        ;; that cannot happen sends the
                                        ;; reviewer to press Accept twice and
                                        ;; read an exception.
                                        (str "Conflicts detected ("
                                             (count (:warnings ok))
                                             "). Accept again to proceed anyway"
                                             " — a code fork will still refuse"
                                             " to merge until the conflict is"
                                             " resolved on its branch."))))

          ;; A FORK THAT DID NOT LAND IS NOT A SUCCESS. The server already names
          ;; each one and why (`ops.proposals` :failed — "surfaced, not
          ;; swallowed"), but this callback fell straight through to
          ;; `load-proposals!`, so a forced accept over a real conflict reset the
          ;; card to "Accept" and said nothing: the reviewer pressed the button
          ;; the warning told them to press, nothing landed, and the UI reported
          ;; no failure. Measured 2026-07-30 — server logged `:landed 0 :failed
          ;; 1`, screen showed a clean card.
          ;;
          ;; No `load-proposals!` here, deliberately. It replaces the whole
          ;; `:proposals` vector with freshly-fetched maps, which would drop the
          ;; `:action-error` written below and reinstate exactly the silence
          ;; being fixed. That is the same trade the `:warnings` and `err`
          ;; branches already make. Cost: on a PARTIAL landing the forks that
          ;; did land keep their old per-fork status until the next natural
          ;; refresh — stale detail is recoverable, an unreported refusal is not.
          (seq (:failed ok))
          (update-proposal! pid
                            #(-> % (dissoc :busy? :accept-warned?)
                                 (assoc :action-error
                                        (str (count (:failed ok))
                                             (if (= 1 (count (:failed ok)))
                                               " fork did not land: "
                                               " forks did not land: ")
                                             (->> (:failed ok)
                                                  (map (fn [{:keys [branch error]}]
                                                         (str branch " — " error)))
                                                  (interpose "; ")
                                                  (apply str))
                                             ". It stays open and can be retried"
                                             " once the conflict is resolved on"
                                             " its branch."))))
          :else (load-proposals!))))))

#?(:cljs
   (defn- comment-text
     "The reviewer's typed remark, read off the DOM at click time — same reason
      as `note-for`: a keystroke-per-render on the comment box would re-render
      the card and its whole diff."
     [pid]
     (some-> (.querySelector js/document (str "[data-comment-for='" pid "']"))
             .-value)))

#?(:cljs
   (defn- clear-comment! [pid]
     (when-let [el (.querySelector js/document (str "[data-comment-for='" pid "']"))]
       (set! (.-value el) ""))))

#?(:cljs
   (defn- comment! [pid]
     (let [body (comment-text pid)]
       (if (or (nil? body) (= "" (.trim body)))
         (update-proposal! pid #(assoc % :action-error "Write something first."))
         (do
           (update-proposal! pid #(-> % (assoc :busy? true) (dissoc :action-error)))
           (rem/spin!
            #(pr/comment-on-proposal! web/server-id pid body "")
            (fn [_ok err]
              (clear-comment! pid)
              (if err
                (update-proposal! pid #(-> % (dissoc :busy?)
                                           (assoc :action-error (rem/error-text err))))
                ;; reload rather than append locally: the server stamps the time
                ;; and resolves the author's display name, and inventing either
                ;; here would show the reviewer something the record does not say
                (load-proposals!)))))))))

#?(:cljs
   (defn- request-changes! [pid]
     (let [body (comment-text pid)]
       (if (or (nil? body) (= "" (.trim body)))
         (update-proposal! pid
                           #(assoc % :action-error
                                   "Say what should change — a request with no
                                    reason gives the agent nothing to act on."))
         (do
           (update-proposal! pid #(-> % (assoc :busy? true) (dissoc :action-error)))
           (rem/spin!
            #(pr/request-changes! web/server-id pid body "")
            (fn [ok err]
              (clear-comment! pid)
              (cond
                err (update-proposal! pid #(-> % (dissoc :busy?)
                                               (assoc :action-error (rem/error-text err))))
                ;; The comment landed but no author was put back to work: each
                ;; one has a proposal of its own open, and an author writes
                ;; into one overlay at a time. Saying nothing here would show
                ;; a request for changes that nobody is acting on.
                (seq (:not-reopened ok))
                (do (load-proposals!)
                    (update-proposal!
                     pid #(assoc % :action-error
                                 (str "Comment posted, but "
                                      (count (:not-reopened ok))
                                      (if (= 1 (count (:not-reopened ok)))
                                        " fork was" " forks were")
                                      " not reopened — the author has another"
                                      " proposal open and writes into one at a"
                                      " time. Ask again once it has filed that"
                                      " one; nothing retries on its own."))))
                :else (load-proposals!)))))))))

#?(:cljs
   (defn- dismiss! [pid]
     (update-proposal! pid #(-> % (assoc :busy? true) (dissoc :action-error)))
     (rem/spin!
      #(pr/dismiss-proposal! web/server-id pid (note-for pid))
      (fn [_ err]
        (if err
          ;; don't reload on failure — that would drop the card and make a
          ;; refused dismiss look like it worked
          (do (js/console.error "[proposals] dismiss error:" err)
              (update-proposal! pid #(-> % (dissoc :busy?)
                                         (assoc :action-error (rem/error-text err)))))
          (load-proposals!))))))

#?(:cljs
   (defn- fork-action!
     "One fork's decision. The card goes busy as a whole — there is one note
      field per card and one decision in flight at a time — and a success
      reloads, so the fork comes back carrying the status the server gave it
      rather than one guessed here.

      Failures land on the card for the same reason the whole-proposal actions
      above do: a per-fork refusal that fails authorization must SAY so, not
      leave a button looking inert."
     [pid make-spin]
     (update-proposal! pid #(-> % (assoc :busy? true) (dissoc :action-error)))
     (rem/spin!
      make-spin
      (fn [ok err]
        (cond
          err (do (js/console.error "[proposals] fork action error:" err)
                  (update-proposal! pid #(-> % (dissoc :busy?)
                                             (assoc :action-error (rem/error-text err)))))
          (:warnings ok)
          (update-proposal! pid
                            #(-> % (dissoc :busy?)
                                 (assoc :action-error
                                        (str "This fork conflicts with its trunk ("
                                             (count (:warnings ok))
                                             ") — it cannot be landed on its own."))))
          :else (load-proposals!))))))

#?(:cljs
   (defn- accept-fork! [pid scope branch]
     (fork-action! pid #(pr/accept-fork! web/server-id pid scope branch
                                         false (note-for pid)))))

#?(:cljs
   (defn- dismiss-fork! [pid scope branch]
     (fork-action! pid #(pr/dismiss-fork! web/server-id pid scope branch
                                          (note-for pid)))))

;; =============================================================================
;; Content-native diff rendering (static — NOT TipTap)
;; =============================================================================

;; `static-block` / `block-op-view` moved to views.core — the timeline's audit
;; panel renders the same ops in the other direction (see vc/block-op-view).

(defn- page-diff-view [{:keys [title ops]}]
  (el/div {:class "proposal-page"}
    (el/div {:class "proposal-page-title"}
      (vc/icon "file-text") (el/span {} (or title "Untitled")))
    (el/div {:class "proposal-page-ops"}
      (for [[i o] (map-indexed vector ops)]
        (el/div {:key (str i)} (vc/block-op-view o))))))

(defn- entry-date
  "The effective date as a plain ISO day. An accounting entry is dated, and the
   date is half of what a reviewer checks — but a full inst renders as a wall of
   timezone, so only the day is shown."
  [d]
  (when d
    (let [s (str #?(:cljs (.toISOString (js/Date. d)) :clj d))]
      (subs s 0 (min 10 (count s))))))

(defn- book-line-view
  "One posting: account, then the amount in the DEBIT or the CREDIT column.
   Two columns rather than a signed number, because that is how a ledger is read
   and because the sign convention (positive = debit) is kontor's, not the
   reviewer's. `amount` arrives as an exact decimal STRING — never re-parse it
   into a JS number here, which is the one operation that could lose a cent."
  [{:keys [account side amount commodity]}]
  (let [cell (str amount " " commodity)]
    (el/div {:class "proposal-book-line"}
      (el/span {:class "proposal-book-account"} (str account))
      (el/span {:class "proposal-book-amount is-debit"}
        (if (= :credit side) "" cell))
      (el/span {:class "proposal-book-amount is-credit"}
        (if (= :credit side) cell "")))))

(defn- book-entry-view
  "One proposed transaction: what it was for, when it takes effect, and its
   legs. `balanced?` is stated rather than assumed — kontor re-validates the
   sum-to-zero rule in the writer when the fork MERGES, so an entry that does
   not balance here is one the reviewer must not press Accept on."
  [{:keys [narration effective-date journal state lines balanced?]}]
  (el/div {:class "proposal-book-entry"}
    (el/div {:class "proposal-book-head"}
      (vc/icon "book")
      (el/span {:class "proposal-book-narration"} (str (or narration "(no narration)")))
      (el/span {:class "proposal-book-meta"}
        (str (entry-date effective-date)
             (when journal (str " · " journal))
             (when state (str " · " (name state)))
             (when-not balanced? " · DOES NOT BALANCE"))))
    (el/div {:class "proposal-book-lines"}
      (el/div {:class "proposal-book-line proposal-book-header"}
        (el/span {:class "proposal-book-account"} "Account")
        (el/span {:class "proposal-book-amount is-debit"} "Debit")
        (el/span {:class "proposal-book-amount is-credit"} "Credit"))
      (for [[i l] (map-indexed vector lines)]
        (el/div {:key (str i)} (book-line-view l))))))

(defn- file-op-view [{:keys [op before after]}]
  (let [n (get (or after before) :name)
        cls (case op :file/add "diff-add" :file/remove "diff-remove" "diff-edit")]
    (el/div {:class (str "proposal-file " cls)}
      (vc/icon "file") (el/span {} (str (name op) " " n)))))

;; =============================================================================
;; :repo — code
;;
;; A repo fork's files do NOT arrive in the drive's `{:op :file/add :after
;; {:name …}}` shape. Geschichte reports `{:status :added|:modified|:deleted
;; :path "src/foo.clj"}` — a path, not a node, because a repository is keyed by
;; path and has no durable per-file identity to resolve. Rendering one through
;; `file-op-view` would print "  " for every file, so code gets its own two
;; views.
;; =============================================================================

(defn- repo-file-view
  "One changed path. The status word is spelled out rather than shown as +/-,
   because a rename reaches us as an add plus a delete and a bare glyph would
   read as two unrelated edits."
  [{:keys [status path]}]
  ;; `diff-mod`, NOT the `diff-edit` the block diffs use: that class carries
  ;; `flex-direction: column` for stacking a before/after pair, and inheriting it
  ;; here turned every modified file into a centred two-line stack while added
  ;; files beside it rendered as proper rows.
  (el/div {:class (str "proposal-file "
                       (case status
                         :added "diff-add"
                         :deleted "diff-remove"
                         "diff-mod"))}
    (vc/icon "file")
    (el/span {} (str (name (or status :modified)) " " path))))

(defn- patch-line-class
  "Colour a unified-diff line by its leading character. `---`/`+++` are FILE
   HEADERS, not a deletion and an insertion — checking them first is the whole
   reason this is a function and not an inline `case`."
  [line]
  (cond
    (str/starts-with? line "+++") "patch-file"
    (str/starts-with? line "---") "patch-file"
    (str/starts-with? line "diff ") "patch-file"
    (str/starts-with? line "@@") "patch-hunk"
    (str/starts-with? line "+") "patch-add"
    (str/starts-with? line "-") "patch-del"
    :else "patch-ctx"))

(defn- repo-patch-view
  "The unified patch, coloured per line.

   Rendered from the SERVER's patch text rather than recomputed here: the diff
   is geschichte's own, so what the reviewer approves is what the merge planner
   compared. `truncated?` is stated — a patch that just stops is indistinguishable
   from a fork that ended there."
  [patch truncated?]
  (when (seq patch)
    (el/div {:class "proposal-patch"}
      (for [[i line] (map-indexed vector (str/split-lines patch))]
        (el/div {:key (str i) :class (str "patch-line " (patch-line-class line))}
          (str line)))
      (when truncated?
        (el/div {:class "patch-line patch-truncated"}
          "… patch truncated for display — the file list above is complete.")))))

(defn- conflict-side
  [label text skipped?]
  (el/div {:class "conflict-side"}
    (el/div {:class "conflict-side-label"} (str label))
    (el/pre {:class "conflict-side-text"}
      (cond
        skipped? "(binary or too large to show)"
        (nil? text) "(file absent on this side)"
        :else (str text)))))

(defn- conflict-view
  "One conflicting path, with what each side actually says.

   Trunk and the fork side by side rather than a merged view with markers: the
   reviewer's question here is not \"what would the merge look like\" — there is
   no merge, geschichte refused it — but \"which of these two changes did I
   mean\". The common ancestor is offered underneath because that is what makes
   the two comparable, and it is exactly what the reviewer has to reconstruct
   from memory otherwise."
  [{:keys [path base ours theirs base-skipped? ours-skipped? theirs-skipped? branch]}]
  (el/div {:class "proposal-conflict"}
    (el/div {:class "proposal-conflict-head"}
      (vc/icon "file")
      (el/span {:class "proposal-conflict-path"} (str path))
      (when branch
        (el/span {:class "proposal-conflict-branch"} (str branch))))
    (el/div {:class "proposal-conflict-sides"}
      (conflict-side "On trunk now" ours ours-skipped?)
      (conflict-side "In this fork" theirs theirs-skipped?))
    (when (or base base-skipped?)
      (el/details {:class "proposal-conflict-base"}
        (el/summary {} "What both started from")
        (conflict-side "Common ancestor" base base-skipped?)))))

(defn- checks-view
  "A code fork's test result, stated as a fact rather than a verdict.

   Deliberately does NOT gate Accept. A failing suite is information the
   reviewer weighs — the change may be a deliberate step, or the tests may be
   the thing being fixed — and turning it into a block would make the honest
   move (look, then decide) impossible. `:none` is its own state because
   \"nothing to run\" and \"everything ran and was fine\" are different facts
   and only one of them is reassuring."
  [{:keys [status tests passed failed errors output]}]
  (when status
    (el/div {:class (str "proposal-checks proposal-checks--" (name status))}
      (el/div {:class "proposal-checks-head"}
        (el/span {:class "proposal-checks-status"}
          (case status
            :pass "TESTS PASS"
            :fail "TESTS FAIL"
            :error "TESTS DID NOT RUN"
            :none "NO TESTS"
            (str status)))
        (when (and tests (pos? tests))
          (el/span {:class "proposal-checks-counts"}
            (str passed " passed"
                 (when (pos? (or failed 0)) (str " · " failed " failed"))
                 (when (pos? (or errors 0)) (str " · " errors " errored"))
                 " · " tests " namespaces"))))
      ;; the report only when it says something a reviewer must act on — a
      ;; passing run's output is noise on a card
      (when (and (#{:fail :error} status) (seq output))
        (el/pre {:class "proposal-checks-output"} (str output))))))

(defn- comment-view
  "One remark in the review conversation. A request for changes is marked,
   because it is the entry that explains why the proposal is still open."
  [{:keys [body author-name at kind fork-branch]}]
  (el/div {:class (str "proposal-comment"
                       (when (= :changes-requested kind) " proposal-comment--request"))}
    (el/div {:class "proposal-comment-head"}
      (el/span {:class "proposal-comment-author"} (str (or author-name "someone")))
      (when (= :changes-requested kind)
        (el/span {:class "proposal-comment-tag"} "requested changes"))
      (when fork-branch
        (el/span {:class "proposal-comment-scope"} (str "on " fork-branch)))
      (el/span {:class "proposal-comment-at"} (str (entry-date at))))
    (el/div {:class "proposal-comment-body"} (str body))))

;; Struck through and dimmed INLINE rather than through core.css: a refused
;; fork has to read as refused wherever this renders, and one stylesheet rule is
;; not worth a second file to keep in step with these two lines.
(def ^:private resolved-style
  {:opacity "0.55" :text-decoration "line-through"})

#?(:cljs
   (defn- fork-actions
     "Accept / Dismiss for ONE fork. Four agents can contribute four forks to a
      single ForkSet, and a reviewer who must refuse one of them cannot be made
      to refuse the other three with it. Safe because each fork is an
      independent branch on its own scope — this decides WHICH branches merge,
      never part of one.

      Rendered ONLY when there is more than one fork. On a single-fork proposal
      these buttons and the card's own Accept / Dismiss do exactly the same
      thing, and two identical pairs a few lines apart is a choice the reviewer
      has to stop and work out before deciding anything."
     [pid {:keys [scope branch busy? may-merge? capability-live?]}]
     (el/div {:class "proposal-fork-actions"}
       ;; `may-merge?` is resolved server-side per fork scope (see
       ;; ops.proposals/with-merge-authority). Since B1 `:merge` is its own verb,
       ;; so a reviewer can hold write on a KB and still not be allowed to land
       ;; onto its trunk — offering an Accept that is certain to refuse wastes a
       ;; round trip and reads as a bug.
       (el/button {:class "btn btn-secondary btn-sm"
                   :disabled (boolean (or busy? (false? may-merge?)
                                          (false? capability-live?)))
                   :title (cond
                            (false? capability-live?)
                            "This process no longer holds the world capability"
                            (false? may-merge?)
                            "You do not have merge rights on this patch's target")
                   :on-click (fn [_] (accept-fork! pid scope branch))}
                  (cond
                    (false? capability-live?) "Unavailable"
                    (false? may-merge?) "Cannot land"
                    :else "Accept this"))
       (el/button {:class "btn btn-secondary btn-sm"
                   :disabled (boolean (or busy? (false? capability-live?)))
                   :on-click (fn [_] (dismiss-fork! pid scope branch))}
                  "Dismiss this"))))

(defn- fork-diff-view
  "One fork as its own block: what it changed, and its own decision.

   `body?` false is the strip rendered beside the AI summary, where the diffs
   are collapsed into sections — a reviewer must still be able to refuse one
   fork there without expanding everything first.

   A resolved fork is shown struck through, never hidden. A refusal is a record,
   and a card that silently lost a contribution would leave the reviewer unable
   to see what they decided. Its diff is gone with its branch, which is why a
   dismissed block carries a line saying so instead of an empty change list."
  ([pid fk] (fork-diff-view pid fk true))
  ([pid {:keys [system-type pages files entries counts stat patch patch-truncated?
                baseless? status author-name checks] :as fk} body?]
   (let [resolved? (some? status)
         ;; A campaign puts one fork per AGENT on the same database, so without
         ;; the author two contributions render as two identical rows and the
         ;; per-fork Accept/Dismiss below asks for a decision with nothing to
         ;; base it on. Empty is stated outright for the same reason: a fork with
         ;; no changes must not look like one whose changes merely failed to load.
         ;; A code fork counts FILES. Its `:counts` is geschichte's
         ;; `{:files-changed :insertions :deletions}` and carries no
         ;; `:entities-touched`, so the datom path would report every code fork
         ;; as "no changes" — the one thing the head line must never say about a
         ;; fork that has some.
         changed (case system-type
                   :book (count entries)
                   :repo (or (:files-changed counts) (count files) 0)
                   (or (:entities-touched counts) 0))]
     (el/div {:class (str "proposal-fork"
                          (when status (str " proposal-fork--" (name status))))
              :style (when resolved? resolved-style)}
       (el/div {:class "proposal-fork-head"}
         (str (case system-type :kb "Wiki" :fs "Drive" :book "Book" :repo "Code"
                    (str system-type))
              (when author-name (str " · " author-name))
              ;; entries, not datoms, for a book: "3 changed" over a double-entry
              ;; fork counts postings and reads as noise
              (cond
                resolved? nil
                (zero? changed) " · no changes"
                (= :book system-type)
                (str " · " changed (if (= 1 changed) " entry" " entries"))
                (= :repo system-type)
                (str " · " changed (if (= 1 changed) " file" " files"))
                :else (str " · " changed " changed"))
              (case status
                :dismissed " · dismissed"
                :accepted " · accepted"
                "")))
       ;; A diff without a fork point is compared against trunk's MOVING head, so
       ;; anything trunk did since reads as this branch removing it. Say so — a
       ;; reviewer should know when they are looking at the weaker comparison.
       (when (and body? (not resolved?) baseless?)
         (el/div {:class "proposal-error"}
           "No fork point recorded — compared against trunk's current state, so
            changes made elsewhere since may appear here as removals."))
       (cond
         (= :dismissed status)
         (el/div {:class "proposal-fork-resolved"}
           "Refused — its branch was discarded, so there is nothing left to show.")
         (= :accepted status)
         (el/div {:class "proposal-fork-resolved"} "Landed on its trunk.")
         (not body?) nil
         (= :kb system-type) (for [[i pg] (map-indexed vector pages)]
                               (el/div {:key (str i)} (page-diff-view pg)))
         (= :fs system-type) (for [[i f] (map-indexed vector files)]
                               (el/div {:key (str i)} (file-op-view f)))
         (= :book system-type) (for [[i e] (map-indexed vector entries)]
                                 (el/div {:key (str i)} (book-entry-view e)))
         (= :repo system-type)
         ;; Files first, then the patch: the path list is the shape of the
         ;; change and fits on a screen, the patch is the detail behind it.
         ;; The check sits above both — whether it runs is the first thing a
         ;; reviewer wants, and it decides how carefully they read the rest.
         (el/div {:class "proposal-repo"}
           (checks-view checks)
           (el/div {:class "proposal-repo-files"}
             (for [[i f] (map-indexed vector files)]
               (el/div {:key (str i)} (repo-file-view f))))
           (when (seq stat)
             (el/div {:class "proposal-repo-stat"} (str stat)))
           (repo-patch-view patch patch-truncated?))
         :else (el/div {:class "proposal-counts"} (pr-str counts)))
       #?(:cljs (when (and (not resolved?) (not (:only-fork? fk)))
                  (fork-actions pid fk))
          :clj nil)))))

;; =============================================================================
;; Summary hierarchy — read this first, expand into the diff above
;; =============================================================================

(defn- pid->page
  "\"s0-p1\" → that page's diff. The digest is built in the same order the forks
   are sent, so the id is a positional address rather than another lookup table."
  [diffs pid]
  (let [[_ si pi] (re-matches #"s(\d+)-p(\d+)" (str pid))]
    (when si
      (get-in (vec diffs) [(parse-long si) :pages (parse-long pi)]))))

(defn- section-counts
  "Roll a section's pages up to +N ~N -N. Shown BESIDE the model's prose: the
   numbers come from the diff itself, so they hold even where the words drift."
  [digest pids]
  (let [by-pid (into {} (map (juxt :pid identity)) digest)
        rows (keep by-pid pids)]
    {:added (reduce + 0 (map :added rows))
     :edited (reduce + 0 (map :edited rows))
     :removed (reduce + 0 (map :removed rows))}))

#?(:cljs
   (defn- summary-section-view [{:keys [pid-set diffs digest section on-toggle expanded?]}]
     (let [{:keys [title summary pages subsections]} section
           {:keys [added edited removed]} (section-counts digest pages)
           open? (expanded? pid-set)]
       (el/div {:class "proposal-section"}
         (el/div {:class "proposal-section-head"
                  :on-click (fn [_] (on-toggle pid-set))}
           (el/span {:class "proposal-section-caret"} (if open? "▾" "▸"))
           (el/span {:class "proposal-section-title"} (or title "Changes"))
           (el/span {:class "proposal-section-counts"}
                    (str "+" added " ~" edited " −" removed)))
         (when summary
           (el/div {:class "proposal-section-summary"} summary))
         (when open?
           (el/div {:class "proposal-section-body"}
             (for [[i pid] (map-indexed vector pages)]
               (el/div {:key (str "p" i)}
                 (if-let [pg (pid->page diffs pid)]
                   (page-diff-view pg)
                   (el/div {:class "proposal-loading"} (str "…" pid)))))))
         (for [[i sub] (map-indexed vector subsections)]
           (el/div {:key (str "sub" i)}
             (summary-section-view {:pid-set (str pid-set "/" i) :diffs diffs
                                    :digest digest :section sub
                                    :on-toggle on-toggle :expanded? expanded?})))))))

;; =============================================================================
;; Cards
;; =============================================================================

(defn- tier-badge
  "dvergr's fork tier, rendered as the first thing a reviewer sees. :trivial
   means the branch contributed no meaningful additions; :conflict means at
   least one system genuinely conflicts and Accept will need a force."
  [tier]
  #?(:cljs
     (when tier
       (el/span {:class (str "proposal-tier proposal-tier--" (name tier))}
                (case tier
                  :trivial "no substantive change"
                  :conflict "conflicts"
                  :reviewable "needs review"
                  (name tier))))
     :clj nil))

(defn- intent-badge
  "Shown only for the non-`:change` intents. A patch needs no label — it is
   what every ForkSet was until intents existed — but a budget rendered as an
   unlabelled diff card would read as an edit someone forgot to describe."
  [intent]
  #?(:cljs
     (when (and intent (not= :change intent))
       (el/span {:class (str "proposal-intent proposal-intent--" (name intent))}
                (name intent)))
     :clj nil))

(defn- check-for
  "The check belonging to `fk`, matched on scope+branch — the same pair every
   per-fork endpoint takes, so no positional index has to stay in step."
  [checks fk]
  (first (filter #(and (= (:scope %) (:scope fk))
                       (= (:branch %) (:branch fk)))
                 checks)))

(defn- may-merge-for
  "Whether this reviewer may LAND `fk`, joined from the list payload's forks on
   scope+branch — the same join `check-for` makes, and for the same reason.

   Two fork lists reach the card: `list-proposals!` carries authority and
   status, `proposal-diff!` carries the actual changes. Authority belongs to the
   first (it is a `can?` answer about the acting party) and must not be inferred
   from the second. Returns nil when unknown — an older payload, or a fork the
   list does not mention — and nil deliberately does NOT disable anything: an
   unknown answer must not silently take a button away."
  [forks fk]
  (some (fn [f] (when (and (= (:scope f) (:scope fk))
                           (= (:branch f) (:branch fk)))
                  (:may-merge? f)))
        forks))

(defn- capability-live-for [forks fk]
  (some (fn [f] (when (and (= (:scope f) (:scope fk))
                           (= (:branch f) (:branch fk)))
                  (:capability-live? f)))
        forks))

(defn- unlandable-open-forks
  "Open forks this reviewer may not land. The card's own Accept lands ALL open
   forks (`∀ patch ∈ selected`), so any one of these makes it certain to refuse."
  [forks]
  (filterv #(and (nil? (:status %)) (false? (:may-merge? %))) forks))

(defn- unavailable-open-forks [forks]
  (filterv #(and (nil? (:status %)) (false? (:capability-live? %))) forks))

(defn- proposal-card [{:keys [id title summary diffs diff-error action-error busy?
                              tier intent conflicts comments checks ai-summary expanded
                              accept-warned? forks]}]
  #?(:cljs
     (let [expanded (or expanded #{})
           toggle (fn [k] (update-proposal! id
                                            #(update % :expanded
                                                     (fn [s] (let [s (or s #{})]
                                                               (if (s k) (disj s k) (conj s k)))))))
           show-all? (contains? expanded :all)]
       (el/div {:key id :class "proposal-card"}
         (el/div {:class "proposal-card-head"}
           (el/div {:class "proposal-title"} (or title "Untitled proposal"))
           (intent-badge intent)
           ;; Tier only where it MEANS something. A budget's branch may merge
           ;; cleanly and it still is not landable, so "no substantive change"
           ;; on a plan reads as a verdict the tier is not making.
           (when (= :change (or intent :change)) (tier-badge tier)))
         (when (fs/auto-mergeable? intent tier)
           (el/div {:class "proposal-note-line"}
             "Nothing substantive to review — this one merges cleanly."))
         (when (seq conflicts)
           (el/div {}
             (el/div {:class "proposal-error"}
               (str (count conflicts) " conflicting change"
                    (when (> (count conflicts) 1) "s")
                    " — must be resolved on the fork's branch before it can land."))
             ;; and WHAT conflicts. A count alone leaves refuse as the only
             ;; informed move; seeing the two versions is what lets a reviewer
             ;; say which one was meant.
             (for [[i c] (map-indexed vector conflicts)]
               (el/div {:key (str "c" i)} (conflict-view c)))))
         ;; The model's headline wins over the author's blurb when we have one:
         ;; it describes what the diff ACTUALLY does, not what was intended.
         (when-let [h (:headline ai-summary)]
           (el/div {:class "proposal-headline"} h))
         (when (and (:risk-note ai-summary)
                    (not= "none" (:risk ai-summary)))
           (el/div {:class (str "proposal-risk proposal-risk--" (or (:risk ai-summary) "low"))}
             (:risk-note ai-summary)))
         (when (and summary (not (:headline ai-summary)))
           (el/div {:class "proposal-summary"} summary))
         (el/div {:class "proposal-diffs"}
           (cond
             diff-error (el/div {:class "proposal-error"}
                          (str "Couldn't load changes — " diff-error))
             (nil? diffs) (el/div {:class "proposal-loading"} "Loading changes…")

             ;; Summary-first: sections collapse the diff into intents, each
             ;; expanding to the real blocks. Falls back to the flat diff
             ;; whenever summarization was unavailable — the raw change is
             ;; always reachable, never replaced by prose.
             (and (seq (:sections ai-summary)) (not show-all?))
             (el/div {}
               (for [[i sec] (map-indexed vector (:sections ai-summary))]
                 (el/div {:key (str "sec" i)}
                   (summary-section-view {:pid-set (str i)
                                          ;; the digest's "sN" is a position in
                                          ;; the DIFFED forks, and only open
                                          ;; forks are diffed — so resolve it
                                          ;; against the same subsequence, or a
                                          ;; dismissal shifts every page it
                                          ;; addresses by one
                                          :diffs (filterv (comp nil? :status) diffs)
                                          :digest (:digest ai-summary)
                                          :section sec
                                          :on-toggle toggle
                                          :expanded? #(contains? expanded %)})))
               ;; the summary collapses the diffs but must not collapse the
               ;; DECISIONS: a reviewer refusing one fork of four should not
               ;; have to expand the full diff to reach its button
               (when (< 1 (count diffs))
                 (el/div {:class "proposal-fork-strip"}
                   (for [[i fk] (map-indexed vector diffs)]
                     (el/div {:key (str "fs" i)}
                       (fork-diff-view id (assoc fk :busy? busy? :only-fork? (= 1 (count diffs))
                                             :checks (check-for checks fk)
                                             :may-merge? (may-merge-for forks fk)
                                             :capability-live?
                                             (capability-live-for forks fk)) false)))))
               (el/button {:class "btn btn-ghost btn-sm proposal-showall"
                           :on-click (fn [_] (toggle :all))}
                          "Show the full diff"))

             :else
             (el/div {}
               (when (seq (:sections ai-summary))
                 (el/button {:class "btn btn-ghost btn-sm proposal-showall"
                             :on-click (fn [_] (toggle :all))}
                            "Back to summary"))
               (for [[i fk] (map-indexed vector diffs)]
                 (el/div {:key (str i)}
                   (fork-diff-view id (assoc fk :busy? busy? :only-fork? (= 1 (count diffs))
                                             :checks (check-for checks fk)
                                             :may-merge? (may-merge-for forks fk)
                                             :capability-live?
                                             (capability-live-for forks fk)) true))))))
         (when action-error
           (el/div {:class "proposal-error"} action-error))
         ;; Optional, and nothing consumes it yet. It is here so the corpus of
         ;; "what this workspace accepts, refuses, and why" starts accruing now —
         ;; an agent reviewer can be few-shot prompted on it later, and the
         ;; reasoning cannot be recovered after the decision is made.
         (when (seq comments)
           (el/div {:class "proposal-comments"}
             (for [[i c] (map-indexed vector comments)]
               (el/div {:key (str (:id c) i)} (comment-view c)))))
         ;; ONE box, several buttons — the GitHub shape, and it is right for the
         ;; same reason: what the reviewer types is the same kind of thing
         ;; whether they are asking a question, requesting a change, or
         ;; explaining a decision. Two boxes would make them choose a category
         ;; before they have decided what to do.
         (el/input {:class "proposal-note"
                    :type "text"
                    :data-note-for id
                    :data-comment-for id
                    :placeholder "Leave a comment, or say why (optional for a decision)"})
         ;; Accept lands EVERY open fork, so one patch you may not land makes the
         ;; whole button certain to refuse. Say which, before it is pressed —
         ;; `authorize-forks!` would name them in an error, but an error after
         ;; the fact is a worse place to learn it than the card.
         (let [blocked (unlandable-open-forks forks)]
           (when (seq blocked)
             (el/div {:class "proposal-merge-blocked"}
                     (str (count blocked)
                          (if (= 1 (count blocked))
                            " patch cannot be landed by you: "
                            " patches cannot be landed by you: ")
                          (->> blocked (map :branch) (interpose ", ") (apply str))
                          ". Decide them individually, or ask someone with merge"
                          " rights on their targets."))))
         (when (seq (unavailable-open-forks forks))
           (el/div {:class "proposal-merge-blocked"}
             "This process no longer holds the adopted world capability. The durable proposal remains auditable, but settlement requires recovery."))
         (el/div {:class "proposal-actions"}
           ;; `force?` is whether THIS press follows a conflict warning. It was
           ;; hardcoded false, so the "Accept again to proceed anyway" the
           ;; warning prints could never be obeyed — every press repeated the
           ;; same request and re-rendered the same message.
           (el/button {:class "btn btn-affirm"
                       :disabled (boolean (or busy?
                                              (seq (unlandable-open-forks forks))
                                              (seq (unavailable-open-forks forks))))
                       :title (cond
                                (seq (unavailable-open-forks forks))
                                "The adopted world capability is unavailable"
                                (seq (unlandable-open-forks forks))
                                "Some patches in this proposal are not yours to land")
                       :on-click (fn [_] (accept! id (boolean accept-warned?)))}
                      (cond busy? "Working…"
                            (seq (unavailable-open-forks forks)) "Unavailable"
                            (seq (unlandable-open-forks forks)) "Cannot land all"
                            accept-warned? "Accept anyway"
                            :else "Accept"))
           (el/button {:class "btn btn-secondary"
                       :disabled (boolean busy?)
                       :on-click (fn [_] (request-changes! id))}
                      "Request changes")
           (el/button {:class "btn btn-secondary"
                       :disabled (boolean busy?)
                       :on-click (fn [_] (comment! id))} "Comment")
           (el/button {:class "btn btn-secondary"
                       :disabled (boolean (or busy? (seq (unavailable-open-forks forks))))
                       :on-click (fn [_] (dismiss! id))} "Dismiss"))))
     :clj nil))

;; The two destinations, in the order a reviewer wants them: what can land
;; today first, what cannot land yet after it. `:unclassified` is last and
;; transient — mergeability arrives per card, a 3-way compare at a time.
(def ^:private group-order
  [[:tasks "Ready to land"
    "These merge into the present as they stand."]
   [:futures "Not yet landable"
    "Held here because they conflict with the present, or because they are
     plans rather than patches."]
   [:unclassified "Checking…"
    "Working out whether these still merge."]])

(defn proposals-view
  "The ForkSet inbox. With `focus-id`, ONE ForkSet — how a Task or Feed row
   opens the proposal it points at instead of the list it was listed in.

   The focused case still renders the same card, so there is no second review
   surface to keep in step with this one."
  ([] (proposals-view nil))
  ([focus-id]
   #?(:cljs
      (spin
        (let [data (iv/get-new (track sig/proposals-data))
              _ (when (nil? data) (load-proposals!))
              all (:proposals data)
              focused (when focus-id
                        (first (filter #(= (str (:id %)) (str focus-id)) all)))
              ;; Not in the list is not yet an answer — ask, once.
              _ (when (and focus-id (nil? focused)) (recheck-focus! focus-id))
              ;; …and only THEN is an absence worth reporting as a resolution.
              focus-resolved? (and focus-id (nil? focused) (focus-answered? focus-id))
              proposals (if focused [focused] all)
              by-dest (group-by #(fs/destination (:intent %) (:tier %)) proposals)]
          (el/div {:class "proposals-view"}
            (el/div {:class "proposals-header"}
              (el/h2 {} (if focused "Proposal" "Proposals"))
              ;; invalidated by `perspectives-sync` on branch lifecycle events
              ;; and on `:proposal/tx-occurred` (filing / resolution)
              (when focused
                (el/button {:class "btn btn-ghost btn-sm"
                            :on-click (fn [e]
                                        (binding [rtc/*execution-context* runtime]
                                          (sig/open-tab! :proposals nil
                                                         {:title "Proposals"
                                                          :new-column? (or (.-metaKey e)
                                                                           (.-ctrlKey e))})))}
                           "All proposals")))
            (cond
              (:error data) (el/div {:class "proposal-error"}
                              (str "Couldn't load proposals — " (:error data)))
              (nil? data) (el/div {:class "proposals-empty"} "Loading…")
              ;; THREE states, not two. A focus id missing from a list that was
              ;; fetched after the id was asked about means resolved; missing
              ;; from any older list means only that the list is older. This
              ;; view used to collapse the two and tell people their brand-new
              ;; proposal had been accepted or dismissed.
              ;; ABSENT ≠ RESOLVED. The list is authz-filtered, so an id missing
              ;; from it means resolved OR invisible to this party. Asserting the
              ;; first is false for a shared link AND discloses that someone
              ;; else's proposal exists. `proposal-status!` answers which, and
              ;; refuses to distinguish "no such proposal" from "not yours".
              focus-resolved?
              (el/div {:class "proposals-empty"}
                (case (get @focus-outcome focus-id)
                  :accepted "That proposal is no longer open — it was accepted."
                  :dismissed "That proposal is no longer open — it was dismissed."
                  :unavailable (str "That proposal isn't available. It may have been "
                                    "removed, or you may not have access to it.")
                  ;; the status answer has not landed yet — say nothing about why
                  "That proposal is no longer open."))
              (and focus-id (nil? focused))
              (el/div {:class "proposals-empty"} "Loading…")
              (empty? proposals)
              (el/div {:class "proposals-empty"}
                "No open proposals. Agents file these for big restructurings.")
              :else
              (el/div {:class "proposals-list"}
                (for [[dest label blurb] group-order
                      :let [ps (get by-dest dest)]
                      :when (seq ps)]
                  (el/div {:key (name dest)
                           :class (str "proposal-group proposal-group--" (name dest))}
                    ;; a single focused card needs no group heading above it
                    (when-not focused
                      (el/div {:class "proposal-group-head"}
                        (el/span {:class "proposal-group-title"} label)
                        (el/span {:class "proposal-group-count"} (str (count ps)))))
                    (when-not focused (el/div {:class "proposal-group-blurb"} blurb))
                    (for [p ps] (proposal-card p)))))))))
      :clj nil)))
