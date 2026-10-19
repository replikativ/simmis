(ns is.simm.uis.web.desktop.views.timelines
  "Timelines — the workspace at a reference.

   Not a dashboard of leftovers. Every panel here answers the SAME question,
   `what is true at this reference?`, and the reference is the one the whole
   app already resolves reads through (`sig/global-ref`, and
   `db-signal/ensure-view-db-signal!` behind it):

     a past cut  → what the workspace looked like then
     now         → what is live
     a fork      → what would be true if that ForkSet landed

   ONE OPERATION, SIGN FLIPPED. Auditing the past and exploring an option look
   like different features and are the same question — `how does this state
   differ from the present?` — asked in opposite directions. A past cut shows
   what changed between then and now; a fork shows what would change if it
   landed. So they share a surface, and the rail is the view's input rather
   than an ornament: moving it re-evaluates everything below.

   What each direction needs BEYOND that shared core is where they part, and it
   is worth naming because it is the roadmap. Audit wants attribution — who
   changed this, and why it was accepted. The accept rationale is recorded, and
   `:tx/author` now rides on the transaction (schema.clj `provenance-schema`),
   so a change CAN be named — for the writers that set it. Seeded wiki writes
   do; the interactive editor does not yet, and an unattributed row says the
   date and nothing else rather than inventing a person. Options want
   consequence — what this would cost — which is what `:budget` intent is for
   and why the book matters.

   Plural deliberately: there is one past and many futures. The rail is
   continuous to the left (time is a total order — every instant maps to
   exactly one state) and DISCRETE to the right (a future is not a distance,
   it is a branch; with three open ForkSets there are three, and no meaning
   whatever between them). A symmetric slider would promise a continuum the
   data cannot deliver, so the two halves are drawn differently on purpose."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.views.core :as vc]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.model.forkset :as fs]
            [clojure.string :as str]
            #?(:cljs [org.replikativ.spindel.engine.core :as rtc])
            #?(:cljs [is.simm.uis.web.desktop.runtime :refer [runtime]])
            #?(:cljs [is.simm.uis.web.desktop.db-signal :as db-sig])
            #?(:cljs [is.simm.uis.web.desktop.refs :as refs])
            #?(:cljs [is.simm.uis.web.desktop.people :as people])
            #?(:cljs [is.simm.uis.web.desktop.timeline-source :as tl-src])
            [is.simm.uis.web.desktop.timeline-layout :as tll]
            #?(:cljs [is.simm.uis.web.desktop.views.schedules :as sched-view])
            #?(:cljs [is.simm.uis.web.desktop.views.proposals :as prop-view])
            #?(:cljs [is.simm.uis.web.desktop.views.history-subway :as subway]))
  #?(:cljs (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]]
                            [org.replikativ.spindel.signal :refer [signal]]
                            [org.replikativ.spindel.incremental.interval]))
  #?(:cljs (:require [org.replikativ.spindel.incremental.interval :as iv]
                     [org.replikativ.spindel.effects.track :refer [track]])))

;; =============================================================================
;; The reference
;; =============================================================================

(defn ref-label
  "How the current reference reads in a sentence. The view's whole claim is
   that you are looking at ONE state, so it says which."
  [gref]
  (cond
    (:as-of gref) (str "as it was on " (:as-of gref))
    (:forkset gref) "a proposed future"
    :else "now"))

(defn- past? [gref] (boolean (:as-of gref)))

;; =============================================================================
;; The rail
;; =============================================================================

;; The past used to come from `branching-remote/kb-commit-graph!` — an RPC per
;; scope, fired only when a wiki tab happened to be open, behind a latch that
;; was never released. So the rail read "No history loaded yet" for anyone who
;; opened Timelines directly, and never moved once loaded. It now derives from
;; the replicas the client already holds; see `timeline-source`.
;;
;; A CUT BEFORE THE STORE WAS INSTALLED is reachable from this rail, and the
;; panels below are not the only readers of it. Seeded content is backdated to
;; the months the scenario describes (`demo.scenario/provenance`), so a dot far
;; enough left puts the workspace at a reference where the seed entities do not
;; exist yet — and a query that names one through a lookup ref,
;; `[?e :instance/of-role [:entity/name "S/Page"]]`, THROWS there rather than
;; returning nothing (measured: `Nothing found for entity id [:entity/name
;; "S/Page"]`). The panels here read plain attributes and were never affected.
;;
;; Closed from both ends (2026-07-27). Stores we seed are installed at the
;; scenario's earliest instant, so their schema and seed are older than their
;; content (`store/install!`'s `at`, threaded from `demo.scenario`), and the
;; page query resolves the role as a value first, so a store nobody back-dated
;; reports an empty wiki instead of throwing (`datahike-query/page-role-eid`).
;; Scrubbing past the beginning of a store is now an ordinary, boring place.

#?(:cljs (defn- pct [x] (str (* 100.0 x) "%")))

#?(:cljs
   (defn- axis
     "The past, on a scale that says what it is.

      Position here is elapsed time, log-warped (see `timeline-layout/past-x`),
      and the gridlines label the warp — a rail that compresses a week into its
      left third while pretending to be linear would misread every distance on
      it. Clicking a commit sets the cut; clicking `now` clears it."
     [commits ticks cut-ms now-ms span]
     (el/div {:class "tl-axis"}
       (for [t ticks]
         (el/div {:key (:label t) :class "tl-tick" :style {:left (pct (:x t))}}
           (el/span {:class "tl-tick-label"} (:label t))))
       ;; The cut, drawn where it actually falls rather than inferred from
       ;; whichever dot happens to be highlighted.
       (when cut-ms
         (el/div {:class "tl-cut" :style {:left (pct (tll/past-x cut-ms now-ms span))}}))
       (el/div {:class (str "tl-now" (when-not cut-ms " tl-now--active"))
                :style {:left (pct tll/past-frac)}
                :title "Back to now"
                :on-click (fn [_] (binding [rtc/*execution-context* runtime]
                                    (reset! sig/global-ref nil)))}
         (el/span {:class "tl-now-dot"} "▮")
         (el/span {:class "tl-now-label"} "now"))
       (for [c commits]
         (el/span {:key (:id c)
                   :class (str "tl-commit"
                               (when (and cut-ms (<= (:ms c) cut-ms)) " tl-commit--before-cut"))
                   :style {:left (pct (tll/past-x (:ms c) now-ms span))}
                   :title (str (:scope-name c) " · " (.toLocaleString (:ts c)))
                   :on-click (fn [_]
                               (binding [rtc/*execution-context* runtime]
                                 (reset! sig/global-ref {:as-of (:ts c)})))}
           "●")))))

#?(:cljs
   (defn- fork-lane
     "One ForkSet as a line from where it diverged to where it ends.

      The end is the claim: a mergeable ForkSet's line reaches `now`, because
      its final commit is concurrent with the present and could BE the present
      the moment you accept it. A conflicted one stops visibly short — it cannot
      reach the present, which is the same fact that routes it to Futures rather
      than Tasks. Nothing here decides that twice: the geometry and the lists
      both read `fs/destination`.

      Clicking opens the proposal. Entering a future — making the workspace show
      what it WOULD look like — attaches here once `ensure-view-db-signal!`
      resolves forkset refs."
     [f now-ms span]
     (let [{:keys [x0 x1 dest reaches-now?]} (tll/fork-line f now-ms span)]
       (el/div {:key (:id f) :class "tl-fork"
                :title (str (case dest
                              :tasks "Ready to merge into the present"
                              :futures "Cannot merge into the present as it stands"
                              "Checking whether this can merge…")
                            " — click to inhabit it")
                :on-click (fn [_]
                            (binding [rtc/*execution-context* runtime]
                              ;; The ref carries the fork's branches: the
                              ;; projector cannot look them up (see
                              ;; `ensure-view-db-signal!`).
                              (reset! sig/global-ref
                                      {:forkset (:id f)
                                       :title (:title f)
                                       :branches (:branches f)})))}
         ;; One class per state, not `not reaches-now?` plus an override. An
         ;; unclassified fork is not blocked — it is unknown — and giving it
         ;; both classes left it rendering correctly only because the pending
         ;; rule happens to come second in the stylesheet.
         (el/div {:class (str "tl-fork-line"
                              (case dest
                                :futures " tl-fork-line--blocked"
                                :unclassified " tl-fork-line--pending"
                                ""))
                  :style {:left (pct x0) :width (pct (max 0.0 (- x1 x0)))}}
           (el/span {:class "tl-fork-label"} (:title f)))
         ;; The terminator is the most information-dense pixel on the line and
         ;; used to be the same glyph in all three states, differing only by
         ;; colour. It now says which: an arrowhead meeting `now`, or a stop.
         (el/span {:class "tl-fork-head" :style {:left (pct x1)}}
                  (case dest
                    :tasks "▸"
                    :futures "✕"
                    "◇"))))))

#?(:cljs
   (defn- schedule-lane
     "What runs without you, placed by when it fires. The only genuinely
      future-tense region on the rail — `:schedule/next-fire` is a real
      timestamp, so this is the one thing here that is later rather than
      merely alternative."
     [schedules now-ms]
     (el/div {:class "tl-sched"}
       (for [s schedules
             :let [t (.getTime (js/Date. (:next-fire s)))]
             :when (>= t now-ms)]
         (el/span {:key (str (:id s))
                   :class "tl-sched-item"
                   :style {:left (pct (tll/future-x t now-ms))}
                   :title (str (or (:agent-name s) (:room-name s) "workflow")
                               " · " (.toLocaleString (js/Date. (:next-fire s))))}
           "▲")))))

#?(:cljs
   (defn- rail
     "One axis, three relations to the present: earlier than, instead of, later
      than. doc/demo-and-timeline-plan.md §2."
     [gref commits futures schedules now-ms]
     (let [span (tll/span-ms commits now-ms)
           ticks (tll/axis-ticks now-ms span)
           cut-ms (some-> (:as-of gref) (.getTime))
           upcoming (filter :next-fire schedules)]
       (el/div {:class "tl-rail"}
         (if (empty? commits)
           (el/div {:class "tl-rail-empty"} "Nothing has changed yet.")
           (axis commits ticks cut-ms now-ms span))
         (el/div {:class "tl-forks"}
           (for [f futures] (fork-lane f now-ms span)))
         (when (seq upcoming)
           (schedule-lane upcoming now-ms))
         ;; The rail's vocabulary, in the marks themselves at their own sizes —
         ;; no legend/figure mismatch to decode, and nothing to look up.
         (el/div {:class "tl-key"}
           (el/span {:class "tl-key-item"}
             (el/span {:class "tl-key-dot"} "●") (el/span {} "change"))
           (el/span {:class "tl-key-item"}
             (el/span {:class "tl-key-line"}) (el/span {:class "tl-key-cap"} "▸")
             (el/span {} "proposal — reaching now means it merges cleanly"))
           (el/span {:class "tl-key-item"}
             (el/span {:class "tl-key-line tl-key-line--blocked"})
             (el/span {:class "tl-key-cap tl-key-cap--blocked"} "✕")
             (el/span {} "conflicts"))
           (el/span {:class "tl-key-item"}
             (el/span {} "▲") (el/span {} "scheduled")))))))

;; =============================================================================
;; Panels — the state at that reference
;; =============================================================================

#?(:cljs
   (defn- attribute
     "Put each change row's AUTHOR NAME in the row.

      `changes-since` returns a party uuid string, because the transaction is in
      a KB store and the roster is not. Resolving it here, into the item rather
      than in the render closure, is the ifor-each rule (sharp edge #2): a name
      that arrives with the roster has to change the ITEM or the row it belongs
      to will not re-render. `people/all` reads `sig/user-rooms`, which this
      view already tracks, so that arrival is a re-evaluation.

      An unresolvable uuid resolves to nil, not to itself: a row reading
      `4f3a-…-91c changed Escalation Policy` is worse than one that just gives
      the date, because it looks like a name and cannot be read as one."
     [rows]
     (let [who (into {} (map (juxt (comp str :entity/uuid) :display-name)) (people/all))]
       (mapv (fn [r] (assoc r :author-name (get who (:author r)))) rows))))

#?(:cljs
   (defn- panel [title subtitle & body]
     (el/div {:class "tl-panel"}
       (el/div {:class "tl-panel-title"} title)
       (when subtitle (el/div {:class "tl-panel-sub"} subtitle))
       ;; Splice seq-valued children into ONE flat child list. Varargs collect
       ;; into a list, so a caller passing a `for` seq hands over a seq nested
       ;; inside that list, and el/div flattens one level only — the panel then
       ;; renders empty. It looked fine for as long as the only seq-passing
       ;; caller (Changed) had nothing to show.
       ;;
       ;; Splicing here rather than `apply`: el/div is a MACRO, so it has no
       ;; applicable value and `(apply el/div …)` dies at runtime reading
       ;; `cljs$lang$applyTo` off undefined.
       (el/div {:class "tl-panel-body"}
         (mapcat #(if (seq? %) % [%]) body)))))

#?(:cljs
   (declare expanded-changes))

#?(:cljs
   (defn- difference
     "THE panel: how the chosen reference differs from the present.

      Audit and option-exploration look like two features and are one operation
      with the sign flipped —

        a past cut  → what changed between then and now      (audit)
        now         → nothing; you are standing in it
        a fork      → what would change if this landed       (an option)

      — so they share a surface instead of being a history view beside a
      proposal view. This replaced three panels (`Needs you`, `Runs without
      you`, `Changed`) that answered a DIFFERENT question, `what wants my
      attention`, which is what Tasks and Schedules are for. They also did not
      move when the reference moved: each one printed `live figure — not
      resolved at this reference`, the view apologising for its own contents."
     [gref changes futures expanded]
     (cond
       (:forkset gref)
       (let [f (first (filter #(= (:forkset gref) (:id %)) futures))]
         (panel "If this lands"
                (:title gref)
                (el/div {:class "tl-diff-hint"}
                  "You are looking at the workspace as it WOULD be. The sidebar, "
                  "pages and links are the future's, and nothing here is editable.")
                (el/button {:class "btn btn-sm"
                            :on-click (fn [_]
                                        (binding [rtc/*execution-context* runtime]
                                          (refs/open! {:kind :proposal :id (:forkset gref)
                                                       :title (:title gref)})))}
                           "Review the change")
                (when f
                  (el/div {:class "tl-diff-note"}
                          ;; `fs/destination` directly. Going through
                          ;; `fork-line` would mean inventing a span just to
                          ;; reach the routing it does internally — same
                          ;; function either way, so no second opinion, but
                          ;; this one does not pretend to be geometry.
                          (case (fs/destination (:intent f) (:tier f))
                            :tasks "This merges into the present as it stands."
                            :futures "This does not merge into the present as it stands."
                            "Still working out whether this merges.")))))

       (past? gref)
       (panel "What changed since then"
              (str (count changes) " "
                   (if (= 1 (count changes)) "page" "pages")
                   " — newest first")
              (if (empty? changes)
                (el/div {:class "tl-empty"} "Nothing changed between then and now.")
                (for [c (take 12 changes)]
                  (let [open? (contains? expanded (:key c))]
                    (el/div {:key (:key c)
                             :class (str "tl-change" (when open? " tl-change--open"))}
                      ;; The row states THAT something changed; expanding it
                      ;; states WHAT. Counting blocks and never showing them is
                      ;; the gap this closes — the diff is a client-local
                      ;; `as-of` away, no round trip.
                      (el/div {:class "tl-change-row"
                               :title "Show what changed on this page"
                               :on-click (fn [_]
                                           (binding [rtc/*execution-context* runtime]
                                             (swap! expanded-changes
                                                    (fn [s] (if (contains? s (:key c))
                                                              (disj s (:key c))
                                                              (conj s (:key c)))))))}
                        (vc/icon (if open? "chevron-down" "chevron-right"))
                        (el/span {:class "tl-change-when"}
                                 (.toLocaleString (:ts c) "en-US"
                                                  #js {:month "short" :day "numeric"
                                                       :hour "numeric" :minute "2-digit"}))
                        (el/span {:class "tl-change-what"} (:page c))
                        (when-let [n (:author-name c)]
                          (el/span {:class "tl-change-who"} "by " n))
                        (el/span {:class "tl-change-scope"} (:scope-name c))
                        (el/span {:class "tl-change-n"}
                                 (str (:n c) (if (= 1 (:n c)) " block" " blocks"))))
                      (when open?
                        (let [ops (tl-src/page-block-ops (:scope c) (:blocks c)
                                                         (.getTime (:as-of gref)))]
                          (el/div {:class "tl-change-diff"}
                            (cond
                              ;; nil = the as-of view could not be taken (see
                              ;; page-block-ops); do not pass that off as
                              ;; "nothing changed".
                              (nil? ops)
                              (el/div {:class "tl-empty"}
                                "Could not reconstruct this page as it was then.")

                              (empty? ops)
                              (el/div {:class "tl-empty"}
                                "These blocks were re-saved without their text changing.")

                              :else
                              (for [[i o] (map-indexed vector ops)]
                                (el/div {:key (str (:key c) "-" i)}
                                  (vc/block-op-view o))))))))))))

       :else
       (panel "The present"
              "pick a point on the rail to audit, or a proposal to inhabit"
              (el/div {:class "tl-diff-hint"}
                "Everything here is live. Moving left asks what changed and when; "
                "moving to a proposal asks what would change if you accepted it.")))))

;; =============================================================================
;; View
;; =============================================================================

;; Keys of the change rows whose diff is open. Session-local and deliberately
;; NOT part of `global-ref`: which rows you have expanded is a reading position,
;; not a property of the reference being read, and it must survive the panel
;; re-rendering when a replica advances. `defonce` so a hot reload does not
;; collapse everything you had open.
#?(:cljs (defonce ^:private expanded-changes (signal runtime #{})))

(defn timelines-view []
  #?(:cljs
     (spin
       ;; Track at the TOP, before anything reads a delta — a mid-body resume
       ;; re-executes from the track point and `iv/get-new` is not idempotent
       ;; (spindel sharp edge #1).
       (let [gref       (iv/get-new (track sig/global-ref))
             ;; Also at the top (sharp edge #1): expanding a row must re-render
             ;; the panel, and a track below a delta consumer would not.
             expanded   (iv/get-new (track expanded-changes))
             ;; Tracked as a change TOKEN, not for its value: it fires whenever
             ;; any replica advances, and `all-commits` then re-reads the dbs by
             ;; deref. Tracking the per-KB signals directly cannot work — see
             ;; `db-sig/kb-heads`.
             _heads     (iv/get-new (track db-sig/kb-heads))
             ;; The axis is entirely relative to the present, so `now` is an
             ;; input like any other and has to be tracked or the rail's
             ;; distances go stale in an idle tab.
             now        (iv/get-new (track tl-src/now-tick))
             sched-data (iv/get-new (track sig/schedules-data))
             props-data (iv/get-new (track sig/proposals-data))
             user-rooms (iv/get-new (track sig/user-rooms))
             _ (when (nil? sched-data) (sched-view/load-schedules!))
             _ (when (nil? props-data) (prop-view/load-proposals!))
             scope-names (into {}
                               (map (fn [kb] [(str (:kb/db-scope kb)) (:kb/name kb)]))
                               (when (map? user-rooms) (:knowledge-bases user-rooms)))
             commits (tl-src/all-commits scope-names)
             ;; Every open ForkSet is a world you could inhabit — including the
             ;; ones that merge cleanly. Readiness decides what you DO with it
             ;; (that is Tasks); it does not decide whether the world exists.
             futures (->> (:proposals props-data)
                          ;; intent + tier, NOT a precomputed destination:
                          ;; `fork-line` routes, and a second `fs/destination`
                          ;; here would be the drift that ns exists to prevent.
                          (mapv (fn [p] {:id (:id p) :title (:title p)
                                         :intent (:intent p) :tier (:tier p)
                                         ;; where its line STARTS: the branch is
                                         ;; minted when the proposal is filed,
                                         ;; so this is the divergence point
                                         :created-at (:created-at p)
                                         ;; scope-str → branch-kw, so entering
                                         ;; this future needs no second lookup
                                         :branches (into {} (map (fn [f] [(str (:scope f))
                                                                          (keyword (:branch f))]))
                                                         (:forks p))})))
             now-ms (.getTime (or now (js/Date.)))
             ;; Only computed under a past cut — it is a query per replica, and
             ;; the present has nothing to compare itself against.
             changes (when (past? gref)
                       (attribute
                        (tl-src/changes-since scope-names (.getTime (:as-of gref)))))]
         (el/div {:class "tl-view"}
           (el/div {:class "tl-header"}
             (el/h2 {} "Timelines")
             (el/div {:class "tl-ref"}
               (el/span {} "Viewing ")
               (el/span {:class (str "tl-ref-value"
                                     (when (past? gref) " tl-ref-value--past")
                                     (when (:forkset gref) " tl-ref-value--future"))}
                        (cond
                          (:as-of gref) (.toLocaleString (:as-of gref) "en-US")
                          (:forkset gref) (or (:title gref) "a proposed future")
                          :else (ref-label gref)))
               (when (not= :now (or gref :now))
                 (el/button {:class "btn btn-ghost btn-sm"
                             :on-click (fn [_] (binding [rtc/*execution-context* runtime]
                                                 (reset! sig/global-ref nil)))}
                            (if (:forkset gref) "Leave this future" "Back to now")))))
           (rail gref commits futures (:schedules sched-data) now-ms)
           (difference gref changes futures expanded))))
     :clj nil))
