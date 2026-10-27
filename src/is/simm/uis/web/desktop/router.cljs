(ns is.simm.uis.web.desktop.router
  "The URL, kept in step with the focused tab.

   REPLACES a hash router whose whole vocabulary was `#/` and `#/page/{uuid}`.
   That one was initialized at boot but inert: its route signal had zero
   readers, its only writer lived in `views/home.cljs` (itself unreachable), and
   `#/page/{uuid}` could not name a page in this data model at all, since page
   identity is (store, uuid). See `routes.cljc` for the contract that replaced
   it.

   THE URL IS A PROJECTION, not a second source of truth. One direction is a
   pure derivation — read the layout, derive the ref, write the path — and the
   only inbound edge is popstate. So the classic route↔layout fight has no cycle
   to suppress: there is simply one writer each way. `last-synced` is a VALUE
   guard on top of that, not a boolean re-entrancy flag: the legs settle
   asynchronously, and a boolean around an async effect is exactly how these
   designs break.

   WHAT IS USED FROM SPINDEL, and what is not. `navigate!`/`replace!` are the
   only push/replaceState implementation in either repo, and the replace half is
   what keeps tab switching out of browser history. NOT used: `start!` and the
   `router` macro — the entire browser half of that namespace is untested
   (16 deftests, 11 of them `#?(:clj` guarded; `start!` has none), and we
   already have a tested path parser in `routes.cljc`. Adding `#?(:cljs …)`
   arms there is worth doing upstream; adopting an untested boot path to get
   parsing we already have is not."
  (:require [org.replikativ.spindel.dom.router :as spin-router]
            [org.replikativ.spindel.engine.core :as rtc]
            [is.simm.uis.web.desktop.routes :as routes]
            [is.simm.uis.web.desktop.refs :as refs]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]))

;; Route table. Only for spindel's `navigate!`, which matches internally; the
;; authoritative parse is `routes/route->ref`. More specific patterns first,
;; though `match-route` is arity-sensitive so it hardly matters.
(def ^:private route-defs
  [["/room/:room/workflows/:workflow/:action" :workflow]
   ["/page/:scope/:page"      :page]
   ["/room/:room/m/:message"  :message]
   ["/room/:room/files"       :files]
   ["/room/:room"             :room]
   ["/proposal/:id"           :proposal]])

;; A PLAIN ATOM for the signal, deliberately. `navigate-impl!` resets it and
;; nothing here tracks it — the route is applied imperatively, so making it a
;; spindel signal would add a second tracker for state the layout already owns.
;; Constructed at NAMESPACE LOAD rather than inside the boot go-block: signal
;; addresses come off the ctx chain, and creating engine state on the async boot
;; path is the sort of thing that perturbs determinism and fork-safety.
(defonce ^:private router
  (spin-router/->Router (spin-router/compile-routes route-defs) (atom nil)))

;; The last path this module wrote or applied. Compared BY VALUE.
(defonce ^:private last-synced (atom ::unset))

(defn current-path []
  (str js/window.location.pathname js/window.location.search))

(defn- write-url!
  "Push or replace, according to WHY focus moved.

   PUSH on `:navigate` — the user asked to look at something, and Back should
   return them to what they were looking at before. REPLACE on `:passive`:
   moving column focus, or closing a tab. Get this backwards and Back becomes
   an undo stack for window management; push on a close and Back re-shows a URL
   whose tab no longer exists.

   The intent comes from the mutator rather than a dynamic var deliberately —
   a binding around effects that settle asynchronously is exactly the shape
   that breaks."
  [path reason]
  (if (= :navigate reason)
    (spin-router/navigate! router path)
    (spin-router/replace! router path)))

(defn sync-url!
  "Derive the path from the focused tab and write it if it changed.

   A tab with no address (settings, admin, the proposals LIST) leaves the URL
   alone rather than clearing it — the address bar keeps naming the last thing
   that had a name, which is less surprising than snapping to `/`."
  ([] (sync-url! :passive))
  ([reason]
   (binding [rtc/*execution-context* runtime]
     (let [tab  (sig/focused-tab @sig/layout-columns @sig/active-column-id)
           path (routes/tab->route tab)]
       (when (and path (not= path @last-synced))
         (reset! last-synced path)
         (write-url! path reason))))))

(defn- apply-path!
  "Open whatever `path` names. Unrecognised paths are ignored — the app stays
   where it is rather than clearing the layout for a typo."
  [path]
  (when-let [ref (routes/route->ref path)]
    ;; Set the guard BEFORE opening. Opening changes focus, which calls
    ;; `sync-url!` synchronously; without this it would compute the same path
    ;; and write it again — harmless with replace!, but a spurious history
    ;; entry under push, and the kind of thing that turns into a loop the first
    ;; time an intermediate step differs.
    (reset! last-synced path)
    (refs/open! ref)))

(defn init!
  "Wire the URL to the layout, then honour whatever the address bar already
   says. Idempotent per session — re-registering the focus listener replaces."
  [_runtime]
  (sig/on-focus-change! ::url sync-url!)   ;; called as (sync-url! reason)
  (.addEventListener js/window "popstate"
                     (fn [_] (apply-path! (current-path))))
  ;; A deep link on cold boot. Nothing to open for "/" — the default layout
  ;; stands, and the first focus change will name it.
  (apply-path! (current-path)))
