(ns is.simm.uis.web.desktop.routes
  "The URL contract: paths ⟷ refs.

   Pure, and dependency-free ON PURPOSE. This started inside `refs.cljc` and
   could not be tested there: `refs` requires `signals`, which does not load on
   the JVM at all. A path grammar has no business depending on the signal graph,
   and the paths outlive every other decision in the UI — once a link is in
   someone's inbox its shape is permanent — so they get a namespace that can be
   exercised without a DOM, a browser, or a running app.

   THE SIX ADDRESSABLE SHAPES, and only these:

     /page/<db-scope>/<page-uuid>     wiki page
     /room/<room-id>                  chat
     /room/<room-id>/m/<message-id>   chat anchored on a message
     /room/<room-id>/t/<root-id>      focused message thread
     /room/<room-id>/files            files
     /proposal/<id>                   proposal

   The ~20-member tab vocabulary stays unaddressable. Most of it is singletons
   (`:settings`, `:admin`, `:home`) that nobody shares, and
   `doc/archive/navigation-redesign-plan.md:175` already flags naming leaking into URLs
   as cheap-now-expensive-later.

   A PAGE TAKES TWO SEGMENTS, and that is forced, not stylistic. Page identity
   is (store, uuid) — `refs/ref->tab`'s `:page` case needs both — because the
   same page uuid can exist in different KBs. The retired prototype router's
   `#/page/{uuid}` could not name a page in this data model at all; that, rather
   than age, is why its vocabulary was replaced instead of revived. Rooms and
   files need only the room id, since `ref->tab` resolves the scope from the
   roster.

   WHY NOT `dh://` AS THE SCHEME. It was considered and cannot carry the
   vocabulary: `dh://<db-id>/(<eid>|<attr>/<value>)[?tx=…&branch=…]` names ONE
   ENTITY, but `:files` is a VIEW of a room and `:message` is an ANCHOR PAIR —
   neither is an entity. A bare URI also cannot say what KIND of thing it points
   at without resolving it server-side (`refs/uri->ref` simply assumes `:page`).
   So `dh://` stays an input format rather than the contract. \"Open whatever
   this entity is\" would be an additive sixth route doing a server-side type
   lookup, and it disturbs none of these five — as would `?tx=`/`?branch=` for
   time-travel links, which dh:// already models."
  (:require [clojure.string :as str]))

(defn ref->route
  "Path for a ref, or nil when the ref is not addressable."
  [{:keys [kind id scope page room message thread]}]
  (case kind
    :page (when (and page scope) (str "/page/" scope "/" page))
    :room (when room (str "/room/" room))
    :message (when (and room message) (str "/room/" room "/m/" message))
    :thread (when (and room thread) (str "/room/" room "/t/" thread))
    :files (when room (str "/room/" room "/files"))
    :proposal (when id (str "/proposal/" id))
    nil))

(defn route->ref
  "A path back into a ref, or nil when it names nothing openable.

   Deliberately strict about arity: a path with the right prefix and the wrong
   number of segments yields nil rather than a partial ref, so a mistyped or
   truncated link lands on the default view instead of opening something
   adjacent to what was meant."
  [path]
  (let [segs (->> (str/split (str path) #"/")
                  (remove str/blank?)
                  vec)]
    (case (first segs)
      "page" (when (= 3 (count segs))
               {:kind :page :scope (segs 1) :page (segs 2)})
      "room" (case (count segs)
               2 {:kind :room :room (segs 1)}
               3 (when (= "files" (segs 2)) {:kind :files :room (segs 1)})
               4 (case (segs 2)
                   "m" {:kind :message :room (segs 1) :message (segs 3)}
                   "t" {:kind :thread :room (segs 1) :thread (segs 3)}
                   nil)
               nil)
      "proposal" (when (= 2 (count segs))
                   {:kind :proposal :id (segs 1)})
      nil)))

(defn tab->ref
  "The ref a tab is showing, or nil when the tab is not addressable.

   The inverse of `refs/ref->tab`, and kept here rather than beside it for the
   same reason the rest of this namespace is: `refs` cannot be loaded off the
   browser. This is what lets the URL be a PROJECTION of the focused tab — read
   the layout, derive the ref, write the path — with popstate as the only
   inbound edge, so there is no cycle to break.

   Nil is the common and correct answer: `:settings`, `:admin`, `:home` and the
   perspectives are all unaddressable, as is the proposals LIST (as opposed to
   one focused proposal). A nil ref means the URL simply does not change."
  [{:keys [type data]}]
  (let [{:keys [page-uuid db-scope room-id anchor-message thread-root-id
                proposal-id]} data]
    (case type
      :wiki (when (and page-uuid db-scope)
              {:kind :page :scope (str db-scope) :page (str page-uuid)})
      ;; a chat windowed on one message is a DIFFERENT address from the room —
      ;; that distinction is the whole point of the :message kind
      :chat (when room-id
              (if anchor-message
                {:kind :message :room (str room-id) :message (str anchor-message)}
                {:kind :room :room (str room-id)}))
      :chat-thread (when (and room-id thread-root-id)
                     {:kind :thread
                      :room (str room-id)
                      :thread (str thread-root-id)})
      :files (when room-id {:kind :files :room (str room-id)})
      :proposals (when proposal-id {:kind :proposal :id (str proposal-id)})
      nil)))

(defn tab->route
  "Path for a tab, or nil when it is not addressable. `tab->ref` then
   `ref->route`, which is the only composition the URL writer needs."
  [tab]
  (some-> (tab->ref tab) ref->route))

;; NOT IMPLEMENTED, deliberately: a `ref -> dh:// URI` converter. The reverse
;; (`refs/uri->ref`) exists because a URI arrives from outside and must be
;; opened. Emitting one needs the ATTRIBUTE a page reference is keyed by —
;; `dh://<db-id>/<attr>/<value>` — and that is not derivable from a ref, which
;; carries only the uuid. Guessing it would produce URIs that parse and resolve
;; to nothing. Add it when there is a caller, next to whatever already knows the
;; attribute (`kb/link` on the agent side does).
