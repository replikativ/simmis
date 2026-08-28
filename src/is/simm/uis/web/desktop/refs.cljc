(ns is.simm.uis.web.desktop.refs
  "One way to open a reference.

   Every perspective is a list of pointers: a Feed row points at the page an
   agent wrote, a Task at the page or proposal it lives in, an Accounting line
   at the team whose book it is, a CRM row at a contact's page. Before this,
   each view invented its own click handler — so Feed opened a wiki tab by
   hand, Tasks opened nothing, and none of them could reach a chat message even
   though the machinery for that has existed all along (`:anchor-message` opens
   a room windowed on one message and scroll-highlights it once).

   So: aggregates emit a `:ref`, and `open!` is the only thing that knows how a
   ref becomes a tab. New perspectives get navigation for free, and a new
   target kind is added HERE rather than in five views.

   A ref is a map with a `:kind`, or a `dh://` URI string — the same
   cross-database reference agents already emit via `kb/link` and the same one
   `[[dh://…]]` links carry in wiki and chat text. That is deliberate: a link
   an agent writes into a digest page and a row in a dashboard should be the
   same act, not two parallel systems."
  (:require [is.simm.uis.web.desktop.signals :as sig]
            #?(:cljs [datahike.reference :as dh-ref])
            #?(:cljs [org.replikativ.spindel.engine.core :as rtc])
            #?(:cljs [is.simm.uis.web.desktop.runtime :refer [runtime]])))

#?(:cljs
   (defn- room-scope
     "The content-db scope for a room, from the user-rooms roster.

      Callers usually have the room id but not its store; the roster already
      carries the mapping, so resolving here keeps every call site from
      repeating it (the wiki backlink handler did this inline)."
     [room-id-str]
     (let [ur @sig/user-rooms
           rooms (or (:rooms ur) (when (vector? ur) ur))]
       (some->> rooms
                (filter #(= room-id-str (str (:room/id %))))
                first
                :room/content-db-scope
                str))))

(defn ref->tab
  "`[tab-type tab-data title]` for a ref, or nil when it cannot be opened.

   Pure apart from the roster lookup, so the mapping can be read in one place
   and tested without a DOM."
  [{:keys [kind id scope page room message thread title] :as _ref}]
  #?(:cljs
     (case kind
       :page (when (and page scope)
               [:wiki {:page-uuid (uuid (str page)) :db-scope (str scope)}
                (or title (str page))])
       ;; The one that already worked and was unreachable: opening a room
       ;; ANCHORED on a message. The chat view windows the timeline around it
       ;; and scroll-highlights once.
       :message (when (and room message)
                  (let [sc (or (some-> scope str) (room-scope (str room)))]
                    [:chat (cond-> {:room-id (str room)
                                    :room-name title
                                    :anchor-message (str message)}
                             sc (assoc :db-scope sc))
                     (or title "Chat")]))
       :thread (when (and room thread)
                 (let [sc (or (some-> scope str) (room-scope (str room)))]
                   [:chat-thread
                    (cond-> {:room-id (str room)
                             :room-name title
                             :thread-root-id (str thread)}
                      sc (assoc :db-scope sc))
                    (or title "Thread")]))
       :room (when room
               (let [sc (or (some-> scope str) (room-scope (str room)))]
                 [:chat (cond-> {:room-id (str room) :room-name title}
                          sc (assoc :db-scope sc))
                  (or title "Chat")]))
       :files (when room [:files {:room-id (str room)} (or title "Files")])
       ;; The proposal itself, not the list it was listed in. This used to
       ;; discard the ref and open the Tasks tab the row was already on — a
       ;; dead end in the one view whose docstring promises navigation. Falls
       ;; through to nil without an id, so `open!` warns rather than pretending.
       :proposal (when id
                   [:proposals {:proposal-id (str id)} (or title "Proposal")])
       nil)
     :clj nil))

#?(:cljs
   (defn- uri->ref
     "A `dh://` URI as a ref. The URI carries the store id and the entity uuid,
      so this is a direct open with no resolution — the property that made
      cross-database links work at all."
     [uri title]
     (let [{:keys [db-id value]} (dh-ref/parse uri)]
       (when (and db-id (uuid? value))
         {:kind :page :scope (str db-id) :page value :title title}))))

#?(:cljs
   (defn open!
     "Open `ref` as a tab. `ref` is a ref map or a `dh://` URI string.

      `opts`: `:new-column?` (cmd/ctrl-click), `:col-id` (source column), and
      `:title` to override the label.

      Silent no-ops are the failure this replaces: a row that looks clickable
      and does nothing is worse than one that is not clickable, so an
      unopenable ref warns."
     ([ref] (open! ref {}))
     ([ref {:keys [new-column? col-id title]}]
      ;; The binding wraps EVERYTHING, including `ref->tab`. It used to start
      ;; only at `open-tab!`, and `ref->tab`'s `:room`/`:message` cases call
      ;; `room-scope`, which derefs `sig/user-rooms` — unbound, that throws.
      ;; DOM handlers are installed raw, so no context is bound at click time:
      ;; every ref WITHOUT a `:scope` (an Accounting book, a dvergr dispatch)
      ;; threw on click, while `:page` refs carry one and short-circuited, which
      ;; is why Feed looked fine.
      (binding [rtc/*execution-context* runtime]
        (let [ref (cond-> ref
                    (string? ref) (uri->ref title)
                    title (assoc :title title))]
          (if-let [[tab-type tab-data tab-title] (ref->tab ref)]
            (sig/open-tab! tab-type tab-data
                           (cond-> {:title (or title tab-title)
                                    :new-column? (boolean new-column?)}
                             col-id (assoc :col-id col-id)))
            (js/console.warn "[refs] cannot open reference:" (pr-str ref))))))))
