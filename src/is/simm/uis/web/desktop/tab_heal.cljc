(ns is.simm.uis.web.desktop.tab-heal
  "Open tabs, reconciled against the room roster.

   Pure, and dependency-free ON PURPOSE — the same reason `routes.cljc` is.
   This started inside `user_rooms_sync.cljs` and could not be tested there:
   that namespace requires `signals`, which does not load on the JVM at all,
   and `clojure -X:test` does not execute `.cljs`. The rules below are subtle
   enough to be worth stating as assertions rather than as prose (a
   placeholder, a missing scope, a missing name, a room that is simply gone,
   and a verdict that must be revocable), so they get a namespace that can be
   exercised without a DOM, a browser, or a running app.

   This owns the WHAT. The WHEN is two moments, and it has to be both,
   because the tab and the roster can arrive in either order:

     - the roster lands while tabs are open — `user-rooms-sync` reconciles
       the whole layout (`reconcile-layout`);
     - a tab is opened while the roster is already loaded — `signals/open-tab!`
       reconciles the one new tab (`reconcile-tab`).

   Either moment alone leaves half the bug. Reconciling only on roster arrival
   was the original fix, and it repairs a cold-boot deep link; it does nothing
   for the far commoner case of clicking a stale link an hour into a session,
   when the roster landed long ago and will not land again on its own. Both
   moments run the SAME rules below, so the outcome does not depend on which
   fact showed up first.")

(def ^:private room-tab-types
  "Tab types that NAME a room, and therefore have a roster verdict.

   `:chat-thread` is one of them. A thread tab is a room tab windowed on one
   thread: it carries the same `:room-id` and needs the same `:db-scope`, so a
   cold-boot deep link to `/room/<id>/thread/<id>` fails in exactly the way a
   `:chat` tab does without this. Its `:thread-root-id` is NOT a room id and is
   never read or written here — thread identity does not collapse into room
   identity, and a thread tab is never rewritten into a chat tab."
  #{:chat :chat-thread})

(defn- roster-title
  "The label a tab adopts from the roster. A thread tab says which room the
   thread is in, matching what `open-tab!` writes when a thread is opened from
   the timeline."
  [tab room]
  (if (= :chat-thread (:type tab))
    (str "Thread · " (:room/name room))
    (:room/name room)))

(defn heal-chat-tab
  "Reconcile one tab against `rooms`: fill in what the roster knows, or mark
   the tab as pointing at no room this party can open. Applies to every tab
   type in `room-tab-types`.

   THE FACT ARRIVES WITH THE ROSTER, which is why the reconciliation is keyed
   to it. A chat tab can be created BEFORE the roster exists: `router/init!`
   runs at boot step 3 and applies a `/room/<id>` deep link immediately, while
   the roster is fetched from the sidebar spin, which does not exist until step
   4 mounts it. `refs/ref->tab` resolves `:db-scope` from that roster, so on a
   cold-boot deep link it resolves to nothing — and the chat column then waits
   forever for a replica nobody ever asked to connect. The URL also carries no
   room name, which is why such a tab reads \"Chat\".

   `:room-missing?` is the other half, and it is a CONCLUSION, not a timeout.
   The roster is the complete list of rooms this party can open; once it has
   arrived and does not name this room, the tab cannot be loaded — now, and not
   merely not-yet. A later refresh that does name the room clears the flag, so
   a room shared with you mid-session heals rather than staying condemned."
  [tab rooms personal-room]
  (if-not (contains? room-tab-types (:type tab))
    tab
    (let [room-id      (get-in tab [:data :room-id])
          placeholder? (= room-id "personal-ai-placeholder")
          room         (cond
                         placeholder? personal-room
                         room-id (first (filter #(= room-id (str (:room/id %))) rooms))
                         :else nil)]
      (if room
        (cond-> (assoc-in tab [:data :room-id] (str (:room/id room)))
          true (update :data dissoc :room-missing?)
          (nil? (get-in tab [:data :db-scope]))
          (assoc-in [:data :db-scope] (str (:room/content-db-scope room)))
          ;; Adopt the room's name when the tab is not showing a real one: it
          ;; either has none (a deep link carries no title) or is showing the
          ;; boot layout's stand-in "Assistants". A tab that has a real name
          ;; keeps it.
          (or placeholder? (nil? (get-in tab [:data :room-name])))
          (-> (assoc-in [:data :room-name] (:room/name room))
              (assoc :title (roster-title tab room))))
        ;; No room-id at all is the same conclusion by a shorter route: the
        ;; legacy `:chat-room` backlink opens a tab carrying only a title.
        (assoc-in tab [:data :room-missing?] true)))))

(defn heal-chat-tabs
  "`heal-chat-tab` across a whole column layout."
  [cols rooms personal-room]
  (mapv (fn [col]
          (update col :tabs
                  (fn [tabs] (mapv #(heal-chat-tab % rooms personal-room) tabs))))
        cols))

;; -----------------------------------------------------------------------------
;; The roster as a fact, and the two moments that apply it
;; -----------------------------------------------------------------------------

(defn roster-known?
  "Has the roster ARRIVED? — which is not the same question as \"does it name
   any rooms\".

   `sig/user-rooms` is nil until `load-rooms!` answers, and the answer for a
   party with no rooms is an EMPTY list, not nil. Conflating the two would make
   every tab opened before the first roster read as `:room-missing?`, which is
   the opposite error from the one being fixed: a verdict passed on no
   evidence. A `load-rooms!` result is a map carrying `:rooms`; the roster
   signal has historically also been read as a bare vector of rooms (see
   `refs/room-scope`), so both shapes count as arrived."
  [roster]
  (boolean (or (and (map? roster) (contains? roster :rooms))
               (vector? roster))))

(defn roster-rooms
  "The room list inside a roster, in either shape. Call only when
   `roster-known?`."
  [roster]
  (if (map? roster) (vec (:rooms roster)) (vec roster)))

(defn personal-room
  "The party's personal-ai room, which the boot layout's placeholder tab names
   without knowing its id."
  [rooms]
  (first (filter #(= :personal-ai (:room/type %)) rooms)))

(defn reconcile-tab
  "Apply the roster's verdict to ONE tab — the tab-opened-second half.

   A no-op while the roster is unknown: the tab-opened-first half is then still
   ahead of it, and `reconcile-layout` will run when the roster lands. That is
   the whole order-independence argument — whichever fact is second triggers
   the same rules."
  [tab roster]
  (if-not (roster-known? roster)
    tab
    (let [rooms (roster-rooms roster)]
      (heal-chat-tab tab rooms (personal-room rooms)))))

(defn reconcile-layout
  "Apply the roster's verdict to every open tab — the roster-arrived-second
   half.

   Returns the ARGUMENT unchanged when nothing changed, identity included.
   `heal-chat-tabs` rebuilds the vectors either way, and this runs on every
   roster refresh — of which there is one per system-DB write that touches any
   roster. Handing spindel a fresh-but-equal layout on each of those is a
   re-render of every column for no new fact."
  [cols roster]
  (if-not (roster-known? roster)
    cols
    (let [rooms  (roster-rooms roster)
          healed (heal-chat-tabs cols rooms (personal-room rooms))]
      (if (= healed cols) cols healed))))
