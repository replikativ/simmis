(ns is.simm.uis.web.desktop.backlink-target
  "What a backlink row opens — decided from the row itself, before any DOM.

   Pure and dependency-free ON PURPOSE, for the same reason `tab-heal.cljc`
   and `routes.cljc` are: the rules are subtle enough to be worth stating as
   assertions, and `columns.cljc` cannot be loaded on the JVM at all.

   A backlink row is a query result, not a stored record — `query-backlinks`
   rebuilds the list from the db on every render. Two kinds reach the panel
   today, and both carry an entity uuid: `:page` (`:entity/uuid`) and
   `:chat-message` (`:room-uuid` plus `:message-uuid`). A third kind,
   `:chat-room`, carried only a DISPLAY NAME. Nothing in this repository has
   ever produced one, but the render branch for it did exist, and it opened a
   chat tab keyed on `{:room-name title}` alone.

   A display name is not an identity. Room names are not unique — two rooms
   may be called \"Design\", and a party may be a member of both, of one, or
   of neither. There is no correct room to open for such a row, so this
   namespace never tries to find one by name. It reports the row as
   unidentified, and the panel renders it as unavailable rather than as a
   control that opens an error tab every time it is clicked.

   THE TWO FAILURES ARE DIFFERENT, and only one of them belongs here:

     - NO IDENTITY (a name-only row) is a property of the row. It cannot be
       repaired by anything that happens later, so the verdict is final and
       can be passed at render time, with no roster in hand.
     - AN UNKNOWN ROOM ID (deleted, or never shared with this party) is a
       property of the ROSTER, and `tab-heal` already owns that judgement —
       revocably, because a room shared with you mid-session must heal. Such
       a row stays clickable and the tab it opens carries `:room-missing?`,
       which the chat view states plainly. Condemning it here would duplicate
       a verdict that this panel cannot revoke: the roster is a closure
       variable, invisible to `ifor-each`'s item diff, so a row rendered
       unavailable would stay unavailable after the roster changed.")

(defn room-scope
  "The content-db scope for `room-id` in a roster room list, or nil.

   The roster already carries the id→store mapping; every call site that
   opens a room by id needs it, and resolving by hand is what left the wiki
   backlink opening rooms with no replica to read (`refs/room-scope` is the
   same lookup against the live signal)."
  [rooms room-id]
  (when (some? room-id)
    (some->> rooms
             (filter #(= (str room-id) (str (:room/id %))))
             first
             :room/content-db-scope
             str)))

(def unavailable-explanations
  "Why a row cannot be opened, in the words the panel shows on hover."
  {:room-not-identified
   (str "This link records only a room name, not a room. "
        "Room names are not unique, so there is no room it can open.")
   :page-not-identified
   (str "This link records only a page title, not a page. "
        "There is no page it can open.")})

(defn explanation
  "The hover text for an `:unavailable` verdict."
  [reason]
  (get unavailable-explanations reason
       "This link does not record enough to open anything."))

(defn target
  "What clicking `backlink` does, as data.

   `scope` is the content-db scope the caller resolved for the row (see
   `room-scope`); it is passed in rather than looked up here so that the
   lookup can happen at CLICK time, against the roster as it is then.

   Returns either

     {:action :open :tab-type ... :tab-data {...} :title \"...\"}

   which is exactly the `open-tab!` call to make, or

     {:action :unavailable :reason ... :title \"...\"}

   Never resolves a room or a page by its display name."
  [{:keys [type title room-uuid message-uuid] :as backlink} scope]
  (case type
    (:chat-message :chat-room)
    (if (nil? room-uuid)
      {:action :unavailable :reason :room-not-identified :title (or title "Chat")}
      {:action :open
       :tab-type :chat
       :tab-data (cond-> {:room-id (str room-uuid)
                          :room-name title}
                   message-uuid (assoc :anchor-message (str message-uuid))
                   scope        (assoc :db-scope scope))
       :title (or title "Chat")})

    ;; Page backlink (default). `query-backlinks` pulls `:entity/uuid` and
    ;; `:S.Page/title`; a page with a title but no uuid used to open a wiki
    ;; tab on `{:page-uuid nil}`, which renders as an empty page and looks
    ;; like data loss rather than like a broken link.
    (let [page-title (or (:S.Page/title backlink) title "Untitled")
          page-uuid  (:entity/uuid backlink)]
      (if (nil? page-uuid)
        {:action :unavailable :reason :page-not-identified :title page-title}
        {:action :open
         :tab-type :wiki
         :tab-data (cond-> {:page-uuid page-uuid}
                     scope (assoc :db-scope scope))
         :title page-title}))))

(defn openable?
  "Does this row carry the identity its kind needs? Decidable without a
   roster, which is why the panel can settle it at render time."
  [backlink]
  (= :open (:action (target backlink nil))))

(defn render-key
  "A key that is stable across renders and UNIQUE within the panel.

   `ifor-each` memoizes on the key, so a key collision drops rows. The panel
   keyed every non-page row as `(str \"chat-\" title)` — a ROOM name — while
   `query-chat-message-backlinks` returns up to eight MESSAGES that are very
   often from the same room. Eight matches in one room rendered as one."
  [{:keys [type title room-uuid message-uuid] :as backlink}]
  (case type
    :chat-message (str "chatmsg-" room-uuid "-" message-uuid)
    :chat-room    (str "chatroom-" room-uuid "-" title)
    (str "page-" (:entity/uuid backlink))))
