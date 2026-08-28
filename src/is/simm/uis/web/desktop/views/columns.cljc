(ns is.simm.uis.web.desktop.views.columns
  "Column-based layout system.

   Features:
   - Dynamic columns with tabs
   - Drag and drop tabs between columns
   - Resize columns with drag handles
   - Context footer per tab (collapsible/resizable)
   - Drop zones at edges for creating new columns
   - Active column tracking for focused navigation"
  (:require [org.replikativ.spindel.dom.elements :as el]
            [org.replikativ.spindel.dom.foreach]
            [org.replikativ.spindel.dom.foreign]
            [org.replikativ.spindel.incremental.interval :as iv]
            [org.replikativ.spindel.effects.await :refer [await]]
            [org.replikativ.spindel.effects.track :refer [track]]
            [is.simm.uis.web.desktop.views.core :as vc]
            [is.simm.uis.web.desktop.routes :as routes]
            [is.simm.uis.web.desktop.views.chat :as chat]
            [is.simm.uis.web.desktop.views.settings :as settings-view]
            [is.simm.uis.web.desktop.views.profile :as profile-view]
            [is.simm.uis.web.desktop.views.admin :as admin-view]
            [is.simm.uis.web.desktop.views.new-room :as new-room-view]
            [is.simm.uis.web.desktop.views.new-kb :as new-kb-view]
            [is.simm.uis.web.desktop.views.new-contact :as new-contact-view]
            [is.simm.uis.web.desktop.views.room-settings :as room-settings-view]
            [is.simm.uis.web.desktop.views.files :as files-view]
            [is.simm.uis.web.desktop.views.mail :as mail-view]
            [is.simm.uis.web.desktop.views.kb-settings :as kb-settings-view]
            [is.simm.uis.web.desktop.views.sandbox-panel :as sandbox-panel]
            #?(:cljs [is.simm.uis.web.desktop.views.schedules :as schedules-view])
            #?(:cljs [is.simm.uis.web.desktop.views.proposals :as proposals-view])
            #?(:cljs [is.simm.uis.web.desktop.views.tasks :as tasks-view])
            #?(:cljs [is.simm.uis.web.desktop.views.timelines :as timelines-view])
            #?(:cljs [is.simm.uis.web.desktop.views.accounting :as accounting-view])
            #?(:cljs [is.simm.uis.web.desktop.views.add-source :as add-source-view])
            #?(:cljs [is.simm.uis.web.desktop.views.feed :as feed-view])
            #?(:cljs [is.simm.uis.web.desktop.views.history-subway :as subway])
            #?(:cljs [is.simm.uis.web.desktop.views.agent-inspector :as agent-inspector])
            [is.simm.uis.web.desktop.signals :as sig]
            [clojure.string :as str]
            #?(:cljs [datahike.api :as d])
            #?(:cljs [datahike.optimistic :as opt])
            #?(:cljs [clojure.core.async :refer [go <! put! promise-chan] :include-macros true])
            #?(:cljs [is.simm.uis.web.desktop.remote :as rem])
            #?(:cljs [is.simm.uis.web.desktop.chat-remote :as chat-remote])
            #?(:cljs [is.simm.uis.web.desktop.settings-remote :as settings-remote])
            #?(:cljs [is.simm.uis.web.desktop.admin-remote :as admin-remote])
            #?(:cljs [is.simm.uis.web.desktop.block-editor :as block-editor])
            #?(:cljs [org.replikativ.spindel.spin.core])
            #?(:cljs [org.replikativ.spindel.engine.core :as rtc])
            #?(:cljs [is.simm.uis.web.desktop.runtime :refer [runtime]])
            #?(:cljs [is.simm.uis.web.desktop.screen-share :as screen-share])
            #?(:cljs [is.simm.uis.web.desktop.db-signal :as db-sig])
            #?(:cljs [is.simm.uis.web.desktop.datahike-query :as dq])
            #?(:cljs [org.replikativ.spindel.incremental.combinators :as comb])
            #?(:cljs [is.simm.uis.web.desktop.chat-tiptap-input :as chat-input])
            #?(:cljs [is.simm.runtimes.web :as web]))
  #?(:cljs (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]]
                            [org.replikativ.spindel.dom.elements :as el]
                            [org.replikativ.spindel.dom.foreach :refer [ifor-each]]
                            [org.replikativ.spindel.dom.foreign :refer [foreign-node]]
                            [org.replikativ.spindel.incremental.combinators :refer [islice]])))

;; =============================================================================
;; Forward declaration for content rendering (will be in content.cljc)
;; =============================================================================

(declare render-tab-content)
(declare render-context-content)

;; =============================================================================
;; Room settings data loading
;; =============================================================================

#?(:cljs
   (defn- watch-delivery!
     "Report an unconfirmed chat prediction and a durable write whose store
      echo stalls. Pre-acceptance expiry retracts the message; after `ack!` the
      overlay deliberately keeps it visible and reports a reconciliation stall
      instead of pretending the accepted write failed."
     [overlay {:keys [ov-id result]}]
     (opt/listen-status!
      overlay ov-id
      (fn [{event-id :ov-id status :status :as event}]
        (when (= ov-id event-id)
          (cond
            (= :reconciliation-stalled status)
            (rem/report-error!
             "The message was accepted, but this replica has not received the
              server update yet. It will remain visible while synchronization
              catches up."
             (ex-info "Optimistic message reconciliation stalled" event))

            (contains? #{:reconciled :rejected :expired :abandoned} status)
            (opt/unlisten-status! overlay ov-id)

            :else nil))))
     (when result
       (go (let [r (<! result)]
             ;; A locally invalid prediction can settle before the status
             ;; listener is installed. Always clean up from the result too;
             ;; otherwise that missed terminal event leaks the listener.
             (when (contains? #{:reconciled :rejected :expired :abandoned}
                              (:status r))
               (opt/unlisten-status! overlay ov-id))
             (case (:status r)
               :expired
               (rem/report-error!
                "A message was not confirmed in time and has been removed
                 from the transcript. Check the room before sending it again."
                (ex-info "Optimistic message expired" r))

               :rejected
               (js/console.warn "[chat] optimistic prediction rejected locally" r)

               nil))))))

#?(:cljs (defonce ^:private room-details-loading (atom #{})))

#?(:cljs
   (defn load-room-details-into-signal!
     "Fire-and-forget load of a room's settings details into sig/admin-data.

      Runs the remote `load-room-details!` spin INSIDE a go block — not
      invoked directly from a render body. A spin invoked from inside a
      render body becomes a created-child of the render spin, so the next
      `invalidate-created-spins!` (which fires on every parent re-run)
      cancels it mid-flight and the result never lands. The go block runs
      outside any spin scope (`*spin-id*` unbound), so the spin is a root
      spin and survives parent re-renders. Idempotent per room-id —
      mirrors db-signal/connect-room! / connect-kb!."
     [room-id]
     (when (and room-id (not (contains? @room-details-loading room-id)))
       (swap! room-details-loading conj room-id)
       (go
         (let [ch (promise-chan)]
           (binding [rtc/*execution-context* runtime]
             (let [s (chat-remote/load-room-details! web/server-id room-id)]
               (s (fn [result] (put! ch {:ok result}))
                  (fn [err] (put! ch {:err err})))))
           (let [{:keys [ok err]} (<! ch)]
             (swap! room-details-loading disj room-id)
             (binding [rtc/*execution-context* runtime]
               (if err
                 (js/console.error "[room-settings] load error:" err)
                 (reset! sig/admin-data ok)))))))))

#?(:cljs (defonce ^:private settings-data-loading (atom false)))

#?(:cljs
   (defn load-settings-data-into-signal!
     "Fire-and-forget load of user settings into sig/settings-data.
      Root-spin go-block pattern — see load-room-details-into-signal!."
     [user-id]
     (when-not @settings-data-loading
       (reset! settings-data-loading true)
       (go
         (let [ch (promise-chan)]
           (binding [rtc/*execution-context* runtime]
             (let [s (settings-remote/load-settings! web/server-id user-id)]
               (s (fn [result] (put! ch {:ok result}))
                  (fn [err] (put! ch {:err err})))))
           (let [{:keys [ok err]} (<! ch)]
             (reset! settings-data-loading false)
             (binding [rtc/*execution-context* runtime]
               (if err
                 (js/console.error "[settings] load error:" err)
                 (do (reset! sig/settings-data ok)
                     (when-let [syn (get-in ok [:ui-prefs :ui-pref/syntax])]
                       (reset! sig/syntax-pref syn)))))))))))

#?(:cljs (defonce ^:private admin-data-loading (atom false)))

#?(:cljs
   (defn load-admin-data-into-signal!
     "Fire-and-forget load of admin data into sig/admin-data.
      Root-spin go-block pattern — see load-room-details-into-signal!."
     [user-id]
     (when-not @admin-data-loading
       (reset! admin-data-loading true)
       (go
         (let [ch (promise-chan)]
           (binding [rtc/*execution-context* runtime]
             (let [s (admin-remote/load-admin-data! web/server-id user-id)]
               (s (fn [result] (put! ch {:ok result}))
                  (fn [err] (put! ch {:err err})))))
           (let [{:keys [ok err]} (<! ch)]
             (reset! admin-data-loading false)
             (binding [rtc/*execution-context* runtime]
               (if err
                 (js/console.error "[admin] load error:" err)
                 (reset! sig/admin-data ok)))))))))

#?(:cljs (defonce ^:private new-room-humans-loading (atom false)))

#?(:cljs
   (defn load-new-room-humans-into-signal!
     "Fire-and-forget load of the human roster (member picker) into
      sig/admin-data. Root-spin go-block pattern — see
      load-room-details-into-signal!."
     []
     (when-not @new-room-humans-loading
       (reset! new-room-humans-loading true)
       (go
         (let [ch (promise-chan)]
           (binding [rtc/*execution-context* runtime]
             (let [s (chat-remote/list-parties! web/server-id :human)]
               (s (fn [result] (put! ch {:ok result}))
                  (fn [err] (put! ch {:err err})))))
           (let [{:keys [ok err]} (<! ch)]
             (reset! new-room-humans-loading false)
             (binding [rtc/*execution-context* runtime]
               (if err
                 (js/console.error "[new-room] list parties error:" err)
                 (reset! sig/admin-data {:all-humans ok})))))))))

#?(:cljs (defonce ^:private video-token-loading (atom #{})))

#?(:cljs
   (defn load-video-token-into-signal!
     "Fire-and-forget mint of a jitsi meeting token for room-id into
      sig/video-call-info. Same root-spin go-block pattern as
      load-room-details-into-signal! (a spin invoked from a render body
      would be cancelled by the next re-render). Idempotent per room-id."
     [room-id]
     (when (and room-id (not (contains? @video-token-loading room-id)))
       (swap! video-token-loading conj room-id)
       (go
         (let [ch (promise-chan)]
           (binding [rtc/*execution-context* runtime]
             (let [s (chat-remote/mint-video-token! web/server-id room-id)]
               (s (fn [result] (put! ch {:ok result}))
                  (fn [err] (put! ch {:err err})))))
           (let [{:keys [ok err]} (<! ch)]
             (binding [rtc/*execution-context* runtime]
               (if (and ok (= :ok (:status ok)))
                 (swap! sig/video-call-info assoc room-id ok)
                 (do (swap! video-token-loading disj room-id)
                     (js/console.error "[video] token mint failed:"
                                       (or err (clj->js ok))))))))))))

#?(:cljs (defonce ^:private room-app-loading (atom #{})))

#?(:cljs
   (defn load-room-app-status-into-signal!
     "Fire-and-forget fetch of a room's static-site status (slug + has-app?)
      into sig/room-app-status, so the chat header's Open-app button has the
      slug ready synchronously at click time (window.open after an async hop
      would be popup-blocked). Same root-spin go-block pattern as
      load-video-token-into-signal!. Idempotent per room-id; the slug is
      stable, so one fetch per room per session is enough."
     [room-id]
     ;; Only real rooms have a static site — skip placeholder ids like the
     ;; "personal-ai-placeholder" default tab (a non-uuid → guaranteed rpc
     ;; denial + console noise).
     (when (and room-id
                (re-matches #"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}"
                            (str room-id))
                (not (contains? @room-app-loading room-id)))
       (swap! room-app-loading conj room-id)
       (go
         (let [ch (promise-chan)]
           (binding [rtc/*execution-context* runtime]
             (let [s (chat-remote/room-app-status! web/server-id room-id)]
               (s (fn [result] (put! ch {:ok result}))
                  (fn [err] (put! ch {:err err})))))
           (let [{:keys [ok err]} (<! ch)]
             (binding [rtc/*execution-context* runtime]
               (if (and ok (= :ok (:status ok)))
                 (swap! sig/room-app-status assoc room-id ok)
                 (do (swap! room-app-loading disj room-id)
                     (js/console.error "[room-app] status fetch failed:"
                                       (or err (clj->js ok))))))))))))

#?(:cljs
   (defn search-screens-into-signal!
     "Load the screens gallery (latest frames, or fulltext hits for
      query) into sig/screens-results. Root-spin go block — same
      pattern as load-room-details-into-signal!."
     [room-id query]
     (binding [rtc/*execution-context* runtime]
       (swap! sig/screens-results update room-id assoc :loading? true :query query))
     (go
       (let [ch (promise-chan)]
         (binding [rtc/*execution-context* runtime]
           ;; owner-scoped now: the gallery is the CALLER's own screen stream,
           ;; not a room pool (doc/archive/screen-capture-scoping.md). room-id keys only
           ;; the UI signal, not the query.
           (let [s (chat-remote/search-screens! web/server-id (or query ""))]
             (s (fn [result] (put! ch {:ok result}))
                (fn [err] (put! ch {:err err})))))
         (let [{:keys [ok err]} (<! ch)]
           (binding [rtc/*execution-context* runtime]
             (if (and ok (= :ok (:status ok)))
               (swap! sig/screens-results assoc room-id
                      {:query query :items (:items ok) :loading? false})
               (do (swap! sig/screens-results update room-id assoc :loading? false)
                   (js/console.error "[screens] search failed:" (or err (clj->js ok)))))))))))

#?(:cljs
   (defn delete-screenshot!
     "Delete one of the caller's own frames by blob-id, then refresh the gallery."
     [room-id blob-id query]
     (go
       (let [ch (promise-chan)]
         (binding [rtc/*execution-context* runtime]
           (let [s (chat-remote/delete-screenshot! web/server-id blob-id)]
             (s (fn [r] (put! ch {:ok r})) (fn [e] (put! ch {:err e})))))
         (<! ch)
         (search-screens-into-signal! room-id (or query ""))))))

#?(:cljs
   (defn load-recordings-into-signal!
     "Load the caller's OWN recordings (owner-scoped) into sig/recordings-results."
     []
     (binding [rtc/*execution-context* runtime]
       (swap! sig/recordings-results assoc :loading? true))
     (go
       (let [ch (promise-chan)]
         (binding [rtc/*execution-context* runtime]
           (let [s (chat-remote/list-recordings! web/server-id)]
             (s (fn [r] (put! ch {:ok r})) (fn [e] (put! ch {:err e})))))
         (let [{:keys [ok err]} (<! ch)]
           (binding [rtc/*execution-context* runtime]
             (if (and ok (= :ok (:status ok)))
               (reset! sig/recordings-results {:sessions (:sessions ok) :loading? false})
               (do (swap! sig/recordings-results assoc :loading? false)
                   (js/console.error "[recordings] load failed:" (or err (clj->js ok)))))))))))

#?(:cljs
   (defn delete-recording!
     "Delete one of the caller's own recording sessions, then refresh."
     [session-id]
     (go
       (let [ch (promise-chan)]
         (binding [rtc/*execution-context* runtime]
           (let [s (chat-remote/delete-recording! web/server-id session-id)]
             (s (fn [r] (put! ch {:ok r})) (fn [e] (put! ch {:err e})))))
         (<! ch)
         (load-recordings-into-signal!)))))

#?(:cljs
   (defn search-pages-into-signal!
     "Load the caller's OWN captured web pages (owner-scoped) into
      sig/web-captures-results, optionally filtered by fulltext query."
     [query]
     (binding [rtc/*execution-context* runtime]
       (swap! sig/web-captures-results assoc :loading? true :query query))
     (go
       (let [ch (promise-chan)]
         (binding [rtc/*execution-context* runtime]
           (let [s (chat-remote/search-pages! web/server-id (or query ""))]
             (s (fn [r] (put! ch {:ok r})) (fn [e] (put! ch {:err e})))))
         (let [{:keys [ok err]} (<! ch)]
           (binding [rtc/*execution-context* runtime]
             (if (and ok (= :ok (:status ok)))
               (swap! sig/web-captures-results assoc :items (:items ok) :loading? false)
               (do (swap! sig/web-captures-results assoc :loading? false)
                   (js/console.error "[web] search failed:" (or err (clj->js ok)))))))))))

#?(:cljs
   (defn delete-page-and-refresh!
     "Delete one captured page, then reload the gallery preserving the query."
     [page-id query]
     (go
       (let [ch (promise-chan)]
         (binding [rtc/*execution-context* runtime]
           (let [s (chat-remote/delete-page! web/server-id page-id)]
             (s (fn [r] (put! ch {:ok r})) (fn [e] (put! ch {:err e})))))
         (<! ch)
         (search-pages-into-signal! (or query ""))))))

#?(:cljs
   (defn render-recordings
     "The caller's own screen recordings — each session's segments as playable
      <video> tiles, with a delete control. Owner-scoped, so it is not per room."
     [{:keys [sessions loading?]}]
     (el/div {:class "recordings-section"}
       (el/div {:class "recordings-header"}
         (el/h4 {:class "recordings-heading"} "Screen recordings")
         (el/span {:class "recording-note"
                   :title "Sharing your screen keeps a video recording here — yours to play back or delete."}
           "recorded while you share"))
       (cond
         loading?
         (el/p {:class "settings-loading"} "Loading recordings…")

         (empty? sessions)
         (el/p {:class "recordings-empty"}
           "No recordings yet — share your screen and the video segments will appear here.")

         :else
         (el/div {:class "recordings-list"}
           (ifor-each :session (vec sessions)
             (fn [{:keys [session started-at segments total-bytes]}]
               (el/div {:key session :class "recording-card"}
                 (el/div {:class "recording-meta"}
                   (el/span {:class "recording-time"}
                     (.toLocaleString (js/Date. started-at)))
                   (el/span {:class "recording-size"}
                     (str (.toFixed (/ total-bytes 1048576.0) 1) " MB · "
                          (count segments) " segment" (when (not= 1 (count segments)) "s")))
                   (el/button {:class "recording-delete"
                               :title "Delete this recording"
                               :on-click (fn [_]
                                           (when (js/confirm "Delete this recording?")
                                             (delete-recording! session)))}
                     "Delete"))
                 (el/div {:class "recording-segments"}
                   (ifor-each :blob-id (vec segments)
                     (fn [{:keys [blob-id]}]
                       (el/video {:key blob-id
                                  :class "recording-video"
                                  :controls true
                                  :preload "none"
                                  :src (str "/blobs/" blob-id)}))))))))))))

#?(:cljs
   (defn- host-tile-letter [host]
     (let [h (or host "")
           h (clojure.string/replace h #"^www\." "")]
       (clojure.string/upper-case (subs h 0 (min 2 (count h)))))))

#?(:cljs
   (defn render-web-captures
     "The caller's OWN captured web pages (owner-scoped). Rows, not thumbnails —
      captures are text. Search + host filter; each row opens the archived HTML
      or deletes. Agents read the same archive via web/*."
     [{:keys [items loading? query host]}]
     (let [hosts (->> items (map :host) (remove clojure.string/blank?) frequencies
                      (sort-by val >))
           shown (if host (filter #(= host (:host %)) items) items)
           run! (fn [] (let [el (js/document.getElementById "web-search")]
                         (search-pages-into-signal! (some-> el .-value))))]
       (el/div {:class "content-web"}
         (el/div {:class "web-toolbar"}
           (el/input {:id "web-search" :class "web-search-input" :type "text"
                      :placeholder "Search your captured pages…"
                      :on-keydown (fn [e] (when (= "Enter" (.-key e)) (run!)))})
           (el/button {:class "settings-btn settings-btn--primary" :on-click (fn [_] (run!))} "Search")
           (el/button {:class "settings-btn"
                       :on-click (fn [_]
                                   (when-let [el (js/document.getElementById "web-search")]
                                     (set! (.-value el) ""))
                                   (binding [rtc/*execution-context* runtime]
                                     (swap! sig/web-captures-results assoc :host nil))
                                   (search-pages-into-signal! ""))} "Latest"))
         (when (seq hosts)
           (el/div {:class "web-hosts"}
             (el/span {:class (str "web-chip" (when (nil? host) " active"))
                       :on-click (fn [_] (binding [rtc/*execution-context* runtime]
                                           (swap! sig/web-captures-results assoc :host nil)))}
               "All " (el/span {:class "n"} (str (count items))))
             (ifor-each :host (mapv (fn [[h c]] {:host h :count c :active? (= h host)}) hosts)
               (fn [{:keys [host count active?]}]
                 (el/span {:key host :class (str "web-chip" (when active? " active"))
                           :on-click (fn [_] (binding [rtc/*execution-context* runtime]
                                               (swap! sig/web-captures-results assoc :host host)))}
                   host " " (el/span {:class "n"} (str count)))))))
         (cond
           loading? (el/div {:class "settings-loading"} (el/p {} "Loading captures…"))
           (empty? items)
           (el/div {:class "empty-state"}
             (vc/icon "globe")
             (el/p {} (if (seq (str query))
                        (str "No captured pages match \"" query "\".")
                        "No captured pages yet — install the simmis Web Intake extension and browse an allowlisted site.")))
           :else
           (el/div {:class "web-list"}
             (ifor-each :id (vec shown)
               (fn [{:keys [id url title host at text blob-id]}]
                 (el/div {:key id :class "web-row"}
                   (el/div {:class "web-tile"} (host-tile-letter host))
                   (el/div {:class "web-body"}
                     (el/a {:class "web-title" :href url :target "_blank"} (or title url))
                     (el/div {:class "web-url"} url)
                     (el/div {:class "web-snip"} (some-> text (subs 0 (min 240 (count text))))))
                   (el/div {:class "web-aside"}
                     (el/span {:class "web-time"} (.toLocaleString (js/Date. at)))
                     (el/div {:class "web-actions"}
                       (el/a {:class "web-act" :href (str "/blobs/" blob-id) :target "_blank"} "Archive")
                       (el/button {:class "web-act del"
                                   :on-click (fn [_]
                                               (when (js/confirm "Delete this captured page?")
                                                 (delete-page-and-refresh! id query)))}
                         "Delete"))))))))))))

;; =============================================================================
;; Backlinks Query
;; =============================================================================

(defn find-ancestor-page
  "Walk up from a block until we find a page (entity with :S.Page/title).
   Returns the page entity-id or nil."
  [db block-id]
  #?(:cljs
     (loop [current block-id
            depth 0]
       (when (and current (< depth 20)) ;; Safety limit
         (let [parent (ffirst (d/q '[:find ?parent :in $ ?e :where [?e :block/parent ?parent]] db current))]
           (if parent
             (if (seq (d/q '[:find ?e :in $ ?e :where [?e :S.Page/title _]] db parent))
               parent ;; Found a page!
               (recur parent (inc depth)))
             nil))))
     :clj nil))

(defn query-chat-message-backlinks
  "Query chat messages that contain [[PageTitle]] or @handle references.
   Returns a list of maps with message info and containing room.
   Works for both page references and user mention backlinks."
  [db page-title & [db-scope]]
  #?(:cljs
     (when (and db page-title)
       (try
         ;; Search for both [[PageTitle]] and @handle patterns
         ;; This allows user pages (handles) to show backlinks from @mentions
         (let [page-pattern (str "[[" page-title "]]")
               ;; For @mentions: match @handle followed by punctuation, space, or end
               ;; Use regex to properly match @handle boundaries
               mention-regex (js/RegExp. (str "@" page-title "(?=[,.\\s!?;:]|$)"))
               ;; Find all messages (entities with S/Message type)
               messages (d/q '[:find ?content ?room-name ?sent-at ?mu ?ru
                               :keys content room-name sent-at message-uuid room-uuid
                               :where
                               [?msg :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-00000000002b"]]
                               [?msg :block/content ?content]
                               [?msg :S.Message/room ?room]
                               [?room :S.ChatRoom/name ?room-name]
                               [?msg :S.Message/sent-at ?sent-at]
                               [?msg :entity/uuid ?mu]
                               [?room :entity/uuid ?ru]]
                             db)
               ;; Filter messages that contain the page reference OR @mention.
               ;; MESSAGE-level results (newest first, capped) so backlinks can
               ;; jump to the exact chat position via :anchor-message.
               matching (->> messages
                             (filter (fn [{:keys [content]}]
                                       (and (string? content)
                                            (or (str/includes? content page-pattern)
                                                (.test mention-regex content)))))
                             (sort-by :sent-at #(compare %2 %1))
                             (take 8)
                             (map (fn [{:keys [room-name sent-at message-uuid room-uuid]}]
                                    (cond-> {:type :chat-message
                                             :title room-name
                                             :sent-at sent-at
                                             :message-uuid (str message-uuid)
                                             :room-uuid (str room-uuid)}
                                      db-scope (assoc :db-scope db-scope))))
                             vec)]
           matching)
         (catch :default e
           (js/console.error "[columns] Failed to query chat backlinks:" e)
           [])))
     :clj []))

(defn query-backlinks
  "Query pages and chat rooms that link to the given page UUID.
   Traverses up the block parent chain to find the containing page.
   Also searches chat messages for [[PageTitle]] references."
  [db page-uuid & [room-states]]
  #?(:cljs
     (when (and db page-uuid)
       (try
         ;; First check if the page entity exists
         (let [page-exists? (seq (d/q '[:find ?e
                                        :in $ ?uuid
                                        :where [?e :entity/uuid ?uuid]]
                                      db page-uuid))]
           (if-not page-exists?
             ;; Page doesn't exist yet, return empty backlinks
             []
             ;; Page exists, query backlinks
             (let [refs (d/q '[:find ?block ?target
                               :in $ ?target-uuid
                               :where
                               [?target :entity/uuid ?target-uuid]
                               [?block :block/references ?target]]
                             db page-uuid)
                   ;; For each referencing block, walk up to find containing page
                   block-ids (map first refs)
                   page-ids (->> block-ids
                                 (map #(find-ancestor-page db %))
                                 (filter some?)
                                 distinct)
                   page-backlinks (when (seq page-ids)
                                    (vec (for [pid page-ids]
                                           (let [page (d/pull db [:entity/uuid :S.Page/title] pid)]
                                             (assoc page :type :page)))))
                   ;; Also find chat message backlinks by searching content
                   page-title (some->> (d/pull db [:S.Page/title] [:entity/uuid page-uuid])
                                       :S.Page/title)
                   ;; Messages live in per-room CONTENT DBs, not the page's
                   ;; db — sweep the CONNECTED room replicas (client-local;
                   ;; rooms not yet opened this session aren't searched).
                   chat-backlinks (when page-title
                                    (->> (concat
                                          (query-chat-message-backlinks db page-title)
                                          (mapcat (fn [[scope st]]
                                                    (when-let [rdb (:db st)]
                                                      (query-chat-message-backlinks rdb page-title scope)))
                                                  (or room-states {})))
                                         (sort-by :sent-at #(compare %2 %1))
                                         (take 8)
                                         vec))]
               (vec (concat (or page-backlinks []) (or chat-backlinks []))))))
         (catch :default e
           (js/console.error "[columns] Failed to query backlinks:" e)
           [])))
     :clj []))

;; =============================================================================
;; Tab Icons
;; =============================================================================

(defn tab-icon
  "Get the Lucide icon name for a tab type."
  [tab-type]
  (case tab-type
    :home "home"
    :wiki "file-text"
    :chat "message-square"
    :chat-thread "messages-square"
    :video "video"
    :screens "images"
    :feed "activity"
    :files "folder"
    :mail "mail"
    :schedules "calendar"
    :proposals "git-pull-request"
    :tasks "circle-check"
    :timelines "git-branch"
    :accounting "wallet"
    :add-memory "plus-circle"
    :add-team "plus-circle"
    :profile "at-sign"
    :settings "settings"
    :admin "shield"
    :new-room "plus-circle"
    :new-kb "plus-circle"
    :new-contact "user-plus"
    :contact "user"
    :room-settings "sliders"
    :kb-settings "database"
    :agent "bot"
    "file"))

;; =============================================================================
;; Tab Bar
;; =============================================================================

(defn render-tab
  "Render a single tab in the tab bar.

   `live-title` is the title resolved at render-column time from the
   per-KB db signal (a map lookup against (:id tab)). It's set for
   wiki tabs whose :db-scope matches the column's active-tab db-scope;
   nil otherwise. We prefer it over the stored :title — the stored
   title is only a fallback for the boot window before the signal
   warms up, for non-:wiki tabs, or for tabs from a different KB."
  [col-id {:keys [id type title data]} active? live-title]
  (let [display-title (or live-title title)
        ;; Only ADDRESSABLE tabs get a link button. A tab with no route (the
        ;; settings pane, the proposals list) simply does not offer one, which
        ;; says "this has no address" without a disabled control or a tooltip.
        share-path (routes/tab->route {:type type :data data})]
    ;; Use compound key (col-id + tab-id) to ensure unique keys across columns
    ;; This helps Spindel's delta rendering track elements correctly during drag
    (el/div {:key (str col-id "-" id)
             :class (vc/class-names "tab" (when active? "active"))
             :draggable "true"
             :on-click (fn [_]
                         #?(:cljs (sig/set-active-tab! col-id id) :clj nil))
             :on-dragstart (fn [e]
                             #?(:cljs
                                (do
                                  (.setData (.-dataTransfer e) "text/plain" "")
                                  (set! (.-effectAllowed (.-dataTransfer e)) "move")
                                  ;; Arms the edge drop zones: they are
                                  ;; pointer-events none outside a drag so they
                                  ;; don't swallow clicks on header buttons
                                  ;; underneath (e.g. Room settings).
                                  (.add (.-classList js/document.body) "tab-dragging")
                                  (sig/start-drag! col-id id)
                                  ;; Also store for cross-component access
                                  (set! (.-dragData js/window)
                                        {:type :tab
                                         :source-col col-id
                                         :tab-id id
                                         :content-type type
                                         :title display-title}))
                                :clj nil))
             :on-dragend (fn [_]
                           #?(:cljs
                              (do
                                ;; dragend fires on the source even when the
                                ;; drop happened elsewhere or was cancelled —
                                ;; the one reliable place to disarm the zones.
                                (.remove (.-classList js/document.body) "tab-dragging")
                                (sig/cancel-drag!)
                                ;; Ensure dragData is cleared even if drop didn't happen
                                (set! (.-dragData js/window) nil))
                              :clj nil))}
      (vc/icon (tab-icon type) {:class "tab-icon"})
      (el/span {:class "tab-title"} display-title)
      (when share-path
        (el/button {:class "tab-link"
                    :on-click (fn [e]
                                (.stopPropagation e)
                                #?(:cljs
                                   (let [url (str js/window.location.origin share-path)]
                                     ;; Absolute, so it survives being pasted
                                     ;; somewhere that is not this origin.
                                     (-> (.writeText js/navigator.clipboard url)
                                         (.catch (fn [err]
                                                   (js/console.error
                                                     "[tabs] could not copy link:" err)))))
                                   :clj nil))
                    :title "Copy link to this"}
          (vc/icon "link" {:class "tab-link-icon"})))
      (el/button {:class "tab-close"
                  :on-click (fn [e]
                              (.stopPropagation e)
                              #?(:cljs (sig/close-tab! col-id id) :clj nil))
                  :title "Close tab"}
        (vc/icon "x" {:class "tab-close-icon"})))))

(defn render-tab-bar
  "Render the tab bar for a column.

   `wiki-titles` is a {tab-id → live-title} map resolved upstream in
   render-column's spin body (where `track` is legal). render-tab uses
   it to display the current page title for :wiki tabs, falling back
   to the stored :title otherwise."
  [col-id tabs active-tab-id wiki-titles]
  (el/div {:key (str col-id "-tab-bar")
           :class "tab-bar"
           :on-dragover (fn [e]
                          (.preventDefault e)
                          #?(:cljs
                             (sig/update-drop-target!
                               {:type :tab-bar :col-id col-id})
                             :clj nil))
           :on-dragleave (fn [_]
                           #?(:cljs (sig/update-drop-target! nil) :clj nil))
           :on-drop (fn [e]
                      (.preventDefault e)
                      #?(:cljs
                         (let [drag-data (.-dragData js/window)]
                           (when drag-data
                             (case (:type drag-data)
                               :tab
                               (when (not= (:source-col drag-data) col-id)
                                 (sig/move-tab! (:source-col drag-data)
                                                (:tab-id drag-data)
                                                col-id))
                               :nav
                               (sig/open-tab! (:content-type drag-data)
                                              (cond-> {:content-id (:content-id drag-data)}
                                                (:page-uuid drag-data) (assoc :page-uuid (:page-uuid drag-data))
                                                (:db-scope drag-data) (assoc :db-scope (:db-scope drag-data)))
                                              {:title (:title drag-data)
                                               :col-id col-id})
                               nil)
                             (set! (.-dragData js/window) nil)))
                         :clj nil))}
    ;; Use ifor-each for proper keyed addressing of tabs.
    ;; NOTE: ifor-each memoizes per key on ITEM equality — closure
    ;; variables (active-tab-id, wiki-titles) are invisible to it, so
    ;; anything that must trigger a re-render has to live IN the item.
    ;; (Stale-active-tab bug: every tab stayed .active forever.)
    (let [tab-items (mapv (fn [tab]
                            (assoc tab
                                   :active? (= (:id tab) active-tab-id)
                                   :live-title (get wiki-titles (:id tab))))
                          tabs)]
      #?(:cljs
         (ifor-each :id tab-items
           (fn [tab]
             (render-tab col-id tab (:active? tab) (:live-title tab))))
         :clj
         (for [tab tab-items]
           (render-tab col-id tab (:active? tab) (:live-title tab)))))))

;; =============================================================================
;; Context Footer
;; =============================================================================

(defn render-context-footer
  "Render the collapsible context footer for a column.

   The context content varies by tab type:
   - :wiki -> Backlinks, Outline
   - :chat -> Members, Pinned
   - :video -> Participants, Recordings
   - :feed -> Filters"
  [col-id active-tab collapsed? height local-db room-states]
  (when active-tab
    (let [tab-type (:type active-tab)
          tab-id (:id active-tab)
          ;; Unique key prefix for this column+tab combination
          key-prefix (str col-id "-" tab-id)]
      (el/div {:key (str key-prefix "-context-footer")
               :class (vc/class-names "context-footer"
                                      (when collapsed? "collapsed"))
               :style (when (and (not collapsed?) height)
                        {:height (str height "px")
                         :max-height (str height "px")})}
        ;; Resize handle (drag persists to the signal so it survives
        ;; re-renders; live inline styles keep the drag smooth). Hidden
        ;; via CSS when collapsed.
        (el/div {:key (str key-prefix "-resize")
                 :class "context-footer-resize"
                 :on-mousedown (fn [e]
                                 (.preventDefault e)
                                 #?(:cljs
                                    (when-not collapsed?
                                      (let [start-y (.-clientY e)
                                            footer (.closest (.-target e) ".context-footer")
                                            start-height (.-offsetHeight footer)
                                            last-height (atom start-height)]
                                        (letfn [(on-move [me]
                                                  (let [delta-y (- start-y (.-clientY me))
                                                        new-height (max 80 (min 400 (+ start-height delta-y)))]
                                                    (reset! last-height new-height)
                                                    (set! (.-height (.-style footer)) (str new-height "px"))
                                                    (set! (.-maxHeight (.-style footer)) (str new-height "px"))))
                                                (on-up [_]
                                                  (.removeEventListener js/document "mousemove" on-move)
                                                  (.removeEventListener js/document "mouseup" on-up)
                                                  (sig/set-context-footer-height! col-id @last-height))]
                                          (.addEventListener js/document "mousemove" on-move)
                                          (.addEventListener js/document "mouseup" on-up))))
                                    :clj nil))})
        ;; Header (click to collapse/expand — state lives in the signal)
        (el/div {:key (str key-prefix "-header")
                 :class "context-footer-header"
                 :on-click (fn [_]
                             #?(:cljs (sig/toggle-context-footer! col-id)
                                :clj nil))}
          (vc/icon "chevron-down" {:class "context-footer-toggle"})
          (el/span {:class "context-footer-title"} "Context"))
        ;; Content
        (el/div {:key (str key-prefix "-content")
                 :class "context-footer-content"}
          (render-context-content key-prefix tab-type (:data active-tab) local-db room-states))))))

;; =============================================================================
;; Column
;; =============================================================================

#?(:cljs
   (defn render-call-hosts
     "Live video calls for THIS column — mounted OUTSIDE the tab-content slot.

      A call cannot be re-mounted. An <iframe>'s browsing context is destroyed the
      moment the element leaves the document, so rendering the call inside the tab
      body meant that switching tabs KILLED the call and coming back rejoined it.
      Keeping the element mounted and merely HIDDEN preserves the whole thing:
      WebRTC stays connected, audio keeps flowing, Jitsi pauses its own outbound
      video while hidden. Verified in Chromium — display:none on a CONNECTED host
      preserves the browsing context; removal does not.

      So the host lives here, as a sibling of the tab content, and a tab switch
      swaps the CONTENT slot while this one is left alone. Reordering columns is
      safe too, now that spindel's move-child! uses Element.moveBefore (a move
      rather than a destroy-and-recreate).

      `:active?` lives IN the item, not in a closure — the reconciler diffs by
      value, and a flag it cannot see is a flag that never re-renders."
     [tabs active-tab video-info]
     (let [calls (->> tabs
                      (filter #(= :video (:type %)))
                      (keep (fn [tab]
                              (let [room-id (get-in tab [:data :room-id])
                                    info    (get video-info room-id)]
                                (when info
                                  {:tab-id  (:id tab)
                                   :room-id room-id
                                   :info    info
                                   :subject (get-in tab [:data :room-name])
                                   :active? (= (:id tab) active-tab)})))))]
       (map (fn [{:keys [tab-id room-id info subject active?]}]
              (el/div {:key (str "call-" room-id)
                       :class (vc/class-names "call-host"
                                              (when-not active? "call-host--hidden"))}
                (el/element :iframe
                  {:key (str "call-frame-" room-id)
                   :class "video-call-frame"
                   :src (str (:url info) "/" (:room info) "?jwt=" (:jwt info)
                             "#config.prejoinConfig.enabled=false"
                             "&config.disableDeepLinking=true"
                             (when subject
                               (str "&config.subject=" (js/encodeURIComponent (pr-str subject)))))
                   :allow "camera; microphone; display-capture; autoplay; clipboard-write"
                   :allowfullscreen "true"})))
            calls))))

(defn render-column
  "Render a single column with tab bar, content, and context footer.
   Returns a spin (CLJS) or vnode (CLJ)."
  [{:keys [id width tabs active-tab _index _total] :as _col} active-column-id local-db chat-windows settings-data admin-data room-states-arg footer-states]
  #?(:cljs
     ;; CLJS: Wrap in spin since render-tab-content returns vnodes inside spin context
     (spin
       (let [;; Own this dependency in the keyed child spin. A plain map passed
             ;; from the parent can leave a retained child holding its initial
             ;; nil snapshot after the room handshake completes.
             room-states (iv/get-new (track db-sig/room-states))
             active-tab-data (first (filter #(= (:id %) active-tab) tabs))
             is-active? (= id active-column-id)
             index _index
             total-columns _total
             ;; Track syntax-pref so chat messages re-mount when pref changes
             syntax-pref-iv (track sig/syntax-pref)
             syntax-pref (iv/get-new syntax-pref-iv)
             ;; GlobalCut: tracked HERE so the CHAT branch (rendered inline
             ;; in this spin — no self-tracking child) resolves its room db
             ;; to as-of(T). For a wiki-tab column this re-run is a cheap
             ;; editor cache-hit (render-page-editor self-tracks global-ref
             ;; and owns its subtree); chat stays a SINGLE lineage owned by
             ;; render-column, so the consume-once query cache is fed once.
             gref (iv/get-new (track sig/global-ref))
             ;; Meeting tokens for :video tabs (map room-id → {:url :room
             ;; :jwt}); tracked at the spin top like every other signal —
             ;; the :video branch re-renders when its mint lands.
             video-info (iv/get-new (track sig/video-call-info))
             ;; Active local screen shares (set of room-ids) — the room
             ;; header's share toggle reflects it.
             screen-sharing (iv/get-new (track sig/screen-sharing))
             ;; Screens gallery data per room (:screens tabs re-render
             ;; when a search lands).
             screens-results (iv/get-new (track sig/screens-results))
             ;; The caller's own recordings (owner-scoped, not per room) —
             ;; tracked here so the recordings section re-renders on load/delete.
             recordings-results (iv/get-new (track sig/recordings-results))
             ;; The caller's own web captures (owner-scoped) — the :web-captures tab.
             web-captures-results (iv/get-new (track sig/web-captures-results))
             ;; Per-room reply composer target. This belongs to the Spindel
             ;; execution context so UI forks do not share an ambient atom.
             chat-reply-targets (iv/get-new (track sig/chat-reply-targets))
             ;; Track the commit graph so the History subway (a plain fn in
             ;; the context footer) re-renders when a graph loads. Value
             ;; unused here — the subway bare-derefs it.
             _cg (iv/get-new (track sig/commit-graph-data))
             ;; Context footer UI state (collapsed by default) —
             ;; tracked at the main spin and passed down; tracking it
             ;; here mid-spin blanked the chat on toggle (see main.cljs).
             footer-state (get footer-states id)
             footer-collapsed? (:collapsed? footer-state true)
             footer-height (:height footer-state)
             ;; Resolve the right db value for the active tab via tracking
             ;; the appropriate per-KB signal (or shared local-db). Doing
             ;; this in the render-column spin (which IS a spin) means
             ;; track works correctly; downstream context-footer doesn't
             ;; need to be a spin and just receives the resolved value.
             ;; Note: render-page-editor self-tracks INDEPENDENTLY of this,
             ;; so a KB write re-runs render-page-editor AND this column
             ;; (which is desired — both regions display KB data).
             tab-db-scope (get-in active-tab-data [:data :db-scope])
             ;; A tab with a `:db-scope` reads ITS store. One without —
             ;; Feed, Tasks, Settings — reads no store at all, and used to be
             ;; handed the app store here "just in case". That is what a
             ;; fallback is: a default that turns "this tab has no database"
             ;; into "here is a database, possibly the wrong one".
             tab-db (when (and active-tab-data tab-db-scope)
                      (iv/get-new (track (db-sig/ensure-kb-db-signal! tab-db-scope))))
             ;; Resolve live :S.Page/title for any :wiki tab whose
             ;; :db-scope matches the column's active tab-db-scope (so
             ;; the lookup is against the db we already tracked). Tabs
             ;; from a different KB or with no :db-scope fall back to
             ;; their stored :title in render-tab. track is illegal in
             ;; plain defns; doing the lookup HERE in the spin body
             ;; keeps the resolution reactive on rename without needing
             ;; multiple dynamic tracks.
             wiki-titles (when (and tab-db tab-db-scope)
                           (into {}
                                 (keep (fn [tab]
                                         (when (and (= :wiki (:type tab))
                                                    (= tab-db-scope (get-in tab [:data :db-scope])))
                                           (when-let [page-uuid (get-in tab [:data :page-uuid])]
                                             (let [pulled (d/q '[:find (pull ?e [:S.Page/title :entity/name]) .
                                                                 :in $ ?uuid
                                                                 :where [?e :entity/uuid ?uuid]]
                                                               tab-db page-uuid)
                                                   live (or (:S.Page/title pulled)
                                                            (:entity/name pulled))]
                                               (when live [(:id tab) live])))))
                                       tabs)))
             tab-result (if active-tab-data
                           (render-tab-content (:type active-tab-data) (:data active-tab-data) local-db chat-windows settings-data admin-data room-states syntax-pref gref video-info screen-sharing screens-results recordings-results web-captures-results chat-reply-targets)
                           (el/div {:class "empty-state"}
                             (vc/icon "layout-grid")
                             (el/h3 {} "No content")
                             (el/p {} "Open a page from the sidebar.")))
             ;; render-tab-content may return a spin (e.g. :wiki) or a plain vnode
             ;; Await spins, pass through plain vnodes
             tab-content (if (instance? org.replikativ.spindel.spin.core/Spin tab-result)
                           (await tab-result)
                           tab-result)]
         (el/div {:key id
                  :class (vc/class-names "column" (when is-active? "active"))
                  :data-col-id id
                  :style {:width (str (* width 100) "%")}
                  :on-click (fn [_]
                              (when (not= id @sig/active-column-id)
                                (sig/set-active-column! id)))}
           ;; Tab bar
           (render-tab-bar id tabs active-tab wiki-titles)

           ;; Content area. The call hosts are SIBLINGS of the tab content, not
           ;; children of it: a tab switch replaces the content slot and leaves a
           ;; live call untouched (see render-call-hosts).
           (el/div {:class "column-content"}
             tab-content
             #?(:cljs (render-call-hosts tabs active-tab video-info) :clj nil))

           ;; Context footer — receives the db value resolved above
           ;; (per-KB signal for wiki tabs, shared local-db for non-KB).
           (render-context-footer id active-tab-data
                                  footer-collapsed? footer-height
                                  tab-db room-states)

           ;; Resize handle (not on last column)
           (when (< index (dec total-columns))
             (el/div {:class "column-resize-handle"
                      :on-mousedown (fn [e]
                                      (.preventDefault e)
                                      (let [start-x (.-clientX e)
                                            container (.closest (.-target e) ".columns-container")
                                            container-width (.-offsetWidth container)
                                            columns @sig/layout-columns
                                            col-idx index
                                            start-width (:width (nth columns col-idx))
                                            pending-width (atom nil)
                                            raf-id (atom nil)]
                                        (letfn [(apply-resize []
                                                  (when-let [w @pending-width]
                                                    (sig/resize-column! id w)
                                                    (reset! pending-width nil)))
                                                (on-move [me]
                                                  (let [delta-x (- (.-clientX me) start-x)
                                                        delta-pct (/ delta-x container-width)
                                                        new-width (max 0.15 (min 0.85 (+ start-width delta-pct)))]
                                                    (reset! pending-width new-width)
                                                    (when-not @raf-id
                                                      (reset! raf-id
                                                              (js/requestAnimationFrame
                                                                (fn []
                                                                  (reset! raf-id nil)
                                                                  (apply-resize)))))))
                                                (on-up [_]
                                                  (.removeEventListener js/document "mousemove" on-move)
                                                  (.removeEventListener js/document "mouseup" on-up)
                                                  (when @raf-id
                                                    (js/cancelAnimationFrame @raf-id))
                                                  (apply-resize))]
                                          (.addEventListener js/document "mousemove" on-move)
                                          (.addEventListener js/document "mouseup" on-up))))})))))

     :clj
     ;; CLJ: Simple vnodes
     (let [room-states room-states-arg
           active-tab-data (first (filter #(= (:id %) active-tab) tabs))
           is-active? (= id active-column-id)
           index _index
           total-columns _total]
       (el/div {:key id
                :class (vc/class-names "column" (when is-active? "active"))
                :data-col-id id
                :style {:width (str (* width 100) "%")}
                :on-click (fn [_] nil)}
         (render-tab-bar id tabs active-tab nil)
         (el/div {:class "column-content"}
           (if active-tab-data
             (render-tab-content (:type active-tab-data) (:data active-tab-data) local-db chat-windows nil nil nil)
             (el/div {:class "empty-state"}
               (vc/icon "layout-grid")
               (el/h3 {} "No content")
               (el/p {} "Open a page from the sidebar."))))
         (render-context-footer id active-tab-data true nil local-db nil)
         (when (< index (dec total-columns))
           (el/div {:class "column-resize-handle"
                    :on-mousedown (fn [_] nil)}))))))

;; =============================================================================
;; Drop Zones
;; =============================================================================

(defn render-drop-zone
  "Render a drop zone at the edge of the columns container."
  [position]
  (el/div {:key (str "drop-zone-" (name position))
           :class (str "column-drop-zone " (name position))
           :data-position (name position)
           :on-dragover (fn [e]
                          (.preventDefault e)
                          #?(:cljs
                             (do
                               (.add (.-classList (.-currentTarget e)) "active")
                               (sig/update-drop-target!
                                 {:type :new-column :position position}))
                             :clj nil))
           :on-dragleave (fn [e]
                           #?(:cljs
                              (do
                                (.remove (.-classList (.-currentTarget e)) "active")
                                (sig/update-drop-target! nil))
                              :clj nil))
           :on-drop (fn [e]
                      (.preventDefault e)
                      #?(:cljs
                         (let [drag-data (.-dragData js/window)]
                           (when drag-data
                             (.remove (.-classList (.-currentTarget e)) "active")
                             (case (:type drag-data)
                               :tab
                               (sig/create-column-with-tab!
                                 (:source-col drag-data)
                                 (:tab-id drag-data)
                                 (if (= position :left) :start :end))
                               :nav
                               (sig/open-tab! (:content-type drag-data)
                                              (cond-> {:content-id (:content-id drag-data)}
                                                (:page-uuid drag-data) (assoc :page-uuid (:page-uuid drag-data))
                                                (:db-scope drag-data) (assoc :db-scope (:db-scope drag-data)))
                                              {:title (:title drag-data)
                                               :new-column? true})
                               nil)
                             (set! (.-dragData js/window) nil)))
                         :clj nil))}))

;; =============================================================================
;; Columns Container
;; =============================================================================

(defn render-columns-container
  "Render the columns container with all columns and drop zones.
   Returns a spin (CLJS) or vnode (CLJ).

   kb-states arg removed — wiki tabs self-track per-KB signals inside
   render-page-editor / render-context-content."
  [columns active-column-id local-db chat-windows settings-data admin-data room-states footer-states]
  #?(:cljs
     ;; CLJS: Wrap in spin to support await on ifor-each-spin
     (spin
       (let [n (count columns)
             indexed-columns (vec (map-indexed
                                    (fn [idx col]
                                      (assoc col :_index idx :_total n))
                                    columns))]
         (el/div {:key "columns-container"
                  :class "columns-container"
                  :id "columns-container"}
           ;; Left drop zone
           (render-drop-zone :left)

           ;; Columns - ifor-each auto-detects and awaits spins from render-fn
           (await (ifor-each :id indexed-columns
                    (fn [col]
                      (render-column col active-column-id local-db chat-windows settings-data admin-data room-states footer-states))))

           ;; Right drop zone
           (render-drop-zone :right))))

     :clj
     ;; CLJ: Simple vnodes
     (let [n (count columns)
           indexed-columns (vec (map-indexed
                                  (fn [idx col]
                                    (assoc col :_index idx :_total n))
                                  columns))]
       (el/div {:key "columns-container"
                :class "columns-container"
                :id "columns-container"}
         (render-drop-zone :left)
         (for [col indexed-columns]
           (render-column col active-column-id local-db chat-windows nil nil nil nil nil))
         (render-drop-zone :right)))))

;; =============================================================================
;; Content Rendering (placeholder - will be replaced by content.cljc)
;; =============================================================================

(defn render-tab-content
  "Render content for a tab based on its type.

   kb-states arg removed — wiki tabs self-track their KB signal."
  [tab-type data local-db chat-windows settings-data admin-data room-states & [syntax-pref gref video-info screen-sharing screens-results recordings-results web-captures-results chat-reply-targets]]
  (case tab-type
    :home
    ;; Newcomer landing: the obvious first action is talking to your
    ;; agents — one prominent entry into My Agents, secondary hint to
    ;; the sidebar. (ChatGPT-style: land people in a conversation.)
    (el/div {:class "content-home"}
      (el/h2 {} "Welcome to Simmis")
      (el/p {} "Your workspace with AI staff that remembers everything.")
      #?(:cljs
         (el/button {:class "home-start-chat"
                     :on-click
                     (fn [_]
                       (let [ur @sig/user-rooms
                             room (or (->> (:rooms ur)
                                           (filter #(= :personal-ai (:room/type %)))
                                           first)
                                       (first (:rooms ur)))]
                         (when room
                           (sig/open-tab! :chat
                                          {:room-id (str (:room/id room))
                                           :room-name (:room/name room)
                                           :db-scope (:room/content-db-scope room)}
                                          {:title (:room/name room)}))))}
           "Start chatting with your agents →")
         :clj nil)
      (el/p {:class "home-hint"}
        "Or open a page or room from the sidebar."))

    :wiki
    #?(:cljs
       (if-let [page-uuid (:page-uuid data)]
         (let [db-scope (:db-scope data)
               ;; Trigger KB connection if needed (fire-and-forget — the
               ;; per-KB signal is created lazily inside render-page-editor
               ;; via ensure-kb-db-signal!, so the spin tracks the right
               ;; signal even before connect-kb! finishes).
               _ (when (and db-scope (nil? (db-sig/kb-db-signal db-scope)))
                   (db-sig/connect-kb! db-scope @is.simm.runtimes.web/client))]
           ;; render-page-editor tracks its KB's signal itself. We no
           ;; longer build an effective-db Interval here, so the parent
           ;; spin's body doesn't depend on kb-states at all for this
           ;; branch.
           (block-editor/render-page-editor page-uuid db-scope))
         (el/div {:class "content-wiki"}
           (el/p {} "No page selected.")))
       :clj
       (el/div {:class "content-wiki"}
         (el/p {} "Wiki content (CLJ mode)")))

    :chat-thread
    ;; A focused thread is a projection of the same Room, not a second chat
    ;; implementation. Keep every dispatch, optimistic overlay, and renderer
    ;; path shared; `:thread-view?` only scopes the query and presentation.
    (render-tab-content :chat (assoc data :thread-view? true)
                        local-db chat-windows settings-data admin-data room-states
                        syntax-pref gref video-info screen-sharing screens-results
                        recordings-results web-captures-results chat-reply-targets)

    :chat
    #?(:cljs
       ;; All chats (including Vár AI) use messages from the room's own Datahike DB
       (let [room-id (or (:room-id data) "11111111-1111-1111-1111-111111111111")
             room-name (or (:room-name data) "Chat")
             room-uuid (uuid room-id)
             room-db-scope (:db-scope data)
             thread-view? (boolean (:thread-view? data))
             thread-root-id (when-let [root (:thread-root-id data)]
                              (if (uuid? root) root (uuid (str root))))
             chat-context-key (if thread-view?
                                [room-uuid thread-root-id]
                                room-uuid)

             ;; Trigger room DB connection if we have a db-scope (fire-and-forget)
             _ (when (and room-db-scope
                          (not (get room-states (str room-db-scope))))
                 (db-sig/connect-room! room-db-scope @is.simm.runtimes.web/client))

             ;; Get the room's DB from room-states (nil while connecting)
             room-db (when room-db-scope (get-in room-states [(str room-db-scope) :db]))

             ;; Ensure room chatroom + user entities exist in room DB (side effect)
             ;; Guard against placeholder room-id before real room is loaded
             _ (when (and (string? room-id)
                          (not= room-id "personal-ai-placeholder"))
                 (when-let [user @sig/current-user]
                   (let [s (chat-remote/ensure-room!
                             is.simm.runtimes.web/server-id room-id (:id user))]
                     (s (fn [_] nil) (fn [_] nil)))))
             ;; Current user info from auth
             current-user-info @sig/current-user
             current-user-uuid (when current-user-info
                                 (uuid (:id current-user-info)))
             current-user-name (or (:name current-user-info) "You")
             reply-target (get chat-reply-targets chat-context-key)

             ;; GlobalCut for chat filters the timeline by DOMAIN time
             ;; (:sent-at) in the query, NOT d/as-of on the db — as-of
             ;; filters by TRANSACTION time, which on a bulk-synced replica
             ;; clusters away from sent-at and made the past look empty.
             as-of-t (:as-of gref)
             time-travel? (some? as-of-t)
             ;; NO FALLBACK to the app store. `(or room-db local-db)` looked
             ;; harmless and was not: the app store does not go through
             ;; `store/install!`, so three of the attributes this timeline reads
             ;; in `:where` position are undeclared there — datahike rejects the
             ;; whole query, and the chat renders as broken rather than as
             ;; not-yet-connected. Worse, when it did answer it answered from the
             ;; wrong database.
             ;;
             ;; A room whose store has not connected yet has no timeline to show.
             ;; Saying so is the honest render, and `room-states` fills in a
             ;; moment later.
             effective-db room-db
             ;; …and "no timeline to show" must be SAID as that, not as "no
             ;; messages". Without a db there is no query and therefore no
             ;; answer — an empty result here is the absence of a database, not
             ;; the absence of messages. The replica connect is seconds of work
             ;; on a cold boot (konserve-sync materialising the room store), and
             ;; for all of it the room asserted it was empty.
             db-loading? (nil? effective-db)
             ;; Wrap db in interval for query-with-deltas
             db-iv (iv/->Interval nil effective-db nil)
             ;; Windowed timeline: the window is applied IN the query layer
             ;; (per-room + window-aware cache, O(window) diffs, no islice
             ;; — whose with-cache address is global per call-site and made
             ;; two open chat tabs diff against each other's slice).
             stored-window (get chat-windows chat-context-key)
             ;; Anchored open (backlink jump): honored only while there is
             ;; no user scroll state — the :end sentinel open-tab! seeds
             ;; counts as none; a real {:start N} map suppresses the anchor.
             anchor-uuid (when (or (nil? stored-window) (= :end stored-window))
                           (:anchor-message data))
             {visible-iv :iv total-count :total anchor-idx :anchor-idx
              thread-root :thread-root}
             (dq/room-timeline-window-with-deltas
              db-iv room-uuid
              (cond-> (if (map? stored-window) stored-window {})
                anchor-uuid (assoc :anchor-uuid (uuid anchor-uuid))
                thread-view? (assoc :thread-root-id thread-root-id))
              sig/CHAT_WINDOW_SIZE as-of-t)
             ;; One-shot scroll+highlight once the anchor is in the DOM.
             ;; DOM-flag guarded (no reactive state); rAF for tree linkage.
             _ #?(:cljs
                  (when anchor-idx
                    (js/requestAnimationFrame
                     (fn []
                       (when-let [el (js/document.querySelector
                                      (str "[data-message-id=\"" anchor-uuid "\"]"))]
                         (when-not (.-_anchorDone el)
                           (set! (.-_anchorDone el) true)
                           (.scrollIntoView el #js {:block "center"})
                           (.add (.-classList el) "chat-message--anchored"))))))
                  :clj nil)

             ;; Detect if this is an AI room
             ;; Check room type from system DB rooms signal, or fall back to
             ;; Detect AI room by type: personal-ai or group rooms with agents respond automatically
             is-ai-room? (let [ur @sig/user-rooms
                               rooms-list (if (map? ur) (:rooms ur) ur)
                               sys-room (when (seq rooms-list)
                                          (first (filter #(= (str (:room/id %)) room-id) rooms-list)))]
                            (boolean (= (:room/type sys-room) :personal-ai)))

             ;; Callbacks for chat panel
             on-scroll (fn [delta]
                         (sig/scroll-chat-window! chat-context-key delta total-count))
             jump-to-message!
             (fn [message-id]
               #?(:cljs
                  (if-let [el (js/document.querySelector
                               (str "[data-message-id=\"" message-id "\"]"))]
                    (do (.scrollIntoView el #js {:block "center"})
                        (.add (.-classList el) "chat-message--anchored"))
                    ;; Ask the relevant projection window for the ancestor when
                    ;; it is outside the current DOM.
                    (sig/open-tab! (if thread-view? :chat-thread :chat)
                                   (assoc data :anchor-message (str message-id))
                                   {:title (if thread-view?
                                             (str "Thread · " room-name)
                                             room-name)}))
                  :clj nil))
             ;; TipTap on-send callback receives HTML content directly.
             ;; ONE send path for every room kind: the server dispatch
             ;; (dispatch-message!) persists to the content DB, posts into
             ;; the dvergr discourse room (agents respond; telegram mirrors
             ;; relay out), and derives the sender from the connection's
             ;; authenticated principal. The old client-side transact branch
             ;; wrote with client-supplied authorship and never reached the
             ;; discourse room — removed.
             on-send-tiptap (fn [html-content]
                              ;; Convert HTML to plain text for storage
                              ;; This callback is installed when the foreign
                              ;; TipTap node MOUNTS and intentionally survives
                              ;; Spindel re-renders. Resolve the reply target
                              ;; NOW; closing over the render's `reply-target`
                              ;; made every later reply send as top-level.
                              (let [content (chat-input/html-to-text html-content)
                                    active-reply-target
                                    (binding [rtc/*execution-context* runtime]
                                      (or (get @sig/chat-reply-targets chat-context-key)
                                          (when thread-view?
                                            {:id thread-root-id
                                             :thread-root-id thread-root-id})))]
                                (when (and content (seq content))
                                  (when-not (and is-ai-room?
                                                 (binding [rtc/*execution-context* runtime] @sig/agent-responding?))
                                    (when is-ai-room?
                                      (binding [rtc/*execution-context* runtime]
                                        (reset! sig/agent-responding? true)))
                                    (let [user-id-str (when-let [u @sig/current-user] (:id u))
                                          ;; Optimistic send: mint the message
                                          ;; uuid HERE, render immediately as
                                          ;; pending; the server posts under
                                          ;; the same uuid, and the sync echo
                                          ;; reconciles (render-column drops
                                          ;; pending entries present in the
                                          ;; replica).
                                          msg-uuid (random-uuid)
                                          ;; Optimistic render via datahike's
                                          ;; overlay (same machinery as the KB
                                          ;; editor): overlay-only — the durable
                                          ;; write goes through the discourse
                                          ;; dispatch below and echoes back via
                                          ;; store sync, at which point the
                                          ;; caught-up predicate (entity with
                                          ;; this uuid exists) drops the
                                          ;; overlay entry.
                                          ;; `current-user-uuid` is ALREADY a UUID
                                          ;; (see the render-body binding) and
                                          ;; cljs.core/uuid asserts (string? s).
                                          author-uuid (some-> user-id-str uuid)
                                          overlay (and author-uuid
                                                       (get-in room-states
                                                               [(str room-db-scope) :overlay]))
                                          parent-id (:id active-reply-target)
                                          thread-root-id (:thread-root-id active-reply-target)
                                          prediction
                                          (when overlay
                                            (try
                                              (let [handle
                                                    (opt/predict!
                                                     overlay
                                                     [(cond->
                                                       {:entity/uuid msg-uuid
                                                        :entity/created-at (js/Date.)
                                                        :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-00000000002b"]
                                                        :block/content content
                                                        :S.Message/author [:entity/uuid author-uuid]
                                                        :S.Message/room [:entity/uuid room-uuid]
                                                        :S.Message/sent-at (js/Date.)}
                                                        parent-id
                                                        (assoc :message/in-reply-to parent-id)
                                                        thread-root-id
                                                        (assoc :message/thread-root-id thread-root-id))]
                                                     (fn [db]
                                                       (some? (d/q '[:find ?e . :in $ ?u
                                                                     :where [?e :entity/uuid ?u]]
                                                                   db msg-uuid))))]
                                                (watch-delivery! overlay handle)
                                                handle)
                                              (catch :default e
                                                ;; e.g. author entity not yet in a
                                                ;; fresh replica — send still goes
                                                ;; through, just without the
                                                ;; optimistic render.
                                                (js/console.warn "[chat] optimistic overlay skipped:" e)
                                                nil)))
                                          dispatch-spin
                                          (binding [rtc/*execution-context* runtime]
                                            (chat-remote/dispatch-message!
                                              web/server-id room-id user-id-str content
                                              (str msg-uuid)
                                              (some-> parent-id str)))]
                                      (dispatch-spin
                                       (fn [_result]
                                         (when prediction
                                           (opt/ack! overlay (:ov-id prediction) _result))
                                         (when is-ai-room?
                                           (binding [rtc/*execution-context* runtime]
                                             (reset! sig/agent-responding? false)))
                                         ;; Do not clear a newer target selected
                                         ;; while this send was in flight.
                                         (binding [rtc/*execution-context* runtime]
                                           (when (= parent-id
                                                    (:id (get @sig/chat-reply-targets chat-context-key)))
                                             (sig/clear-chat-reply-target! chat-context-key)))
                                         (go (db-sig/refresh-db!)))
                                       (fn [err]
                                         (js/console.error "[chat] Send error:" err)
                                         (when prediction
                                           (opt/reject! overlay (:ov-id prediction) err))
                                          ;; The optimistic entry renders this
                                          ;; message as pending and is dropped
                                          ;; only when the durable write echoes
                                          ;; back. It never will — say so now
                                          ;; rather than let it sit there for
                                          ;; the TTL and then vanish.
                                          (rem/report-error!
                                           "That message was not sent." err)
                                          (when is-ai-room?
                                            (binding [rtc/*execution-context* runtime]
                                              (reset! sig/agent-responding? false))))))))))
             ;; Prefetch the room's static-site status (slug + has-app?) so the
             ;; header's Open-app button can open /apps/<slug>/ synchronously on
             ;; click (window.open after an async hop would be popup-blocked).
             ;; Idempotent per room; a raw go block, so not cancelled on re-render.
             _ #?(:cljs (load-room-app-status-into-signal! room-id) :clj nil)
             ;; Store editor instance for cleanup
             editor-atom (atom nil)]

         ;; NOTE: We use the locally computed `window` for rendering
         ;; and do NOT update signals during render to avoid cascade re-renders.
         ;; The window signal gets updated when user scrolls.

         ;; Render chat panel inline with ifor-each for O(delta) message rendering
         (el/div {:class (str "chat-panel" (when time-travel? " chat-read-only"))}
           ;; Header with settings button
           (el/div {:class "chat-header-wrapper"}
             (chat/chat-header {:chat-name (if thread-view?
                                             (str "Thread · " room-name)
                                             room-name)})
             (when thread-view?
               (el/div {:class "chat-thread-header-actions"}
                 (el/button {:class "chat-settings-btn"
                             :title "Back to room"
                             :on-click (fn [_]
                                         (sig/open-tab! :chat
                                                        (dissoc data
                                                                :thread-view?
                                                                :thread-root-id
                                                                :anchor-message)
                                                        {:title room-name}))}
                   (vc/icon "arrow-left" {:class "chat-settings-icon"}))
                 (el/button {:class "chat-settings-btn"
                             :title "Open room beside this thread"
                             :on-click (fn [_]
                                         (sig/open-tab! :chat
                                                        (dissoc data
                                                                :thread-view?
                                                                :thread-root-id
                                                                :anchor-message)
                                                        {:title room-name
                                                         :new-column? true}))}
                   (vc/icon "panel-right-open" {:class "chat-settings-icon"}))))
             (el/button {:class (vc/class-names "chat-settings-btn"
                                                (when (and screen-sharing
                                                           (contains? screen-sharing room-id))
                                                  "chat-settings-btn--active"))
                         :title (if (and screen-sharing (contains? screen-sharing room-id))
                                  "Stop sharing screen with agents"
                                  "Share screen with agents")
                         :on-click (fn [_]
                                     #?(:cljs
                                        (let [on-change
                                              (fn [active?]
                                                (binding [rtc/*execution-context* runtime]
                                                  (swap! sig/screen-sharing
                                                         (if active? conj disj) room-id)))]
                                          (if (screen-share/sharing? room-id)
                                            (screen-share/stop! room-id on-change)
                                            (screen-share/start! room-id on-change)))
                                        :clj nil))}
               (vc/icon "monitor" {:class "chat-settings-icon"}))
             (el/button {:class "chat-settings-btn"
                         :title "Screens (recorded shares)"
                         :on-click (fn [_]
                                     #?(:cljs
                                        (sig/open-tab!
                                          :screens
                                          {:room-id room-id :room-name room-name}
                                          {:title (str room-name " Screens")
                                           :new-tab? true})
                                        :clj nil))}
               (vc/icon "images" {:class "chat-settings-icon"}))
             (el/button {:class "chat-settings-btn"
                         :title "Video call"
                         :on-click (fn [_]
                                     #?(:cljs
                                        (sig/open-tab!
                                          :video
                                          {:room-id room-id :room-name room-name}
                                          {:title (str room-name " Call")
                                           :new-tab? true})
                                        :clj nil))}
               (vc/icon "video" {:class "chat-settings-icon"}))
             ;; Open the room's static site (dvergr.web.apps → /apps/<slug>/).
             ;; Reads the prefetched slug synchronously so window.open stays in
             ;; the click gesture (no popup block). If the room hasn't built an
             ;; app, dvergr serves a friendly "no app here yet" page.
             (el/button {:class "chat-settings-btn"
                         :title "Open this room's app"
                         :on-click (fn [_]
                                     #?(:cljs
                                        (when-let [slug (:slug (binding [rtc/*execution-context* runtime]
                                                                 (get @sig/room-app-status room-id)))]
                                          (js/window.open (str "/apps/" slug "/") "_blank"))
                                        :clj nil))}
               (vc/icon "app-window" {:class "chat-settings-icon"}))
             (el/button {:class "chat-settings-btn"
                         :title "Files"
                         :on-click (fn [_]
                                     #?(:cljs
                                        (sig/open-tab!
                                          :files
                                          {:room-id room-id}
                                          {:title (str room-name " Files")
                                           :new-tab? true})
                                        :clj nil))}
               (vc/icon "folder" {:class "chat-settings-icon"}))
             (el/button {:class "chat-settings-btn"
                         :title "Room settings"
                         :on-click (fn [_]
                                     #?(:cljs
                                        (do
                                          ;; Clear admin-data so room settings loads fresh
                                          (reset! sig/admin-data nil)
                                          (sig/open-tab!
                                            :room-settings
                                            {:room-id room-id}
                                            {:title (str room-name " Settings")
                                             :new-tab? true}))
                                        :clj nil))}
               (vc/icon "settings" {:class "chat-settings-icon"})))

           (when thread-view?
             (el/div {:class "chat-thread-context"
                      :data-message-id (str thread-root-id)}
               (el/div {:class "chat-thread-context-label"}
                 (vc/icon "messages-square")
                 (el/span {:class "chat-thread-context-author"}
                   (or (:S.Message/author-name thread-root) "Thread root")))
               (el/div {:class "chat-thread-context-content"}
                 (cond
                   db-loading? "Loading thread context…"
                   thread-root (:block/content thread-root)
                   :else "The root message is not available in this replica yet."))))

           ;; Messages container — native CSS scroll (overflow-y: auto)
           ;; (Old exploration-diff-summary / fork-controls bars removed
           ;;  alongside the prior fork-tree scaffolding. The chat/*
           ;;  components survive in chat.cljc as the visual idiom for
           ;;  the upcoming active-overlay UI — see Phase 5+.)
           (el/div {:class (vc/class-names "chat-messages"
                                             (when thread-view?
                                               "chat-messages--thread"))
                    ;; Scroll-back paging: near the top → extend the window
                    ;; upward; back at the bottom → restore tail-following.
                    ;; COLUMN-REVERSE geometry: scrollTop runs from
                    ;; -(scrollHeight - clientHeight) at the OLDEST message
                    ;; up to 0 at the newest. The browser natively keeps the
                    ;; viewport stable under prepends and pins to the tail at
                    ;; scrollTop 0 — no manual anchoring needed (the old
                    ;; MutationObserver compensated with normal-column math
                    ;; and snapped the view to the tail on every height
                    ;; change, which is why scroll-up never worked).
                    :on-scroll
                    #?(:cljs
                       (fn [e]
                         (let [el (.-target e)
                               top (.-scrollTop el)
                               max-up (- (.-clientHeight el) (.-scrollHeight el)) ; negative
                               at-top? (<= top (+ max-up 60))
                               at-bottom? (> top -40)
                               win (binding [rtc/*execution-context* runtime]
                                     (get @sig/chat-scroll-windows chat-context-key))]
                           (cond
                             (and at-top?
                                  (or (= win :end) (nil? win)
                                      (pos? (:start win 0))))
                             (binding [rtc/*execution-context* runtime]
                               (sig/grow-chat-window-up! chat-context-key total-count 20))

                             (and at-bottom? (map? win))
                             (binding [rtc/*execution-context* runtime]
                               (sig/follow-chat-end! chat-context-key)))))
                       :clj nil)}
            ;; Messages list with ifor-each for incremental rendering
            ;; Using :entity/uuid as key for categorical schema
            (el/div {:class "chat-messages-list"}
              (ifor-each :entity/uuid visible-iv
                (fn [item]
                  (case (:timeline/type item)
                    :kb-event
                    (chat/kb-event-view
                      {:id (str (:entity/uuid item))
                       :event-type (:S.KBEvent/type item)
                       :title (:S.KBEvent/title item)
                       :block-count (or (:S.KBEvent/block-count item) 0)
                       :author-name (:S.KBEvent/author-name item)
                       ;; `chat/msg-timestamp`, not a bare time: a room's history
                       ;; spans days and a time-only label made a six-day-old
                       ;; conversation read as this morning.
                       :timestamp (chat/msg-timestamp
                                   (when-let [ts (:S.KBEvent/timestamp item)] (js/Date. ts)))})
                    :eval-run
                    (chat/eval-run-view (assoc item :syntax-pref syntax-pref))
                    :eval-entry
                    (chat/eval-entry-view (assoc item :syntax-pref syntax-pref))
                    ;; default: :message
                    (chat/message-view
                      {:id (str (:entity/uuid item))
                       :author-id (str (:S.Message/author-uuid item))
                       :author-name (:S.Message/author-name item)
                       :content (:block/content item)
                       :reasoning (:S.Message/reasoning item)
                       :timestamp (chat/msg-timestamp
                                   (when-let [ts (:S.Message/sent-at item)] (js/Date. ts)))
                       :is-own? (= (:S.Message/author-uuid item) current-user-uuid)
                       :is-ai? (:S.Message/is-ai item)
                       :audience (:message/audience item)
                       :mention-handles (:message/mention-handles item)
                       :attachment-blob (:S.Message/attachment-blob item)
                       :attachment-mime (:S.Message/attachment-mime item)
                       :in-reply-to (:message/in-reply-to item)
                       :thread-parent (:thread/parent item)
                       :reply-count (:thread/reply-count item)
                       :on-open-thread
                       (when (and (pos? (or (:thread/reply-count item) 0))
                                  (:thread/root-id item))
                         (fn [event]
                           (let [new-column? (or (.-metaKey event) (.-ctrlKey event))]
                             (sig/open-tab!
                              :chat-thread
                              (-> data
                                  (dissoc :thread-view? :anchor-message)
                                  (assoc :thread-root-id (str (:thread/root-id item))))
                              {:title (str "Thread · " room-name)
                               :new-tab? (not new-column?)
                               :new-column? new-column?}))))
                       :on-jump-parent (when-let [parent-id (:message/in-reply-to item)]
                                         (fn [] (jump-to-message! parent-id)))
                       :on-reply (fn []
                                   (sig/set-chat-reply-target!
                                    chat-context-key
                                    {:id (:entity/uuid item)
                                     :thread-root-id (:thread/root-id item)
                                     :author-name (:S.Message/author-name item)
                                     :content (:block/content item)}))
                       :syntax-pref syntax-pref})))))

             ;; Empty state — three distinct states, because they
             ;; are three distinct facts: the replica has not arrived (nothing
             ;; is known yet), the past cut holds nothing, or the room is
             ;; genuinely empty. Only the last one is an invitation to type.
             (when (empty? (iv/get-new visible-iv))
               (el/div {:class (str "chat-empty" (when db-loading? " chat-empty--loading"))}
                 (vc/icon (if db-loading? :loader :message-circle)
                          {:class "chat-empty-icon"})
                 (el/p {} (cond
                            db-loading?
                            "Loading messages…"
                            time-travel?
                            "No messages had been sent by this point in time."
                            thread-view?
                            "No replies yet. Continue the thread below."
                            :else
                            "No messages yet. Start the conversation!"))))

             ;; AI thinking indicator
             (when (and is-ai-room?
                        (binding [rtc/*execution-context* runtime] @sig/agent-responding?))
               (el/div {:class "chat-message chat-message--ai"}
                 (el/div {:class "chat-avatar ai"} "AI")
                 (el/div {:class "chat-message-bubble message-body"}
                   (el/div {:class "chat-message-content message-text"}
                     "Thinking...")))))

           ;; Read-only affordance while time-traveling (input hidden via
           ;; .chat-read-only CSS — sending into the past is meaningless).
           (when time-travel?
             (el/div {:class "chat-readonly-note"}
               "🕰 Viewing a past version — read-only"))

           ;; Active reply target is room-local UI state; the outgoing message
           ;; persists only the canonical parent UUID.
           (when (or reply-target thread-view?)
             (el/div {:class "chat-reply-composer"}
               (vc/icon "reply" {:class "chat-reply-composer-icon"})
               (el/div {:class "chat-reply-composer-copy"}
                 (el/span {:class "chat-reply-composer-label"}
                   (if reply-target
                     (str "Replying to " (or (:author-name reply-target) "message"))
                     "Replying in thread"))
                 (el/span {:class "chat-reply-composer-preview"}
                   (if reply-target
                     (str (:content reply-target))
                     "Replies stay attached to this topic.")))
               (when reply-target
                 (el/button {:class "chat-reply-composer-close"
                             :title "Reply to thread root instead"
                             :on-click (fn [_]
                                         (sig/clear-chat-reply-target! chat-context-key))}
                   (vc/icon "x")))))

           ;; TipTap input with autocomplete for @mentions and [[page refs]]
           (el/div {:class "chat-input-container"}
             ;; Prompt chevron — the "type here" affordance (no send
             ;; button; Enter sends, like a terminal prompt).
             (el/span {:class "chat-prompt-marker"} ">")
             (foreign-node
               {:key (str "chat-editor-" room-id
                          (when thread-view? (str "-thread-" thread-root-id)))
                :class "chat-tiptap-container"
                :on-mount (fn [el]
                            (let [editor (chat-input/create-chat-editor
                                           {:element el
                                            :on-send on-send-tiptap
                                            :placeholder "Ask your agents anything — Enter to send"})]
                              (reset! editor-atom editor)))
                            ;; No scroll controller needed: .chat-messages is
                            ;; flex column-reverse, which natively opens at
                            ;; the tail (scrollTop 0), keeps the viewport
                            ;; stable when history is prepended, and pins to
                            ;; the tail while there. The former
                            ;; MutationObserver used normal-column math and
                            ;; defeated all three.
                :on-unmount (fn [_el]
                              (when-let [editor @editor-atom]
                                (chat-input/destroy-chat-editor editor)
                                (reset! editor-atom nil)))}))))  ;; closes }, foreign-node, div.input, div.chat-panel, let
       :clj
       (el/div {:class "content-chat"}
         (el/p {} "Chat (CLJ mode)")))

    :video
    #?(:cljs
       (let [room-id (:room-id data)
             info (get video-info room-id)]
         ;; Mint runs in a root-spin go block (idempotent) — invoking the
         ;; remote spin from this render body would get cancelled on the
         ;; next re-render (see load-room-details-into-signal!).
         (when (and room-id (not info))
           (load-video-token-into-signal! room-id))
         ;; The iframe is NOT here. It is mounted as a sibling of this content
         ;; slot (render-call-hosts), because a tab switch REPLACES this slot —
         ;; and an <iframe> that leaves the document has its browsing context
         ;; destroyed, i.e. the call ends. Once the token arrives the call host
         ;; appears and fills the content area; until then, this placeholder.
         (if info
           nil
           (el/div {:class "content-video"}
             (el/div {:class "video-placeholder"}
               (vc/icon "video"))
             (el/p {} (if room-id
                        "Connecting to the call..."
                        "Open a call from a room header.")))))
       :clj
       (el/div {:class "content-video"}
         (el/p {} "Video call (CLJ mode)")))

    :screens
    #?(:cljs
       (let [room-id (:room-id data)
             {:keys [items loading? query] :as res} (get screens-results room-id)
             input-id (str "screens-search-" room-id)
             run-search! (fn []
                           (let [el (js/document.getElementById input-id)]
                             (search-screens-into-signal! room-id (some-> el .-value))))]
         ;; initial load (latest frames) — go-block loader, idempotent
         ;; enough via the :loading?/result presence check
         (when (and room-id (nil? res))
           (search-screens-into-signal! room-id ""))
         (when (and (nil? (:sessions recordings-results))
                    (not (:loading? recordings-results)))
           (load-recordings-into-signal!))
         (el/div {:class "content-screens"}
           (el/div {:class "screens-toolbar"}
             (el/input {:id input-id
                        :class "screens-search-input"
                        :type "text"
                        :placeholder "Search recorded screens (fulltext)…"
                        :on-keydown (fn [e] (when (= "Enter" (.-key e)) (run-search!)))})
             (el/button {:class "settings-btn settings-btn--primary"
                         :on-click (fn [_] (run-search!))}
               "Search")
             (el/button {:class "settings-btn"
                         :title "Show latest"
                         :on-click (fn [_]
                                     (when-let [el (js/document.getElementById input-id)]
                                       (set! (.-value el) ""))
                                     (search-screens-into-signal! room-id ""))}
               "Latest"))
           (cond
             loading?
             (el/div {:class "settings-loading"} (el/p {} "Loading screens..."))

             (empty? items)
             (el/div {:class "empty-state"}
               (vc/icon "images")
               (el/p {} (if (seq (str query))
                          (str "No screens match \"" query "\".")
                          "No recorded screen shares yet — use the monitor button in the room header to share.")))

             :else
             (el/div {:class "screens-grid"}
               (ifor-each :blob-id (vec items)
                 (fn [{:keys [blob-id at text score]}]
                   (el/div {:key blob-id :class "screen-card"}
                     (el/button {:class "screen-card-delete"
                                 :title "Delete this screenshot"
                                 :on-click (fn [_]
                                             (when (js/confirm "Delete this screenshot?")
                                               (delete-screenshot! room-id blob-id query)))}
                       "×")
                     (el/a {:href (str "/blobs/" blob-id) :target "_blank"}
                       (el/img {:src (str "/blobs/" blob-id)
                                :class "screen-card-img"
                                :loading "lazy"}))
                     (el/div {:class "screen-card-meta"}
                       (el/span {:class "screen-card-time"}
                         (.toLocaleString (js/Date. at)))
                       (when score
                         (el/span {:class "screen-card-score"}
                           (str "score " (.toFixed score 2)))))
                     (el/div {:class "screen-card-text"} text))))))

           ;; Recordings — the caller's own video archive (owner-scoped)
           (render-recordings recordings-results)))
       :clj
       (el/div {:class "content-screens"}
         (el/p {} "Screens (CLJ mode)")))

    :web-captures
    #?(:cljs
       (do
         ;; initial load (latest) when the tab first opens
         (when (and (nil? (:items web-captures-results))
                    (not (:loading? web-captures-results)))
           (search-pages-into-signal! ""))
         (render-web-captures web-captures-results))
       :clj (el/div {:class "content-web"} (el/p {} "Web captures (CLJ mode)")))

    :mail
    (mail-view/render-mail)

    :feed
    #?(:cljs (feed-view/feed-view)
       :clj (el/div {:class "content-feed"}
              (el/h2 {} "Feed")))

    :schedules
    #?(:cljs (schedules-view/render-schedules)
       :clj (el/div {:class "content-schedules"}
              (el/h2 {} "Schedules")))

    :proposals
    ;; `:proposal-id` focuses one ForkSet — how a Task or Feed row opens the
    ;; thing it points at rather than the list it lives in.
    #?(:cljs (proposals-view/proposals-view (:proposal-id data))
       :clj (el/div {:class "content-proposals"}
              (el/h2 {} "Proposals")))

    :tasks
    #?(:cljs (tasks-view/tasks-view)
       :clj (el/div {:class "content-tasks"}
              (el/h2 {} "Tasks")))

    :timelines
    #?(:cljs (timelines-view/timelines-view)
       :clj (el/div {:class "content-timelines"}
              (el/h2 {} "Timelines")))

    :accounting
    #?(:cljs (accounting-view/accounting-view)
       :clj (el/div {:class "content-accounting"}
              (el/h2 {} "Accounting")))

    :add-memory
    #?(:cljs (add-source-view/add-source-view :memory)
       :clj (el/div {:class "content-add-memory"}
              (el/h2 {} "Add memory")))

    :add-team
    #?(:cljs (add-source-view/add-source-view :team)
       :clj (el/div {:class "content-add-team"}
              (el/h2 {} "Add team")))

    :profile
    ;; @mention destination — a lightweight party card read from the tracked
    ;; The roster comes from the `:contacts`/`:directory` signal now, not a
    ;; store — see uis.desktop.people. The db argument is vestigial.
    (profile-view/render-profile-content (:handle data) nil)

    :settings
    #?(:cljs
       (do
         ;; Trigger load if data not yet available
         (when (nil? settings-data)
           (when-let [user @sig/current-user]
             (load-settings-data-into-signal! (:id user))))
         ;; Render from tracked data (nil = loading)
         (if settings-data
           (settings-view/render-settings-content settings-data)
           (el/div {:class "settings-page"}
             (el/div {:class "settings-header"}
               (vc/icon "settings" {:class "settings-header-icon"})
               (el/h2 {} "Settings"))
             (el/div {:class "settings-loading"}
               (el/p {} "Loading settings...")))))
       :clj
       (el/div {:class "content-settings"}
         (el/p {} "Settings (CLJ mode)")))

    :admin
    #?(:cljs
       (do
         ;; Trigger load if data not yet available
         (when (nil? (:parties admin-data))
           (when-let [user @sig/current-user]
             (load-admin-data-into-signal! (:id user))))
         ;; Render from tracked data. Keyed on :parties, not on non-nil: the
         ;; signal is shared with five other panels (see the room-settings
         ;; branch), so "non-nil" can mean somebody else's shape.
         (if (:parties admin-data)
           (admin-view/render-admin-content admin-data)
           (el/div {:class "admin-page"}
             (el/div {:class "admin-header"}
               (vc/icon "shield" {:class "settings-header-icon"})
               (el/h2 {} "Admin Dashboard"))
             (el/div {:class "settings-loading"}
               (el/p {} "Loading...")))))
       :clj
       (el/div {:class "content-admin"}
         (el/p {} "Admin (CLJ mode)")))

    :new-room
    #?(:cljs
       (do
         ;; Load all human parties for member picker
         (when (nil? (:all-humans admin-data))
           (when @sig/current-user
             (load-new-room-humans-into-signal!)))
         (new-room-view/render-new-room-content
           (when admin-data (:all-humans admin-data))))
       :clj
       (el/div {:class "content-new-room"}
         (el/p {} "New Room (CLJ mode)")))

    :files
    (files-view/files-view data)

    :room-settings
    #?(:cljs
       (do
         ;; Kick off the room-details load if not yet loaded. The load runs
         ;; in a go block (see load-room-details-into-signal!) — NOT as a
         ;; spin invoked from this render body, which would become a
         ;; created-child and get cancelled by the next re-render. The
         ;; helper is idempotent, so calling it on every render while
         ;; admin-data is nil is safe.
         ;; Guard on THIS ROOM's data, not merely on nil. `sig/admin-data` is
         ;; shared by six unrelated panels (admin, new-room, new-contact,
         ;; room-settings, kb-settings, agent inspector), each writing its own
         ;; shape. A nil-guard therefore misfires two ways: open KB Settings
         ;; after Room Settings and the load never runs (non-nil, wrong shape,
         ;; stuck on "Loading…"); open room B after room A and B's tab renders
         ;; A's settings, with A's Save and Delete wired up.
         (let [want (:room-id data)
               have (some-> admin-data :room :room/id str)]
           (when (or (nil? admin-data) (not= have (str want)))
             (when want (load-room-details-into-signal! want))))
         ;; Render
         (if (and admin-data (map? admin-data)
                  (= (some-> admin-data :room :room/id str) (str (:room-id data))))
           (room-settings-view/render-room-settings admin-data)
           (el/div {:class "settings-page"}
             (el/div {:class "settings-loading"}
               (el/p {} "Loading room settings...")))))
       :clj
       (el/div {:class "content-room-settings"}
         (el/p {} "Room Settings (CLJ mode)")))

    :new-kb
    #?(:cljs
       (new-kb-view/render-new-kb-content)
       :clj
       (el/div {:class "content-new-kb"}
         (el/p {} "New Wiki (CLJ mode)")))

    :new-contact
    #?(:cljs
       (do
         ;; Load all humans into admin-data for the directory
         (when (nil? (:all-humans admin-data))
           (when-let [user @sig/current-user]
             (let [s (chat-remote/list-parties!
                       is.simm.runtimes.web/server-id :human)]
               (s (fn [result] (reset! sig/admin-data {:all-humans result}))
                  (fn [err]
                    (js/console.error "[new-contact] list parties error:" err))))))
         (let [ur @sig/user-rooms
               existing (into #{} (map :id (:contacts ur)))]
           (new-contact-view/render-new-contact-content
             (when admin-data (:all-humans admin-data))
             existing)))
       :clj
       (el/div {:class "content-new-contact"}
         (el/p {} "New Contact (CLJ mode)")))

    :contact
    #?(:cljs
       (let [contact-id (:contact-id data)
             ur         @sig/user-rooms
             contact    (some #(when (= (:id %) contact-id) %) (:contacts ur))]
         (if contact
           (el/div {:class "settings-page"}
             (el/div {:class "settings-header"}
               (vc/icon (if (= :agent (:type contact)) "bot" "user")
                        {:class "settings-header-icon"})
               (el/h2 {} (or (:display-name contact) "Contact")))
             (el/div {:class "settings-sections"}
               (el/div {:class "settings-section"}
                 (el/h3 {:class "settings-section-title"} "Profile")
                 (el/div {:class "settings-field"}
                   (el/label {} "Type")
                   (el/div {:class "settings-value"}
                     (name (or (:type contact) :human))))
                 (when (:handle contact)
                   (el/div {:class "settings-field"}
                     (el/label {} "Handle")
                     (el/div {:class "settings-value"} (str "@" (:handle contact)))))
                 (when (:room-name contact)
                   (el/div {:class "settings-field"}
                     (el/label {} "Shared room")
                     (el/div {:class "settings-value"} (:room-name contact)))))))
           (el/div {:class "settings-page"}
             (el/div {:class "settings-loading"}
               (el/p {} "Contact not found.")))))
       :clj
       (el/div {:class "content-contact"}
         (el/p {} "Contact (CLJ mode)")))

    :kb-settings
    #?(:cljs
       (do
         ;; Guard on THIS KB's data — see the room-settings branch above for
         ;; why a nil-guard on the shared `admin-data` signal is not enough.
         (let [want (str (:kb-id data))
               have (some-> admin-data :kb :kb/id str)]
           (when (or (nil? admin-data) (not= have want))
             (when-let [kb-id (:kb-id data)]
               (let [s (chat-remote/load-kb-details!
                         is.simm.runtimes.web/server-id kb-id)]
                 (s (fn [result] (reset! sig/admin-data result))
                    (fn [err]
                      (js/console.error "[kb-settings] load error:" err)
                      (sig/show-error! "Could not load wiki settings." (str err))))))))
         ;; Render
         (if (and admin-data (map? admin-data)
                  (= (some-> admin-data :kb :kb/id str) (str (:kb-id data))))
           (kb-settings-view/render-kb-settings admin-data)
           (el/div {:class "settings-page"}
             (el/div {:class "settings-loading"}
               (el/p {} "Loading KB settings...")))))
       :clj
       (el/div {:class "content-kb-settings"}
         (el/p {} "KB Settings (CLJ mode)")))

    :agent
    #?(:cljs
       (agent-inspector/render-agent-inspector data admin-data room-states)
       :clj
       (el/div {:class "content-agent"}
         (el/p {} "Agent inspector (CLJ mode)")))

    ;; Default
    (el/div {:class "content-unknown"}
      (el/p {} (str "Unknown tab type: " tab-type)))))

(defn render-backlink-item
  "Render a single backlink item.
   Handles both page backlinks and chat room backlinks."
  [backlink]
  (let [bl-type (:type backlink)
        title (:title backlink)]
    (case bl-type
      ;; Chat message backlink — jump to the exact position in the room
      ;; (:anchor-message; db-scope resolved from the user-rooms roster).
      :chat-message
      (el/div {:key (str "chatmsg-" (:message-uuid backlink))
               :class "context-item context-item--clickable"
               :on-click (fn [e]
                           #?(:cljs
                              (let [new-column? (or (.-metaKey e) (.-ctrlKey e))
                                    room-uuid (:room-uuid backlink)]
                                (binding [rtc/*execution-context* runtime]
                                  (let [scope (or (:db-scope backlink)
                                                  (let [ur @sig/user-rooms
                                                        rooms (or (:rooms ur) (when (vector? ur) ur))]
                                                    (:room/content-db-scope
                                                     (first (filter #(= room-uuid (str (:room/id %))) rooms)))))]
                                    (sig/open-tab! :chat
                                                   (cond-> {:room-id room-uuid
                                                            :room-name title
                                                            :anchor-message (:message-uuid backlink)}
                                                     scope (assoc :db-scope scope))
                                                   {:title title
                                                    :new-column? new-column?}))))
                              :clj nil))}
        (vc/icon "message-circle")
        (el/span {} title)
        (when-let [ts (:sent-at backlink)]
          (el/span {:class "context-item-meta"}
            #?(:cljs (.toLocaleDateString (js/Date. ts)) :clj ""))))

      ;; Chat room backlink - link to chat room (legacy shape)
      :chat-room
      (el/div {:key (str "chat-" title)
               :class "context-item context-item--clickable"
               :on-click (fn [e]
                           #?(:cljs
                              (let [new-column? (or (.-metaKey e) (.-ctrlKey e))]
                                (sig/open-tab! :chat {:room-name title}
                                               {:title title
                                                :new-column? new-column?}))
                              :clj nil))}
        (vc/icon "message-circle")
        (el/span {} title))

      ;; Page backlink (default) - link to wiki page
      (let [uuid (:entity/uuid backlink)
            page-title (or (:S.Page/title backlink) title "Untitled")]
        (el/div {:key (str uuid)
                 :class "context-item context-item--clickable"
                 :on-click (fn [e]
                             #?(:cljs
                                (let [new-column? (or (.-metaKey e) (.-ctrlKey e))]
                                  (sig/open-tab! :wiki (cond-> {:page-uuid uuid}
                                                         (:db-scope backlink) (assoc :db-scope (:db-scope backlink)))
                                                 {:title page-title
                                                  :new-column? new-column?}))
                                :clj nil))}
          (vc/icon "corner-down-left")
          (el/span {} page-title))))))

(defn render-context-content
  "Render context content for a tab based on its type.
   key-prefix ensures all children have unique keys tied to the column+tab.
   The `local-db` arg here is the db value the caller already resolved
   (per-KB signal for wiki tabs with a db-scope, shared local-db otherwise)."
  [key-prefix tab-type data local-db room-states]
  (case tab-type
    :wiki
    #?(:cljs
       (let [page-uuid (:page-uuid data)
             ;; local-db here is the db value the caller (render-column)
             ;; already resolved by tracking the right signal. For wiki
             ;; tabs render-column tracks (ensure-kb-db-signal! scope)
             ;; or sig/local-db depending on db-scope.
             ;; Backlinks are queried from the SAME db as this tab, so page
             ;; backlinks must inherit the tab's db-scope — without it the
             ;; click opens the page against the shared local-db, where
             ;; KB pages don't exist (rendered as an empty page).
             backlinks (when local-db
                         (cond->> (query-backlinks local-db page-uuid room-states)
                           (:db-scope data)
                           ;; page backlinks inherit the tab's db-scope;
                           ;; chat backlinks carry their ROOM's scope.
                           (mapv #(cond-> %
                                    (not (#{:chat-room :chat-message} (:type %)))
                                    (assoc :db-scope (:db-scope data))))))]
         (el/div {:key (str key-prefix "-wiki-context")}
        (el/div {:key (str key-prefix "-backlinks-section")
                 :class "context-section"}
          (el/div {:key (str key-prefix "-backlinks-title")
                   :class "context-section-title"}
            (str "Backlinks (" (count backlinks) ")"))
          (if (seq backlinks)
            (ifor-each #(or (some-> (:entity/uuid %) str)
                            (str "chat-" (:title %)))
              backlinks
              (fn [bl] (render-backlink-item bl)))
            (el/div {:key (str key-prefix "-no-backlinks")
                     :class "context-item context-item--empty"}
              (vc/icon "link-2")
              "No pages link here")))
        (el/div {:key (str key-prefix "-outline-section")
                 :class "context-section"}
          (el/div {:key (str key-prefix "-outline-title")
                   :class "context-section-title"} "Outline")
          (el/div {:key (str key-prefix "-no-headings")
                   :class "context-item context-item--empty"}
            (vc/icon "list")
            "No headings"))
        ;; History subway — the KB commit backbone (jump the GlobalCut).
        #?(:cljs
           (when-let [scope (:db-scope data)]
             (el/div {:key (str key-prefix "-history-section")
                      :class "context-section"}
               (subway/history-subway scope)))
           :clj nil)))
       :clj nil)

    :chat
    (let [room-id     (:room-id data)
          ur          #?(:cljs @sig/user-rooms :clj nil)
          rooms-list  (when (map? ur) (:rooms ur))
          contacts-list (when (map? ur) (:contacts ur))
          sys-room    (first (filter #(= (str (:room/id %)) room-id) (or rooms-list [])))
          is-ai-room? (= (:room/type sys-room) :personal-ai)
          ;; Agents present in this room (contacts are already filtered to rooms I'm in;
          ;; here we narrow to this specific room)
          room-agents (filterv #(and (= (:type %) :agent)
                                     (= (:room-id %) room-id))
                               (or contacts-list []))
          ;; Room DB from room-states (reactive — updated via konserve-sync)
          room-db-scope (str (:room/content-db-scope sys-room))
          room-db #?(:cljs (when (and room-states room-db-scope)
                             (get-in room-states [room-db-scope :db]))
                     :clj nil)
          ;; Stats derived from room DB (ledger entries written by account-tokens!)
          msg-count #?(:cljs (when room-db
                               (or (d/q '[:find (count ?e) . :where [?e :S.Message/sent-at _]] room-db) 0))
                       :clj nil)
          total-cost-microdollars #?(:cljs (when room-db
                                             (or (d/q '[:find (sum ?c) . :where [?l :ledger/cost-microdollars ?c]] room-db) 0))
                                     :clj nil)
          cost-str #?(:cljs (when (and total-cost-microdollars (pos? total-cost-microdollars))
                              (str "$" (.toFixed (/ total-cost-microdollars 1000000.0) 4)))
                      :clj nil)]
      (el/div {:key (str key-prefix "-chat-context")}
        ;; Participants — the room's actual party list (humans + agents),
        ;; resolved against `contacts` as the name directory. Parties not
        ;; in contacts are the viewer (contacts exclude self) → "You".
        (el/div {:key (str key-prefix "-members-section")
                 :class "context-section"}
          (el/div {:key (str key-prefix "-members-title")
                   :class "context-section-title"} "Participants")
          (let [contact-by-id (into {} (map (juxt :id identity))
                                    (or contacts-list []))
                assignment-by-actor
                (into {} (map (juxt :actor-id identity))
                      (or (:room/assignments sys-room) []))
                participants (mapv (fn [pid]
                                     (merge (or (get contact-by-id pid)
                                                {:id pid :type :human :you? true})
                                            (get assignment-by-actor pid)))
                                   (or (:room/parties sys-room) []))
                ;; Fallback for rooms whose parties haven't synced yet
                participants (if (seq participants)
                               participants
                               (into [{:id "you" :type :human :you? true}]
                                     room-agents))]
            (ifor-each :id participants
              (fn [p]
                (el/div {:key (:id p)
                         :class (vc/class-names
                                  "context-item"
                                  (when (= :agent (:type p))
                                    "context-item--clickable"))
                         :title (when (= :agent (:type p))
                                  "Inspect this agent")
                         :on-click (fn [_]
                                     #?(:cljs
                                        (when (= :agent (:type p))
                                          (sig/open-tab!
                                           :agent
                                           {:agent-id (:id p)
                                            :room-id room-id
                                            :agent-name (:display-name p)
                                            :model (:model p)}
                                           {:title (or (:display-name p) "Agent")
                                            :new-tab? true}))
                                        :clj nil))}
                  (if (= :agent (:type p))
                    (el/span {:class "context-item-badge context-item-badge--ai"} "AI")
                    (el/span {:class "online-dot"}))
                  (el/span {} (if (:you? p)
                                "You"
                                (or (:display-name p) (:handle p) "Member")))
                  (when (= :agent (:type p))
                    (el/span {:class "room-team-policy"}
                      (str (name (or (:role p) :specialist))
                           " · "
                           (case (:response-policy p)
                             :always "always"
                             :mention "@mention"
                             :manual "manual"
                             "legacy"))))))))
          #?(:cljs
             (el/button {:key (str key-prefix "-team-settings")
                         :class "context-team-settings"
                         :on-click (fn [_]
                                     (reset! sig/admin-data nil)
                                     (sig/open-tab!
                                      :room-settings
                                      {:room-id room-id}
                                      {:title (str (or (:room/name sys-room) "Room")
                                                   " Settings")
                                       :new-tab? true}))}
               (vc/icon "sliders")
               "Manage team")
             :clj nil)
          ;; Room-level usage stats (reactive from room DB)
          (when (and msg-count (pos? msg-count))
            (el/div {:key (str key-prefix "-usage-stats")
                     :class "context-item context-item--stats"}
              (el/span {:class "context-stats-text"}
                (str msg-count " msgs"
                     (when cost-str (str " · " cost-str)))))))
        ;; Sandbox panel — only for AI rooms, moved here from chat content area
        #?(:cljs
           (when is-ai-room?
             (el/div {:key (str key-prefix "-sandbox-section")
                      :class "context-section"}
               (el/div {:key (str key-prefix "-sandbox-title")
                        :class "context-section-title"} "Sandbox")
               (sandbox-panel/sandbox-panel
                 (when room-id #?(:cljs (uuid room-id) :clj nil)))))
           :clj nil)))

    :video
    (el/div {:key (str key-prefix "-video-context")}
      (el/div {:key (str key-prefix "-participants-section")
               :class "context-section"}
        (el/div {:key (str key-prefix "-participants-title")
                 :class "context-section-title"} "Participants")
        (el/div {:key (str key-prefix "-you-participant")
                 :class "context-item"}
          (el/span {:class "online-dot"})
          "You"))
      (el/div {:key (str key-prefix "-recordings-section")
               :class "context-section"}
        (el/div {:key (str key-prefix "-recordings-title")
                 :class "context-section-title"} "Recordings")
        (el/div {:key (str key-prefix "-no-recordings")
                 :class "context-item"}
          (vc/icon "play-circle")
          "No recordings")))

    :feed
    (el/div {:key (str key-prefix "-feed-context")}
      (el/div {:key (str key-prefix "-filters-section")
               :class "context-section"}
        (el/div {:key (str key-prefix "-filters-title")
                 :class "context-section-title"} "Filters")
        (el/div {:key (str key-prefix "-all-projects")
                 :class "context-item"}
          (vc/icon "check-square")
          "All projects")))

    ;; Settings, admin, new-room, room-settings, kb, agent tabs have no context footer
    :profile nil
    :settings nil
    :mail nil
    :admin nil
    :new-room nil
    :new-kb nil
    :new-contact nil
    :contact nil
    :room-settings nil
    :kb-settings nil
    :agent nil

    ;; Default
    nil))
