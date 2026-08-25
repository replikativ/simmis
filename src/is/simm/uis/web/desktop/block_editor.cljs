(ns is.simm.uis.web.desktop.block-editor
  "Block editor with Datahike backend.

   Architecture:
   - TipTap for rich text editing via foreign-node
   - spindel for reactive rendering with ifor-each
   - Datahike queries via db-signal for reactive data
   - Remote writes via distributed-scope
   - konserve-sync for database synchronization"
  (:require [org.replikativ.spindel.dom.elements :as el]
            [org.replikativ.spindel.dom.browser :as browser]
            [org.replikativ.spindel.dom.render :as render]
            [org.replikativ.spindel.dom.foreach]
            [org.replikativ.spindel.dom.foreign]
            [org.replikativ.spindel.incremental.interval :as iv]
            [org.replikativ.spindel.effects.track :refer [track]]
            [org.replikativ.spindel.effects.await :refer [await]]
            [org.replikativ.spindel.engine.core :as rtc]
            [org.replikativ.spindel.engine.addressing]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [org.replikativ.spindel.spin.core]
            [is.simm.partial-cps.async]
            [is.simm.uis.web.desktop.tiptap-references :as tiptap-refs]
            [is.simm.uis.web.desktop.people :as people]
            [is.simm.uis.web.desktop.db-signal :as db-signal]
            [is.simm.uis.web.desktop.datahike-query :as dq]
            [is.simm.uis.web.desktop.block-remote :as remote]
            [is.simm.uis.web.desktop.remote :as rem]
            [is.simm.uis.web.desktop.views.types :as types]
            [is.simm.uis.web.desktop.widget-sandbox :as widget-sandbox]
            ;; Use shared kabel client from web.cljs
            [superv.async :refer [S] :refer-macros [go-try <?]]
            [clojure.core.async :refer [go timeout]]
            [is.simm.distributed-scope :refer [connect-distributed-scope]]
            [is.simm.runtimes.web :as web]
            [is.simm.uis.web.desktop.signals :as sig]
            ;; Datahike
            [is.simm.model.fractional-index :as frac]
            [is.simm.model.references :as refs]
            [datahike.api :as dh]
            [datahike.optimistic :as opt]
            [clojure.string :as str]
            [cljs.reader :as reader]
            ["@tiptap/core" :refer [Editor Extension]]
            ["@tiptap/starter-kit" :default StarterKit]
            ["@tiptap/extension-link" :default TiptapLink]
            ["@tiptap/extension-code-block" :default CodeBlock]
            [is.simm.uis.web.desktop.markdown :as md]
            [is.simm.uis.web.desktop.sandbox-remote :as sandbox-remote]
            [is.simm.uis.web.desktop.branching.naming :as bn]
            [datahike.writing :as dw]
            [konserve.core :as kc]
            [superficie.api :refer [toSup]])
  (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]]
                   [org.replikativ.spindel.dom.elements :as el]
                   [org.replikativ.spindel.dom.foreach :refer [ifor-each]]
                   [org.replikativ.spindel.dom.foreign :refer [foreign-node]]
                   [clojure.core.async :refer [go]]))

;; =============================================================================
;; Runtime and State
;; =============================================================================

;; Runtime is imported from runtime.cljc (shared across the app)

;; Forward declarations for atoms defined later in the file
(declare current-editor-db-scope)

;; =============================================================================
;; Page Search for Autocomplete
;; =============================================================================

(defn search-pages
  "Search pages by title for autocomplete.
   Uses the KB database if the current editor has a db-scope, otherwise the shared DB.
   On a non-trunk branch, projects via `dh/branch-as-db` to read the branch's view.
   Returns a JS array of {title: string} objects."
  [query]
  (let [db-scope @current-editor-db-scope
        branch (when db-scope (get @sig/active-kb-branch (str db-scope) :db))
        trunk-db (when db-scope (db-signal/get-kb-db db-scope))
        db (cond
             (and db-scope branch (not= :db branch))
             ;; Falling back to trunk here is a LIE the user cannot see — the
             ;; autocomplete would offer trunk's pages while the editor claims
             ;; to be on a branch. Keep the fallback (an empty list is worse)
             ;; but say so, so a broken branch projection is diagnosable.
             (or (try (when-let [conn (db-signal/get-kb-conn db-scope)]
                        (dh/branch-as-db conn branch))
                      (catch :default e
                        (js/console.error "[block-editor] branch-as-db failed for"
                                          (str db-scope) "branch" (str branch)
                                          "— falling back to TRUNK:" e)
                        nil))
                 trunk-db)
             db-scope trunk-db
             :else @sig/local-db)]
    (if db
      (try
        (let [query-lower (str/lower-case (or query ""))
              pages (dh/q '[:find ?title
                            :where
                            [?e :S.Page/title ?title]]
                          db)
              matching (->> pages
                            (map first)
                            (filter #(str/includes? (str/lower-case %) query-lower))
                            (sort)
                            (take 10)
                            (map #(js-obj "title" %))
                            into-array)]
          matching)
        (catch :default e
          (js/console.error "[search-pages] Error:" e)
          #js []))
      #js [])))

(def page-suggestion-extension
  "TipTap extension for page autocomplete when typing [[."
  (tiptap-refs/create-page-suggestion-extension
    #js {:searchFn search-pages
         :onSelect (fn [page-title]
                     (js/console.log "[PageSuggestion] Selected:" page-title))}))

(defn search-users
  "Search people (users, agents, contacts) by name/handle for @ autocomplete.
   Returns a JS array of {name: handle, displayName: string, id: string}.
   The dropdown shows displayName, but inserts the handle.

   Backed by `people/all` — the roster the server already sends as `:contacts`
   — which replaced a datahike projection of the same data."
  [query]
  (let [query-lower (when query (str/lower-case query))]
    (try
      (->> (people/all)
           ;; Only mentionable people have a handle to insert.
           (filter (fn [{:keys [handle display-name]}]
                     (and (seq handle)
                          (or (empty? query)
                              (str/includes? (str/lower-case (str display-name)) query-lower)
                              (str/includes? (str/lower-case (str handle)) query-lower)))))
           (take 10)
           (map (fn [{:keys [entity/uuid handle display-name]}]
                  ;; name is the handle (inserted), displayName is shown
                  #js {:name handle
                       :displayName display-name
                       :id (str uuid)}))
           into-array)
      (catch :default e
        (js/console.error "[search-users] Error:" e)
        #js []))))

(def user-suggestion-extension
  "TipTap extension for user autocomplete when typing @."
  (tiptap-refs/create-user-suggestion-extension
    #js {:searchFn search-users
         :onSelect (fn [user]
                     (js/console.log "[UserSuggestion] Selected:" (.-name user)))}))

;; NOTE: TipTap editors are now stored directly on DOM elements via .-_tiptapEditor
;; This allows the same block to be rendered in multiple columns without conflicts.
;; The old global registry caused issues when the same page was open in two columns.

(defonce render-handle (atom nil))

;; Default page UUID for testing
(def ^:const DEFAULT_PAGE_UUID "30b7101d-db61-4266-a616-cb7bd6546784")

;; Page UUID currently being edited
(defonce current-page-uuid (atom nil))

;; UI-only state: focused block (not persisted to Datahike)
(defonce ui-state (atom {:focused-id nil}))

;; Track visible blocks in order for arrow key navigation
(defonce visible-blocks-order (atom []))

;; Flag to prevent onBlur saves during navigation and structural changes
(defonce operation-in-progress? (atom false))

;; Track which block needs focus restored after structural changes
(defonce focus-restore-block (atom nil))

;; Pending focus target: when a brand-new block is created (e.g. via
;; Enter), we set its UUID here. The new block's foreign-node :on-mount
;; reads it after TipTap is initialised and focuses the editor, then
;; clears the atom. This avoids using setTimeout-as-patch — the focus
;; lands exactly when the DOM element is mounted.
(defonce focus-on-mount (atom nil))

;; Track the db-scope of the currently focused editor (for search-pages autocomplete)
(defonce current-editor-db-scope (atom nil))

;; =============================================================================
;; Kabel Connection (uses shared client from web.cljs)
;; =============================================================================

(def server-url web/url)

;; Use shared kabel client from web.cljs (has auth middleware configured)
(def kabel-client web/client)

(defn connect-to-server!
  "Connect to the server via WebSocket.
   Returns a channel that completes when connected."
  []
  (js/console.log "[BlockEditor] Connecting to server:" server-url)
  (connect-distributed-scope S kabel-client server-url))

;; =============================================================================
;; Forward Declarations
;; =============================================================================

(declare create-block-remote!)
(declare ensure-page-remote!)
(declare update-block-content-remote!)
(declare update-block-collapsed-remote!)
(declare delete-block-remote!)
(declare indent-block-remote!)
(declare outdent-block-remote!)

;; =============================================================================
;; Datahike Block Transformation
;; =============================================================================

(defn datahike-block->ui-block-base
  "Transform a Datahike block to UI format (without hidden state).
   Datahike: {:entity/uuid :block/content :block/order :block/parent :block/collapsed :block/viz-spec :block/widget-code}
   UI: {:id :content :parent-id :order :collapsed :focused :viz-spec :widget-code}"
  [dh-block focused-id]
  (let [uuid (:entity/uuid dh-block)
        parent-uuid (get-in dh-block [:block/parent :entity/uuid])]
    {:id uuid
     :content (or (:block/content dh-block) "")
     :parent-id parent-uuid
     :order (or (:block/order dh-block) 0)
     :collapsed (boolean (:block/collapsed dh-block))
     :focused (= uuid focused-id)
     :viz-spec (:block/viz-spec dh-block)
     :widget-code (:block/widget-code dh-block)}))

(defn compute-hidden-state
  "Compute hidden state for a block by checking ancestor collapsed states."
  [block block-index]
  (loop [current-parent-id (:parent-id block)]
    (if-let [parent (get block-index current-parent-id)]
      (if (:collapsed parent)
        true
        (recur (:parent-id parent)))
      false)))

(defn add-hidden-state
  "Add hidden state to all blocks based on ancestor collapsed states."
  [blocks]
  (let [block-index (into {} (map (juxt :id identity)) blocks)]
    (mapv #(assoc % :hidden (compute-hidden-state % block-index)) blocks)))

(defn- collapse-or-parent-changed?
  "True if an :update delta on a raw datahike block changes
  :block/collapsed or :block/parent — either of which can flip the
  :hidden state of descendants that aren't in the input deltas."
  [delta]
  (and (= :update (:delta delta))
       (let [o (:old-value delta)
             n (:value delta)]
         (or (not= (:block/collapsed o) (:block/collapsed n))
             (not= (get-in o [:block/parent :entity/uuid])
                   (get-in n [:block/parent :entity/uuid]))))))

(defn transform-datahike-blocks
  "Transform datahike block records into the UI-shaped block maps with
   computed `:hidden` state. The downstream `ifor-each` ignores
   source deltas and recomputes its child vnodes from this function's
   `:new` (per-key memoised), so we don't bother translating or
   synthesising delta shapes here — `:deltas nil` is the right
   advisory signal."
  [dh-blocks-iv focused-id]
  (let [ui-of (fn [bs]
                (when bs
                  (-> (mapv #(datahike-block->ui-block-base % focused-id) bs)
                      add-hidden-state)))]
    (iv/->Interval (ui-of (iv/get-old dh-blocks-iv))
                   (ui-of (iv/get-new dh-blocks-iv))
                   nil)))

(defn make-page-blocks-query
  "Create a Datahike query function for blocks under a page.

   NOTE: Datahike CLJS has a bug where :in parameters don't work correctly.
   Workaround: Query all blocks with parents and filter in Clojure.

   This query returns ALL descendants of the page (including nested blocks),
   not just direct children."
  [page-uuid]
  (fn [db]
    (when db
      (let [all-blocks (dh/q '[:find [(pull ?b [:entity/uuid :block/content :block/order
                                                :block/collapsed :block/viz-spec :block/widget-code
                                                {:block/parent [:entity/uuid :db/id]}]) ...]
                               :where
                               [?b :block/parent _]]
                             db)
            ;; Build parent->children index for depth-first traversal
            parent->children (group-by #(get-in % [:block/parent :entity/uuid]) all-blocks)]
        ;; Return all blocks that are descendants of the page, sorted by order
        ;; We need to do a depth-first traversal to maintain parent-child hierarchy
        ;; and sort siblings by :block/order
        (letfn [(get-sorted-children [parent-uuid]
                  (->> (get parent->children parent-uuid [])
                       (sort-by :block/order)
                       vec))
                (depth-first-collect [parent-uuid]
                  (let [children (get-sorted-children parent-uuid)]
                    (mapcat (fn [child]
                              (cons child (depth-first-collect (:entity/uuid child))))
                            children)))]
          (vec (depth-first-collect page-uuid)))))))

;; =============================================================================
;; Index Building (for tree operations)
;; =============================================================================

(defn build-block-index
  "Build id->block index for O(1) lookups."
  [blocks]
  (into {} (map (juxt :id identity)) blocks))

(defn build-children-index
  "Build parent-id->[children] index for tree traversal."
  [blocks]
  (group-by :parent-id blocks))

;; =============================================================================
;; Block Tree Operations
;; =============================================================================

(defn block-depth
  "Calculate nesting depth of a block.
   Only counts ancestors that are blocks (not the page)."
  [block block-index]
  (loop [depth 0
         current block]
    (if-let [parent-id (:parent-id current)]
      ;; Only increment depth if parent is a block (in the index)
      (if-let [parent (get block-index parent-id)]
        (recur (inc depth) parent)
        depth)  ;; Parent not in index (probably the page), stop here
      depth)))

(defn has-children?
  "Check if a block has children."
  [block-id children-index]
  (seq (get children-index block-id)))

;; =============================================================================
;; Remote CRUD Operations
;; =============================================================================
;;
;; Branch awareness
;; ----------------
;; KB writes are dispatched against `(active-branch-for db-scope)` which
;; reads `sig/active-kb-branch`. Two cases:
;;
;;   - **Trunk** (`:db` or nil) — optimistic local `opt/transact!` on the
;;     replicated kabel conn, server reconciles via store sync. Snappy.
;;   - **Fork** — must round-trip through the server's spin-remote with
;;     `:branch-kw` so the server's writer-per-store serializes the tx
;;     against the right branch-bound conn. Editor view re-renders when
;;     kabel store sync replicates the new branch state back. Slightly
;;     less snappy than trunk; correct under SI + writer-per-store.
;;
;; The local-only optimistic path can't target a fork: the local conn is
;; on trunk, and `opt/transact!` is branch-unaware. Until datahike's
;; optimistic overlay grows branch addressing, forks pay the round-trip.

(defn- active-branch-for
  "Active branch keyword for `db-scope` from the session-local signal.
   Returns `:db` (trunk) when nothing is set, or when `db-scope` is nil
   (= shared DB write, no branching)."
  [db-scope]
  (or (when db-scope
        (get @sig/active-kb-branch (str db-scope)))
      :db))

(defn- trunk?
  "True when no branch is active or the active branch is the trunk."
  [branch-kw]
  (or (nil? branch-kw) (= :db branch-kw)))

(defn- viewing-past?
  "True while the workspace is scrubbed off `now` — the same condition the
   editor calls `time-travel?` and renders its read-only banner for."
  []
  (binding [rtc/*execution-context* runtime]
    (not= (or @sig/global-ref :now) :now)))

(defn- refuse-past-write!
  "Refuse `what` while the workspace is scrubbed off `now`; returns true when
   refused, so callers read `(when-not (refuse-past-write! …) …)`.

   The banner promises read-only and the CSS did not deliver it: it sets
   `pointer-events: none` on `.blocks-container`/`.block-content`, which is a
   MOUSE-only hint — keyboard focus reaches the editors, and the page title
   lives outside both selectors entirely. Worse, a write that did fire went to
   `get-kb-conn` and the branch of the moment, i.e. it edited the PRESENT while
   the screen said you were looking at the past.

   So the lock lives at the write path, not in the stylesheet. Affordances are
   disabled too (that is what stops the accidental keystroke), but the refusal
   here is what makes the promise true — for keyboard, for assistive tech, and
   for anything that calls these directly."
  [what]
  (when (viewing-past?)
    (js/console.warn "[read-only] refused" what "— viewing a past reference")
    (binding [rtc/*execution-context* runtime]
      (sig/show-error!
       "You're viewing an earlier version — editing is off. Use \"Back to now\" to make changes."))
    true))

(defn create-block-remote!
  "Create a block remotely via distributed-scope."
  [parent-uuid content order & [db-scope]]
  (let [branch (active-branch-for db-scope)]
    (js/console.log "[Remote] Creating block, parent:" (str parent-uuid)
                    "order:" order "branch:" branch)
    (rem/invoke! (remote/create-block-remote! parent-uuid content order db-scope branch)
                 {:message "Could not create that block."})))

(defn ensure-page-remote!
  "Ensure a page exists. Optimistic local write for KB pages on trunk,
   remote invoke for shared DB or fork writes."
  [page-uuid title & [db-scope]]
  (let [branch (active-branch-for db-scope)
        overlay (when (and db-scope (trunk? branch))
                  (db-signal/get-kb-overlay db-scope))]
    (if overlay
      (let [db (opt/db overlay)
            exists? (seq (dh/q '[:find ?e :in $ ?uuid :where [?e :entity/uuid ?uuid]] db page-uuid))]
        (js/console.log "[Local] Ensuring page exists:" (str page-uuid) "exists?" (boolean exists?))
        (if exists?
          (go {:status :exists :uuid page-uuid})
          (let [now (js/Date.)
                resolved-title (or title "Untitled Page")
                ;; Pre-compute backfill for any existing blocks that
                ;; contain `[[title]]` — same shape as the server's
                ;; -ensure-page-handler. Lights up backlinks
                ;; optimistically the moment the page is created.
                backfill (refs/backfill-tx-for-page db page-uuid resolved-title)]
            ;; Page + an empty first block in one optimistic transaction.
            ;; Mirrors the server-side -ensure-page-handler, which always
            ;; seeds a new page with an empty first block so the editor has
            ;; a block to render and focus. The block references the page
            ;; via the :db/id tempid so the parent link resolves within the
            ;; same tx.
            (opt/transact! overlay
                           (into [{:db/id "new-page"
                                   :entity/uuid page-uuid
                                   :entity/name (str "Page " (subs (str page-uuid) 0 8))
                                   :entity/created-at now
                                   :entity/updated-at now
                                   :instance/of-role [:entity/name "S/Page"]
                                   :S.Page/title resolved-title}
                                  {:entity/uuid (random-uuid)
                                   :entity/created-at now
                                   :entity/updated-at now
                                   :instance/of-role [:entity/name "S/Block"]
                                   :block/content ""
                                   :block/order (frac/generate-key-between nil nil)
                                   :block/parent "new-page"}]
                                 backfill))
            (go {:status :created :uuid page-uuid}))))
      (do
        (js/console.log "[Remote] Ensuring page exists:" (str page-uuid) "branch:" branch)
        (remote/ensure-page-remote! page-uuid title db-scope branch)))))

(defn update-block-content-remote!
  "Update block content. Optimistic local write for KB pages on trunk,
   remote invoke for shared DB or fork writes.

   For the local path we skip the transact when the new content matches
   what's already in the effective DB. Reason: the Enter handler runs
   a coalesced transact that already saves the leaving block's content,
   and TipTap then fires `onBlur` with the same HTML — without this
   guard that fires a redundant `opt/transact!`, which in turn triggers
   another full reactive pass for no benefit."
  [block-uuid content & [db-scope]]
  (when-not (refuse-past-write! "edit block content")
    (let [branch (active-branch-for db-scope)
          overlay (when (and db-scope (trunk? branch))
                    (db-signal/get-kb-overlay db-scope))]
      (if overlay
        (let [db (opt/db overlay)
              current (some-> (dh/entity db [:entity/uuid block-uuid])
                              :block/content)]
          (if (= current content)
          (js/console.log "[Local] Skipping content update (unchanged) for:" (str block-uuid))
          (let [;; Resolve [[Page]] refs against the local KB db, so
                ;; backlinks light up optimistically — no waiting for
                ;; the server roundtrip. Pages that don't exist
                ;; locally yet are dropped silently; the server's
                ;; authoritative re-extract fills them in via sync,
                ;; and `backfill-references-for-page!` covers the
                ;; case where the referenced page is created later.
                resolved-refs (refs/resolve-references db content)
                ;; @handle party-mentions — value-level handle strings, so no
                ;; local resolution needed (the server reconciles + notifies).
                  mentions (refs/extract-user-mentions content)]
              (js/console.log "[Local] Updating content for:" (str block-uuid)
                              "refs:" (count resolved-refs) "mentions:" (count mentions))
              (opt/transact! overlay [(cond-> {:entity/uuid block-uuid
                                               :block/content content}
                                        (seq resolved-refs)
                                        (assoc :block/references resolved-refs)
                                   (seq mentions)
                                   (assoc :block/mentions (vec mentions)))]))))
      (do
        (js/console.log "[Remote] Updating content for:" (str block-uuid) "branch:" branch)
        (rem/invoke! (remote/update-block-content-remote! block-uuid content db-scope branch)
                     {:message "Could not save that block."}))))))

(defn update-block-collapsed-remote!
  "Update block collapsed state. Optimistic local for KB pages on trunk,
   remote invoke for shared DB or fork writes."
  [block-uuid collapsed & [db-scope]]
  (when-not (refuse-past-write! "collapse a block")
    (let [branch (active-branch-for db-scope)
          overlay (when (and db-scope (trunk? branch))
                    (db-signal/get-kb-overlay db-scope))]
      (if overlay
        (do
          (js/console.log "[Local] Updating collapsed for:" (str block-uuid) "to:" collapsed)
          (opt/transact! overlay [{:entity/uuid block-uuid :block/collapsed collapsed}]))
        (do
          (js/console.log "[Remote] Updating collapsed for:" (str block-uuid) "to:" collapsed "branch:" branch)
          (rem/invoke! (remote/update-block-collapsed-remote! block-uuid collapsed db-scope branch)
                     {:message "Could not fold that block."}))))))

(defn delete-block-remote!
  "Delete a block. Optimistic local for KB pages on trunk, remote invoke
   for shared DB or fork writes."
  [block-uuid & [db-scope]]
  (when-not (refuse-past-write! "delete a block")
    (let [branch (active-branch-for db-scope)
          overlay (when (and db-scope (trunk? branch))
                    (db-signal/get-kb-overlay db-scope))]
      (if overlay
        (do
          (js/console.log "[Local] Deleting block:" (str block-uuid))
          (opt/transact! overlay [[:db/retractEntity [:entity/uuid block-uuid]]]))
        (do
          (js/console.log "[Remote] Deleting block:" (str block-uuid) "branch:" branch)
          (rem/invoke! (remote/delete-block-remote! block-uuid true db-scope branch)
                     {:message "Could not delete that block."}))))))

(defn indent-block-remote!
  "Indent a block remotely (server-only operation)."
  [block-uuid & [db-scope]]
  (when-not (refuse-past-write! "indent a block")
    (let [branch (active-branch-for db-scope)]
    (js/console.log "[Remote] Indenting block:" (str block-uuid) "branch:" branch)
    (rem/invoke! (remote/indent-block-remote! block-uuid db-scope branch)
                 {:message "Could not indent that block."}))))

(defn outdent-block-remote!
  "Outdent a block remotely (server-only operation)."
  [block-uuid & [db-scope]]
  (when-not (refuse-past-write! "outdent a block")
    (let [branch (active-branch-for db-scope)]
    (js/console.log "[Remote] Outdenting block:" (str block-uuid) "branch:" branch)
    (rem/invoke! (remote/outdent-block-remote! block-uuid db-scope branch)
                 {:message "Could not outdent that block."}))))

(defn create-sibling-block-after-remote!
  "Create a new sibling block after the given block (server-only operation).
   Wraps `remote/create-sibling-block-after-remote!` with branch routing."
  [block-uuid content & [db-scope]]
  (when-not (refuse-past-write! "create a block")
    (let [branch (active-branch-for db-scope)]
    (js/console.log "[Remote] Creating sibling after:" (str block-uuid) "branch:" branch)
    (rem/invoke! (remote/create-sibling-block-after-remote! block-uuid content db-scope branch)
                 {:message "Could not create that block."}))))

;; =============================================================================
;; Local KB Block Operations
;; =============================================================================

(defn- create-sibling-after-local!
  "Create a sibling block after block-uuid via opt/transact!.
   Replicates the server-side create-sibling-after logic on the client."
  [overlay block-uuid content]
  (let [db (opt/db overlay)
        ;; Get current block's parent and order — use q since pull with :in has CLJS bugs
        all-blocks (dh/q '[:find [(pull ?b [:entity/uuid :block/order {:block/parent [:entity/uuid]}]) ...]
                           :where [?b :block/parent _]]
                          db)
        block (first (filter #(= (:entity/uuid %) block-uuid) all-blocks))
        parent-uuid (get-in block [:block/parent :entity/uuid])
        current-order (:block/order block)]
    (when (and parent-uuid current-order)
      ;; Find next sibling order
      (let [siblings (filter #(= (get-in % [:block/parent :entity/uuid]) parent-uuid) all-blocks)
            ;; `>` on strings in CLJS coerces to NaN and returns nonsense.
            ;; Use `compare` for proper lexical ordering — without this
            ;; next-order is always nil and `generate-key-between` falls
            ;; back to "append after current-order" which puts the new
            ;; block at the end of the order space rather than next to
            ;; the current block.
            next-order (->> siblings
                            (map :block/order)
                            (filter #(when % (pos? (compare % current-order))))
                            sort
                            first)
            new-order (frac/generate-key-between current-order next-order)
            new-uuid (random-uuid)
            resolved-refs (refs/resolve-references db content)]
        (js/console.log "[Local] Creating sibling after:" (str block-uuid) "order:" new-order
                        "refs:" (count resolved-refs))
        (opt/transact! overlay [(cond-> {:entity/uuid new-uuid
                                         :entity/created-at (js/Date.)
                                         :entity/updated-at (js/Date.)
                                         :instance/of-role [:entity/name "S/Block"]
                                      :block/content content
                                      :block/order new-order
                                      :block/parent [:entity/uuid parent-uuid]}
                               (seq resolved-refs)
                               (assoc :block/references resolved-refs))])
        new-uuid))))

(defn- update-content-and-create-sibling-local!
  "Coalesced version of update-block-content-remote! + create-sibling-
  after-local! for the Enter handler. Both mutations go through a
  single `opt/transact!` so the kb-state listener fires ONCE for the
  optimistic apply and ONCE for the server reply (instead of twice each)."
  [overlay block-uuid current-content sibling-content]
  (let [db (opt/db overlay)
        all-blocks (dh/q '[:find [(pull ?b [:entity/uuid :block/order {:block/parent [:entity/uuid]}]) ...]
                           :where [?b :block/parent _]]
                         db)
        block (first (filter #(= (:entity/uuid %) block-uuid) all-blocks))
        parent-uuid (get-in block [:block/parent :entity/uuid])
        current-order (:block/order block)]
    (when (and parent-uuid current-order)
      (let [siblings (filter #(= (get-in % [:block/parent :entity/uuid]) parent-uuid) all-blocks)
            ;; `>` on strings in CLJS coerces to NaN and returns nonsense.
            ;; Use `compare` for proper lexical ordering — without this
            ;; next-order is always nil and `generate-key-between` falls
            ;; back to "append after current-order" which puts the new
            ;; block at the end of the order space rather than next to
            ;; the current block.
            next-order (->> siblings
                            (map :block/order)
                            (filter #(when % (pos? (compare % current-order))))
                            sort
                            first)
            new-order (frac/generate-key-between current-order next-order)
            new-uuid (random-uuid)]
        (js/console.log "[Local] Enter: update content +" (str block-uuid) "+ create sibling order:" new-order)
        ;; Stash focus target BEFORE the transact: spindel processes
        ;; the change synchronously and the new block's `:on-mount`
        ;; reads `focus-on-mount` during `opt/transact!`. Setting it
        ;; after would race the on-mount and the new block would
        ;; render without focus.
        (reset! focus-on-mount new-uuid)
        (let [current-refs (refs/resolve-references db current-content)
              sibling-refs (refs/resolve-references db sibling-content)]
          (opt/transact! overlay
                         [(cond-> {:entity/uuid block-uuid
                                   :block/content current-content}
                            (seq current-refs)
                            (assoc :block/references current-refs))
                          (cond-> {:entity/uuid new-uuid
                                   :entity/created-at (js/Date.)
                                   :entity/updated-at (js/Date.)
                                   :instance/of-role [:entity/name "S/Block"]
                                   :block/content sibling-content
                                   :block/order new-order
                                   :block/parent [:entity/uuid parent-uuid]}
                            (seq sibling-refs)
                            (assoc :block/references sibling-refs))]))
        new-uuid))))

;; =============================================================================
;; Content Preprocessing
;; =============================================================================

(defn preprocess-page-references
  "Convert [[Page Name]] and @handle patterns in raw HTML to reference marks, so
   agent/intake-written content (which stores plain [[..]]/@handle, not tiptap
   spans) renders as clickable references. Content already typed in tiptap
   arrives with `.page-reference`/`.user-reference` spans and is untouched (the
   @ rule excludes a `>` before the @, so a span's `>@handle` is not re-wrapped).
   Transforms:
     [[Page Name]] -> <span class=\"page-reference\" data-page-name=\"Page Name\">[[Page Name]]</span>
     @handle       -> <span class=\"user-reference\" data-user-name=\"handle\">@handle</span>"
  [html-content]
  (if (string? html-content)
    (-> html-content
        ;; [[Title]] / [[Title][Display]] / [[dh://…][Display]] — same pattern as
        ;; the tokenizer. Both render as marked `[[Display]]` text; a same-KB link
        ;; carries the title in data-page-name, a cross-db link carries its exact
        ;; dh:// pointer in data-ref (+ title tooltip) so the pageReference Mark
        ;; round-trips it instead of ProseMirror dropping the span on load.
        (str/replace refs/page-reference-pattern
                     (fn [[_ target display]]
                       (let [disp (or display target)]
                         (if (str/starts-with? target "dh://")
                           (str "<span class=\"page-reference\" data-ref=\"" target "\" title=\"" target "\">[[" disp "]]</span>")
                           (str "<span class=\"page-reference\" data-page-name=\"" target "\">[[" disp "]]</span>")))))
        ;; Shared HTML-mention pattern (refs/user-mention-html-pattern): captures
        ;; the leading char in $1 and excludes `>` so an existing span isn't
        ;; re-wrapped. Single source of truth with the tokenizer + notify path.
        (str/replace refs/user-mention-html-pattern
                     "$1<span class=\"user-reference\" data-user-name=\"$2\">@$2</span>"))
    html-content))

;; =============================================================================
;; TipTap Editor Management
;; =============================================================================

(defn find-editor-element
  "Find the DOM element containing a TipTap editor for a block-id.
   Searches within the document for elements with matching data-id."
  [block-id]
  (js/document.querySelector (str ".block-content[data-id=\"" block-id "\"]")))

(defn get-editor-for-block
  "Get the TipTap editor for a block-id by finding its DOM element."
  [block-id]
  (when-let [el (find-editor-element block-id)]
    (.-_tiptapEditor el)))

(defn focus-tiptap-editor!
  "Focus the TipTap editor for a block."
  [block-id]
  (js/setTimeout
    (fn []
      (when-let [editor (get-editor-for-block block-id)]
        (.focus (.-commands editor))))
    50))

(defn focus-block-at-position!
  "Focus a block and set cursor position.

   Args:
   - block-id: UUID of block to focus
   - position: :start or :end"
  [block-id position]
  (js/setTimeout
    (fn []
      (when-let [editor (get-editor-for-block block-id)]
        (let [commands (.-commands editor)]
          (.focus commands)
          (case position
            :start (.setTextSelection commands 0)
            :end (.setTextSelection commands (.-size (.-content (.-doc (.-state editor)))))
            nil))))
    50))

(defn restore-focus-after-structural-change!
  "Restore focus to a block after a structural change (indent/outdent).

   Args:
   - block-id: UUID of block to restore focus to
   - delay-ms: Delay in milliseconds (default 350ms to account for database update)"
  ([block-id] (restore-focus-after-structural-change! block-id 350))
  ([block-id delay-ms]
   (js/setTimeout
     (fn []
       (when-let [editor (get-editor-for-block block-id)]
         (js/console.log "[FOCUS-RESTORE] Restoring focus to block:" (str block-id))
         (.focus (.-commands editor)))
       ;; Clear the flag and restore block
       (reset! operation-in-progress? false)
       (reset! focus-restore-block nil))
     delay-ms)))

(defn find-previous-visible-block
  "Find the previous visible (non-hidden) block in the list."
  [current-block-id]
  (let [blocks @visible-blocks-order
        current-idx (.indexOf (clj->js (map :id blocks)) current-block-id)]
    (when (> current-idx 0)
      ;; Find previous non-hidden block
      (loop [idx (dec current-idx)]
        (when (>= idx 0)
          (let [block (nth blocks idx)]
            (if (:hidden block)
              (recur (dec idx))
              (:id block))))))))

(defn find-next-visible-block
  "Find the next visible (non-hidden) block in the list."
  [current-block-id]
  (let [blocks @visible-blocks-order
        current-idx (.indexOf (clj->js (map :id blocks)) current-block-id)]
    (when (and (>= current-idx 0) (< current-idx (dec (count blocks))))
      ;; Find next non-hidden block
      (loop [idx (inc current-idx)]
        (when (< idx (count blocks))
          (let [block (nth blocks idx)]
            (if (:hidden block)
              (recur (inc idx))
              (:id block))))))))

(defn- suggestion-dropdown-open?
  "Is a reference/user suggestion popup actually open?

   Checks for a `.suggestion-dropdown` that has at least one
   `.suggestion-item` child — a real result or the 'No pages found'
   placeholder. This guards against the failure mode where a Tippy
   destroy left an empty `<div class=\"suggestion-dropdown\"></div>`
   orphaned in <body>; the bare-selector check used to find the leaked
   element and route every Enter/ArrowUp/ArrowDown into 'let TipTap
   handle it', breaking new-block-on-Enter."
  []
  (some? (js/document.querySelector ".suggestion-dropdown .suggestion-item")))

(defn make-block-keyboard-extension
  "Create a TipTap extension that handles keyboard shortcuts.

   Parameters:
   - block-id: UUID of the current block
   - db-scope: Optional KB scope UUID string for targeting the correct database

   NOTE: We fetch parent-id and order fresh from the database on each keypress
   to avoid stale state issues. TipTap editors are long-lived and don't automatically
   update when block data changes."
  [block-id & [db-scope]]
  (.create Extension
    #js {:name "blockKeyboard"
         :addKeyboardShortcuts
         (fn []
           (this-as this
                    (let [editor (.-editor this)]
                      #js {"Enter"
                           (fn []
                             ;; Skip if suggestion dropdown is open (let it handle Enter)
                             (if (suggestion-dropdown-open?)
                               false
                               ;; Check if cursor is at the end of the block
                               (let [state (.-state editor)
                                   selection (.-selection state)
                                   $to (.-$to selection)
                                   ;; Check if cursor is at end of its parent node (the paragraph)
                                   parent-offset (.-parentOffset $to)
                                   parent-size (.-size (.-content (.-parent $to)))
                                   at-end? (= parent-offset parent-size)
                                   current-content (.getHTML editor)]
                               (js/console.log "[KB-ENTER] block-id=" (str block-id)
                                               "at-end?=" at-end?
                                               "content=" current-content)
                               (if at-end?
                                 ;; At end: save current content + create new sibling block.
                                 ;; For KB pages, both writes go through a single opt/transact!
                                 ;; so the kb-state listener fires ONCE (instead of twice)
                                 ;; and we get ONE server roundtrip (instead of two).
                                 ;; Stash the new block's UUID in `focus-on-mount` so the
                                 ;; foreign-node's :on-mount callback focuses TipTap as
                                 ;; soon as the new DOM element appears.
                                          (do
                                            (js/console.log "[KB-ENTER] Saving content and creating sibling")
                                            (if-let [overlay (when (and db-scope
                                                                        (trunk? (active-branch-for db-scope)))
                                                               (db-signal/get-kb-overlay db-scope))]
                                              (when-let [new-uuid (update-content-and-create-sibling-local!
                                                                   overlay block-id current-content "")]
                                                (reset! focus-on-mount new-uuid))
                                              (do
                                                (update-block-content-remote! block-id current-content db-scope)
                                       (create-sibling-block-after-remote! block-id "" db-scope)))
                                   true)
                                   ;; Not at end: allow default TipTap behavior (create new paragraph)
                                  (do
                                    (js/console.log "[KB] Not at end, allowing default TipTap behavior")
                                    false)))))

                           "Backspace"
                           (fn []
                             ;; Delete block if empty
                             (let [text (.getText editor)
                                   is-empty? (str/blank? text)]
                               (js/console.log "[KB-BACKSPACE] block-id=" (str block-id)
                                               "is-empty?=" is-empty?
                                               "text=" (pr-str text))
                               (if is-empty?
                                 (do
                                   (js/console.log "[KB-BACKSPACE] Deleting empty block")
                                   (delete-block-remote! block-id db-scope)
                                   true)
                                 ;; Not empty: allow default backspace behavior
                                 false)))

                           "Shift-Enter"
                           (fn []
                             ;; Allow Shift+Enter for line breaks in TipTap
                             false)
                           "Tab"
                           (fn []
                             ;; Indent block (make it child of previous sibling)
                             ;; Server handles all logic - just pass block-id
                             (js/console.log "[KB-TAB] Indenting block:" (str block-id))
                             ;; Set flag to prevent onBlur save during structural change
                             (reset! operation-in-progress? true)
                             (reset! focus-restore-block block-id)
                             ;; Perform the indent operation
                             (indent-block-remote! block-id db-scope)
                             ;; Restore focus after database update completes
                             (restore-focus-after-structural-change! block-id)
                             true)
                           "Shift-Tab"
                           (fn []
                             ;; Outdent block (make it sibling of parent)
                             ;; Server handles all logic - just pass block-id
                             (js/console.log "[KB-TAB] Outdenting block:" (str block-id))
                             ;; Set flag to prevent onBlur save during structural change
                             (reset! operation-in-progress? true)
                             (reset! focus-restore-block block-id)
                             ;; Perform the outdent operation
                             (outdent-block-remote! block-id db-scope)
                             ;; Restore focus after database update completes
                             (restore-focus-after-structural-change! block-id)
                             true)

                           "ArrowUp"
                           (fn []
                             ;; Skip if suggestion dropdown is open (let it handle arrows)
                             (if (suggestion-dropdown-open?)
                               false
                               ;; Navigate to previous block if cursor is at start
                               (let [state (.-state editor)
                                     selection (.-selection state)
                                     $from (.-$from selection)
                                     ;; Check if cursor is at the start of the block
                                     at-start? (= (.-pos $from) 1)]
                                 (js/console.log "[KB-ARROW] ArrowUp pressed, at-start?=" at-start?)
                                 (if at-start?
                                   (do
                                     (js/console.log "[KB-ARROW] Navigating to previous block")
                                     ;; Set flag to prevent onBlur save during navigation
                                     (reset! operation-in-progress? true)
                                     (when-let [prev-block (find-previous-visible-block block-id)]
                                       (focus-block-at-position! prev-block :end))
                                     ;; Clear flag after navigation and blur complete (200ms covers focus + blur delay)
                                     (js/setTimeout #(reset! operation-in-progress? false) 200)
                                     true)
                                   ;; Not at start: allow default arrow behavior
                                   false))))

                           "ArrowDown"
                           (fn []
                             ;; Skip if suggestion dropdown is open (let it handle arrows)
                             (if (suggestion-dropdown-open?)
                               false
                               ;; Navigate to next block if cursor is at end
                               (let [state (.-state editor)
                                     selection (.-selection state)
                                     $to (.-$to selection)
                                     doc-size (.-size (.-content (.-doc state)))
                                     ;; Check if cursor is at the end of the block
                                     at-end? (= (.-pos $to) (dec doc-size))]
                                 (js/console.log "[KB-ARROW] ArrowDown pressed, at-end?=" at-end?)
                                 (if at-end?
                                   (do
                                     (js/console.log "[KB-ARROW] Navigating to next block")
                                     ;; Set flag to prevent onBlur save during navigation
                                     (reset! operation-in-progress? true)
                                     (when-let [next-block (find-next-visible-block block-id)]
                                       (focus-block-at-position! next-block :start))
                                     ;; Clear flag after navigation and blur complete (200ms covers focus + blur delay)
                                     (js/setTimeout #(reset! operation-in-progress? false) 200)
                                     true)
                                 ;; Not at end: allow default arrow behavior
                                 false))))})))}))

;; =============================================================================
;; Runnable Code Block Extension
;; =============================================================================

;; Vár personal AI room UUID — default sandbox for wiki code execution
(def ^:private var-room-uuid #uuid "00000000-0000-0000-0000-000000000301")

(defn make-run-code-extension
  "TipTap extension wrapping CodeBlock with a ▶ Run button.
   The syntax view (Clojure vs Superficie) is controlled globally via
   sig/syntax-pref — no per-block tab state needed here."
  [& [room-uuid]]
  (let [target-room (or room-uuid var-room-uuid)]
    (.extend CodeBlock
             #js {:name "codeBlock"
                  :addNodeView (fn []
                                 (fn [^js _props]
                                   (let [wrapper   (doto (js/document.createElement "div")
                                                     (-> .-className (set! "code-block-wrapper")))
                                         pre       (js/document.createElement "pre")
                                         code-el   (js/document.createElement "code")
                                         run-btn   (doto (js/document.createElement "button")
                                                     (-> .-className (set! "code-block-run-btn"))
                                                     (-> .-title (set! "Run in sandbox (Ctrl+Enter)"))
                                                     (-> .-textContent (set! "▶ Run")))
                                         result-el (doto (js/document.createElement "div")
                                                     (-> .-className (set! "code-block-result"))
                                                     (-> .-style .-display (set! "none")))]
                                     (.appendChild pre code-el)
                                     (.appendChild wrapper run-btn)
                                     (.appendChild wrapper pre)
                                     (.appendChild wrapper result-el)
                                     (set! (.-onclick run-btn)
                                           (fn []
                                             (let [code (.-textContent code-el)]
                                               (when (seq code)
                                                 (set! (.-textContent run-btn) "Running…")
                                                 (let [user-id (when-let [u @sig/current-user] (:id u))
                                                       spin (sandbox-remote/eval-in-room-remote!
                                                              target-room code user-id)]
                                                   (spin
                                                     (fn [result]
                                                       (let [code-result (js/document.createElement "code")
                                                             css (if (:success result)
                                                                   "hljs language-clojure code-block-result--ok"
                                                                   "hljs language-clojure code-block-result--err")]
                                                         (set! (.-className code-result) css)
                                                         (set! (.-innerHTML code-result)
                                                               (or (md/highlight-code (:result result)) (:result result)))
                                                         (set! (.-innerHTML result-el) "")
                                                         (.appendChild result-el code-result)
                                                         (set! (.-style.display result-el) "block"))
                                                       (set! (.-textContent run-btn) "▶ Run"))
                                                     (fn [err]
                                                       (set! (.-textContent run-btn) "▶ Run")
                                                       (js/console.error "[run-code] eval error" err))))))))
                                     #js {:dom wrapper
                                          :contentDOM code-el
                                          :update (fn [^js _] true)})))})))

(defn create-tiptap-editor!
  "Create a TipTap editor for a block.

   Parameters:
   - element: DOM element to mount editor in
   - block-id: UUID of the block
   - initial-content: HTML content to initialize editor with
   - opts (optional): {:db-scope ... :room-uuid ... :autofocus 'start|'end|true|false}

   The editor is stored on the DOM element itself (element._tiptapEditor) rather than
   in a global registry. This allows the same block to be rendered in multiple columns
   without conflicts.

   NOTE: parent-id and order are fetched fresh from the database on each keypress,
   not captured at editor creation time."
  [element block-id initial-content & [db-scope room-uuid autofocus]]
  (let [;; Preprocess content to convert [[Page Name]] to proper marks
        processed-content (preprocess-page-references (or initial-content ""))
        editor (Editor.
                 #js {:element element
                      :extensions #js [;; Disable built-in codeBlock — we provide our own with Run button.
                                       ;; Disable built-in link — we register TiptapLink below with
                                       ;; our own openOnClick/autolink config. Registering both makes
                                       ;; TipTap warn "Duplicate extension names found: ['link']".
                                       (.configure StarterKit #js {:codeBlock false :link false})
                                       ;; External links: plain click FOLLOWS (the
                                       ;; The outline-editor contract: editing happens via
                                       ;; cursor/keyboard); URLs auto-link on type/paste.
                                       ;; Without this extension <a> in stored content
                                       ;; was stripped and URLs stayed dead text.
                                       (.configure TiptapLink
                                                   #js {:openOnClick true
                                                        :autolink true
                                                        :linkOnPaste true
                                                        :HTMLAttributes #js {:target "_blank"
                                                                             :rel "noopener noreferrer"}})
                                       tiptap-refs/page-reference-extension
                                       tiptap-refs/block-reference-extension
                                       tiptap-refs/user-reference-extension
                                       page-suggestion-extension
                                       user-suggestion-extension
                                       (make-block-keyboard-extension block-id db-scope)
                                       (make-run-code-extension room-uuid)]
                      :content processed-content
                      :onBlur (fn [^js props]
                                ;; Save content to Datahike on blur (unless operation in progress)
                                (when-not @operation-in-progress?
                                  (let [html (.getHTML (.-editor props))]
                                    (update-block-content-remote! block-id html db-scope))))
                      :onFocus (fn [_]
                                 ;; Update local UI state
                                 (swap! ui-state assoc :focused-id block-id)
                                 ;; Track db-scope for autocomplete (search-pages)
                                 (reset! current-editor-db-scope db-scope)
                                 ;; Set the parent column as active (only if different)
                                 (when-let [col-el (.closest element ".column")]
                                   (when-let [col-id (.getAttribute col-el "data-col-id")]
                                     ;; Only update if actually changing to avoid re-render loop
                                     (when (not= col-id @sig/active-column-id)
                                       (sig/set-active-column! col-id)))))})]
    ;; Store editor on the DOM element itself
    (set! (.-_tiptapEditor element) editor)
    (set! (.-_blockId element) (str block-id))
    ;; Focus on initial creation if requested. TipTap fires `create`
    ;; once the editor is fully initialised AND attached. Calling
    ;; .commands.focus() at constructor time can no-op if the element
    ;; isn't yet in document.body; the `create` event guarantees it is.
    (when autofocus
      (.on editor "create"
           (fn []
             (let [cmds (.-commands editor)]
               (.focus cmds)
               (case autofocus
                 "start" (.setTextSelection cmds 0)
                 "end"   (.setTextSelection cmds
                                            (.-size (.-content (.-doc (.-state editor)))))
                 nil)))))
    editor))

(defn destroy-tiptap-editor!
  "Destroy a TipTap editor stored on a DOM element."
  [element]
  (when element
    (when-let [editor (.-_tiptapEditor element)]
      (.destroy editor)
      (set! (.-_tiptapEditor element) nil))))

;; =============================================================================
;; Block Rendering
;; =============================================================================

(defn render-viz-block
  "Render a Vega-Lite visualization block.
   viz-spec is a string containing EDN Vega-Lite spec.

   If the spec contains :data {:db-query {:query [...] :columns [...] :args [...]}}
   the plot is reactive: it re-runs the Datalog query whenever the Datahike DB
   updates and refreshes the Vega view in place.

   db-scope: optional KB scope UUID string. When provided, the KB-specific
   Datahike connection is watched. Otherwise the shared local-conn is used.

   Example reactive spec:
     {:$schema \"https://vega.github.io/schema/vega-lite/v5.json\"
      :mark \"line\"
      :encoding {:x {:field \"step\" :type \"quantitative\"}
                 :y {:field \"loss\" :type \"quantitative\"}}
      :data {:db-query {:query [:find ?step ?loss
                                :in $ ?run
                                :where [?m :metric/run ?run]
                                       [?m :metric/step ?step]
                                       [?m :metric/value ?loss]]
                        :columns [\"step\" \"loss\"]
                        :args [\"my-run-id\"]}}}"
  [block & [db-scope]]
  (let [block-id  (str (:id block))
        watcher-k (keyword "viz-watcher" block-id)
        ;; Get the appropriate datahike connection to watch.
        ;; Datahike connections implement IWatchable (unlike spindel SignalRefs).
        get-conn  (fn []
                    (if db-scope
                      (db-signal/get-kb-conn db-scope)
                      @db-signal/local-conn))]
    (foreign-node
      {:key (str "viz-" block-id)
       :class "block-viz"
       :data-id block-id
       :on-mount
       (fn [el]
         (when (exists? js/vegaEmbed)
           (try
             (let [raw-spec  (reader/read-string (:viz-spec block))
                   db-query  (-> raw-spec :data :db-query)
                   ;; Strip :db-query before passing to vegaEmbed (Vega doesn't know it)
                   vega-spec (if db-query
                               (update raw-spec :data dissoc :db-query)
                               raw-spec)
                   ;; Run query against a DB value, returning row maps
                   run-query (fn [db]
                               (when (and db-query db)
                                 (let [{:keys [query columns args]} db-query
                                       rows (apply dh/q query db (or args []))]
                                   (mapv (fn [row] (zipmap columns (seq row))) rows))))
                   ;; Seed spec with initial data from current DB
                   init-db   (when-let [c (get-conn)] (dh/db c))
                   init-data (run-query init-db)
                   embed-spec (if init-data
                                (assoc-in vega-spec [:data :values] init-data)
                                vega-spec)]
               (-> (js/vegaEmbed el (clj->js embed-spec)
                     ;; Export ON, the rest OFF. `:actions false` hid the whole
                     ;; menu, which also removed the only way to get a chart
                     ;; OUT of the page — the one action a reader actually
                     ;; wants. `:source`/`:compiled`/`:editor` stay off: they
                     ;; open the Vega spec and the online editor, which is
                     ;; developer surface, not reader surface.
                     (clj->js {:actions {:export true :source false
                                         :compiled false :editor false}
                               :renderer "svg" :theme "quartz"}))
                   (.then
                     (fn [result]
                       (let [view (.-view result)]
                         (set! (.-_vegaView el) view)
                         ;; Watch the datahike connection for DB changes
                         (when db-query
                           (when-let [conn (get-conn)]
                             (add-watch conn watcher-k
                               (fn [_ _ old-db new-db]
                                 ;; Only update when the DB actually changed
                                 (when-not (= (:max-tx old-db) (:max-tx new-db))
                                   (when-let [data (run-query new-db)]
                                     (.data view "source_0" (clj->js data))
                                     (-> (.runAsync view)
                                         (.catch (fn [e]
                                                   (js/console.error "Viz reactive update error:" e)))))))))))))
                   (.catch
                     (fn [e]
                       (js/console.error "Vega-Lite embed error:" e)
                       (set! (.-textContent el) (str "Chart error: " (.-message e)))))))
             (catch :default e
               (js/console.error "Vega-Lite render error:" e)
               (set! (.-textContent el) (str "Viz error: " (.-message e)))))))
       :on-unmount
       (fn [_el]
         ;; Remove watcher from the connection to avoid leaks
         (when-let [conn (get-conn)]
           (remove-watch conn watcher-k))
         ;; Finalize Vega view; spindel passes nil as _el, guard before property access
         (when _el
           (when-let [view (.-_vegaView _el)]
             (.finalize view))))})))

(defn- code-block? [block]
  (str/includes? (or (:content block) "") "<pre>"))

(defn- extract-code-text
  "Extract plain text from a code block's HTML content."
  [html]
  (when html
    (let [tmp (js/document.createElement "div")]
      (set! (.-innerHTML tmp) html)
      (some-> (.querySelector tmp "code") (.-textContent)))))

(defn- render-widget-block
  "Render a block whose :widget-code is a Clojure expression evaluated in
   the curated client-side SCI sandbox. Returns a vnode or an error indicator."
  [block db conn]
  (let [{:keys [ok error]} (widget-sandbox/eval-widget (:widget-code block) db conn)]
    (el/div {:class "block-content widget-block"}
      (if ok
        ok
        (el/div {:class "widget-error"
                 :style "color:#c33;background:#fee;border:1px solid #fcc;padding:8px;border-radius:4px;font-family:monospace;white-space:pre-wrap"}
          (el/strong {} "widget error: ")
          (el/span {} (str error)))))))

(defonce render-block-count (atom 0))

(defn render-block
  "Render a single block with TipTap editor, Vega-Lite visualization, or widget.
   syntax-pref: :clojure or :superficie — controls how code blocks are displayed.
   db: current Datahike DB value (for widget reads).
   conn: current Datahike connection (for widget writes through kabel writer).
   active-branch: branch keyword (`:db` for trunk). Embedded in the
   foreign-node's :key so a branch switch unmounts + remounts TipTap
   with the new branch's content. Within a branch the key is stable,
   so normal typing reuses the same TipTap instance + DOM nodes."
  [block depth has-children & [db-scope syntax-pref db conn active-branch]]
  (swap! render-block-count inc)
  (let [hidden?  (:hidden block)
        viz?     (some? (:viz-spec block))
        widget?  (some? (:widget-code block))
        code?    (code-block? block)
        sup?     (and code? (= syntax-pref :superficie))]
    (el/div {:key (str "block-" (:id block))
             :class (str "block"
                         (when (:focused block) " focused")
                         (when hidden? " hidden")
                         (when viz? " block-viz-wrapper")
                         (when widget? " block-widget-wrapper"))
             :style (str "margin-left: " (* depth 24) "px"
                         (when hidden? "; display: none"))
             :data-id (str (:id block))}
      ;; Collapse toggle
      (el/div {:class "block-bullet"}
        (if has-children
          (el/span {:class (str "collapse-toggle" (when (:collapsed block) " collapsed"))
                    :data-action "toggle"
                    :data-id (str (:id block))}
            (if (:collapsed block) "+" "-"))
          (el/span {:class "bullet"} "\u2022")))
      ;; Content: widget, Vega-Lite viz, Superficie read-only view, or TipTap editor
      (cond
        widget?
        (render-widget-block block db conn)

        viz?
        (render-viz-block block db-scope)

        sup?
        (let [sup-text (try (toSup (extract-code-text (:content block)))
                            (catch :default _ (extract-code-text (:content block))))]
          (el/div {:class "block-content code-block-wrapper"}
            (el/pre {:class "code-panel-sup"}
              (el/code {:class "hljs language-superficie"
                        :innerHTML (or (md/highlight-code sup-text "superficie") sup-text)}))))

        :else
        (foreign-node
          {:key (str "editor-"
                     (name (or active-branch :db))
                     "-"
                     (:id block))
           :class "block-content"
           :data-id (str (:id block))
           :on-mount (fn [el]
                       ;; `focus-on-mount` carries the UUID of the
                       ;; most recently created block (set by the
                       ;; Enter handler before `opt/transact!`). The
                       ;; new block's `:on-mount` fires once and we
                       ;; autofocus iff this mount matches the pending
                       ;; UUID, then clear the atom so a stale UUID
                       ;; doesn't accidentally focus a later mount.
                       ;;
                       ;; With spindel's discharge in-place
                       ;; reconciliation (`:update` deltas reuse the
                       ;; DOM element when `:tag`+`:key`+`:addr` match
                       ;; — see dom/discharge.cljc `reconcile-vnode!`),
                       ;; `:on-mount` no longer re-fires on each
                       ;; per-render closure flip; only the actual
                       ;; mount of the new element triggers us.
                       (when-not (.-_tiptapEditor el)
                         (let [should-focus? (= @focus-on-mount (:id block))]
                           (when should-focus? (reset! focus-on-mount nil))
                           (create-tiptap-editor!
                             el (:id block) (:content block) db-scope nil
                             (when should-focus? "start")))))
           :on-unmount (fn [el]
                         (destroy-tiptap-editor! el))})))))

;; =============================================================================
;; Main App Spin
;; =============================================================================

(defn make-app-spin
  "Create the main app spin that renders blocks from Datahike.

   Pipeline:
   1. Track db-signal/local-db - signal holding synced Datahike db
   2. Query blocks for page-uuid using query-with-deltas
   3. Transform Datahike blocks to UI format
   4. Render with ifor-each for incremental updates"
  [page-uuid]
  (spin
    (let [;; Track the Datahike database signal (direct def, not a function)
          db-iv (track db-signal/local-db)

          ;; Query blocks under the page with delta detection
          dh-blocks-iv (dq/query-with-deltas
                         db-iv
                         (make-page-blocks-query page-uuid)
                         :entity/uuid
                         [:page-blocks page-uuid])

          ;; Transform Datahike format to UI format
          focused-id (:focused-id @ui-state)
          blocks-iv (transform-datahike-blocks dh-blocks-iv focused-id)

          ;; Build indices for tree operations
          all-blocks (iv/get-new blocks-iv)
          block-index (when all-blocks (build-block-index all-blocks))
          children-index (when all-blocks (build-children-index all-blocks))]

      (el/div {:class "block-editor-app"}
        (el/header {:class "editor-header"}
          (el/h1 "Simmis Block Editor")
          (el/p {:class "subtitle"} "Datahike-backed reactive editor"))

        (el/div {:class "blocks-container"}
          (if (seq all-blocks)
            (ifor-each :id blocks-iv
              (fn [block]
                (let [depth (block-depth block block-index)
                      has-children (has-children? (:id block) children-index)]
                  (render-block block depth has-children))))
            ;; No blocks yet - auto-creation happens during init
            (el/div {:class "empty-state"}
              (el/p "Loading..."))))

        (el/footer {:class "editor-footer"}
          (el/p {:class "help-text"}
            "Enter: new block | Backspace (empty): delete | Tab: indent | Shift+Tab: outdent"))))))

;; =============================================================================
;; Event Handling
;; =============================================================================

(defn handle-keydown [e]
  (let [target (.-target e)
        key (.-key e)
        shift? (.-shiftKey e)
        block-el (.closest target ".block")
        block-id-str (when block-el (.getAttribute block-el "data-id"))
        block-id (when block-id-str (uuid block-id-str))]
    (case key
      ;; Enter is handled by TipTap's keyboard extension

      "Backspace"
      (when block-id
        (let [editor (get-editor-for-block block-id)
              content (when editor (.getText editor))]
          (when (str/blank? content)
            (.preventDefault e)
            (delete-block-remote! block-id))))

      "Tab"
      (when block-id
        (.preventDefault e)
        (if shift?
          (outdent-block-remote! block-id)
          (indent-block-remote! block-id)))

      nil)))

(defn handle-click [e]
  (let [target (.-target e)
        ;; Find toggle element - either clicked directly or find it inside .block-bullet parent
        toggle-el (or (.closest target ".collapse-toggle")
                      (when-let [bullet (.closest target ".block-bullet")]
                        (.querySelector bullet ".collapse-toggle")))]
    (when toggle-el
      (let [block-id-str (.getAttribute toggle-el "data-id")
            block-id (when block-id-str (uuid block-id-str))
            is-collapsed (.contains (.-classList toggle-el) "collapsed")
            new-state (not is-collapsed)]
        (js/console.log "[Click] Toggling collapsed for:" (str block-id) "to:" new-state)
        (update-block-collapsed-remote! block-id new-state)))))

;; =============================================================================
;; Initialization
;; =============================================================================

(defn- ensure-first-block!
  "Ensure the page has at least one block. Creates an empty block if none exist."
  [page-uuid]
  (binding [rtc/*execution-context* runtime]
    (when-let [db @db-signal/local-db]
      ;; Query blocks under this page (with parent for filtering)
      (let [all-blocks (dh/q '[:find [(pull ?b [:entity/uuid {:block/parent [:entity/uuid]}]) ...]
                               :where
                               [?b :block/parent _]]
                             db)
            page-blocks (filter #(= page-uuid (get-in % [:block/parent :entity/uuid])) all-blocks)]
        (js/console.log "[BlockEditor] Found" (count page-blocks) "blocks for page" (str page-uuid))
        (when (empty? page-blocks)
          (js/console.log "[BlockEditor] No blocks found, creating first block...")
          ;; Pass nil for order to let server generate first fractional index
          (create-block-remote! page-uuid "" nil))))))

(defn- render-ui!
  "Render the UI after connection is established."
  [page-uuid]
  (let [container (js/document.getElementById "editor-container")
        discharge (browser/make-dom-discharge js/document)]

    (when-not container
      (js/console.error "Container #editor-container not found!")
      (throw (js/Error. "Container #editor-container not found")))

    ;; Clear any existing render
    (when @render-handle
      (set! (.-innerHTML container) ""))

    ;; Render the app
    (binding [rtc/*execution-context* runtime]
      (let [app-spin (make-app-spin page-uuid)]
        (reset! render-handle
                (render/render-spin! container app-spin discharge))))

    ;; Event listeners
    (.addEventListener container "keydown" (rtc/make-handler runtime handle-keydown))
    (.addEventListener container "click" (rtc/make-handler runtime handle-click))

    (js/console.log "Simmis Block Editor UI rendered!")))

(defn ^:export init
  "Initialize block editor.
   Can be called with a page-uuid string, or will look for SIMMIS_PAGE_UUID global.

   Usage from browser console:
     is.simm.uis.web.desktop.block_editor.init('30b7101d-db61-4266-a616-cb7bd6546784')

   Or set window.SIMMIS_PAGE_UUID before loading."
  ([]
   ;; Try to get page UUID from global variable, or use default
   (let [page-uuid-str (or (aget js/window "SIMMIS_PAGE_UUID")
                           DEFAULT_PAGE_UUID)]
     (js/console.log "[BlockEditor] Using page UUID:" page-uuid-str)
     (init page-uuid-str)))
  ([page-uuid-str]
   (js/console.log "Initializing Simmis Block Editor...")

   (let [page-uuid (uuid page-uuid-str)]
    (reset! current-page-uuid page-uuid)

    ;; Async initialization sequence
    (go-try S
      ;; Step 1: Connect to server
      (js/console.log "[BlockEditor] Step 1/5: Connecting to server...")
      (<? S (connect-to-server!))
      (js/console.log "[BlockEditor] Step 1/5: Connected to server!")

      ;; Step 2: Initialize reactive database
      ;; Runtime is now shared via runtime.cljc, just pass kabel-peer
      (js/console.log "[BlockEditor] Step 2/5: Initializing reactive database...")
      (<? S (db-signal/init-reactive-db! kabel-client))
      (js/console.log "[BlockEditor] Step 2/5: Reactive database initialized!")

      ;; Step 3: Ensure page exists (creates if needed)
      (js/console.log "[BlockEditor] Step 3/5: Ensuring page exists...")
      (<? S (ensure-page-remote! page-uuid "Block Editor Page"))
      (js/console.log "[BlockEditor] Step 3/5: Page ensured!")

      ;; Step 4: Ensure page has at least one block
      ;; Give the sync a moment to propagate the page creation
      (<? S (timeout 500))
      (js/console.log "[BlockEditor] Step 4/5: Ensuring first block exists...")
      (ensure-first-block! page-uuid)

      ;; Step 5: Render UI
      (js/console.log "[BlockEditor] Step 5/5: Rendering UI...")
      (render-ui! page-uuid)
      (js/console.log "[BlockEditor] Initialization complete!")))))

;; =============================================================================
;; Debug Functions
;; =============================================================================

(defn ^:export get-db-info
  "Get info about the current local database state."
  []
  (binding [rtc/*execution-context* runtime]
    (if-let [db @db-signal/local-db]
      (do
        (js/console.log "[INFO] Local DB max-tx:" (:max-tx db))
        {:max-tx (:max-tx db) :has-db true})
      (do
        (js/console.log "[INFO] Local DB not loaded yet")
        {:max-tx nil :has-db false}))))

(defn ^:export list-pages
  "List all pages in the database."
  []
  (binding [rtc/*execution-context* runtime]
    (if-let [db @db-signal/local-db]
      ;; via `dq/page-role-eid` rather than a lookup ref: this is a console
      ;; helper and someone will reach for it while time-travelled, where the
      ;; lookup-ref form throws instead of reporting an empty wiki.
      (let [pages (dq/all-pages-query db)]
        (js/console.log "[PAGES] Found" (count pages) "pages:")
        (doseq [page pages]
          (js/console.log "  -" (str (:entity/uuid page)) "-" (or (:S.Page/title page) "(untitled)")))
        pages)
      (do
        (js/console.log "[PAGES] Database not loaded")
        nil))))

(defn ^:export list-blocks
  "List all blocks in the database."
  []
  (binding [rtc/*execution-context* runtime]
    (if-let [db @db-signal/local-db]
      (let [blocks (dh/q '[:find [(pull ?b [:entity/uuid :block/content :block/order
                                            {:block/parent [:entity/uuid]}]) ...]
                           :where [?b :block/content]]
                         db)]
        (js/console.log "[BLOCKS] Found" (count blocks) "blocks:")
        (doseq [block blocks]
          (js/console.log "  -" (str (:entity/uuid block))
                          "parent:" (str (get-in block [:block/parent :entity/uuid]))
                          "content:" (subs (or (:block/content block) "") 0 (min 50 (count (:block/content block))))))
        blocks)
      (do
        (js/console.log "[BLOCKS] Database not loaded")
        nil))))

;; =============================================================================
;; Embeddable Component
;; =============================================================================

(defn render-page-header
  "Render the page header with type tags and property box.

   Args:
   - page-uuid - UUID of the page
   - db - Current database value
   - db-scope - (optional) KB scope-uuid. When present, the header
                shows a branch pill on the top row whenever the user is
                viewing a non-trunk branch. Pill is read-only in v1;
                merge/discard live in the sidebar branches tree.
   - read-only? - (optional) true while the workspace is scrubbed off `now`.
                Passed DOWN rather than tracked here: the caller already
                tracks `sig/global-ref` at the top of its spin, and a second
                track inside a nested spin is the placement sharp edge.

   Returns a spin that produces the vnode for the page header."
  [page-uuid db & [db-scope read-only?]]
  ;; Wrap in spin to allow awaiting nested spins (like property-box)
  (spin
    (let [;; Query page types and properties
        page-types (dq/page-types-query db page-uuid)
        available-types (dq/all-types-query db)
        {:keys [properties values]} (dq/page-properties-query db page-uuid)

        ;; Check if this page is a type (for showing property definitions)
        type-entity (dq/is-type-page? db page-uuid)
        type-name (when type-entity (:entity/name type-entity))
        ;; Get properties defined ON this type (morphisms where type is source)
        type-properties (when type-name (dq/type-properties-query db type-name))

        ;; Handlers for type operations
        on-tag-click (fn [type-entity]
                       ;; Navigate to type page
                       (when-let [type-uuid (:entity/uuid type-entity)]
                         ;; The type entity came out of THIS page's db, so its
                         ;; uuid only identifies a page together with this
                         ;; scope — see on-instance-click below.
                         (sig/open-tab! :wiki (cond-> {:page-uuid type-uuid}
                                                db-scope (assoc :db-scope db-scope))
                                        {:title (types/format-type-name (:entity/name type-entity))})))

        on-remove-tag (fn [type-entity]
                        (when-not (refuse-past-write! "remove a type tag")
                        ;; Remove type from page (transact locally)
                        ;; Through the server: it resolves the page's OWN store
                        ;; (and branch). This used to transact into the client's
                        ;; default conn — the app store — so a type set on a KB
                        ;; page landed in a different database and the UI just
                        ;; appeared to do nothing.
                        (go
                          (try
                            (<! (remote/remove-type-remote! page-uuid (:entity/uuid type-entity)
                                                        db-scope))
                            (catch :default e
                              (js/console.error "[page-header] Failed to remove type:" e)
                              (binding [rtc/*execution-context* runtime]
                                (sig/show-error! "Could not remove that type.")))))))

        on-add-type (fn [type-entity]
                        (when-not (refuse-past-write! "add a type tag")
                      (go
                        (try
                          (<! (remote/add-type-remote! page-uuid (:entity/uuid type-entity)
                                                   db-scope))
                          (binding [rtc/*execution-context* runtime]
                            (sig/close-type-selector!))
                          (catch :default e
                            (js/console.error "[page-header] Failed to add type:" e)
                            (binding [rtc/*execution-context* runtime]
                              (sig/show-error! "Could not add that type.")))))))

        ;; Handler for adding a new property to this type.
        ;; The client sends the SHAPE it wants; the server derives the attribute
        ;; keyword and transacts it, because `:db/ident` is append-only schema
        ;; and was previously declarable from any browser, unchecked.
        on-add-property (fn [{:keys [name property-type target-type-uuid cardinality optional?]}]
                          (when (and type-name (seq name))
                            (let [formatted-name (-> name str/trim str/lower-case
                                                     (str/replace #"\s+" "-"))
                                  type-short-name (types/format-type-name type-name)
                                  morphism-name (str "S/" type-short-name "/" formatted-name)
                                  is-relation? (and target-type-uuid (seq target-type-uuid))
                                  ;; The primitive OBJECT this property points at.
                                  ;; `:color` used to resolve "S/Keyword", which
                                  ;; exists in no store — every colour property
                                  ;; failed. A colour is a string.
                                  ;; The primitive object NAMES the type; the
                                  ;; server derives `:db/valueType` from it via
                                  ;; `schema/codomain->db-type`. A UI number
                                  ;; field accepts decimals, so it is S/Float —
                                  ;; S/Number is the integer primitive, and
                                  ;; sending it here while separately declaring
                                  ;; :db.type/double is how the client's own
                                  ;; copy of that table came to disagree with
                                  ;; the canonical one.
                                  target-object (cond
                                                  (= :number property-type) "S/Float"
                                                  (= :checkbox property-type) "S/Boolean"
                                                  (= :date property-type) "S/Date"
                                                  :else "S/String")]
                              (go
                                (try
                                  (<! (remote/add-property-remote!
                                       {:type-page-uuid page-uuid
                                        :morphism-name morphism-name
                                        :cardinality (if (= cardinality :many)
                                                       :db.cardinality/many
                                                       :db.cardinality/one)
                                        :optional? optional?
                                        :property-type (or property-type :text)
                                        :target-type-uuid (when is-relation? (uuid target-type-uuid))
                                        :target-object-name (when-not is-relation? target-object)}
                                       db-scope))
                                  (catch :default e
                                    (js/console.error "[page-header] Failed to create property:" e)
                                    (binding [rtc/*execution-context* runtime]
                                      (sig/show-error! "Could not create that property."))))))))

        ;; Handler for removing a property from this type.
        ;; DESTRUCTIVE: it retracts every VALUE stored under the attribute, not
        ;; just the definition. Against the app store that found nothing, so it
        ;; looked harmless; pointed at the store the pages actually live in it
        ;; deletes user data — hence the confirmation.
        on-remove-property (fn [property]
                        (when-not (refuse-past-write! "remove a property")
                             (let [prop-name (:entity/name property)]
                               (when (js/confirm
                                      (str "Remove the property \"" prop-name "\"?\n\n"
                                           "This also deletes its value on every page "
                                           "that has one. This cannot be undone."))
                                 (go
                                   (try
                                     (let [res (<! (remote/remove-property-remote!
                                                    (:entity/uuid property) prop-name db-scope))]
                                       (js/console.log "[page-header] Removed property:" prop-name
                                                       "values:" (:values-removed res)))
                                     (catch :default e
                                       (js/console.error "[page-header] Failed to remove property:" e)
                                       (binding [rtc/*execution-context* runtime]
                                         (sig/show-error! "Could not remove that property.")))))))))

        ;; Handler for saving a property value on this page
        on-save-property (fn [property new-value]
                        (when-not (refuse-past-write! "edit a property")
                           (go
                             (try
                               (<! (remote/save-property-remote! page-uuid (:entity/name property)
                                                             new-value db-scope))
                               (catch :default e
                                 (js/console.error "[page-header] Failed to save property:" e)
                                 (binding [rtc/*execution-context* runtime]
                                   (sig/show-error! "Could not save that property.")))))))

        page-title (dq/page-title-query db page-uuid)

        ;; Rename is sent as an INSTRUCTION, not a transaction. The server
        ;; repoints inbound `:block/references` and rewrites the literal
        ;; `[[Old Title]]` text of every referring block — a transaction that is
        ;; O(referrers × content), against a constant-size instruction, over
        ;; content the server already holds. It is also the only place the
        ;; rename invariant (and, soon, authorization) can be enforced.
        ;;
        ;; Only the title datom is applied optimistically: one datom, instant
        ;; feedback. The reference rewrites arrive through store sync, and the
        ;; overlay drops when the durable db carries the new title.
        restore-title-text!
        (fn [el] (when el (set! (.-textContent el) (or page-title "Untitled"))))

        commit-title!
        (fn [el new-title]
          (let [new-title (str/trim (or new-title ""))
                branch-kw (when db-scope (get @sig/active-kb-branch (str db-scope) :db))]
            (cond
              ;; Viewing the past: put the old title back and refuse. The title
              ;; is the one editable surface the read-only CSS never covered —
              ;; it sits outside `.blocks-container`/`.block-content` — and the
              ;; write below goes to the LIVE conn on the CURRENT branch, so a
              ;; rename here edited the present while the banner said past.
              (refuse-past-write! "rename a page")
              (restore-title-text! el)

              ;; Empty title: nothing to do, put the old one back.
              (or (empty? new-title) (= new-title page-title))
              (restore-title-text! el)

              ;; Pre-flight the conflict against our own replica. An overlay the
              ;; server rejects would linger until its TTL — overlays clear only
              ;; when their caught-up predicate turns true, and that predicate
              ;; can never fire for a rename that never happened.
              (dq/page-with-title-query db new-title page-uuid)
              (do (restore-title-text! el)
                  (binding [rtc/*execution-context* runtime]
                    (sig/show-error!
                      (str "A page named \"" new-title "\" already exists in this wiki."))))

               :else
               (let [overlay (if db-scope
                               (db-signal/get-kb-overlay db-scope)
                               (db-signal/get-overlay))
                     prediction
                     (when overlay
                       (opt/predict!
                        overlay
                        [[:db/add [:entity/uuid page-uuid] :S.Page/title new-title]]
                        (fn [durable-db]
                          (= new-title (dq/page-title-query durable-db page-uuid)))))]
                 (go
                   (let [res (<! (remote/rename-page-remote!
                                  page-uuid new-title
                                 (cond-> {}
                                   db-scope (assoc :db-scope db-scope)
                                   (and branch-kw (not= :db branch-kw))
                                    (assoc :branch-kw branch-kw))))]
                     (cond
                       (:success res)
                       (do
                         (when prediction
                           (opt/ack! overlay (:ov-id prediction) res))
                         (js/console.log "[page-header] renamed;" (:blocks-updated res)
                                         "referring block(s) updated"))

                      ;; Lost a race against another writer. The overlay expires
                      ;; on its own TTL; put the visible text back now.
                       (:conflict res)
                       (do (when prediction
                             (opt/reject! overlay (:ov-id prediction)
                                          (ex-info "Page title conflict"
                                                   {:type :rename/conflict})))
                           (restore-title-text! el)
                           (binding [rtc/*execution-context* runtime]
                             (sig/show-error!
                              (str "A page named \"" new-title "\" already exists in this wiki."))))

                       :else
                       (do (when prediction
                             (opt/reject! overlay (:ov-id prediction)
                                          (ex-info "Page rename failed"
                                                   {:type :rename/failed
                                                    :response res})))
                           (restore-title-text! el)
                           (binding [rtc/*execution-context* runtime]
                             (sig/show-error! "Rename failed."))))))))))]

    (el/div {:class "page-header"}
      ;; Title + type tags share one row: the tags sit to the RIGHT of the title
      ;; text, always visible — no expander gating them.
      (el/div {:class "page-title-row"}
        ;; Inline-editable page title. `contenteditable` on a plain element
        ;; rather than an input so it wraps and inherits typography; commit on
        ;; blur or Enter, abandon on Escape.
        (el/h1 {:class (str "page-title" (when read-only? " page-title--read-only"))
                ;; Not merely styled off: `contenteditable` IS the affordance,
                ;; and the read-only CSS never reached this element.
                :contenteditable (if read-only? "false" "true")
                :spellcheck "false"
                :data-placeholder "Untitled"
                :on-blur (fn [e] (commit-title! (.-target e) (.-textContent (.-target e))))
                :on-keydown (fn [e]
                              (case (.-key e)
                                "Enter" (do (.preventDefault e) (.blur (.-target e)))
                                "Escape" (do (restore-title-text! (.-target e))
                                             (.blur (.-target e)))
                                nil))}
          (or page-title "Untitled"))
        ;; Type tags inline, right of the title. The "+" add button is revealed
        ;; on hover (CSS). Rendered whenever there are tags OR types to add.
        (when (or (seq page-types) (seq available-types))
          (el/div {:class "page-header-types"}
            (types/type-tags-with-add
              {:types page-types
               :available-types available-types
               :on-tag-click on-tag-click
               :on-remove-tag on-remove-tag
               :on-add-type on-add-type
               :on-create-type nil}))))
      ;; Branch chrome — only visible when viewing a non-trunk branch.
      ;; Read-only pill in v1 (sidebar tree owns merge/discard actions).
      ;; Same DOM root is always rendered to keep layout stable; the pill
      ;; itself is just an extra child element that appears/disappears.
      (let [active-branch (when db-scope (get @sig/active-kb-branch (str db-scope) :db))
            row (when active-branch
                  (get-in @sig/kb-branches [(str db-scope) active-branch]))]
        (when (and active-branch (not= :db active-branch))
          (el/div {:class "page-header-branch"}
            (el/span {:class "branch-pill"}
              (el/span {:class "branch-pill-marker"} "●")
              (el/span {:class "branch-pill-label"}
                (bn/display-name active-branch row))))))

      ;; Property definitions — only for TYPE pages (a page that IS a type,
      ;; e.g. S/Company), shown directly. Regular pages never see this.
      (when type-entity
        (el/div {:class "page-header-property-definitions"}
          (types/property-definitions-box
            {:type-name type-name
             :properties type-properties
             :available-types available-types
             :on-property-click nil
             :on-add-property on-add-property
             :on-remove-property on-remove-property})))

      ;; Property VALUES stay visible when present — they're content,
      ;; not machinery.
      (when (seq properties)
        (el/div {:class "page-header-properties"}
          (await (types/property-box
                   {:properties properties
                    :values values
                    :on-save on-save-property
                    :on-property-click nil}))))))))

(defn render-page-editor
  "Render the page editor as an embeddable component using incremental rendering.

   Self-tracking: instead of receiving a db interval from a parent
   spin, this looks up its own KB's db signal (`kb-db-signal db-scope`)
   and tracks it directly. A write to a different KB doesn't fire this
   spin; only writes to *this* KB do. The parent columns/column spins
   no longer need to close over kb-states, so they stay stable when a
   KB is edited.

   Args:
   - page-uuid - UUID of the page to edit
   - db-scope  - KB scope-uuid (string or uuid). nil → shared local-db.

   Returns a spin that produces the vnode for the blocks editor."
  [page-uuid & [db-scope]]
  ;; Wrap in spin to allow awaiting nested spins (like render-page-header)
  (spin
    (let [;; GlobalCut tracked at the TOP (sharp edge #1). ref=:now →
          ;; the untouched live signal (hot path unchanged); {:as-of T} →
          ;; the (scope,ref) registry signal carrying the as-of db, and
          ;; the page renders read-only.
          gref (iv/get-new (track sig/global-ref))
          ;; Tracked at the TOP too (sharp edge #1). It used to live inside the
          ;; `db-value` branch below, so it was registered only once the db had
          ;; arrived — a conditional subscription. Tracks must not be downstream
          ;; of a branch or of interval consumers.
          syntax-pref (iv/get-new (track sig/syntax-pref))
          ref (or gref :now)
          time-travel? (not= ref :now)
          db-sig (db-signal/ensure-view-db-signal! db-scope ref)
          db-iv (track db-sig)
          trunk-db (iv/get-new db-iv)

          ;; Branch projection: if the user has switched this KB to a
          ;; non-trunk branch, project the db at that branch's HEAD via
          ;; `dh/branch-as-db`. In CLJS this is async (returns a
          ;; channel), so we can't project synchronously inside the
          ;; spin. Instead:
          ;;   1. Track `sig/projected-branch-dbs` so the spin re-fires
          ;;      when a projection lands.
          ;;   2. Kick off the projection as a side effect when on a
          ;;      fork. The go-block writes the result back into the
          ;;      signal, which retriggers (1).
          ;;
          ;; Eventually consistent: first render shows trunk briefly,
          ;; then snaps to branch once the konserve read completes.
          ;; Kabel store sync replicates every branch's nodes, so the
          ;; projection resolves locally without an RPC.
          active-branches (iv/get-new (track sig/active-kb-branch))
          projected-map (iv/get-new (track sig/projected-branch-dbs))
          active-branch (when db-scope
                          (get active-branches (str db-scope) :db))
          on-fork? (and db-scope active-branch (not= :db active-branch))
          proj-key (when on-fork? [(str db-scope) active-branch])
          projected-db (when proj-key (get projected-map proj-key))
          db-value (cond
                     time-travel? trunk-db          ; as-of db; no branch overlay
                     (nil? trunk-db) nil
                     (not on-fork?) trunk-db
                     projected-db projected-db
                     :else trunk-db)
          ;; Side effect: (re)project the branch view via `dh/branch-as-db`
          ;; when we're on a fork and the cached projection is stale.
          ;;
          ;; The swap! that publishes the new projection re-fires this
          ;; spin, so we must guard against re-entry: only kick off a
          ;; projection when the local trunk-db advanced past the cached
          ;; projection's :max-tx (or when no cache exists). The go-block
          ;; ALSO guards its swap! the same way, in case multiple
          ;; projections race. Together this terminates the trivial fixed
          ;; point: once cached + max-tx aligned, the spin re-runs without
          ;; triggering any swap.
          stale? (and (not time-travel?) on-fork?
                      (or (nil? projected-db)
                          (not= (:max-tx projected-db) (:max-tx trunk-db))))
          _ (when stale?
              (when-let [conn (db-signal/get-kb-conn db-scope)]
                (go
                  (try
                    ;; Read the stored branch root and materialize a DB over
                    ;; the local replica. Datahike's current canonical CBOR
                    ;; codec reconstructs persistent-sorted-set roots while
                    ;; decoding, so the old Fressian deferred-index repair is
                    ;; neither available nor necessary.
                    (let [store (:store conn)
                          raw-ch (kc/get store active-branch nil {:sync? false})
                          raw-db (<? S raw-ch)
                          new-projected (when raw-db
                                          (dw/stored->db raw-db store))]
                      (when (and new-projected
                                 (not= (:max-tx new-projected)
                                       (:max-tx projected-db)))
                        (binding [rtc/*execution-context* runtime]
                          (swap! sig/projected-branch-dbs assoc proj-key new-projected))))
                    (catch :default e
                      (js/console.warn "[PE-BRANCH-PROJ] branch-as-db failed for"
                                       (str db-scope) active-branch e))))))
          _pe-run (js/console.log "[PE-RUN] page-uuid=" (str page-uuid)
                                  "db-tx=" (when db-value (:max-tx db-value))
                                  "branch=" active-branch)]

      (if-not db-value
        (el/div {:class "block-editor loading" :key (str "page-view-" page-uuid)}
          (el/p "Loading database..."))

      (let [;; If we projected onto a branch, wrap the projected db-value
            ;; as a fresh interval so `query-with-deltas` runs the query
            ;; against the branch's view. On trunk we reuse `db-iv` as-is
            ;; (full delta-detection path).
            ;; Off-Now dbs (as-of / branch) are non-incremental — feed them
            ;; as a FRESH interval so query-with-deltas full-recomputes
            ;; (hash-keyed); only the live trunk path passes deltas through.
            ;; Construct the interval EXPLICITLY: as-interval's coercion
            ;; probes maps with `contains?`, and datahike's CLJS AsOfDB
            ;; throws on -contains-key? — the throw rejected this spin and
            ;; the reject cascaded silently to the root (frozen DOM, one
            ;; :render/error). Same shape the chat path uses.
            blocks-source-iv (if (and (not time-travel?) (identical? db-value trunk-db))
                               db-iv
                               (iv/->Interval nil db-value nil))

            ;; Query blocks with delta detection for incremental updates
            ;; The query cache is keyed by this id and shared globally —
            ;; the REF must be part of it, or Now and as-of runs clobber
            ;; each other's diff baseline (back-to-Now then renders the
            ;; past state until an unrelated db change).
            dh-blocks-iv (dq/query-with-deltas
                           blocks-source-iv
                           (make-page-blocks-query page-uuid)
                           :entity/uuid
                           [:page-blocks page-uuid db-scope ref])

            ;; Transform Datahike format to UI format
            focused-id (:focused-id @ui-state)
            blocks-iv (transform-datahike-blocks dh-blocks-iv focused-id)

            ;; Build indices for tree operations
            all-blocks (iv/get-new blocks-iv)
            ;; Update visible blocks order for arrow key navigation
            _ (when all-blocks (reset! visible-blocks-order all-blocks))

            block-index (when all-blocks (build-block-index all-blocks))
            children-index (when all-blocks (build-children-index all-blocks))

            ;; Check if this page is a type (for rendering instances)
            type-entity (dq/is-type-page? db-value page-uuid)
            type-name (when type-entity (:entity/name type-entity))
            ;; Get instances with their property values for table display
            instances-data (when type-name (dq/type-instances-with-properties-query db-value type-name))
            type-properties (when instances-data (:properties instances-data))
            instances (when instances-data (:instances instances-data))]

        ;; Use explicit :key with page-uuid so each page gets a unique DOM subtree
        (el/div {:class (str "block-editor" (when time-travel? " time-travel-readonly"))
                 :key (str "page-view-" page-uuid)
                 :data-page-uuid (str page-uuid)}
          (when time-travel?
            (el/div {:class "time-travel-banner"}
              (el/span {} "🕰 Viewing a past version — read-only")))
          ;; Page header with types and properties - await the spin
          (await (render-page-header page-uuid db-value db-scope time-travel?))

          ;; Instances table (only for type pages)
          (when (and type-entity (seq instances))
            (types/instances-box
              {:type-name type-name
               :properties type-properties
               :instances instances
               :on-instance-click (fn [instance]
                                    (when-let [inst-uuid (:entity/uuid instance)]
                                      (sig/open-tab! :wiki (cond-> {:page-uuid inst-uuid}
                                                             db-scope (assoc :db-scope db-scope))
                                                     {:title (or (:S.Page/title instance)
                                                                 (:entity/name instance)
                                                                 "Instance")})))}))

          ;; Blocks content
          (el/div {:class "blocks-container"}
            (if (seq all-blocks)
              (el/div {:class "blocks-list"}
                ;; Use ifor-each for incremental DOM updates - only changed blocks re-render
                (ifor-each :id blocks-iv
                  (fn [block]
                    (let [depth (block-depth block block-index)
                          has-children (has-children? (:id block) children-index)
                          ;; Resolve live conn for widget writes when this is a KB-scoped
                          ;; tab AND the active branch is the trunk. Optimistic widget
                          ;; writes go through opt/transact! on the trunk-bound conn;
                          ;; passing it on a fork would silently land widget writes on
                          ;; trunk. Widgets-on-forks need branch-aware writes (deferred).
                          conn (when (and db-scope (trunk? active-branch))
                                 (db-signal/get-kb-conn db-scope))]
                      (render-block block depth has-children db-scope syntax-pref db-value conn active-branch)))))
              ;; Empty state: new pages auto-create first block, so this is just loading
              (el/div {:class "empty-state"}
                (el/p "Loading blocks...")))
          (el/div {:class "editor-help"}
            (el/p {:class "help-text"}
              "Enter: new block | Backspace (empty): delete | Tab: indent | Shift+Tab: outdent")))))))))
