(ns is.simm.model.references
  "Utilities for parsing and managing block/page references.

   Supports two types of references:
   - Page references: [[Page Name]]
   - Block references: ((block-uuid))"
  (:require [clojure.string :as str]
            [datahike.api :as d]))

;; ============================================================================
;; Reference Patterns
;; ============================================================================

(def page-reference-pattern
  "Regex pattern for page references: [[Page Name]] or [[Page Name][Display Text]]"
  #"\[\[([^\]]+)\](?:\[([^\]]+)\])?\]")

(def block-reference-pattern
  "Regex pattern for block references: ((uuid))"
  #"\(\(([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})\)\)")

(def user-mention-pattern
  "Regex for @handle party-mentions. The leading `(?:^|[^\\w@])` requires a
   non-word char (or start) before the @, so email addresses (`a@b.com`) and
   `@@` do NOT match. Handles accept ASCII letters/digits plus every non-ASCII
   BMP character, so generated names such as `vár-*` and CJK handles work in
   both CLJ and CLJS without relying on JavaScript Unicode-property flags.
   Works on tiptap HTML and plain/markdown text; no lookbehind is used."
  #"(?:^|[^\w@])@([A-Za-z\u0080-\uFFFF][A-Za-z0-9_.\-\u0080-\uFFFF]*)")

(def user-mention-html-pattern
  "Like `user-mention-pattern` but the leading char is CAPTURED (group 1) and
   the class `[^\\w@>]` also excludes `>` — so an already-wrapped `>@handle`
   inside a `.user-reference` span is not re-wrapped. For string-level HTML
   linkification (`str/replace` with `$1…$2`), not the tokenizer."
  #"(^|[^\w@>])@([A-Za-z\u0080-\uFFFF][A-Za-z0-9_.\-\u0080-\uFFFF]*)")

(def ^:private combined-reference-pattern
  "One pattern matching a wiki link `[[Target]]` / `[[Target][Display]]`
   (group 1 = target, group 2 = optional display) OR `@handle` (group 3), used
   by `tokenize-references`. The target is a page TITLE (same-KB link) or a
   `dh://…` datahike reference URI (cross-database link). The @ branch optionally
   consumes ONE leading non-word char (the email/`@@` guard) which the tokenizer
   re-emits as text. No lookahead/lookbehind — identical on the JVM and every JS
   engine."
  #"\[\[([^\]]+)\](?:\[([^\]]+)\])?\]|(?:^|[^\w@])@([A-Za-z\u0080-\uFFFF][A-Za-z0-9_.\-\u0080-\uFFFF]*)")

(defn tokenize-references
  "Split `text` into an ordered vector of tokens for rendering links + mentions
   uniformly in chat and wiki. Tokens:
     {:type :text    :value s}
     {:type :page    :name title    :display s}   ; same-KB link (by title)
     {:type :ref     :uri dh://…    :display s}   ; cross-database link
     {:type :mention :handle s}
   Email-safe and JVM+JS regex-safe. Non-strings pass through as a single
   :text token."
  [text]
  (if-not (string? text)
    [{:type :text :value (str text)}]
    (loop [s text, out []]
      (if (empty? s)
        out
        (if-let [m (re-find combined-reference-pattern s)]
          (let [[whole target display handle] m
                idx (str/index-of s whole)
                ;; For the @ branch `whole` may start with a captured leading
                ;; non-word char (the email guard); keep it as text.
                lead (if (and handle (not (str/starts-with? whole "@")))
                       (subs whole 0 1) "")
                before (str (subs s 0 idx) lead)
                after (subs s (+ idx (count whole)))
                out (cond-> out
                      (seq before) (conj {:type :text :value before}))
                out (cond
                      target (conj out
                                   (if (str/starts-with? target "dh://")
                                     {:type :ref  :uri target  :display (or display target)}
                                     {:type :page :name target :display (or display target)}))
                      handle (conj out {:type :mention :handle handle})
                      :else  out)]
            (recur after out))
          (conj out {:type :text :value s}))))))

;; ============================================================================
;; Reference Extraction
;; ============================================================================

(defn extract-page-references
  "Extract all page reference names from text content.
   Returns a set of page names."
  [content]
  (when content
    (->> (re-seq page-reference-pattern content)
         (map second)
         (into #{}))))

(defn extract-block-references
  "Extract all block reference UUIDs from text content.
   Returns a set of UUID strings."
  [content]
  (when content
    (->> (re-seq block-reference-pattern content)
         (map second)
         (into #{}))))

(defn extract-user-mentions
  "Extract @handle party-mentions from content (tiptap HTML or plain/markdown).
   Returns a set of handle strings — resolved to parties at notify time (see
   doc/archive/mentions-notifications-contacts-design.md), mirroring how page mentions
   store `[[Title]]` strings rather than cross-DB refs."
  [content]
  (when content
    (->> (re-seq user-mention-pattern (str content))
         (map second)
         (remove str/blank?)
         (into #{}))))

(defn extract-all-references
  "Extract all references (both page and block) from text content.
   Returns a map with :pages (set of names) and :blocks (set of UUID strings)."
  [content]
  {:pages (extract-page-references content)
   :blocks (extract-block-references content)})

;; ============================================================================
;; Reference Resolution
;; ============================================================================

(defn resolve-page-reference
  "Find a page entity by its title.
   Returns the page entity or nil if not found.

   Cross-platform: datahike.api/q has the same shape in CLJ and CLJS,
   so this works on the frontend's local KB db too — letting
   `update-block-content!` compute refs in the optimistic transact
   instead of waiting for the server to extract them."
  [db page-title]
  (when page-title
    (d/q '[:find (pull ?e [:entity/uuid]) .
           :in $ ?title
           :where
           [?e :S.Page/title ?title]]
         db page-title)))

(defn resolve-block-reference
  "Find a block entity by its UUID string.
   Returns the block entity or nil if not found."
  [db block-uuid-str]
  (when block-uuid-str
    (try
      (let [uuid #?(:clj (java.util.UUID/fromString block-uuid-str)
                    :cljs (cljs.core/uuid block-uuid-str))]
        (d/q '[:find (pull ?e [:entity/uuid]) .
               :in $ ?uuid
               :where
               [?e :entity/uuid ?uuid]]
             db uuid))
      (catch #?(:clj Exception :cljs :default) _
        nil))))

(defn resolve-references
  "Resolve all references in content to entity lookup refs.
   Returns a vector of `[:entity/uuid uuid]` lookup refs for every
   reference whose target exists in `db`. Targets that don't yet
   exist locally are dropped silently — they'll get filled in by the
   server's authoritative extraction when sync delivers, and by a
   retroactive pass when the missing page is later created (see
   `is.simm.model.crud/backfill-references-for-page!`)."
  [db content]
  (let [{:keys [pages blocks]} (extract-all-references content)
        page-entities (keep #(resolve-page-reference db %) pages)
        block-entities (keep #(resolve-block-reference db %) blocks)
        all-uuids (concat (map :entity/uuid page-entities)
                          (map :entity/uuid block-entities))]
    (mapv (fn [uuid] [:entity/uuid uuid]) all-uuids)))

(defn backfill-tx-for-page
  "Build a transaction that adds the new page as a :block/references
   target on every block whose content already contains `[[title]]`.

   This closes the timing gap: when a user types `[[Bar]]` BEFORE the
   Bar page exists, `resolve-references` returns nil for that ref and
   the block lands with no :block/references. Bar only gets created
   later (e.g. on link-click). Without backfill the block's refs stay
   empty forever even though the data model now CAN resolve the link.

   Returns a vector of `{:entity/uuid block-uuid :block/references
   [[:entity/uuid page-uuid]]}` maps — datahike merges cardinality-many
   refs on transact, so an already-referencing block is a no-op. Pass
   the returned vector to `d/transact` / `opt/transact!` directly.

   Empty vector when no blocks contain `[[title]]`."
  [db page-uuid page-title]
  (when (and page-uuid page-title)
    (let [marker (str "[[" page-title "]]")
          ;; Find blocks that mention the page by literal `[[title]]`.
          ;; A query-time `clojure.string/includes?` predicate would be
          ;; cleaner but is not supported uniformly across the CLJ/CLJS
          ;; datahike query engines we currently ride, so we pull all
          ;; block contents and filter in Clojure. The block table is
          ;; small enough that this is fine; if it grows we can switch
          ;; to a denormalized index.
          all-blocks (d/q '[:find ?e ?content
                            :where [?e :block/content ?content]]
                          db)
          target-ref [:entity/uuid page-uuid]]
      (vec
       (for [[eid content] all-blocks
             :when (and (string? content)
                        (str/includes? content marker))]
         {:db/id eid
          :block/references [target-ref]})))))

;; ============================================================================
;; Reference Formatting
;; ============================================================================

(defn format-page-reference
  "Format a page name as a reference: [[Page Name]]"
  [page-name]
  (str "[[" page-name "]]"))

(defn format-block-reference
  "Format a block UUID as a reference: ((uuid))"
  [block-uuid]
  (str "((" block-uuid "))"))

(defn rename-page-references
  "Rewrite `[[old-name]]` / `[[old-name][Display]]` in `content` to use
   `new-name`, preserving any display text. Other references are untouched.

   `:block/references` is a ref datom to the target entity, so a rename does not
   invalidate the link — but the literal text a user reads would still say the
   old title. Callers rewrite the referring blocks' content with this."
  [content old-name new-name]
  (when content
    (str/replace content
                 page-reference-pattern
                 (fn [[match page-name display-text]]
                   (cond
                     (not= page-name old-name) match
                     display-text (str "[[" new-name "][" display-text "]]")
                     :else (str "[[" new-name "]]"))))))

;; ============================================================================
;; HTML Rendering Helpers
;; ============================================================================

(defn replace-page-references
  "Replace [[Page Name]] or [[Page Name][Display Text]] with HTML links in content."
  [content]
  (when content
    (str/replace content
                 page-reference-pattern
                 (fn [[match page-name display-text]]
                   (let [display (or display-text page-name)]
                     (str "<span class=\"page-reference\" data-page-name=\"" page-name "\" data-display-text=\"" display "\">[[" display "]]</span>"))))))

#?(:clj
   (defn get-block-preview
     "Get a preview of block content (first ~50 chars, stripped of HTML tags)"
     [db block-uuid-str]
     (when block-uuid-str
       (try
         (let [uuid (java.util.UUID/fromString block-uuid-str)
               block (d/q '[:find (pull ?e [:block/content]) .
                           :in $ ?uuid
                           :where
                           [?e :entity/uuid ?uuid]]
                         db uuid)]
           (when-let [content (:block/content block)]
             (let [;; Strip HTML tags
                   plain-text (str/replace content #"<[^>]*>" "")
                   ;; Trim whitespace
                   trimmed (str/trim plain-text)
                   ;; Take first 50 chars
                   preview (if (> (count trimmed) 50)
                            (str (subs trimmed 0 47) "...")
                            trimmed)]
               (if (empty? preview) "Empty block" preview))))
         (catch Exception _
           nil)))))

(defn replace-block-references
  "Replace ((uuid)) with HTML links in content.
   If db is provided, shows block preview instead of UUID.
   If db is nil, shows UUID (for client-side rendering)."
  ([content] (replace-block-references content nil))
  ([content db]
   (when content
     (str/replace content
                  block-reference-pattern
                  (fn [[_ uuid]]
                    (let [display #?(:clj (if db
                                           (or (get-block-preview db uuid) uuid)
                                           uuid)
                                    :cljs uuid)]
                      (str "<span class=\"block-reference\" data-block-uuid=\"" uuid "\">(" display ")</span>")))))))

(defn render-references
  "Replace all references with HTML links in content.
   If db is provided, block references show content preview instead of UUID."
  ([content] (render-references content nil))
  ([content db]
   (-> content
       replace-page-references
       (replace-block-references db))))

;; =============================================================================
;; HTML → prose
;; =============================================================================

(def ^:private html-entities
  ;; The five XML predefined entities plus nbsp. An excerpt showing
  ;; "productivity &amp; notes" is a bug people notice immediately.
  {"&amp;" "&" "&lt;" "<" "&gt;" ">" "&quot;" "\"" "&#39;" "'" "&apos;" "'"
   "&nbsp;" " "})

(defn strip-html
  "Block and message content is HTML; an excerpt, a summary or a prompt is prose.

   ONE implementation. There were four — `ops/feed`, `ops/diff_summary`,
   `agents/room_agents` and `agents/merger` — plus two raw inline
   `#\"<[^>]+>\"` replacements, and only the feed's decoded entities at all. The
   others rendered `&amp;` and `&nbsp;` literally into agent prompts and LLM
   summaries."
  [s]
  (when s
    (-> (reduce (fn [t [e c]] (str/replace t e c))
                (str/replace (str s) #"<[^>]*>" " ")
                html-entities)
        (str/replace #"\s+" " ")
        str/trim)))
