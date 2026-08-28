(ns is.simm.uis.web.desktop.views.chat
  "The message-rendering half of the chat view.

   This namespace once held a whole chat panel — its own message list, input
   and fork-controls bar. `views/columns` reimplemented all of that (with the
   tiptap input and the optimistic overlay) and reuses the same CSS classes;
   the originals sat here unreferenced until they were removed. What is left
   is what columns actually calls, which is the rendering of one entry:

   Components:
   - message-view: Single message display
   - eval-entry-view / eval-run-view: sandbox runs
   - kb-event-view: a KB change as a chat entry
   - fork-indicator: Shows current fork type with badge
   - chat-header: room title and controls

   Fork Types:
   - :thread - Conversation thread (shared DB, isolated messages)
   - :exploration - AI speculation (speculative DB snapshot)
   - :branch - Durable branch (uses Datahike versioning)

   Supports TIER 3 correlation highlighting:
   - Page references highlight matching elements across UI
   - User mentions highlight related user elements
   - Uses data-* attributes for DOM-based highlighting"
  (:require [org.replikativ.spindel.dom.elements :as el]
            [org.replikativ.spindel.dom.foreign]
            [is.simm.uis.web.desktop.views.core :as vc]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.model.references :as refs]
            [clojure.string :as str]
            #?(:cljs [is.simm.uis.web.desktop.markdown :as md])
            #?(:cljs [is.simm.uis.web.desktop.views.mermaid :as mmd])
            #?(:cljs [superficie.api :refer [toSup]]))
  #?(:cljs (:require-macros [org.replikativ.spindel.dom.elements :as el]
                            [org.replikativ.spindel.dom.foreign :refer [foreign-node]])))

;; =============================================================================
;; Text Parsing - Convert [[page]] and @user to Rich Content
;; =============================================================================

(defn parse-text-references
  "Parse plain text to extract [[page]] and @user references.
   Returns a vector of content items for render-rich-content.

   Example:
   \"Check out [[Getting Started]] and ask @alice-chen\"
   => [{:type :text :value \"Check out \"}
       {:type :page-ref :page \"Getting Started\" :text \"Getting Started\"}
       {:type :text :value \" and ask \"}
       {:type :mention :user \"alice-chen\" :text \"alice-chen\"}]"
  [text]
  (if (string? text)
    ;; One shared tokenizer (refs/tokenize-references) drives BOTH this and the
    ;; markdown post-processor (linkify-page-refs!), so `[[page]]`/`@handle`
    ;; detection can never drift between the two chat render paths — nor from the
    ;; notify/extract path in references.cljc.
    (mapv (fn [{:keys [type value name display uri handle]}]
            (case type
              :text    {:type :text :value value}
              :page    {:type :page-ref :page name :text (or display name)}
              :ref     {:type :dh-ref  :uri uri :text (or display uri)}
              :mention {:type :mention :user handle :text handle}))
          (refs/tokenize-references text))
    ;; Not a string, return as-is
    text))

;; =============================================================================
;; Rich Content Rendering
;; =============================================================================

#?(:cljs
   (defn linkify-page-refs!
     "Post-process a rendered markdown container: wrap [[Page]] references in
      .page-ref spans and @handle mentions in .mention spans (the same shapes
      render-rich-content produces for human messages) so the app-level click
      handler resolves pages against the room's KBs and mentions to a profile.
      Skips text inside code/pre. Uses the shared refs/tokenize-references, which
      is email-safe (`a@b.com`, `@@` excluded) and JS+JVM-consistent — no more
      Chromium-only lookbehind."
     [container]
     (let [walker (.createTreeWalker js/document container
                                     js/NodeFilter.SHOW_TEXT)
           nodes (loop [acc []]
                   (if-let [n (.nextNode walker)]
                     (recur (conj acc n))
                     acc))]
       (doseq [n nodes
               :let [text (.-textContent n)
                     tokens (refs/tokenize-references text)]
               :when (and (some #(not= :text (:type %)) tokens)
                          (not (.closest (.-parentElement n) "pre, code")))]
         (let [frag (.createDocumentFragment js/document)]
           (doseq [{:keys [type value name display uri handle]} tokens]
             (case type
               :text (.appendChild frag (.createTextNode js/document value))
               :page (let [span (.createElement js/document "span")]
                       (set! (.-className span) "page-ref")
                       (.setAttribute span "data-page" name)
                       (set! (.-textContent span) (str "[[" (or display name) "]]"))
                       (.appendChild frag span))
               ;; cross-database link: render as a bracketed [[Display]] link
               ;; (same shape as a same-KB ref) and carry the dh:// URI (which
               ;; already holds the target store + entity uuid). The raw pointer
               ;; is surfaced on hover via `title`.
               :ref (let [span (.createElement js/document "span")]
                      (set! (.-className span) "page-ref")
                      (.setAttribute span "data-ref" uri)
                      (.setAttribute span "title" uri)
                      (set! (.-textContent span) (str "[[" (or display uri) "]]"))
                      (.appendChild frag span))
               :mention (let [span (.createElement js/document "span")]
                          (set! (.-className span) "mention")
                          (.setAttribute span "data-user" handle)
                          (set! (.-textContent span) (str "@" handle))
                          (.appendChild frag span))))
           (.replaceWith n frag))))))

(defn render-rich-content
  "Render message content that may contain page refs and mentions.

   Content can be:
   - A plain string (auto-parsed for [[page]] and @user patterns)
   - A vector of mixed strings and content maps like:
     [{:type :text :value \"Have you seen the \"}
      {:type :page-ref :page \"api-design\" :text \"API Design Notes\"}
      {:type :text :value \"?\"}]"
  [content]
  (cond
    ;; Plain string - parse for references first
    (string? content)
    (let [parsed (parse-text-references content)]
      (if (and (= 1 (count parsed))
               (= :text (:type (first parsed))))
        ;; No references found, just return text
        content
        ;; Has references, render as rich content
        (render-rich-content parsed)))

    (vector? content)
    (map-indexed
      (fn [idx item]
        (cond
          (string? item)
          item

          (map? item)
          (case (:type item)
            :text (:value item)
            :page-ref (el/span {:key (str "ref-" idx)
                                :class "page-ref"
                                :data-page (:page item)}
                        (str "[[" (:text item) "]]"))
            :dh-ref (el/span {:key (str "dhref-" idx)
                              :class "page-ref"
                              :data-ref (:uri item)
                              :title (:uri item)}
                      (str "[[" (:text item) "]]"))
            :mention (el/span {:key (str "mention-" idx)
                               :class "mention"
                               :data-user (:user item)}
                       (str "@" (:text item)))
            ;; Default: just return the item
            (str item))

          :else (str item)))
      content)

    :else (str content)))

;; =============================================================================
;; Message Components
;; =============================================================================

(defn kb-event-view
  "Render a compact KB event chip in the chat timeline.

   Props:
   - :id          - Unique event ID
   - :event-type  - :page-created, :blocks-added, :block-removed, :page-updated
   - :title       - Page title affected
   - :block-count - Number of blocks (for :blocks-added)
   - :author-name - Who caused the event
   - :timestamp   - Display timestamp string"
  [{:keys [id event-type title block-count author-name timestamp]}]
  (let [icon (case event-type
               :page-created  "file-plus"
               :blocks-added  "file-text"
               :block-removed "file-minus"
               :page-updated  "file-edit"
               :chat-summary  "book-open"
               "file")
        label (case event-type
                :page-created  (str "Created page \"" title "\"")
                :blocks-added  (str "Added " block-count " block" (when (> block-count 1) "s")
                                    " to \"" title "\"")
                :block-removed (str "Removed block from \"" title "\"")
                :page-updated  (str "Updated \"" title "\"")
                :chat-summary  (str "\uD83D\uDCDD Summarized the conversation → \"" title "\"")
                (str "Changed \"" title "\""))]
    (el/div {:key id
             :class "kb-event-chip"}
      (vc/icon icon {:class "kb-event-icon"})
      (el/span {:class "kb-event-label"} label)
      (el/span {:class "kb-event-time"} (or timestamp "")))))

;; Above this size, highlighting a pane costs more than it communicates —
;; hljs on a 100KB PDF dump janks the timeline, and prose gets no benefit
;; from Clojure tokenization anyway. Render such panes as plain text.
(def ^:private max-highlight-chars 4000)

(def ^:private tool-icons
  {"clojure_eval" "terminal"
   "shell" "square-terminal"
   "screen_look" "monitor"
   "read_file" "file-text"
   "write_file" "file-plus"
   "edit_file" "file-pen"
   "clojure_edit" "file-pen"
   "knowledge_search" "search"
   "knowledge_add" "book-plus"})

(defn- humanize-size
  "Byte count as a short human string — the collapsed chip's cue for
   'is there anything in here worth opening?'"
  [n]
  (cond
    (nil? n) ""
    (< n 1000) (str n " B")
    (< n 1000000) (str (int (/ n 1000)) " KB")
    :else (str (/ (double (int (/ n 100000))) 10.0) " MB")))

(defn- first-meaningful-line
  "First non-blank line of s, ellipsized to n chars."
  [s n]
  (let [line (or (->> (str/split-lines (or s ""))
                      (remove str/blank?)
                      first)
                 "")
        line (str/trim line)]
    (if (> (count line) n)
      (str (subs line 0 (- n 1)) "…")
      line)))

#?(:cljs
   (defn- copy-button
     "Copy `text` to the clipboard, confirming in place."
     [text label]
     (el/button {:class "eval-pane-copy"
                 :title (str "Copy " label)
                 :on-click (fn [e]
                             (.preventDefault e)
                             (.stopPropagation e)
                             (let [btn (.-currentTarget e)]
                               (-> (js/navigator.clipboard.writeText (or text ""))
                                   (.then (fn []
                                            (set! (.-textContent btn) "copied")
                                            (js/setTimeout
                                              #(set! (.-textContent btn) "copy")
                                              1200))))))}
       "copy")))

#?(:cljs
   (defn- code-pane
     "A bounded, optionally highlighted monospace pane. Scrolls internally —
      a chip must never be able to blow the timeline apart, however much
      output a tool returned."
     [{:keys [key-str text lang class]}]
     (foreign-node
       {:key key-str
        :class class
        :on-mount (fn [container]
                    (let [text (or text "")
                          code-el (js/document.createElement "code")
                          highlight? (and lang (<= (count text) max-highlight-chars))]
                      (set! (.-className code-el)
                            (if highlight? (str "hljs language-" lang) "hljs"))
                      (if highlight?
                        (set! (.-innerHTML code-el)
                              (or (md/highlight-code text lang) text))
                        ;; textContent, not innerHTML — unhighlighted panes carry
                        ;; raw tool output, which must never parse as markup
                        (set! (.-textContent code-el) text))
                      (.appendChild container code-el)))
        :on-unmount (fn [container]
                      (when container
                        (set! (.-innerHTML container) "")))})))

(defn eval-entry-view
  "One tool call in the chat timeline: a collapsed chip that opens into
   its input and its result.

   Collapsed, it answers 'which tool, on what, did it work, is there
   output worth opening'. Open, it shows input and result as separate
   labeled panes — each bounded and independently scrollable, each
   copyable.

   Props (from S.EvalEntry timeline item):
   - :entity/uuid              - unique key
   - :S.EvalEntry/tool         - tool name (clojure_eval, shell, …)
   - :S.EvalEntry/code         - code evaluated, or the tool's input
   - :S.EvalEntry/result       - result string (possibly truncated)
   - :S.EvalEntry/success?     - whether the call succeeded
   - :S.EvalEntry/agent-name   - display name of the agent
   - :S.EvalEntry/evaluated-at - timestamp"
  [{:keys [entity/uuid
           S.EvalEntry/tool
           S.EvalEntry/code
           S.EvalEntry/result
           S.EvalEntry/success?
           S.EvalEntry/agent-name
           S.EvalEntry/evaluated-at
           syntax-pref]}]
  (let [tool (or tool "clojure_eval")
        eval? (= tool "clojure_eval")
        preview (first-meaningful-line code 72)
        result-size (count (or result ""))
        timestamp-str #?(:cljs (when evaluated-at
                                 (.toLocaleTimeString evaluated-at))
                         :clj (str evaluated-at))
        pref-key (name (or syntax-pref :clojure))
        ;; Only the eval tool returns Clojure values; shell/vision/file tools
        ;; return prose, and tokenizing that as code just adds noise.
        result-lang (when eval? (if (= syntax-pref :superficie) "superficie" "clojure"))
        code-lang (if eval?
                    (if (= syntax-pref :superficie) "superficie" "clojure")
                    "clojure")] ; non-eval input is a pretty-printed EDN map
    (el/details {:key (str uuid)
                 :class (vc/class-names "eval-entry-chip"
                                        (if success?
                                          "eval-entry-chip--success"
                                          "eval-entry-chip--error"))}
      (el/summary {:class "eval-entry-summary"}
        (vc/icon (if success?
                   (get tool-icons tool "wrench")
                   "alert-circle")
                 {:class "eval-entry-icon"})
        (el/span {:class "eval-entry-tool"} tool)
        (el/span {:class "eval-entry-preview"} preview)
        (el/span {:class "eval-entry-size"}
          (if (pos? result-size) (humanize-size result-size) ""))
        (el/span {:class "eval-entry-agent"} (or agent-name "Agent"))
        (el/span {:class "eval-entry-time"} (or timestamp-str "")))
      (el/div {:class "eval-entry-body"}
        (el/div {:class "eval-pane"}
          (el/div {:class "eval-pane-head"}
            (el/span {:class "eval-pane-label"} (if eval? "code" "input"))
            #?(:cljs (copy-button code "code") :clj nil))
          #?(:cljs
             (code-pane {:key-str (str uuid "-code-" pref-key)
                         :text (let [raw (or code "")]
                                 (if (and eval? (= syntax-pref :superficie))
                                   (try (toSup raw) (catch :default _ raw))
                                   raw))
                         :lang code-lang
                         :class "eval-entry-code"})
             :clj
             (el/pre {:class "eval-entry-code"} (or code ""))))
        (el/div {:class "eval-pane"}
          (el/div {:class "eval-pane-head"}
            (el/span {:class "eval-pane-label"}
              (if success? "result" "error"))
            (el/span {:class "eval-pane-meta"}
              (if (pos? result-size) (humanize-size result-size) "empty"))
            #?(:cljs (copy-button result "result") :clj nil))
          #?(:cljs
             (code-pane {:key-str (str uuid "-result-" pref-key)
                         :text (let [raw (or result "")]
                                 (if (and result-lang (= syntax-pref :superficie))
                                   (try (toSup raw) (catch :default _ raw))
                                   raw))
                         :lang (when success? result-lang)
                         :class (vc/class-names "eval-entry-result"
                                                (if success?
                                                  "eval-entry-result--success"
                                                  "eval-entry-result--error"))})
             :clj
             (el/pre {:class (vc/class-names "eval-entry-result"
                                             (if success?
                                               "eval-entry-result--success"
                                               "eval-entry-result--error"))}
               (or result ""))))))))

(defn message-view
  "Render a single chat message.

   Props:
   - :id - Unique message ID for keying
   - :author-id - User ID for highlighting (e.g. \"bob\", \"alice\")
   - :author-name - Display name of author
   - :content - Message text (string or rich content vector)
   - :timestamp - Message timestamp string (e.g. \"2:34 PM\")
   - :is-own? - Whether this is the current user's message
   - :is-ai? - Whether this is an AI assistant message
   - :thread-parent - Bounded direct-parent preview, or nil when not loaded
   - :in-reply-to - Stable direct-parent UUID
   - :reply-count - Known descendant count when this message is a thread root
   - :on-jump-parent, :on-reply, :on-open-thread - Optional interactions

   Supports TIER 3 correlation highlighting via data-* attributes:
   - data-author on message container
   - data-user on author name

   Note: Always renders all elements to avoid spindel delta rendering issues.
  CSS handles visibility based on .chat-message--own class."
  [{:keys [id author-id author-name content reasoning timestamp is-own? is-ai?
           syntax-pref attachment-blob attachment-mime thread-parent
           in-reply-to reply-count on-jump-parent on-reply audience
           mention-handles on-open-thread]}]
  (el/div {:key id
           :class (vc/class-names "chat-message" "message"
                                  (when is-own? "chat-message--own"))
           :data-author author-id
           :data-message-id id}
    ;; Always render avatar (CSS hides for own messages)
    (el/div {:class (vc/class-names "chat-avatar" "message-avatar"
                                    (when is-ai? "ai"))}
      (if is-ai?
        "AI"
        (->> (str/split (or author-name "?") #"\s+")
             (take 2)
             (map first)
             (apply str)
             str/upper-case)))
    (el/div {:class "chat-message-bubble message-body"}
      ;; A reply remains in chronological room order, but carries enough local
      ;; context to read the causal conversation without opening another view.
      ;; Missing parents are explicit: bounded/async clients must not turn an
      ;; unloaded ancestor into a fake top-level message.
      (when in-reply-to
        (el/button {:class "chat-thread-parent"
                    :title "Go to the message this replies to"
                    :on-click (fn [event]
                                (.stopPropagation event)
                                (when on-jump-parent (on-jump-parent)))}
          (vc/icon "reply" {:class "chat-thread-parent-icon"})
          (el/span {:class "chat-thread-parent-author"}
            (or (:author-name thread-parent) "Earlier message"))
          (el/span {:class "chat-thread-parent-preview"}
            (or (:content thread-parent) "Load parent to view context"))))
      ;; Message header with author and time
      (el/div {:class "message-header"}
        ;; Always render author (CSS hides for own messages)
        (el/span {:class "chat-message-author message-author"
                  :data-user author-id}
          author-name)
        ;; Always render time container
        (el/span {:class "chat-message-time message-time"}
          (or timestamp ""))
        (when (pos? (or reply-count 0))
          (el/button {:class "chat-thread-count"
                      :title "Open focused thread"
                      :on-click (fn [event]
                                  (.stopPropagation event)
                                  (when on-open-thread
                                    (on-open-thread event)))}
            (str reply-count (if (= reply-count 1) " reply" " replies"))))
        (when on-reply
          (el/button {:class "chat-message-reply"
                      :title "Reply in thread"
                      :on-click (fn [event]
                                  (.stopPropagation event)
                                  (on-reply))}
            "Reply")))
      ;; The visible @handle is presentation; this chip confirms that dispatch
      ;; resolved it to canonical actor identities before the message was
      ;; accepted. Keep the ids in the title for inspection without making the
      ;; ordinary conversation read like a database trace.
      (when (seq audience)
        (let [handles (sort (map str (or mention-handles [])))
              actor-ids (sort (map str audience))]
          (el/div {:class "message-audience"
                   :title (str "Resolved audience: " (str/join ", " actor-ids))}
            (vc/icon "users" {:class "message-audience-icon"})
            (el/span {}
              (str "To "
                   (if (seq handles)
                     (str/join ", " (map #(str "@" %) handles))
                      (str (count actor-ids) " participant"
                           (when (not= 1 (count actor-ids)) "s"))))))))
      ;; Collapsed agent reasoning (<think> content), when present.
      ;; Reuses the eval-entry chip idiom so it reads as "process detail" —
      ;; but reasoning is PROSE: it renders as markdown in a reading face,
      ;; not as monospace, and the summary previews its opening words so a
      ;; closed chip still says what the agent was thinking about.
      (when (and reasoning (seq reasoning))
        (el/details {:key (str id "-thinking")
                     :class "eval-entry-chip thinking-chip"}
          (el/summary {:class "eval-entry-summary"}
            (vc/icon "brain" {:class "eval-entry-icon"})
            (el/span {:class "eval-entry-tool"} "thinking")
            (el/span {:class "eval-entry-preview thinking-preview"}
              (first-meaningful-line reasoning 90))
            (el/span {:class "eval-entry-size"} (humanize-size (count reasoning)))
            (el/span {:class "eval-entry-agent"} (or author-name "Agent")))
          (el/div {:class "eval-entry-body"}
            #?(:cljs
               (foreign-node
                 {:key (str id "-thinking-text")
                  :class "thinking-text"
                  :on-mount (fn [container]
                              (set! (.-innerHTML container)
                                    (md/render-markdown (or reasoning ""))))
                  :on-unmount (fn [container]
                                (when container
                                  (set! (.-innerHTML container) "")))})
               :clj
               (el/div {:class "thinking-text"} reasoning)))))
      ;; Attachment (e.g. original voice-note audio) — served from the
      ;; content-addressed blob store.
      #?(:cljs
         (when (and attachment-blob attachment-mime
                    (clojure.string/starts-with? attachment-mime "audio/"))
           (el/div {:class "chat-attachment-audio"}
             (el/audio {:controls true
                        :preload "metadata"
                        :src (str "/blobs/" attachment-blob)})))
         :clj nil)
      ;; Message content - markdown for AI, rich text for user
      #?(:cljs
         (if is-ai?
           (foreign-node
             {:key (str "md-" id "-" (name (or syntax-pref :clojure)))
              :class "chat-message-content message-text"
              :on-mount (fn [container]
                          (let [md-el (md/create-markdown-element (str content))]
                            (.appendChild container md-el)
                            ;; [[Page]] refs in agent markdown → clickable
                            ;; .page-ref spans (human messages get this via
                            ;; render-rich-content already)
                            (linkify-page-refs! md-el)
                            ;; Post-process: convert Clojure code blocks to Superficie if pref is set
                            (when (= syntax-pref :superficie)
                              (doseq [code-el (array-seq
                                               (.querySelectorAll container "code.language-clojure"))]
                                (let [clj-text (.-textContent code-el)
                                      sup-text (try (toSup clj-text)
                                                    (catch :default _ clj-text))]
                                  (set! (.-className code-el) "hljs language-superficie")
                                  (set! (.-innerHTML code-el)
                                        (or (md/highlight-code sup-text "superficie") sup-text)))))
                            ;; Render ```vega-lite fences as inline charts
                            (when (exists? js/vegaEmbed)
                              (doseq [code-el (array-seq
                                               (.querySelectorAll container
                                                                   "code.language-vega-lite"))]
                                (let [pre-el (.-parentElement code-el)
                                      spec-text (.-textContent code-el)
                                      chart-div (.createElement js/document "div")]
                                  (set! (.-className chart-div) "chat-vega-chart")
                                  (.replaceWith pre-el chart-div)
                                  (-> (js/vegaEmbed chart-div
                                        (js/JSON.parse spec-text)
                                        (clj->js {:actions false
                                                  :renderer "svg"
                                                  :theme "quartz"}))
                                      (.catch (fn [e]
                                                (set! (.-textContent chart-div)
                                                      (str "Chart error: " (.-message e)))))))))
                            ;; Render ```mermaid fences as inline SVG diagrams —
                            ;; the agent can post a diagram (workflow, chat/doc
                            ;; overview, domain map) straight into chat.
                            (doseq [code-el (array-seq
                                             (.querySelectorAll container "code.language-mermaid"))]
                              (let [pre-el (.-parentElement code-el)
                                    code   (.-textContent code-el)
                                    holder (.createElement js/document "div")]
                                (set! (.-className holder) "mermaid-diagram")
                                (.replaceWith pre-el holder)
                                (mmd/render-into! holder code)))))
              :on-unmount (fn [container]
                            (when container
                              (set! (.-innerHTML container) "")))})
           (el/div {:class "chat-message-content message-text"}
             (render-rich-content content)))
         :clj
         (el/div {:class "chat-message-content message-text"}
           (render-rich-content content))))))

;; =============================================================================
;; Tool runs: the agent's work between two things it SAID
;; =============================================================================

(defn msg-timestamp
  "A message's time, carrying its DATE once the message is not from today.

   The label used to be `.toLocaleTimeString` alone, so a conversation from six
   days ago read as `1:12 AM` — indistinguishable from this morning. That is a
   small bug in most products and a self-inflicted one here: the pitch is that
   every state is addressable in time, and the first screen a viewer sees was
   quietly asserting that all of it happened moments ago.

   Today keeps the bare time, because repeating today's date on every line is
   noise. Yesterday is named rather than dated — people read `Yesterday` faster
   than they parse a number. Anything older gets day and month, and a year only
   when it is not the current one, so a two-year-old message cannot masquerade
   as a recent one."
  [ts]
  #?(:cljs
     (when ts
       (let [now (js/Date.)
             same-day? (fn [a b] (and (= (.getFullYear a) (.getFullYear b))
                                      (= (.getMonth a) (.getMonth b))
                                      (= (.getDate a) (.getDate b))))
             yday (js/Date. (- (.getTime now) 86400000))
             hm (.toLocaleTimeString ts "en-US" #js {:hour "numeric" :minute "2-digit"})]
         (cond
           (same-day? ts now) hm
           (same-day? ts yday) (str "Yesterday " hm)
           (= (.getFullYear ts) (.getFullYear now))
           (str (.toLocaleDateString ts "en-US" #js {:day "numeric" :month "short"}) ", " hm)
           :else
           (str (.toLocaleDateString ts "en-US" #js {:day "numeric" :month "short" :year "numeric"})
                ", " hm))))
     :clj (str ts)))

(defn eval-run-view
  "A run of tool calls as a single row of dots — one dot per call, red where
   it failed. Opens into the chips; each chip opens into its detail."
  [{:keys [entity/uuid items syntax-pref]}]
  (let [n (count items)
        failed (count (remove :S.EvalEntry/success? items))
        agent-name (some :S.EvalEntry/agent-name items)
        last-ts (:S.EvalEntry/evaluated-at (last items))
        time-str #?(:cljs (when last-ts (.toLocaleTimeString last-ts))
                    :clj (str last-ts))]
    (el/details {:key (str "run-" uuid)
                 :class "eval-run"}
      (el/summary {:class "eval-run-summary"}
        (el/span {:class "eval-run-dots"}
          (map-indexed
            (fn [i {:keys [S.EvalEntry/tool S.EvalEntry/success?] :as _it}]
              (el/span {:key (str uuid "-dot-" i)
                        :class (vc/class-names "eval-dot"
                                               (when-not success? "eval-dot--error"))
                        :title (str tool (when-not success? " — failed"))}))
            items))
        (el/span {:class "eval-run-meta"}
          (str n " step" (when (not= n 1) "s")
               (when (pos? failed) (str ", " failed " failed"))))
        (el/span {:class "eval-run-agent"} (or agent-name "Agent"))
        (el/span {:class "eval-run-time"} (or time-str "")))
      (el/div {:class "eval-run-body"}
        (map #(eval-entry-view (assoc % :syntax-pref syntax-pref)) items)))))

;; RAF-based scroll throttling to prevent excessive re-renders
#?(:cljs (defonce ^:private scroll-raf-id (atom nil)))

;; =============================================================================
;; Chat Input
;; =============================================================================

;; =============================================================================
;; Fork Indicator and Controls
;; =============================================================================

(defn fork-type-icon
  "Get icon name for fork type."
  [fork-type]
  (case fork-type
    :thread "git-branch"
    :exploration "flask-conical"
    :branch "git-commit"
    "git-branch"))

(defn fork-type-label
  "Get human-readable label for fork type."
  [fork-type]
  (case fork-type
    :thread "Thread"
    :exploration "Exploration"
    :branch "Branch"
    "Thread"))

(defn fork-indicator
  "Render fork type badge with icon.

   Props:
   - :fork - Fork info map {:type :thread|:exploration|:branch :name string}
   - :on-click - Optional click handler to show fork menu"
  [{:keys [fork on-click]}]
  (let [fork-type (or (:type fork) :thread)
        fork-name (or (:name fork) "Main")]
    (el/div {:class (vc/class-names "fork-indicator"
                                    (str "fork-indicator--" (name fork-type)))
             :on-click on-click
             :title (str (fork-type-label fork-type) ": " fork-name)}
      (vc/icon (fork-type-icon fork-type) {:class "fork-indicator-icon"})
      (el/span {:class "fork-indicator-name"} fork-name)
      (el/span {:class "fork-indicator-badge"} (fork-type-label fork-type)))))

;; =============================================================================
;; Chat Panel
;; =============================================================================

(defn chat-header
  "Render the chat panel header with fork indicator.

   Props:
   - :chat-name - Name/title of the chat
   - :fork - Current fork info (optional)
   - :on-close - Called to close the panel (optional)"
  [{:keys [chat-name fork on-close]}]
  (el/div {:class "chat-header"}
    (el/div {:class "chat-header-info"}
      (vc/icon :message-circle {:class "chat-header-icon"})
      (el/span {:class "chat-header-title"} (or chat-name "Chat"))
      ;; Fork indicator (if fork info provided)
      (when fork
        (fork-indicator {:fork fork})))
    (when on-close
      (vc/icon-button :x {:class "chat-close-btn"
                          :on-click on-close
                          :title "Close chat"}))))

;; =============================================================================
;; Demo/Test Data (matches a-tiered-color.html prototype)
;; =============================================================================
