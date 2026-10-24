(ns is.simm.documents.markdown
  "Lossless Markdown source and a disposable, source-positioned syntax view.

   Offsets are UTF-16 (the browser editor's coordinates), not byte offsets.
   Parsing never serializes the AST back to Markdown or executes code/HTML.
   This is not the legacy HTML-block migration or the programmable-schema codec."
  (:require [clj-yaml.core :as yaml]
            [is.simm.documents.math :as math]
            [hasch.core :as hasch])
  (:import [com.vladsch.flexmark.parser Parser]
           [com.vladsch.flexmark.ast Heading Text HtmlEntity Code FencedCodeBlock IndentedCodeBlock Link LinkRef Reference OrderedList]
           [com.vladsch.flexmark.util.sequence Escaping]
           [com.vladsch.flexmark.ext.tables TablesExtension TableCell]
           [com.vladsch.flexmark.ext.gfm.tasklist TaskListExtension TaskListItem]
           [com.vladsch.flexmark.ext.gfm.strikethrough StrikethroughExtension]
           [com.vladsch.flexmark.ext.footnotes FootnoteExtension Footnote]
           [com.vladsch.flexmark.ext.wikilink WikiLinkExtension WikiNode WikiImage]
           [com.vladsch.flexmark.util.ast Node]
           [java.nio ByteBuffer CharBuffer]
           [java.nio.charset CodingErrorAction StandardCharsets]))

(def max-source-bytes 2097152)
(def max-syntax-nodes 20000)
(def max-syntax-depth 100)

(def syntax-profile
  "Pinned syntax projection, not a claim of complete Obsidian/plugin support."
  {:id :simmis/markdown-syntax :version 8 :parser "flexmark/0.64.8"
   :features #{:tables :tasks :strikethrough :footnotes :wikilinks :wiki-embeds
               :inline-math :standalone-display-math :reference-links}})

(defn- parser []
  (-> (Parser/builder)
      (.extensions [(TablesExtension/create) (TaskListExtension/create)
                    (StrikethroughExtension/create) (FootnoteExtension/create)
                    (WikiLinkExtension/create) (math/extension)])
      (.set WikiLinkExtension/LINK_FIRST_SYNTAX true)
      (.set WikiLinkExtension/ALLOW_ANCHORS true)
      (.set WikiLinkExtension/IMAGE_LINKS true)
      (.build)))

(defn- invalid! [message]
  (throw (ex-info message {:type ::invalid})))

(defn source-bytes
  "Strict UTF-8; refuse unpaired surrogates rather than silently replacing them."
  [source]
  (when-not (string? source) (invalid! "Document source must be text"))
  (when (> (count source) max-source-bytes) (invalid! "Document is too large"))
  (let [buffer (.encode (doto (.newEncoder StandardCharsets/UTF_8)
                          (.onMalformedInput CodingErrorAction/REPORT)
                          (.onUnmappableCharacter CodingErrorAction/REPORT))
                        (CharBuffer/wrap ^String source))
        bytes (byte-array (.remaining buffer))]
    (when (> (alength bytes) max-source-bytes) (invalid! "Document is too large"))
    (.get buffer bytes)
    bytes))

(defn decode
  "Decode a bounded UTF-8 document without changing its BOM or newlines."
  [^bytes bytes]
  (when (> (alength bytes) max-source-bytes) (invalid! "Document is too large"))
  (str (.decode (doto (.newDecoder StandardCharsets/UTF_8)
                  (.onMalformedInput CodingErrorAction/REPORT)
                  (.onUnmappableCharacter CodingErrorAction/REPORT))
                (ByteBuffer/wrap bytes))))

(defn revision [source] (hasch/uuid (source-bytes source)))

(defn yaml-options
  "One safe YAML policy for semantic parsing and source-span composition."
  []
  {:keywords false :unsafe false :allow-duplicate-keys false
   :allow-recursive-keys false :max-aliases-for-collections 0
   :nesting-depth-limit 30 :code-point-limit max-source-bytes})

(defn- frontmatter [source]
  (when-let [opening (re-find #"\A\uFEFF?---[ \t]*(?:\r\n|\n|\r)" source)]
    (let [start (count opening)
          matcher (re-matcher #"(?m)^---[ \t]*(?:\r\n|\n|\r|\z)" source)]
      (.region matcher start (count source))
      (if (.find matcher)
        (let [end (.start matcher)
              raw (subs source start end)]
          (merge {:from 0 :to (.end matcher) :value-from start :value-to end :raw raw}
                 (try
                   (let [value (apply yaml/parse-string raw (mapcat identity (yaml-options)))]
                     (if (or (nil? value) (map? value))
                       {:properties (or value {}) :diagnostics []}
                       {:properties nil :diagnostics [{:kind :frontmatter/not-a-map}]}))
                   (catch Exception _
                     ;; Do not copy parser exceptions containing source text into
                     ;; receipts/logs. Invalid YAML remains present and editable.
                     {:properties nil :diagnostics [{:kind :frontmatter/invalid}]}))))
        {:from 0 :to 0 :properties nil
         :diagnostics [{:kind :frontmatter/unclosed}]}))))

(defn- children [^Node parent]
  (take-while some? (iterate #(some-> ^Node % .getNext) (.getFirstChild parent))))

(defn- link-data [^Node node]
  (let [target (cond
                 (instance? Link node) node
                 (instance? LinkRef node) (.getReferenceNode ^LinkRef node (.getDocument node)))]
    (cond
      (instance? Link target)
      {:url (Escaping/unescapeString (.getUrl ^Link target))
       :title (Escaping/unescapeString (.getTitle ^Link target))}
      (instance? Reference target)
      {:url (Escaping/unescapeString (.getUrl ^Reference target))
       :title (Escaping/unescapeString (.getTitle ^Reference target))})))

(defn- syntax-node [^Node node offset depth remaining]
  (when (or (> depth max-syntax-depth) (neg? (vswap! remaining dec)))
    (invalid! "Document syntax exceeds the projection limit"))
  (let [link (link-data node)
        projected-children (mapv #(syntax-node % offset (inc depth) remaining) (children node))
        ;; Flexmark represents an empty table cell with a literal padding-space
        ;; Text node. GFM cell padding is not content. Keep explicit entities
        ;; and code spans distinct so a deliberately entered space survives.
        projected-children (if (and (instance? TableCell node)
                                     (every? #(and (= :Text (:kind %))
                                                    (re-matches #"[ \t]*" (:text %))) projected-children))
                             [] projected-children)]
    (cond-> {:kind (keyword (.getNodeName node))
             :from (+ offset (.getStartOffset node))
             :to (+ offset (.getEndOffset node))
             :children projected-children}
      (instance? Heading node) (assoc :heading-level (.getLevel ^Heading node))
      (instance? Text node) (assoc :text (Escaping/unescapeString (.getChars node)))
      ;; Decode exactly once into inert text; never reparse it as HTML/Markdown.
      ;; Keep the original entity's source span for lossless no-op projection.
      (instance? HtmlEntity node) (assoc :text (Escaping/unescapeHtml ^CharSequence (.getChars node)))
      (instance? Footnote node)
      (assoc :footnote {:raw (str (.getChars node)) :label (str (.getText ^Footnote node))})
      (math/token-data node)
      (assoc :math (-> (math/token-data node)
                      (update :content-from + offset) (update :content-to + offset)))
      (instance? Code node) (assoc :literal (str (.getText ^Code node)))
      (instance? FencedCodeBlock node)
      (assoc :literal (str (.getContentChars ^FencedCodeBlock node))
             :language (str (.getInfo ^FencedCodeBlock node)))
      (instance? IndentedCodeBlock node) (assoc :literal (str (.getContentChars ^IndentedCodeBlock node)))
      (instance? OrderedList node) (assoc :start (.getStartNumber ^OrderedList node))
      link (merge link)
      (instance? TaskListItem node) (assoc :checked? (.isItemDoneMarker ^TaskListItem node))
      (instance? TableCell node)
      (merge (let [alignment (.getAlignment ^TableCell node)]
               (cond-> {:table-header? (.isHeader ^TableCell node) :table-span (.getSpan ^TableCell node)}
                 alignment (assoc :table-alignment (keyword (.toLowerCase (.name alignment)))))))
      (instance? WikiNode node)
      (assoc :reference
             (let [^WikiNode wiki node
                   target (.getPageRef wiki)
                   label (.getText wiki)
                   fragment (.getAnchorRef wiki)]
               (cond-> {:kind (if (instance? WikiImage node) :embed :wikilink)
                        :target (str target)
                        :target-from (+ offset (.getStartOffset target))
                        :target-to (+ offset (.getEndOffset target))
                        :status :unresolved}
                 (seq (str label)) (assoc :label (str label))
                 (seq (str fragment)) (assoc :fragment (str fragment))))))))

(defn references
  "References recognized in valid Markdown syntax contexts, with exact source
   spans and unresolved spelling. Resolution/authority is a separate operation."
  [document]
  (->> (:nodes document)
       (mapcat #(tree-seq (comp seq :children) :children %))
       (keep (fn [{:keys [reference from to]}]
               (when reference (assoc reference :from from :to to
                                                :revision (:revision document)))))
       vec))

(defn link-references
  "Source-positioned wiki and ordinary Markdown links. Reference definitions
   count only when used by an actual link; code/comments are never reparsed."
  [document]
  (into []
        (keep (fn [{:keys [reference url kind from to]}]
                (when-let [target (or reference (when url {:kind :link :target url :syntax kind}))]
                  (assoc target :from from :to to :revision (:revision document)))))
        (mapcat #(tree-seq (comp seq :children) :children %) (:nodes document))))

(defn parse
  "Retain EXACT source and derive syntax spans/properties. Invalid frontmatter
   produces diagnostics, not a lossy repair. Property typing is a separate layer."
  [source]
  (let [id (revision source)
        fm (frontmatter source)
        ;; An initial UTF-8 BOM belongs to the immutable file prefix, not the
        ;; first paragraph. Keep absolute source coordinates. Unclosed
        ;; frontmatter's explicit :to 0 retains its source-only diagnostics.
        offset (or (:to fm) (when (.startsWith ^String source "\uFEFF") 1) 0)
        tree (.parse (parser) (subs source offset))
        ;; Per-call traversal budget; never a registry or shared editor state.
        remaining (volatile! max-syntax-nodes)]
    {:source source :revision id :syntax-profile syntax-profile :body-from offset :frontmatter fm
     :diagnostics (vec (:diagnostics fm))
     :nodes (mapv #(syntax-node % offset 0 remaining) (children tree))}))

(defn- boundary? [^String source position]
  (not (and (pos? position) (< position (.length source))
            (Character/isHighSurrogate (.charAt source (dec position)))
            (Character/isLowSurrogate (.charAt source position)))))

(defn patch
  "Apply non-overlapping edits in the expected source revision's coordinates.
   Untouched bytes survive exactly. No implicit rebase or last-writer-wins."
  [source expected-revision edits]
  (when-not (= expected-revision (revision source))
    (throw (ex-info "Document changed; review the current revision before saving"
                    {:type ::conflict})))
  (when-not (and (vector? edits) (<= (count edits) 1000))
    (invalid! "Document edits must be a bounded vector"))
  (doseq [{:keys [from to text] :as edit} edits]
    (when-not (and (map? edit) (= #{:from :to :text} (set (keys edit)))
                   (integer? from) (integer? to) (<= 0 from to (count source))
                   (string? text) (boundary? source from) (boundary? source to))
      (invalid! "Invalid document edit"))
    (source-bytes text))
  (let [ordered (sort-by (juxt :from :to) edits)]
    (doseq [[a b] (partition 2 1 ordered)]
      (when (or (> (:to a) (:from b)) (= (:from a) (:from b)))
        (invalid! "Document edits overlap or have ambiguous ordering")))
    (parse (reduce (fn [text {:keys [from to] replacement :text}]
                     (str (subs text 0 from) replacement (subs text to)))
                   source (reverse ordered)))))
