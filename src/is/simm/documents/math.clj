(ns is.simm.documents.math
  "Bounded source-preserving TeX tokens, recognized by Markdown parser hooks.
   No TeX evaluation, source rewriting, global registry or rendered HTML."
  (:import [com.vladsch.flexmark.parser Parser$ParserExtension InlineParserExtension
            InlineParserExtensionFactory LightInlineParser]
           [com.vladsch.flexmark.parser.block AbstractBlockParser BlockParser BlockStart
            BlockContinue CustomBlockParserFactory BlockParserFactory ParserState]
           [com.vladsch.flexmark.util.ast Node Block Document]
           [com.vladsch.flexmark.util.sequence BasedSequence]))

(def max-expression-chars 8192)
(def max-document-expressions 256)
(def max-document-lookahead 1048576)

(defn token-data [node]
  (when (instance? clojure.lang.IMeta node)
    (::token (meta node))))

(defn- token [^BasedSequence chars content-from content-to display?]
  (let [data {:raw (str chars) :tex (str (.baseSubSequence chars content-from content-to))
              :content-from content-from :content-to content-to :display? display?}]
    (if display?
      (proxy [Block clojure.lang.IMeta] [chars]
        (getNodeName [] "DisplayMath")
        (getSegments [] (into-array BasedSequence []))
        (meta [] {::token data}))
      (proxy [Node clojure.lang.IMeta] [chars]
        (getNodeName [] "InlineMath")
        (getSegments [] (into-array BasedSequence []))
        (meta [] {::token data})))))

(defn- whitespace? [c] (Character/isWhitespace ^char c))
(defn- digit? [c] (Character/isDigit ^char c))

(defn- inline-end [^BasedSequence input start lookahead]
  (let [length (.length input) limit (min length (+ start max-expression-chars))]
    (when (and (< (inc start) length)
               (not= \$ (.charAt input (inc start)))
               (not (whitespace? (.charAt input (inc start))))
               (or (zero? start) (not= \$ (.charAt input (dec start)))))
      (loop [i (inc start) slashes 0 escaped-dollar? false]
        (when (and (< i limit) (pos? @lookahead))
          (vswap! lookahead dec)
          (let [c (.charAt input i)]
            (cond
              (or (= c \newline) (= c \return)) nil
              (and (= c \$) (even? slashes)
                   (not (whitespace? (.charAt input (dec i))))
                   (or (not= \$ (.charAt input (dec i))) escaped-dollar?)
                   (or (= (inc i) length)
                       (let [next (.charAt input (inc i))]
                         (and (not= next \$) (not (digit? next)))))) (inc i)
              ;; Do not jump over a malformed delimiter/run to claim a later
              ;; independent formula as part of this failed opening.
              (and (= c \$) (even? slashes)) nil
              :else (recur (inc i) (if (= c \\) (inc slashes) 0)
                           (and (= c \$) (odd? slashes))))))))))

(defn- inline-factory [remaining lookahead]
  (reify InlineParserExtensionFactory
    (getCharacters [_] "$")
    (getAfterDependents [_] nil)
    (getBeforeDependents [_] nil)
    (affectsGlobalScope [_] false)
    (^InlineParserExtension apply [_ ^LightInlineParser _]
      (reify InlineParserExtension
        (finalizeDocument [_ _])
        (finalizeBlock [_ _])
        (^boolean parse [_ ^LightInlineParser parser]
          (boolean
           (when (pos? @remaining)
             (let [input (.getInput parser) start (.getIndex parser)]
               (when-let [end (inline-end input start lookahead)]
                 (let [chars (.subSequence input start end)]
                   (.flushTextNode parser)
                   (.appendNode parser (token chars (inc (.getStartOffset chars))
                                              (dec (.getEndOffset chars)) false))
                   (.setIndex parser end)
                   (vswap! remaining dec)
                   true))))))))))

(defn- line-span [^BasedSequence base start limit lookahead]
  (loop [end start]
    (when (and (<= end limit) (pos? @lookahead))
      (vswap! lookahead dec)
      (if (or (= end (.length base)) (#{\newline \return} (.charAt base end)))
        (let [next (cond
                     (= end (.length base)) end
                     (and (= \return (.charAt base end)) (< (inc end) (.length base))
                          (= \newline (.charAt base (inc end)))) (+ end 2)
                     :else (inc end))]
          {:from start :to end :next next})
        (recur (inc end))))))

(defn- display-span [^BasedSequence opening lookahead]
  ;; Only complete bounded blocks are claimed. An unmatched opening never
  ;; swallows the rest of the document or creates a guessed closing delimiter.
  (let [base (.getBaseSequence opening) from (.getStartOffset opening)
        content-from (.getEndOffset opening) limit (min (.length base) (+ from max-expression-chars))]
    (loop [start content-from lines 0]
      (when (and (< start limit) (< lines 64))
        (when-let [{:keys [to next]} (line-span base start limit lookahead)]
          (let [text (str (.subSequence base start to))]
            (cond
              (re-matches #" {0,3}\$\$[ \t]*" text)
              {:chars (.subSequence base from to) :content-from content-from
               :content-to start :close-from start}
              (or (re-matches #"[ \t]*" text) (= next to)) nil
              :else (recur next (inc lines)))))))))

(defn- display-parser [{:keys [chars content-from content-to close-from]}]
  (let [node (token chars content-from content-to true)]
    (proxy [AbstractBlockParser] []
      (getBlock [] node)
      (isRawText [] true)
      (tryContinue [^ParserState state]
        (if (>= (.getStartOffset (.getLine state)) close-from)
          (BlockContinue/finished)
          (BlockContinue/atIndex (.getIndex state))))
      (addLine [_ _])
      (closeBlock [_])
      (parseInlines [_]))))

(defn- display-factory [remaining lookahead]
  (reify CustomBlockParserFactory
    (getAfterDependents [_] nil)
    (getBeforeDependents [_] nil)
    (affectsGlobalScope [_] false)
    (^BlockParserFactory apply [_ ^com.vladsch.flexmark.util.data.DataHolder _]
      (reify BlockParserFactory
        (^BlockStart tryStart [_ ^ParserState state ^com.vladsch.flexmark.parser.block.MatchedBlockParser matched]
          ;; First dialect: standalone top-level $$ lines. Container prefixes
          ;; in lists/quotes remain ordinary source rather than guessed TeX.
          (let [line (.getLine state) active (.getBlockParser matched)]
            (if-let [span (when (and (pos? @remaining) (< (.getIndent state) 4)
                                     (instance? Document (.getBlock ^BlockParser active))
                                     (re-matches #" {0,3}\$\$[ \t]*" (str line)))
                            (display-span (.getLineWithEOL state) lookahead))]
              (do (vswap! remaining dec)
                  (.atIndex (BlockStart/of (into-array BlockParser [(display-parser span)]))
                            (.length line)))
              (BlockStart/none))))))))

(defn extension
  "A fresh parser-local budget. Construct with each Markdown parser; never
   retain this extension across documents or execution contexts."
  []
  (let [remaining (volatile! max-document-expressions)
        lookahead (volatile! max-document-lookahead)]
    (reify Parser$ParserExtension
      (parserOptions [_ _])
      (extend [_ builder]
        (.customInlineParserExtensionFactory builder (inline-factory remaining lookahead))
        (.customBlockParserFactory builder (display-factory remaining lookahead))))))
