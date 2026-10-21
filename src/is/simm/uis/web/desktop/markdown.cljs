(ns is.simm.uis.web.desktop.markdown
  "Markdown rendering for chat messages using marked + highlight.js."
  (:require ["marked" :refer [Marked]]
            ["marked-highlight" :refer [markedHighlight]]
            ["highlight.js/lib/core" :as hljs]
            ;; Register common languages for code blocks
            ["highlight.js/lib/languages/clojure" :as clojure-lang]
            ["highlight.js/lib/languages/javascript" :as js-lang]
            ["highlight.js/lib/languages/python" :as python-lang]
            ["highlight.js/lib/languages/bash" :as bash-lang]
            ["highlight.js/lib/languages/json" :as json-lang]
            ["highlight.js/lib/languages/css" :as css-lang]
            ["highlight.js/lib/languages/xml" :as xml-lang]
            ["highlight.js/lib/languages/sql" :as sql-lang]
            ["highlight.js/lib/languages/diff" :as diff-lang]
            ["highlight.js/lib/languages/yaml" :as yaml-lang]
            ["highlight.js/lib/languages/markdown" :as md-lang]
            ["./superficie_hljs.js" :as sup-hljs]
            [is.simm.uis.web.desktop.sanitize :as sanitize]))

;; Register languages
(.registerLanguage hljs "clojure" clojure-lang)
(.registerLanguage hljs "javascript" js-lang)
(.registerLanguage hljs "js" js-lang)
(.registerLanguage hljs "python" python-lang)
(.registerLanguage hljs "bash" bash-lang)
(.registerLanguage hljs "shell" bash-lang)
(.registerLanguage hljs "json" json-lang)
(.registerLanguage hljs "css" css-lang)
(.registerLanguage hljs "html" xml-lang)
(.registerLanguage hljs "xml" xml-lang)
(.registerLanguage hljs "sql" sql-lang)
(.registerLanguage hljs "diff" diff-lang)
(.registerLanguage hljs "yaml" yaml-lang)
(.registerLanguage hljs "markdown" md-lang)
(.registerLanguage hljs "superficie" sup-hljs)

;; Create marked instance with highlight.js integration
(def marked-instance
  (let [m (Marked.)]
    (.use m (markedHighlight
             #js {:emptyLangClass "hljs"
                  :langPrefix "hljs language-"
                  :highlight (fn [code lang]
                               (if (and lang (.getLanguage hljs lang))
                                 (.-value (.highlight hljs code #js {:language lang}))
                                 (.-value (.highlightAuto hljs code))))}))
    ;; Configure for GFM (tables, strikethrough, etc.)
    (.use m #js {:gfm true
                 :breaks true})
    m))

(defn render-markdown
  "Render untrusted Markdown to sanitized HTML, after highlighting finishes."
  [text]
  (when (and text (seq text))
    (sanitize/html (.parse marked-instance text))))

(defn highlight-code
  "Highlight code with hljs and return the highlighted HTML string.
   Language defaults to 'clojure' if not specified.
   Returns nil for empty/nil input."
  ([code] (highlight-code code "clojure"))
  ([code language]
   (when (and code (seq code))
     (if (.getLanguage hljs language)
       (.-value (.highlight hljs code #js {:language language}))
       (.-value (.highlightAuto hljs code))))))

(defn create-markdown-element
  "Create a DOM element with rendered markdown content.
   Returns a div element with innerHTML set to rendered markdown."
  [text]
  (let [el (js/document.createElement "div")]
    (set! (.-className el) "markdown-content")
    (set! (.-innerHTML el) (or (render-markdown text) ""))
    el))
