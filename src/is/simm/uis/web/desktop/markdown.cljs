(ns is.simm.uis.web.desktop.markdown
  "Markdown rendering for chat messages using marked + highlight.js."
  (:require ["marked" :refer [Marked]]
            ["marked-highlight" :refer [markedHighlight]]
            [is.simm.uis.web.desktop.syntax-highlight :as syntax]
            [is.simm.uis.web.desktop.sanitize :as sanitize]))

(def ^js hljs syntax/engine)

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
