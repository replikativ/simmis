(ns is.simm.uis.web.desktop.code-highlight
  "Bounded presentation-only code tokens. Highlighting never changes the model."
  (:require [clojure.string :as str]
            ["@tiptap/core" :refer [Extension]]
            ["@tiptap/pm/state" :refer [Plugin PluginKey]]
            ["@tiptap/pm/view" :refer [Decoration DecorationSet]]
            [is.simm.uis.web.desktop.syntax-highlight :as syntax]))

(def max-cell-chars 16384)
(def max-document-chars 65536)
(def max-token-ranges 4096)
(def max-highlight-html 524288)

(defn language [info]
  (when (and (string? info) (<= (count info) 512))
    (let [token (some-> (re-find #"^\s*([A-Za-z0-9_+-]+)(?:\s|$)" info) second str/lower-case)
          name (get {"clj" "clojure" "cljs" "clojure" "cljc" "clojure" "edn" "clojure" "bb" "clojure"
                     "js" "javascript" "py" "python" "sh" "bash" "shell" "bash" "yml" "yaml" "md" "markdown"
                     "simmis-chart" "json"}
                    token token)]
      (when (contains? #{"clojure" "javascript" "python" "bash" "json" "css" "html" "xml"
                         "sql" "diff" "yaml" "markdown" "superficie"} name)
        name))))

(defn- invalid! [] (throw (js/Error. "Invalid highlight token output")))

(defn html-ranges
  "Decode the highlighter's escaped span output in an inert template. Admit
   only closed token markup whose UTF-16 text exactly equals the source. No
   parsed node is ever inserted into the editor or another live document."
  [text html]
  (try
    (when-not (and (string? text) (<= (count text) max-cell-chars)
                   (string? html) (<= (count html) max-highlight-html)) (invalid!))
    (let [markup (str/replace html "\r" "&#13;")
          ^js template (.createElement js/document "template")
          offset (volatile! 0) nodes (volatile! 0) ranges (volatile! [])]
      ;; Literal CRs normalize during HTML parsing; character references retain
      ;; their original code points. This transforms markup, never source.
      (when (> (count markup) max-highlight-html) (invalid!))
      (set! (.-innerHTML template) markup)
      (when-not (= text (.. template -content -textContent)) (invalid!))
      (letfn [(walk! [^js node classes depth]
                (when (or (> depth 32) (> (vswap! nodes inc) 16384)) (invalid!))
                (case (.-nodeType node)
                  3 (let [from @offset to (+ from (count (.-nodeValue node)))]
                      (vreset! offset to)
                      (when (and (< from to) (seq classes))
                        (when (>= (count @ranges) max-token-ranges) (invalid!))
                        (vswap! ranges conj {:from from :to to :class (str/join " " classes)})))
                  1 (do
                      (when-not (and (= "SPAN" (.-nodeName node))
                                     (= 1 (.. node -attributes -length))
                                     (.hasAttribute node "class")) (invalid!))
                      (let [own (str/split (.getAttribute node "class") #"\s+")]
                        (when-not (and (seq own)
                                       (every? #(re-matches #"(?:hljs-[a-z0-9_-]+|language-[a-z0-9_-]+|[a-z][a-z0-9]*_+)" %) own))
                          (invalid!))
                        (doseq [child (array-seq (.-childNodes node))]
                          (walk! child (vec (distinct (concat classes own))) (inc depth)))))
                  (invalid!)))]
        (doseq [node (array-seq (.. template -content -childNodes))] (walk! node [] 0)))
      (when-not (= @offset (count text)) (invalid!))
      @ranges)
    (catch :default _ [])))

(defn token-ranges [text info]
  (if-let [name (when (and (string? text) (<= (count text) max-cell-chars)) (language info))]
    (try
      (let [^js engine syntax/engine]
        (html-ranges text (.-value (.highlight engine text #js {:language name :ignoreIllegals true}))))
      (catch :default _ []))
    []))

(defn- decorations [^js doc]
  (let [chars (volatile! 0) cells (volatile! 0) spans (volatile! [])]
    (.descendants doc
                  (fn [^js node position]
                    (when (= "codeBlock" (.. node -type -name))
                      (let [text (.-textContent node)]
                        (vswap! chars + (count text))
                        (vswap! cells inc)
                        (when (and (<= @chars max-document-chars) (<= @cells 128)
                                   (< (count @spans) (* 2 max-token-ranges)))
                          (let [tokens (token-ranges text (aget (.-attrs node) "language"))]
                            (when (<= (+ (count @spans) (count tokens)) (* 2 max-token-ranges))
                              (doseq [{:keys [from to class]} tokens]
                                (vswap! spans conj (.inline Decoration (+ position 1 from) (+ position 1 to)
                                                           #js {:class class})))))))
                      false)))
    (.create DecorationSet doc (to-array @spans))))

(defn extension []
  (let [^js key (PluginKey. "memoryCodeHighlight")]
    (.create Extension
             #js {:name "memoryCodeHighlight"
                  :addProseMirrorPlugins
                  (fn []
                    #js [(Plugin.
                          #js {:key key
                               :state #js {:init (fn [_ ^js state] (decorations (.-doc state)))
                                           :apply (fn [^js transaction current _ ^js new-state]
                                                    (if (.-docChanged transaction) (decorations (.-doc new-state)) current))}
                               :props #js {:decorations (fn [state] (.getState key state))}})])})))
