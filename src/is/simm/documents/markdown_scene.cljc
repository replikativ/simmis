(ns is.simm.documents.markdown-scene
  "Inert display of source-preserved Markdown. Never a serializer or HTML parser."
  (:require [is.simm.documents.rich-text :as rich]))

(def preview-kinds #{:TableBlock :BlockQuote :HtmlCommentBlock})
(def max-preview-chars 65536)
(def max-preview-nodes 5000)

(defn preview [source node]
  (when (and (contains? preview-kinds (:kind node))
             (<= 0 (:from node) (:to node) (count source))
             (<= (- (:to node) (:from node)) max-preview-chars))
    (let [remaining (volatile! max-preview-nodes)
          raw #(subs source (:from %) (:to %))]
      (letfn [(walk [n depth]
                (when (or (> depth 100) (neg? (vswap! remaining dec)))
                  (throw (ex-info "Markdown preview exceeds display bounds" {:type ::limit})))
                (let [children #(vec (keep (fn [child] (walk child (inc depth))) (:children n)))
                      element (fn [tag & [attrs]] {:tag tag :attrs (or attrs {}) :children (children)})]
                  (case (:kind n)
                    (:Text :HtmlEntity) (:text n)
                    :SoftLineBreak " "
                    :HardLineBreak {:tag :br :attrs {} :children []}
                    :StrongEmphasis (element :strong)
                    :Emphasis (element :em)
                    :Strikethrough (element :s)
                    :Code {:tag :code :attrs {} :children [(:literal n)]}
                    :Link (if (rich/safe-link? (:url n))
                            (element :a {:href (:url n) :rel "noopener noreferrer" :target "_blank"})
                            (element :span))
                    :Paragraph (element :p)
                    :Heading (element (get {1 :h1 2 :h2 3 :h3 4 :h4 5 :h5 6 :h6} (:heading-level n) :p))
                    :BlockQuote (element :blockquote)
                    :BulletList (element :ul)
                    :OrderedList (element :ol {:start (:start n)})
                    (:BulletListItem :OrderedListItem) (element :li)
                    :TableBlock (element :table)
                    :TableHead (element :thead)
                    :TableBody (element :tbody)
                    :TableRow (element :tr)
                    :TableCaption (element :caption)
                    :TableCell (element (if (:table-header? n) :th :td)
                                        (cond-> {} (:table-header? n) (assoc :scope "col")
                                          (contains? #{:left :right :center} (:table-alignment n))
                                          (assoc :class (str "memory-align-" (name (:table-alignment n))))))
                    :TableSeparator nil
                    (:HtmlCommentBlock :HtmlInlineComment) nil
                    (:FencedCodeBlock :IndentedCodeBlock)
                    {:tag :pre :attrs {} :children [{:tag :code :attrs {:data-language (or (:language n) "")}
                                                    :children [(:literal n)]}]}
                    ;; Unknown syntax stays visible as text, including raw
                    ;; HTML and images. No image/network/iframe/widget effect.
                    (raw n))))]
        (try
          {:source (raw node) :scene (walk node 0)}
          (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) error
            (if (= ::limit (:type (ex-data error))) nil (throw error))))))))

(defn index [source analysis]
  ;; Limit the whole page, not merely one table. This is disposable display
  ;; data derived from the same frozen source as the editor's opaque labels.
  (loop [nodes (mapcat #(tree-seq (comp seq :children) :children %) (:nodes analysis))
         remaining max-preview-chars out {}]
    (if-let [node (first nodes)]
      (if (and (contains? preview-kinds (:kind node))
               (<= (- (:to node) (:from node)) remaining))
        (if-let [projection (preview source node)]
          (recur (next nodes) (- remaining (count (:source projection)))
                 (assoc out (str "opaque:" (:from node)) projection))
          (recur (next nodes) remaining out))
        (recur (next nodes) remaining out))
      out)))
