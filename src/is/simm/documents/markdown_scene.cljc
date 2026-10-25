(ns is.simm.documents.markdown-scene
  "Inert display of source-preserved Markdown. Never a serializer or HTML parser."
  (:require [is.simm.documents.rich-text :as rich]
            [is.simm.documents.attachments :as attachments]))

(def preview-kinds #{:TableBlock :BlockQuote :HtmlCommentBlock})
(def max-preview-chars 65536)
(def max-preview-nodes 5000)

(defn- previewable? [node]
  (or (contains? preview-kinds (:kind node))
      (and (= :Paragraph (:kind node))
           (some #(#{:Image :ImageRef} (:kind %))
                 (tree-seq (comp seq :children) :children node)))))

(defn preview
  ([source node] (preview source node {}))
  ([source node image-urls] (preview source node image-urls nil))
  ([source node image-urls file-base]
  (when (and (previewable? node)
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
                    (:Link :LinkRef) (let [href (attachments/resolve-link file-base (:url n))]
                                      (if (rich/safe-link? href)
                                        (element :a {:href href :rel "noopener noreferrer" :target "_blank"})
                                        (element :span)))
                    (:Image :ImageRef)
                    (if-let [url (get image-urls (:url n))]
                      {:tag :img :attrs {:src url :alt (apply str (keep :text (:children n))) :loading "lazy"}
                       :children []}
                      (raw n))
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
                    ;; HTML and unresolved images. Only explicitly resolved
                    ;; owned images above may issue a media request.
                    (raw n))))]
        (try
          {:source (raw node) :scene (walk node 0)}
          (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) error
            (if (= ::limit (:type (ex-data error))) nil (throw error)))))))))

(defn index
  ([source analysis] (index source analysis {}))
  ([source analysis image-urls] (index source analysis image-urls nil))
  ([source analysis image-urls file-base]
  ;; Limit the whole page, not merely one table. This is disposable display
  ;; data derived from the same frozen source as the editor's opaque labels.
  (loop [nodes (mapcat #(tree-seq (comp seq :children) :children %) (:nodes analysis))
         remaining max-preview-chars out {}]
    (if-let [node (first nodes)]
      (if (and (previewable? node)
               (<= (- (:to node) (:from node)) remaining))
        (if-let [projection (preview source node image-urls file-base)]
          (recur (next nodes) (- remaining (count (:source projection)))
                 (assoc out (str "opaque:" (:from node)) projection))
          (recur (next nodes) remaining out))
        (recur (next nodes) remaining out))
      out))))
