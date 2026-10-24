(ns is.simm.documents.rich-text
  "A bounded, deliberately restricted rich-text Markdown adapter. It operates
   on ONE selected source region; unrelated file bytes are never serialized."
  (:require [clojure.string :as str]))

(defn- invalid! []
  (throw (ex-info "This rich-text structure needs Source editing" {:type ::unsupported})))

(def max-table-rows 50)
(def max-table-columns 20)

(defn- attributes-size [attrs]
  (when-not (or (nil? attrs)
                (and (map? attrs) (<= (count attrs) 32)
                     (every? keyword? (keys attrs))
                     (every? #(or (nil? %) (string? %) (number? %) (boolean? %)) (vals attrs))))
    (invalid!))
  (+ (reduce + 0 (map #(count (str %)) (keys attrs)))
     (reduce + 0 (map #(if (string? %) (count %) 0) (vals attrs)))))

(defn- footnote-shape! [node]
  (let [{:keys [sourceKey raw label] :as attrs} (:attrs node)]
    (when-not (and (= #{:sourceKey :raw :label} (set (keys attrs)))
                   (string? sourceKey) (<= (count sourceKey) 64)
                   (re-matches #"^footnote:[0-9]+:[0-9]+$" sourceKey)
                   (string? raw) (<= 4 (count raw) 2048)
                   (not (re-find #"[\r\n]" raw))
                   (string? label) (<= 1 (count label) 2048)
                   (not (seq (:content node))) (nil? (:text node)))
      (invalid!))))

(defn- math-shape! [node]
  (let [{:keys [sourceKey raw tex] :as attrs} (:attrs node)]
    (when-not (and (= #{:sourceKey :raw :tex} (set (keys attrs)))
                   (string? sourceKey) (<= (count sourceKey) 64)
                   (re-matches #"^math:[0-9]+:[0-9]+$" sourceKey)
                   (string? raw) (<= 3 (count raw) 8192)
                   (string? tex) (<= (count tex) 8192)
                   (not (seq (:content node))) (nil? (:text node))
                   (or (not= "displayMath" (:type node)) (not (seq (:marks node))))
                   (not (some #(= "code" (:type %)) (:marks node))))
      (invalid!))))

(defn- table-cell-attrs! [node]
  (let [attrs (:attrs node)]
    (when-not (and (not (seq (:marks node))) (nil? (:text node))
                   (every? #{:colspan :rowspan :colwidth :align} (keys attrs))
                   (= 1 (get attrs :colspan 1)) (= 1 (get attrs :rowspan 1))
                   (nil? (:colwidth attrs)) (contains? #{nil "left" "right" "center"} (:align attrs))
                   (= 1 (count (:content node))) (= "paragraph" (get-in node [:content 0 :type]))
                   (empty? (get-in node [:content 0 :attrs]))
                   (empty? (get-in node [:content 0 :marks])))
      (invalid!))))

(defn- table-shape! [node]
  (let [rows (:content node) width (count (:content (first rows)))
        aligns (mapv #(get-in % [:attrs :align]) (:content (first rows)))]
    (when-not (and (empty? (:attrs node)) (not (seq (:marks node)))
                   (<= 1 (count rows) max-table-rows) (<= 1 width max-table-columns)
                   (every? #(and (= "tableRow" (:type %)) (empty? (:attrs %))
                                  (not (seq (:marks %)))
                                  (= width (count (:content %)))) rows))
      (invalid!))
    (doseq [[i row] (map-indexed vector rows)
            [j cell] (map-indexed vector (:content row))]
      (when-not (= (if (zero? i) "tableHeader" "tableCell") (:type cell)) (invalid!))
      (table-cell-attrs! cell)
      (when-not (= (nth aligns j) (get-in cell [:attrs :align])) (invalid!)))))

(defn validate-model!
  "Bound the whole input before recursive normalization or serialization,
   including nodes such as list items and code text consumed by special cases."
  [model]
  (loop [todo (list [model 0]) nodes 0 chars 0]
    (when-let [[node depth] (first todo)]
      (when-not (and (map? node) (string? (:type node)) (<= depth 100) (< nodes 20000)
                     (or (nil? (:content node)) (sequential? (:content node)))
                     (or (nil? (:marks node)) (and (sequential? (:marks node)) (<= (count (:marks node)) 8))))
        (invalid!))
      (when (= "footnoteReference" (:type node)) (footnote-shape! node))
      (when (contains? #{"inlineMath" "displayMath"} (:type node)) (math-shape! node))
      ;; Validate representability BEFORE normalization can discard attrs.
      (when (= "table" (:type node)) (table-shape! node))
      (when (contains? #{"tableCell" "tableHeader"} (:type node)) (table-cell-attrs! node))
      (let [text (:text node) chars (+ chars (if (string? text) (count text) 0)
                                      (attributes-size (:attrs node))
                                      (reduce + 0 (map #(attributes-size (:attrs %)) (:marks node))))]
        (when (> chars 2097152) (invalid!))
        (recur (concat (map #(vector % (inc depth)) (:content node)) (rest todo)) (inc nodes) chars))))
  model)

(defn safe-link? [s]
  (and (string? s) (<= 1 (count s) 2048)
       (not (re-find #"[\u0000-\u0020\u007f-\u009f\s<>\\]" s))
       (not (str/starts-with? s "//"))
       (or (not (re-find #"^[a-zA-Z][a-zA-Z0-9+.-]*:" s))
           (re-find #"(?i)^(https?|mailto):" s))))

(defn- escaped [s]
  (-> s (str/replace #"\r\n|\r|\n" " ")
      ;; Escape authored ampersands BEFORE creating our own entities: literal
      ;; "&#32;" must stay that text, not become whitespace on the next parse.
      (str/replace "&" "&amp;")
      (str/replace #"[\\`*_{}\[\]<>#.!|~+()$\-]" #(str "\\" %))
      ;; Markdown trims paragraph/heading edges and interprets indentation.
      ;; Ordinary keystrokes (including a space-only paragraph) must survive
      ;; strict round-trip validation. Leaf-local encoding also keeps marked
      ;; whitespace INSIDE its mark; code and raw footnote tokens bypass this.
      (str/replace #"^[ \t]+|[ \t]+$"
                   #(apply str (map (fn [c] (if (= c \tab) "&#9;" "&#32;")) %)))))

(defn- code-span [s]
  (let [s (str/replace s #"\r\n|\r|\n" " ")
        n (inc (reduce max 0 (map count (re-seq #"`+" s))))
        fence (apply str (repeat n "`"))
        padding? (or (str/starts-with? s "`") (str/ends-with? s "`")
                     (and (str/starts-with? s " ") (str/ends-with? s " ")
                          (not (re-matches #" +" s))))]
    (str fence (when padding? " ") s (when padding? " ") fence)))

(declare emit)

(defn- emit-blocks [nodes depth budget newline]
  ;; Native list editing can split one list into adjacent lists. Blank lines
  ;; alone do not separate those in CommonMark. Alternate ordinary Markdown
  ;; markers so reparsing retains every list boundary (and ordered-list start).
  (second
   (reduce (fn [[previous out] node]
             (let [kind (:type node)
                   marker (case kind
                            "bulletList" (if (= previous "-") "+" "-")
                            "orderedList" (if (= previous ".") ")" ".")
                            nil)
                   emitted (emit node depth budget newline marker)]
               [(if (str/blank? emitted) previous marker) (conj out emitted)]))
           [nil []] nodes)))

(defn- delimited [marker s]
  ;; Explicit anchors matter: cljs re-matches executes the JS regex first;
  ;; an unanchored lazy body can choose an empty match and fail its size check.
  (let [[_ before body after] (re-matches #"^(\s*)([\s\S]*?)(\s*)$" s)]
    (if (seq body) (str before marker body marker after) s)))

(defn- marked [initial marks & [table?]]
  (reduce (fn [s mark]
            (case (:type mark)
              "code" s
              "bold" (delimited "**" s)
              "italic" (delimited "*" s)
              "strike" (delimited "~~" s)
              "link" (let [url (get-in mark [:attrs :href]) title (get-in mark [:attrs :title])]
                       (when-not (safe-link? url) (invalid!))
                       (str "[" s "](<" (if table? (str/replace url "|" "\\|") url) ">"
                            (when (seq title)
                              (let [title (str/replace title #"[\\\"]" #(str "\\" %))]
                                (str " \"" (if table? (str/replace title "|" "\\|") title) "\""))) ")"))
              (invalid!))) initial marks))

(defn- inline [node depth budget newline & [table?]]
  (when (or (> depth 100) (neg? (vswap! budget dec))) (invalid!))
  (case (:type node)
    "hardBreak" (str "  " newline)
    "footnoteReference" (do (when (some #(= "code" (:type %)) (:marks node)) (invalid!))
                             (marked (get-in node [:attrs :raw]) (:marks node) table?))
    "inlineMath" (marked (get-in node [:attrs :raw]) (:marks node) table?)
    "text" (let [text (:text node) marks (:marks node)
                 _ (when-not (string? text) (invalid!))
                 code? (some #(= "code" (:type %)) marks)
                 initial (if code? (code-span (if table? (str/replace text "|" "\\|") text)) (escaped text))]
             (marked initial marks table?))
    (invalid!)))

(defn- inlines [node depth budget newline & [table?]]
  ;; An emphasized footnote and its adjacent text form ONE marked span. Closing
  ;; and reopening '*' at each inline atom creates ambiguous adjacent delimiters.
  (apply str
         (map (fn [run]
                (let [marks (:marks (first run))]
                  (if (some #(= "code" (:type %)) marks)
                    (apply str (map #(inline % (inc depth) budget newline table?) run))
                    (marked (apply str (map #(inline (dissoc % :marks) (inc depth) budget newline table?) run)) marks table?))))
              (partition-by #(vec (:marks %)) (:content node)))))

(defn- emit [node depth budget newline & [list-marker]]
  (when (or (> depth 100) (neg? (vswap! budget dec))) (invalid!))
  (case (:type node)
    "doc" (str/join (str newline newline) (emit-blocks (:content node) (inc depth) budget newline))
    "paragraph" (inlines node depth budget newline)
    "heading" (let [level (get-in node [:attrs :level])]
                (when-not (and (integer? level) (<= 1 level 6)) (invalid!))
                (str (apply str (repeat level "#")) " " (inlines node depth budget newline)))
    "horizontalRule" "---"
    "displayMath" (get-in node [:attrs :raw])
    "table"
    (let [rows (:content node)
          cell (fn [node]
                 (let [paragraph (first (:content node))]
                   (when (some #(or (= "hardBreak" (:type %))
                                    (and (= "text" (:type %)) (re-find #"[\r\n]" (:text %)))) (:content paragraph))
                     (invalid!))
                   (inlines paragraph (inc depth) budget newline true)))
          row (fn [cells] (str "| " (str/join " | " cells) " |"))
          separator (mapv #(case (get-in % [:attrs :align]) "left" ":---" "right" "---:" "center" ":---:" "---")
                          (:content (first rows)))]
      (str/join newline (concat [(row (mapv cell (:content (first rows)))) (row separator)]
                                (map #(row (mapv cell (:content %))) (rest rows)))))
    "codeBlock" (let [language (or (get-in node [:attrs :language]) "")
                      _ (when-not (and (string? language) (re-matches #"[a-zA-Z0-9_+.-]{0,40}" language)) (invalid!))
                      text (apply str (map (fn [n] (when-not (= "text" (:type n)) (invalid!)) (:text n)) (:content node)))
                      text (str/replace text #"\r\n|\r|\n" newline)
                      n (max 3 (inc (reduce max 0 (map count (re-seq #"`+" text)))))
                      fence (apply str (repeat n "`"))]
                  (str fence language newline text newline fence))
    ("bulletList" "orderedList")
    (let [start (get-in node [:attrs :start] 1)
          _ (when-not (and (integer? start) (<= 0 start 999999999)) (invalid!))]
      (str/join
       newline
       (map-indexed
        (fn [index item]
          (when-not (= "listItem" (:type item)) (invalid!))
          (let [marker (if (= "bulletList" (:type node)) (str (or list-marker "-") " ")
                           (str (+ start index) (or list-marker ".") " "))
                body (str/join (str newline newline) (emit-blocks (:content item) (inc depth) budget newline))
                [first-line & more] (str/split body (re-pattern newline) -1)
                padding (apply str (repeat (count marker) " "))]
            (str marker first-line
                 (when (seq more) (str newline (str/join newline (map #(str padding %) more)))))))
        (:content node))))
    (invalid!)))

(defn markdown
  "Generate separators in the selected newline style. Frozen source tokens
   retain their own bytes, including mixed newline conventions."
  ([document] (markdown document "\n"))
  ([document newline]
   (when-not (contains? #{"\n" "\r\n" "\r"} newline) (invalid!))
   (let [result (emit (validate-model! document) 0 (volatile! 20000) newline)]
     (when (> (count result) 2097152) (invalid!))
     result)))

(defn- normalized-marks [marks]
  (vec (sort-by :type
                (map (fn [mark]
                       (cond-> {:type (:type mark)}
                         (= "link" (:type mark))
                         (assoc :attrs {:href (get-in mark [:attrs :href])
                                        :title (not-empty (get-in mark [:attrs :title]))}))) marks))))

(defn- normalize*
  "Ignore native editor defaults and merge adjacent equal-mark text. A no-op
   open/selection must not reserialize source because Tiptap added attrs."
  [node]
  (let [type (:type node)
        content (reduce (fn [out n]
                          (let [n (normalize* n) before (peek out)]
                            (if (and (= "text" (:type n)) (= "text" (:type before))
                                     (= (:marks n) (:marks before)))
                              (conj (pop out) (update before :text str (:text n)))
                              (conj out n)))) [] (:content node))
        content (if (and (= type "doc") (= content [{:type "paragraph" :content []}])) [] content)]
    (cond
      (contains? #{"footnoteReference" "inlineMath" "displayMath"} type)
      {:type type :attrs (:attrs node) :marks (normalized-marks (:marks node))}
      (= type "text")
      {:type type :text (:text node)
       :marks (normalized-marks (:marks node))}
      :else
      (cond-> {:type type :content content}
        (= type "heading") (assoc :attrs {:level (get-in node [:attrs :level])})
        (= type "orderedList") (assoc :attrs {:start (get-in node [:attrs :start] 1)})
        (= type "codeBlock") (assoc :attrs {:language (or (get-in node [:attrs :language]) "")})
        (contains? #{"tableCell" "tableHeader"} type)
        (assoc :attrs {:colspan 1 :rowspan 1 :colwidth nil :align (get-in node [:attrs :align])})))))

(defn normalize [node] (normalize* (validate-model! node)))

(defn- source-tokens [model]
  (filter #(contains? #{"footnoteReference" "inlineMath" "displayMath"} (:type %))
          (tree-seq #(seq (:content %)) :content model)))

(defn validate-provenance!
  "Footnote/math tokens may be removed or moved within this frozen region, never
   fabricated, duplicated or imported from another region. No registry/authority
   is accepted from the editor: callers reconstruct the baseline from source."
  [baseline edited]
  (validate-model! baseline)
  (validate-model! edited)
  (let [identity #(select-keys % [:type :attrs])
        original (frequencies (map identity (source-tokens baseline)))
        current (frequencies (map identity (source-tokens edited)))]
    (doseq [[attrs amount] current]
      (when (> amount (get original attrs 0)) (invalid!))))
  edited)

(defn roundtrip-normalize
  "Semantic equivalence AFTER frozen-source provenance validation. Ignore only
   projection-local footnote keys and empty ROOT paragraphs (Markdown blank-line
   layout). Preserve significant text, token spelling and nested list structure."
  [model]
  (letfn [(walk [node]
            (cond-> node
              (contains? #{"footnoteReference" "inlineMath" "displayMath"} (:type node))
              (update :attrs dissoc :sourceKey)
              (contains? node :content) (update :content #(mapv walk %))))]
    (let [normalized (normalize model)
          normalized (if (= "doc" (:type normalized))
                       (update normalized :content
                               #(filterv (fn [node]
                                           (not (and (= "paragraph" (:type node))
                                                     (empty? (:content node))))) %))
                       normalized)]
      (walk normalized))))

(defn- inline-model [node marks]
  (let [children #(vec (mapcat (fn [n] (inline-model n %)) (:children node)))]
    (case (:kind node)
      (:Text :HtmlEntity) (if (seq (:text node)) [(cond-> {:type "text" :text (:text node)}
                                    (seq marks) (assoc :marks marks))] [])
      :SoftLineBreak [(cond-> {:type "text" :text " "} (seq marks) (assoc :marks marks))]
      :HardLineBreak [{:type "hardBreak"}]
      :Footnote [(cond-> {:type "footnoteReference"
                          :attrs {:sourceKey (str "footnote:" (:from node) ":" (:to node))
                                  :raw (get-in node [:footnote :raw]) :label (get-in node [:footnote :label])}}
                   (seq marks) (assoc :marks marks))]
      :InlineMath [(cond-> {:type "inlineMath"
                            :attrs {:sourceKey (str "math:" (:from node) ":" (:to node))
                                    :raw (get-in node [:math :raw]) :tex (get-in node [:math :tex])}}
                     (seq marks) (assoc :marks marks))]
      :Emphasis (children (conj marks {:type "italic"}))
      :StrongEmphasis (children (conj marks {:type "bold"}))
      :Strikethrough (children (conj marks {:type "strike"}))
      :Code (let [s (str/replace (:literal node) #"\r\n|\r|\n" " ")
                  s (if (and (str/starts-with? s " ") (str/ends-with? s " ")
                              (not (re-matches #" +" s))) (subs s 1 (dec (count s))) s)]
              (if (seq s) [{:type "text" :text s :marks (conj marks {:type "code"})}] []))
      :Link (do (when-not (safe-link? (:url node)) (invalid!))
                (children (conj marks {:type "link" :attrs {:href (:url node) :title (:title node)}})))
      (invalid!))))

(declare block-model)

(defn- table-inline-model [node]
  (letfn [(decode-code [n]
            (cond-> n
              (= :Code (:kind n)) (update :literal str/replace "\\|" "|")
              (contains? n :children) (update :children #(mapv decode-code %))))]
    (inline-model (decode-code node) [])))

(defn- table-model [node]
  (let [head (filter #(= :TableHead (:kind %)) (:children node))
        body (filter #(= :TableBody (:kind %)) (:children node))
        separator (filter #(= :TableSeparator (:kind %)) (:children node))]
    (when-not (and (= 1 (count head)) (<= (count body) 1) (= 1 (count separator))
                   (= (count (:children node)) (+ (count head) (count body) (count separator))))
      (invalid!))
    (let [header-rows (:children (first head)) rows (concat header-rows (:children (first body)))]
      (when-not (= 1 (count header-rows)) (invalid!))
      {:type "table"
       :content (mapv (fn [i row]
                        (when-not (= :TableRow (:kind row)) (invalid!))
                        {:type "tableRow"
                         :content (mapv (fn [cell]
                                          (when-not (and (= :TableCell (:kind cell)) (= 1 (:table-span cell))
                                                         (= (zero? i) (:table-header? cell))
                                                         (contains? #{nil :left :right :center} (:table-alignment cell)))
                                            (invalid!))
                                          {:type (if (zero? i) "tableHeader" "tableCell")
                                           :attrs {:colspan 1 :rowspan 1 :colwidth nil
                                                   :align (some-> (:table-alignment cell) name)}
                                           :content [{:type "paragraph" :content (vec (mapcat table-inline-model (:children cell)))}]})
                                        (:children row))})
                      (range) rows)})))

(defn- block-model [node]
  (let [inline #(vec (mapcat (fn [n] (inline-model n [])) (:children node)))
        children #(mapv block-model (:children node))]
    (case (:kind node)
      :Paragraph {:type "paragraph" :content (inline)}
      :Heading {:type "heading" :attrs {:level (:heading-level node)} :content (inline)}
      :BulletList {:type "bulletList" :content (children)}
      :OrderedList {:type "orderedList" :attrs {:start (:start node)} :content (children)}
      (:BulletListItem :OrderedListItem)
      ;; Markdown permits a bare list marker with no child nodes. Tiptap's
      ;; listItem schema requires a leading paragraph, including while Enter
      ;; has just created a new empty bullet. Represent that same empty item
      ;; canonically instead of losing its editor-required paragraph.
      {:type "listItem" :content (let [content (children)]
                                   (if (empty? content) [{:type "paragraph" :content []}] content))}
      (:FencedCodeBlock :IndentedCodeBlock)
      {:type "codeBlock" :attrs {:language (or (:language node) "")}
       :content (let [text (str/replace (:literal node) #"(?:\r\n|\n|\r)$" "")]
                  (if (seq text) [{:type "text" :text text}] []))}
      :ThematicBreak {:type "horizontalRule"}
      :TableBlock (table-model node)
      :DisplayMath {:type "displayMath"
                    :attrs {:sourceKey (str "math:" (:from node) ":" (:to node))
                            :raw (get-in node [:math :raw]) :tex (get-in node [:math :tex])}}
      (invalid!))))

(defn model
  "A supported region as inert Tiptap JSON, or nil (keep source-editable).
   Importing arbitrary HTML, tasks or complex tables/widgets never guesses a lossy codec."
  [node]
  (try (let [result {:type "doc" :content [(block-model node)]}]
         (markdown result) result)
       (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) _ nil)))

(defn apply-region
  "Replace just the selected syntax span. No-op models preserve exact source;
   changed regions retain their original trailing newlines and newline style."
  ([source region edited] (apply-region source region edited (:model region)))
  ([source {:keys [from to model]} edited provenance]
  (when-not (and (integer? from) (integer? to) (<= 0 from to (count source)) model) (invalid!))
  (validate-provenance! provenance edited)
  (if (= (normalize model) (normalize edited))
    source
    (let [before (subs source from to)
          ending (or (re-find #"[\r\n]+$" before) "")
          newline (or (re-find #"\r\n|\n|\r" before) "\n")
          replacement (markdown (normalize edited) newline)]
      (str (subs source 0 from) replacement ending (subs source to))))))
