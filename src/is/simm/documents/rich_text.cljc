(ns is.simm.documents.rich-text
  "A bounded, deliberately restricted rich-text Markdown adapter. It operates
   on ONE selected source region; unrelated file bytes are never serialized."
  (:require [clojure.string :as str]))

(defn- invalid! []
  (throw (ex-info "This rich-text structure needs Source editing" {:type ::unsupported})))

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
      (let [text (:text node) chars (+ chars (if (string? text) (count text) 0))]
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
      (str/replace #"[\\`*_{}\[\]<>#.!|~+()\-]" #(str "\\" %))))

(defn- code-span [s]
  (let [s (str/replace s #"\r\n|\r|\n" " ")
        n (inc (reduce max 0 (map count (re-seq #"`+" s))))
        fence (apply str (repeat n "`"))
        padding? (or (str/starts-with? s "`") (str/ends-with? s "`")
                     (and (str/starts-with? s " ") (str/ends-with? s " ")
                          (not (re-matches #" +" s))))]
    (str fence (when padding? " ") s (when padding? " ") fence)))

(declare emit)

(defn- delimited [marker s]
  (let [[_ before body after] (re-matches #"(?s)(\s*)(.*?)(\s*)" s)]
    (if (seq body) (str before marker body marker after) s)))

(defn- inline [node depth budget]
  (when (or (> depth 100) (neg? (vswap! budget dec))) (invalid!))
  (case (:type node)
    "hardBreak" "  \n"
    "text" (let [text (:text node) marks (:marks node)
                 _ (when-not (string? text) (invalid!))
                 code? (some #(= "code" (:type %)) marks)
                 initial (if code? (code-span text) (escaped text))]
             (reduce (fn [s mark]
                       (case (:type mark)
                         "code" s
                         "bold" (delimited "**" s)
                         "italic" (delimited "*" s)
                         "strike" (delimited "~~" s)
                         "link" (let [url (get-in mark [:attrs :href]) title (get-in mark [:attrs :title])]
                                  (when-not (safe-link? url) (invalid!))
                                  (str "[" s "](<" url ">"
                                       (when (seq title)
                                         (str " \"" (str/replace title #"[\\\"]" #(str "\\" %)) "\"")) ")"))
                         (invalid!))) initial marks))
    (invalid!)))

(defn- inlines [node depth budget]
  (apply str (map #(inline % (inc depth) budget) (:content node))))

(defn- emit [node depth budget]
  (when (or (> depth 100) (neg? (vswap! budget dec))) (invalid!))
  (case (:type node)
    "doc" (str/join "\n\n" (map #(emit % (inc depth) budget) (:content node)))
    "paragraph" (inlines node depth budget)
    "heading" (let [level (get-in node [:attrs :level])]
                (when-not (and (integer? level) (<= 1 level 6)) (invalid!))
                (str (apply str (repeat level "#")) " " (inlines node depth budget)))
    "horizontalRule" "---"
    "codeBlock" (let [language (or (get-in node [:attrs :language]) "")
                      _ (when-not (and (string? language) (re-matches #"[a-zA-Z0-9_+.-]{0,40}" language)) (invalid!))
                      text (apply str (map (fn [n] (when-not (= "text" (:type n)) (invalid!)) (:text n)) (:content node)))
                      text (str/replace text #"\r\n|\r" "\n")
                      n (max 3 (inc (reduce max 0 (map count (re-seq #"`+" text)))))
                      fence (apply str (repeat n "`"))]
                  (str fence language "\n" text "\n" fence))
    ("bulletList" "orderedList")
    (let [start (get-in node [:attrs :start] 1)
          _ (when-not (and (integer? start) (<= 0 start 999999999)) (invalid!))]
      (str/join
       "\n"
       (map-indexed
        (fn [index item]
          (when-not (= "listItem" (:type item)) (invalid!))
          (let [marker (if (= "bulletList" (:type node)) "- " (str (+ start index) ". "))
                body (str/join "\n\n" (map #(emit % (inc depth) budget) (:content item)))
                [first-line & more] (str/split body #"\n" -1)
                padding (apply str (repeat (count marker) " "))]
            (str marker first-line
                 (when (seq more) (str "\n" (str/join "\n" (map #(str padding %) more)))))))
        (:content node))))
    (invalid!)))

(defn markdown [document]
  (let [result (emit (validate-model! document) 0 (volatile! 20000))]
    (when (> (count result) 2097152) (invalid!))
    result))

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
                              (conj out n)))) [] (:content node))]
    (if (= type "text")
      {:type type :text (:text node)
       :marks (vec (sort-by :type
                           (map (fn [mark]
                                  (cond-> {:type (:type mark)}
                                    (= "link" (:type mark))
                                    (assoc :attrs {:href (get-in mark [:attrs :href])
                                                   :title (not-empty (get-in mark [:attrs :title]))}))) (:marks node))))}
      (cond-> {:type type :content content}
        (= type "heading") (assoc :attrs {:level (get-in node [:attrs :level])})
        (= type "orderedList") (assoc :attrs {:start (get-in node [:attrs :start] 1)})
        (= type "codeBlock") (assoc :attrs {:language (or (get-in node [:attrs :language]) "")})))))

(defn normalize [node] (normalize* (validate-model! node)))

(defn- inline-model [node marks]
  (let [children #(vec (mapcat (fn [n] (inline-model n %)) (:children node)))]
    (case (:kind node)
      :Text (if (seq (:text node)) [(cond-> {:type "text" :text (:text node)}
                                    (seq marks) (assoc :marks marks))] [])
      :SoftLineBreak [(cond-> {:type "text" :text " "} (seq marks) (assoc :marks marks))]
      :HardLineBreak [{:type "hardBreak"}]
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

(defn- block-model [node]
  (let [inline #(vec (mapcat (fn [n] (inline-model n [])) (:children node)))
        children #(mapv block-model (:children node))]
    (case (:kind node)
      :Paragraph {:type "paragraph" :content (inline)}
      :Heading {:type "heading" :attrs {:level (:heading-level node)} :content (inline)}
      :BulletList {:type "bulletList" :content (children)}
      :OrderedList {:type "orderedList" :attrs {:start (:start node)} :content (children)}
      (:BulletListItem :OrderedListItem) {:type "listItem" :content (children)}
      (:FencedCodeBlock :IndentedCodeBlock)
      {:type "codeBlock" :attrs {:language (or (:language node) "")}
       :content (let [text (str/replace (:literal node) #"(?:\r\n|\n|\r)$" "")]
                  (if (seq text) [{:type "text" :text text}] []))}
      :ThematicBreak {:type "horizontalRule"}
      (invalid!))))

(defn model
  "A supported region as inert Tiptap JSON, or nil (keep source-editable).
   Importing arbitrary HTML, tasks/tables/widgets never guesses a lossy codec."
  [node]
  (try (let [result {:type "doc" :content [(block-model node)]}]
         (markdown result) result)
       (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) _ nil)))

(defn apply-region
  "Replace just the selected syntax span. No-op models preserve exact source;
   changed regions retain their original trailing newlines and newline style."
  [source {:keys [from to model]} edited]
  (when-not (and (integer? from) (integer? to) (<= 0 from to (count source)) model) (invalid!))
  (if (= (normalize model) (normalize edited))
    source
    (let [before (subs source from to)
          ending (or (re-find #"[\r\n]+$" before) "")
          newline (or (re-find #"\r\n|\n|\r" before) "\n")
          replacement (-> (markdown (normalize edited)) (str/replace #"\n" newline))]
      (str (subs source 0 from) replacement ending (subs source to)))))
