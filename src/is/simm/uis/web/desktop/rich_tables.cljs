(ns is.simm.uis.web.desktop.rich-tables
  "Native tables in the existing editor/undo model. No separate cell state."
  (:require [clojure.string :as str]
            ["@tiptap/extension-table" :refer [Table TableRow TableCell TableHeader]]
            ["@tiptap/pm/tables" :refer [CellSelection isInTable selectedRect addRowAfter]]
            [is.simm.documents.rich-text :as rich]))

(defn restore-selection [^js doc native]
  (let [anchor (aget native "anchor") head (aget native "head")
        cell? (fn [p] (and (integer? p) (<= 0 p (.. doc -content -size))
                           (contains? #{"tableCell" "tableHeader"} (some-> (.nodeAt doc p) .-type .-name))))]
    (when (and (cell? anchor) (cell? head)
               (= (.start (.resolve doc anchor) -1) (.start (.resolve doc head) -1)))
      (.create CellSelection doc anchor head))))

(defn- add-aligned-row []
  (fn [^js props]
    (let [state (.-state props) dispatch (.-dispatch props) ^js tr (.-tr props)]
      (if-not (isInTable state) false
        (let [rect (selectedRect state) table (.-table rect)
              row-index (.-bottom rect) table-start (.-tableStart rect)
              aligns (mapv #(aget (.-attrs (.child (.child table 0) %)) "align")
                           (range (.. table -firstChild -childCount)))]
          (if (>= (.-childCount table) rich/max-table-rows) false
            ;; Chainable state exposes the same transaction to the upstream
            ;; command. Alignment joins it BEFORE the one final dispatch.
            (let [added? (addRowAfter state (when dispatch (fn [_] nil)))]
              (when (and added? dispatch)
                (let [^js changed-doc (.-doc tr) ^js table (.nodeAt changed-doc (dec table-start))
                      ^js row (.child table row-index)
                      row-start (+ table-start (reduce + 0 (map (fn [i] (let [^js child (.child table i)] (.-nodeSize child))) (range row-index))))]
                  (loop [i 0 pos (inc row-start)]
                    (when (< i (.-childCount row))
                      (let [^js cell (.child row i)]
                        (.setNodeMarkup tr pos nil (js/Object.assign #js {} (.-attrs cell) #js {:align (nth aligns i)}))
                        (recur (inc i) (+ pos (.-nodeSize cell))))))))
              added?)))))))

(defn table-extension []
  (.configure
   (.extend Table
            #js {:addCommands (fn []
                                (this-as this
                                  (let [parent (aget this "parent") commands (if parent (.call parent this) #js {})]
                                    (aset commands "addRowAfter" add-aligned-row)
                                    commands)))})
   #js {:resizable false :HTMLAttributes #js {:class "memory-data-table"}}))

(defn row-extension [] TableRow)
(defn cell-extension [] (.extend TableCell #js {:content "paragraph"}))
(defn header-extension [] (.extend TableHeader #js {:content "paragraph"}))

(def ^:private table-tags #{"TABLE" "THEAD" "TBODY" "TR" "TH" "TD" "COLGROUP" "COL"
                           "P" "STRONG" "B" "EM" "I" "S" "DEL" "STRIKE" "CODE" "A" "SPAN"})

(defn- allowed-attributes? [el]
  (let [tag (.-tagName el)
        allowed (case tag "TABLE" #{"class" "style"} "COL" #{"style"}
                      ("TD" "TH") #{"colspan" "rowspan" "style" "align"}
                      "P" #{"data-pm-slice"}
                      "A" #{"href" "title" "target" "rel"} #{})
        allowed (if (= tag "TABLE") (conj allowed "data-pm-slice") allowed)
        style (.-style el)]
    (and (or (not (.hasAttribute el "data-pm-slice"))
             ;; Native rectangle/paragraph slices only; never reconstruct
             ;; semantic wrappers or source identities from clipboard context.
             (boolean (re-matches #"[0-3] [0-3](?: -[12])? \[\]" (.getAttribute el "data-pm-slice"))))
         (every? #(contains? allowed (.-name %)) (array-seq (.-attributes el)))
         (or (not (.hasAttribute el "class")) (= "memory-data-table" (.getAttribute el "class")))
         (or (not (.hasAttribute el "align"))
             (and (contains? #{"left" "right" "center"} (.getAttribute el "align"))
                  (or (str/blank? (.-textAlign style)) (= (.getAttribute el "align") (.-textAlign style)))))
         (or (not= "A" tag) (rich/safe-link? (.getAttribute el "href")))
         (every? (fn [i]
                   (let [key (.item style i) value (str/trim (.getPropertyValue style key))]
                     (case key
                       "text-align" (and (contains? #{"TH" "TD"} tag) (contains? #{"left" "right" "center"} value))
                       "min-width" (and (contains? #{"TABLE" "COL"} tag) (boolean (re-matches #"[0-9]+(?:\.[0-9]+)?px" value)))
                       false)))
                 (range (.-length style))))))

(defn- supported-table-html? [table inside?]
  (let [rows (vec (array-seq (.-rows table))) width (some-> (first rows) .-cells .-length)]
    (and (nil? (some-> (.-parentElement table) (.closest "table")))
         (<= 1 (count rows) rich/max-table-rows) (number? width) (<= 1 width rich/max-table-columns)
         (every? (fn [el] (and (contains? table-tags (.-tagName el)) (allowed-attributes? el)))
                 (cons table (array-seq (.querySelectorAll table "*"))))
         (every? (fn [[i row]]
                   (and (= width (.. row -cells -length))
                        (every? (fn [cell]
                                  (let [paragraphs (.querySelectorAll cell "p")]
                                    (and (= 1 (.-colSpan cell)) (= 1 (.-rowSpan cell))
                                         (or inside? (= (if (zero? i) "TH" "TD") (.-tagName cell)))
                                         (<= (.-length paragraphs) 1)
                                         (or (zero? (.-length paragraphs))
                                             (every? #(or (identical? % (.item paragraphs 0))
                                                          (and (= 3 (.-nodeType %)) (str/blank? (.-nodeValue %))))
                                                     (array-seq (.-childNodes cell))))
                                         (every? #(not (re-find #"[\r\n]" (.-textContent %)))
                                                 (array-seq (.querySelectorAll cell "code"))))))
                                (array-seq (.-cells row)))))
                 (map-indexed vector rows)))))

(defn- only-tables? [fragment]
  (every? #(or (= "TABLE" (.-nodeName %))
                (and (= 3 (.-nodeType %)) (str/blank? (.-nodeValue %))))
          (array-seq (.-childNodes fragment))))

(defn unsupported-paste?
  "Inspect original clipboard data before native parsing/fitting can discard
   unsupported content. Non-table HTML inside a cell needs the same proof."
  [view event]
  (when-let [clipboard (.-clipboardData event)]
    (let [html (.getData clipboard "text/html") text (.getData clipboard "text/plain")
          inside? (isInTable (.-state view)) table? (boolean (re-find #"(?i)<table(?:\s|>)" html))]
      (cond
        (and (or table? inside?) (> (count html) 2097152)) true
        table?
        (let [template (.createElement js/document "template")]
          (set! (.-innerHTML template) html)
          (let [fragment (.-content template) tables (array-seq (.querySelectorAll fragment "table"))]
            (not (and (seq tables) (or (not inside?) (= 1 (count tables))) (only-tables? fragment)
                      (every? #(supported-table-html? % inside?) tables)))))
        (and inside? (not (str/blank? html)))
        (let [template (.createElement js/document "template")
              table (.createElement js/document "table")
              row (.insertRow table) cell (.insertCell row)]
          (set! (.-innerHTML template) html)
          (.appendChild cell (.-content template))
          (not (supported-table-html? table true)))
        :else (and inside? (boolean (re-find #"[\r\n\t]" text)))))))

(defn blocked! [el reason]
  (.dispatchEvent el (js/CustomEvent. "input" #js {:bubbles true :composed true
                                                  :detail #js {:blocked true :reason reason}})))

(defn handle-key! [el view event]
  ;; GFM cells contain a single paragraph. Intercept Enter before a browser
  ;; fallback can introduce a line/paragraph that the file cannot preserve.
  (when (and (= "Enter" (.-key event)) (.-editable view) (isInTable (.-state view)))
    (.preventDefault event)
    (blocked! el "Table cells use one paragraph. Use Tab and Shift+Tab to move between cells; Tab in the last cell adds a row.")
    true))
