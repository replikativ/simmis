(ns is.simm.documents.docx
  "Bounded, inert DOCX reading projection. Never serializes a package or
   resolves external relationships, document styles, fields or resources."
  (:require [clojure.string :as str])
  (:import [java.io ByteArrayInputStream]
           [java.net URI]
           [javax.xml XMLConstants]
           [javax.xml.parsers SAXParserFactory]
           [org.xml.sax InputSource]
           [org.xml.sax.helpers DefaultHandler]))

(def max-xml-bytes (* 1024 1024))
(def max-xml-nodes 30000)
(def max-output-nodes 5000)
(def max-text-chars 100000)

(def ^:private package-ns "http://schemas.openxmlformats.org/package/2006/relationships")
(def ^:private content-ns "http://schemas.openxmlformats.org/package/2006/content-types")
(def ^:private word-namespaces #{"http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                                 "http://purl.oclc.org/ooxml/wordprocessingml/main"})
(def ^:private document-relations #{"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument"
                                    "http://purl.oclc.org/ooxml/officeDocument/relationships/officeDocument"})
(def ^:private main-content-type "application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml")

(defn- refuse! [kind message]
  (throw (ex-info message {:type kind})))

(defn- xml-tree
  "Streaming limits apply before retaining additional tree nodes. The XML
   tokenizer itself is also bounded by the byte input cap and JAXP hardening."
  [^bytes bytes]
  (when-not (and bytes (<= (alength bytes) max-xml-bytes))
    (refuse! ::limit "An XML part exceeds the document reading limit."))
  (let [factory (doto (SAXParserFactory/newInstance)
                  (.setNamespaceAware true)
                  (.setXIncludeAware false)
                  (.setFeature XMLConstants/FEATURE_SECURE_PROCESSING true)
                  (.setFeature "http://apache.org/xml/features/disallow-doctype-decl" true)
                  (.setFeature "http://xml.org/sax/features/external-general-entities" false)
                  (.setFeature "http://xml.org/sax/features/external-parameter-entities" false)
                  (.setFeature "http://apache.org/xml/features/nonvalidating/load-external-dtd" false))
        parser (.newSAXParser factory)
        _ (.setProperty parser XMLConstants/ACCESS_EXTERNAL_DTD "")
        _ (.setProperty parser XMLConstants/ACCESS_EXTERNAL_SCHEMA "")
        stack (java.util.ArrayDeque.) root (volatile! nil)
        nodes (volatile! 0) chars (volatile! 0)
        count! (fn [n]
                 (when (or (> (vswap! nodes inc) max-xml-nodes)
                           (> (vswap! chars + n) max-xml-bytes))
                   (refuse! ::limit "Document XML exceeds the reading complexity limit.")))
        handler (proxy [DefaultHandler] []
                  (startElement [uri local _qname attrs]
                    (when (or (>= (.size stack) 128) (> (.getLength attrs) 128))
                      (refuse! ::limit "Document XML exceeds the reading depth or attribute limit."))
                    (count! (+ (count uri) (count local)))
                    (let [attributes
                          (into {} (for [i (range (.getLength attrs))
                                         :let [value (.getValue attrs i)
                                               uri (.getURI attrs i) name (.getLocalName attrs i)]]
                                     (do (count! (+ (count uri) (count name) (count value)))
                                         [[uri name] value])))]
                      (.push stack {:ns uri :name local :attrs attributes :children (transient [])})))
                  (endElement [_uri _local _qname]
                    (let [node (.pop stack) node (update node :children persistent!)]
                      (if-let [parent (.peek stack)]
                        (conj! (:children parent) node)
                        (vreset! root node))))
                  (characters [ch start length]
                    (when-let [parent (.peek stack)]
                      (count! length)
                      (conj! (:children parent) (String. ^chars ch (int start) (int length))))))]
    (.parse parser (InputSource. (ByteArrayInputStream. bytes)) ^DefaultHandler handler)
    @root))

(defn- element? [node uri name]
  (and (map? node) (= uri (:ns node)) (= name (:name node))))

(defn- elements [node]
  (filter map? (:children node)))

(defn- attr [node name]
  (get (:attrs node) ["" name]))

(defn- package-path! [target]
  (when-not (and (string? target) (<= 1 (count target) 512) (not (str/includes? target "%")))
    (refuse! ::package "The main document relationship has no supported internal target."))
  (let [uri (URI. target) path (.getRawPath uri)
        path (when path (if (str/starts-with? path "/") (subs path 1) path))]
    (when-not (and (not (.isAbsolute uri)) (nil? (.getRawAuthority uri))
                   (nil? (.getRawQuery uri)) (nil? (.getRawFragment uri))
                   (seq path) (not (re-find #"[\\\p{Cntrl}]" path))
                   (every? #(and (not (str/blank? %)) (not (#{"." ".."} %))) (str/split path #"/" -1)))
      (refuse! ::package "The main document relationship is not an unambiguous internal part."))
    path))

(defn- main-part! [parts]
  (let [relationships (xml-tree (get parts "_rels/.rels"))
        types (xml-tree (get parts "[Content_Types].xml"))]
    (when-not (and (element? relationships package-ns "Relationships")
                   (element? types content-ns "Types"))
      (refuse! ::package "The file has no supported DOCX package declarations."))
    (let [main (filter #(and (element? % package-ns "Relationship")
                             (contains? document-relations (attr % "Type"))) (elements relationships))]
      (when-not (= 1 (count main))
        (refuse! ::package "The package must identify exactly one main document."))
      (let [relationship (first main)
            _ (when-not (contains? #{nil "Internal"} (attr relationship "TargetMode"))
                (refuse! ::package "An external main document is not loaded."))
            path (package-path! (attr relationship "Target"))
            overrides (filter #(and (element? % content-ns "Override")
                                    (= path (package-path! (attr % "PartName")))) (elements types))]
        (when-not (and (= 1 (count overrides)) (= main-content-type (attr (first overrides) "ContentType"))
                       (contains? parts path))
          (refuse! ::package "The main part has no unambiguous supported Word document content type."))
        path))))

(defn- word? [node name]
  (and (map? node) (contains? word-namespaces (:ns node)) (= name (:name node))))

(defn- child [node name]
  (first (filter #(word? % name) (elements node))))

(defn- word-attr [node name]
  (get (:attrs node) [(:ns node) name]))

(defn- on? [node]
  (and node (contains? #{nil "1" "true" "on"} (word-attr node "val"))))

(defn- emit! [state value]
  (when (> (vswap! (:nodes state) inc) max-output-nodes)
    (refuse! ::limit "The document exceeds the simplified reading node limit."))
  value)

(defn- text! [state text]
  (when (> (vswap! (:chars state) + (count text)) max-text-chars)
    (refuse! ::limit "The document exceeds the simplified reading text limit."))
  text)

(defn- warning! [state text]
  (when (< (count @(:warnings state)) 32)
    (vswap! (:warnings state) conj text)))

(defn- placeholder [state node id]
  (let [name (or (:name node) "content")
        name (subs name 0 (.offsetByCodePoints ^String name 0 (min 80 (.codePointCount ^String name 0 (count name)))))
        label (str "Unsupported document content: " name)]
    (warning! state label)
    (emit! state {:kind :placeholder :id id :text label})))

(defn- run [state node id]
  (let [properties (child node "rPr")
        underline (child properties "u")
        marks {:bold (boolean (on? (child properties "b")))
               :italic (boolean (on? (child properties "i")))
               :strike (boolean (on? (child properties "strike")))
               :underline (boolean (and underline (not (contains? #{"none" "0" "false" "off"}
                                                                  (word-attr underline "val")))))}]
    (if (on? (child properties "vanish"))
      [(placeholder state (assoc node :name "hidden text") id)]
      (into []
            (keep (fn [[index item]]
                    (cond
                      (word? item "rPr") nil
                      (word? item "t")
                      (if (every? string? (:children item))
                        (emit! state {:kind :text :id (conj id index) :marks marks
                                      :text (text! state (apply str (:children item)))})
                        (placeholder state item (conj id index)))
                      (or (word? item "br") (word? item "cr"))
                      (emit! state {:kind :text :id (conj id index) :marks marks :text (text! state "\n")})
                      (word? item "tab")
                      (emit! state {:kind :text :id (conj id index) :marks marks :text (text! state "\t")})
                      (word? item "noBreakHyphen")
                      (emit! state {:kind :text :id (conj id index) :marks marks :text (text! state "\u2011")})
                      (word? item "softHyphen")
                      (emit! state {:kind :text :id (conj id index) :marks marks :text (text! state "\u00ad")})
                      :else (placeholder state item (conj id index)))))
            (map-indexed vector (elements node))))))

(defn- paragraph [state node id]
  (let [properties (child node "pPr")
        outline (some-> (child properties "outlineLvl") (word-attr "val"))
        heading (when (and outline (re-matches #"[0-5]" outline)) (inc (Long/parseLong outline)))
        numbering (child properties "numPr")
        number-id (some-> (child numbering "numId") (word-attr "val"))
        list? (boolean (and number-id (re-matches #"[+]?[0-9]{1,10}" number-id)
                            (pos? (Long/parseLong number-id))))
        _ (when list? (warning! state "List numbering and indentation are not rendered."))
        runs (into []
                   (mapcat (fn [[index item]]
                             (cond
                               (word? item "r") (run state item (conj id index))
                               (some #(word? item %) ["pPr" "bookmarkStart" "bookmarkEnd" "proofErr"]) []
                               :else [(placeholder state item (conj id index))])))
                   (map-indexed vector (elements node)))]
    (emit! state {:kind :paragraph :id id :heading heading :list? list? :runs runs})))

(defn- field-affected!
  "Complex field scopes can span paragraphs or tables. Suppress every block
   touched by one, rather than presenting its cached result as ordinary text."
  [state node]
  (let [before @(:field-depth state)
        field? (volatile! (pos? before))]
    (doseq [item (tree-seq map? elements node)]
      (when (some #(word? item %) ["fldSimple" "instrText" "delInstrText"])
        (vreset! field? true))
      (when (word? item "fldChar")
        (vreset! field? true)
        (case (word-attr item "fldCharType")
          "begin" (when (> (vswap! (:field-depth state) inc) 128)
                    (refuse! ::limit "Document fields exceed the reading nesting limit."))
          "end" (when (neg? (vswap! (:field-depth state) dec))
                  (refuse! ::package "The document has an unmatched field ending."))
          "separate" (when (zero? @(:field-depth state))
                       (refuse! ::package "The document has an unmatched field separator."))
          (refuse! ::package "The document has an unsupported field marker."))))
    @field?))

(defn- review-content? [node]
  (some (fn [item]
          (and (contains? word-namespaces (:ns item))
               (or (contains? #{"ins" "del" "moveFrom" "moveTo" "moveFromRangeStart" "moveFromRangeEnd"
                                "moveToRangeStart" "moveToRangeEnd" "cellDel" "cellIns" "cellMerge"}
                              (:name item))
                   (str/ends-with? (:name item) "Change"))))
        (tree-seq map? elements node)))

(defn- table [state node id]
  (let [rows (filter #(word? % "tr") (elements node))]
    (if (or (empty? rows) (> (count rows) 200)
            (> (count (set (map #(count (filter (fn [item] (word? item "tc")) (elements %))) rows))) 1)
            (some (fn [row]
                    (let [cells (filter #(word? % "tc") (elements row))]
                      (or (> (count cells) 64)
                          (some #(not (some (partial word? %) ["trPr" "tc"])) (elements row))
                          (child (child row "trPr") "gridBefore")
                          (child (child row "trPr") "gridAfter")
                          (some (fn [cell]
                                  (let [properties (child cell "tcPr")]
                                    (or (when-let [span (child properties "gridSpan")] (not= "1" (word-attr span "val")))
                                        (child properties "vMerge")
                                        (child properties "hMerge") (child cell "tbl")))) cells)))) rows)
            (some #(not (some (partial word? %) ["tblPr" "tblGrid" "tr"])) (elements node)))
      (placeholder state (assoc node :name "merged, nested or complex table") id)
      (emit! state
             {:kind :table :id id
              :rows (mapv
                     (fn [ri row]
                       (emit! state
                              {:id (conj id ri)
                               :header? (boolean (on? (child (child row "trPr") "tblHeader")))
                               :cells (mapv
                                       (fn [ci cell]
                                         (emit! state
                                                {:id (conj id ri ci)
                                                 :blocks (into []
                                                               (keep (fn [[pi item]]
                                                                       (cond
                                                                         (word? item "p") (paragraph state item (conj id ri ci pi))
                                                                         (word? item "tcPr") nil
                                                                         :else (placeholder state item (conj id ri ci pi)))))
                                                               (map-indexed vector (elements cell)))}))
                                       (range) (filter #(word? % "tc") (elements row)))}))
                     (range) rows)}))))

(defn inspect-parts
  "Read-only DTO from already bounded archive parts. No connection, filesystem,
   callback, HTML, style sheet or package mutation is involved."
  [parts]
  (try
    (let [path (main-part! parts) document (xml-tree (get parts path))
          _ (when-not (word? document "document")
              (refuse! ::package "The main part is not a supported WordprocessingML document."))
          bodies (filter #(word? % "body") (elements document))
          _ (when-not (= 1 (count bodies))
              (refuse! ::package "The main document must have exactly one body."))
          state {:nodes (volatile! 0) :chars (volatile! 0) :warnings (volatile! (sorted-set)) :field-depth (volatile! 0)}
          blocks (into []
                       (keep (fn [[index node]]
                               (cond
                                 (field-affected! state node)
                                 (placeholder state (assoc node :name "field-containing content (cached result not shown)") [index])
                                 (review-content? node)
                                 (placeholder state (assoc node :name "tracked or structural review content") [index])
                                 (word? node "p") (paragraph state node [index])
                                 (word? node "tbl") (table state node [index])
                                 (word? node "sectPr") nil
                                 :else (placeholder state node [index]))))
                       (map-indexed vector (elements (first bodies))))
          _ (when-not (zero? @(:field-depth state))
              (refuse! ::package "The document has an unfinished field scope."))]
      {:status :ready :main-part path :blocks blocks
       :layout :simplified :diagnostics (vec @(:warnings state))
       :notice "Simplified saved document. Page layout, style inheritance, headers, footers and embedded objects are not reproduced."})
    (catch clojure.lang.ExceptionInfo e
      {:status :unsupported :diagnostics [(ex-message e)]})
    (catch Exception _
      {:status :unsupported :diagnostics ["Document XML or package relationships could not be read safely."]})))
