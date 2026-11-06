(ns is.simm.benchmarks.matter-evidence
  "Bounded, inert evidence projection for explicitly adapted Office benchmarks.
   No filesystem, model, formula evaluation or application/room state."
  (:require [clojure.string :as str]
            [is.simm.documents.docx :as docx]
            [is.simm.media.office :as office])
  (:import [java.net URI]
           [java.security MessageDigest]
           [java.nio.charset StandardCharsets]))

(def policy {:version 1 :representation :decoded-xml-evidence
             :max-input-bytes 1048576 :max-output-bytes 524288
             :max-entries 1000 :max-entry-uncompressed 4194304 :max-total-uncompressed 25165824
             :max-records 10000 :formula-values :cached-not-recalculated
             :layout :not-reproduced :page-fields :cached-footer-values-marked
             :native-office-reproduction false})
(def word-ns "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
(def sheet-ns "http://schemas.openxmlformats.org/spreadsheetml/2006/main")
(def rel-ns "http://schemas.openxmlformats.org/package/2006/relationships")
(def office-rel-ns "http://schemas.openxmlformats.org/officeDocument/2006/relationships")
(defn- refuse! [reason] (throw (ex-info "Unsupported benchmark evidence projection" {:type ::unsupported :reason reason})))
(defn sha256 [^bytes bytes]
  (.formatHex (java.util.HexFormat/of) (.digest (MessageDigest/getInstance "SHA-256") bytes)))
(defn- nodes [root]
  (tree-seq map? #(filter map? (:children %)) root))
(defn- named? [uri local node] (and (= uri (:ns node)) (= local (:name node))))
(defn- children [node uri local] (filter #(named? uri local %) (:children node)))
(defn- strings [node]
  (apply str (mapcat #(filter string? (:children %)) (nodes node))))
(defn- attr [node name] (get-in node [:attrs ["" name]]))
(defn- xml! [parts part]
  (if-let [bytes (get parts part)] (docx/xml-tree bytes) (refuse! :missing-part)))
(defn- word-text [node]
  (apply str
         (keep (fn [n]
                 (when (= word-ns (:ns n))
                   (case (:name n) "t" (apply str (filter string? (:children n)))
                         "tab" "\t" "br" "\n" "cr" "\n"
                         "noBreakHyphen" "‑" "softHyphen" "­" nil))) (nodes node))))
(defn- word-records [part tree]
  (when (some #(and (= word-ns (:ns %))
                   (contains? #{"ins" "del" "moveFrom" "moveTo" "fldSimple" "altChunk" "drawing" "object" "vanish" "strike" "dstrike"} (:name %))) (nodes tree))
    (refuse! :review-field-or-embedded-content))
  (let [codes (map (comp str/trim strings) (filter (partial named? word-ns "instrText") (nodes tree)))]
    (when (and (seq codes)
               (or (not (re-matches #"word/footer[0-9]+\.xml" part))
                   (some #(not (contains? #{"PAGE" "NUMPAGES"} %)) codes)))
      (refuse! :unsupported-field)))
  (letfn [(walk [node context]
            (let [context (if (= word-ns (:ns node))
                            (case (:name node) "tbl" (assoc context :table (:source-index node))
                                  "tr" (assoc context :row (:source-index node))
                                  "tc" (assoc context :cell (:source-index node)) context) context)]
              (if (named? word-ns "p" node)
                [(let [codes (mapv (comp str/trim strings) (filter (partial named? word-ns "instrText") (nodes node)))]
                   (cond-> (merge context {:id (str "xml-" (:source-index node)) :part part :text (word-text node)})
                     (seq codes) (assoc :cached-page-fields codes)))]
                (mapcat #(walk % context) (filter map? (:children node))))))]
    (vec (walk tree {}))))

(defn- internal-part [base target]
  (when-not (and (string? target) (<= 1 (count target) 512)
                 (not (re-find #"[\\%]" target))) (refuse! :relationship-target))
  (let [uri (.normalize (.resolve (URI. (str "/" base)) (URI. target)))
        path (.getPath uri)]
    (when-not (and (nil? (.getScheme uri)) (nil? (.getAuthority uri))
                   (nil? (.getQuery uri)) (nil? (.getFragment uri))
                   (str/starts-with? path "/xl/")) (refuse! :relationship-target))
    (subs path 1)))
(defn- cell-text [cell shared]
  (let [type (attr cell "t") raw (some-> (first (children cell sheet-ns "v")) strings)]
    (case type
      "inlineStr" (apply str (map strings (filter #(named? sheet-ns "t" %) (nodes cell))))
      "s" (do (when-not (and raw (re-matches #"[0-9]{1,8}" raw)) (refuse! :shared-string-index))
              (let [i (Long/parseLong raw)]
                (when-not (< i (count shared)) (refuse! :shared-string-index)) (nth shared i)))
      (or raw ""))))
(defn- spreadsheet [parts]
  (let [book (xml! parts "xl/workbook.xml")
        relationships (into {} (for [node (children (xml! parts "xl/_rels/workbook.xml.rels") rel-ns "Relationship")]
                                 [(attr node "Id") node]))
        shared (if (contains? parts "xl/sharedStrings.xml")
                 (mapv #(apply str (map strings (filter (partial named? sheet-ns "t") (nodes %))))
                       (children (xml! parts "xl/sharedStrings.xml") sheet-ns "si")) [])
        sheets (vec (filter (partial named? sheet-ns "sheet") (nodes book)))
        _ (when (or (empty? sheets) (> (count sheets) 100)
                    (not= (count sheets) (count (set (map #(attr % "name") sheets))))) (refuse! :sheet-list))
        styles (when (contains? parts "xl/styles.xml") (xml! parts "xl/styles.xml"))
        formats {:custom (mapv #(select-keys % [:name :attrs]) (children (first (children styles sheet-ns "numFmts")) sheet-ns "numFmt"))
                 :cells (mapv #(select-keys % [:name :attrs]) (children (first (children styles sheet-ns "cellXfs")) sheet-ns "xf"))}]
    {:date-1904? (= "1" (some-> (first (children book sheet-ns "workbookPr")) (attr "date1904")))
     :number-formats formats
     :sheets
     (mapv (fn [sheet]
             (let [relationship (get relationships (get-in sheet [:attrs [office-rel-ns "id"]]))
                   _ (when-not (and relationship (not= "External" (attr relationship "TargetMode"))
                                    (str/ends-with? (or (attr relationship "Type") "") "/worksheet")) (refuse! :sheet-relationship))
                   part (internal-part "xl/workbook.xml" (attr relationship "Target"))
                   tree (xml! parts part)
                   _ (when (some #(or (and (= sheet-ns (:ns %))
                                          (contains? #{"mergeCell" "drawing" "legacyDrawing" "oleObject" "tablePart"} (:name %)))
                                     (and (contains? #{"row" "col"} (:name %))
                                          (contains? #{"1" "true"} (attr % "hidden")))) (nodes tree))
                       (refuse! :unsupported-sheet-content))
                   cells (mapv (fn [cell]
                                 (let [ref (attr cell "r")]
                                   (when-not (and ref (re-matches #"[A-Z]{1,3}[1-9][0-9]{0,6}" ref)) (refuse! :cell-reference))
                                   (cond-> {:id ref :part part :type (or (attr cell "t") "n") :text (cell-text cell shared)}
                                     (attr cell "s") (assoc :style-index (attr cell "s"))
                                     (seq (children cell sheet-ns "f")) (assoc :formula (strings (first (children cell sheet-ns "f")))))))
                               (filter (partial named? sheet-ns "c") (nodes tree)))]
               (when-not (= (count cells) (count (set (map :id cells)))) (refuse! :duplicate-cell))
               {:name (attr sheet "name") :state (or (attr sheet "state") "visible") :part part :cells cells})) sheets)}))

(defn project
  "Return source-bound decoded paragraph/table or sheet/cell evidence plus the
   exact selected XML strings. Quotes address these decoded records, not pages.
   Native formatting/artifact handling is deliberately outside this projection."
  [filename ^bytes bytes]
  (when-not (and (string? filename) (<= 1 (count filename) 256) (re-matches #"[A-Za-z0-9_.-]+" filename)
                 bytes (<= (alength bytes) (:max-input-bytes policy))) (refuse! :input-bound))
  (let [parts (office/read-parts bytes (select-keys policy [:max-entries :max-entry-uncompressed :max-total-uncompressed]))
        _ (when (some #(re-find #"^xl/(?:comments|threadedComments)" %) (keys parts))
            (refuse! :unsupported-sheet-comments))
        format (cond (str/ends-with? filename ".docx") :docx (str/ends-with? filename ".xlsx") :xlsx :else (refuse! :format))
        selected (into (sorted-map)
                       (filter (fn [[p _]]
                                 (if (= format :docx)
                                   (boolean (re-matches #"word/(?:document|header[0-9]+|footer[0-9]+|footnotes|endnotes|comments)\.xml" p))
                                   (or (contains? #{"xl/workbook.xml" "xl/_rels/workbook.xml.rels" "xl/sharedStrings.xml" "xl/styles.xml"} p)
                                       (boolean (re-matches #"xl/(?:worksheets|comments|threadedComments)/[^/]+\.xml" p)))))) parts)
        _ (when (and (= format :docx) (not (contains? selected "word/document.xml"))) (refuse! :main-part))
        _ (when (and (= format :docx) (not (named? word-ns "document" (xml! parts "word/document.xml")))) (refuse! :main-part))
        projection (if (= format :docx)
                     {:paragraphs (vec (mapcat (fn [[p _]] (word-records p (xml! parts p))) selected))}
                     (spreadsheet parts))
        records (if (= format :docx) (count (:paragraphs projection)) (reduce + 0 (map #(count (:cells %)) (:sheets projection))))
        _ (when (> records (:max-records policy)) (refuse! :record-bound))
        result (merge {:version 1 :source filename :sha256 (sha256 bytes) :format format :policy policy
                       :excluded-parts (vec (sort (remove (set (keys selected)) (keys parts))))} projection)]
    (when (> (alength (.getBytes (pr-str result) StandardCharsets/UTF_8)) (:max-output-bytes policy)) (refuse! :output-bound))
    {:evidence result
     :xml-parts (into (sorted-map) (map (fn [[p b]] [p (String. ^bytes b StandardCharsets/UTF_8)])) selected)}))
