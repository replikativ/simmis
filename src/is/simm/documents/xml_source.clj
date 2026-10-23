(ns is.simm.documents.xml-source
  "Lexical positions in already validated XML. This locator supplies no XML
   validity, namespace or entity interpretation; callers cross-check its element
   sequence against a hardened semantic parser before using any range."
  (:require [clojure.string :as str]
            [is.simm.documents.markdown :as markdown]))

(def max-source-chars (* 1024 1024))
(def max-elements 30000)

(defn- refuse! []
  (throw (ex-info "XML source positions cannot be established safely" {:type ::unsupported})))

(defn- after! [^String source delimiter from]
  (let [end (.indexOf source ^String delimiter (int from))]
    (when (neg? end) (refuse!))
    (+ end (count delimiter))))

(defn- tag-end! [^String source from]
  (loop [i from quote nil]
    (when (>= i (count source)) (refuse!))
    (let [c (.charAt source i)]
      (cond
        quote (recur (inc i) (when-not (= quote c) quote))
        (or (= c \') (= c \")) (recur (inc i) c)
        (= c \>) (inc i)
        :else (recur (inc i) nil)))))

(defn locate
  "Locate all elements in preorder, preserving UTF-16 string coordinates.
   A plain leaf has one explicit opening/closing pair and no internal markup.
   Input MUST already have passed bounded, namespace-aware XML validation."
  [source]
  (when-not (and (string? source) (<= (count source) max-source-chars)) (refuse!))
  (loop [offset 0 stack [] nodes []]
    (let [start (.indexOf ^String source "<" (int offset))]
      (if (neg? start)
        (do (when (seq stack) (refuse!)) nodes)
        (let [dirty-parent (fn [items]
                             (if-let [index (peek stack)] (assoc-in items [index :plain?] false) items))]
          (cond
            (.startsWith ^String source "<!--" start)
            (recur (after! source "-->" (+ start 4)) stack (dirty-parent nodes))

            (.startsWith ^String source "<?" start)
            (recur (after! source "?>" (+ start 2)) stack (dirty-parent nodes))

            (.startsWith ^String source "<![CDATA[" start)
            (recur (after! source "]]>" (+ start 9)) stack (dirty-parent nodes))

            (.startsWith ^String source "<!" start) (refuse!)

            :else
            (let [end (tag-end! source (inc start))
                  tag (subs source start end)
                  closing? (str/starts-with? tag "</")
                  qname (second (re-find (if closing? #"^</([^\s>]+)" #"^<([^\s/>]+)") tag))]
              (when-not qname (refuse!))
              (if closing?
                (let [index (peek stack)]
                  (when-not (and index (= qname (:qname (get nodes index)))) (refuse!))
                  (recur end (pop stack)
                         (assoc nodes index (assoc (get nodes index) :content-to start :end-to end))))
                (let [index (count nodes) empty? (boolean (re-find #"/\s*>$" tag))]
                  (when (or (>= index max-elements) (>= (count stack) 128)) (refuse!))
                  (recur end (if empty? stack (conj stack index))
                         (conj (dirty-parent nodes)
                               {:qname qname :tag-from start :tag-to end :content-from end
                                :content-to (when empty? end) :end-to (when empty? end)
                                :plain? (not empty?)})))))))))))

(defn checked-locations
  "Pair lexical ranges with semantic preorder nodes, refusing any disagreement.
   Semantic nodes must carry their original SAX :qname, not a guessed prefix."
  [source semantic-nodes]
  (let [locations (locate source)]
    (when-not (and (= (count locations) (count semantic-nodes))
                   (= (mapv :qname locations) (mapv :qname semantic-nodes)))
      (refuse!))
    locations))

(defn replace-content
  "Copy original UTF-8 bytes around one verified plain-leaf character span.
   Replacement is XML character data, never markup. Caller reparses the result
   and verifies the semantic change before publication."
  [source location replacement]
  (when-not (and (:plain? location) (string? replacement)
                 (<= (count replacement) 10000)
                 (not (re-find #"[\p{Cc}]" replacement)))
    (refuse!))
  (let [escaped (-> replacement (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;"))
        ;; Strict UTF-8 preserves the original source spelling, BOM and CRLF.
        ;; The two untouched slices are not parsed or serialized.
        before (markdown/source-bytes (subs source 0 (:content-from location)))
        middle (markdown/source-bytes escaped)
        after (markdown/source-bytes (subs source (:content-to location)))
        out (byte-array (+ (alength before) (alength middle) (alength after)))]
    (when (> (alength out) max-source-chars) (refuse!))
    (System/arraycopy before 0 out 0 (alength before))
    (System/arraycopy middle 0 out (alength before) (alength middle))
    (System/arraycopy after 0 out (+ (alength before) (alength middle)) (alength after))
    out))
