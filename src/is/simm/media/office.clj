(ns is.simm.media.office
  "Faithful, structural editing of Office documents — the `office/*` SCI
   vocabulary. Companion to `is.simm.media.sheet` (spreadsheets as a live
   model) and `dvergr.media.doc` (a PDF as text).

   A .docx / .pptx / .odt is a ZIP of XML parts. The design follows from two
   findings we verified rather than assumed:

   1. **The SCI sandbox is a VALUE sandbox.** Its class allowlist is enforced
      at the METHOD level: a POI/odfdom object handed into a sandbox is inert —
      even `.getClass` is refused, so there is no reflection escape. We
      therefore CANNOT hand agents a live document object; parsing and
      serialising must happen HOST-side, and the agent receives DATA.

   2. **Re-serialising a whole document is lossy.** Apache POI rewrites 9 of 10
      parts on a no-op open+save — each rewritten part's fidelity then rests on
      POI's coverage of that file's features (tracked changes, content
      controls, custom XML). So we NEVER rebuild the container. We parse only
      the part the agent edits, re-emit only that part, and copy every other
      part as RAW BYTES. Untouched parts are byte-identical by construction;
      fidelity is a structural guarantee, not a hope.

   The agent is trained on OOXML/ODF XML, so we meet it there: a part opens as
   a plain-data tree with PREFIXED tags exactly as the agent knows them
   (`{:tag :w:t :attrs {...} :content [\"…\"]}`), editable with the
   `clojure.walk` / `clojure.zip` it already has. No bespoke document API to
   learn; no Java objects in the sandbox.

   Hardening is not optional on caller-supplied archives: DTDs are DISALLOWED
   (kills XXE and billion-laughs), and the unzip enforces entry-count, per-entry
   and total-uncompressed caps plus zip-slip path checks BEFORE anything is
   read into memory.

   The filesystem is injected (`sci-namespace`), identical contract to
   `is.simm.media.sheet`: :read-bytes / :stat / :write-file!. This ns knows
   nothing about drives, rooms or muschel."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.component :as component]
            [taoensso.telemere :as log])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]
           [java.util.zip ZipInputStream ZipOutputStream ZipEntry]
           [javax.xml.parsers SAXParserFactory]
           [org.xml.sax InputSource]
           [org.xml.sax.helpers DefaultHandler]))

;; =============================================================================
;; Limits — the "be willing to say no" tier
;; =============================================================================

;; The whole container in memory. Larger documents are a streamed/background
;; design we have not built; refuse with the size rather than OOM.
(def max-open-bytes (* 25 1024 1024))

(def ^:private format-mime
  {:docx "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
   :pptx "application/vnd.openxmlformats-officedocument.presentationml.presentation"
   :odt  "application/vnd.oasis.opendocument.text"
   :odp  "application/vnd.oasis.opendocument.presentation"
   :ods  "application/vnd.oasis.opendocument.spreadsheet"})

;; Zip-bomb ceilings, enforced while inflating (we do NOT trust the central
;; directory's declared sizes — they are a claim by the party we defend against).
(def max-entries 4000)
(def max-entry-uncompressed (* 60 1024 1024))
(def max-total-uncompressed (* 200 1024 1024))

;; A single XML part we will parse to data for the agent. document.xml for a
;; large doc can be big; past this we refuse the parse and point at text/parts.
(def max-part-parse-bytes (* 12 1024 1024))

;; Bounded text extraction — a preview, not the whole novel.
(def max-text-chars 40000)

;; Handles are server-side and hold the container bytes; cap them.
(def max-handles 16)
(def max-retained-bytes (* 200 1024 1024))

;; =============================================================================
;; Errors as data (same contract as sheet/*)
;; =============================================================================

(defn- fail!
  ([kind msg] (fail! kind msg {}))
  ([kind msg data] (throw (ex-info msg (assoc data :error kind)))))

(defmacro ^:private guarded
  "Every office/* fn returns DATA. A failure raised by `fail!` carries its own
   :error keyword and becomes {:error … :message …}; anything else is a bug and
   is logged and surfaced generically."
  [& body]
  `(try
     ~@body
     (catch clojure.lang.ExceptionInfo e#
       (if-let [k# (or (:error (ex-data e#))
                      (case (:type (ex-data e#))
                        :effect/denied :not-authorized
                        :is.simm.agents.source-memory/wrong-world :wrong-world
                        :is.simm.agents.source-memory/missing-binding :unavailable
                        :is.simm.agents.source-memory/missing-repository :unavailable
                        :is.simm.agents.source-memory/missing-proposal-workspace :unavailable
                        :is.simm.agents.source-memory/wrong-proposal-repository :unavailable
                        nil))]
         (assoc (dissoc (ex-data e#) :error) :error k# :message (ex-message e#))
         (do (log/log! {:level :error :id ::office-bug :error e#})
             {:error :office-error :message (ex-message e#)})))
     (catch Throwable e#
       (log/log! {:level :error :id ::office-bug :error e#})
       {:error :office-error :message (ex-message e#)})))

;; =============================================================================
;; Container — hardened read / write of the zip parts
;; =============================================================================

(defn- safe-entry-name
  "Reject zip-slip: absolute paths and any `..` segment. Returns the name or
   throws — a crafted entry name must never influence where bytes could land."
  [nm]
  (when (or (str/starts-with? nm "/")
            (str/starts-with? nm "\\")
            (some #{".."} (str/split nm #"[/\\]")))
    (fail! :unsafe-entry (str "refusing archive with an unsafe entry name: " (pr-str nm))))
  nm)

(defn read-parts
  "Inflate `bytes` into an ORDERED map {entry-name → ^bytes}, enforcing the
   zip-bomb caps and zip-slip checks against the actual inflated stream. Order
   is preserved (ODF requires `mimetype` first), so a caller writing the parts
   back keeps the original layout."
  ([bytes] (read-parts bytes {}))
  ([^bytes bytes limits]
  (let [entry-cap (min max-entries (get limits :max-entries max-entries))
        part-cap (min max-entry-uncompressed (get limits :max-entry-uncompressed max-entry-uncompressed))
        total-cap (min max-total-uncompressed (get limits :max-total-uncompressed max-total-uncompressed))]
  (when-not (and (every? #{:max-entries :max-entry-uncompressed :max-total-uncompressed} (keys limits))
                 (every? #(and (integer? %) (pos? %)) [entry-cap part-cap total-cap]))
    (fail! :invalid-limits "Archive limits must be positive and cannot raise host caps"))
  (with-open [zis (ZipInputStream. (ByteArrayInputStream. bytes))]
    (loop [acc (array-map) n 0 total 0]
      (if-let [e (.getNextEntry zis)]
        (do
          (when (>= n entry-cap)
            (fail! :too-many-entries (str "archive has more than " entry-cap " entries")))
          (if (.isDirectory e)
            (do (safe-entry-name (.getName e)) (recur acc (inc n) total))
            (let [nm (safe-entry-name (.getName e))
                  _ (when (contains? acc nm) (fail! :duplicate-entry "Archive repeats a part name"))
                  buf (byte-array 65536)
                  out (ByteArrayOutputStream.)]
              (loop [read 0]
                (let [c (.read zis buf)]
                  (cond
                    (neg? c) nil
                    (> (+ read c) part-cap)
                    (fail! :entry-too-large
                           (str "entry " nm " inflates past " part-cap " bytes"))
                    (> (+ total read c) total-cap)
                    (fail! :archive-too-large
                           (str "archive inflates past " total-cap " bytes"))
                    :else (do (.write out buf 0 c) (recur (+ read c))))))
              (recur (assoc acc nm (.toByteArray out)) (inc n) (+ total (.size out))))))
        acc))))))

(defn write-parts
  "Zip an ordered map {name → ^bytes} back into a container. `mimetype`, if
   present, is written FIRST and STORED (uncompressed) — the one ODF layout
   requirement a strict reader relies on. Everything else is deflated."
  ^bytes [parts]
  (let [out (ByteArrayOutputStream.)]
    (with-open [zos (ZipOutputStream. out)]
      (let [write-one
            (fn [nm ^bytes bs stored?]
              (let [e (ZipEntry. ^String nm)]
                (when stored?
                  (.setMethod e ZipEntry/STORED)
                  (.setSize e (alength bs))
                  (.setCompressedSize e (alength bs))
                  (let [crc (java.util.zip.CRC32.)]
                    (.update crc bs)
                    (.setCrc e (.getValue crc))))
                (.putNextEntry zos e)
                (.write zos bs)
                (.closeEntry zos)))]
        (when-let [mt (get parts "mimetype")]
          (.setMethod zos ZipOutputStream/DEFLATED)
          (write-one "mimetype" mt true))
        (.setMethod zos ZipOutputStream/DEFLATED)
        (doseq [[nm bs] parts :when (not= nm "mimetype")]
          (write-one nm bs false))))
    (.toByteArray out)))

;; =============================================================================
;; XML — hardened parse to plain data, and a flat emitter
;;
;; We keep tags PREFIXED (`:w:t`, `:a:p`, `:text:span`) — namespace-awareness is
;; OFF, so `xmlns:*` declarations survive as ordinary attributes and re-emit
;; unchanged. This is the representation the agent is trained on. We only ever
;; emit a part the agent explicitly edited; untouched parts are raw bytes, so
;; the emitter never has to be byte-perfect — only well-formed and reopenable.
;; =============================================================================

(defn- hardened-sax-parser
  "A SAXParser with DTDs DISALLOWED and external entities OFF — the XXE and
   entity-expansion gate. `disallow-doctype-decl` alone kills both attack
   classes; the external-entity features are belt-and-braces."
  []
  (let [f (SAXParserFactory/newInstance)]
    (.setNamespaceAware f false)
    (doto f
      (.setFeature "http://apache.org/xml/features/disallow-doctype-decl" true)
      (.setFeature "http://xml.org/sax/features/external-general-entities" false)
      (.setFeature "http://xml.org/sax/features/external-parameter-entities" false))
    (.newSAXParser f)))

(defn parse-xml
  "Parse an XML byte array into a plain-data tree:
     element → {:tag :w:t :attrs {:xml:space \"preserve\"} :content [children…]}
     text    → String (whitespace preserved verbatim)
   Comments and processing instructions are dropped in parts edited through
   this structural API. Untouched parts retain their raw bytes; this is not
   a source-preserving XML editor."
  [^bytes bs]
  (when (> (alength bs) max-part-parse-bytes)
    (fail! :part-too-large "XML part exceeds the parser byte limit"))
  (let [stack (java.util.ArrayDeque.)
        nodes (volatile! 0)
        root (volatile! nil)
        handler
        (proxy [DefaultHandler] []
          (startElement [_uri _local qname attrs]
            (when (or (>= (.size stack) 256) (> (vswap! nodes inc) 100000))
              (fail! :xml-too-complex "XML exceeds the bounded depth or element count"))
            (let [am (persistent!
                       (reduce (fn [m i]
                                 (assoc! m (keyword (.getQName attrs i)) (.getValue attrs i)))
                               (transient {})
                               (range (.getLength attrs))))
                  node {:tag (keyword qname) :attrs am :content (transient [])}]
              (.push stack node)))
          (endElement [_uri _local _qname]
            (let [node (.pop stack)
                  done (assoc node :content (persistent! (:content node)))]
              (if-let [parent (.peek stack)]
                (conj! (:content parent) done)
                (vreset! root done))))
          (characters [ch start length]
            (when-let [parent (.peek stack)]
              (conj! (:content parent) (String. ^chars ch (int start) (int length))))))]
    (.parse (hardened-sax-parser)
            (InputSource. (ByteArrayInputStream. bs))
            ^DefaultHandler handler)
    @root))

(defn- esc-text ^String [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")))

(defn- esc-attr ^String [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace "\"" "&quot;")))

(defn- emit-node! [^StringBuilder sb node]
  (cond
    (string? node) (.append sb (esc-text node))
    (nil? node) nil
    (map? node)
    (let [{:keys [tag attrs content]} node
          t (name tag)]
      (.append sb "<") (.append sb t)
      (doseq [[k v] attrs]
        (.append sb " ") (.append sb (name k))
        (.append sb "=\"") (.append sb (esc-attr v)) (.append sb "\""))
      (if (seq content)
        (do (.append sb ">")
            (doseq [c content] (emit-node! sb c))
            (.append sb "</") (.append sb t) (.append sb ">"))
        (.append sb "/>")))
    :else (fail! :bad-tree (str "not an XML node: " (pr-str node)))))

(defn- bounded-tree!
  "Bound caller-supplied trees before recursive emission or allocation."
  [tree]
  (loop [pending (list [tree 0]) nodes 0 chars 128]
    (when (> (* 6 chars) max-part-parse-bytes)
      (fail! :xml-too-complex "XML tree exceeds the bounded output size"))
    (when-let [[node depth] (first pending)]
      (when (or (> depth 256) (>= nodes 100000) (> (* 6 chars) max-part-parse-bytes))
        (fail! :xml-too-complex "XML tree exceeds the bounded depth, node count or output size"))
      (cond
        (nil? node) (recur (next pending) (inc nodes) chars)
        (string? node) (recur (next pending) (inc nodes) (+ chars (count node)))
        (map? node)
        (let [{:keys [tag attrs content]} node]
          (when-not (and (or (keyword? tag) (string? tag))
                         (or (nil? attrs) (map? attrs))
                         (or (nil? content) (sequential? content))
                         (<= (count attrs) 10000)
                         (every? (fn [[k v]] (and (or (keyword? k) (string? k)) (string? v))) attrs))
            (fail! :bad-tree "Expected an XML element with named string attributes and sequential content"))
          (recur (concat (map #(vector % (inc depth)) content) (next pending))
                 (inc nodes)
                 (+ chars (* 2 (count (name tag))) 5
                    (reduce + 0 (map (fn [[k v]] (+ (count (name k)) (count v) 4)) attrs)))))
        :else (fail! :bad-tree "Expected an XML element or text")))))

(defn emit-xml
  "Serialise a plain-data tree back to an XML byte array (UTF-8, flat — no
   reindentation, so `xml:space=\"preserve\"` text is untouched)."
  ^bytes [tree]
  (bounded-tree! tree)
  (let [sb (StringBuilder. "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>")]
    (emit-node! sb tree)
    (.getBytes (.toString sb) "UTF-8")))

;; =============================================================================
;; Formats & the parts that carry the body text
;; =============================================================================

(def ^:private format-table
  "Detected format → {:label … :main <primary body part> :text-parts <fn of parts→[names]>
                      :text-tags #{tags whose #content is visible text}
                      :para-tags #{tags that end a line}}."
  {:docx {:label "Word (OOXML)"
          :main "word/document.xml"
          :text-parts (fn [_] ["word/document.xml"])
          :text-tags #{:w:t}
          :para-tags #{:w:p}}
   :pptx {:label "PowerPoint (OOXML)"
          :main "ppt/slides/slide1.xml"
          :text-parts (fn [parts]
                        (->> (keys parts)
                             (filter #(re-matches #"ppt/slides/slide\d+\.xml" %))
                             (sort-by (fn [s] (Long/parseLong (re-find #"\d+" (subs s (count "ppt/slides/slide")))))) ))
          :text-tags #{:a:t}
          :para-tags #{:a:p}}
   :odt {:label "OpenDocument Text (ODF)"
         :main "content.xml"
         :text-parts (fn [_] ["content.xml"])
         :text-tags #{:text:p :text:span :text:h}
         :para-tags #{:text:p :text:h}}
   :odp {:label "OpenDocument Presentation (ODF)"
         :main "content.xml"
         :text-parts (fn [_] ["content.xml"])
         :text-tags #{:text:p :text:span}
         :para-tags #{:text:p}}
   :ods {:label "OpenDocument Spreadsheet (ODF)"
         :main "content.xml"
         :text-parts (fn [_] ["content.xml"])
         :text-tags #{:text:p}
         :para-tags #{:text:p}}})

(defn- detect-format
  "Which office format is this part set? nil if unrecognised."
  [parts]
  (let [mimetype (some-> (get parts "mimetype") (String. "UTF-8") str/trim)]
    (cond
      (= mimetype "application/vnd.oasis.opendocument.text") :odt
      (= mimetype "application/vnd.oasis.opendocument.presentation") :odp
      (= mimetype "application/vnd.oasis.opendocument.spreadsheet") :ods
      (contains? parts "word/document.xml") :docx
      (some #(re-matches #"ppt/slides/slide\d+\.xml" %) (keys parts)) :pptx
      (contains? parts "xl/workbook.xml") :xlsx
      :else nil)))

;; =============================================================================
;; Handle registry — server-side, so the container stays out of the context
;; =============================================================================

(def ^:dynamic ^:private *registry-id* nil)
(def ^:dynamic ^:private *scope* nil)

(defn- fork-registry [value _fork-id _directive] value)

(defn- registry []
  (let [entry (ec/get-state [:world/components *registry-id*])]
    (when-not entry (fail! :unavailable "Office staging is unavailable in this world"))
    (component/realization entry)))

(defn- retained-bytes [handles]
  (reduce + 0 (for [h (vals handles)
                    bs (concat (vals (:parts h)) (vals (:edits h)))]
                (alength ^bytes bs))))

(defn- update-registry! [f]
  (ec/swap-state!
   [:world/components *registry-id*]
   (fn [entry]
     (when-not entry (fail! :unavailable "Office staging is unavailable in this world"))
     (let [handles (f (component/realization entry))]
       (when (or (> (count handles) max-handles)
                 (> (retained-bytes handles) max-retained-bytes))
         (fail! :office-limit "Close an Office document before retaining more data"))
       (component/forkable handles fork-registry)))))

(defn- handle-id [h]
  (cond (string? h) h
        (map? h) (or (:handle h) (:id h))
        :else h))

(defn- get-handle [h]
  (let [id (handle-id h)]
    (or (let [entry (get (registry) id)] (when (= *scope* (:scope entry)) entry))
        (fail! :unknown-handle
               "No open document for this handle in the selected world"))))

(defn- new-id [] (str "office-" (java.util.UUID/randomUUID)))

(defn- commit-edits! [before edits]
  (update-registry!
   (fn [handles]
     (let [current (get handles (:id before))]
       (when-not (and current (= *scope* (:scope current))
                      (= (:generation before) (:generation current)))
         (fail! :stale-handle "Office staging changed; inspect the document again"))
       (assoc handles (:id before) (-> current (assoc :edits edits) (update :generation inc)))))))

;; =============================================================================
;; Text extraction — a bounded preview from the body parts
;; =============================================================================

(defn- collect-text
  "Walk a parsed tree, concatenating the #content of text-bearing tags and
   emitting a newline after each paragraph tag. Bounded by `max-text-chars`."
  [^StringBuilder sb tree text-tags para-tags]
  (when (< (.length sb) max-text-chars)
    (cond
      (string? tree) nil
      (map? tree)
      (let [{:keys [tag content]} tree]
        (when (contains? text-tags tag)
          (doseq [c content :when (string? c)] (.append sb c)))
        (doseq [c content :when (map? c)] (collect-text sb c text-tags para-tags))
        (when (contains? para-tags tag) (.append sb "\n"))))))

(defn- extract-text [format parts]
  (let [{:keys [text-parts text-tags para-tags]} (format-table format)
        sb (StringBuilder.)]
    (doseq [pname (text-parts parts)
            :when (and (get parts pname) (< (.length sb) max-text-chars))]
      (collect-text sb (parse-xml (get parts pname)) text-tags para-tags))
    (let [s (.toString sb)]
      (if (> (count s) max-text-chars)
        {:text (subs s 0 max-text-chars) :truncated? true}
        {:text s :truncated? false}))))

;; =============================================================================
;; Summaries — what `open` returns (and nothing more)
;; =============================================================================

(defn- summarize [id path format parts]
  (let [{:keys [label main]} (format-table format)]
    {:handle id
     :path path
     :format format
     :description label
     :parts (vec (keys parts))
     :main-part main
     :text-preview (let [{:keys [text truncated?]} (extract-text format parts)]
                     (cond-> (subs text 0 (min 500 (count text)))
                       truncated? (str " …")))}))

;; =============================================================================
;; Operations (bound into SCI by `sci-namespace`)
;; =============================================================================

(defn- current-bytes
  "The bytes of part `pname` accounting for any staged edit."
  [h pname]
  (or (get (:edits h) pname)
      (get (:parts h) pname)))

(defn- do-open [{:keys [read-bytes stat]} scope path]
  (let [size (:size (when stat (stat path)))]
    (when (and size (> (long size) max-open-bytes))
      (fail! :too-large
             (str path " is " size " bytes; office/open holds the whole file in memory"
                  " and caps at " max-open-bytes)
             {:size size :limit max-open-bytes}))
    (let [bs (read-bytes path)]
      (when (nil? bs) (fail! :not-found (str "no file at " path)))
      (when (> (count bs) max-open-bytes)
        (fail! :too-large (str path " is " (count bs) " bytes; caps at " max-open-bytes)))
      (let [parts (read-parts bs)
            format (detect-format parts)]
        (when (nil? format)
          (fail! :unknown-format
                 (str path " is not a recognised office document (docx/pptx/odt/odp/ods)")
                 {:parts (vec (take 20 (keys parts)))}))
        (when (= format :xlsx)
          (fail! :use-sheet
                 (str path " is a spreadsheet — use sheet/open, which models it as a"
                      " live grid rather than raw XML")))
        (let [id (new-id)
              h {:id id :path path :scope scope :format format
                 :parts parts :edits {} :generation 0}
              summary (summarize id path format parts)]
          (update-registry! #(assoc % id h))
          summary)))))

(defn- do-parts [h]
  (let [hh (get-handle h)]
    {:handle (:id hh)
     :parts (mapv (fn [[nm bs]]
                    {:name nm
                     :bytes (count bs)
                     :edited? (contains? (:edits hh) nm)})
                  (:parts hh))}))

(defn- do-text [h]
  (let [hh (get-handle h)]
    ;; text over the CURRENT bytes (edits included)
    (let [parts (reduce (fn [m [nm _]] (assoc m nm (current-bytes hh nm)))
                        (array-map) (:parts hh))]
      (assoc (extract-text (:format hh) parts) :handle (:id hh)))))

(defn- require-part [hh pname]
  (when-not (contains? (:parts hh) pname)
    (fail! :unknown-part
           (str (pr-str pname) " is not a part of this document")
           {:parts (vec (keys (:parts hh)))}))
  (let [bs (current-bytes hh pname)]
    (when (> (count bs) max-part-parse-bytes)
      (fail! :part-too-large
             (str pname " is " (count bs) " bytes; too large to parse to data"
                  " — use office/text for its content")
             {:size (count bs) :limit max-part-parse-bytes}))
    bs))

(defn- do-xml [h pname]
  (let [hh (get-handle h)]
    (parse-xml (require-part hh pname))))

(defn- do-set-xml! [h pname tree]
  (let [hh (get-handle h)]
    (require-part hh pname)                 ; existence + size gate
    ;; round-trip guard: the tree must emit to well-formed XML that re-parses.
    (let [bs (emit-xml tree)]
      (try (parse-xml bs)
           (catch Throwable _
             (fail! :malformed-tree
                    (str "the edited tree for " pname " did not serialise to well-formed XML"))))
      (commit-edits! hh (assoc (:edits hh) pname bs))
      {:handle (:id hh) :part pname :bytes (count bs) :staged? true})))

(defn- replace-in-tree
  "postwalk the tree replacing `old` with `new` inside the #content strings of
   text-bearing tags only. Returns [tree count]."
  [tree text-tags old new]
  (let [n (atom 0)]
    [(walk/postwalk
       (fn [node]
         (if (and (map? node) (contains? text-tags (:tag node)))
           (update node :content
                   (fn [content]
                     (mapv (fn [c]
                             (if (and (string? c) (str/includes? c old))
                               (do (swap! n + (count (re-seq (java.util.regex.Pattern/compile
                                                               (java.util.regex.Pattern/quote old)) c)))
                                   (str/replace c old new))
                               c))
                           content)))
           node))
       tree)
     @n]))

(defn- do-replace-text [h old new]
  (let [hh (get-handle h)]
    (when (str/blank? old) (fail! :bad-arg "office/replace-text needs a non-empty search string"))
    (let [{:keys [text-parts text-tags]} (format-table (:format hh))
          parts (text-parts (:parts hh))
          [edits total] (reduce (fn [[edits total] pname]
                                  (let [[tree n] (replace-in-tree (parse-xml (current-bytes hh pname)) text-tags old new)]
                                    [(if (pos? n) (assoc edits pname (emit-xml tree)) edits) (+ total n)]))
                                [(:edits hh) 0] (filter #(contains? (:parts hh) %) parts))]
      (when (pos? total) (commit-edits! hh edits))
      {:handle (:id hh) :replaced total
       :note (when (zero? total)
               (str "no visible occurrence of " (pr-str old) " — note it may be split"
                    " across runs; office/xml + a structural edit reaches those"))})))

(defn- do-save-as! [{:keys [write-file!]} h dest]
  (when-not write-file!
    (fail! :read-only "Office publication is unavailable in this filesystem; staged changes remain in this world"))
  (let [hh (get-handle h)]
    (when-not (str/ends-with? (str dest) (str "." (name (:format hh))))
      (fail! :bad-extension
             (str "save-as! must keep the format extension ." (name (:format hh))
                  " — got " dest)))
    (let [parts (reduce (fn [m [nm _]] (assoc m nm (current-bytes hh nm)))
                        (array-map) (:parts hh))
          bytes (write-parts parts)
          mime (get format-mime (:format hh) "application/octet-stream")
          node (write-file! dest bytes mime)]
      {:handle (:id hh) :saved dest :bytes (count bytes)
       :edited-parts (vec (keys (:edits hh)))
       :node (when (map? node) (select-keys node [:fs.node/id :fs.node/name]))})))

(defn- do-close [h]
  (let [hh (get-handle h)]
    (update-registry! #(dissoc % (:id hh)))
    {:handle (:id hh) :closed true}))

;; =============================================================================
;; The SCI vocabulary
;; =============================================================================

(defn sci-namespace
  "Build the `office/*` map for `sci/add-namespace!`.

   `fs` injects the filesystem (same contract as is.simm.media.sheet):
     :read-bytes  (fn [path] → bytes)          required
     :stat        (fn [path] → {:size n} | nil) optional — enables the size gate
     :write-file! (fn [path bytes mime] → node) optional — without it, save-as!
                                                is unavailable
   The two-argument constructor is for explicitly context-bound host adapters.
   Production adapters use owner/capability/with-environment!: each invocation
   resolves current authority and calls a synchronous callback with
   {:ctx selected :fs adapter :scope id}, inside its effect boundary.
   Handle data is immutable, private and fork-local; no global handles or TTL."
  ([fs scope]
   (let [owner (ec/current-execution-context)]
     (sci-namespace owner (random-uuid)
                    (fn [consume!] (consume! {:ctx (ec/current-execution-context) :fs fs :scope scope})))))
  ([owner capability-id with-environment!]
   (let [id [::handles capability-id]
         _ (binding [ec/*execution-context* owner]
             (when-not (contains? (component/registered) id)
               (component/register! id (component/forkable {} fork-registry))))
         invoke (fn [operation & args]
                  (guarded
                   (with-environment!
                    (fn [{:keys [ctx fs scope]}]
                     (when-not (and ctx fs scope)
                       (fail! :unavailable "Office invocation has no authorized environment"))
                     (binding [ec/*execution-context* ctx *registry-id* id *scope* scope]
                       (registry)
                       (case operation
                         :open (apply do-open fs scope args)
                         :save (apply do-save-as! fs args)
                         (apply operation args)))))))]
     {'open (partial invoke :open)
      'parts (partial invoke do-parts)
      'text (partial invoke do-text)
      'xml (partial invoke do-xml)
      'set-xml! (partial invoke do-set-xml!)
      'replace-text (partial invoke do-replace-text)
      'save-as! (partial invoke :save)
      'close (partial invoke do-close)})))

(def prompt-block
  "What the agent is told about `office/*`. Kept next to the code so the two
   cannot drift."
  (str "\n\nOFFICE DOCUMENTS (.docx .pptx .odt in the room source repository) are a ZIP of XML"
       " parts. `office/*` opens one HOST-side and hands you the XML as DATA;"
       " staging belongs to the current execution world. In clojure_eval:\n"
       "- `(def h (office/open \"documents/report.docx\"))` — handle + format,"
       " part list, and a text preview (never the raw bytes)\n"
       "- `(office/text h)` — the document's visible text (bounded)\n"
       "- `(office/replace-text h \"18%\" \"23%\")` — replace visible text within runs;"
       " returns how many it changed\n"
       "- `(office/xml h \"word/document.xml\")` — that part as a data tree with the"
       " tags you know (`{:tag :w:t :content [\"…\"]}`). Edit it with clojure.walk"
       " or clojure.zip, then:\n"
       "- `(office/set-xml! h \"word/document.xml\" edited-tree)` — stage the edited part\n"
       "Binary publication through office/save-as! and legacy /drive paths are"
       " currently unavailable; staged edits stay in this world. The human Memory"
       " XML-part editor retains its revision-bound save path. Do not claim a"
       " staged edit has been published. Changed parts lose XML comments and"
       " processing instructions; untouched parts retain their bytes.\n"
       "- `(office/close h)` when done.\n"
       "Spreadsheets (.xlsx) use sheet/*, not office/*."))
