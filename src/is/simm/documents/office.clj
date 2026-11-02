(ns is.simm.documents.office
  "Stateless, revision-bound Office inspection and XML part editing. Never
   retain global document handles or rebuild untouched XML parts."
  (:require [clojure.string :as str]
            [hasch.core :as hasch]
            [is.simm.documents.files :as files]
            [is.simm.documents.docx :as docx]
            [is.simm.documents.markdown :as markdown]
            [is.simm.media.office :as office])
  (:import [java.util Base64]
           [java.io ByteArrayOutputStream]
           [java.util.zip ZipEntry ZipOutputStream]))

(def limits {:max-entries 1000 :max-entry-uncompressed (* 4 1024 1024)
             :max-total-uncompressed (* 24 1024 1024)})
(def max-xml-bytes (* 1024 1024))

;; XML relationship kinds understood by pinned docx-preview 0.4.1.
(def ^:private preview-xml-relations
  #{"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument"
    "http://schemas.openxmlformats.org/officeDocument/2006/relationships/fontTable"
    "http://schemas.openxmlformats.org/officeDocument/2006/relationships/numbering"
    "http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles"
    "http://schemas.microsoft.com/office/2007/relationships/stylesWithEffects"
    "http://schemas.openxmlformats.org/officeDocument/2006/relationships/theme"
    "http://schemas.openxmlformats.org/officeDocument/2006/relationships/settings"
    "http://schemas.openxmlformats.org/officeDocument/2006/relationships/webSettings"
    "http://schemas.openxmlformats.org/officeDocument/2006/relationships/footnotes"
    "http://schemas.openxmlformats.org/officeDocument/2006/relationships/endnotes"
    "http://schemas.openxmlformats.org/officeDocument/2006/relationships/footer"
    "http://schemas.openxmlformats.org/officeDocument/2006/relationships/header"
    "http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties"
    "http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties"
    "http://schemas.openxmlformats.org/package/2006/relationships/metadata/custom-properties"
    "http://schemas.openxmlformats.org/officeDocument/2006/relationships/comments"
    "http://schemas.microsoft.com/office/2011/relationships/commentsExtended"})

(defn- preview-package
  "Create a deterministic, preview-only ZIP from the validated inventory.
   Do not forward attacker-controlled central-directory metadata to JSZip."
  [parts]
  (let [out (ByteArrayOutputStream.)]
    (with-open [zip (ZipOutputStream. out)]
      (doseq [[name data] (sort-by key parts)]
        ;; JSZip normalizes slash paths. Refuse aliases rather than allowing
        ;; two validated parts to become one browser-visible part.
        (when (or (str/includes? name "\\")
                  (some #{"" "." ".."} (str/split name #"/" -1)))
          (throw (ex-info "DOCX preview contains an ambiguous part path" {:type ::unsupported})))
        (let [entry (doto (ZipEntry. ^String name) (.setTime 0))]
          (.putNextEntry zip entry)
          (.write zip ^bytes data)
          (.closeEntry zip))))
    (let [bytes (.toByteArray out)]
      (when (> (alength bytes) (* 8 1024 1024))
        (throw (ex-info "DOCX preview exceeds the byte limit" {:type ::limit})))
      bytes)))

(defn preview-bytes-in
  "Validate bounded DOCX input before sending it to an isolated browser
   renderer. All XML parts share the existing parser complexity/DTD limits.
   Returned bytes are a normalized preview copy. Canonical/download bytes are
   untouched; all part payloads are preserved."
  [db path expected]
  (files/path! path)
  (when-not (and (str/ends-with? (str/lower-case path) ".docx")
                 (= expected (files/entry db path)))
    (throw (ex-info "DOCX preview source changed or is unsupported" {:type ::conflict})))
  (let [bytes (files/source-bytes db path expected)
        parts (office/read-parts bytes limits)]
    ;; XML beyond document.xml (styles, numbering, notes, relationships, …)
    ;; also reaches the browser renderer. Bound every one, not just the body.
    (let [nodes (volatile! 0)]
      (doseq [[name data] parts
              :when (or (str/ends-with? (str/lower-case name) ".xml")
                        (str/ends-with? (str/lower-case name) ".rels"))]
        (let [tree (docx/xml-tree data)
              elements (tree-seq map? #(filter map? (:children %)) tree)
              n (count elements)]
          ;; The renderer dispatches XML by relationship type, not extension.
          ;; Refuse aliases outside the fully parsed .xml inventory.
          (when (str/ends-with? (str/lower-case name) ".rels")
            (doseq [node elements
                    :let [type (get-in node [:attrs ["" "Type"]])
                          target (get-in node [:attrs ["" "Target"]])]
                    :when (contains? preview-xml-relations type)]
              (when-not (and (string? target)
                             (str/ends-with? (str/lower-case target) ".xml"))
                (throw (ex-info "DOCX preview XML relationship needs an XML part path" {:type ::unsupported})))))
          (when (> (vswap! nodes + n) docx/max-xml-nodes)
            (throw (ex-info "DOCX package exceeds the preview complexity limit" {:type ::limit}))))))
    (when-not (= :ready (:status (docx/inspect-parts parts)))
      (throw (ex-info "This DOCX cannot be previewed within the document limits" {:type ::unsupported})))
    (preview-package parts)))

(defn- parts-in! [db path expected]
  (when-not (= :office (files/file-kind (files/path! path)))
    (throw (ex-info "Not a supported Office container" {:type ::unsupported})))
  (let [selected (files/entry db path)]
    (when-not (and selected (= expected selected))
      (throw (ex-info "Office file changed; reload before editing" {:type ::conflict})))
    (office/read-parts (files/source-bytes db path selected) limits)))

(defn- utf8-xml! [bytes]
  (office/parse-xml bytes)
  (when (or (and (>= (alength ^bytes bytes) 2)
                 (contains? #{[255 254] [254 255]}
                            (mapv #(bit-and 255 %) (take 2 bytes))))
            (some zero? bytes))
    (throw (ex-info "The advanced XML editor supports UTF-8 parts only" {:type ::encoding})))
  (let [text (markdown/decode bytes)
        encoding (nth (re-find #"\A\uFEFF?<\?xml\s[^?]*\bencoding\s*=\s*(['\"])([^'\"]+)\1" text) 2 nil)]
    (when (and encoding (not= "UTF-8" (str/upper-case encoding)))
      (throw (ex-info "The advanced XML editor supports UTF-8 parts only" {:type ::encoding})))
    text))

(defn- xml! [parts part]
  (when-not (and (string? part) (str/ends-with? (str/lower-case part) ".xml") (contains? parts part))
    (throw (ex-info "Select an existing XML part" {:type ::invalid})))
  (let [bytes (get parts part)]
    (when (> (alength ^bytes bytes) max-xml-bytes)
      (throw (ex-info "This XML part exceeds the interactive editor limit" {:type ::limit})))
    (utf8-xml! bytes)))

(defn- correction-tokens [parts path expected]
  (let [located (docx/correction-source parts)
        revision (markdown/revision (:source located))]
    (assoc located :tokens
           (into {} (map (fn [[index target]]
                           [index (hasch/uuid [:simmis/docx-text-correction-v1 path expected
                                              (:main-part located) revision index (:text target)])]))
                 (:targets located)))))

(defn- correction-view [parts path expected]
  (try
    (let [{:keys [tokens targets]} (correction-tokens parts path expected)]
      {:status :ready :tokens (into {} (filter (fn [[index _]]
                                               (let [text (get-in targets [index :text])]
                                                 (and (<= (count text) 10000) (not (re-find #"[\p{Cc}]" text)))))) tokens)})
    (catch Exception _ {:status :unsupported
                        :notice "Selective text correction is unavailable for this document. Advanced XML remains available."})))

(defn inspect-in
  "Inert Office projection from one explicit immutable DB; no authorization."
  [db path expected part]
  (let [parts (parts-in! db path expected)
        reading (when (str/ends-with? (str/lower-case path) ".docx")
                  (assoc (docx/inspect-parts parts) :corrections (correction-view parts path expected)))
        selected (or part (first (filter #(and (string? %) (str/ends-with? (str/lower-case %) ".xml") (contains? parts %))
                                         [(:main-part reading) "word/document.xml" "content.xml" "ppt/slides/slide1.xml" "xl/workbook.xml"])))
        xml (when selected
              (try {:text (xml! parts selected)}
                   (catch Exception e
                     (if part (throw e)
                         {:xml-error (if (or (= ::encoding (:type (ex-data e)))
                                             (instance? java.nio.charset.CharacterCodingException e))
                                       "The advanced XML editor supports UTF-8 parts only."
                                       "The default XML part cannot be opened safely in the advanced editor.")}))))]
    (merge {:path path :entry expected :part selected :reading reading
            :parts (mapv (fn [[name bytes]] {:name name :size (alength ^bytes bytes)
                                             :editable? (and (str/ends-with? (str/lower-case name) ".xml")
                                                             (<= (alength ^bytes bytes) max-xml-bytes))}) (sort-by key parts))
            :editing-scope :xml-part :execution-policy :source-only} xml)))

(defn inspect! [room path expected part]
  (files/with-workspace room #(inspect-in @% path expected part)))

(defn prepare-save-in
  "Validate a closed XML-part edit against one frozen DB and return an ordinary
   file-save request. Does not write, authorize, route or invoke a callback.
   An unchanged edit retains the exact original ZIP bytes."
  [db {:keys [path expected part text] :as request}]
  (when-not (= #{:path :expected :part :text} (set (keys request)))
    (throw (ex-info "Invalid Office edit" {:type ::invalid})))
  (let [parts (parts-in! db path expected) before (xml! parts part)
        bytes (markdown/source-bytes text)]
    (when (> (alength ^bytes bytes) max-xml-bytes)
      (throw (ex-info "XML edit exceeds the interactive editor limit" {:type ::limit})))
    ;; Validate, but save the submitted XML bytes, not a lossy re-emission.
    (utf8-xml! bytes)
    (let [container (if (= before text)
                      ;; Exact no-op: don't even recompress the original ZIP.
                      (:base64 (files/read-binary-in db path expected))
                      (.encodeToString (Base64/getEncoder) (office/write-parts (assoc parts part bytes))))]
      {:path path :expected expected :base64 container})))

(defn save-part! [room request options]
  (let [prepared (files/with-workspace room #(prepare-save-in @% request))]
    (files/save-file! room prepared options)))

(defn prepare-correction-in
  "Reconstruct one revision-bound text token and prepare an ordinary binary
   save. The caller retains its original owned binding through publication."
  [db {:keys [path expected token replacement] :as request}]
  (when-not (and (= #{:path :expected :token :replacement} (set (keys request))) (uuid? token))
    (throw (ex-info "Invalid Word text correction" {:type ::invalid})))
  (let [parts (parts-in! db path expected)
        located (correction-tokens parts path expected)
        index (some (fn [[index actual]] (when (= token actual) index)) (:tokens located))]
    (when-not index (throw (ex-info "The selected text belongs to another saved version" {:type ::conflict})))
    (let [{:keys [changed? main-part bytes]} (docx/correct-text parts index replacement)]
      {:path path :expected expected
       :base64 (if changed?
                 (.encodeToString (Base64/getEncoder) (office/write-parts (assoc parts main-part bytes)))
                 (:base64 (files/read-binary-in db path expected)))})))
