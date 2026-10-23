(ns is.simm.documents.office
  "Stateless, revision-bound Office inspection and XML part editing. Never
   retain global document handles or rebuild untouched XML parts."
  (:require [clojure.string :as str]
            [hasch.core :as hasch]
            [is.simm.documents.files :as files]
            [is.simm.documents.docx :as docx]
            [is.simm.documents.markdown :as markdown]
            [is.simm.media.office :as office])
  (:import [java.util Base64]))

(def limits {:max-entries 1000 :max-entry-uncompressed (* 4 1024 1024)
             :max-total-uncompressed (* 24 1024 1024)})
(def max-xml-bytes (* 1024 1024))

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
