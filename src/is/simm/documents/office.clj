(ns is.simm.documents.office
  "Stateless, revision-bound Office inspection and XML part editing. Never
   retain global document handles or rebuild untouched XML parts."
  (:require [clojure.string :as str]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.media.office :as office])
  (:import [java.util Base64]))

(def limits {:max-entries 1000 :max-entry-uncompressed (* 4 1024 1024)
             :max-total-uncompressed (* 24 1024 1024)})
(def max-xml-bytes (* 1024 1024))

(defn- parts! [room path expected]
  (when-not (= :office (files/file-kind (files/path! path)))
    (throw (ex-info "Not a supported Office container" {:type ::unsupported})))
  (files/with-workspace room
    (fn [conn]
      (let [db @conn selected (files/entry db path)]
        (when-not (and selected (= expected selected))
          (throw (ex-info "Office file changed; reload before editing" {:type ::conflict})))
        (office/read-parts (files/source-bytes db path selected) limits)))))

(defn- xml! [parts part]
  (when-not (and (string? part) (str/ends-with? (str/lower-case part) ".xml") (contains? parts part))
    (throw (ex-info "Select an existing XML part" {:type ::invalid})))
  (let [bytes (get parts part)]
    (when (> (alength ^bytes bytes) max-xml-bytes)
      (throw (ex-info "This XML part exceeds the interactive editor limit" {:type ::limit})))
    (office/parse-xml bytes)
    (markdown/decode bytes)))

(defn inspect! [room path expected part]
  (let [parts (parts! room path expected)
        selected (or part (first (filter #(contains? parts %)
                                        ["word/document.xml" "content.xml" "ppt/slides/slide1.xml" "xl/workbook.xml"])))
        text (when selected (xml! parts selected))]
    {:path path :entry expected :part selected :text text
     :parts (mapv (fn [[name bytes]] {:name name :size (alength ^bytes bytes)
                                     :editable? (and (str/ends-with? (str/lower-case name) ".xml")
                                                      (<= (alength ^bytes bytes) max-xml-bytes))}) (sort-by key parts))
     :editing-scope :xml-part :execution-policy :source-only}))

(defn save-part! [room {:keys [path expected part text] :as request} options]
  (when-not (= #{:path :expected :part :text} (set (keys request)))
    (throw (ex-info "Invalid Office edit" {:type ::invalid})))
  (let [parts (parts! room path expected) before (xml! parts part)
        bytes (markdown/source-bytes text)]
    (when (> (alength ^bytes bytes) max-xml-bytes)
      (throw (ex-info "XML edit exceeds the interactive editor limit" {:type ::limit})))
    ;; Validate, but save the submitted XML bytes, not a lossy re-emission.
    (office/parse-xml bytes)
    (let [container (if (= before text)
                      ;; Exact no-op: don't even recompress the original ZIP.
                      (:base64 (files/read-binary room path expected))
                      (.encodeToString (Base64/getEncoder) (office/write-parts (assoc parts part bytes))))]
      (files/save-file! room {:path path :expected expected :base64 container} options))))
