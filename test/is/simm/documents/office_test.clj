(ns is.simm.documents.office-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.room.store.memory :as memory]
            [is.simm.documents.files :as files]
            [is.simm.documents.office :as documents]
            [is.simm.documents.docx-test :as docx-fixtures]
            [is.simm.media.office :as office])
  (:import [java.util Base64 Arrays]
           [java.io ByteArrayInputStream]
           [org.apache.poi.xwpf.usermodel XWPFDocument]))

(defn- encoded [bytes] (.encodeToString (Base64/getEncoder) bytes))
(defn- bytes-of [s] (.getBytes ^String s "UTF-8"))

(deftest whole-text-corrections-use-current-package-tokens-and-retain-noop-bytes
  (let [room (discourse/make-room {:id (keyword (str "word-correction-" (random-uuid))) :store (memory/make)})
        parts (-> (docx-fixtures/package-parts "<w:p><w:r><w:t> A &amp; B </w:t><w:t>Second</w:t></w:r></w:p>")
                  (assoc "custom/untouched.bin" (bytes-of "opaque"))
                  (update "[Content_Types].xml"
                          #(bytes-of (str/replace (String. ^bytes % "UTF-8") "</Types>"
                                                 (str "<Default Extension='rels' ContentType='application/vnd.openxmlformats-package.relationships+xml'/>"
                                                      "<Default Extension='bin' ContentType='application/octet-stream'/></Types>")))))
        original (office/write-parts parts)]
    (try
      (workspace/ensure-workspace! room)
      (let [saved (files/save-file! room {:path "correct.docx" :expected nil :base64 (encoded original)})
            conn (files/with-workspace room identity)
            db @conn opened (documents/inspect-in db "correct.docx" (:entry saved) nil)
            tokens (get-in opened [:reading :corrections :tokens])
            [index token] (first (sort-by key tokens))
            request {:path "correct.docx" :expected (:entry saved) :token token :replacement " A & B "}
            noop (documents/prepare-correction-in db request)]
        (is (integer? index))
        (is (uuid? token))
        (is (= (encoded original) (:base64 noop)))
        (is (thrown? clojure.lang.ExceptionInfo (documents/prepare-correction-in db (assoc request :token (random-uuid)))))
        (is (thrown? clojure.lang.ExceptionInfo (documents/prepare-correction-in db (assoc request :offset 0))))
        (let [prepared (documents/prepare-correction-in db (assoc request :replacement "Corrected <text> &amp; 🦋"))
              next (files/save-file! room prepared)
              binary (files/read-binary room "correct.docx" (:entry next))
              updated (office/read-parts (.decode (Base64/getDecoder) ^String (:base64 binary)))]
          (with-open [word (XWPFDocument. (ByteArrayInputStream. (.decode (Base64/getDecoder) ^String (:base64 binary))))]
            (is (= "Corrected <text> &amp; 🦋Second" (.getText (first (.getParagraphs word))))))
          (doseq [[part bytes] parts :when (not= part "word/document.xml")]
            (is (Arrays/equals ^bytes bytes ^bytes (get updated part))))
          (is (thrown? clojure.lang.ExceptionInfo (documents/prepare-correction-in @conn request)))
          (is (thrown? clojure.lang.ExceptionInfo
                       (documents/prepare-correction-in @conn (assoc request :expected (:entry next)))))))
      (finally (discourse/close-room! room)))))

(deftest office-editing-is-revision-bound-and-preserves-untouched-part-bytes
  (let [room (discourse/make-room {:id (keyword (str "office-editor-" (random-uuid))) :store (memory/make)})
        xml "<?xml version=\"1.0\"?><w:document xmlns:w=\"urn:test\"><!-- keep --><w:t>Hello</w:t></w:document>"
        untouched (bytes-of "opaque metadata and signatures")
        original (office/write-parts (array-map "word/document.xml" (bytes-of xml) "custom/data.bin" untouched))]
    (try
      (workspace/ensure-workspace! room)
      (let [saved (files/save-file! room {:path "report.docx" :expected nil :base64 (encoded original)})
            opened (documents/inspect! room "report.docx" (:entry saved) nil)]
        (is (= xml (:text opened)))
        (is (= :xml-part (:editing-scope opened)))
        (is (nil? (:commit (documents/save-part! room {:path "report.docx" :expected (:entry saved)
                                                      :part (:part opened) :text xml} {}))))
        (let [next (documents/save-part! room {:path "report.docx" :expected (:entry saved)
                                               :part (:part opened) :text (.replace xml "Hello" "Updated")} {})
              binary (files/read-binary room "report.docx" (:entry next))
              parts (office/read-parts (.decode (Base64/getDecoder) ^String (:base64 binary)))]
          (is (Arrays/equals untouched ^bytes (get parts "custom/data.bin")))
          (is (= (.replace xml "Hello" "Updated") (String. ^bytes (get parts "word/document.xml") "UTF-8")))
          (is (thrown? clojure.lang.ExceptionInfo (documents/save-part! room {:path "report.docx" :expected (:entry saved)
                                                                            :part (:part opened) :text xml} {})))
          (is (thrown? org.xml.sax.SAXParseException (documents/save-part! room {:path "report.docx" :expected (:entry next)
                                                                            :part (:part opened) :text "<!DOCTYPE x><x/>"} {})))))
      (finally (discourse/close-room! room)))))

(deftest interactive-office-caps-are-per-call-and-cannot-be-raised
  (let [container (office/write-parts {"part.xml" (bytes-of "<x>longer than four bytes</x>")})]
    (is (thrown? clojure.lang.ExceptionInfo (office/read-parts container {:max-entry-uncompressed 4})))
    (is (= 1 (count (office/read-parts container))))))

(deftest document-reading-is-independent-of-advanced-xml-availability
  (doseq [[name xml charset expected-status]
          [["utf16.docx" (str "<?xml version=\"1.0\" encoding=\"UTF-16\"?><w:document xmlns:w=\"" docx-fixtures/word-ns
                              "\"><w:body><w:p><w:r><w:t>Readable Unicode</w:t></w:r></w:p></w:body></w:document>") "UTF-16" :ready]
           ["latin-declaration.docx" (str "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><w:document xmlns:w=\"" docx-fixtures/word-ns
                                          "\"><w:body><w:p/></w:body></w:document>") "UTF-8" :ready]
           ["malformed.docx" "<broken" "UTF-8" :unsupported]
           ["oversized.docx" (str "<x>" (apply str (repeat documents/max-xml-bytes "x")) "</x>") "UTF-8" :unsupported]]]
    (let [room (discourse/make-room {:id (keyword (str "docx-reading-" (random-uuid))) :store (memory/make)})
          parts (assoc (docx-fixtures/package-parts "<w:p/>") "word/document.xml" (.getBytes ^String xml ^String charset))
          original (office/write-parts parts)]
      (try
        (workspace/ensure-workspace! room)
        (let [saved (files/save-file! room {:path name :expected nil :base64 (encoded original)})
              opened (documents/inspect! room name (:entry saved) nil)]
          (is (= expected-status (get-in opened [:reading :status])) (pr-str opened))
          (is (string? (:xml-error opened)))
          (is (nil? (:text opened)))
          (is (= (encoded original) (:base64 (files/read-binary room name (:entry saved)))))
          (is (thrown? Exception (documents/inspect! room name (:entry saved) "word/document.xml"))))
        (finally (discourse/close-room! room))))))

(deftest nonconventional-main-parts-remain-readable-without-fabricating-an-xml-buffer
  (let [room (discourse/make-room {:id (keyword (str "docx-main-part-" (random-uuid))) :store (memory/make)})
        original (office/write-parts (docx-fixtures/package-parts "<w:p><w:r><w:t>Custom main</w:t></w:r></w:p>"
                                                               {:main "custom/main.data"}))]
    (try
      (workspace/ensure-workspace! room)
      (let [saved (files/save-file! room {:path "custom.docx" :expected nil :base64 (encoded original)})
            opened (documents/inspect! room "custom.docx" (:entry saved) nil)]
        (is (= :ready (get-in opened [:reading :status])))
        (is (= "custom/main.data" (get-in opened [:reading :main-part])))
        (is (nil? (:text opened)))
        (is (nil? (:part opened))))
      (finally (discourse/close-room! room)))))
