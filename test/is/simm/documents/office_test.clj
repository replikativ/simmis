(ns is.simm.documents.office-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.room.store.memory :as memory]
            [is.simm.documents.files :as files]
            [is.simm.documents.office :as documents]
            [is.simm.media.office :as office])
  (:import [java.util Base64 Arrays]))

(defn- encoded [bytes] (.encodeToString (Base64/getEncoder) bytes))
(defn- bytes-of [s] (.getBytes ^String s "UTF-8"))

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
