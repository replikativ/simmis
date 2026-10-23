(ns is.simm.documents.docx-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [is.simm.documents.docx :as docx]
            [is.simm.media.office :as office])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]
           [java.util Arrays]
           [org.apache.poi.xwpf.usermodel XWPFDocument]))

(def word-ns "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
(def strict-ns "http://purl.oclc.org/ooxml/wordprocessingml/main")
(defn bytes-of [text] (.getBytes ^String text "UTF-8"))

(defn package-parts
  ([body] (package-parts body {}))
  ([body {:keys [main namespace prefix mode target type]
          :or {main "word/document.xml" namespace word-ns prefix "w"
               type "application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"}}]
   {"_rels/.rels" (bytes-of (str "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">"
                                 "<Relationship Id=\"main\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\""
                                 " Target=\"" (or target main) "\"" (when mode (str " TargetMode=\"" mode "\"")) "/></Relationships>"))
    "[Content_Types].xml" (bytes-of (str "<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">"
                                         "<Override PartName=\"/" main "\" ContentType=\"" type "\"/></Types>"))
    main (bytes-of (str "<" prefix ":document xmlns:" prefix "=\"" namespace "\"><" prefix ":body>"
                        body "</" prefix ":body></" prefix ":document>"))}))

(defn- text-of [reading]
  (apply str (for [block (:blocks reading) run (:runs block) :when (= :text (:kind run))] (:text run))))

(deftest actual-word-corpus-is-read-without-changing-its-bytes
  (let [output (ByteArrayOutputStream.)]
    (with-open [word (XWPFDocument.)]
      (let [paragraph (.createParagraph word) run (.createRun paragraph)]
        (.setBold run true)
        (.setText run "Quarterly research")
        (-> paragraph .getCTP .addNewPPr .addNewOutlineLvl (.setVal java.math.BigInteger/ZERO)))
      (-> (.createParagraph word) .createRun (.setText "Revenue grew steadily."))
      (let [table (.createTable word 2 2)]
        (-> table (.getRow 0) (.getCell 0) (.setText "Product"))
        (-> table (.getRow 0) (.getCell 1) (.setText "Revenue"))
        (-> table (.getRow 1) (.getCell 0) (.setText "Research"))
        (-> table (.getRow 1) (.getCell 1) (.setText "$1200")))
      (.write word output))
    (let [original (.toByteArray output) before (aclone original)
          parts (office/read-parts original)
          copies (update-vals parts aclone)
          reading (docx/inspect-parts parts)]
      (is (= :ready (:status reading)) (pr-str reading))
      (is (= 1 (get-in reading [:blocks 0 :heading])))
      (is (= true (get-in reading [:blocks 0 :runs 0 :marks :bold])))
      (is (= [:paragraph :paragraph :table] (mapv :kind (:blocks reading))))
      (is (= "Product" (get-in reading [:blocks 2 :rows 0 :cells 0 :blocks 0 :runs 0 :text])))
      (is (Arrays/equals ^bytes original ^bytes before))
      (is (every? (fn [[path bytes]] (Arrays/equals ^bytes bytes ^bytes (get copies path))) parts))
      (with-open [reopened (XWPFDocument. (ByteArrayInputStream. original))]
        (is (= "Quarterly research" (.getText (first (.getParagraphs reopened)))))
        (is (= 1 (count (.getTables reopened))))))))

(deftest package-relationships-and-namespace-uris-select-the-main-document
  (doseq [namespace [word-ns strict-ns] prefix ["w" "business"]]
    (let [body (str "<" prefix ":p><" prefix ":r><" prefix ":t>Source text</" prefix ":t></" prefix ":r></" prefix ":p>")
          reading (docx/inspect-parts (package-parts body {:namespace namespace :prefix prefix :main "custom/main.data"}))]
      (is (= :ready (:status reading)) (pr-str reading))
      (is (= "custom/main.data" (:main-part reading)))
      (is (= "Source text" (text-of reading)))))
  (let [parts (package-parts "")
        xml (str "<document xmlns=\"" word-ns "\"><body><p><r><t>Default namespace</t></r></p></body></document>")]
    (is (= "Default namespace" (text-of (docx/inspect-parts (assoc parts "word/document.xml" (bytes-of xml))))))))

(deftest text-whitespace-breaks-and-disabled-emphasis-are-preserved
  (let [reading (docx/inspect-parts
                 (package-parts (str "<w:p><w:pPr><w:pStyle w:val=\"Heading1\"/></w:pPr><w:r>"
                                     "<w:rPr><w:b w:val=\"0\"/><w:i w:val=\"false\"/><w:strike w:val=\"off\"/><w:u w:val=\"none\"/></w:rPr>"
                                     "<w:t xml:space=\"preserve\">  Space  </w:t><w:tab/><w:t>tab</w:t><w:br/><w:t>line</w:t>"
                                     "</w:r></w:p><w:p/>")))]
    (is (= :ready (:status reading)))
    (is (= "  Space  \ttab\nline" (text-of reading)))
    (is (= {:bold false :italic false :strike false :underline false} (get-in reading [:blocks 0 :runs 0 :marks])))
    (is (nil? (get-in reading [:blocks 0 :heading])) "style IDs do not define heading semantics")
    (is (= [] (get-in reading [:blocks 1 :runs])))))

(deftest unsupported-semantic-constructs-are-not-flattened-into-final-text
  (let [reading (docx/inspect-parts
                 (package-parts
                  (str "<w:p><w:del><w:r><w:t>deleted-secret</w:t></w:r></w:del><w:ins><w:r><w:t>inserted-review</w:t></w:r></w:ins></w:p>"
                       "<w:p><w:r><w:fldChar w:fldCharType=\"begin\"/><w:instrText>INCLUDETEXT http://example.invalid</w:instrText><w:fldChar w:fldCharType=\"separate\"/><w:t>cached-result</w:t><w:fldChar w:fldCharType=\"end\"/></w:r></w:p>"
                       "<w:p><w:r><w:rPr><w:vanish/></w:rPr><w:t>hidden-text</w:t></w:r></w:p>"
                       "<mc:AlternateContent xmlns:mc=\"http://schemas.openxmlformats.org/markup-compatibility/2006\"><mc:Fallback><w:p><w:r><w:t>fallback-choice</w:t></w:r></w:p></mc:Fallback></mc:AlternateContent>")))]
    (is (= :ready (:status reading)))
    (doseq [text ["deleted-secret" "inserted-review" "cached-result" "hidden-text" "fallback-choice" "http://example.invalid"]]
      (is (not (str/includes? (pr-str reading) text))))
    (is (some #(= :placeholder (:kind %)) (:blocks reading)))
    (is (seq (:diagnostics reading)))))

(deftest merged-and-nested-tables-have-explicit-placeholders
  (doseq [body ["<w:tbl><w:tr><w:tc><w:tcPr><w:gridSpan w:val=\"2\"/></w:tcPr><w:p/></w:tc></w:tr></w:tbl>"
                "<w:tbl><w:tr><w:tc><w:tbl><w:tr><w:tc><w:p/></w:tc></w:tr></w:tbl></w:tc></w:tr></w:tbl>"]]
    (let [reading (docx/inspect-parts (package-parts body))]
      (is (= :ready (:status reading)))
      (is (= :placeholder (get-in reading [:blocks 0 :kind]))))))

(deftest external-ambiguous-and-wrong-package-declarations-refuse
  (doseq [options [{:target "http://example.invalid/document.xml"}
                   {:target "//example.invalid/document.xml"}
                   {:target "word/../document.xml"}
                   {:target "word/document.xml?download=yes"}
                   {:target "word/document.xml#section"}
                   {:target "word%2Fdocument.xml"}
                   {:target "word/%64ocument.xml"}
                   {:target "word/document.xml" :mode "External"}
                   {:type "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"}]]
    (is (= :unsupported (:status (docx/inspect-parts (package-parts "<w:p/>" options))))))
  (let [parts (package-parts "<w:p/>")
        duplicate (fn [text tag] (str/replace text (str "</" tag ">")
                                              (str (second (re-find #"(<(?:Relationship|Override)\s[^>]+/>)" text)) "</" tag ">")))]
    (doseq [[path tag] [["_rels/.rels" "Relationships"] ["[Content_Types].xml" "Types"]]]
      (is (= :unsupported (:status (docx/inspect-parts
                                    (update parts path #(bytes-of (duplicate (String. ^bytes % "UTF-8") tag))))))))))

(deftest hostile-xml-and-projection-limits-refuse-explicitly
  (let [parts (package-parts "<w:p><w:r><w:t>Hello</w:t></w:r></w:p>")]
    (with-redefs [docx/max-xml-bytes 64]
      (is (= :unsupported (:status (docx/inspect-parts parts)))))
    (with-redefs [docx/max-xml-nodes 3]
      (is (= :unsupported (:status (docx/inspect-parts parts)))))
    (with-redefs [docx/max-text-chars 4]
      (is (= :unsupported (:status (docx/inspect-parts parts)))))
    (with-redefs [docx/max-output-nodes 1]
      (is (= :unsupported (:status (docx/inspect-parts parts)))))
    (is (= :unsupported (:status (docx/inspect-parts
                                  (assoc parts "word/document.xml"
                                         (bytes-of "<!DOCTYPE document [<!ENTITY xxe SYSTEM 'file:///etc/passwd'>]><document>&xxe;</document>")))))))
  (let [body (str (apply str (repeat 130 "<w:p>")) (apply str (repeat 130 "</w:p>")))]
    (is (= :unsupported (:status (docx/inspect-parts (package-parts body)))))))

(deftest complex-field-scopes-cover-intermediate-paragraphs-and-tables
  (let [reading (docx/inspect-parts
                 (package-parts
                  (str "<w:p><w:r><w:t>Before</w:t></w:r></w:p>"
                       "<w:p><w:r><w:fldChar w:fldCharType=\"begin\"/><w:instrText>TOC</w:instrText><w:fldChar w:fldCharType=\"separate\"/></w:r></w:p>"
                       "<w:p><w:r><w:t>cross-paragraph-cache</w:t></w:r></w:p>"
                       "<w:tbl><w:tr><w:tc><w:p><w:r><w:t>cross-table-cache</w:t></w:r></w:p></w:tc></w:tr></w:tbl>"
                       "<w:p><w:r><w:fldChar w:fldCharType=\"end\"/></w:r></w:p>"
                       "<w:p><w:r><w:t>After</w:t></w:r></w:p>")))]
    (is (= :ready (:status reading)))
    (is (= "BeforeAfter" (text-of reading)))
    (is (not (str/includes? (pr-str reading) "cross-paragraph-cache")))
    (is (not (str/includes? (pr-str reading) "cross-table-cache"))))
  (is (= :unsupported (:status (docx/inspect-parts
                                (package-parts "<w:p><w:r><w:fldChar w:fldCharType=\"begin\"/></w:r></w:p>"))))))

(deftest structural-review-properties-do-not-appear-as-final-content
  (doseq [body ["<w:p><w:pPr><w:rPr><w:del/></w:rPr></w:pPr><w:r><w:t>deleted-paragraph-mark</w:t></w:r></w:p>"
                "<w:tbl><w:tr><w:trPr><w:del/></w:trPr><w:tc><w:p><w:r><w:t>deleted-row</w:t></w:r></w:p></w:tc></w:tr></w:tbl>"
                "<w:tbl><w:tr><w:tc><w:tcPr><w:cellDel/></w:tcPr><w:p><w:r><w:t>deleted-cell</w:t></w:r></w:p></w:tc></w:tr></w:tbl>"]]
    (let [reading (docx/inspect-parts (package-parts body))]
      (is (= :ready (:status reading)))
      (is (= :placeholder (get-in reading [:blocks 0 :kind])))
      (is (not (str/includes? (pr-str reading) "deleted-"))))))

(deftest removed-list-numbering-is-not-invented-as-a-bullet
  (doseq [id ["0" "+0" "000" "7"]]
    (let [reading (docx/inspect-parts
                   (package-parts (str "<w:p><w:pPr><w:numPr><w:numId w:val=\"" id
                                       "\"/></w:numPr></w:pPr><w:r><w:t>Paragraph</w:t></w:r></w:p>")))]
      (is (= :ready (:status reading)))
      (is (= (= "7" id) (get-in reading [:blocks 0 :list?]))))))
