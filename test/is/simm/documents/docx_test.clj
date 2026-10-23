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

(deftest whole-leaf-correction-preserves-source-and-occurrence
  (let [parts (package-parts "<w:p><w:r><w:t>Repeated &amp; &#x1F98B;</w:t><w:t>Repeated &amp; &#x1F98B;</w:t></w:r></w:p>")
        located (docx/correction-source parts) indices (sort (keys (:targets located)))
        selected (last indices) old (get-in located [:targets selected :text])
        noop (docx/correct-text parts selected old)
        edit (docx/correct-text parts selected "Literal </w:t> &amp; 🦋")
        changed (assoc parts (:main-part edit) (:bytes edit))
        text (String. ^bytes (:bytes edit) "UTF-8")]
    (is (= 2 (count indices)))
    (is (false? (:changed? noop)))
    (is (nil? (:bytes noop)))
    (is (str/includes? text "<w:t>Repeated &amp; &#x1F98B;</w:t><w:t>Literal &lt;/w:t&gt; &amp;amp; 🦋</w:t>"))
    (is (= "Repeated & 🦋Literal </w:t> &amp; 🦋" (text-of (docx/inspect-parts changed))))
    (is (thrown? clojure.lang.ExceptionInfo (docx/correct-text parts 999 "other")))))

(deftest correction-proof-normalizes-parser-callback-chunks
  (let [long-text (apply str (repeat 1800 "α🦋 &amp; text\r\n"))
        parts (package-parts (str "<w:p><w:r><w:t>short</w:t><w:t xml:space='preserve'>" long-text "</w:t></w:r></w:p>"))
        source (docx/correction-source parts) index (first (sort (keys (:targets source))))
        edit (docx/correct-text parts index (apply str (repeat 1000 "longer")))
        text (String. ^bytes (:bytes edit) "UTF-8")]
    (is (true? (:changed? edit)))
    (is (str/includes? text (str "<w:t xml:space='preserve'>" long-text "</w:t>")))))

(deftest correction-eligibility-is-conservative
  (doseq [body ["<w:p><w:hyperlink><w:r><w:t>linked</w:t></w:r></w:hyperlink></w:p>"
                "<w:p><w:sdt><w:sdtContent><w:r><w:t>controlled</w:t></w:r></w:sdtContent></w:sdt></w:p>"
                "<w:p><w:ins><w:r><w:t>tracked</w:t></w:r></w:ins></w:p>"
                "<w:p><w:r><w:rPr><w:vanish/></w:rPr><w:t>hidden</w:t></w:r></w:p>"
                "<w:p><w:r><w:fldChar w:fldCharType='begin'/><w:t>cached</w:t><w:fldChar w:fldCharType='end'/></w:r></w:p>"
                "<w:p><w:r><w:t>a<!-- comment -->b</w:t></w:r></w:p>"
                "<w:p><w:r><w:t><![CDATA[not plain lexical text]]></w:t></w:r></w:p>"
                "<w:p><w:r><w:t/></w:r></w:p>"]]
    (is (empty? (:targets (docx/correction-source (package-parts body)))) body))
  (let [parts (package-parts "<w:p><w:r><w:t>ordinary</w:t></w:r></w:p><w:p><w:bookmarkStart w:id='1'/><w:bookmarkEnd w:id='1'/></w:p>")]
    (is (thrown? clojure.lang.ExceptionInfo (docx/correction-source parts)))))

(deftest correction-whitespace-and-package-policy
  (doseq [text [" A &amp; B " "line&#10;break" (apply str (repeat 12000 "x"))]]
    (let [parts (package-parts (str "<w:p><w:r><w:t>" text "</w:t></w:r></w:p>"))
          targets (:targets (docx/correction-source parts)) index (first (keys targets))]
      (is (false? (:changed? (docx/correct-text parts index (:text (get targets index))))))))
  (doseq [space ["" " xml:space='preserve'"]]
    (let [parts (package-parts (str "<w:p><w:r><w:t" space ">ordinary</w:t></w:r></w:p>"))
          index (first (keys (:targets (docx/correction-source parts))))]
      (if (empty? space)
        (is (thrown? clojure.lang.ExceptionInfo (docx/correct-text parts index " edge ")))
        (is (= " edge " (text-of (docx/inspect-parts (assoc parts "word/document.xml" (:bytes (docx/correct-text parts index " edge "))))))))))
  (doseq [kind ["application/vnd.ms-office.vbaProject" "application/vnd.openxmlformats-package.digital-signature-xmlsignature+xml"]]
    (let [parts (package-parts "<w:p><w:r><w:t>ordinary</w:t></w:r></w:p>")
          types (String. ^bytes (get parts "[Content_Types].xml") "UTF-8")
          parts (assoc parts "[Content_Types].xml" (bytes-of (str/replace types "</Types>" (str "<Override PartName='/renamed/payload.bin' ContentType='" kind "'/></Types>"))))]
      (is (thrown? clojure.lang.ExceptionInfo (docx/correction-source parts))))))
