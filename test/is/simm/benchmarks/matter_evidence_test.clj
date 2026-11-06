(ns is.simm.benchmarks.matter-evidence-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.benchmarks.matter-evidence :as evidence]
            [is.simm.media.office :as office]))
(defn text-bytes [s] (.getBytes ^String s "UTF-8"))
(defn package [parts] (office/write-parts (update-vals parts text-bytes)))
(defn word [body]
  (str "<x:document xmlns:x=\"" evidence/word-ns "\"><x:body>" body "</x:body></x:document>"))
(defn workbook [target mode sheet]
  {"xl/workbook.xml" (str "<workbook xmlns=\"" evidence/sheet-ns "\" xmlns:r=\"" evidence/office-rel-ns "\"><sheets><sheet name=\"Terms\" r:id=\"r1\"/></sheets></workbook>")
   "xl/_rels/workbook.xml.rels" (str "<Relationships xmlns=\"" evidence/rel-ns "\"><Relationship Id=\"r1\" Type=\"" evidence/office-rel-ns "/worksheet\" Target=\"" target "\"" (when mode (str " TargetMode=\"" mode "\"")) "/></Relationships>")
   "xl/sharedStrings.xml" (str "<sst xmlns=\"" evidence/sheet-ns "\"><si><r><t>Invoice </t></r><r><t>date</t></r></si></sst>")
   "xl/worksheets/sheet1.xml" (str "<worksheet xmlns=\"" evidence/sheet-ns "\"><sheetData><row r=\"1\">" sheet "</row></sheetData></worksheet>")})
(deftest paragraphs-preserve-whitespace-unicode-and-table-provenance
  (let [input (package {"word/document.xml" (word "<x:p><x:r><x:t xml:space=\"preserve\">  A &amp; λ </x:t><x:tab/><x:t>B</x:t></x:r></x:p><x:tbl><x:tr><x:tc><x:p><x:r><x:t>$1m</x:t></x:r></x:p></x:tc></x:tr></x:tbl>")})
        p (evidence/project "terms.docx" input) paragraphs (get-in p [:evidence :paragraphs])]
    (is (= ["  A & λ \tB" "$1m"] (mapv :text paragraphs)))
    (is (every? integer? ((juxt :table :row :cell) (second paragraphs))))
    (is (= (evidence/sha256 input) (get-in p [:evidence :sha256])))
    (is (false? (get-in p [:evidence :policy :native-office-reproduction])))
    (is (= 2 (count (set (map :id paragraphs)))))
    (is (= (word "<x:p/>") (get-in (evidence/project "empty.docx" (package {"word/document.xml" (word "<x:p/>")})) [:xml-parts "word/document.xml"])))))
(deftest cached-pagination-is-explicit-and-never-a-page-locator
  (let [parts {"word/document.xml" (word "<x:p/>")
               "word/footer1.xml" (str "<ftr xmlns=\"" evidence/word-ns "\"><p><r><instrText> PAGE </instrText><t>1</t></r></p></ftr>")}
        p (evidence/project "pages.docx" (package parts))]
    (is (= ["PAGE"] (->> (get-in p [:evidence :paragraphs]) (filter :cached-page-fields) first :cached-page-fields)))
    (is (thrown? clojure.lang.ExceptionInfo (evidence/project "bad.docx" (package (update parts "word/footer1.xml" #(clojure.string/replace % " PAGE " " INCLUDETEXT http://example.invalid "))))))))
(deftest review-hidden-and-embedded-content-refuses-instead-of-looking-complete
  (doseq [body ["<x:ins><x:p><x:r><x:t>new</x:t></x:r></x:p></x:ins>"
                "<x:p><x:r><x:instrText>PAGE</x:instrText><x:t>5</x:t></x:r></x:p>"
                "<x:p><x:r><x:rPr><x:vanish/></x:rPr><x:t>hidden</x:t></x:r></x:p>"
                "<x:p><x:r><x:drawing/></x:r></x:p>"]]
    (is (thrown? clojure.lang.ExceptionInfo (evidence/project "terms.docx" (package {"word/document.xml" (word body)})))))
  (is (thrown? clojure.lang.ExceptionInfo (evidence/project "terms.docx" (package {"word/document.xml" "<not-word/>"})))))
(deftest spreadsheet-keeps-cell-identities-exact-values-and-formulas-without-evaluation
  (let [p (evidence/project "terms.xlsx" (package (workbook "worksheets/sheet1.xml" nil "<c r=\"A1\" t=\"s\"><v>0</v></c><c r=\"B1\"><f>1+2</f><v>3</v></c>")))
        sheet (get-in p [:evidence :sheets 0])]
    (is (= "Terms" (:name sheet)))
    (is (= ["A1" "B1"] (mapv :id (:cells sheet))))
    (is (= ["Invoice date" "3"] (mapv :text (:cells sheet))))
    (is (= "1+2" (get-in sheet [:cells 1 :formula])))
    (is (= :cached-not-recalculated (get-in p [:evidence :policy :formula-values])))))
(deftest spreadsheet-refuses-external-escaping-duplicate-and-invalid-shared-references
  (doseq [[target mode cells] [["https://example.invalid/sheet.xml" "External" ""]
                               ["../../outside.xml" nil ""]
                               ["worksheets/sheet1.xml" nil "<c r=\"A1\"/><c r=\"A1\"/>"]
                               ["worksheets/sheet1.xml" nil "<c r=\"A1\" t=\"s\"><v>900</v></c>"]]]
    (is (thrown? clojure.lang.ExceptionInfo (evidence/project "terms.xlsx" (package (workbook target mode cells)))))))

(deftest cell-style-indexes-do-not-include-the-style-inheritance-table
  (let [styles (str "<styleSheet xmlns=\"" evidence/sheet-ns "\">"
                    "<numFmts><numFmt numFmtId=\"164\" formatCode=\"yyyy-mm-dd\"/></numFmts>"
                    "<cellStyleXfs><xf numFmtId=\"0\"/></cellStyleXfs>"
                    "<cellXfs><xf numFmtId=\"14\"/><xf numFmtId=\"164\"/></cellXfs></styleSheet>")
        parts (assoc (workbook "worksheets/sheet1.xml" nil "<c r=\"A1\" s=\"1\"><v>45678</v></c>") "xl/styles.xml" styles)
        p (:evidence (evidence/project "styles.xlsx" (package parts)))
        formats (:number-formats p)]
    (is (= "1" (get-in p [:sheets 0 :cells 0 :style-index])))
    (is (= ["14" "164"] (mapv #(get-in % [:attrs ["" "numFmtId"]]) (:cells formats))))
    (is (= "yyyy-mm-dd" (get-in formats [:custom 0 :attrs ["" "formatCode"]])))))

(deftest hidden-rows-columns-and-omitted-worksheet-structures-refuse
  (let [base (workbook "worksheets/sheet1.xml" nil "<c r=\"A1\"><v>1</v></c>")
        part "xl/worksheets/sheet1.xml"]
    (doseq [parts [(update base part #(clojure.string/replace % "<row r=\"1\">" "<row r=\"1\" hidden=\"1\">"))
                   (update base part #(clojure.string/replace % "<sheetData>" "<cols><col min=\"1\" max=\"1\" hidden=\"true\"/></cols><sheetData>"))
                   (update base part #(clojure.string/replace % "</worksheet>" "<mergeCells><mergeCell ref=\"A1:B1\"/></mergeCells></worksheet>"))
                   (update base part #(clojure.string/replace % "</worksheet>" "<drawing/></worksheet>"))
                   (assoc base "xl/comments1.xml" "<comments/>")]]
      (is (thrown? clojure.lang.ExceptionInfo (evidence/project "hidden.xlsx" (package parts)))))))
