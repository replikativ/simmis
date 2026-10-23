(ns is.simm.documents.markdown-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [is.simm.documents.source-fixtures :as fixtures]
            [is.simm.documents.markdown :as md]))

(def fixture
  (str "\uFEFF---\r\n"
       "title: Research\r\ncustom: [one, two]\r\nnumber: 12\r\n---\r\n"
       "# Research 🦋\r\n\r\n<!-- preserve this exactly -->\r\n\r\n"
       "- Claim [[Source#Evidence]] ^claim-1\r\n  - Child\r\n\r\n"
       "```clojure\r\n(+ 1 2)\r\n```\r\n\r\n"
       "<simmis-widget data-id=\"custom\">opaque</simmis-widget>"))

(deftest parsing-is-lossless-and-retains-source-coordinates
  (let [document (md/parse fixture)]
    (is (= fixture (:source document)))
    (is (= fixture (md/decode (md/source-bytes fixture)))))
  (let [document (md/parse fixture)
        nodes (rest (tree-seq #(seq (:children %)) :children {:children (:nodes document)}))]
    (is (= {"title" "Research" "custom" '("one" "two") "number" 12}
           (get-in document [:frontmatter :properties])))
    (is (empty? (:diagnostics document)))
    (is (some #(= :FencedCodeBlock (:kind %)) nodes))
    (is (some #(= :BulletList (:kind %)) nodes))
    (doseq [{:keys [from to]} nodes]
      (is (<= (:body-from document) from to (count fixture))))
    (is (= fixture (:source (md/patch fixture (:revision document) []))))))

(deftest selective-edits-preserve-every-unrelated-character
  (let [from (.indexOf fixture "Claim")
        document (md/patch fixture (md/revision fixture)
                           [{:from from :to (+ from 5) :text "Finding"}])]
    (is (= (str (subs fixture 0 from) "Finding" (subs fixture (+ from 5)))
           (:source document)))
    (is (= (get-in (md/parse fixture) [:frontmatter :raw])
           (get-in document [:frontmatter :raw])))
    (is (not= (md/revision fixture) (:revision document)))))

(deftest bare-bom-is-an-immutable-prefix-with-original-source-coordinates
  (doseq [newline ["\n" "\r\n" "\r"]
          body [(str "# Heading 🦋" newline newline "Text\uFEFFinside [[Note]]." newline)
                (str "Paragraph 🦋[^one]." newline newline "[^one]: Definition." newline)]]
    (let [source (str "\uFEFF" body)
          parsed (md/parse source)
          nodes (rest (tree-seq (comp seq :children) :children {:children (:nodes parsed)}))]
      (is (= source (:source parsed) (md/decode (md/source-bytes source))))
      (is (= 1 (:body-from parsed)))
      (is (nil? (:frontmatter parsed)))
      (is (= 1 (:from (first (:nodes parsed)))))
      (is (= (if (str/starts-with? body "#") :Heading :Paragraph) (:kind (first (:nodes parsed)))))
      (is (every? #(<= 1 (:from %) (:to %) (count source)) nodes))
      (is (= source (:source (md/patch source (:revision parsed) []))))
      (when (str/includes? body "\uFEFF")
        (is (some #(and (= :Text (:kind %)) (str/includes? (:text %) "\uFEFF")) nodes)))
      (doseq [{:keys [from to]} (md/references parsed)]
        (is (= "[[Note]]" (subs source from to))))))
  (let [parsed (md/parse "\uFEFF")]
    (is (= 1 (:body-from parsed)))
    (is (empty? (:nodes parsed))))
  (let [parsed (md/parse "\uFEFF---\ntitle: Unclosed")]
    (is (= 0 (:body-from parsed)))
    (is (= :frontmatter/unclosed (-> parsed :diagnostics first :kind)))))

(deftest frontmatter-errors-are-visible-but-never-repaired-or-executed
  (doseq [[source kind] [["---\na: 1\na: 2\n---\nbody" :frontmatter/invalid]
                         ["---\n- one\n---\nbody" :frontmatter/not-a-map]
                         ["---\na: [unclosed\n---\nbody" :frontmatter/invalid]
                         ["---\na: 1" :frontmatter/unclosed]
                         ["---\nx: !!java.net.URL [https://example.org]\n---\n" :frontmatter/invalid]]]
    (let [document (md/parse source)]
      (is (= source (:source document)))
      (is (= kind (-> document :diagnostics first :kind)))
      (is (nil? (get-in document [:frontmatter :properties]))))))

(deftest edits-require-a-current-revision-and-unambiguous-valid-ranges
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Document changed"
                       (md/patch "new" (md/revision "old") [])))
  (doseq [edits [[{:from 0 :to 2 :text "x"} {:from 1 :to 3 :text "y"}]
                [{:from 0 :to 0 :text "x"} {:from 0 :to 0 :text "y"}]
                [{:from -1 :to 0 :text "x"}]
                [{:from 0 :to 4 :text "x"}]
                [{:from 0 :to 1 :text "x" :unexpected true}]]]
    (is (thrown? clojure.lang.ExceptionInfo (md/patch "abc" (md/revision "abc") edits))))
  (testing "source and browser coordinates agree without splitting emoji"
    (let [text "a🦋b"]
      (is (= "a🌱b" (:source (md/patch text (md/revision text)
                                     [{:from 1 :to 3 :text "🌱"}]))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (md/patch text (md/revision text) [{:from 1 :to 2 :text "x"}])))))
  (is (thrown? java.nio.charset.CharacterCodingException
               (md/decode (byte-array [(unchecked-byte 0xff)]))))
  (is (thrown? java.nio.charset.CharacterCodingException
               (md/source-bytes (str (char 0xD800))))))

(deftest edits-use-one-base-not-cascading-offsets
  (is (= "A + BC" (:source (md/patch "a + bc" (md/revision "a + bc")
                                    [{:from 0 :to 1 :text "A"}
                                     {:from 4 :to 6 :text "BC"}])))))

(deftest projections-are-bounded
  (with-redefs [md/max-syntax-nodes 2]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"projection limit"
                         (md/parse "# Heading\n\nParagraph\n"))))
  (with-redefs [md/max-syntax-depth 0]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"projection limit"
                         (md/parse "# Heading\n"))))
  (with-redefs [md/max-source-bytes 4]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"too large"
                         (md/parse "🦋🦋")))))

(deftest representative-documents-preserve-source-before-semantic-codecs-are-added
  ;; This intentionally proves preservation/locality only, not support for all
  ;; features named in :required-semantics. Separate codec tests must prove those.
  (doseq [{:keys [id source editable]} fixtures/cases
          separator ["\n" "\r\n" "\r"]
          bom ["" "\uFEFF"]]
    (testing (str id " newline=" (pr-str separator) " bom=" (boolean (seq bom)))
      (let [text (str bom (str/replace source "\n" separator))
            parsed (md/parse text)
            start (.indexOf text editable)
            end (+ start (count editable))
            changed (md/patch text (:revision parsed)
                              [{:from start :to end :text "Edited text"}])]
        (is (= text (:source parsed)))
        (is (= text (md/decode (md/source-bytes text))))
        (is (= text (:source (md/patch text (:revision parsed) []))))
        (is (= (str (subs text 0 start) "Edited text" (subs text end))
               (:source changed)))))))

(deftest syntax-profile-projects-structure-without-equating-it-to-semantic-blocks
  (let [source (str "# Heading\n\n- [ ] Pending\n- [x] Done\n\n"
                    "| Name | Cost |\n| :--- | ---: |\n| A | 100 |\n\n"
                    "~~Old~~ and a footnote.[^note]\n\n[^note]: Source.\n")
        doc (md/parse source)
        nodes (rest (tree-seq (comp seq :children) :children {:children (:nodes doc)}))]
    (is (= md/syntax-profile (:syntax-profile doc)))
    (is (= 1 (:heading-level (first (:nodes doc)))))
    (is (= [false true] (mapv :checked? (filter #(= :TaskListItem (:kind %)) nodes))))
    (is (= #{:left :right} (set (keep :table-alignment nodes))))
    (doseq [kind [:TableBlock :TableRow :TableCell :Strikethrough :FootnoteBlock]]
      (is (some #(= kind (:kind %)) nodes)))
    (is (= source (:source doc)))))

(deftest references-retain-exact-spans-labels-fragments-and-remain-unresolved
  (let [source (str "\uFEFF---\r\ntitle: Links\r\n---\r\n"
                    "[[Plan#^block-1|Label]] ![[assets/chart.svg]] [[#Heading]]\n")
        doc (md/parse source)
        refs (md/references doc)]
    (is (= [:wikilink :embed :wikilink] (mapv :kind refs)))
    (is (= ["Plan" "assets/chart.svg" ""] (mapv :target refs)))
    (is (= ["^block-1" nil "Heading"] (mapv :fragment refs)))
    (is (= "Label" (:label (first refs))))
    (doseq [{:keys [from to target-from target-to target status revision]} refs]
      (is (= target (subs source target-from target-to)))
      (is (<= (:body-from doc) from target-from target-to to (count source)))
      (is (= :unresolved status))
      (is (= (:revision doc) revision)))))

(deftest literal-reference-contexts-do-not-invent-backlinks
  (let [source (:source (first (filter #(= :literal-reference-contexts (:id %)) fixtures/cases)))
        refs (md/references (md/parse source))]
    (is (= ["Target"] (mapv :target refs)))
    (is (= ["alias"] (mapv :label refs)))))

(deftest target-span-edits-preserve-reference-labels-fragments-and-literal-examples
  (let [source "See [[Old/Plan#^evidence|Our plan]]. `[[Old/Plan]]` stays literal.\n"
        doc (md/parse source)
        ref (first (md/references doc))
        changed (md/patch source (:revision doc)
                          [{:from (:target-from ref) :to (:target-to ref) :text "New/Plan"}])]
    (is (= "See [[New/Plan#^evidence|Our plan]]. `[[Old/Plan]]` stays literal.\n"
           (:source changed)))
    (is (= "^evidence" (:fragment (first (md/references changed)))))
    (is (= "Our plan" (:label (first (md/references changed)))))))
