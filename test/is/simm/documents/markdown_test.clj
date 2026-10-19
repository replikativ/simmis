(ns is.simm.documents.markdown-test
  (:require [clojure.test :refer [deftest is testing]]
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
