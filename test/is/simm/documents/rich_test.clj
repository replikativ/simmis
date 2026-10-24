(ns is.simm.documents.rich-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.rich :as rich]
            [is.simm.documents.files :as files]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.documents.rich-text :as text]))

(def repo-id #uuid "00000000-0000-4000-8000-000000000001")

(deftest adjacent-native-lists-retain-their-boundaries-and-numbering
  (let [paragraph (fn [s] {:type "paragraph" :content [{:type "text" :text s}]})
        item (fn [s] {:type "listItem" :content [(paragraph s)]})
        list-model (fn [kind n s] (cond-> {:type kind :content [(item s)]}
                                   (= kind "orderedList") (assoc :attrs {:start n})))]
    (doseq [kind ["bulletList" "orderedList"] newline ["\n" "\r\n" "\r"] nested? [false true]]
      (let [lists [(list-model kind 3 "First") (list-model kind 8 "Second") (list-model kind 2 "Third")]
            model {:type "doc" :content (if nested?
                                          [{:type "bulletList" :content [{:type "listItem" :content (into [(paragraph "Parent")] lists)}]}]
                                          lists)}
            serialized (text/markdown model newline)
            decoded {:type "doc" :content (vec (mapcat :content (map text/model (:nodes (markdown/parse serialized)))))}]
        (is (= (text/roundtrip-normalize model) (text/roundtrip-normalize decoded)) (pr-str [kind nested? serialized]))
        (let [source "Original paragraph.\n"
              result (rich/apply-edit repo-id "wiki/lists.md" source (markdown/revision source) 0 (count source) model)
              reread {:type "doc" :content (vec (mapcat :content (map text/model (:nodes (markdown/parse result)))))}]
          (is (= (text/roundtrip-normalize model) (text/roundtrip-normalize reread))))))))

(deftest selective-edits-of-existing-separated-lists-keep-every-list
  (doseq [source ["- A\n\n+ B\n" "+ A\n\n- B\n" "3. A\n\n8) B\n"]
          indices [[0] [1] [0 1]]]
    (let [original {:type "doc" :content (vec (mapcat :content (map text/model (:nodes (markdown/parse source)))))}
          model (reduce (fn [m i] (assoc-in m [:content i :content 0 :content 0 :content 0 :text] "Changed")) original indices)
          result (rich/apply-edit repo-id "wiki/split-lists.md" source (markdown/revision source) 0 (count source) model)
          decoded {:type "doc" :content (vec (mapcat :content (map text/model (:nodes (markdown/parse result)))))}]
      (is (= (text/roundtrip-normalize model) (text/roundtrip-normalize decoded)) (pr-str [source indices result]))))
  (let [model {:type "doc" :content [{:type "bulletList" :content [{:type "listItem" :content [{:type "paragraph" :content [{:type "text" :text "A"}]}]}]}
                                     {:type "paragraph"}
                                     {:type "bulletList" :content [{:type "listItem" :content [{:type "paragraph" :content [{:type "text" :text "B"}]}]}]}]}
        decoded {:type "doc" :content (vec (mapcat :content (map text/model (:nodes (markdown/parse (text/markdown model))))))}]
    (is (= (text/roundtrip-normalize model) (text/roundtrip-normalize decoded)))
    (let [source "- A\n"
          result (rich/apply-edit repo-id "wiki/new-list.md" source (markdown/revision source) 0 (count source) model)
          actual {:type "doc" :content (vec (mapcat :content (map text/model (:nodes (markdown/parse result)))))}]
      (is (= (text/roundtrip-normalize model) (text/roundtrip-normalize actual))))))

(deftest supported-regions-have-serializable-inert-models
  (doseq [source ["# Heading **bold** and *italic*.\n" "A [link](https://example.com) and `code`.\n"
                  "- One\n  - Nested\n- Two\n" "3. Three\n4. Four\n"
                  "~~Old~~ text\n" "```clojure\n(+ 1 2)\n```\n"]]
    (let [parsed (markdown/parse source) node (first (:nodes parsed)) model (text/model node)]
      (is (map? model) source)
      (is (string? (text/markdown model)))
      (is (= source (text/apply-region source (assoc node :model model) model))))))

(deftest unknown-and-active-content-is-never-guessed-into-rich-text
  (doseq [source ["<script>bad()</script>\n" "[[Wiki]]\n" "- [ ] Task\n"
                  "| A | B |\n| --- | --- |\n| merged ||\n" "[bad](javascript:alert%281%29)\n"]]
    (is (nil? (text/model (first (:nodes (markdown/parse source))))) source))
  (doseq [url ["javascript:alert(1)" "data:text/html,bad" "//example.com" "\nhttps://example.com" "java\tscript:bad"]]
    (is (not (text/safe-link? url)))))

(deftest empty-list-items-round-trip-with-their-required-editor-paragraph
  (doseq [kind ["bulletList" "orderedList"]
          position [0 1 2 :sole :consecutive]
          newline ["\n" "\r\n" "\r"]]
    (let [paragraph (fn [s] {:type "paragraph" :content [{:type "text" :text s}]})
          item (fn [p] {:type "listItem" :content [p]})
          items [(item (paragraph "First")) (item (paragraph "Last"))]
          empty-item (item {:type "paragraph"})
          items (case position
                  :sole [empty-item]
                  :consecutive [empty-item empty-item]
                  (vec (concat (subvec items 0 position) [empty-item] (subvec items position))))
          empty-index (if (integer? position) position 0)
          list-model (cond-> {:type kind :content items} (= kind "orderedList") (assoc :attrs {:start 3}))
          model {:type "doc" :content [list-model]}
          serialized (text/markdown model newline)
          parsed (text/model (first (:nodes (markdown/parse serialized))))]
      (is (= (text/roundtrip-normalize model) (text/roundtrip-normalize parsed))
          (pr-str [kind position newline serialized]))
      (is (= {:type "paragraph" :content []}
             (first (get-in (text/normalize parsed) [:content 0 :content empty-index :content]))))
      ;; The real HOST validator must accept the transient Enter state without
      ;; relaxing semantic equivalence or creating a knowledge-block identity.
      (let [source (str "- Existing" newline)
            result (rich/apply-edit repo-id "wiki/empty-item.md" source
                                    (markdown/revision source) 0 (count source) model)
            reread (text/model (first (:nodes (markdown/parse result))))]
        (is (= (text/roundtrip-normalize model) (text/roundtrip-normalize reread)))))))

(deftest native-defaults-and-text-merging-do-not-cause-source-rewrites
  (let [source "Some **bold** text.\r\n"
        node (first (:nodes (markdown/parse source))) model (text/model node)
        native (-> model (assoc-in [:content 0 :attrs] {})
                   (update-in [:content 0 :content] #(mapv (fn [n] (assoc n :marks (vec (:marks n)))) %)))]
    (is (= source (text/apply-region source (assoc node :model model) native)))))

(deftest changing-one-rich-region-preserves-unknown-syntax-metadata-and-children
  (let [source (str "---\r\ncustom: untouched\r\n---\r\n"
                    "<!-- simmis:block 00000000-0000-4000-8000-000000000002 -->\r\n\r\n"
                    "Hello **bold**.\r\n\r\n"
                    "<!-- simmis:block 00000000-0000-4000-8000-000000000003 -->\r\n\r\n"
                    "Child [[opaque]] text.\r\n\r\n<!-- /simmis:block -->\r\n\r\n<!-- /simmis:block -->\r\n")
        before (markdown/parse source)
        node (first (filter #(= :Paragraph (:kind %)) (:nodes before)))
        model (text/model node)
        edited (assoc-in model [:content 0 :content 0 :text] "Revised ")
        result (rich/apply-edit repo-id "wiki/note.md" source (:revision before) (:from node) (:to node) edited)]
    (is (str/starts-with? result "---\r\ncustom: untouched\r\n---\r\n"))
    (is (str/includes? result "Revised&#32;**bold**\\.\r\n"))
    (is (str/includes? result "Child [[opaque]] text.\r\n"))
    (is (= (str/replace source "Hello **bold**." "Revised&#32;**bold**\\.") result))
    (is (thrown? clojure.lang.ExceptionInfo
                 (rich/apply-edit repo-id "wiki/note.md" result (:revision before) (:from node) (:to node) edited)))))

(deftest rich-edits-cannot-create-frontmatter-through-a-first-paragraph
  (let [source "Hello\n"
        model {:type "doc" :content [{:type "horizontalRule"}
                                     {:type "paragraph" :content [{:type "text" :text "title: Identity change"}]}
                                     {:type "horizontalRule"}]}]
    (is (thrown? clojure.lang.ExceptionInfo
                 (rich/apply-edit repo-id "wiki/note.md" source (markdown/revision source) 0 6 model)))))

(deftest continuous-rich-drafts-are-source-local-and-batch-validation-agrees
  (let [source "---\r\ncustom: retained\r\n---\r\n\r\n# Title\r\n\r\nFirst paragraph.\r\n\r\n[[Opaque]]\r\n\r\nLast paragraph.\r\n"
        analysis (files/analyze repo-id "wiki/test.md" source)
        file {:path "wiki/test.md" :kind :markdown :entry {:content :initial} :text source :analysis analysis}
        d (editor/draft file) base-id (get-in d [:rich-base :id])
        slots (filter :model (:slots analysis)) first-slot (second slots) last-slot (last slots)
        revised (-> d
                    (editor/rich-changed base-id (:from first-slot)
                                         (assoc-in (:model first-slot) [:content 0 :content 0 :text] "Edited first") {:from 1 :to 1})
                    (editor/rich-changed base-id (:from last-slot)
                                         (assoc-in (:model last-slot) [:content 0 :content 0 :text] "Edited last") {:from 2 :to 2}))
        expected (rich/apply-edits repo-id "wiki/test.md" source (:revision analysis) (editor/rich-edits revised))]
    (is (= :rich (:mode d)))
    (is (= source (:text d)))
    (is (= 2 (count (editor/rich-edits revised))))
    (is (= expected (:text revised)))
    (is (= expected (-> source (str/replace "First paragraph." "Edited first") (str/replace "Last paragraph." "Edited last"))))
    (is (str/includes? expected "[[Opaque]]\r\n"))
    (let [opaque (first (filter #(nil? (:model %)) (:slots analysis)))
          selection (editor/source-selection revised (:from opaque) (:to opaque))]
      (is (= (str/index-of (:text revised) "[[Opaque]]") (:anchor selection))))
    (is (= revised (editor/rich-changed revised :old-world (:from first-slot) (:model first-slot) nil)))
    (let [saved (editor/saved-at-generation revised (:generation d) (:text revised) {:entry {:content :saved}})
          typed (editor/rich-changed revised base-id (:from first-slot)
                                     (assoc-in (:model first-slot) [:content 0 :content 0 :text] "Typed during save") nil)
          ack (editor/saved-at-generation typed (:generation d) (:text revised) {:entry {:content :saved}})]
      (is (not (editor/dirty? saved)))
      (is (editor/dirty? ack))
      (is (str/includes? (:text ack) "Typed during save"))
      (is (= base-id (get-in ack [:rich-base :id]))))
    (is (nil? (:rich-base (editor/changed revised "# Source edit\n" {:anchor 0 :head 0}))))))

(deftest batch-rich-edits-refuse-overlap-stale-source-and-metadata-injection
  (let [source "First\n\nSecond\n" analysis (files/analyze repo-id "test.md" source)
        a (first (:slots analysis)) edit (select-keys a [:from :to :model])]
    (is (thrown? clojure.lang.ExceptionInfo (rich/apply-edits repo-id "test.md" source (:revision analysis) [edit edit])))
    (is (thrown? clojure.lang.ExceptionInfo (rich/apply-edits repo-id "test.md" "changed" (:revision analysis) [edit])))
    (is (thrown? clojure.lang.ExceptionInfo (rich/apply-edits repo-id "test.md" source (:revision analysis)
                  [(assoc edit :model {:type "doc" :content [{:type "horizontalRule"} {:type "paragraph" :content [{:type "text" :text "title: injected"}]} {:type "horizontalRule"}]})])))))

(deftest empty-top-level-paragraphs-are-blank-line-layout-not-semantic-blocks
  (let [source "First\n\nSecond\n"
        paragraph (fn [text] {:type "paragraph" :content [{:type "text" :text text}]})
        first-p (paragraph "First") second-p (paragraph "Second") empty-p {:type "paragraph" :content []}
        baseline {:type "doc" :content [first-p second-p]}]
    (doseq [[content placement] [[[empty-p first-p second-p] :leading]
                                 [[first-p empty-p second-p] :middle]
                                 [[first-p second-p empty-p] :trailing]]]
      (let [model {:type "doc" :content content}
            result (rich/apply-edit repo-id "wiki/blank-lines.md" source (markdown/revision source)
                                    0 (count source) model)
            reopened {:type "doc" :content (vec (mapcat #(get-in (text/model %) [:content])
                                                        (:nodes (markdown/parse result))))}]
        (is (= (text/roundtrip-normalize baseline) (text/roundtrip-normalize model)))
        (is (= (text/roundtrip-normalize model) (text/roundtrip-normalize reopened)))
        (is (case placement
              :leading (str/starts-with? result "\n\nFirst")
              :middle (str/includes? result "First\n\n\n\nSecond")
              :trailing (str/ends-with? result "Second\n\n\n")))))
    (let [list-of (fn [content] {:type "doc" :content [{:type "bulletList"
                                                       :content [{:type "listItem" :content content}]}]})]
      (is (not= (text/roundtrip-normalize (list-of [first-p]))
                (text/roundtrip-normalize (list-of [first-p empty-p])))))
    (is (not= (text/roundtrip-normalize baseline)
              (text/roundtrip-normalize (assoc-in baseline [:content 0 :content 0 :text] " First"))))))

(deftest blank-layout-does-not-relax-significant-text-or-nested-structure
  (doseq [[source model]
          [["- First\n" {:type "doc" :content [{:type "bulletList"
                                                :content [{:type "listItem"
                                                           :content [{:type "paragraph" :content [{:type "text" :text "First"}]}
                                                                     {:type "paragraph" :content []}]}]}]}]]]
    (is (= ::rich/unsupported
           (try (rich/apply-edit repo-id "wiki/layout-guard.md" source (markdown/revision source)
                                 0 (count source) model)
                :unexpected
                (catch clojure.lang.ExceptionInfo error (:type (ex-data error)))))))
  (is (= "" (rich/apply-edit repo-id "wiki/empty.md" "" (markdown/revision "") 0 0
                             {:type "doc" :content [{:type "paragraph"} {:type "paragraph"}]}))))

(deftest every-whitespace-keystroke-roundtrips-without-relaxing-equivalence
  (doseq [value [" " "   " "Sentence. " " leading" "\tLeading" "Trailing\t"
                 "&#32;" "&copy;" "&amp;copy;" "&not-an-entity;"
                 "<script>literal</script>" "[^not-a-footnote]" "`literal`" "\\"]
          block [:paragraph :heading :list]
          mark [nil "bold" "italic" "link"]]
    (let [leaf (cond-> {:type "text" :text value}
                 mark (assoc :marks [(cond-> {:type mark}
                                      (= mark "link") (assoc :attrs {:href "notes.md"}))]))
          paragraph {:type "paragraph" :content [leaf]}
          model {:type "doc" :content [(case block
                                         :paragraph paragraph
                                         :heading {:type "heading" :attrs {:level 2} :content [leaf]}
                                         :list {:type "bulletList" :content [{:type "listItem" :content [paragraph]}]})]}
          original "Original\n"
          result (rich/apply-edit repo-id "wiki/whitespace.md" original (markdown/revision original)
                                  0 (count original) model)
          decoded {:type "doc" :content (vec (mapcat :content (map text/model (:nodes (markdown/parse result)))))}]
      (is (= (text/roundtrip-normalize model) (text/roundtrip-normalize decoded)) [value block mark]))))

(deftest entities-decode-once-and-remain-inert-source-positioned-text
  (let [source "&#60;script&#62; &#91;^not-a-note&#93; &amp;copy; &#92; &#42;\n"
        parsed (markdown/parse source)
        node (first (:nodes parsed))
        model (text/model node)]
    (is (some #(= :HtmlEntity (:kind %)) (:children node)))
    (is (= "<script> [^not-a-note] &copy; \\ *"
           (apply str (map :text (get-in model [:content 0 :content])))))
    (is (empty? (markdown/references parsed)))
    (is (= source (text/apply-region source (assoc node :model model) model)))
    (is (not-any? #(#{:HtmlInline :Footnote} (:kind %)) (:children node)))))
