(ns is.simm.documents.rich-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.rich :as rich]
            [is.simm.documents.rich-text :as text]))

(def repo-id #uuid "00000000-0000-4000-8000-000000000001")

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
                  "| A |\n| --- |\n| B |\n" "[bad](javascript:alert%281%29)\n"]]
    (is (nil? (text/model (first (:nodes (markdown/parse source))))) source))
  (doseq [url ["javascript:alert(1)" "data:text/html,bad" "//example.com" "\nhttps://example.com" "java\tscript:bad"]]
    (is (not (text/safe-link? url)))))

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
    (is (str/includes? result "Revised **bold**\\.\r\n"))
    (is (str/includes? result "Child [[opaque]] text.\r\n"))
    (is (= (str/replace source "Hello **bold**." "Revised **bold**\\.") result))
    (is (thrown? clojure.lang.ExceptionInfo
                 (rich/apply-edit repo-id "wiki/note.md" result (:revision before) (:from node) (:to node) edited)))))

(deftest rich-edits-cannot-create-frontmatter-through-a-first-paragraph
  (let [source "Hello\n"
        model {:type "doc" :content [{:type "horizontalRule"}
                                     {:type "paragraph" :content [{:type "text" :text "title: Identity change"}]}
                                     {:type "horizontalRule"}]}]
    (is (thrown? clojure.lang.ExceptionInfo
                 (rich/apply-edit repo-id "wiki/note.md" source (markdown/revision source) 0 6 model)))))
