(ns is.simm.documents.rich-tables-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as md]
            [is.simm.documents.rich-text :as rich]
            [is.simm.documents.rich-document :as document]
            [is.simm.documents.rich :as host]))

(def repository #uuid "00000000-0000-4000-8000-000000000001")
(def block-id "00000000-0000-4000-8000-000000000002")
(defn analysis [source] (files/analyze repository "wiki/table.md" source))
(defn model [source] (get-in (analysis source) [:slots 0 :model]))
(defn nodes [model] (tree-seq (comp seq :content) :content model))

(deftest basic-tables-have-a-proven-initial-roundtrip
  (doseq [source ["| A | B |\n|---|---|\n| x | y |\n"
                  "| Left | Right | Centre |\n|:---|---:|:---:|\n| a | b | c |\n"
                  "| Header only |\n|---|\n"
                  "| A | B |\n|---|---|\n| | |\n"
                  "| f\\|oo |\n|---|\n| b `\\|` az |\n| b **\\|** im |\n"
                  "| [a\\|b](<https://example.test/a\\|b> \"t\\|itle\") |\n|---|\n| c |\n"
                  "| ``a`\\|`b`` |\n|---|\n| $a \\mid b$ |\n"]]
    (let [original (model source)
          encoded (when original (rich/markdown original))
          decoded (when encoded (model encoded))]
      (is (some? original) source)
      (is (= "table" (get-in original [:content 0 :type])) source)
      (is (= (rich/roundtrip-normalize original) (rich/roundtrip-normalize decoded)) source)
      (is (= source (rich/apply-region source (assoc (first (:nodes (md/parse source))) :model original) original))))))

(deftest gfm-pipes-inside-code-have-their-logical-cell-text
  (let [m (model "| `\\|` |\n|---|\n| plain\\|pipe |\n")
        text (map :text (filter #(= "text" (:type %)) (nodes m)))]
    (is (= ["|" "plain|pipe"] text))))

(deftest table-edits-change-only-their-source-slot-and-retain-identities
  (let [source (str "---\r\nsimmis-id: " repository "\r\ncustom: retained\r\n---\r\n"
                    "<!-- simmis:block " block-id " -->\r\n\r\n"
                    "__Original spelling__\r\n\r\n| Name | Value |\r\n|:---|---:|\r\n| Before | 42 |\r\n\r\n"
                    "<!-- /simmis:block -->\r\n\r\n<!-- untouched -->\r\n")
        projection (document/project source (analysis source))
        edited (letfn [(walk [n]
                         (cond-> (if (and (= "text" (:type n)) (= "Before" (:text n))) (assoc n :text "After | 🦋") n)
                           (:content n) (update :content #(mapv walk %))))]
                 (walk (:model projection)))
        output (document/apply-model source projection edited)
        patches (document/edits projection edited)]
    (is (= source (document/apply-model source projection (:model projection))))
    (is (= output (host/apply-edits repository "wiki/table.md" source (md/revision source) patches)))
    (is (str/includes? output "__Original spelling__\r\n\r\n"))
    (is (str/includes? output "| After \\| 🦋 | 42 |"))
    (is (str/ends-with? output "<!-- /simmis:block -->\r\n\r\n<!-- untouched -->\r\n"))
    (is (= (get-in (md/parse source) [:frontmatter :raw]) (get-in (md/parse output) [:frontmatter :raw])))
    (is (= (document/topology (:model projection)) (document/topology edited)))))

(deftest unsupported-table-shapes-and-attrs-cannot-be-normalized-away
  (let [m (model "| A | B |\n|---|---|\n| x | y |\n")
        cell [:content 0 :content 1 :content 0]]
    (doseq [bad [(assoc-in m (conj cell :attrs :colspan) 2)
                 (assoc-in m (conj cell :attrs :rowspan) 2)
                 (assoc-in m (conj cell :attrs :colwidth) [200])
                 (assoc-in m (conj cell :attrs :align) "right")
                 (assoc-in m (conj cell :attrs :unknown) "must not disappear")
                 (update-in m (conj cell :content) conj {:type "paragraph" :content []})
                 (update-in m [:content 0 :content 1 :content] pop)
                 (assoc-in m [:content 0 :content 0 :content 0 :type] "tableCell")]]
      (is (thrown? clojure.lang.ExceptionInfo (rich/normalize bad)))
      (is (thrown? clojure.lang.ExceptionInfo (rich/markdown bad)))))
  (doseq [source ["| A | B |\n|---|---|\n| merged ||\n"
                  "| A |\n|---|\n| <br> |\n"
                  "| A |\n|---|\n| x |\n[Caption]\n"]]
    (is (nil? (model source)) source)))

(deftest initial-roundtrip-bound-refusal-keeps-table-source-editable
  (let [source "| A |\n|---|\n| B |\n" parse md/parse]
    (with-redefs [md/parse (fn [text]
                            (if (= source text) (parse text)
                              (throw (ex-info "Encoded table exceeds parser bound" {:type ::md/invalid}))))]
      (is (nil? (model source)))))
  (let [source "| A |\n|---|\n| B |\n" parse md/parse]
    (with-redefs [md/parse (fn [text]
                            (if (= source text) (parse text)
                              (throw (ex-info "Unexpected parser failure" {:type ::unexpected}))))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unexpected parser failure" (model source))))))

(deftest literal-backslashes-before-table-code-pipes-survive-editing
  (doseq [n (range 3)]
    (let [slashes (apply str (repeat n "\\"))
          source (str "| `a" slashes "\\|b` |\n|---|\n| x |\n")
          original (model source)
          decoded (some-> original rich/markdown model)]
      (is (some? original) source)
      (is (= (str "a" slashes "|b") (get-in original [:content 0 :content 0 :content 0 :content 0 :content 0 :text])))
      (is (= (rich/roundtrip-normalize original) (rich/roundtrip-normalize decoded))))))
