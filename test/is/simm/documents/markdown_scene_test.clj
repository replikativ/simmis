(ns is.simm.documents.markdown-scene-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [is.simm.documents.files :as files]
            [is.simm.documents.attachments :as attachments]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.markdown-scene :as scene]
            [is.simm.documents.rich-document :as document]))

(def source "| ns | what |\r\n| :--- | ---: |\r\n| `x` | **Bold** and [link](https://example.test) |\r\n\r\n> Quoted **text**\r\n\r\n<!-- hidden -->\r\n")

(deftest parsed-table-and-quote-render-without-changing-the-source-model
  (let [analysis (files/analyze (random-uuid) "README.md" source)
        previews (scene/index source analysis)
        table (first (vals (select-keys previews ["opaque:0"])))
        nodes (tree-seq map? :children (:scene table))
        projection (document/project source analysis)]
    (is (= :table (get-in table [:scene :tag])))
    (is (= 2 (count (filter #(= :th (:tag %)) nodes))))
    (is (= 2 (count (filter #(= :td (:tag %)) nodes))))
    (is (some #(= :code (:tag %)) nodes))
    (is (some #(= :strong (:tag %)) nodes))
    (is (some #(= "memory-align-right" (get-in % [:attrs :class])) nodes))
    (is (some #(= "https://example.test" (get-in % [:attrs :href])) nodes))
    (is (some #(= :blockquote (get-in % [:scene :tag])) (vals previews)))
    (is (some #(nil? (:scene %)) (vals previews)) "Comments are preserved, not displayed")
    (is (= source (document/apply-model source projection (:model projection))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (document/edits projection (assoc-in (:model projection) [:content 1 :attrs :label] "forged"))))))

(deftest raw-html-images-and-unsafe-links-never-create-active-elements
  (let [text "> [bad](javascript:alert(1)) <img src=https://example.test/x onerror=alert(1)>\n"
        preview (scene/preview text (first (:nodes (markdown/parse text))))
        nodes (tree-seq map? :children (:scene preview))]
    (is (not-any? #(#{:img :script :iframe} (:tag %)) nodes))
    (is (not-any? #(get-in % [:attrs :href]) nodes))
    (is (str/includes? (apply str (filter string? nodes)) "<img"))))

(deftest oversized-previews-retain-the-source-fallback
  (let [text (str "> " (apply str (repeat (inc scene/max-preview-chars) "x")) "\n")
        parsed (markdown/parse text)]
    (is (nil? (scene/preview text (first (:nodes parsed)))))
    (is (= {} (scene/index text parsed)))))

(deftest editing-near-a-table-retains-its-exact-original-markdown
  (let [text (str "Before\r\n\r\n" source)
        analysis (files/analyze (random-uuid) "README.md" text)
        projection (document/project text analysis)
        edited (assoc-in (:model projection) [:content 0 :content 0 :content 0 :text] "Revised")]
    (is (= (str/replace-first text "Before" "Revised") (document/apply-model text projection edited)))))

(deftest relative-images-use-resolved-metadata-without-changing-original-markdown
  (let [text "Before\r\n\r\n![Screen](images/frame.png)\r\n\r\n![Ref][frame]\r\n\r\n[frame]: images/frame.png\r\n"
        id (random-uuid) analysis (files/analyze id "captures/observations.md" text)
        urls (attachments/urls (str id) "captures/observations.md" analysis
                               {"captures/images/frame.png" {:content id :size 12 :mode 33188}} id)
        previews (scene/index text analysis urls)
        images (mapcat #(filter (fn [node] (= :img (:tag node)))
                                (tree-seq map? :children (:scene %))) (vals previews))
        projection (document/project text analysis)]
    (is (= 2 (count images)))
    (is (every? #(= (get urls "images/frame.png") (get-in % [:attrs :src])) images))
    (is (= text (document/apply-model text projection (:model projection))))
    (is (not-any? #(some (fn [node] (= :img (:tag node))) (tree-seq map? :children (:scene %)))
                  (vals (scene/index text analysis))))))
