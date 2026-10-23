(ns is.simm.documents.rich-codec-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.documents.rich-text :as rich]))

(defn- doc [text mark]
  {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text text :marks [{:type mark}]}]}]})

(deftest formatting-is-identical-in-the-jvm-and-browser
  (is (= "**one region**" (rich/markdown (doc "one region" "bold"))))
  (is (= "*&#32;italic&#32;*" (rich/markdown (doc " italic " "italic"))))
  (is (= "~~gone~~" (rich/markdown (doc "gone" "strike"))))
  (is (= "**&#32;&#32;**" (rich/markdown (doc "  " "bold"))))
  (is (= "**🦋**" (rich/markdown (doc "🦋" "bold")))))

(deftest literal-entities-and-typing-whitespace-remain-text-in-both-runtimes
  (doseq [[text expected] [["Sentence. " "Sentence\\.&#32;"]
                           [" " "&#32;"] ["\tLeading" "&#9;Leading"]
                           ["A & B" "A &amp; B"]
                           ["&#32;" "&amp;\\#32;"]
                           ["&amp;lt;" "&amp;amp;lt;"]]]
    (is (= expected
           (rich/markdown {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text text}]}]})))))

(deftest no-op-and-edits-preserve-the-surrounding-source-in-both-runtimes
  (let [source "front\r\n\r\n**old**\r\n\r\n[[opaque]]\r\n"
        slot {:from 9 :to 18 :model (doc "old" "bold")}]
    (is (= source (rich/apply-region source slot (:model slot))))
    (is (= "front\r\n\r\n**new**\r\n\r\n[[opaque]]\r\n"
           (rich/apply-region source slot (doc "new" "bold"))))))

(deftest empty-editor-documents-normalize-to-empty-markdown
  (is (= {:type "doc" :content []}
         (rich/normalize {:type "doc" :content [{:type "paragraph" :content []}]}))))

(deftest empty-parsed-list-items-project-to-editable-paragraphs-in-both-runtimes
  (doseq [[kind type] [[:BulletList "bulletList"] [:OrderedList "orderedList"]]]
    (let [node {:kind kind :start 3 :children [{:kind (if (= kind :BulletList) :BulletListItem :OrderedListItem)
                                             :children []}]}
          model (rich/model node)]
      (is (= type (get-in model [:content 0 :type])))
      (is (= [{:type "paragraph" :content []}] (get-in model [:content 0 :content 0 :content])))
      (is (= (if (= kind :BulletList) "- " "3. ") (rich/markdown model))))))
