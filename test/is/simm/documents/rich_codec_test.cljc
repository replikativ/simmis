(ns is.simm.documents.rich-codec-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.documents.rich-text :as rich]))

(defn- doc [text mark]
  {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text text :marks [{:type mark}]}]}]})

(deftest formatting-is-identical-in-the-jvm-and-browser
  (is (= "**one region**" (rich/markdown (doc "one region" "bold"))))
  (is (= " *italic* " (rich/markdown (doc " italic " "italic"))))
  (is (= "~~gone~~" (rich/markdown (doc "gone" "strike"))))
  (is (= "  " (rich/markdown (doc "  " "bold"))))
  (is (= "**🦋**" (rich/markdown (doc "🦋" "bold")))))

(deftest no-op-and-edits-preserve-the-surrounding-source-in-both-runtimes
  (let [source "front\r\n\r\n**old**\r\n\r\n[[opaque]]\r\n"
        slot {:from 9 :to 18 :model (doc "old" "bold")}]
    (is (= source (rich/apply-region source slot (:model slot))))
    (is (= "front\r\n\r\n**new**\r\n\r\n[[opaque]]\r\n"
           (rich/apply-region source slot (doc "new" "bold"))))))

(deftest empty-editor-documents-normalize-to-empty-markdown
  (is (= {:type "doc" :content []}
         (rich/normalize {:type "doc" :content [{:type "paragraph" :content []}]}))))
