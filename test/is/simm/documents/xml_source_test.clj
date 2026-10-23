(ns is.simm.documents.xml-source-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.xml-source :as source]))

(deftest locator-keeps-exact-lexical-boundaries
  (let [xml (str "\uFEFF<?xml version='1.0' encoding='UTF-8'?>\r\n"
                 "<root attr='a>b'><!-- ignored <x/> --><?test <ignored?>"
                 "<t xml:space='preserve'>A &amp; &#x1F98B; B</t>"
                 "<t>duplicate</t><t>duplicate</t><empty/>"
                 "<mixed>a<![CDATA[b<c]]>d</mixed></root>")
        nodes (source/locate xml)]
    (is (= ["root" "t" "t" "t" "empty" "mixed"] (mapv :qname nodes)))
    (is (= [false true true true false false] (mapv :plain? nodes)))
    (is (= "A &amp; &#x1F98B; B" (subs xml (:content-from (nodes 1)) (:content-to (nodes 1)))))
    (let [patched (markdown/decode (source/replace-content xml (nodes 3) "<safe> &amp; 🦋"))]
      (is (str/includes? patched "<t>duplicate</t><t>&lt;safe&gt; &amp;amp; 🦋</t>"))
      (is (= (subs xml 0 (:content-from (nodes 3))) (subs patched 0 (:content-from (nodes 3)))))
      (is (str/ends-with? patched (subs xml (:content-to (nodes 3))))))
    (is (= nodes (source/checked-locations xml (mapv #(select-keys % [:qname]) nodes))))
    (is (thrown? clojure.lang.ExceptionInfo (source/checked-locations xml [{:qname "different"}])))))

(deftest locator-refuses-unlocatable-or-unsupported-spans
  (doseq [xml ["<a><b></a>" "<a attr='unterminated>" "<a><!--" "<!DOCTYPE a><a/>" "<a>"]]
    (is (thrown? clojure.lang.ExceptionInfo (source/locate xml))))
  (doseq [xml ["<t/>" "<t>a<!-- comment -->b</t>" "<t><![CDATA[text]]></t>" "<t>a<?pi?>b</t>" "<t><child/></t>"]]
    (is (thrown? clojure.lang.ExceptionInfo (source/replace-content xml (first (source/locate xml)) "replacement"))))
  (let [xml "<t>before</t>" target (first (source/locate xml))]
    (doseq [text ["line\nbreak" "tab\there" "\u0000" (apply str (repeat 10001 "x"))]]
      (is (thrown? clojure.lang.ExceptionInfo (source/replace-content xml target text))))
    (is (thrown? java.nio.charset.CharacterCodingException (source/replace-content xml target (str (char 0xD800)))))))
