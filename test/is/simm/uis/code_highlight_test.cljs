(ns is.simm.uis.code-highlight-test
  (:require [cljs.test :refer-macros [deftest is]]
            [is.simm.uis.web.desktop.code-highlight :as highlight]))

(deftest explicit-languages-and-aliases-are-presentation-only
  (doseq [language ["clojure" "clj" "cljc" "cljs" "edn" "bb" "clojure :results value"]]
    (is (= "clojure" (highlight/language language)))
    (is (seq (highlight/token-ranges "(def answer 42) ; comment" language))))
  (doseq [language [nil "" "unknown" "clojure<script>" "javascript:alert(1)"]]
    (is (empty? (highlight/token-ranges "(def answer 42)" language)))))

(deftest read-highlighting-preserves-text-and-shares-language-and-page-bounds
  (let [text "; comment\r\n(def title \"😀 <script>\")"
        code (fn [text] {:type "codeBlock" :attrs {:language "clj"} :content [{:type "text" :text text}]})
        model {:type "doc" :content [(code text)]}
        parts (get (highlight/document-segments [model]) [0 0])
        large (code (apply str (repeat 16384 "x")))
        page (highlight/document-segments [{:type "doc" :content [large large large large (code text)]}])]
    (is (= text (apply str (map :text parts))))
    (is (some :class parts))
    (is (nil? (get page [0 4])))
    (is (= [{:text text}] (highlight/segments text "unknown-language")))))

(deftest utf16-and-original-line-endings-match-the-highlighted-token-ranges
  (let [code "; note\r\n(def title \"😀 <tag>\")\r\n(+ 1 2)"
        tokens (highlight/token-ranges code "clj")]
    (is (seq tokens))
    (is (some #(and (re-find #"hljs-string" (:class %))
                     (= "\"😀 <tag>\"" (subs code (:from %) (:to %)))) tokens))
    (is (every? #(<= 0 (:from %) (:to %) (count code)) tokens))))

(deftest token-markup-has-a-closed-grammar-and-exact-original-text
  (is (= [{:from 0 :to 1 :class "hljs-title function_"}]
         (highlight/html-ranges "x" "<span class=\"hljs-title function_\">x</span>")))
  (is (= [{:from 0 :to 1 :class "language-javascript hljs-number"}]
         (highlight/html-ranges "1" "<span class=\"language-javascript\"><span class=\"hljs-number\">1</span></span>")))
  (doseq [html ["<span class=\"hljs-string\">wrong</span>"
                "<span class=\"hljs-string\" onclick=\"alert(1)\">x</span>"
                "<a class=\"hljs-string\">x</a>"
                "<span class=\"arbitrary-class\">x</span>"
                "<!--x-->x"]]
    (is (empty? (highlight/html-ranges "x" html))))
  (is (empty? (highlight/html-ranges "x" (apply str (repeat (inc highlight/max-highlight-html) "x")))))
  (is (empty? (highlight/token-ranges (apply str (repeat (inc highlight/max-cell-chars) "x")) "clj"))))
