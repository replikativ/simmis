(ns is.simm.uis.superficie-test
  (:require [cljs.test :refer-macros [deftest is]]
            [clojure.string :as str]
            [superficie.api :as superficie]
            [is.simm.uis.web.desktop.markdown :as markdown]))

(deftest browser-projection-keeps-number-and-character-spellings
  (let [tokens ["4.0" "1e3" "0xFF" "1/2" "2N" "1.5M" "\\u0041"]
        rendered (superficie/toSup (str "[" (str/join " " tokens) "]"))]
    (doseq [token tokens] (is (str/includes? rendered token)))))

(deftest browser-projection-keeps-both-reader-conditional-branches
  (let [rendered (superficie/toSup "#?(:clj (jvm-function) :cljs (browser-function))")]
    (doseq [part ["#?(" ":clj" "jvm-function" ":cljs" "browser-function"]]
      (is (str/includes? rendered part)))))

(deftest vendored-highlighter-recognizes-current-arrow-syntax
  (let [html (markdown/highlight-code "x -> x + 1\nA[i] <- v" "superficie")]
    (is (str/includes? html "hljs-operator"))
    (is (str/includes? html "-&gt;"))
    (is (str/includes? html "&lt;-"))))
