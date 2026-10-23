(ns is.simm.documents.math-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.math :as math])
  (:import [com.vladsch.flexmark.parser Parser]))

(defn- tokens [source]
  (let [parser (-> (Parser/builder) (.extensions [(math/extension)]) .build)
        document (.parse parser source)]
    (into [] (keep (fn [node]
                    (when-let [data (math/token-data node)]
                      (assoc data :kind (.getNodeName node)
                                  :from (.getStartOffset node) :to (.getEndOffset node)))))
          (iterator-seq (.iterator (.getDescendants document))))))

(deftest math-composes-with-the-production-parser-extensions
  (let [production-parser ((ns-resolve 'is.simm.documents.markdown 'parser))
        parser production-parser
        source (str "[$x$](https://example.org/$destination$) [^n] [[Page]]\n\n"
                    "[^n]: A note with $y$.\n\n"
                    "<span data-formula=\"$attribute$\">$z$</span>\n\n"
                    "$a$$ $$$x$$$\n")
        nodes (vec (iterator-seq (.iterator (.getDescendants (.parse parser source)))))
        found (into [] (keep math/token-data) nodes)]
    (is (= ["x" "y" "z"] (mapv :tex found)))
    (is (some #(= "Footnote" (.getNodeName %)) nodes))
    (is (some #(= "WikiLink" (.getNodeName %)) nodes))
    (doseq [{:keys [raw content-from content-to tex]} found]
      (is (= tex (subs source content-from content-to)))
      (is (str/starts-with? raw "$")))))

(deftest tex-is-recognized-before-markdown-escape-and-emphasis-processing
  (let [source "Emoji 😀, *before $x_1 + \\alpha * y$ after*, [$z$](https://example.org)."
        found (tokens source)]
    (is (= ["x_1 + \\alpha * y" "z"] (mapv :tex found)))
    (doseq [{:keys [raw tex from to content-from content-to kind]} found]
      (is (= "InlineMath" kind))
      (is (= raw (subs source from to)))
      (is (= tex (subs source content-from content-to))))))

(deftest currency-escapes-code-and-incomplete-input-remain-ordinary-markdown
  (doseq [source ["$20,000 and $30,000" "$ a$" "$a $" "$x$2" "$x"
                  "\\$x$" "`$x$`" "```tex\n$x$\n$$\na\n$$\n```"
                  "    $x$\n" "$$x$$" "$a$$" "$$$x$$$" "$a\nb$"]]
    (is (empty? (tokens source)) (pr-str source)))
  (is (= ["x"] (mapv :tex (tokens "\\\\$x$"))) "even backslash parity permits math")
  (is (= ["a\\$b"] (mapv :tex (tokens "$a\\$b$"))) "escaped closing delimiter stays TeX")
  (is (= ["a\\$"] (mapv :tex (tokens "$a\\$$"))) "escaped final dollar belongs to TeX")
  (is (= ["a\\\\"] (mapv :tex (tokens "$a\\\\$"))) "even closing parity ends math"))

(deftest malformed-delimiters-do-not-consume-a-later-independent-formula
  (doseq [source ["$a$$ then $b$" "$a $ then $b$" "$x$2 then $b$"]]
    (is (= ["b"] (mapv :tex (tokens source))) source)))

(deftest complete-display-blocks-preserve-newlines-and-protect-tex-from-block-syntax
  (doseq [newline ["\n" "\r\n" "\r"]]
    (let [tex (str "\\sum_{i=1}^n x_i" newline "# TeX, not a heading" newline "- TeX, not a list" newline)
          raw (str "$$" newline tex "$$")
          source (str "Before." newline newline raw newline newline "After.")
          [data :as found] (tokens source)]
      (is (= 1 (count found)))
      (is (= "DisplayMath" (:kind data)))
      (is (true? (:display? data)))
      (is (= raw (:raw data)))
      (is (= tex (:tex data)))
      (is (= raw (subs source (:from data) (:to data))))
      (is (= tex (subs source (:content-from data) (:content-to data)))))))

(deftest display-admission-does-not-claim-unclosed-oversized-or-container-source
  (doseq [source ["$$\nunclosed\n" "$$\na\n\nb\n$$\n"
                  "> $$\n> x\n> $$\n" "- $$\n  x\n  $$\n"
                  (str "$$\n" (str/join "\n" (repeat 65 "x")) "\n$$\n")
                  (str "$$\n" (apply str (repeat math/max-expression-chars "x")) "\n$$\n")]]
    (is (empty? (tokens source)) (subs source 0 (min 60 (count source))))))

(deftest parser-local-count-and-expression-limits-do-not-leak-between-documents
  (is (= math/max-document-expressions
         (count (tokens (str/join " " (repeat (+ 10 math/max-document-expressions) "$x$"))))))
  (is (empty? (tokens (str "$" (apply str (repeat math/max-expression-chars "x")) "$"))))
  (is (= ["x"] (mapv :tex (tokens "$x$")))))

(deftest failed-lookahead-is-bounded-and-depletion-leaves-later-source-inert
  (with-redefs [math/max-document-lookahead 5]
    (is (empty? (tokens "$unfinished then $x$")))
    (is (empty? (tokens "$$\nlong expression\n$$\n\n$x$")))
    (is (= ["x"] (mapv :tex (tokens "$x$ then $long$")))))
  (is (= ["x"] (mapv :tex (tokens "$x$"))) "a new parser gets a new budget"))
