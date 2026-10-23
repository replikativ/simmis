(ns is.simm.uis.web.desktop.syntax-highlight
  "Shared shipped grammars. This registry contains no document or UI state."
  (:require ["highlight.js/lib/core" :as hljs]
            ["highlight.js/lib/languages/clojure" :as clojure-lang]
            ["highlight.js/lib/languages/javascript" :as js-lang]
            ["highlight.js/lib/languages/python" :as python-lang]
            ["highlight.js/lib/languages/bash" :as bash-lang]
            ["highlight.js/lib/languages/json" :as json-lang]
            ["highlight.js/lib/languages/css" :as css-lang]
            ["highlight.js/lib/languages/xml" :as xml-lang]
            ["highlight.js/lib/languages/sql" :as sql-lang]
            ["highlight.js/lib/languages/diff" :as diff-lang]
            ["highlight.js/lib/languages/yaml" :as yaml-lang]
            ["highlight.js/lib/languages/markdown" :as md-lang]
            ["./superficie_hljs.js" :as sup-hljs]))

(def ^js engine hljs)

(doseq [[name grammar] [["clojure" clojure-lang] ["javascript" js-lang] ["js" js-lang]
                        ["python" python-lang] ["bash" bash-lang] ["shell" bash-lang]
                        ["json" json-lang] ["css" css-lang] ["html" xml-lang] ["xml" xml-lang]
                        ["sql" sql-lang] ["diff" diff-lang] ["yaml" yaml-lang]
                        ["markdown" md-lang] ["superficie" sup-hljs]]]
  (.registerLanguage engine name grammar))
