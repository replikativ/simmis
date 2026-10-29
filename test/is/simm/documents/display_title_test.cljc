(ns is.simm.documents.display-title-test
  (:require #?(:clj [clojure.test :refer [deftest is]] :cljs [cljs.test :refer-macros [deftest is]])
            [is.simm.documents.display-title :as display]))

(deftest authored-title-is-a-label-not-a-new-path-or-identity
  (let [document {:path "reports/benchmarks/166dc9e7-a032-505a-a23b-308975953ef3.md"
                  :frontmatter {:properties {"title" "  Knowledge maintenance results  "}}}]
    (is (= "Knowledge maintenance results" (display/title document)))
    (is (= "notes" (display/title {:path "wiki/notes.md"})))
    (doseq [label [nil "" "bad\ntitle" (apply str (repeat 257 "x"))]]
      (is (= "notes" (display/title {:path "wiki/notes.md" :frontmatter {:properties {"title" label}}}))))))
