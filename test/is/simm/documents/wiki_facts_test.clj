(ns is.simm.documents.wiki-facts-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.wiki-facts :as facts]))

(def repository #uuid "0418db64-3755-45f1-b522-891941563963")
(def page "1cce4d07-3c56-4678-b348-1af7eb2d49ed")
(def parent "9ec3553a-8879-4056-8e48-28593b9e2815")
(def child "c122c4df-7c50-41f9-b046-634cc34395a4")
(def guard (constantly nil))

(defn- frontmatter [body]
  (str "---\nsimmis-id: " page "\nrevenue: 0\nactive: false\n---\n" body))

(defn- block [id body]
  (str "<!-- simmis:block " id " -->\n\n" body "\n\n<!-- /simmis:block -->\n\n"))

(defn- observe [source] (facts/document repository "wiki/Company.md" source guard))

(deftest version-two-keeps-only-real-links-and-visible-prose
  (let [source (frontmatter
                (str "# Company\n\nVisible claim [inline](/in/inline.md) and [reference][used].\n\n"
                     "![image](/in/image.md) ![reference image][picture]\n\n"
                     "![alt [nested link](/in/nested.md)](/in/outer-image.md)\n\n"
                     "`inline-secret [code](/in/inline-code.md)`\n\n"
                     "```md\nfenced-secret [code](/in/fenced.md)\n```\n\n"
                     "~~~md\ntilde-secret [code](/in/tilde.md)\n~~~\n\n"
                     "    indented-secret [code](/in/indented.md)\n\n"
                     "<!-- comment-secret [comment](/in/comment.md) -->\n\n"
                     "[used]: /in/reference.md\n[picture]: /in/reference-image.md\n[unused]: /in/unused.md\n"))
        legacy (observe source)
        current (facts/document repository "wiki/Company.md" source guard 2)]
    (is (= legacy (facts/document repository "wiki/Company.md" source guard 1)))
    (is (not (contains? legacy :prose-text)))
    (is (some #{"/in/image.md"} (:links legacy)) "V1 keeps its historical image observation")
    (is (= ["/in/inline.md" "/in/reference.md"] (:links current)))
    (is (re-find #"Visible claim" (:prose-text current)))
    (is (not (re-find #"secret|nested link|reference image" (:prose-text current))))
    (is (= facts/citation-profile (:profile (facts/documents repository {"wiki/Company.md" source} guard 2))))
    (is (= (:metadata-digest legacy) (:metadata-digest current)))
    (is (= (:page-id legacy) (:page-id current)))))

(deftest unsupported-observations-refuse-before-parsing
  (with-redefs [markdown/parse (fn [& _] (throw (AssertionError. "Unsupported mode must not parse")))]
    (is (thrown? clojure.lang.ExceptionInfo (facts/document repository "wiki/A.md" "A" guard 3)))
    (is (thrown? clojure.lang.ExceptionInfo (facts/documents repository {} guard 3)))))

(deftest version-two-traversal-respects-the-scoring-resource-guard
  (let [calls (atom 0)
        stopped (ex-info "Scoring traversal cancelled" {})
        source (frontmatter (str "# Company\n\n" (apply str (repeat 100 "[text](/in/source.md) "))))]
    (is (identical? stopped
                    (try (facts/document repository "wiki/Company.md" source
                                         #(when (> (swap! calls inc) 30) (throw stopped)) 2)
                         nil (catch clojure.lang.ExceptionInfo error error))))))

(deftest metadata-preservation-is-semantic-and-keeps-zero-and-false
  (let [source (frontmatter "# Company\n")
        a (observe source)
        reordered (observe (str "---\nactive: false\nrevenue: 0\nsimmis-id: '" page "'\n---\n# Renamed heading\n"))
        changed (observe (str "---\nsimmis-id: " page "\nrevenue: 1\nactive: false\n---\n# Company\n"))]
    (is (:valid? a))
    (is (= page (:page-id a)))
    (is (= :explicit (:identity-status a)))
    (is (= (:metadata-digest a) (:metadata-digest reordered)))
    (is (not= (:revision a) (:revision reordered)))
    (is (not= (:metadata-digest a) (:metadata-digest changed)))
    (is (not (contains? a :source)))
    (is (not (contains? a :raw)))))

(deftest explicit-ancestry-and-relative-order-ignore-provisional-paragraphs
  (let [nested (block parent (str "Parent\n\n" (block child "Child")))
        a (observe (frontmatter nested))
        prefixed (observe (frontmatter (str "# Another unmarked heading\n\n" nested)))
        reordered (observe (frontmatter (str (block child "Child") (block parent "Parent"))))]
    (is (:valid? a))
    (is (= [{:id parent :parent nil} {:id child :parent parent}] (:blocks a)))
    (is (= (:blocks a) (:blocks prefixed)))
    (is (not= (:blocks a) (:blocks reordered)))
    (is (= [page parent child] (:ids a)))))

(deftest ordinary-citations-come-from-syntax-not-code-comments-or-wikilinks
  (let [result (observe
                (frontmatter
                 (str "[Evidence](/in/knowledge/sources/inbox/note.md)\n\n"
                      "`[inline code](/in/knowledge/sources/inbox/fake1.md)`\n\n"
                      "```markdown\n[Code](/in/knowledge/sources/inbox/fake2.md)\n```\n\n"
                      "<!-- [Comment](/in/knowledge/sources/inbox/fake3.md) -->\n\n"
                      "[Reference][evidence]\n\n[evidence]: /in/knowledge/sources/inbox/reference.md\n\n"
                      "[unused]: /in/knowledge/sources/inbox/unused.md\n\n"
                      "[[wiki/Other]]\n")))]
    (is (:valid? result))
    (is (= ["/in/knowledge/sources/inbox/note.md" "/in/knowledge/sources/inbox/reference.md"] (:links result)))))

(deftest malformed-metadata-and-identity-never-become-valid-structural-facts
  (doseq [source [(str "---\nsimmis-id: " page "\nrevenue: 0\nrevenue: 1\n---\n# Body\n")
                  "---\nsimmis-id: bad\n---\n# Body\n"
                  (frontmatter (str "<!-- simmis:block " parent " -->\nUnclosed\n"))
                  (frontmatter (str (block parent "A") (block parent "B")))]]
    (let [result (observe source)]
      (is (false? (:valid? result)))
      (is (seq (:diagnostic-kinds result))))))

(deftest bounds-refuse-before-parsing-and-resource-guards-are-not-swallowed
  (with-redefs [markdown/parse (fn [& _] (throw (AssertionError. "No parse beyond the per-file bound")))]
    (is (= {:path "wiki/Company.md" :valid? false :diagnostic-kinds [:file-too-large]}
           (observe (apply str (repeat (inc (:max-file-bytes facts/profile)) "x"))))))
  (let [calls (atom 0) stopped (ex-info "Scoring stopped" {:type ::stopped})]
    (is (identical? stopped
                    (try (facts/document repository "wiki/Company.md" (frontmatter "# Body\n")
                                         #(when (= 3 (swap! calls inc)) (throw stopped)))
                         nil (catch clojure.lang.ExceptionInfo error error)))))
  (is (thrown? clojure.lang.ExceptionInfo
               (facts/documents repository (zipmap (map str (range 101)) (repeat "")) guard))))

(deftest returned-facts-have-only-portable-immutable-values
  (let [result (facts/documents repository {"wiki/Company.md" (frontmatter (block parent "Body"))} guard)
        portable? (fn [value]
                    (or (nil? value) (string? value) (keyword? value) (boolean? value) (integer? value)
                        (map? value) (vector? value) (set? value) (map-entry? value)))]
    (is (= facts/profile (:profile result)))
    (is (every? portable? (tree-seq coll? seq result)))
    (is (not-any? uuid? (tree-seq coll? seq result)))))
