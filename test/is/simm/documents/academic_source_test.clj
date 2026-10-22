(ns is.simm.documents.academic-source-test
  "Acceptance corpus for source fidelity, not a claim of math/citation rendering."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [is.simm.documents.source-fixtures :as fixtures]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.blocks :as blocks]
            [is.simm.documents.rich-text :as rich]
            [is.simm.documents.rich-document :as document]
            [is.simm.documents.rich :as host]))

(def notebook (:source (first (filter #(= :academic-notebook (:id %)) fixtures/cases))))
(def document-id #uuid "edca3a84-0361-4829-85fe-611bb63743cc")

(defn- analysis [source]
  (let [parsed (markdown/parse source)]
    (assoc parsed :semantic (blocks/project document-id parsed)
           :slots (mapv #(assoc (select-keys % [:from :to :kind]) :model (rich/model %)) (:nodes parsed)))))

(deftest academic-source-remains-exact-in-both-newline-conventions
  (doseq [source [notebook (str/replace notebook "\n" "\r\n")]]
    (let [parsed (analysis source) projection (document/project source parsed)]
      (is (= source (:source parsed)))
      (is (= :ready (get-in parsed [:semantic :status])))
      (is (= source (document/apply-model source projection (:model projection))))
      (is (empty? (document/edits projection (:model projection))))
      (is (empty? (markdown/references parsed)) "Literal code examples are not backlinks")
      (is (= "references.bib" (get-in parsed [:frontmatter :properties "bibliography"]))))))

(deftest neighboring-rich-edit-does-not-rewrite-academic-syntax-or-code
  (doseq [source [notebook (str/replace notebook "\n" "\r\n")]]
   (let [parsed (analysis source) heading (first (:nodes parsed))
        model {:type "doc" :content [{:type "heading" :attrs {:level 1}
                                      :content [{:type "text" :text "Edited experiment notebook 🦋"}]}]}
        result (host/apply-edit document-id "wiki/experiment.md" source (:revision parsed)
                                (:from heading) (:to heading) model)]
    (is (str/includes? result "# Edited experiment notebook 🦋"))
    (is (= (subs source (:to heading))
           (subs result (+ (:to heading) (count "Edited ")))))
    (is (= (:frontmatter parsed) (:frontmatter (markdown/parse result)))))))

(deftest current-profile-recognizes-footnotes-without-pretending-to-render-tex
  (let [parsed (markdown/parse notebook)
        nodes (mapcat #(tree-seq (comp seq :children) :children %) (:nodes parsed))
        kinds (set (map :kind nodes))
        code (first (filter #(= :FencedCodeBlock (:kind %)) nodes))]
    (is (contains? kinds :Footnote))
    (is (contains? kinds :FootnoteBlock))
    (is (str/includes? (:literal code) "$not-math$ [^not-a-note] [@not-a-citation]"))
    (is (not (contains? (get-in parsed [:syntax-profile :features]) :math)))
    (is (not (contains? (get-in parsed [:syntax-profile :features]) :citations)))))
