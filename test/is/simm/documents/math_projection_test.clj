(ns is.simm.documents.math-projection-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.files :as files]
            [is.simm.documents.rich :as host]
            [is.simm.documents.rich-text :as rich]
            [is.simm.documents.rich-document :as document]))

(def repository #uuid "d810af0a-c021-40e9-a6c1-f81591953e50")
(def path "wiki/formulas.md")
(defn- formulas [model]
  (filter #(contains? #{"inlineMath" "displayMath"} (:type %))
          (tree-seq #(seq (:content %)) :content model)))

(deftest nearby-prose-edits-preserve-exact-frozen-tex-and-prefixes
  (doseq [newline ["\n" "\r\n"]]
    (let [prefix (str "\uFEFF---" newline "title: '🦋'" newline "---" newline)
          source (str prefix "Before *$x_1 + \\alpha$* and $y^2$." newline newline
                      "$$" newline "\\int_0^1 x^2 dx" newline "$$" newline)
          analysis (files/analyze repository path source)
          slot (first (:slots analysis)) model (:model slot)
          edited (assoc-in model [:content 0 :content 0 :text] "Changed ")
          result (host/apply-edit repository path source (:revision analysis) (:from slot) (:to slot) edited)
          after (files/analyze repository path result)
          projection (document/project source analysis)]
      (is (= source (document/apply-model source projection (:model projection))))
      (is (= ["$x_1 + \\alpha$" "$y^2$"] (mapv #(get-in % [:attrs :raw]) (formulas model))))
      (is (= (rich/roundtrip-normalize edited)
             (rich/roundtrip-normalize (get-in after [:slots 0 :model]))))
      (is (str/starts-with? result prefix))
      (is (str/ends-with? result (subs source (:to slot))))
      (doseq [node (mapcat #(tree-seq (comp seq :children) :children %) (:nodes analysis))
              :when (:math node)]
        (is (= (get-in node [:math :raw]) (subs source (:from node) (:to node))))
        (is (= (get-in node [:math :tex])
               (subs source (get-in node [:math :content-from]) (get-in node [:math :content-to]))))))))

(deftest source-backed-math-cannot-be-forged-duplicated-or-borrowed
  (let [source "Before $x$.\n\nOther $z$.\n"
        slots (:slots (files/analyze repository path source))
        slot (first slots) model (:model slot)
        index (first (keep-indexed #(when (= "inlineMath" (:type %2)) %1) (get-in model [:content 0 :content])))
        at [:content 0 :content index]
        token (get-in model at)
        foreign (first (formulas (:model (second slots))))]
    (doseq [edited [(assoc-in model (conj at :attrs :tex) "injected")
                    (assoc-in model (conj at :attrs :raw) "$forged$")
                    (assoc-in model (conj at :attrs :sourceKey) "math:999:1000")
                    (assoc-in model (conj at :type) "displayMath")
                    (assoc-in model at foreign)
                    (update-in model [:content 0 :content] conj token)]]
      (is (thrown? clojure.lang.ExceptionInfo (rich/apply-region source slot edited)))
      (is (thrown? clojure.lang.ExceptionInfo
                   (host/apply-edit repository path source (markdown/revision source) (:from slot) (:to slot) edited))))))

(deftest typed-dollar-text-does-not-implicitly-create-active-formulas
  (let [source "Text.\n" slot (first (:slots (files/analyze repository path source)))
        edited (assoc-in (:model slot) [:content 0 :content 0 :text] "Cost $20 and literal $x$.")
        result (host/apply-edit repository path source (markdown/revision source) (:from slot) (:to slot) edited)
        model (get-in (files/analyze repository path result) [:slots 0 :model])]
    (is (empty? (formulas model)))
    (is (= (rich/normalize edited) (rich/normalize model)))))

(deftest frozen-display-newlines-survive-neighbor-edits-and-continuous-insertion
  (doseq [newline ["\n" "\r\n" "\r"]]
    (let [raw (str "$$" newline "\\int_0^1 x^2 dx" newline "$$")
          source (str "Before." newline newline raw newline newline "After." newline)
          analysis (files/analyze repository path source) slots (:slots analysis)
          baseline {:type "doc" :content (vec (mapcat #(get-in % [:model :content]) slots))}
          edited (assoc-in baseline [:content 0 :content 0 :text] "Changed")
          together (rich/apply-region source {:from (:from (first slots)) :to (:to (peek slots))
                                              :model baseline} edited)
          continuous (document/apply-run source slots edited)
          moved (update baseline :content #(vec (concat [(first %)] [(last %)] [(second %)])))
          inserted (document/apply-run source slots moved)]
      (doseq [result [together continuous inserted]]
        (is (str/includes? result raw))
        (is (= [raw] (mapv #(get-in % [:math :raw])
                           (filter #(= :DisplayMath (:kind %)) (:nodes (markdown/parse result)))))))
      (is (str/starts-with? together "Changed"))
      (is (str/starts-with? continuous "Changed")))))
