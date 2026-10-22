(ns is.simm.documents.footnote-reference-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.rich :as host]
            [is.simm.documents.rich-text :as rich]
            [is.simm.documents.rich-document :as document]
            [is.simm.documents.files :as files]))

(def repository #uuid "b848c440-6245-4d2b-a8ab-124d7a7849f0")
(def path "wiki/research.md")

(defn- paragraph [source]
  (first (filter #(= :Paragraph (:kind %)) (:nodes (markdown/parse source)))))

(defn- refs [model]
  (filter #(= "footnoteReference" (:type %)) (tree-seq #(seq (:content %)) :content model)))

(defn- apply-host [source node model]
  (host/apply-edit repository path source (markdown/revision source) (:from node) (:to node) model))

(deftest named-references-are-inline-and-retain-raw-spelling
  (doseq [newline ["\n" "\r\n"]
          prefix ["" (str "\uFEFF---" newline "title: '🦋'" newline "---" newline)]]
    (let [source (str prefix "Before[^ method ] and *emphasized[^missing]* and again[^missing]." newline newline
                      "[^ method ]: Definition with **formatting**." newline)
          node (paragraph source) model (rich/model node)
          edited (assoc-in model [:content 0 :content 0 :text] "Changed🦋")
          result (apply-host source node edited)]
      (is (some? model))
      (is (= ["[^ method ]" "[^missing]" "[^missing]"] (mapv #(get-in % [:attrs :raw]) (refs model))))
      (is (= ["method" "missing" "missing"] (mapv #(get-in % [:attrs :label]) (refs model))))
      (is (= 3 (count (set (map #(get-in % [:attrs :sourceKey]) (refs model))))))
      (is (= source (rich/apply-region source (assoc node :model model) model)))
      (is (= (str/replace source "Before" "Changed🦋")
             ;; Ordinary prose punctuation is serialized; footnote tokens/definitions are not.
             (str/replace result "\\." ".")))
      (is (= (subs source (:to node)) (subs result (+ (:to node) (- (count result) (count source)))))))))

(deftest frozen-provenance-rejects-forgery-before-normalization
  (let [source "Before[^one] and after.\n\nSeparate[^two].\n"
        node (paragraph source) model (rich/model node)
        index (first (keep-indexed #(when (= "footnoteReference" (:type %2)) %1) (get-in model [:content 0 :content])))
        at [:content 0 :content index]
        foreign (first (refs (rich/model (second (:nodes (markdown/parse source))))))
        malformed [(assoc-in model (conj at :attrs :raw) "[^forged]\n<!-- simmis:block -->")
                   (assoc-in model (conj at :attrs :label) "other")
                   (assoc-in model (conj at :attrs :sourceKey) "footnote:999:1000")
                   (assoc-in model (conj at :attrs :extra) "ignored?")
                   (assoc-in model (conj at :content) [{:type "text" :text "hidden"}])
                   (assoc-in model (conj at :text) "hidden")
                   (assoc-in model at foreign)
                   (update-in model [:content 0 :content] conj (get-in model at))]]
    (doseq [edited malformed]
      (is (thrown? clojure.lang.ExceptionInfo (rich/apply-region source (assoc node :model model) edited)))
      (is (thrown? clojure.lang.ExceptionInfo (apply-host source node edited))))
    (let [deleted (update-in model [:content 0 :content] #(vec (concat (take index %) (drop (inc index) %))))]
      (is (not (str/includes? (apply-host source node deleted) "[^one]"))))))

(deftest references-can-move-within-one-region-not-across-semantic-owners
  (let [source "First[^one].\n\nSecond.\n\n[^one]: Untouched definition.\n"
        analysis (files/analyze repository path source)
        slots (vec (take 2 (:slots analysis)))
        baseline {:type "doc" :content (vec (mapcat #(get-in % [:model :content]) slots))}
        ref (first (refs baseline))
        moved (-> baseline
                  (update-in [:content 0 :content] #(vec (remove (fn [n] (= "footnoteReference" (:type n))) %)))
                  (update-in [:content 1 :content] conj ref))
        local (document/apply-run source slots moved)
        result (host/apply-edit repository path source (:revision analysis)
                                (:from (first slots)) (:to (peek slots)) moved)]
    (is (= local result))
    (is (str/includes? result "Second\\.[^one]"))
    (is (str/ends-with? result "[^one]: Untouched definition.\n"))
    (is (= 1 (count (refs moved))))
    (let [projection (document/project source analysis)
          forged (update-in (:model projection) [:content 0 :content 0 :content] conj ref)]
      (is (thrown? clojure.lang.ExceptionInfo (document/edits projection forged))))))

(deftest token-location-is-only-ignored-for-validated-roundtrip-equivalence
  (let [a (rich/model (paragraph "A[^one].\n"))
        b (rich/model (paragraph "Longer[^one].\n"))
        same-text (assoc-in b [:content 0 :content 0 :text] "A")]
    (is (not= (rich/normalize a) (rich/normalize same-text)))
    (is (= (rich/roundtrip-normalize a) (rich/roundtrip-normalize same-text)))
    (is (thrown? clojure.lang.ExceptionInfo (rich/validate-provenance! a same-text)))))

(deftest native-attribute-size-is-part-of-the-model-budget
  (is (thrown? clojure.lang.ExceptionInfo
               (rich/validate-model! {:type "doc" :attrs {:hidden (apply str (repeat 2097153 "x"))}})))
  (is (thrown? clojure.lang.ExceptionInfo
               (rich/validate-model! {:type "doc" :attrs {:nested {:unbounded "x"}}})))
  (let [model (rich/model (paragraph "A[^one].\n"))]
    (is (thrown? clojure.lang.ExceptionInfo
                 (rich/normalize (assoc-in model [:content 0 :content 1 :attrs :raw]
                                           (apply str (repeat 2049 "x"))))))))

(deftest multiline-references-remain-source-only-without-newline-rewriting
  (doseq [newline ["\n" "\r\n"]]
    (let [source (str "Before[^ two" newline " lines ]." newline)
          node (paragraph source)
          ref (first (filter :footnote (tree-seq #(seq (:children %)) :children node)))
          analysis (files/analyze repository path source)
          projection (document/project source analysis)]
      (is (= (str "[^ two" newline " lines ]") (get-in ref [:footnote :raw])))
      (is (nil? (rich/model node)))
      (is (= source (document/apply-model source projection (:model projection)))))
    (let [source (str "# Title" newline newline "Before[^ two" newline " lines ]." newline)
          heading (first (:nodes (markdown/parse source)))
          edited (assoc-in (rich/model heading) [:content 0 :content 0 :text] "Changed")]
      (is (= (str/replace source "# Title" "# Changed") (apply-host source heading edited))))))

(deftest references-survive-nested-lists-in-an-explicit-semantic-block
  (let [source (str "<!-- simmis:block b735e81b-bc36-4b2e-9c3b-442f1ca26926 -->\n\n"
                    "- Parent[^one]\n  - Child[^one]\n\n<!-- /simmis:block -->\n\n[^one]: Definition.\n")
        node (first (filter #(= :BulletList (:kind %)) (:nodes (markdown/parse source))))
        model (rich/model node)
        edited (update-in model [:content 0 :content 0 :content 0 :content 0 :text] str " changed")
        result (apply-host source node edited)]
    (is (= 2 (count (refs model))))
    (is (= 2 (count (set (map #(get-in % [:attrs :sourceKey]) (refs model))))))
    (is (str/includes? result "Parent changed[^one]"))
    (is (str/includes? result "Child[^one]"))
    (is (str/ends-with? result "<!-- /simmis:block -->\n\n[^one]: Definition.\n"))))

(deftest deleting-one-repeated-reference-retains-the-other-occurrence
  (let [source "Before[^one] and again[^one].\n"
        node (paragraph source) model (rich/model node)
        first-ref (first (refs model)) second-ref (second (refs model))
        edited (update-in model [:content 0 :content] #(vec (remove #{first-ref} %)))
        result (apply-host source node edited)]
    (is (not= (:attrs first-ref) (:attrs second-ref)))
    (is (= 1 (count (refs edited))))
    (is (= 1 (count (re-seq #"\[\^one\]" result))))))

(deftest relocation-through-lcs-deletion-and-insertion-keeps-provenance
  (let [source "First[^one].\n\nSecond.\n\nThird.\n\n[^one]: Keep exact definition.\n"
        analysis (files/analyze repository path source) slots (vec (take 3 (:slots analysis)))
        baseline {:type "doc" :content (vec (mapcat #(get-in % [:model :content]) slots))}
        edited {:type "doc" :content [(second (:content baseline)) (nth (:content baseline) 2)
                                      {:type "paragraph" :content [(first (refs baseline))]}
                                      {:type "paragraph" :content [{:type "text" :text "Tail"}]}]}
        local (document/apply-run source slots edited)
        result (host/apply-edit repository path source (:revision analysis)
                                (:from (first slots)) (:to (peek slots)) edited)]
    (is (= local result))
    (is (not (str/includes? result "First")))
    (is (str/includes? result "Third.\n\n[^one]"))
    (is (str/ends-with? result "[^one]: Keep exact definition.\n"))))
