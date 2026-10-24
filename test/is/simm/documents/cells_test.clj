(ns is.simm.documents.cells-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [is.simm.documents.cells :as cells]
            [is.simm.documents.code-cells :as code-cells]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.rich-document :as document]))

(defn saved [source]
  (let [repo (random-uuid) path "wiki/Cells.md"]
    {:repository-id repo :path path :kind :markdown :text source
     :entry {:content (markdown/revision source) :size (alength (markdown/source-bytes source)) :mode 33188}
     :analysis (files/analyze repo path source)}))

(defn fences [file]
  (filter #(= :FencedCodeBlock (:kind %))
          (mapcat #(tree-seq (comp seq :children) :children %) (get-in file [:analysis :nodes]))))

(defn request [file node]
  {:path (:path file) :repository-id (:repository-id file) :expected (:entry file)
   :revision (get-in file [:analysis :revision]) :from (:from node) :to (:to node)
   :block nil :namespace "user" :expected-target-stamp (random-uuid)})

(defn- projected [file] (document/project (:text file) (:analysis file)))
(defn- node-at [model path] (reduce (fn [node i] (nth (:content node) i)) model path))

(deftest supported-slot-correspondence-does-not-confuse-opaque-or-indented-code
  (doseq [[source expected-path]
          [["    (+ 9 1)\n\n```clj\n(+ 1 2)\n```\n" [0 1]]
           ["> ```clojure\n> (+ 9 1)\n> ```\n\n```clj\n(+ 1 2)\n```\n" [1 0]]
           ["<!-- simmis:block 5d9fb58d-ce00-42e2-8a56-66cec0b1879b -->\n\n```clojure\n(+ 1 2)\n```\n\n<!-- /simmis:block -->\n" [0 0 0]]]]
    (let [file (saved source) projection (projected file) [cell] (:cells projection)
          node (node-at (:model projection) (:node-path cell))]
      (is (= 1 (count (:cells projection))))
      (is (= expected-path (:node-path cell)))
      (is (= "codeBlock" (:type node)))
      (is (= "(+ 1 2)" (get-in node [:content 0 :text])))
      (is (:runnable? cell))
      (is (str/starts-with? (subs source (:from cell) (:to cell)) "```cl")))))

(deftest nested-markdown-and-crlf-have-exact-fence-provenance
  (doseq [source ["- example\n\n  ```clojure\n  (+ 1 2)\n  ```\n"
                  "```clojure\r\n(str \"😀\")\r\n```\r\n"]]
    (let [file (saved source) node (first (fences file))
          result (cells/inspect file (request file node))
          projection (projected file) [cell] (:cells projection)
          rendered (node-at (:model projection) (:node-path cell))]
      (is (= (:literal node) (:code result)))
      (is (= (subs source (:from node) (:to node)) (get-in result [:source :original])))
      (is (= (str/replace (:code result) #"(?:\r\n|\n|\r)$" "")
             (get-in rendered [:content 0 :text])))
      (is (= :clj (get-in result [:source :backend])))
      (is (= :saved-markdown-cell (get-in result [:source :kind]))))))

(deftest execution-allowlist-is-narrower-than-syntax-highlighting
  (doseq [language ["cljs" "cljc" "edn" "bb" "javascript" ""]]
    (let [file (saved (str "```" language "\n(+ 1 2)\n```\n")) node (first (fences file))]
      (is (empty? (:cells (projected file))))
      (is (thrown? Exception (cells/inspect file (request file node))))))
  (doseq [language ["clj" "clojure" "Clojure"]]
    (let [file (saved (str "```" language "\n(+ 1 2)\n```\n")) node (first (fences file))]
      (is (= "(+ 1 2)\n" (:code (cells/inspect file (request file node))))))))

(deftest cell-input-is-closed-bounded-and-derived-from-the-saved-file
  (let [file (saved "```clojure\n(+ 1 2)\n```\n") node (first (fences file)) q (request file node)]
    (doseq [extra [:code :text :actor :world :interpreter :source :sandbox]]
      (is (thrown? Exception (cells/inspect file (assoc q extra "(+ 90 10)")))))
    (doseq [changed [(assoc q :path nil) (assoc q :from (inc (:from q)))
                     (assoc q :to (dec (:to q))) (assoc q :revision (random-uuid))
                     (assoc q :repository-id (random-uuid)) (assoc q :block (random-uuid))
                     (assoc q :expected (assoc (:entry file) :content (random-uuid)))
                     (assoc q :namespace "user) (evil") (assoc q :expected-target-stamp nil)]]
      (is (thrown? Exception (cells/inspect file changed)))))
  (doseq [code ["" "   \n" (apply str (repeat (inc code-cells/max-code-characters) "x"))]]
    (let [file (saved (str "```clj\n" code "\n```\n")) node (first (fences file))]
      (is (false? (:runnable? (first (:cells (projected file))))))
      (is (thrown? Exception (cells/inspect file (request file node)))))))

(deftest explicit-block-identity-is-certified-from-the-source
  (let [id #uuid "5d9fb58d-ce00-42e2-8a56-66cec0b1879b"
        file (saved (str "<!-- simmis:block " id " -->\n\n```clojure\n(+ 1 2)\n```\n\n<!-- /simmis:block -->\n"))
        node (first (fences file)) q (request file node)]
    (is (= id (get-in (cells/inspect file q) [:source :block])))
    (is (= id (get-in (cells/inspect file (assoc q :block id)) [:source :block])))
    (is (thrown? Exception (cells/inspect file (assoc q :block (random-uuid)))))))

(deftest changed-native-baselines-and-invalid-structure-offer-no-run-controls
  (let [file (saved "```clj\n(+ 1 2)\n```\n") projection (projected file)]
    (is (= 1 (count (:cells projection))))
    (is (empty? (code-cells/descriptors (:analysis file)
                                       (assoc-in projection [:model :content 0 :content 0 :content 0 :text] "(+ 90 10)"))))
    (is (empty? (code-cells/descriptors (assoc-in (:analysis file) [:semantic :status] :invalid) projection)))))

(deftest cell-projection-equivalence-retains-every-protected-identity
  (let [id "5d9fb58d-ce00-42e2-8a56-66cec0b1879b"
        file (saved (str "<!-- simmis:block " id " -->\n\n```clj\n(+ 1 2)\n```\n\n<!-- /simmis:block -->\n\n> opaque\n"))
        model (:model (projected file))
        shifted (-> model (assoc-in [:content 0 :content 0 :attrs :key] "region:999")
                    (assoc-in [:content 1 :attrs :key] "opaque:2000"))]
    (is (code-cells/equivalent-projection? model shifted))
    (letfn [(native-defaults [node]
              (cond-> node
                (contains? #{"memoryBlock" "memoryRegion" "memoryOpaque"} (:type node))
                (update :attrs #(merge {:key nil :blockId nil :label nil} %))
                (contains? node :content) (update :content #(mapv native-defaults %))))]
      (is (code-cells/equivalent-projection? model (native-defaults shifted)))
      (is (false? (code-cells/equivalent-projection? model (assoc-in (native-defaults shifted) [:content 0 :attrs :label] "forged")))))
    (doseq [bad [(assoc-in shifted [:content 0 :attrs :blockId] (str (random-uuid)))
                 (assoc-in shifted [:content 0 :attrs :key] "block:forged")
                 (assoc-in shifted [:content 0 :content 0 :attrs :blockId] "forged")
                 (assoc-in shifted [:content 1 :attrs :label] "> changed")
                 (assoc-in shifted [:content 1 :attrs :key] "not-derived")
                 (update shifted :content #(vec (reverse %)))
                 (update shifted :content conj (first (:content shifted)))
                 (assoc-in shifted [:content 0 :content 0 :content 0 :content 0 :text] "(+ 99 1)")
                 (assoc-in shifted [:content 0 :content 0 :content 0 :attrs :language] "cljs")]]
      (is (false? (code-cells/equivalent-projection? model bad))))))

(deftest identical-cells-remain-separate-revision-bound-source-addresses
  (let [file (saved "```clj ignored-header\n(+ 1 2)\n```\n\n```clj\n(+ 1 2)\n```\n")
        [a b] (:cells (projected file))]
    (is (not= (:key a) (:key b)))
    (is (not= (:node-path a) (:node-path b)))
    (doseq [node (fences file)]
      (let [request (request file node) result (cells/inspect file request)]
        (is (= "(+ 1 2)\n" (:code result)))
        (is (= (:expected-target-stamp request) (get-in result [:source :target-stamp])))))))
