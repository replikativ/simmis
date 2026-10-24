(ns is.simm.documents.inline-charts-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.documents.inline-charts :as charts]
            [is.simm.documents.chart-spec :as spec]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.rich-text :as rich]))

(def data "{\"version\":1,\"kind\":\"line\",\"series\":[{\"label\":\"Revenue\",\"points\":[[1,100],[2,140]]}]}")
(def fence (str "```simmis-chart\n" data "\n```\n"))

(deftest nested-crlf-fences-match-the-projected-code-without-mutating-it
  (let [source (str "- Nested\r\n\r\n  ```simmis-chart\r\n  " data "\r\n  ```\r\n")
        parsed (markdown/parse source) nodes (:nodes parsed)
        preview (first (charts/analyze nodes))
        models (mapv rich/model nodes)
        originals models found (charts/positions models [preview] 4)]
    (is (= :ready (:status preview)))
    (is (= data (:text preview)))
    (is (= :line (get-in preview [:data :kind])))
    (is (= 1 (count found)))
    (is (= originals models))
    (is (= source (:source parsed)))))

(deftest only-data-fences-are-decoded-and-invalid-source-is-retained
  (let [bad "{\"version\":1,\"url\":\"https://example.com\"}"
        source (str "```clojure\n(+ 1 2)\n```\n\n" "```simmis-chart\n" bad "\n```\n\n<!--\n" fence "-->\n")
        parsed (markdown/parse source) previews (charts/analyze (:nodes parsed))]
    (is (= 1 (count previews)))
    (is (= :invalid (:status (first previews))))
    (is (= bad (:text (first previews))))
    (is (= source (:source parsed)))))

(deftest actual-occurrences-share-the-display-budget-even-for-identical-fences
  (let [parsed (markdown/parse fence) model (rich/model (first (:nodes parsed)))
        previews (charts/analyze (:nodes parsed)) copies (vec (repeat 7 model))]
    (is (= 4 (count (charts/positions copies previews 99))))
    (is (= 2 (count (charts/positions copies previews 2))))
    (is (empty? (charts/positions copies previews 0)))
    (is (= 4 (count (charts/analyze (:nodes (markdown/parse (apply str (repeat 7 (str fence "\n")))))))))
    (is (empty? (charts/positions [(assoc-in model [:content 0 :content 0 :text] "changed")] previews 4)))))

(deftest unexpected-decoder-failures-are-not-presented-as-bad-user-data
  (let [error (ex-info "Unexpected implementation failure" {:type ::unexpected})]
    (with-redefs [spec/decode (fn [_] (throw error))]
      (is (identical? error
                      (try (charts/analyze (:nodes (markdown/parse fence)))
                           nil (catch clojure.lang.ExceptionInfo actual actual)))))))
