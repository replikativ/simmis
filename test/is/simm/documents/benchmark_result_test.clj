(ns is.simm.documents.benchmark-result-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [is.simm.documents.benchmark-result :as result]
            [is.simm.documents.scorecard-views-test :as fixture]))

(deftest chat-projection-retains-native-values-and-unknown-costs
  (let [summary (assoc-in fixture/summary [:leaderboard 0 :spend :priced?] false)
        text (result/content fixture/context summary fixture/definition)]
    (is (str/includes? text "| Candidate | Attempts | Passed |"))
    (is (str/includes? text "Unknown"))
    (is (str/includes? text "List-price cost"))
    (is (not (str/includes? text "__host_private_credentials__")))))

(deftest candidate-labels-remain-literal-markdown-text
  (let [label "![x](https://tracking.invalid/a)|<img> `code` **bold**"
        summary (assoc-in fixture/summary [:leaderboard 0 :candidate] label)
        definition (assoc-in fixture/definition [:experiment/candidates 0 :candidate/id] (keyword label))
        text (result/content fixture/context summary definition)]
    (is (str/includes? text "\\!\\[x\\]\\(https://tracking\\.invalid/a\\)\\|\\<img\\>"))
    (is (str/includes? text "\\`code\\` \\*\\*bold\\*\\*"))
    (is (not (str/includes? text "![x](")))))
