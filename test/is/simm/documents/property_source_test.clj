(ns is.simm.documents.property-source-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.property-source :as property-source]))

(defn- edit [source path value]
  (property-source/replace-scalar source (markdown/revision source) path value))

(deftest codepoint-marks-map-to-original-utf16-crlf-and-bom
  (let [source "﻿---\r\nemoji: \"😀\"\r\nrevenue: 0 # keep this\r\nunknown: {retain: [x, y]}\r\n---\r\n# Body 😀\r\n"
        span (property-source/scalar-span (markdown/parse source) ["revenue"])
        changed (edit source ["revenue"] 42)]
    (is (= (.indexOf source "0 # keep") (:from span)))
    (is (= "0" (subs source (:from span) (:to span))))
    (is (= "﻿---\r\nemoji: \"😀\"\r\nrevenue: 42 # keep this\r\nunknown: {retain: [x, y]}\r\n---\r\n# Body 😀\r\n"
           (:source changed)))
    (is (= {"retain" '("x" "y")} (get-in changed [:frontmatter :properties "unknown"])))
    (is (empty? (:diagnostics changed)))))

(deftest supported-no-ops-retain-every-byte-and-revision
  (doseq [[token value] [["false" false] ["0" 0] ["''" ""] ["null" nil]
                         ["'quoted text'" "quoted text"]]]
    (let [source (str "---\nvalue: " token " # exact comment\n---\nbody\n")
          changed (edit source ["value"] value)]
      (is (= source (:source changed)))
      (is (= (markdown/revision source) (:revision changed))))))

(deftest scalar-quotes-and-comments-are-not-whole-map-serialization
  (let [source "---\nvalue: 'old' # keep\nuntouched: \"same\" # stay\n---\nbody\n"]
    (is (= "---\nvalue: 'Sam''s shop' # keep\nuntouched: \"same\" # stay\n---\nbody\n"
           (:source (edit source ["value"] "Sam's shop")))))
  (let [source "---\nvalue: \"old\" # keep\n---\nbody\n"
        changed (edit source ["value"] "a\n\"b\"")]
    (is (= "a\n\"b\"" (get-in changed [:frontmatter :properties "value"])))
    (is (= "---\nvalue: \"a\\n\\\"b\\\"\" # keep\n---\nbody\n" (:source changed))))
  (let [source "---\nvalue: old\n---\nbody\n"
        changed (edit source ["value"] "false")]
    (is (= "false" (get-in changed [:frontmatter :properties "value"])))
    (is (= "---\nvalue: \"false\"\n---\nbody\n" (:source changed)))))

(deftest nested-existing-leaves-preserve-surrounding-metadata
  (let [source "---\nsimmis-id: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa'\nsimmis:\n  version: 1\n  blocks:\n    block-id:\n      values:\n        active: true # retain\n---\n<!-- not executed -->\n"
        changed (edit source ["simmis" "blocks" "block-id" "values" "active"] false)]
    (is (= false (get-in changed [:frontmatter :properties "simmis" "blocks" "block-id" "values" "active"])))
    (is (= "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa" (get-in changed [:frontmatter :properties "simmis-id"])))
    (is (= (.replace source "active: true" "active: false") (:source changed)))))

(deftest unsupported-yaml-never-gets-a-lossy-rewrite
  (doseq [source ["---\nvalue: &v one\n---\nbody\n"
                  "---\nother: &v one\nvalue: *v\n---\nbody\n"
                  "---\nvalue: |\n  multiline\n---\nbody\n"
                  "---\nvalue: 'two\n  lines'\n---\nbody\n"
                  "---\nvalue: !!str old\n---\nbody\n"
                  "---\nvalue: [a, b]\n---\nbody\n"
                  "---\nvalue: 1\nvalue: 2\n---\nbody\n"
                  "---\nvalue: # absent token\n---\nbody\n"]]
    (is (thrown? clojure.lang.ExceptionInfo (edit source ["value"] "new"))))
  (is (thrown? clojure.lang.ExceptionInfo (edit "---\nother: 1\n---\n" ["value"] 2)))
  (is (thrown? clojure.lang.ExceptionInfo (edit "---\nvalue: 1\n---\n" ["value"] {"not" "scalar"}))))

(deftest stale-revisions-refuse-before-patching
  (let [source "---\nvalue: 0\n---\nbody\n"]
    (is (= :is.simm.documents.property-source/conflict
           (try (property-source/replace-scalar source (random-uuid) ["value"] 1)
                (catch clojure.lang.ExceptionInfo error (:type (ex-data error))))))))

(deftest flow-mappings-unicode-and-numeric-boundaries
  (let [source "---\nvalues: {active: true, keep: '😀'} # stay\nafter: 0\n---\nbody\n"
        changed (edit source ["values" "keep"] "future 🦋")]
    (is (= (.replace source "'😀'" "'future 🦋'") (:source changed)))
    (is (= (.replace (:source changed) "after: 0" "after: 2")
           (:source (edit (:source changed) ["after"] 2)))))
  (doseq [value [Double/NaN Double/POSITIVE_INFINITY Double/NEGATIVE_INFINITY
                 9007199254740993.123456789M]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (edit "---\nvalue: 0\n---\n" ["value"] value))))
  (is (thrown? clojure.lang.ExceptionInfo
               (edit "---\nvalue: !!str 'old'\n---\n" ["value"] "new"))))
