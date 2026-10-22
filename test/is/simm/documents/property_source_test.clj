(ns is.simm.documents.property-source-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
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

(defn- insert [source path value]
  (property-source/insert-value source (markdown/revision source) path value))

(deftest missing-mapping-insertion-retains-unknowns-comments-and-newlines
  (let [source "﻿---\r\nsimmis-id: aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa # identity\r\n# untouched 😀\r\nsimmis:\r\n  version: 1 # version comment\r\nunknown: {keep: [x, y]}\r\n---\r\n# Body\r\n"
        changed (insert source ["simmis" "properties" "active"]
                        {"id" "property-id" "type" "type-id"})]
    (is (= (.replace source "  version: 1"
                     "  \"properties\": {\"active\": {\"id\": \"property-id\", \"type\": \"type-id\"}}\r\n  version: 1")
           (:source changed)))
    (is (= {"id" "property-id" "type" "type-id"}
           (get-in changed [:frontmatter :properties "simmis" "properties" "active"])))
    (is (empty? (:diagnostics changed)))))

(deftest insertion-preserves-flow-mappings-and-explicit-false-zero-null
  (doseq [original ["{}" "{keep: '😀'}"]
          value [false 0 nil "" 9007199254740993]]
    (let [source (str "---\nvalues: " original " # stay\n---\nbody\n")
          changed (insert source ["values" "new"] value)]
      (is (= value (get-in changed [:frontmatter :properties "values" "new"])))
      (is (str/ends-with? (:source changed) " # stay\n---\nbody\n"))
      (when-not (= original "{}")
        (is (= "😀" (get-in changed [:frontmatter :properties "values" "keep"]))))))
  (doseq [value [nil false 0 ""]]
    (let [source (str "---\nexisting: " (pr-str value) "\n---\nbody\n")]
      ;; EDN nil is a YAML string here; presence, not truthiness, still refuses.
      (is (thrown? clojure.lang.ExceptionInfo (insert source ["existing"] 1)))))
  (is (= "---\n\"new\": false\n# retain\n---\nbody\n"
         (:source (insert "---\n# retain\n---\nbody\n" ["new"] false)))))

(deftest type-sequence-changes-are-local-not-frontmatter-rewrites
  (doseq [newline ["\n" "\r\n" "\r"]
          sequence ["[one, two]" "\n  - one\n  - two"]]
    (let [source (str/replace (str "---\nsimmis:\n  version: 1 # keep\n  types: " sequence
                                   "\nunknown: 0 # stay\n---\nbody 😀\n") "\n" newline)
          changed (property-source/set-types source (markdown/revision source) ["simmis" "types"] ["three"])]
      (is (= ["three"] (get-in changed [:frontmatter :properties "simmis" "types"])))
      (is (str/includes? (:source changed) (str "unknown: 0 # stay" newline "---" newline "body 😀")))
      (is (str/includes? (:source changed) "version: 1 # keep"))))
  (let [source "---\nsimmis: {version: 1, types: [one, two]} # keep\n---\nbody\n"]
    (is (= source (:source (property-source/set-types source (markdown/revision source)
                                                      ["simmis" "types"] ["one" "two"]))))
    (is (= (.replace source "[one, two]" "[]")
           (:source (property-source/set-types source (markdown/revision source) ["simmis" "types"] [])))))
  (let [source "---\nsimmis-id: stable\n---\nbody\n"]
    (is (= ["type-id"] (get-in (property-source/set-types source (markdown/revision source)
                                                          ["simmis" "types"] ["type-id"])
                               [:frontmatter :properties "simmis" "types"])))))

(deftest declaration-source-edits-refuse-unsafe-or-lossy-forms
  (doseq [source ["---\nvalues: &v {}\n---\n"
                  "---\nvalues: null\n---\n"
                  "---\nvalues: []\n---\n"
                  "---\nvalues: {}\nvalues: {}\n---\n"]]
    (is (thrown? clojure.lang.ExceptionInfo (insert source ["values" "new"] 0))))
  (doseq [source ["---\ntypes:\n- one # must retain\n- two\n---\n"
                  "---\ntypes: [one, # annotation\n two]\n---\n"
                  "---\ntypes: !!seq [one]\n---\n"
                  "---\ntypes: [!!str one]\n---\n"
                  "---\ntypes: &x [one]\n---\n"]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (property-source/set-types source (markdown/revision source) ["types"] ["three"]))))
  (doseq [value [Double/NaN Double/POSITIVE_INFINITY 9007199254740993.123456789M
                 {"x" (vec (repeat 65 1))} {:keyword "not a YAML string key"}]]
    (is (thrown? clojure.lang.ExceptionInfo (insert "---\nexisting: 1\n---\n" ["new"] value))))
  (is (thrown? clojure.lang.ExceptionInfo
               (property-source/insert-value "---\nexisting: 1\n---\n" (random-uuid) ["new"] 0)))
  (is (= :oversized-new-value
         (try (insert "---\nexisting: 1\n---\n" ["new"]
                      (vec (repeat 64 (apply str (repeat 32768 "😀")))))
              (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))
