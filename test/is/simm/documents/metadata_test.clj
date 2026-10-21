(ns is.simm.documents.metadata-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.documents.blocks :as blocks]
            [is.simm.documents.identity :as identity]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.metadata :as metadata]
            [is.simm.documents.properties :as properties]))

(def page-id #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")
(def company-id #uuid "11111111-1111-4111-8111-111111111111")
(def research-id #uuid "22222222-2222-4222-8222-222222222222")
(def revenue-id #uuid "44444444-4444-4444-8444-444444444444")
(def founder-id #uuid "55555555-5555-4555-8555-555555555555")
(def block-id #uuid "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb")
(def person-id #uuid "cccccccc-cccc-4ccc-8ccc-cccccccccccc")
(def bindings {"revenue" {"id" (str revenue-id) "type" (str company-id)}
               "founders" {"id" (str founder-id) "type" (str company-id)}})
(def company
  {"simmis-id" (str page-id)
   "simmis" {"version" 1 "types" [(str company-id) (str research-id)] "properties" bindings
             "blocks" {(str block-id) {"types" [(str company-id)] "properties" bindings
                                       "values" {"revenue" 0 "founders" []}}}}
   "revenue" 120000 "founders" [{"id" (str person-id) "path" "people/alice.md"}]
   "active" false "tags" ["research"] "unknown" {"keep" "verbatim"}})

(defn decode [raw] (metadata/decode raw #{block-id}))
(defn kinds [result] (set (map :kind (:diagnostics result))))

(deftest absent-frontmatter-is-not-the-same-as-a-non-map-value
  (is (= :plain (:status (decode nil))))
  (doseq [raw [false 0 "" []]]
    (is (= #{:invalid-metadata-input} (kinds (decode raw)))))
  (is (= #{:invalid-metadata-input} (kinds (metadata/decode company nil)))))

(deftest declarations-keep-stable-ids-and-independent-entity-scopes
  (let [result (decode company)]
    (is (= :ready (:status result)))
    (is (= page-id (:document-id result)))
    (is (= company (:raw result)))
    (is (= [company-id research-id] (get-in result [:page :types])))
    (is (= {:id revenue-id :declaring-type company-id} (get-in result [:page :bindings "revenue"])))
    (is (= 0 (get-in result [:blocks block-id :values "revenue"])))
    (is (= [] (get-in result [:blocks block-id :values "founders"])))
    (is (false? (get-in result [:page :values "active"])))
    (is (= ["research"] (get-in result [:page :values "tags"])))
    (is (not= metadata/profile properties/profile))))

(deftest values-remain-inert-until-trusted-schema-says-what-they-mean
  (let [raw (assoc company "ordinary-uuid" (str person-id)
                   "program" "(throw (ex-info \"must never execute\" {}))")
        result (decode raw)]
    (is (= :ready (:status result)))
    (is (= (get raw "ordinary-uuid") (get-in result [:page :values "ordinary-uuid"])))
    (is (= (get raw "founders") (get-in result [:page :values "founders"])))
    (is (= (get raw "program") (get-in result [:page :values "program"])))
    ;; No filename/title lookup, reference interpretation or execution result.
    (is (= "people/alice.md" (get-in result [:page :values "founders" 0 "path"])))))

(deftest unknown-ordinary-data-is-preserved-but-future-reserved-semantics-diagnose
  (doseq [raw [(assoc-in company ["simmis" "version"] 2)
               (assoc-in company ["simmis" "definitions"] {"Company" "inert declaration"})
               (assoc-in company ["simmis" "properties" "revenue" "computation"] "never run")]]
    (let [result (decode raw)]
      (is (= raw (:raw result)))
      (is (= :invalid (:status result)))
      (is (seq (:diagnostics result)))))
  (is (= :ready (:status (decode company)))))

(deftest stable-typed-documents-need-explicit-valid-identity
  (let [result (decode (dissoc company "simmis-id"))]
    (is (= :invalid (:status result)))
    (is (contains? (kinds result) :typed-document-needs-explicit-identity))
    (is (not (contains? (:raw result) "simmis-id"))))
  (doseq [value [false 0 "bad" "AAAAAAAA-AAAA-4AAA-8AAA-AAAAAAAAAAAA"]]
    (is (= #{:invalid-document-id} (kinds (decode {"simmis-id" value})))))
  (let [result (decode {"title" "Plain import" "tags" ["Company"]})]
    (is (= :plain (:status result)))
    (is (nil? (:document-id result)))
    (is (empty? (get-in result [:page :types])))))

(deftest ambiguous-or-unmatched-declarations-never-publish-a-partial-ready-view
  (doseq [[raw expected]
          [[(assoc-in company ["simmis" "types"] [(str company-id) (str company-id)]) :duplicate-type-assignment]
           [(assoc-in company ["simmis" "properties" "turnover"] (get bindings "revenue")) :ambiguous-property-alias]
           [(assoc-in company ["simmis" "properties" "simmis"] (get bindings "revenue")) :invalid-metadata-scope]
           [(assoc-in company ["simmis" "blocks" (str person-id)] {"types" [(str research-id)]}) :invalid-block-metadata]
           [(assoc-in company ["simmis" "blocks" (str block-id) "values"] "not a map") :invalid-block-metadata]]]
    (let [result (decode raw)]
      (is (= :invalid (:status result)))
      (is (contains? (kinds result) expected)))))

(deftest declaration-counts-and-canonical-id-shapes-are-bounded
  (doseq [raw [(assoc-in company ["simmis" "types"] (mapv (fn [_] (str (random-uuid))) (range 65)))
               (assoc-in company ["simmis" "types"] (str company-id))
               (assoc-in company ["simmis" "properties"]
                         (into {} (for [n (range 65)] [(str "field-" n) {"id" (str (random-uuid)) "type" (str company-id)}])))
               (assoc-in company ["simmis" "properties" "revenue" "id"] "1-1-1-1-1")]]
    (let [result (decode raw)]
      (is (= :invalid (:status result)))
      (is (contains? (kinds result) :invalid-metadata-scope)))))

(deftest actual-markdown-and-explicit-block-identity-use-the-same-codec
  (let [source (str "---\r\nsimmis-id: '" page-id "'\r\nsimmis:\r\n  version: 1\r\n  types: ['" company-id "']\r\n"
                    "  blocks:\r\n    '" block-id "':\r\n      types: ['" research-id "']\r\n"
                    "title: 'Company 😀' # keep comment\r\ntags: [research]\r\n---\r\n\r\n"
                    "<!-- simmis:block " block-id " -->\r\nResearch notes.\r\n<!-- /simmis:block -->\r\n")
        parsed (markdown/parse source)
        document (identity/document (random-uuid) "wiki/company.md" parsed)
        semantic (blocks/project (:id document) parsed)
        ids (into #{} (comp (filter #(= :explicit (:identity-status %))) (map :id)) (:blocks semantic))
        result (metadata/decode (get-in parsed [:frontmatter :properties]) ids)]
    (is (= :ready (:status semantic)))
    (is (= :ready (:status result)))
    (is (= page-id (:document-id result) (:id document)))
    (is (= [research-id] (get-in result [:blocks block-id :types])))
    (is (= source (:source parsed)))))
