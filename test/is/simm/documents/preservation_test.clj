(ns is.simm.documents.preservation-test
  (:require [clojure.test :refer [deftest is]]
            [clj-yaml.core :as yaml]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.preservation :as preservation]))

(def page #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")
(def block #uuid "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb")
(def type-id #uuid "cccccccc-cccc-4ccc-8ccc-cccccccccccc")
(def property #uuid "dddddddd-dddd-4ddd-8ddd-dddddddddddd")
(def extra #uuid "eeeeeeee-eeee-4eee-8eee-eeeeeeeeeeee")
(def binding-value {"id" (str property) "type" (str type-id)})
(def raw
  {"simmis-id" (str page) "metric" 0 "unknown" {"nested" [false nil 0]}
   "simmis" {"version" 1 "types" [(str type-id)] "properties" {"metric" binding-value}
             "blocks" {(str block) {"types" [(str type-id)] "properties" {"metric" binding-value}
                                    "values" {"metric" false "unknown" {"nested" [nil 0 false]}}}}}})

(def body (str "# Note\n\n<!-- simmis:block " block " -->\n\nContent\n\n<!-- /simmis:block -->\n"))
(defn- parsed [properties text]
  (markdown/parse (str "---\n" (yaml/generate-string properties) "---\n" text)))
(defn- check [after] (preservation/check {:before (parsed raw body) :after (parsed after body)}))
(defn- kinds [answer] (set (map :kind (:diagnostics answer))))

(deftest value-updates-preserve-false-zero-null-and-metadata-semantics
  (is (= :valid (:status (check raw))))
  ;; The preservation policy does not decide whether these values satisfy the
  ;; actual trusted property definition. Canonical validation owns that gate.
  (doseq [value [false 0 nil {"id" (str extra)} [1 2]]]
    (is (= :valid (:status (check (-> raw (assoc "metric" value)
                                      (assoc-in ["simmis" "blocks" (str block) "values" "metric"] value)))))))
  (is (= :valid (:status (preservation/check {:before (parsed raw body)
                                              :after (parsed raw (str body "\nA new paragraph\n"))})))))

(deftest removals-and-rebinding-are-not-ordinary-maintenance
  (doseq [[after expected]
          [[(dissoc raw "metric") :removed-property-value]
           [(assoc-in raw ["simmis" "types"] []) :removed-type-assignment]
           [(assoc-in raw ["simmis" "properties" "metric" "id"] (str extra)) :changed-property-binding]
           [(update-in raw ["simmis" "properties"] dissoc "metric") :changed-property-binding]
           [(update-in raw ["simmis"] dissoc "blocks") :removed-block-metadata]
           [(update-in raw ["simmis" "blocks" (str block) "values"] dissoc "metric") :removed-property-value]
           [(assoc raw "simmis-id" (str extra)) :changed-document-identity]
           [(dissoc raw "unknown") :changed-unknown-metadata]
           [(assoc-in raw ["unknown" "nested"] [false 0 0]) :changed-unknown-metadata]
           [(assoc-in raw ["simmis" "blocks" (str block) "values" "unknown"] nil) :changed-unknown-metadata]]]
    (let [answer (check after)]
      (is (= :invalid (:status answer)))
      (is (contains? (kinds answer) expected)))))

(deftest additions-are-allowed-but-cannot-reinterpret-existing-unknown-data
  (let [with-new (-> raw (update-in ["simmis" "types"] conj (str extra))
                     (assoc-in ["simmis" "properties" "additional"] {"id" (str extra) "type" (str type-id)})
                     (assoc "additional" "new authored data"))]
    (is (= :valid (:status (check with-new)))))
  (doseq [value [{"nested" [false nil 0]} "laundered update"]]
    (let [answer (check (-> raw
                            (assoc-in ["simmis" "properties" "unknown"] {"id" (str extra) "type" (str type-id)})
                            (assoc "unknown" value)))]
      (is (contains? (kinds answer) :reinterpreted-unbound-property))))
  (let [answer (check (assoc-in raw ["simmis" "blocks" (str block) "properties" "unknown"]
                                {"id" (str extra) "type" (str type-id)}))]
    (is (contains? (kinds answer) :reinterpreted-unbound-property))))

(deftest absence-is-distinct-from-false-zero-and-null-in-both-scopes
  (doseq [value [nil false 0]]
    (let [before (-> raw (assoc "metric" value "unknown" value)
                     (assoc-in ["simmis" "blocks" (str block) "values" "metric"] value))
          original (parsed before body)]
      (is (= :valid (:status (preservation/check {:before original :after original}))))
      (doseq [after [(dissoc before "metric") (dissoc before "unknown")
                     (update-in before ["simmis" "blocks" (str block) "values"] dissoc "metric")]]
        (is (= :invalid (:status (preservation/check {:before original :after (parsed after body)})))))))
  (let [answer (preservation/check {:before (parsed raw body)
                                    :after (parsed (update raw "simmis" dissoc "blocks") "# No block\n")})]
    (is (contains? (kinds answer) :removed-explicit-block-identity))))

(deftest syntax-and-reserved-metadata-must-be-valid-without-claiming-schema-validity
  (doseq [request [nil {} {:before (parsed raw body)}
                   {:before (parsed raw body) :after (parsed (assoc-in raw ["simmis" "version"] 2) body)}
                   {:before (parsed raw body) :after (parsed (dissoc raw "simmis-id") body)}
                   {:before (parsed raw body) :after (parsed raw "<!-- simmis:block broken -->")}
                   {:before (parsed raw body) :after (parsed raw body) :extra true}]]
    (is (= :invalid (:status (preservation/check request)))))
  (let [answer (preservation/check {:before (parsed raw body) :after (parsed raw "Block removed")})]
    (is (= :invalid (:status answer)))))
