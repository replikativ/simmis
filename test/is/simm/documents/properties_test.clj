(ns is.simm.documents.properties-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.documents.properties :as properties]))

(defn- compiled [type cardinality]
  {:bindings {"value" {:attr :S.Page/value :schema {:db/valueType type}
                        :cardinality cardinality :owned? true}}})

(deftest strict-scalar-types-and-unknown-metadata
  (doseq [[type raw expected]
          [[:db.type/string "text" "text"] [:db.type/boolean false false]
           [:db.type/long 42 42] [:db.type/double 1.5 1.5]
           [:db.type/uuid "00000000-0000-4000-8000-000000000001"
            #uuid "00000000-0000-4000-8000-000000000001"]
           [:db.type/instant "2026-10-19T00:00:00Z" #inst "2026-10-19T00:00:00Z"]]]
    (let [result (properties/decode (compiled type :one) {"value" raw "custom" {"keep" [1 2]}})]
      (is (= expected (get-in result [:values :S.Page/value])))
      (is (empty? (:diagnostics result)))
      (is (= {"custom" {"keep" [1 2]}} (:unknown result)))))
  (doseq [[type raw] [[:db.type/string 42] [:db.type/boolean "false"] [:db.type/long "42"]
                     [:db.type/long 1.5] [:db.type/double Double/NaN]
                     [:db.type/uuid "1-1-1-1-1"] [:db.type/instant "2026-10-19"]]]
    (let [result (properties/decode (compiled type :one) {"value" raw})]
      (is (empty? (:values result)))
      (is (= :invalid-property-value (-> result :diagnostics first :kind))))))

(deftest cardinality-is-explicit-not-guessed
  (is (= {:S.Page/value [1 2]} (:values (properties/decode (compiled :db.type/long :many)
                                                        {"value" '(1 2)}))))
  (doseq [raw ["one" 1 {"nested" 1} [1 "2"]]]
    (is (seq (:diagnostics (properties/decode (compiled :db.type/long :many) {"value" raw})))))
  (is (= {:S.Page/value []} (:values (properties/decode (compiled :db.type/long :many) {"value" []}))))
  (is (empty? (:values (properties/decode (compiled :db.type/long :one) {"value" nil})))))

(deftest governed-and-unsupported-properties-never-become-facts
  (doseq [[binding kind]
          [[{:owned? false} :owner-managed-property]
           [{:schema {:db/valueType :db.type/string :db/isComponent true}} :component-property]
           [{:schema {:db/valueType :db.type/string :db/unique :db.unique/identity}}
            :unique-property-needs-validation]
           [{:schema {:db/valueType :db.type/ref}} :unresolved-property-reference]
           [{:morphism {:morphism/validation "do not evaluate me"}} :property-needs-validator]]]
    (let [schema (update-in (compiled :db.type/string :one) [:bindings "value"] merge binding)
          result (properties/decode schema {"value" "raw preserved"})]
      (is (= kind (-> result :diagnostics first :kind)))
      (is (empty? (:values result)))
      (is (= {"value" "raw preserved"} (:raw result))))))

(deftest required-and-many-value-limits-are-explicit
  (let [schema (assoc-in (compiled :db.type/long :one)
                        [:bindings "value" :morphism :morphism/optional?] false)]
    (doseq [raw [{} {"value" nil} {"value" []}]]
      (is (some #(= :missing-required-property (:kind %))
                (:diagnostics (properties/decode schema raw)))))
    (is (empty? (:diagnostics (properties/decode schema {"value" 0})))))
  (is (= :too-many-property-values
         (-> (properties/decode (compiled :db.type/long :many) {"value" (range 1001)})
             :diagnostics first :kind))))

(deftest property-schema-digest-comes-from-real-captured-facts
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
             :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg) id (random-uuid)]
      (try
        (d/transact conn [{:db/ident :entity/uuid :db/valueType :db.type/uuid
                           :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
                          {:db/ident :entity/name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
                          {:db/ident :morphism/cardinality :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
                          {:db/ident :morphism/storage-attr :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
                          {:db/ident :S.Page/value :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
                          {:entity/uuid id :entity/name "S/Page/value" :morphism/cardinality :one}])
        (let [before (:max-tx @conn)
              compiled (properties/compile-schema @conn {"value" id})]
          (is (= before (:max-tx @conn)))
          (is (= :S.Page/value (get-in compiled [:bindings "value" :attr])))
          (is (= {:S.Page/value 42} (:values (properties/decode compiled {"value" 42}))))
          (is (thrown? clojure.lang.ExceptionInfo
                       (properties/compile-schema @conn {"a" id "b" id})))
          (is (thrown? clojure.lang.ExceptionInfo
                       (properties/compile-schema @conn {"value" (random-uuid)})))
          (d/transact conn [{:entity/uuid id :morphism/storage-attr :S.Page/value}])
          (let [changed (properties/compile-schema @conn {"value" id})]
            (is (not= (:revision compiled) (:revision changed)))
            (is (= :owner-managed-property
                   (-> (properties/decode changed {"value" 42}) :diagnostics first :kind))))
          (d/transact conn [[:db/retract [:entity/uuid id] :morphism/storage-attr :S.Page/value]
                            {:db/ident :document.node/heading-level :db/valueType :db.type/long
                             :db/cardinality :db.cardinality/one}
                            {:entity/uuid id :entity/name "document.node/heading-level"}])
          (is (thrown? clojure.lang.ExceptionInfo
                       (properties/compile-schema @conn {"value" id})))
          (d/transact conn [{:db/ident :Research.Case/value :db/valueType :db.type/long
                             :db/cardinality :db.cardinality/one}
                            {:entity/uuid id :entity/name "Research/Case/value"}])
          (is (= :Research.Case/value
                 (get-in (properties/compile-schema @conn {"value" id}) [:bindings "value" :attr]))))
        (finally (d/release conn))))))
