(ns is.simm.documents.property-candidate-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.documents.properties :as properties]))

(def property-id #uuid "11111111-1111-4111-8111-111111111111")
(def person-type #uuid "22222222-2222-4222-8222-222222222222")
(def target-id #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")

(defn- compiled
  ([type cardinality] (compiled type cardinality nil))
  ([type cardinality unique]
   (let [binding {:attr :S.Company/value :schema (cond-> {:db/valueType type}
                                                 unique (assoc :db/unique unique))
                  :cardinality cardinality :owned? true
                  :morphism {:entity/uuid property-id :morphism/dst {:entity/uuid person-type}}}]
     {:profile properties/typed-profile :bindings {"value" binding} :properties {property-id binding}})))

(defn- kinds [result] (set (map :kind (:diagnostics result))))

(deftest references-remain-logical-pending-claims-not-final-facts
  (doseq [raw [(str target-id) {"id" (str target-id) "path" "../../a-misleading-file.md"}]]
    (let [result (properties/decode-candidate (compiled :db.type/ref :one) {"value" raw})]
      (is (= :pending (:status result)))
      (is (= properties/candidate-profile (:profile result)))
      (is (not (contains? result :values)))
      (is (empty? (:candidate-values result)))
      (is (empty? (:diagnostics result)))
      (is (= [{:kind :property-reference :property "value" :attribute :S.Company/value
               :property-id property-id :cardinality :one :codomain person-type :targets [target-id]}]
             (:claims result)))
      (is (= {"value" raw} (:raw result))))))

(deftest uuid-scalars-and-unknown-data-do-not-become-relations
  (let [raw {"value" (str target-id) "unknown" {"id" (str target-id)}}
        result (properties/decode-candidate (compiled :db.type/uuid :one) raw)]
    (is (= {:S.Company/value target-id} (:candidate-values result)))
    (is (empty? (:claims result)))
    (is (= {"unknown" {"id" (str target-id)}} (:unknown result)))
    (is (= :pending (:status result)))))

(deftest invalid-reference-shapes-cannot-acquire-identity
  (doseq [raw ["1-1-1-1-1" "AAAAAAAA-AAAA-4AAA-8AAA-AAAAAAAAAAAA" false 0
               {"id" (str target-id) "tool" "never execute"}
               {"id" (str target-id) "path" nil}
               {"id" (str target-id) "path" (apply str (repeat 513 "a"))}
               {"id" (str target-id) "path" "a\nb"}
               {"path" "people/alice.md"} {:id (str target-id)}]]
    (let [result (properties/decode-candidate (compiled :db.type/ref :one) {"value" raw})]
      (is (= #{:invalid-property-reference} (kinds result)))
      (is (empty? (:claims result)))
      (is (empty? (:candidate-values result)))
      (is (= {"value" raw} (:raw result))))))

(deftest cardinality-and-reference-work-are-bounded
  (let [result (properties/decode-candidate (compiled :db.type/ref :many)
                                          {"value" (list (str target-id) (str person-type))})]
    (is (= [target-id person-type] (-> result :claims first :targets))))
  (let [result (properties/decode-candidate (compiled :db.type/ref :many)
                                          {"value" (repeat 1001 (str target-id))})]
    (is (= #{:too-many-property-values} (kinds result)))
    (is (empty? (:claims result))))
  (is (= #{:invalid-property-reference}
         (kinds (properties/decode-candidate (compiled :db.type/ref :many) {"value" (str target-id)})))))

(deftest both-uniqueness-modes-are-explicit-pending-claims
  (doseq [unique [:db.unique/value :db.unique/identity]]
    (let [schema (compiled :db.type/long :one unique)
          result (properties/decode-candidate schema {"value" 0})
          strict (properties/decode-scope schema {"value" 0})]
      (is (= :pending (:status result)))
      (is (= {:S.Company/value 0} (:candidate-values result)))
      (is (= [{:kind :unique-property :property "value" :attribute :S.Company/value
               :property-id property-id :reference? false :uniqueness unique :items [0]}]
             (:claims result)))
      (is (= #{:unique-property-needs-validation} (kinds strict)))
      (is (empty? (:values strict))))))

(deftest reference-uniqueness-keeps-logical-identities-for-both-claims
  (let [result (properties/decode-candidate (compiled :db.type/ref :one :db.unique/identity)
                                          {"value" {"id" (str target-id)}})]
    (is (= [:property-reference :unique-property] (mapv :kind (:claims result))))
    (is (= [target-id] (-> result :claims first :targets)))
    (is (= [target-id] (-> result :claims second :items)))
    (is (true? (-> result :claims second :reference?)))
    (is (empty? (:candidate-values result)))))

(deftest ordinary-validation-requiredness-and-false-values-are-shared
  (let [schema (assoc-in (compiled :db.type/boolean :one)
                        [:properties property-id :morphism :morphism/optional?] false)
        result (properties/decode-candidate schema {"value" false})]
    (is (= {:S.Company/value false} (:candidate-values result)))
    (is (empty? (:diagnostics result)))
    (is (= #{:missing-required-property}
           (kinds (properties/decode-candidate (assoc schema :bindings {}) {})))))
  (doseq [[alter expected] [[#(assoc % :owned? false) :owner-managed-property]
                            [#(assoc-in % [:schema :db/isComponent] true) :component-property]
                            [#(assoc-in % [:morphism :morphism/validation] "never run") :property-needs-validator]]]
    (let [schema (update-in (compiled :db.type/ref :one) [:bindings "value"] alter)
          result (properties/decode-candidate schema {"value" (str target-id)})]
      (is (= #{expected} (kinds result)))
      (is (empty? (:claims result)))))
  (let [strict (properties/decode-scope (compiled :db.type/ref :one) {"value" (str target-id)})]
    (is (= #{:unresolved-property-reference} (kinds strict)))
    (is (empty? (:values strict)))))
