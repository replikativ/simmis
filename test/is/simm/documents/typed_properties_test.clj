(ns is.simm.documents.typed-properties-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [is.simm.documents.properties :as properties]
            [is.simm.documents.repository :as documents]
            [is.simm.model.crud :as crud]
            [is.simm.model.store :as store]
            [kontor.governance :as governance]))

(defn- kinds [decoded] (set (map :kind (:diagnostics decoded))))

(deftest declarations-bind-to-real-governed-definitions
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false
             :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        ;; An explicitly new knowledge space uses the REAL store installer,
        ;; not a copied global KB or a second Yggdrasil adapter.
        (documents/initialize! conn)
        (store/install! conn)
        (let [company (crud/create-type! conn {:name "FixtureCompany"})
              research (crud/create-type! conn {:name "FixtureResearch"})
              revenue (crud/create-morphism! conn {:name "S/FixtureCompany/revenue"
                                                   :src "S/FixtureCompany" :dst "S/Number"
                                                   :optional? false})
              active (crud/create-morphism! conn {:name "S/FixtureResearch/active"
                                                  :src "S/FixtureResearch" :dst "S/Boolean"})
              scope {:types [company research]
                     :bindings {"turnover" {:id revenue :declaring-type company}
                                "active" {:id active :declaring-type research}}}
              db @conn
              before (:max-tx db)
              compiled (properties/compile-scope db scope)]
          (testing "multiple types and aliases retain canonical storage, zero and false"
            (let [result (properties/decode-scope compiled {"turnover" 0 "active" false
                                                           "extra" {"id" "not a reference"}})]
              (is (empty? (:diagnostics result)))
              (is (= {:S.FixtureCompany/revenue 0 :S.FixtureResearch/active false} (:values result)))
              (is (= {"extra" {"id" "not a reference"}} (:unknown result)))
              (is (= #{company research} (set (keys (:types compiled)))))
              (is (= #{revenue active} (set (keys (:properties compiled)))))
              (is (= before (:max-tx @conn)))))
          (testing "omitting a required binding cannot evade requiredness"
            (let [unbound (properties/compile-scope db (assoc scope :bindings {}))
                  result (properties/decode-scope unbound {})]
              (is (= [{:kind :missing-required-property :property-id revenue}] (:diagnostics result))))
            (doseq [raw [{} {"turnover" nil}]]
              (is (contains? (kinds (properties/decode-scope compiled raw)) :missing-required-property))))
          (testing "a property must belong to the exact assigned declaring type"
            (doseq [bad [(assoc-in scope [:bindings "turnover" :declaring-type] research)
                         (assoc scope :types [research])
                         (assoc-in scope [:bindings "turnover" :id] (random-uuid))
                         (assoc-in scope [:bindings "duplicate"] {:id revenue :declaring-type company})
                         (assoc scope :types [company company])
                         (assoc scope :types [(random-uuid)])
                         (assoc scope :types [(:entity/uuid (d/pull db [:entity/uuid] [:entity/name "S/Number"]))])]]
              (is (thrown? clojure.lang.ExceptionInfo (properties/compile-scope db bad)))))
          (testing "digest observes unaliased definitions, optionality and validation"
            (d/transact conn [{:entity/uuid revenue :morphism/optional? true}])
            (let [changed (properties/compile-scope @conn scope)]
              (is (not= (:revision compiled) (:revision changed)))
              (is (empty? (:diagnostics (properties/decode-scope changed {})))))
            (d/transact conn [{:entity/uuid revenue :morphism/validation "(throw (ex-info \"never run\" {}))"}])
            (let [changed (properties/compile-scope @conn scope)
                  result (properties/decode-scope changed {"turnover" 2})]
              (is (not= (:revision compiled) (:revision changed)))
              (is (= #{:property-needs-validator} (kinds result)))
              (is (empty? (:values result)))))
          (testing "codomain and installed storage must agree"
            (doseq [tx [[{:entity/uuid revenue :morphism/dst [:entity/name "S/Boolean"]}]
                       [[:db/retract [:entity/uuid revenue] :morphism/dst
                         (:db/id (d/pull db [:db/id] [:entity/name "S/Number"]))]]
                       [{:entity/uuid revenue :morphism/attr-type :NoSuchType}]
                       [{:entity/uuid revenue :morphism/attr-type :Boolean}]
                       [{:entity/uuid revenue :morphism/of-category [:entity/name "Comp"]}]]]
              (is (thrown? clojure.lang.ExceptionInfo
                           (properties/compile-scope (d/db-with db tx) scope)))))
          (testing "exact attr-type override is honored, not discarded for a primitive guess"
            (let [changed (properties/compile-scope
                           (d/db-with db [{:entity/uuid revenue :morphism/attr-type :Long}]) scope)]
              (is (not= (:revision compiled) (:revision changed)))
              (is (= 1 (get-in (properties/decode-scope changed {"turnover" 1})
                              [:values :S.FixtureCompany/revenue])))))
          (testing "new unaliased properties also invalidate the captured digest"
            (crud/create-morphism! conn {:name "S/FixtureCompany/hidden-required"
                                        :src "S/FixtureCompany" :dst "S/String" :optional? false})
            (let [changed (properties/compile-scope @conn scope)]
              (is (not= (:revision compiled) (:revision changed)))
              (is (contains? (kinds (properties/decode-scope changed {})) :missing-required-property))))
          (testing "required owner values are not presented as missing YAML fields"
            (let [owner-db (d/db-with db [{:entity/uuid revenue :morphism/computation "never evaluate"}])
                  bound (properties/compile-scope owner-db scope)
                  unbound (properties/compile-scope owner-db (assoc scope :bindings {}))]
              (is (= #{:required-owner-managed-property} (kinds (properties/decode-scope bound {}))))
              (is (= #{:required-owner-managed-property} (kinds (properties/decode-scope unbound {}))))))
          (testing "derived value constraints are not silently treated as plain strings"
            (let [website (crud/create-morphism! conn {:name "S/FixtureResearch/website"
                                                      :src "S/FixtureResearch" :dst "S/URL"})
                  scope {:types [research] :bindings {"site" {:id website :declaring-type research}}}
                  compiled (properties/compile-scope @conn scope)
                  result (properties/decode-scope compiled {"site" "not a URL"})]
              (is (= #{:property-needs-validator} (kinds result)))
              (is (empty? (:values result))))))
        (finally
          ;; Registration belongs only to this uniquely identified fixture.
          (governance/ungovern! conn)
          (d/release conn)
          (d/delete-database cfg))))))
