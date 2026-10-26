(ns is.simm.documents.property-presentation-test
  (:require [is.simm.documents.property-presentation :as presentation]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])))

(def founder #uuid "58ab8578-0b6a-4685-b0c2-78101110b077")
(def person-type #uuid "6b5b21f2-93bd-4b9a-8bb3-230bef5653d3")
(def reference-field {:present? true :cardinality :one :value-type :db.type/ref
                      :input {:text (str founder)} :relation {:target founder :codomain person-type}})
(def reference-options {:reference-options {person-type {:status :ready :complete? true
                                                        :items [{:id (str founder) :label "Sam" :path "people/sam.md"}]}}})

(deftest property-values-keep-semantic-states-and-authoritative-text
  (let [base {:present? true :cardinality :one}]
    (doseq [[field expected]
            [[{:present? false :input {:null true}} {:kind :missing}]
             [{:input {:null true}} {:kind :null}]
             [{:value-type :db.type/boolean :input {:text "false"}} {:kind :boolean :value false}]
             [{:value-type :db.type/boolean :input {:text "true"}} {:kind :boolean :value true}]
             [{:value-type :db.type/long :value 9007199254740992 :input {:text "9007199254740993"}}
              {:kind :number :text "9007199254740993"}]
             [{:value-type :db.type/long :input {:text "0"}} {:kind :number :text "0"}]
             [{:value-type :db.type/string :input {:text ""}} {:kind :text :text "" :complete? true}]
             [{:value-type :db.type/string :render-hint :number :input {:text "0012"}}
              {:kind :text :text "0012" :complete? true}]
             [{:value-type :db.type/boolean :input {:text "unknown"}} {:kind :unavailable-value}]]]
      (is (= expected (presentation/cell {} (merge base field)))))))

(deftest structured-values-remain-source-previews-regardless-of-hint
  (is (= {:kind :source-value :text "[false 0 nil]" :complete? true}
         (presentation/cell {} {:present? true :cardinality :many :value-type :db.type/long
                                :render-hint :number :input {:text "[false 0 nil]"}})))
  (is (= {:kind :source-value :text "opaque" :complete? true}
         (presentation/cell {} {:present? true :cardinality :one :value-type :db.type/bytes
                                :render-hint :text :input {:text "opaque"}}))))

(deftest references-use-only-the-supplied-ready-source-options
  (is (= {:kind :reference :id founder :label "Sam" :path "people/sam.md"}
         (presentation/cell reference-options reference-field)))
  (is (= {:kind :reference :id founder :label "Sam"}
         (presentation/cell (update-in reference-options [:reference-options person-type :items]
                                       #(mapv (fn [item] (dissoc item :path)) %)) reference-field)))
  (doseq [[options expected] [[nil :lookup-unavailable]
                              [{:status :unavailable :items (get-in reference-options [:reference-options person-type :items])}
                               :lookup-unavailable]
                              [{:status :ready :complete? false :items []} :lookup-incomplete]
                              [{:status :ready :complete? true :items []} :not-resolved]]]
    (is (= {:kind :unresolved-reference :id founder :reason expected}
           (presentation/cell {:reference-options {person-type options}} reference-field))))
  (is (= :reference (:kind (presentation/cell (assoc-in reference-options [:reference-options person-type :complete?] false)
                                             reference-field))))
  (is (= {:kind :unavailable-value}
         (presentation/cell {} (-> reference-field (dissoc :relation) (assoc :input {:text "not-an-id"}))))))

(deftest display-preview-does-not-split-unicode-or-change-source
  (let [source (str (apply str (repeat 1023 "x")) "😀tail")
        field {:present? true :cardinality :one :value-type :db.type/string :input {:text source}}
        shown (presentation/cell {} field)]
    (is (= 1023 (count (:text shown))))
    (is (false? (:complete? shown)))
    (is (= source (get-in field [:input :text])))))
