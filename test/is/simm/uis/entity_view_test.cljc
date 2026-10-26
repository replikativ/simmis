(ns is.simm.uis.entity-view-test
  (:require [clojure.string :as str]
            [is.simm.uis.entity-view :as entity]
            [is.simm.documents.property-presentation-test :as data]
            [org.replikativ.spindel.dom.ssr :as ssr]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])))

(def company #uuid "60f22016-2119-4fc1-8390-59574460de55")
(def model
  {:entity {:kind :page :id company}
   :fields [{:id #uuid "6ad706fd-cec1-467c-8122-dde9f31b36e7" :label "Revenue"
             :present? true :cardinality :one :value-type :db.type/long :input {:text "9007199254740993"}}
            (assoc data/reference-field :id #uuid "4e79e8a3-2467-48af-871b-47c75eab2409" :label "Founder")]
   :reference-options (:reference-options data/reference-options)})

(deftest same-read-component-renders-default-and-styled-static-values-without-actions
  (let [normal (ssr/render-to-string (entity/view model {:title "Company"}))
        styled (ssr/render-to-string (entity/view model {:title "Company" :density :compact :layout :columns
                                                       :class "public-company-card"}))]
    (doseq [html [normal styled]]
      (is (str/includes? html "9007199254740993"))
      (is (str/includes? html "Sam"))
      (is (str/includes? html (str "data-entity-id=\"" company "\"")))
      (is (not (re-find #"<(?:input|button|select|a)(?:\s|>)" html)))
      (is (not (str/includes? html "onclick="))))
    (is (str/includes? styled "semantic-entity--compact"))
    (is (str/includes? styled "public-company-card"))))

(deftest field-data-and-labels-stay-data-in-static-rendering
  (let [malicious "<img src=x onerror=alert(1)>"
        source (assoc-in model [:fields 0] {:id (get-in model [:fields 0 :id]) :label malicious
                                          :present? true :cardinality :one :value-type :db.type/string
                                          :input {:text malicious} :attrs {:onclick "evil()"}})
        html (ssr/render-to-string (entity/view source {:title malicious}))]
    (is (str/includes? html "&lt;img"))
    (is (not (str/includes? html "<img")))
    (is (not (str/includes? html "evil()")))
    (is (not (str/includes? html "<script")))))

(deftest acquisition-state-never-looks-like-an-empty-success
  (let [pending (ssr/render-to-string (entity/view nil {}))
        held (ssr/render-to-string (entity/view {:status :unavailable} {}))
        empty-result (ssr/render-to-string (entity/view {:fields []} {}))]
    (is (str/includes? pending "Loading properties"))
    (is (str/includes? held "Properties unavailable"))
    (is (str/includes? empty-result "No properties selected"))
    (is (not= pending empty-result))
    (is (not= held empty-result))))

(deftest resolved-non-navigable-references-retain-their-label
  (let [source (update-in model [:reference-options data/person-type :items]
                          #(mapv (fn [item] (dissoc item :path)) %))
        html (ssr/render-to-string (entity/view source {}))]
    (is (str/includes? html (str "title=\"" data/founder "\"")))
    (is (str/includes? html ">Sam</span>"))
    (is (not (re-find #"<(?:a|button)(?:\s|>)" html)))))
