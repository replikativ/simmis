(ns is.simm.documents.chart-scene-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [clojure.string :as str]
            [is.simm.documents.chart-scene :as chart]))

(defn- nodes [tree] (filter map? (tree-seq map? :children tree)))

(deftest fixed-scene-never-turns-labels-into-elements-or-attributes
  (let [label "<script src='https://example.com'>bad()</script>"
        scene (chart/scene {:kind :line :title label
                            :series [{:label label :points [[0 1] [1 2]]}]})
        elements (nodes scene)]
    (is (every? #{:figure :figcaption :svg :path :text :p :details :summary
                  :ul :li :circle :table :caption :thead :tbody :tr :th :td} (map :tag elements)))
    (is (= label (get-in scene [:children 0 :children 0])))
    (is (every? #(not-any? #{:innerHTML :href :src :style :on-click} (keys (:attrs %))) elements))
    (is (= 3 (count (filter #(= :tr (:tag %)) elements))))))

(deftest finite-scaling-for-constant-tiny-extreme-and-negative-data
  (doseq [[kind points] [[:line [[0 0]]] [:line [[1 2] [1 2]]]
                         [:point [[-1.0e12 1.0e12] [1.0e12 -1.0e12]]]
                         [:line [[1.0e-300 2.0e-300] [2.0e-300 3.0e-300]]]
                         [:bar [["Loss" -4] ["Gain" 9]]] [:bar [["Zero" 0]]]]]
    (let [scene (chart/scene {:kind kind :series [{:label "A" :points points}]})
          attributes (mapcat (comp vals :attrs) (nodes scene))]
      (is (not-any? #(and (number? %) (not #?(:clj (Double/isFinite (double %))
                                                          :cljs (js/Number.isFinite %)))) attributes))
      (is (not-any? #(and (string? %) (or (str/includes? % "NaN") (str/includes? % "Infinity"))) attributes))
      (is (= (inc (count points)) (count (filter #(= :tr (:tag %)) (nodes scene))))))))

(deftest axes-identify-categories-and-bound-visible-tick-count
  (let [scene (chart/scene {:kind :bar :series [{:label "Costs" :points [["Research" 12] ["Delivery" 9]]}]})
        labels (mapcat :children (filter #(= :text (:tag %)) (nodes scene)))]
    (is (some #{"Research"} labels))
    (is (some #{"Delivery"} labels)))
  (let [points (mapv #(vector (str "Category " %) %) (range 100))
        scene (chart/scene {:kind :bar :series [{:label "A" :points points}]})
        labels (mapcat :children (filter #(and (= :text (:tag %)) (= 280 (get-in % [:attrs :y]))) (nodes scene)))]
    (is (= 8 (count labels)))
    (is (= "Category 0" (first labels)))
    (is (= "Category 99" (last labels)))))
