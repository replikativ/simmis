(ns is.simm.uis.entity-view-browser-test
  (:require [cljs.test :refer-macros [deftest is]]
            [is.simm.uis.entity-view :as entity]
            [is.simm.uis.entity-view-test :as data]
            [org.replikativ.spindel.dom.browser :as browser]
            [org.replikativ.spindel.dom.elements :as el]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.context :as context]))

(defn- pair-view [model]
  (el/div {}
          (entity/view model {:title "Company"})
          (entity/view model {:title "Company preview" :density :compact :layout :columns
                              :class "public-company-card"})))

(deftest two-presentations-have-distinct-mounts-and-the-same-semantic-identity
  (let [ctx (context/create-execution-context)
        container (.createElement js/document "div")
        discharge (atom nil)]
    (.appendChild (.-body js/document) container)
    (try
      (binding [ec/*execution-context* ctx]
        (let [initial (pair-view data/model)]
          (reset! discharge (browser/mount! container initial))
          (let [sections (array-seq (.querySelectorAll container "section"))
                nodes (tree-seq #(and (map? %) (:children %)) #(seq @(:children %)) initial)
                addresses (keep :addr nodes)]
            (is (= 2 (count sections)))
            (is (= [(str data/company) (str data/company)]
                   (mapv #(.getAttribute % "data-entity-id") sections)))
            (is (= (count addresses) (count (set addresses)))
                "Semantic identity does not collapse independent DOM mounts")
            (is (not (identical? (first sections) (second sections))))
            (is (.contains (.-classList (second sections)) "public-company-card"))
            (is (every? #(.includes (.-textContent %) "Sam") sections)))
          (is (nil? (.querySelector container "button, input, select, a, [contenteditable]")))
          (let [next-model (assoc-in data/model [:fields 0 :input :text] "42")
                updated (pair-view next-model)]
            (browser/refresh! @discharge initial updated)
            (is (= ["42" "42"]
                   (mapv #(.-textContent %) (array-seq (.querySelectorAll container "section .semantic-entity-field:first-child dd span"))))
                "Both read presentations consume the supplied new value")
            (is (= "9007199254740993" (get-in data/model [:fields 0 :input :text]))
                "Rendering never modifies the supplied source projection"))))
      (finally
        (when @discharge (browser/unmount! @discharge))
        (.remove container)
        (context/stop-context! ctx)))))
