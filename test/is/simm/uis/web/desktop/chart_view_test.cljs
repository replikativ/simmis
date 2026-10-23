(ns is.simm.uis.web.desktop.chart-view-test
  (:require [cljs.test :refer-macros [deftest is]]
            [clojure.string :as str]
            [is.simm.documents.rich-document :as document]
            [is.simm.uis.web.desktop.rich-editor]
            [is.simm.uis.web.desktop.views.memory :as memory]
            [org.replikativ.spindel.dom.browser :as browser]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]))

(def block-id "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb")
(def chart {:kind :line :title "Quarterly sales <script>bad()</script>"
            :series [{:label "Revenue" :points [[0 1] [1 2]]}]})
(def model
  {:type "doc" :content
   [{:type "memoryBlock" :attrs {:key (str "block:" block-id) :blockId block-id}
     :content [{:type "memoryRegion" :attrs {:key "region:one" :blockId block-id}
                :content [{:type "paragraph" :content [{:type "text" :text "Editable explanation"}]}]}]}]})

(defn- native-model [^js editor]
  (js->clj (js/JSON.parse (js/JSON.stringify (.getJSON editor))) :keywordize-keys true))

(defn- mount! [parent data]
  (let [el (.createElement js/document "simmis-rich-editor")]
    (.setAttribute el "data-document" "true")
    (.setAttribute el "data-model" (js/JSON.stringify (clj->js model)))
    (.setAttribute el "data-charts" (js/JSON.stringify (clj->js {block-id data})))
    (.setAttribute el "data-read-only" "false")
    (.appendChild parent el)
    el))

(deftest continuous-node-view-keeps-one-editor-source-and-element-owned-projections
  (let [a (mount! (.-body js/document) chart)
        b (mount! (.-body js/document) (assoc chart :title "Other panel"))
        editor (.-simmisRichEditor a)
        projection (.querySelector a ".memory-chart-slot")
        svg (.querySelector projection "svg")]
    (try
      (is (= (document/normalized model) (document/normalized (native-model editor))))
      (is (= "http://www.w3.org/2000/svg" (.-namespaceURI svg)))
      (is (= 3 (.-length (.querySelectorAll projection "tr"))))
      (is (= 0 (.-length (.querySelectorAll projection "script, iframe, img, a, foreignObject"))))
      (is (str/includes? (.-textContent projection) "<script>bad()</script>"))
      (is (not (str/includes? (.getHTML editor) "Quarterly sales")) "clipboard/source serialization contains prose only")
      (.. editor -commands (setTextSelection 5))
      (is (true? (.. editor -commands (insertContent "X"))))
      (is (identical? svg (.querySelector projection "svg")) "ordinary typing retains the existing plot DOM")
      (is (= (document/topology model) (document/topology (native-model editor))))
      (let [before (native-model editor)]
        (.setAttribute a "data-charts" (js/JSON.stringify (clj->js {block-id (assoc chart :title "Changed data")})))
        (is (str/includes? (.-textContent projection) "Changed data"))
        (is (str/includes? (.-textContent b) "Other panel"))
        (is (not (str/includes? (.-textContent b) "Changed data")))
        (is (= before (native-model editor)) "data-only updates are not editor transactions")
        (.setAttribute a "data-charts" "{}")
        (is (zero? (.-childElementCount projection)))
        (is (= before (native-model editor))))
      (finally
        (.remove a) (.remove b)
        (is (nil? (.-simmisRichEditor a)))
        (is (nil? (.-simmisChartData a)))
        (is (nil? (.-simmisChartProjection projection)))
        (is (zero? (.-childElementCount projection)))))))

(deftest actual-read-view-places-the-same-chart-at-its-semantic-block-boundary
  (let [ctx (context/create-execution-context)
        container (.createElement js/document "div")
        source (str "<!-- simmis:block " block-id " -->\n\nExplanation\n\n<!-- /simmis:block -->\n")
        start (.indexOf source "Explanation") end (+ start (count "Explanation"))
        close (.indexOf source "<!-- /simmis:block")
        analysis {:semantic {:blocks [{:id (uuid block-id) :identity-status :explicit
                                      :from 0 :to (count source) :children []}]}
                  :slots [{:from 0 :to start :kind :HtmlBlock}
                          {:from start :to end :kind :Paragraph
                           :model {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text "Explanation"}]}]}}
                          {:from close :to (count source) :kind :HtmlBlock}]}
        draft {:mode :read :text source :analysis analysis}
        discharge (atom nil)]
    (.appendChild (.-body js/document) container)
    (try
      (binding [ec/*execution-context* ctx]
        (reset! discharge (browser/mount! container (memory/document-view "wiki/a.md" draft
                                                                                        {:chart-models (constantly {block-id chart})} false)))
        (is (str/includes? (.-textContent container) "Explanation"))
        (is (= 1 (.-length (.querySelectorAll container "svg.memory-chart-plot"))))
        (is (= "http://www.w3.org/2000/svg" (.-namespaceURI (.querySelector container "svg"))))
        (is (= source (:text draft))))
      (finally (when @discharge (browser/unmount! @discharge))
               (.remove container) (context/stop-context! ctx)))))
