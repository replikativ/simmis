(ns is.simm.uis.web.desktop.math-view-test
  (:require [cljs.test :refer-macros [deftest is]]
            [clojure.string :as str]
            [is.simm.documents.rich-document :as document]
            [is.simm.uis.web.desktop.rich-editor]
            [is.simm.uis.web.desktop.math-view :as math]
            [org.replikativ.spindel.dom.browser :as browser]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]))

(def inline {:type "inlineMath" :attrs {:sourceKey "math:7:12" :raw "$x^2$" :tex "x^2"}})
(def display {:type "displayMath" :attrs {:sourceKey "math:14:26" :raw "$$\r\ny^2\r\n$$" :tex "y^2\r\n"}})
(def model {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text "Before "} inline]}
                                  display {:type "paragraph" :content [{:type "text" :text "After"}]}]})

(deftest native-inline-and-display-formulas-keep-editor-source-and-undo
  (let [element (.createElement js/document "simmis-rich-editor")
        continuous {:type "doc" :content [{:type "memoryRegion" :attrs {:key "region:0"}
                                          :content (:content model)}]}]
    (.setAttribute element "data-document" "true")
    (.setAttribute element "data-model" (js/JSON.stringify (clj->js continuous)))
    (.appendChild (.-body js/document) element)
    (let [editor (.-simmisRichEditor element)]
      (try
        (is (= 2 (.-length (.querySelectorAll element "math"))) "both formulas provide accessible MathML")
        (is (= 1 (.-length (.querySelectorAll element ".memory-math-display"))))
        (is (= (document/normalized continuous)
               (document/normalized (js->clj (js/JSON.parse (js/JSON.stringify (.getJSON editor))) :keywordize-keys true))))
        (is (not (str/includes? (.getHTML editor) "katex")))
        (is (str/includes? (.getHTML editor) "$x^2$"))
        (.. editor -commands (setTextSelection 2))
        (is (true? (.. editor -commands (insertContent "X"))))
        (is (= 2 (.-length (.querySelectorAll element "math"))))
        (is (true? (.. editor -commands (undo))))
        (is (= 2 (.-length (.querySelectorAll element "math"))))
        (finally (.remove element)
                 (is (nil? (.-simmisRichEditor element))))))))

(deftest formula-errors-and-hostile-commands-are-inert-with-independent-macro-scopes
  (let [element (.createElement js/document "span")]
    (doseq [tex ["\\href{https://example.org}{link}" "\\includegraphics{https://example.org/image}"
                 "\\htmlClass{attack}{x}" "\\rule{99999em}{99999em}" "\\def\\loop{\\loop}\\loop"]]
      (math/paint! element tex tex false)
      (is (zero? (.-length (.querySelectorAll element "a,img,iframe,script,[src],[href],.attack")))))
    (math/paint! element "\\gdef\\private{x}\\private" "first" false)
    (math/paint! element "\\private" "$\\private$" false)
    (is (= "$\\private$" (.-textContent element)))
    (math/paint! element "\\frac{" "$\\frac{$" false)
    (is (= "$\\frac{$" (.-textContent element)))))

(deftest read-projection-owns-its-element-and-clears-on-detach
  (let [ctx (context/create-execution-context) container (.createElement js/document "div")
        release (atom nil) element (atom nil)]
    (.appendChild (.-body js/document) container)
    (try
      (binding [ec/*execution-context* ctx]
        (reset! release (browser/mount! container (math/view inline)))
        (reset! element (.querySelector container "simmis-math"))
        (is (= 1 (.-length (.querySelectorAll container "math"))))
        (browser/unmount! @release)
        (reset! release nil)
        ;; browser/unmount! clears tracking; its documented contract leaves
        ;; DOM in place. Native disconnectedCallback runs on actual removal.
        (.remove container)
        (is (zero? (.-childElementCount @element))))
      (finally (when @release (browser/unmount! @release))
               (.remove container) (context/stop-context! ctx)))))
