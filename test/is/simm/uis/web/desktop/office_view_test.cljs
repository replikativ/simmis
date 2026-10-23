(ns is.simm.uis.web.desktop.office-view-test
  (:require [cljs.test :refer-macros [deftest is]]
            [clojure.string :as str]
            [is.simm.uis.web.desktop.office-view :as office]
            [org.replikativ.spindel.dom.browser :as browser]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- paragraph [id text]
  {:kind :paragraph :id [id] :runs [{:kind :text :id [id 0] :text text :marks {}}]})

(def reading
  {:status :ready :notice "Simplified saved document."
   :blocks [(assoc (paragraph 0 "First heading") :heading 2)
            (assoc (paragraph 1 "A list item") :list? true)
            (paragraph 2 "<script>visible source text</script>")
            {:kind :table :id [3]
             :rows [{:id [3 0] :header? true :cells [{:id [3 0 0] :blocks [(paragraph 10 "Product")]}
                                                     {:id [3 0 1] :blocks [(paragraph 11 "Revenue")]}]}
                    {:id [3 1] :cells [{:id [3 1 0] :blocks [(paragraph 12 "Research")]}
                                       {:id [3 1 1] :blocks [(paragraph 13 "$1200")]}]}]}]
   :diagnostics ["No external resources are loaded."]})

(deftest native-document-reading-retains-all-siblings-tables-and-list-markers
  (let [ctx (context/create-execution-context) container (.createElement js/document "div")
        release (atom nil)]
    (.appendChild (.-body js/document) container)
    (try
      (binding [ec/*execution-context* ctx]
        (reset! release (browser/mount! container (office/reading-view reading)))
        (is (= "First heading" (.-textContent (.querySelector container "h2"))))
        (is (= "• " (.-textContent (.querySelector container ".office-list-indicator"))))
        (is (= 2 (.-length (.querySelectorAll container "tr"))))
        (is (= 2 (.-length (.querySelectorAll container "th"))))
        (is (= 2 (.-length (.querySelectorAll container "td"))))
        (is (= ["Product" "Revenue" "Research" "$1200"]
               (mapv #(.-textContent %) (array-seq (.querySelectorAll container "th, td")))))
        (is (str/includes? (.-textContent container) "<script>visible source text</script>"))
        (is (zero? (.-length (.querySelectorAll container "script, a, img, iframe, [src], [href]")))))
      (finally
        (when @release (browser/unmount! @release))
        (.remove container)
        (context/stop-context! ctx)))))

(deftest document-controls-keep-xml-drafts-visible-as-retained-work
  (let [ctx (context/create-execution-context) container (.createElement js/document "div")
        release (atom nil) changes (atom [])
        draft {:source {:entry :saved} :office {:reading reading :reading-entry :saved
                                                :part "word/document.xml" :text "Human XML" :saved-text "Saved XML"}}
        actions {:owner :test :change (fn [& args] (swap! changes conj args))}]
    (.appendChild (.-body js/document) container)
    (try
      (binding [ec/*execution-context* ctx]
        (reset! release (browser/mount! container (office/view {:path "report.docx" :draft draft :state {}
                                                                :actions actions :write? true :coherent? true})))
        (is (str/includes? (.-textContent container) "Unsaved XML changes are retained"))
        (is (nil? (.querySelector container ".cm-editor")) "XML editor is not injected into Document mode")
        (let [buttons (array-seq (.querySelectorAll container ".office-mode-controls button"))]
          (is (= ["true" "false"] (mapv #(.getAttribute % "aria-pressed") buttons))))
        (let [button (first (filter #(= "Advanced XML" (.-textContent %)) (array-seq (.querySelectorAll container "button"))))]
          (.click button)
          (is (= [["report.docx" assoc :office-mode :xml]] @changes))))
      (finally
        (when @release (browser/unmount! @release))
        (.remove container)
        (context/stop-context! ctx)))))
