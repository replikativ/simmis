(ns is.simm.uis.github-memory-render-test
  (:require [cljs.test :refer-macros [deftest is]]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.dom.browser :as browser]
            [is.simm.documents.rich-text :as rich]
            [is.simm.uis.web.desktop.views.memory :as memory]
            [is.simm.uis.web.desktop.widget-sandbox :as widgets]))

(deftest opening-imported-markdown-keeps-code-and-legacy-widget-fences-inert
  (let [ctx (context/create-execution-context)
        container (.createElement js/document "div") release (atom nil)
        executions (atom [])
        code "(throw (js/Error. \"Imported source must not execute\"))"
        nodes (mapv (fn [index language]
                      {:kind :FencedCodeBlock :from index :to (inc index)
                       :language language :literal code :children []})
                    (range) ["clojure" "widget" "simmis-ui"])
        source (apply str (map #(str "```" (:language %) "\n" code "\n```\n") nodes))
        analysis {:slots (mapv #(assoc (select-keys % [:from :to :kind]) :model (rich/model %)) nodes)
                  :semantic {:status :ready
                             :blocks (mapv (fn [node] {:id (str "block-" (:from node))
                                                      :identity-status :provisional
                                                      :from (:from node) :to (:to node)
                                                      :syntax [node] :children []}) nodes)}}
        draft {:mode :read :text source :source {:text source :kind :markdown} :analysis analysis}
        record (fn [& args] (swap! executions conj args))
        actions {:evaluate record :run-cell record :repl-evaluate record}]
    (.appendChild (.-body js/document) container)
    (try
      (with-redefs [widgets/eval-widget
                    (fn ([_ _] (record :widget) {:error "must stay inert"})
                        ([_ _ _] (record :widget) {:error "must stay inert"})
                        ([_ _ _ _] (record :widget) {:error "must stay inert"}))]
        (binding [ec/*execution-context* ctx]
          (reset! release (browser/mount! container (memory/document-view "README.md" draft actions false)))
          (is (= 3 (count (array-seq (.querySelectorAll container "pre")))))
          (is (.includes (.-textContent container) "Imported source must not execute"))
          (is (empty? @executions) "Reading does not call SCI, legacy widgets or evaluation actions")
          (is (nil? (.querySelector container "script, iframe, .widget-block")))
          (is (= source (:text draft)))))
      (finally
        (when @release (browser/unmount! @release))
        (.remove container)
        (context/stop-context! ctx)))))
