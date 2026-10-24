(ns is.simm.uis.inline-chart-test
  (:require [cljs.test :refer-macros [deftest is]]
            [is.simm.uis.web.desktop.rich-editor]
            [is.simm.uis.web.desktop.views.memory :as memory]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.dom.browser :as browser]))

(def json-source "{\"version\":1,\"kind\":\"line\",\"series\":[{\"label\":\"Revenue\",\"points\":[[1,100],[2,140]]}]}")
(def chart {:kind :line :series [{:label "Revenue" :points [[1 100] [2 140]]}]})
(def preview {:language "simmis-chart" :text json-source :status :ready :data chart})
(def code {:type "codeBlock" :attrs {:language "simmis-chart"} :content [{:type "text" :text json-source}]})
(defn- model [n]
  {:type "doc" :content [{:type "memoryRegion" :attrs {:key "region:0" :blockId nil}
                         :content (into [{:type "paragraph" :content [{:type "text" :text "Explanation"}]}]
                                        (repeat n code))}]})
(defn- config! [el limit]
  (.setAttribute el "data-inline-charts" (js/JSON.stringify (clj->js {:entries [preview] :limit limit}))))
(defn- mount! [n]
  (let [el (.createElement js/document "simmis-rich-editor")]
    (.setAttribute el "data-model" (js/JSON.stringify (clj->js (model n))))
    (.setAttribute el "data-document" "true") (.setAttribute el "data-read-only" "false")
    (config! el 4)
    (.appendChild (.-body js/document) el)
    el))

(deftest inline-previews-preserve-source-undo-and-unrelated-typing
  (let [el (mount! 1) editor (.-simmisRichEditor el)
        original (js/JSON.stringify (.getJSON editor))
        svg (.querySelector el ".memory-inline-chart svg.memory-chart-plot")]
    (try
      (is (some? svg))
      (is (= "http://www.w3.org/2000/svg" (.-namespaceURI svg)))
      (is (not (re-find #"Chart preview|<svg|<button" (.getHTML editor))))
      (is (every? #(.-hidden (.-parentElement %))
                  (array-seq (.querySelectorAll el "button[aria-label='Run saved Clojure cell']")))
          "A chart never gains a Clojure Run action")
      (.dispatch (.-view editor) (.insertText (.. editor -state -tr) "!" 3))
      (is (identical? svg (.querySelector el ".memory-inline-chart svg.memory-chart-plot")))
      (let [position (volatile! nil)]
        (.descendants (.. editor -state -doc)
                      (fn [node pos] (when (= "codeBlock" (.. node -type -name)) (vreset! position (inc pos)))))
        (.dispatch (.-view editor) (.insertText (.. editor -state -tr) " " @position))
        (is (nil? (.querySelector el ".memory-inline-chart svg.memory-chart-plot")))
        (.undo (.-commands editor))
        (is (some? (.querySelector el ".memory-inline-chart svg.memory-chart-plot")))
        (.undo (.-commands editor))
        (is (= original (js/JSON.stringify (.getJSON editor)))))
      (.removeAttribute el "data-inline-charts")
      (is (nil? (.querySelector el ".memory-inline-chart svg.memory-chart-plot")))
      (is (= original (js/JSON.stringify (.getJSON editor))))
      (finally
        (.remove el)
        (is (nil? (.-simmisInlineChartContext el)))))))

(deftest duplicate-fences-cannot-exceed-the-shared-preview-budget
  (let [el (mount! 7) editor (.-simmisRichEditor el) original (js/JSON.stringify (.getJSON editor))]
    (try
      (is (= 4 (.-length (.querySelectorAll el ".memory-inline-chart svg.memory-chart-plot"))))
      (config! el 2)
      (is (= 2 (.-length (.querySelectorAll el ".memory-inline-chart svg.memory-chart-plot"))))
      (config! el 0)
      (is (zero? (.-length (.querySelectorAll el ".memory-inline-chart svg.memory-chart-plot"))))
      (is (= original (js/JSON.stringify (.getJSON editor))))
      (finally (.remove el)))))

(deftest read-renderer-caps-identical-fences-and-reconciles-budget-changes
  (let [ctx (context/create-execution-context) container (.createElement js/document "div") release (atom nil)
        source "1234567"
        syntax (mapv (fn [i] {:kind :FencedCodeBlock :from i :to (inc i) :language "simmis-chart" :literal json-source}) (range 7))
        slots (mapv #(assoc (select-keys % [:from :to :kind]) :model {:type "doc" :content [code]}) syntax)
        draft {:mode :read :text source :analysis {:nodes syntax :slots slots :semantic {:status :ready :blocks []}}}
        frame (fn [typed]
                (memory/document-view "wiki/Charts.md" draft
                                      {:chart-models (fn [_] typed) :inline-chart-previews (fn [_] [preview])} false))]
    (.appendChild (.-body js/document) container)
    (try
      (binding [ec/*execution-context* ctx]
        (let [first-tree (frame {}) next-tree (frame {:a chart :b chart})]
          (reset! release (browser/mount! container first-tree))
          (is (= 4 (.-length (.querySelectorAll container "svg.memory-chart-plot"))))
          (browser/refresh! @release first-tree next-tree)
          (is (= 2 (.-length (.querySelectorAll container "svg.memory-chart-plot"))))
          (is (= 7 (.-length (.querySelectorAll container "pre > code"))))))
      (finally
        (when @release (browser/unmount! @release))
        (.remove container) (context/stop-context! ctx)))))

(def mixed-ids ["e1d35fd5-47f9-402f-a6ce-e513313552ed" "7b06bf8b-9fdc-498b-8d40-1c75c3a04bcf"])

(defn- mixed-draft [mode]
  (let [fence (str "```simmis-chart\n" json-source "\n```\n\n")
        parts (reduce
               (fn [{:keys [text] :as result} [index id]]
                 (let [start (count text) open (str "<!-- simmis:block " id " -->\n\n")
                       body-from (+ start (count open)) close-from (+ body-from (* 3 (count fence)))
                       close "<!-- /simmis:block -->\n\n"
                       nodes (mapv (fn [n] {:kind :FencedCodeBlock :language "simmis-chart" :literal (str json-source "\n")
                                            :from (+ body-from (* n (count fence)))
                                            :to (+ body-from (* n (count fence)) (- (count fence) 1))}) (range 3))
                       block {:id (uuid id) :identity-status :explicit :from start :to (+ close-from (count close))
                              :children [] :syntax nodes}]
                   (-> result
                       (assoc :text (str text open (apply str (repeat 3 fence)) close))
                       (update :blocks conj block)
                       (update :slots into (into [{:from start :to body-from :kind :HtmlBlock}]
                                                 (concat (map #(assoc (select-keys % [:from :to :kind])
                                                                      :model {:type "doc" :content [code]}) nodes)
                                                         [{:from close-from :to (:to block) :kind :HtmlBlock}])))
                       (update :content conj {:type "memoryBlock" :attrs {:key (str "block:" id) :blockId id}
                                              :content [{:type "memoryRegion" :attrs {:key (str "region:" index) :blockId id}
                                                         :content [code code code]}]}))))
               {:text "" :blocks [] :slots [] :content []} (map-indexed vector mixed-ids))
        analysis {:slots (:slots parts) :semantic {:status :ready :blocks (:blocks parts)}}]
    {:mode mode :text (:text parts) :analysis analysis
     :rich-base {:id :mixed :text (:text parts) :analysis analysis
                 :document {:model {:type "doc" :content (:content parts)}}}}))

(deftest real-typed-and-inline-charts-share-four-mounts-in-each-view
  (doseq [mode [:read :outline :rich]]
    (let [ctx (context/create-execution-context) container (.createElement js/document "div") release (atom nil)
          draft (mixed-draft mode) data (zipmap mixed-ids (repeat chart))
          frame (fn [typed]
                  (memory/document-view "wiki/Mixed.md" draft
                                        (merge {:owner :mixed :chart-models (fn [_] typed)
                                                :inline-chart-previews (fn [_] [preview])}
                                               (zipmap [:cell-config :block-links :linked-block :jump] (repeat (constantly nil)))) false))]
      (.appendChild (.-body js/document) container)
      (try
        (binding [ec/*execution-context* ctx]
          (let [original (frame data) changed (frame (dissoc data (second mixed-ids)))]
            (reset! release (browser/mount! container original))
            (is (= 4 (.-length (.querySelectorAll container "svg.memory-chart-plot"))))
            (is (= 2 (.-length (.querySelectorAll container ".memory-inline-chart svg.memory-chart-plot"))))
            (browser/refresh! @release original changed)
            (is (= 4 (.-length (.querySelectorAll container "svg.memory-chart-plot"))))
            (is (= 3 (.-length (.querySelectorAll container ".memory-inline-chart svg.memory-chart-plot"))))))
        (finally
          (when @release (browser/unmount! @release))
          (.remove container) (context/stop-context! ctx))))))
