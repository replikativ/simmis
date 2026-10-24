(ns is.simm.uis.web.desktop.rich-tables-test
  (:require [cljs.test :refer-macros [deftest is]]
            [is.simm.documents.rich-document :as document]
            [is.simm.uis.web.desktop.rich-editor]
            [is.simm.uis.web.desktop.rich-tables :as tables]))

(defn table-model [body?]
  {:type "doc" :content
   [{:type "memoryBlock" :attrs {:key "block:table" :blockId "table"}
     :content [{:type "memoryRegion" :attrs {:key "region:table" :blockId "table"}
                :content [{:type "table" :content
                           (mapv (fn [row]
                                   {:type "tableRow" :content
                                    (mapv (fn [column]
                                            {:type (if (zero? row) "tableHeader" "tableCell")
                                             :attrs {:colspan 1 :rowspan 1 :colwidth nil
                                                     :align (nth ["left" "right" "center"] column)}
                                             :content [{:type "paragraph" :content [{:type "text" :text (str row column)}]}]})
                                          (range 3))}) (range (if body? 2 1)))}]}]}]})

(defn portable [editor]
  (js->clj (js/JSON.parse (js/JSON.stringify (.getJSON editor))) :keywordize-keys true))

(defn mount! [model]
  (let [el (.createElement js/document "simmis-rich-editor")]
    (.setAttribute el "data-document" "true")
    (.setAttribute el "data-model" (js/JSON.stringify (clj->js model)))
    (.setAttribute el "data-read-only" "false")
    (.appendChild (.-body js/document) el)
    el))

(defn cell-positions [editor]
  (let [out (volatile! [])]
    (.descendants (.. editor -state -doc)
                  (fn [node pos]
                    (when (contains? #{"tableHeader" "tableCell"} (.. node -type -name))
                      (vswap! out conj pos))))
    @out))

(defn key! [editor key shift?]
  (.dispatchEvent (.. editor -view -dom)
                  (js/KeyboardEvent. "keydown" #js {:key key :code key :bubbles true :cancelable true :shiftKey shift?})))

(deftest native-table-keeps-alignment-and-uses-existing-edit-undo-model
  (let [model (table-model true) el (mount! model) editor (.-simmisRichEditor el)]
    (try
      (is (= (document/normalized model) (document/normalized (portable editor))))
      (is (= 6 (.-length (.querySelectorAll el "th,td"))))
      (.. editor -commands (setTextSelection (+ 2 (nth (cell-positions editor) 3))))
      (let [before (portable editor)]
        (is (true? (.. editor -commands (insertContent "Edited"))))
        (is (not= before (portable editor)))
        (is (true? (.. editor -commands (undo))))
        (is (= before (portable editor))))
      (finally (.remove el)))))

(deftest tab-shift-tab-and-last-cell-row-insertion-preserve-alignment-in-one-undo
  (doseq [body? [true false]]
    (let [el (mount! (table-model body?)) editor (.-simmisRichEditor el)]
      (try
        (let [positions (cell-positions editor)]
          (.. editor -commands (setTextSelection (+ 2 (first positions))))
          (key! editor "Tab" false)
          (is (= (+ 2 (second positions)) (.. editor -state -selection -from)))
          (key! editor "Tab" true)
          (is (= (+ 2 (first positions)) (.. editor -state -selection -from)))
          (.. editor -commands (setTextSelection (+ 2 (last positions))))
          (let [before (portable editor)]
            (key! editor "Tab" false)
            (is (= (+ 3 (count positions)) (count (cell-positions editor))))
            (is (= ["left" "right" "center"]
                   (mapv #(get-in % [:attrs :align])
                         (-> (portable editor) :content first :content first :content first :content last :content))))
            (is (true? (.. editor -commands (undo))))
            (is (= before (portable editor)))))
        (finally (.remove el))))))

(deftest native-cell-selection-is-restored-and-unsupported-input-does-not-mutate
  (let [el (mount! (table-model true)) editor (.-simmisRichEditor el)
        positions (cell-positions editor) before (portable editor)]
    (try
      (.setAttribute el "data-selection"
                     (js/JSON.stringify #js {:native #js {:type "cell" :anchor (first positions) :head (last positions)}}))
      (is (= "cell" (aget (.toJSON (.. editor -state -selection)) "type")))
      (is (= before (portable editor)))
      (.. editor -commands (setTextSelection (+ 2 (first positions))))
      (let [reason (volatile! nil)]
        (.addEventListener el "input" #(when (aget (.-detail %) "blocked") (vreset! reason (aget (.-detail %) "reason"))))
        (key! editor "Enter" false)
        (is (string? @reason))
        (is (= before (portable editor))))
      (finally (.remove el)))))

(defn paste-event [html text]
  #js {:clipboardData #js {:getData (fn [kind] (if (= kind "text/html") html text))}})

(deftest clipboard-preflight-refuses-loss-before-the-native-parser
  (let [el (mount! (table-model true)) editor (.-simmisRichEditor el)]
    (try
      (doseq [html ["<table><caption>Must retain</caption><tr><th>A</th></tr></table>"
                    "<table><tr><th colspan='2'>A</th></tr></table>"
                    "<table><tr><th><p>A</p><p>B</p></th></tr></table>"
                    "<table><tr><th align='justify'>A</th></tr></table>"]]
        (is (true? (tables/unsupported-paste? (.-view editor) (paste-event html "")))))
      (is (false? (tables/unsupported-paste? (.-view editor) (paste-event "<table><tr><th>A</th></tr><tr><td>B</td></tr></table>" ""))))
      (.. editor -commands (setTextSelection (+ 2 (first (cell-positions editor)))))
      (is (true? (tables/unsupported-paste? (.-view editor) (paste-event "" "A\tB"))))
      (is (true? (tables/unsupported-paste? (.-view editor) (paste-event "<p>A</p><p>B</p>" "AB"))))
      (is (true? (tables/unsupported-paste? (.-view editor) (paste-event "<img alt=\"A\">" "A"))))
      (is (false? (tables/unsupported-paste? (.-view editor) (paste-event "<p><strong>A</strong></p>" "A"))))
      (is (false? (tables/unsupported-paste? (.-view editor) (paste-event "" "ordinary text"))))
      (finally (.remove el)))))

(deftest real-native-cell-clipboard-retains-its-supported-rectangle
  (let [el (mount! (table-model true)) editor (.-simmisRichEditor el)
        positions (cell-positions editor)]
    (try
      (.setAttribute el "data-selection"
                     (js/JSON.stringify #js {:native #js {:type "cell" :anchor (nth positions 3) :head (last positions)}}))
      (let [view (.-view editor)
            clipboard (.serializeForClipboard view (.content (.. editor -state -selection)))
            html (.. clipboard -dom -innerHTML)]
        (is (re-find #"data-pm-slice" html))
        (is (false? (tables/unsupported-paste? view (paste-event html (.-text clipboard)))))
        (is (true? (.pasteHTML view html)))
        (is (= (document/normalized (table-model true)) (document/normalized (portable editor)))))
      (finally (.remove el)))))

(deftest actual-rectangle-content-paste-is-one-undo-and-column-mismatch-is-refused
  (let [el (mount! (table-model true)) editor (.-simmisRichEditor el)
        view (.-view editor) positions (cell-positions editor)
        select! (fn [a b] (.setAttribute el "data-selection"
                                         (js/JSON.stringify #js {:native #js {:type "cell" :anchor a :head b}})))
        before (portable editor)]
    (try
      (select! (nth positions 3) (last positions))
      (let [clipboard (.serializeForClipboard view (.content (.. editor -state -selection)))
            dom (.-dom clipboard)]
        (set! (.-textContent (.querySelector dom "td p")) "Changed")
        (is (false? (tables/unsupported-paste? view (paste-event (.-innerHTML dom) "Changed"))))
        (is (true? (.pasteHTML view (.-innerHTML dom))))
        (is (not= before (portable editor)))
        (is (= "Changed" (.-textContent (nth (array-seq (.querySelectorAll el "td")) 0)))))
      (is (true? (.. editor -commands (undo))))
      (is (= before (portable editor)))
      (doseq [[source target] [[0 3] [3 4]]]
        (select! (nth positions source) (nth positions source))
        (let [clipboard (.serializeForClipboard view (.content (.. editor -state -selection)))
              html (.. clipboard -dom -innerHTML) blocked? (volatile! false)
              listener #(when (aget (.-detail %) "blocked") (vreset! blocked? true))]
          (.addEventListener el "input" listener)
          (try
            (select! (nth positions target) (nth positions target))
            (.pasteHTML view html)
            (is (= before (portable editor)))
            (is @blocked? "Unsupported source header/column alignment is visibly refused")
            (finally (.removeEventListener el "input" listener)))))
      (select! (nth positions 3) (nth positions 3))
      (is (true? (tables/unsupported-paste? view
                    (paste-event "<table><tr><td>A</td></tr></table><table><tr><td>B</td></tr></table>" "AB"))))
      (finally (.remove el)))))

(deftest stale-cell-selection-across-two-tables-is-not-restored
  (let [model (update-in (table-model true) [:content 0 :content 0 :content] #(conj % (first %)))
        el (mount! model) editor (.-simmisRichEditor el) positions (cell-positions editor)]
    (try
      (is (nil? (tables/restore-selection (.. editor -state -doc)
                    #js {:anchor (first positions) :head (last positions)})))
      (finally (.remove el)))))
