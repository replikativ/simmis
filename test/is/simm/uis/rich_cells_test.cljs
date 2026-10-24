(ns is.simm.uis.rich-cells-test
  (:require [cljs.test :refer-macros [deftest is]]
            [is.simm.documents.rich-text :as rich]
            [is.simm.uis.web.desktop.rich-editor]))

(def model {:type "doc" :content [{:type "memoryRegion" :attrs {:key "region:0" :blockId nil}
                                  :content [{:type "codeBlock" :attrs {:language "clj"}
                                             :content [{:type "text" :text "(+ 1 2)"}]}]}]})
(def config {:model model :namespace "user" :enabled? true
             :cells [{:key "cell:0:22" :from 0 :to 22 :node-path [0 0] :runnable? true}]})

(deftest explicit-run-controls-use-the-latest-context-callback-without-serializing-results
  (let [el (.createElement js/document "simmis-rich-editor") events (atom [])]
    (.setAttribute el "data-model" (js/JSON.stringify (clj->js model)))
    (.setAttribute el "data-cells" (js/JSON.stringify (clj->js config)))
    (.setAttribute el "data-document" "true")
    (.setAttribute el "data-read-only" "false")
    (aset el "onsimmis-run-cell" #(swap! events conj (aget (.-detail %) "key")))
    (.appendChild (.-body js/document) el)
    (try
      (let [editor (.-simmisRichEditor el) before (js/JSON.stringify (.getJSON editor))
            button (.querySelector el "button[aria-label='Run saved Clojure cell']")]
        (is (empty? @events) "Mounting, highlighting and reading never run source")
        (is (not (.-hidden (.-parentElement button))))
        (.click button)
        (is (= ["cell:0:22"] @events))
        (let [next-events (atom 0)]
          (aset el "onsimmis-run-cell" (fn [_] (swap! next-events inc)))
          (.click button)
          (is (= 1 @next-events))
          (is (= 1 (count @events))))
        (.setAttribute el "data-cells"
                       (js/JSON.stringify (clj->js (assoc config :receipts
                                                         [{:id "receipt-1" :success true :namespace "user" :value "<script>3</script>"
                                                           :source {:from 0 :to 22}}]))))
        (is (re-find #"<script>3</script>" (.-textContent (.querySelector el ".memory-code-cell-result"))))
        (is (nil? (.querySelector el "script")))
        (is (= before (js/JSON.stringify (.getJSON editor))))
        (is (not (re-find #"receipt|button|Result" (.getHTML editor))))
        ;; Unreported native content must disable correspondence immediately.
        (.dispatch (.-view editor) (.insertText (.. editor -state -tr) "x" 2))
        (is (.-hidden (.-parentElement button)))
        (.undo (.-commands editor))
        (is (not (.-hidden (.-parentElement button))))
        (is (= before (js/JSON.stringify (.getJSON editor))))
        (.remove el)
        (let [n (atom 0)]
          (aset el "onsimmis-run-cell" (fn [_] (swap! n inc)))
          (.dispatchEvent el (js/CustomEvent. "simmis-run-cell" #js {:detail #js {:key "cell:0:22"}}))
          (is (zero? @n) "Detach releases the property bridge")
          (.appendChild (.-body js/document) el)
          (.click (.querySelector el "button[aria-label='Run saved Clojure cell']"))
          (is (= 1 @n) "Reattach owns exactly one listener")))
      (finally (.remove el)))))

(deftest saved-cell-presentations-remain-outside-the-model-and-release-on-edit
  (let [el (.createElement js/document "simmis-rich-editor")
        table {:profile {:id :simmis/cell-result :version 1} :kind :table
               :columns ["Amount" "Evidence"] :rows [["9007199254740993" "<img src=x onerror=alert(1)>"]]}
        chart {:profile {:id :simmis/cell-result :version 1} :kind :chart
               :data {:kind :line :series [{:label "Forecast" :points [[0 1] [1 2]]}]}}
        receipt {:id "result-1" :success true :namespace "user" :value "Original printed receipt" :source {:from 0 :to 22}}
        configure! (fn [presentation]
                     (.setAttribute el "data-cells"
                                    (js/JSON.stringify
                                     (clj->js (assoc config :presentation-id "result-1"
                                                           :receipts [(assoc receipt :presentation presentation)])))))]
    (.setAttribute el "data-model" (js/JSON.stringify (clj->js model)))
    (.setAttribute el "data-document" "true") (.setAttribute el "data-read-only" "false")
    (configure! table)
    (.appendChild (.-body js/document) el)
    (try
      (let [^js editor (.-simmisRichEditor el) original (js/JSON.stringify (.getJSON editor))]
        (is (= ["9007199254740993" "<img src=x onerror=alert(1)>"]
               (mapv #(.-textContent %) (array-seq (.querySelectorAll el ".memory-code-cell-result td")))))
        (is (nil? (.querySelector el "img")))
        (is (.includes (.-textContent el) "Original printed receipt"))
        (is (= original (js/JSON.stringify (.getJSON editor))))
        (is (not (re-find #"9007199254740993|Forecast|Result" (.getHTML editor))))
        (configure! chart)
        (is (= 1 (.-length (.querySelectorAll el "svg.memory-chart-plot"))))
        (.dispatch (.-view editor) (.insertText (.. editor -state -tr) "x" 2))
        (is (nil? (.querySelector el "svg.memory-chart-plot")))
        (.undo (.-commands editor))
        (is (= original (js/JSON.stringify (.getJSON editor))))
        (is (= 1 (.-length (.querySelectorAll el "svg.memory-chart-plot"))))
        (.setAttribute el "data-cells" (js/JSON.stringify (clj->js (dissoc config :presentation-id))))
        (is (nil? (.querySelector el "svg.memory-chart-plot")))
        (is (= original (js/JSON.stringify (.getJSON editor)))))
      (finally (.remove el)))))
