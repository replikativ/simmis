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
