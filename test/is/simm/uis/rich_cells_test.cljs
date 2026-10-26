(ns is.simm.uis.rich-cells-test
  (:require [cljs.test :refer-macros [deftest is async]]
            ["@tiptap/pm/history" :refer [undoDepth]]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.rich-saved-views :as saved-views]
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


(def view-id #uuid "075aa061-e6e8-44ec-80cc-49de22c149eb")
(def view-entry {:from 10 :to 30 :view view-id :status :ready})
(def view-model {:type "doc" :content [{:type "memoryRegion" :attrs {:key "region:0" :blockId nil}
                                       :content [{:type "paragraph" :content [{:type "text" :text "Before"}]}
                                                 {:type "codeBlock" :attrs {:language "simmis-view"}
                                                  :content [{:type "text" :text "query"}]}
                                                 {:type "paragraph" :content [{:type "text" :text "After"}]}]}]})
(def view-config {:model view-model :entries [{:path [0 1] :from 10 :to 30 :view (str view-id)}]})

(defn- view-element [callback]
  (let [native (.createElement js/document "simmis-rich-editor")]
    (.setAttribute native "data-model" (js/JSON.stringify (clj->js view-model)))
    (.setAttribute native "data-saved-views" (js/JSON.stringify (clj->js view-config)))
    (.setAttribute native "data-document" "true")
    (.setAttribute native "data-read-only" "false")
    (aset native "onsimmis-present-view" callback)
    native))

(deftest saved-view-islands-preserve-native-source-selection-undo-and-release-caches
  (let [ctx (context/create-execution-context) refs (atom []) clicks (atom 0)
        ref! #(swap! refs conj (some? %))
        callback (saved-views/presenter ctx :editor [view-entry]
                   (fn [_] (el/div {:ref ref!}
                                   (el/table {} (el/tbody {} (el/tr {} (el/td {} "Rendered result"))))
                                   (el/button {:on-click #(swap! clicks inc)} "Inspect result"))))
        native (view-element callback)]
    (.appendChild (.-body js/document) native)
    (try
      (let [editor (.-simmisRichEditor native) view (.-view editor)
            before (js/JSON.stringify (.getJSON editor))
            pre (.querySelector native ".memory-code-cell > pre")
            island (.querySelector native ".memory-code-cell > .memory-inline-saved-view")]
        (is (= [true] @refs))
        (is (.includes (.-textContent island) "Rendered result"))
        (is (.-hidden pre))
        (is (= 0 (undoDepth (.-state editor))))
        (.click (.querySelector island "button"))
        (is (= 1 @clicks))
        (is (= before (js/JSON.stringify (.getJSON editor))))
        (is (not (.includes (.getHTML editor) "Rendered result")))
        (.focus view)
        (.setTextSelection (.-commands editor) 10)
        (is (not (.-hidden pre)) "Caret entry reveals the preserved contentDOM")
        (is (= before (js/JSON.stringify (.getJSON editor))))
        (is (= 0 (undoDepth (.-state editor))))
        (.dispatch view (.insertText (.. editor -state -tr) "x" 10))
        (is (.-hidden island))
        (is (not (.-hidden pre)))
        (is (= [true false] @refs))
        (.undo (.-commands editor))
        (is (= before (js/JSON.stringify (.getJSON editor))))
        (is (not (.-hidden island)))
        (is (= [true false true] @refs))
        (.remove native)
        (is (= [true false true false] @refs))
        (is (nil? (.-simmisSavedViewMount island)))
        (binding [ec/*execution-context* ctx]
          (doseq [path [[:dom/cache] [:dom/attr-cache] [:dom/keyed-cache] [:dom/foreign]]]
            (is (empty? (ec/get-state path))))))
      (finally (.remove native) (context/stop-context! ctx)))))

(deftest callback-only-updates-coalesce-and-retired-editors-cannot-repaint
  (async done
    (let [ctx (context/create-execution-context) calls (atom [])
          callback (fn [label]
                     (let [render (saved-views/presenter ctx :same-editor-owner [view-entry]
                                    (fn [_] (el/div {} label)))]
                       (fn [key token] (swap! calls conj label) (render key token))))
          a (view-element (callback "a")) b (view-element (callback "b"))
          finish (fn [] (.remove a) (.remove b) (context/stop-context! ctx) (done))]
      (.appendChild (.-body js/document) a)
      (.appendChild (.-body js/document) b)
      (let [before (js/JSON.stringify (.getJSON (.-simmisRichEditor a)))]
        (is (= ["a" "b"] @calls))
        (aset a "onsimmis-present-view" (callback "intermediate"))
        (aset a "onsimmis-present-view" (callback "latest"))
        (js/queueMicrotask
         (fn []
           (try
             (is (= ["a" "b" "latest"] @calls))
             (is (.includes (.-textContent (.querySelector a ".memory-inline-saved-view")) "latest"))
             (is (.includes (.-textContent (.querySelector b ".memory-inline-saved-view")) "b"))
             (is (= before (js/JSON.stringify (.getJSON (.-simmisRichEditor a)))) )
             (let [scope-a (:scope (.-simmisSavedViewMount (.querySelector a ".memory-inline-saved-view")))
                   scope-b (:scope (.-simmisSavedViewMount (.querySelector b ".memory-inline-saved-view")))]
               (is (not= scope-a scope-b) "Same view UUID and owner still have distinct native mount scopes"))
             (aset a "onsimmis-present-view" (callback "retired"))
             (.remove a)
             (js/queueMicrotask
              (fn []
                (try
                  (is (= ["a" "b" "latest"] @calls))
                  (.remove b)
                  (binding [ec/*execution-context* ctx]
                    (is (empty? (ec/get-state [:dom/cache]))))
                  (finally (finish)))))
             (catch :default error (is false (str error)) (finish)))))))))


(deftest island-root-identity-and-parent-slot-changes-have-explicit-lifetimes
  (let [ctx (context/create-execution-context) target (.createElement js/document "div")
        refs (atom []) ref! #(swap! refs conj (some? %))
        same-root (fn [label] (el/div {:ref ref!} label))
        different-root (fn [_] (el/section {:ref ref!} "Replacement root"))
        key {:from 10 :to 30 :view (str view-id)}
        first-render (saved-views/presenter ctx :owner [view-entry] #(same-root "First"))
        relocated (saved-views/presenter (assoc-in ctx [:bindings :dom/current-slot] 99)
                                        :owner [view-entry] #(same-root "Moved"))
        replacement (saved-views/presenter ctx :owner [view-entry] different-root)]
    (try
      (is (saved-views/paint! target first-render key "mount"))
      (let [root (.-firstChild target)]
        (is (saved-views/paint! target relocated key "mount"))
        (is (identical? root (.-firstChild target)))
        (is (= "Moved" (.-textContent target)))
        (is (= [true] @refs))
        (is (saved-views/paint! target replacement key "mount"))
        (is (not (identical? root (.-firstChild target))))
        (is (= "Replacement root" (.-textContent target)))
        (is (= [true false true] @refs)))
      (finally
        (saved-views/clear! target)
        (is (= [true false true false] @refs))
        (binding [ec/*execution-context* ctx]
          (is (empty? (ec/get-state [:dom/cache]))))
        (context/stop-context! ctx)))))
