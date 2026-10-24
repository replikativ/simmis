(ns is.simm.uis.reference-clipboard-test
  (:require [cljs.test :refer-macros [deftest is]]
            [is.simm.documents.links :as links]
            [is.simm.uis.web.desktop.rich-editor]))

(def target {:kind :memory-block :room "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"
             :repository "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb" :block "cccccccc-cccc-4ccc-8ccc-cccccccccccc"
             :path "wiki/Research.md"})
(def href (links/route target))
(defn- portable-model [^js editor]
  (js->clj (js/JSON.parse (js/JSON.stringify (.getJSON editor))) :keywordize-keys true))
(def model {:type "doc" :content [{:type "memoryRegion" :attrs {:key "region:0" :blockId nil}
                                  :content [{:type "paragraph" :content [{:type "text" :text "A useful label"}]}]}]})

(defn- with-editor [f]
  (let [^js el (.createElement js/document "simmis-rich-editor")]
    (.setAttribute el "data-model" (js/JSON.stringify (clj->js model)))
    (.setAttribute el "data-document" "true")
    (.setAttribute el "data-read-only" "false")
    (.appendChild (.-body js/document) el)
    (try (f el (.-simmisRichEditor el)) (finally (.remove el)))))

(defn- paste! [^js editor text html]
  (let [prevented (atom false)
        event #js {:clipboardData #js {:getData (fn [kind] (case kind "text/plain" text "text/html" html ""))}
                   :preventDefault #(reset! prevented true)}
        ^js view (.-view editor)]
    (.someProp view "handlePaste" (fn [handler] (handler view event nil)))
    @prevented))

(deftest canonical-url-paste-keeps-selected-label-and-is-one-undo-step
  (with-editor
    (fn [_ ^js editor]
      (let [original (js/JSON.stringify (.getJSON editor))]
        (.setTextSelection (.-commands editor) #js {:from 2 :to 16})
        (is (paste! editor (str (.-origin js/location) href) ""))
        (is (= "A useful label" (.. editor -state -doc -textContent)))
        (let [actual (portable-model editor)]
          (is (= href (get-in actual [:content 0 :content 0 :content 0 :marks 0 :attrs :href]))))
        (.undo (.-commands editor))
        (is (= original (js/JSON.stringify (.getJSON editor))))))))

(deftest copied-markdown-inserts-a-link-without-copying-block-identity
  (with-editor
    (fn [_ ^js editor]
      (.setTextSelection (.-commands editor) 2)
      (let [before (js/JSON.stringify (.getJSON editor))]
        (is (paste! editor (links/reference-markdown href) ""))
        (is (= href (get-in (portable-model editor)
                           [:content 0 :content 0 :content 0 :marks 0 :attrs :href])))
        (is (not (.includes (.getHTML editor) (str "data-block-id=\"" (:block target)))))
        (.undo (.-commands editor))
        (is (= before (js/JSON.stringify (.getJSON editor))))))))

(deftest foreign-html-and-code-pastes-keep-normal-editor-behavior
  (with-editor
    (fn [^js el ^js editor]
      (.setTextSelection (.-commands editor) 2)
      (let [before (js/JSON.stringify (.getJSON editor))]
        (doseq [[text html] [[(str "https://foreign.example" href) ""] [href "<b>HTML paste</b>"] ["Ordinary text" ""]]]
          (is (not (paste! editor text html))))
        (.setAttribute el "data-read-only" "true")
        (is (not (paste! editor href "")))
        (.setAttribute el "data-read-only" "false")
        (is (= before (js/JSON.stringify (.getJSON editor))))
        (.setCode (.-commands editor))
        (is (not (paste! editor href "")))))))

(deftest explicit-copy-reference-is-portable-and-copy-link-remains-shareable
  (with-editor
    (fn [^js el _]
      (let [block-model {:type "doc" :content [{:type "memoryBlock" :attrs {:key (str "block:" (:block target)) :blockId (:block target)}
                                               :content (:content model)}]}
            previous (.getOwnPropertyDescriptor js/Object js/navigator "clipboard") writes (atom [])]
        (try
          (.defineProperty js/Object js/navigator "clipboard"
                           #js {:configurable true :value #js {:writeText (fn [text] (swap! writes conj text) (js/Promise.resolve nil))}})
          (.setAttribute el "data-block-links" (js/JSON.stringify (clj->js {(:block target) href})))
          (.setAttribute el "data-model" (js/JSON.stringify (clj->js block-model)))
          (let [^js editor (.-simmisRichEditor el) before (js/JSON.stringify (.getJSON editor))
                ^js reference (.querySelector el "button[aria-label='Copy reference']")
                ^js link (.querySelector el "a[aria-label='Copy block link']")]
            (is (some? reference))
            (.click reference)
            (.click link)
            (is (= [(links/reference-markdown href) (str (.-origin js/location) href)] @writes))
            (is (= href (.getAttribute link "href")))
            (is (= before (js/JSON.stringify (.getJSON editor)))))
          (finally
            (if previous (.defineProperty js/Object js/navigator "clipboard" previous)
                (js-delete js/navigator "clipboard"))))))))
