(ns is.simm.uis.file-links-test
  (:require [cljs.test :refer-macros [deftest is]]
            [is.simm.documents.attachments :as attachments]
            [is.simm.uis.web.desktop.rich-editor]))

(deftest relative-file-mark-views-preserve-model-through-context-and-native-edits
  (let [id #uuid "0490e6e3-a857-48ae-a47f-d78fd328628e"
        base {:room (str id) :path "captures/session/observations/note.md" :enabled? true}
        href "../recordings/video.webm"
        model {:type "doc" :content [{:type "memoryRegion" :attrs {:key "region:0"}
                                      :content [{:type "paragraph" :content [{:type "text" :text "Recording"
                                                                            :marks [{:type "link" :attrs {:href href}}]}]}]}]}
        el (.createElement js/document "simmis-rich-editor")
        context! #(.setAttribute el "data-file-base" (js/JSON.stringify (clj->js %)))
        link-hrefs (fn [editor]
                     (let [out (atom [])]
                       (.descendants (.. editor -state -doc)
                                     (fn [node _]
                                       (doseq [mark (array-seq (.-marks node)) :when (= "link" (.. mark -type -name))]
                                         (swap! out conj (.. mark -attrs -href)))))
                       @out))]
    (.setAttribute el "data-model" (js/JSON.stringify (clj->js model)))
    (.setAttribute el "data-document" "true")
    (.setAttribute el "data-read-only" "false")
    (context! base)
    (.appendChild (.-body js/document) el)
    (try
      (let [editor (.-simmisRichEditor el) before (js/JSON.stringify (.getJSON editor))
            link (.querySelector el "a[data-memory-source-href]")
            expected (attachments/resolve-link base href)]
        (is (some? link))
        (is (= expected (.getAttribute link "href")))
        (is (= [href] (link-hrefs editor)))
        (is (.includes (.getHTML editor) href) "Export/clipboard serialization retains portable relative Markdown hrefs")
        (context! (assoc base :enabled? false))
        (is (nil? (.getAttribute link "href")))
        (context! base)
        (is (= expected (.getAttribute link "href")))
        (is (= before (js/JSON.stringify (.getJSON editor))))
        ;; A native DOM text edit forces ProseMirror to parse around the mark,
        ;; detecting accidental persistence of the display-only absolute href.
        (set! (.-nodeValue (.-firstChild link)) "Revised recording")
        (.flush (.. editor -view -domObserver))
        (is (= "Revised recording" (.. editor -state -doc -textContent)))
        (is (= [href] (link-hrefs editor))))
      (finally (.remove el)))))
