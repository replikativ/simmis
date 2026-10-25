(ns is.simm.uis.source-preview-test
  (:require [cljs.test :refer-macros [deftest is]]
            [is.simm.documents.markdown-scene :as scene]
            [is.simm.documents.links :as links]
            [is.simm.uis.web.desktop.rich-editor]
            [is.simm.uis.web.desktop.source-preview :as preview]))

(def source "| Name | Value |\n|---|---|\n| Example | <script>literal</script> |\n")
(defn projection []
  (scene/preview source
                 {:kind :TableBlock :from 0 :to (count source)
                  :children [{:kind :TableHead :children [{:kind :TableRow
                               :children [{:kind :TableCell :table-header? true
                                           :children [{:kind :Text :text "Name"}]}
                                          {:kind :TableCell :table-header? true
                                           :children [{:kind :Text :text "Value"}]}]}]}
                             {:kind :TableBody :children [{:kind :TableRow
                               :children [{:kind :TableCell :children [{:kind :Text :text "Example"}]}
                                          {:kind :TableCell :children [{:kind :Text :text "<script>literal</script>"}]}]}]}]}))

(deftest read-and-rich-table-previews-share-inert-dom-and-preserve-source-and-export
  (let [read-el (.createElement js/document "simmis-markdown-preview")
        edit-el (.createElement js/document "simmis-rich-editor")
        model {:type "doc" :content [{:type "memoryOpaque" :attrs {:key "opaque:0" :label source}}
                                     {:type "memoryRegion" :attrs {:key "region:end"}
                                      :content [{:type "paragraph" :content [{:type "text" :text "After"}]}]}]}
        projection (projection) inputs (atom 0)]
    (.setAttribute read-el "data-preview" (js/JSON.stringify (clj->js projection)))
    (.setAttribute edit-el "data-model" (js/JSON.stringify (clj->js model)))
    (.setAttribute edit-el "data-source-previews" (js/JSON.stringify (clj->js {"opaque:0" projection})))
    (.setAttribute edit-el "data-document" "true")
    (.setAttribute edit-el "data-read-only" "false")
    (.addEventListener edit-el "input" #(swap! inputs inc))
    (.append (.-body js/document) read-el edit-el)
    (try
      (let [editor (.-simmisRichEditor edit-el) before (js/JSON.stringify (.getJSON editor))
            mounted-inputs @inputs]
        (is (some? (.querySelector read-el "table")))
        (is (= (.-textContent (.querySelector read-el "table"))
               (.-textContent (.querySelector edit-el "table"))))
        (is (zero? (.-length (.querySelectorAll edit-el "script,img,iframe"))))
        (is (.includes (.-textContent (.querySelector edit-el "td:last-child")) "<script>literal</script>"))
        (is (not (.includes (.getHTML editor) "<table>")) "Display is outside the source model/clipboard HTML")
        (.setAttribute edit-el "data-source-previews"
                       (js/JSON.stringify (clj->js {"opaque:0" (assoc projection :source "different source")})))
        (is (nil? (.querySelector edit-el "table")) "Never display a projection for another source")
        (is (.includes (.-textContent edit-el) source))
        (.setAttribute edit-el "data-source-previews" (js/JSON.stringify (clj->js {"opaque:0" projection})))
        (is (some? (.querySelector edit-el "table")))
        (is (= before (js/JSON.stringify (.getJSON editor))))
        (is (= mounted-inputs @inputs) "Refreshing display sends no editing/evaluation events")
        (let [table (.querySelector edit-el "table") position (volatile! nil)]
          (.descendants (.. editor -state -doc)
                        (fn [node p] (when (= "After" (.-text node)) (vreset! position (+ p 5)))))
          (.dispatch (.-view editor) (.insertText (.. editor -state -tr) "!" @position))
          (is (identical? table (.querySelector edit-el "table")) "Nearby typing keeps the table mounted")
          (.undo (.-commands editor))
          (is (= before (js/JSON.stringify (.getJSON editor))))
          (is (identical? table (.querySelector edit-el "table")))))
      (finally
        (.remove read-el) (.remove edit-el)
        (is (zero? (.-size (.-simmisSourcePreviewViews edit-el))) "Detach releases every native paint callback")))))

(deftest image-display-uses-only-owned-routes-and-keeps-the-editor-source-model
  (let [id #uuid "0490e6e3-a857-48ae-a47f-d78fd328628e"
        text "![Frame](images/frame.png)\n" el (.createElement js/document "simmis-rich-editor")
        model {:type "doc" :content [{:type "memoryOpaque" :attrs {:key "opaque:0" :label text}}]}
        url #(links/media-route {:room (str id) :path "images/frame.png" :content % :target id})
        projection (fn [src] {:source text :scene {:tag :p :attrs {} :children [{:tag :img :attrs {:src src :alt "Frame" :loading "lazy"} :children []}]}})
        display! (fn [src] (.setAttribute el "data-source-previews"
                                         (js/JSON.stringify (clj->js {"opaque:0" (projection src)}))))]
    (.setAttribute el "data-model" (js/JSON.stringify (clj->js model)))
    (.setAttribute el "data-document" "true")
    (.setAttribute el "data-read-only" "false")
    (display! (url id))
    (.appendChild (.-body js/document) el)
    (try
      (let [editor (.-simmisRichEditor el) before (js/JSON.stringify (.getJSON editor))]
        (is (= (url id) (.getAttribute (.querySelector el "img") "src")))
        (let [replacement (random-uuid)]
          (display! (url replacement))
          (is (= (url replacement) (.getAttribute (.querySelector el "img") "src"))))
        (doseq [bad ["https://external.invalid/tracker.png" "//external.invalid/tracker.png" "data:image/png;base64,AA"]]
          (display! bad)
          (is (nil? (.getAttribute (.querySelector el "img") "src"))))
        (is (= before (js/JSON.stringify (.getJSON editor))))
        (.setAttribute el "data-source-previews" "{}")
        (is (nil? (.querySelector el "img")))
        (is (.includes (.-textContent el) text)))
      (finally (.remove el)))))
