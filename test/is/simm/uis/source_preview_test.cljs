(ns is.simm.uis.source-preview-test
  (:require [cljs.test :refer-macros [deftest is]]
            [is.simm.documents.markdown-scene :as scene]
            [is.simm.uis.web.desktop.rich-editor]
            [is.simm.uis.web.desktop.source-preview]))

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
