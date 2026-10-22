(ns is.simm.uis.rich-editor-test
  (:require [cljs.test :refer-macros [deftest is]]
            [is.simm.documents.rich-text :as rich]
            [is.simm.uis.web.desktop.rich-editor]))

(defn- model [text] {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text text}]}]})
(defn- mounted [text f]
  (let [^js el (.createElement js/document "simmis-rich-editor")]
    (.setAttribute el "data-model" (js/JSON.stringify (clj->js (model text))))
    (.setAttribute el "data-read-only" "false")
    (.appendChild (.-body js/document) el)
    (try (f el (.-simmisRichEditor el)) (finally (.remove el)))))

(deftest markup-in-text-is-never-executed
  (mounted "<script>bad()</script>"
           (fn [^js el ^js editor]
             (is (nil? (.querySelector el "script")))
             (is (= "<script>bad()</script>" (.getText editor))))))

(deftest native-attribute-objects-cross-the-json-boundary-as-data
  (doseq [block [{:type "heading" :attrs {:level 2} :content [{:type "text" :text "Heading"}]}
                 {:type "orderedList" :attrs {:start 3}
                  :content [{:type "listItem" :content [{:type "paragraph" :content [{:type "text" :text "Item"}]}]}]}
                 {:type "paragraph" :content [{:type "text" :text "Link"
                                               :marks [{:type "link" :attrs {:href "doc/README.md"}}]}]}]]
    (let [doc-model {:type "doc" :content [block]}
          ^js el (.createElement js/document "simmis-rich-editor")]
      (.setAttribute el "data-model" (js/JSON.stringify (clj->js doc-model)))
      (.appendChild (.-body js/document) el)
      (try
        (let [^js native (.-simmisRichEditor el)
              decoded (@#'is.simm.uis.web.desktop.rich-editor/portable-model (.getJSON native))]
          (is (= (rich/normalize doc-model) (rich/normalize decoded)))
          (is (= (rich/markdown (rich/normalize doc-model)) (rich/markdown (rich/normalize decoded))))
          ;; A controlled echo must not fail normalization or replace the
          ;; native editor (which would also throw away its undo history).
          (.setAttribute el "data-model" (js/JSON.stringify (.getJSON native)))
          (is (identical? native (.-simmisRichEditor el)))
          (let [events (atom [])]
            (.addEventListener el "input"
                               #(when (= el (.-target %))
                                  (swap! events conj (js->clj (.. % -detail -model) :keywordize-keys true))))
            (.. native -commands (insertContent "x"))
            (is (seq @events))
            (is (= (rich/normalize (@#'is.simm.uis.web.desktop.rich-editor/portable-model (.getJSON native)))
                   (rich/normalize (last @events))))))
        (finally (.remove el))))))

(deftest enter-layout-survives-controlled-echo-typing-and-native-undo
  (mounted "First"
           (fn [^js el ^js editor]
             (let [decode #(@#'is.simm.uis.web.desktop.rich-editor/portable-model (.getJSON editor))
                   baseline (decode)]
               (.. editor -commands (setTextSelection 6))
               (is (true? (.. editor -commands (splitBlock))))
               (let [after-enter (decode) position (.. editor -state -selection -from)]
                 (is (= 2 (count (:content after-enter))))
                 (is (= "First\n\n" (rich/markdown after-enter)))
                 (is (= (rich/roundtrip-normalize baseline) (rich/roundtrip-normalize after-enter)))
                 (.setAttribute el "data-model" (js/JSON.stringify (.getJSON editor)))
                 (is (identical? editor (.-simmisRichEditor el)))
                 (is (= position (.. editor -state -selection -from))))
               (.. editor -commands (insertContent "Second"))
               (is (= "First\n\nSecond" (rich/markdown (decode))))
               (is (true? (.. editor -commands (undo))))
               (is (= (rich/normalize baseline) (rich/normalize (decode))))))))

(deftest each-element-owns-and-releases-its-native-interpreter
  (mounted "one"
           (fn [^js el ^js first]
             (mounted "two"
                      (fn [^js other ^js second]
                        (is (not (identical? first second)))
                        (.remove el)
                        (is (nil? (.-simmisRichEditor el)))
                        (is (identical? second (.-simmisRichEditor other)))
                        (.appendChild (.-body js/document) el)
                        (is (not (identical? first (.-simmisRichEditor el)))))))))

(deftest controlled-echo-does-not-reset-local-undo-but-authoritative-source-does
  (mounted "base"
           (fn [^js el ^js first]
             (let [events (atom [])]
               (.addEventListener el "input" #(when (= el (.-target %))
                                                (swap! events conj (js->clj (.. % -detail -model) :keywordize-keys true))))
               (.. first -commands (insertContent "local"))
               (is (seq @events))
               (.setAttribute el "data-model" (js/JSON.stringify (.getJSON first)))
               (is (identical? first (.-simmisRichEditor el)))
               (.setAttribute el "data-model" (js/JSON.stringify (clj->js (model "remote"))))
               (let [^js next (.-simmisRichEditor el)]
                 (is (not (identical? first next)))
                 (is (= "remote" (.getText next)))
                 (is (false? (.. next -commands (undo))))
                 (.setAttribute el "data-owner" "other-world")
                 (is (not (identical? next (.-simmisRichEditor el)))))))))

(deftest readonly-and-link-schemes-are-constrained
  (mounted "base"
           (fn [^js el ^js first]
             (.setAttribute el "data-read-only" "true")
             (is (false? (.-isEditable first)))
             (is (= "false" (.getAttribute (.querySelector el ".ProseMirror") "contenteditable")))))
  (doseq [uri ["javascript:alert(1)" "data:text/html,bad" "//external.test" "https://a.test/\u0000bad"]]
    (is (not (rich/safe-link? uri))))
  (is (rich/safe-link? "https://simm.is")))

(deftest selection-only-restoration-is-controlled-without-an-input-echo
  (mounted "abcdef"
           (fn [^js el ^js editor]
             (let [events (atom 0)]
               (.addEventListener el "input" #(when (= el (.-target %)) (swap! events inc)))
               (.setAttribute el "data-selection" "{\"from\":2,\"to\":4}")
               (is (= 2 (.. editor -state -selection -from)))
               (is (= 4 (.. editor -state -selection -to)))
               (is (zero? @events))
               (is (identical? editor (.-simmisRichEditor el)))))))
