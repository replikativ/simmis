(ns is.simm.uis.rich-editor-test
  (:require [cljs.test :refer-macros [deftest is]]
            [is.simm.documents.rich-text :as rich]
            [is.simm.uis.web.desktop.rich-editor]))

(defn- model [text] {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text text}]}]})
(defn- mounted [text f]
  (let [el (.createElement js/document "simmis-rich-editor")]
    (.setAttribute el "data-model" (js/JSON.stringify (clj->js (model text))))
    (.setAttribute el "data-read-only" "false")
    (.appendChild (.-body js/document) el)
    (try (f el (.-simmisRichEditor el)) (finally (.remove el)))))

(deftest markup-in-text-is-never-executed
  (mounted "<script>bad()</script>"
           (fn [el editor]
             (is (nil? (.querySelector el "script")))
             (is (= "<script>bad()</script>" (.getText editor))))))

(deftest each-element-owns-and-releases-its-native-interpreter
  (mounted "one"
           (fn [el first]
             (mounted "two"
                      (fn [other second]
                        (is (not (identical? first second)))
                        (.remove el)
                        (is (nil? (.-simmisRichEditor el)))
                        (is (identical? second (.-simmisRichEditor other)))
                        (.appendChild (.-body js/document) el)
                        (is (not (identical? first (.-simmisRichEditor el)))))))))

(deftest controlled-echo-does-not-reset-local-undo-but-authoritative-source-does
  (mounted "base"
           (fn [el first]
             (let [events (atom [])]
               (.addEventListener el "input" #(when (= el (.-target %))
                                                (swap! events conj (js->clj (.. % -detail -model) :keywordize-keys true))))
               (.. first -commands (insertContent "local"))
               (is (seq @events))
               (.setAttribute el "data-model" (js/JSON.stringify (.getJSON first)))
               (is (identical? first (.-simmisRichEditor el)))
               (.setAttribute el "data-model" (js/JSON.stringify (clj->js (model "remote"))))
               (let [next (.-simmisRichEditor el)]
                 (is (not (identical? first next)))
                 (is (= "remote" (.getText next)))
                 (is (false? (.. next -commands (undo))))
                 (.setAttribute el "data-owner" "other-world")
                 (is (not (identical? next (.-simmisRichEditor el)))))))))

(deftest readonly-and-link-schemes-are-constrained
  (mounted "base"
           (fn [el first]
             (.setAttribute el "data-read-only" "true")
             (is (false? (.-isEditable first)))
             (is (= "false" (.getAttribute (.querySelector el ".ProseMirror") "contenteditable")))))
  (doseq [uri ["javascript:alert(1)" "data:text/html,bad" "//external.test" "https://a.test/\u0000bad"]]
    (is (not (rich/safe-link? uri))))
  (is (rich/safe-link? "https://simm.is")))

(deftest selection-only-restoration-is-controlled-without-an-input-echo
  (mounted "abcdef"
           (fn [el editor]
             (let [events (atom 0)]
               (.addEventListener el "input" #(when (= el (.-target %)) (swap! events inc)))
               (.setAttribute el "data-selection" "{\"from\":2,\"to\":4}")
               (is (= 2 (.. editor -state -selection -from)))
               (is (= 4 (.. editor -state -selection -to)))
               (is (zero? @events))
               (is (identical? editor (.-simmisRichEditor el)))))))
