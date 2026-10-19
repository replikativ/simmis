(ns is.simm.uis.source-editor-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            ["@codemirror/commands" :refer [undo]]
            [is.simm.uis.web.desktop.source-editor :as editor]))

(defn- mounted [source f]
  (let [el (.createElement js/document "simmis-source-editor")]
    (.setAttribute el "data-source" source)
    (.setAttribute el "data-language" "markdown")
    (.setAttribute el "data-read-only" "false")
    (.setAttribute el "data-selection" "{\"anchor\":0,\"head\":0}")
    (.appendChild (.-body js/document) el)
    (try (f el (.-simmisEditorView el))
         (finally (.remove el)))))

(deftest languages-are-selected-by-file-type-not-executed
  (is (= "markdown" (editor/language "README.MD")))
  (is (= "clojure" (editor/language "checker.clj")))
  (is (= "clojure" (editor/language "workflow.edn")))
  (is (= "json" (editor/language "result.json")))
  (is (= "text" (editor/language "unknown.ext"))))

(deftest source-preserves-newlines-and-opaque-content
  (let [source "\uFEFF---\r\ncustom: yes\n---\r\n# 🦋\n<script>bad()</script>"]
    (mounted source
             (fn [el view]
               (is (= source (.toString (.. view -state -doc))))
               (is (nil? (.querySelector el "script")))
               (.setAttribute el "data-source" (str source "\r\n"))
               (is (= (str source "\r\n") (.toString (.. view -state -doc))))))))

(deftest changes-emit-serializable-draft-data-and-controlled-updates-do-not-echo
  (mounted "before"
           (fn [el view]
             (let [events (atom [])]
               (.addEventListener el "input"
                                  (fn [e] (when (= el (.-target e))
                                            (swap! events conj (js->clj (.-detail e) :keywordize-keys true)))))
               (.dispatch view #js {:changes #js {:from 0 :to 6 :insert "after"}
                                    :selection #js {:anchor 5}})
               (is (= [{:source "after" :selection {:anchor 5 :head 5}}] @events))
               (.setAttribute el "data-source" "remote")
               (.setAttribute el "data-selection" "{\"anchor\":2,\"head\":4}")
               (is (= 1 (count @events)))
               (is (= "remote" (.toString (.. view -state -doc))))
               (is (= 2 (.. view -state -selection -main -anchor)))
               (is (= 4 (.. view -state -selection -main -head)))))))

(deftest native-resources-are-per-element-and-released-on-detach
  (mounted "one"
           (fn [^js el first-view]
             (mounted "two"
                      (fn [^js other second-view]
                        (is (not (identical? first-view second-view)))
                        (.remove el)
                        (is (nil? (.-simmisEditorView el)))
                        (is (identical? second-view (.-simmisEditorView other)))
                        (.appendChild (.-body js/document) el)
                        (is (some? (.-simmisEditorView el)))
                        (is (not (identical? first-view (.-simmisEditorView el))))
                        (is (= "one" (.toString (.. el -simmisEditorView -state -doc)))))))))

(deftest permissions-reconfigure-the-existing-view
  (mounted "text"
           (fn [^js el view]
             (.setAttribute el "data-read-only" "true")
             (is (true? (.. view -state -readOnly)))
             (is (= "false" (.getAttribute (.-contentDOM view) "contenteditable")))
             (.setAttribute el "data-read-only" "false")
             (is (false? (.. view -state -readOnly)))
             (is (identical? view (.-simmisEditorView el))))))

(deftest repl-submit-key-does-not-run-the-native-insert-blank-line-command
  (mounted "(+ 1 2)"
           (fn [el view]
             (let [evaluations (atom 0) changes (atom 0)]
               (.setAttribute el "data-evaluate" "true")
               (.addEventListener el "input" (fn [e]
                                               (when (= el (.-target e))
                                                 (if (.. e -detail -evaluate) (swap! evaluations inc) (swap! changes inc)))))
               (.dispatchEvent (.-contentDOM view)
                               (js/KeyboardEvent. "keydown" #js {:key "Enter" :code "Enter" :ctrlKey true :bubbles true :cancelable true}))
               (is (= 1 @evaluations))
               (is (zero? @changes))
               (is (= "(+ 1 2)" (.toString (.. view -state -doc))))))))

(deftest authoritative-replacements-and-owner-changes-do-not-inherit-local-undo
  (mounted "base"
           (fn [^js el ^js view]
             (.dispatch view #js {:changes #js {:from 0 :to 4 :insert "local"}})
             (.setAttribute el "data-source" "authoritative")
             (is (= "authoritative" (.toString (.. view -state -doc))))
             (is (false? (undo view)))
             (.dispatch view #js {:changes #js {:from 0 :to 13 :insert "other"}})
             (.setAttribute el "data-source" "other")
             (.setAttribute el "data-owner" "different-execution-context")
             (is (not (identical? view (.-simmisEditorView el))))
             (is (= "other" (.toString (.. el -simmisEditorView -state -doc))))
             (is (false? (undo (.-simmisEditorView el)))))))
