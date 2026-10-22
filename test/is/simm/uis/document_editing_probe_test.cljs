(ns is.simm.uis.document-editing-probe-test
  "Isolated interaction experiment, not a file writer or an application mode.
   show! mounts disposable editors without reading or changing room drafts."
  (:require [cljs.test :refer-macros [deftest is]]
            [clojure.string :as str]
            [is.simm.documents.rich-text :as rich]
            [is.simm.documents.rich-document :as document]
            [is.simm.uis.web.desktop.rich-editor]))

(defn paragraph [s]
  {:type "paragraph" :content [{:type "text" :text s}]})

(def specimen
  {:type "doc"
   :content [{:type "heading" :attrs {:level 2}
              :content [{:type "text" :text "Product discovery"}]}
             (paragraph "First paragraph: investigate the customer's problem before proposing a solution. This line wraps in the narrow experiment pane.")
             (paragraph "Second paragraph: retain evidence and distinguish observations from hypotheses.")
             {:type "bulletList"
              :content [{:type "listItem" :content [(paragraph "Interview founders")
                                                    {:type "bulletList" :content [{:type "listItem" :content [(paragraph "Record counterexamples")]}]}]}
                        {:type "listItem" :content [(paragraph "Review the findings")]}]}
             {:type "codeBlock" :attrs {:language "clojure"}
              :content [{:type "text" :text "(map :evidence observations)"}]}]})

(defn- native-model [^js editor]
  (js->clj (js/JSON.parse (js/JSON.stringify (.getJSON editor))) :keywordize-keys true))

(defn- mount! [parent model]
  (let [^js el (.createElement js/document "simmis-rich-editor")]
    (.setAttribute el "data-model" (js/JSON.stringify (clj->js model)))
    (.setAttribute el "data-read-only" "false")
    (.appendChild parent el)
    el))

(defn- with-editor [model f]
  (let [^js el (mount! (.-body js/document) model)]
    (try (f el (.-simmisRichEditor el)) (finally (.remove el)))))

(deftest a-single-native-document-selects-and-replaces-across-paragraphs
  (with-editor {:type "doc" :content [(paragraph "Alpha") (paragraph "Beta")]}
    (fn [_ ^js editor]
      ;; Alpha occupies positions 1..6, Beta 8..12. Selection crosses the
      ;; paragraph boundary rather than transferring focus to another editor.
      (.. editor -commands (setTextSelection #js {:from 3 :to 10}))
      (is (= 3 (.. editor -state -selection -from)))
      (is (= 10 (.. editor -state -selection -to)))
      (.. editor -commands (insertContent "X"))
      (is (= "AlXta" (.getText editor)))
      (is (= 1 (count (:content (native-model editor)))))
      (is (true? (.. editor -commands (undo))))
      (is (= "Alpha\n\nBeta" (.getText editor)))
      (is (true? (.. editor -commands (redo))))
      (is (= "AlXta" (.getText editor))))))

(deftest split-and-join-are-native-document-operations
  (with-editor {:type "doc" :content [(paragraph "Alpha")]}
    (fn [_ ^js editor]
      (.. editor -commands (setTextSelection 3))
      (is (true? (.. editor -commands (splitBlock))))
      (is (= ["Al" "pha"] (mapv #(apply str (map :text (:content %))) (:content (native-model editor)))))
      (is (true? (.. editor -commands (joinBackward))))
      (is (= "Alpha" (.getText editor)))
      (is (= 1 (count (:content (native-model editor))))))))

(deftest structural-list-editing-and-code-remain-available
  (with-editor specimen
    (fn [_ ^js editor]
      (let [model (native-model editor)]
        (is (= (rich/normalize specimen) (rich/normalize model)))
        (is (str/includes? (rich/markdown model) "  - Record counterexamples"))
        (is (str/includes? (rich/markdown model) "```clojure\n(map :evidence observations)\n```"))))))

(def protected-specimen
  {:type "doc"
   :content [{:type "memoryBlock" :attrs {:key "block:one" :blockId "one"}
              :content [{:type "memoryRegion" :attrs {:key "region:one" :blockId "one"}
                         :content [(paragraph "Alpha") (paragraph "Beta")]}]}
             {:type "memoryBlock" :attrs {:key "block:two" :blockId "two"}
              :content [{:type "memoryRegion" :attrs {:key "region:two" :blockId "two"}
                         :content [(paragraph "Gamma")]}]}
             {:type "memoryOpaque" :attrs {:key "opaque:one" :label "[[Retained source]]"}}]})

(deftest continuous-containers-keep-ids-and-undo-through-a-controlled-echo
  (with-editor protected-specimen
    (fn [^js el ^js editor]
      (.setAttribute el "data-document" "true")
      (is (= (document/normalized protected-specimen) (document/normalized (native-model editor))))
      (.. editor -commands (setTextSelection 5))
      (is (true? (.. editor -commands (splitBlock))))
      (is (= (document/topology protected-specimen) (document/topology (native-model editor))))
      (.setAttribute el "data-model" (js/JSON.stringify (.getJSON editor)))
      (is (identical? editor (.-simmisRichEditor el)))
      (is (true? (.. editor -commands (undo))))
      (is (= (document/normalized protected-specimen) (document/normalized (native-model editor)))))))

(deftest copying-selection-can-cross-identities-but-destructive-transactions-cannot
  (with-editor protected-specimen
    (fn [^js el ^js editor]
      (.setAttribute el "data-document" "true")
      (let [blocked (atom 0) ranges (atom [])]
        (.addEventListener el "input" (fn [event] (when-let [detail (.-detail event)]
                                                   (when (aget detail "blocked") (swap! blocked inc)))))
        (.descendants (.. editor -state -doc)
                      (fn [^js n pos]
                        (when (= "paragraph" (.. n -type -name))
                          (swap! ranges conj {:from (inc pos) :to (+ pos (.-nodeSize n) -1)}))))
        (.. editor -commands (setTextSelection #js {:from (:from (first @ranges)) :to (:to (last @ranges))}))
        (is (str/includes? (.textBetween (.. editor -state -doc)
                                        (.. editor -state -selection -from) (.. editor -state -selection -to) "\n") "Gamma"))
        (.. editor -commands (deleteSelection))
        (is (pos? @blocked))
        (is (= (document/normalized protected-specimen) (document/normalized (native-model editor))))))))

(deftest opaque-source-is-copied-as-inert-text-not-dropped
  (with-editor protected-specimen
    (fn [_ ^js editor]
      (.. editor -commands (selectAll))
      (let [serialize (.someProp (.-view editor) "clipboardTextSerializer")
            text (serialize)]
        (is (str/includes? text "Alpha"))
        (is (str/includes? text "Gamma"))
        (is (str/includes? text "[[Retained source]]"))
        (is (not (str/includes? text "block:one")))))))

(defn positional-projection
  "Deliberately limited experiment: reuses source-local region replacement
   for text edits only. Structural operations require a proper source map.
   Not called by any application/save path. Equal arity alone would be unsafe
   for reorders or replacements, so callers must not use this as a writer."
  [source slots model]
  (if (not= (count slots) (count (:content model)))
    {:status :needs-structural-source-map}
    {:status :text-projection-only
     :text (reduce (fn [text [slot node]]
                     (rich/apply-region text slot {:type "doc" :content [node]}))
                   source (reverse (map vector slots (:content model))))}))

(deftest source-local-text-projection-is-lossless-but-structural-mapping-is-not-solved
  (let [source "---\r\ncustom: retained\r\n---\r\n\r\nAlpha\r\n\r\nBeta\r\n\r\n[[Opaque]]\r\n"
        start (str/index-of source "Alpha") second (str/index-of source "Beta")
        model {:type "doc" :content [(paragraph "Alpha") (paragraph "Beta")]}
        slots [{:from start :to (+ start 7) :model {:type "doc" :content [(paragraph "Alpha")]}}
               {:from second :to (+ second 6) :model {:type "doc" :content [(paragraph "Beta")]}}]]
    (is (= source (:text (positional-projection source slots model))))
    (is (= (str/replace source "Alpha" "Changed")
           (:text (positional-projection source slots (assoc-in model [:content 0 :content 0 :text] "Changed")))))
    (is (= :needs-structural-source-map
           (:status (positional-projection source slots {:type "doc" :content [(paragraph "AlphaBeta")]}))))))

(defn show!
  "Disposable side-by-side interaction lab. Native state belongs to these
   DOM elements only; there is no persistence, RPC or application draft access."
  []
  (when-let [old (.getElementById js/document "simmis-document-editing-probe")] (.remove old))
  (let [^js host (.createElement js/document "section")
        ^js shadow (.attachShadow host #js {:mode "open"})]
    (set! (.-id host) "simmis-document-editing-probe")
    (set! (.. host -style -cssText) "position:fixed;inset:36px 24px;z-index:100000;background:#191a1d;color:#ddd;border:1px solid #555;border-radius:10px;overflow:auto;box-shadow:0 10px 50px #0008;")
    ;; Constant markup only, never user-supplied text or file contents.
    (set! (.-innerHTML shadow)
          "<style>:host{font:14px system-ui}header{padding:16px;display:flex;justify-content:space-between;gap:16px}p{line-height:1.65}main{display:grid;grid-template-columns:1fr 1fr;gap:28px;padding:0 24px 24px}article{min-width:0}h2{font-size:17px}button{color:#ddd;background:#303136;border:1px solid #666;border-radius:5px;padding:6px 12px}.ProseMirror{outline:none;min-height:24px}.ProseMirror:focus{box-shadow:inset 2px 0 #8981bc}simmis-rich-editor{display:block;padding:4px 10px}pre{background:#24252a;padding:12px;white-space:pre-wrap}small{color:#aaa}hr{border:0;border-top:1px solid #444}</style><header><div><strong>Document interaction lab — disposable sample, never saved</strong><p>Try arrows, Shift+arrows, drag selection across paragraphs, Enter, Backspace, undo/redo, and nested lists.</p></div><button id='close'>Close</button></header><main><article><h2>Current: independent regions</h2><small>Separate caret, selection and undo per region.</small><div id='regions'></div></article><article><h2>Experiment: continuous document</h2><small>One native selection and undo history. Source mapping is not complete.</small><div id='continuous'></div></article></main>")
    (.addEventListener (.getElementById shadow "close") "click" #(.remove host))
    (.appendChild (.-body js/document) host)
    (doseq [node (:content specimen)]
      (mount! (.getElementById shadow "regions") {:type "doc" :content [node]}))
    (mount! (.getElementById shadow "continuous") specimen)
    :shown))
