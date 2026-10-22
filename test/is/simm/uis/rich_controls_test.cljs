(ns is.simm.uis.rich-controls-test
  (:require [cljs.test :refer-macros [deftest is]]
            [is.simm.uis.web.desktop.rich-controls :as controls]
            [is.simm.uis.web.desktop.rich-editor]
            [is.simm.documents.rich-document :as document]
            [is.simm.uis.document-editing-probe-test :as probe]))

(defn- mounted
  ([f] (mounted probe/protected-specimen f))
  ([model f]
  (let [^js container (.createElement js/document "div")]
    (set! (.-className container) "memory-document")
    (set! (.. container -style -cssText) "position:fixed;left:200px;top:200px;width:500px;z-index:100000")
    (set! (.-innerHTML container) "<div class='memory-document-toolbar' role='toolbar'><button aria-label='Bold'>B</button><button aria-label='Italic'>I</button></div>")
    (.appendChild (.-body js/document) container)
    (let [^js el (.createElement js/document "simmis-rich-editor")]
      (.setAttribute el "data-model" (js/JSON.stringify (clj->js model)))
      (.setAttribute el "data-document" "true")
      (.appendChild container el)
      (try (f container el (.-simmisRichEditor el))
           (finally (.remove container)))))))

(deftest command-keeps-selection-when-keyboard-focus-is-in-the-toolbar
  (mounted
   (fn [^js container ^js el ^js editor]
     (let [^js button (.querySelector container "button")]
       (.. editor -commands (setTextSelection #js {:from 3 :to 8}))
       (.focus button)
       (controls/command! #js {:target button} :bold)
       (is (.isActive editor "bold"))
       (is (= 3 (.. editor -state -selection -from)))
       (is (= 8 (.. editor -state -selection -to)))
       (is (true? (get-in (controls/format-state el editor) [:bold :active?])))
       (is (identical? (.-dom (.-view editor)) (.-activeElement js/document)))))))

(deftest format-availability-respects-ownership-and-read-only
  (mounted
   (fn [_ ^js el ^js editor]
     (.. editor -commands (selectAll))
     (is (false? (get-in (controls/format-state el editor) [:bullet-list :enabled?])))
     (is (false? (get-in (controls/format-state el editor) [:heading :enabled?])))
     (.setAttribute el "data-read-only" "true")
     (is (every? false? (map :enabled? (vals (controls/format-state el editor))))))))

(deftest restored-selections-are-scope-bound-and-resolve-to-valid-text
  (mounted
   (fn [_ ^js el ^js editor]
     (.setAttribute el "data-owner" "current")
     (let [^js current (.-simmisRichEditor el)]
       (.setAttribute el "data-selection" (js/JSON.stringify #js {:owner "previous" :selection #js {:from 0 :to 0}}))
       (.setAttribute el "data-owner" "replacement")
       (let [^js replacement (.-simmisRichEditor el)]
         (is (some? (controls/region replacement)))
         (.setAttribute el "data-selection" (js/JSON.stringify #js {:owner "replacement" :selection #js {:from 0 :to 0}}))
         (is (true? (.. replacement -state -selection -$head -parent -inlineContent)))
         (.. replacement -commands (selectAll))
         (.setAttribute el "data-selection"
                        (js/JSON.stringify #js {:owner "replacement" :selection #js {:native (.toJSON (.. replacement -state -selection))}}))
         (is (= "all" (aget (.toJSON (.. replacement -state -selection)) "type"))))))))

(deftest keyboard-navigation-and-element-owned-observers-are-released
  (mounted
   (fn [^js container ^js el ^js editor]
     (let [^js toolbar (.querySelector container "[role=toolbar]")
           buttons (.querySelectorAll toolbar "button")
           prevented (atom false)]
       (.focus (.-view editor))
       (controls/focus-toolbar! el)
       (is (identical? (aget buttons 0) (.-activeElement js/document)))
       (controls/toolbar-keydown! #js {:currentTarget toolbar :target (aget buttons 0)
                                       :key "ArrowRight" :preventDefault #(reset! prevented true)})
       (is @prevented)
       (is (identical? (aget buttons 1) (.-activeElement js/document)))
       (is (fn? (.-simmisReleaseControls el)))
       (.remove el)
       (is (nil? (.-simmisScheduleControls el)))
       (is (nil? (.-simmisPositionControls el)))
       (is (nil? (.-simmisToolbarFrame el)))
       (is (nil? (.-simmisReleaseControls el)))))))

(deftest enabled-mixed-content-commands-pass-the-final-boundary-guard
  (let [p probe/paragraph
        item {:type "listItem" :content [(p "Item")]}
        nested {:type "bulletList" :content [{:type "listItem" :content [(p "Parent")
                                               {:type "bulletList" :content [item]}]}]}]
    (doseq [content [[(p "Alpha") {:type "bulletList" :content [item]}]
                     [(p "Alpha") {:type "codeBlock" :attrs {:language "clojure"}
                                   :content [{:type "text" :text "(+ 1 2)"}]}]
                     [nested]]
            op (map :op (mapcat identity controls/groups))]
      (let [model (assoc-in probe/protected-specimen [:content 0 :content 0 :content] content)]
        (mounted model
                 (fn [^js container ^js el ^js editor]
                   (let [positions (atom []) blocked (atom 0)]
                     (.descendants (.child (.child (.. editor -state -doc) 0) 0)
                                   (fn [^js node pos] (when (.-isText node) (swap! positions conj [pos (.-nodeSize node)]))))
                     (let [[last-pos last-size] (last @positions)]
                       (.. editor -commands (setTextSelection #js {:from (+ 2 (ffirst @positions))
                                                                   :to (+ 2 last-pos last-size)})))
                     (.addEventListener el "input" #(when (some-> % .-detail (aget "blocked")) (swap! blocked inc)))
                     (when (get-in (controls/format-state el editor) [op :enabled?])
                       (controls/command! #js {:target (.querySelector container "button")} op)
                       (is (zero? @blocked) (str "Enabled command must be accepted: " op))
                       (let [actual (js->clj (js/JSON.parse (js/JSON.stringify (.getJSON editor))) :keywordize-keys true)]
                         (is (= (document/topology model) (document/topology actual)))
                         (is (= (document/normalized {:type "doc" :content [(second (:content model))]})
                                (document/normalized {:type "doc" :content [(second (:content actual))]}))))))))))))
