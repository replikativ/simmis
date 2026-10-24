(ns is.simm.uis.list-indentation-test
  (:require [cljs.test :refer-macros [deftest is]]
            [clojure.string :as str]
            [is.simm.documents.rich-text :as rich]
            [is.simm.documents.rich-document :as document]
            [is.simm.uis.web.desktop.rich-editor]))

(defn- paragraph [text] {:type "paragraph" :content [{:type "text" :text text}]})
(defn- item [text & nested]
  {:type "listItem" :content (into [(paragraph text)] nested)})
(defn- bullets [& items] {:type "bulletList" :content (vec items)})

(defn- with-list [list-model f]
  (let [id "cccccccc-cccc-4ccc-8ccc-cccccccccccc"
        prefix (str "---\nsimmis-id: aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa\n---\n\n<!-- simmis:block " id " -->\n\n")
        suffix "\n\n<!-- /simmis:block -->\n\nUntouched outside text.\n"
        region {:type "doc" :content [list-model]} body (rich/markdown region)
        from (count prefix) to (+ from (count body)) key (str "region:" from)
        model {:type "doc" :content [{:type "memoryBlock" :attrs {:key (str "block:" id) :blockId id}
                                      :content [{:type "memoryRegion" :attrs {:key key :blockId id}
                                                 :content [list-model]}]}]}
        projection {:model model :regions {key {:from from :to to :model region
                                               :slots [{:from from :to to :model region}]}}}
        source (str prefix body suffix) ^js el (.createElement js/document "simmis-rich-editor")]
    (.setAttribute el "data-model" (js/JSON.stringify (clj->js model)))
    (.setAttribute el "data-document" "true") (.setAttribute el "data-read-only" "false")
    (.appendChild (.-body js/document) el)
    (try (f {:el el :editor (.-simmisRichEditor el) :source source :prefix prefix :suffix suffix :projection projection})
         (finally (.remove el)))))

(defn- select-item! [^js editor label]
  (let [position (volatile! nil)]
    (.descendants (.. editor -state -doc)
                  (fn [^js node pos]
                    (when (and (= "paragraph" (.. node -type -name)) (= label (.-textContent node)))
                      (vreset! position (inc pos)))))
    (assert @position)
    (.setTextSelection (.-commands editor) @position)
    (.focus (.-view editor))))

(defn- tab! [^js editor shift?]
  (let [event (js/KeyboardEvent. "keydown" #js {:key "Tab" :code "Tab" :keyCode 9
                                               :shiftKey shift? :bubbles true :cancelable true})]
    (.dispatchEvent (.. editor -view -dom) event)
    (.-defaultPrevented event)))

(defn- list-depth [^js editor]
  (let [^js position (.. editor -state -selection -$from)]
    (count (filter #(= "bulletList" (.. (.node position %) -type -name))
                   (range (inc (.-depth position)))))))

(defn- model [^js editor]
  ;; ProseMirror attribute maps have null prototypes; use the production
  ;; adapter's portable JSON boundary before comparing or serializing them.
  (js->clj (js/JSON.parse (js/JSON.stringify (.getJSON editor))) :keywordize-keys true))

(deftest native-tab-can-reach-third-and-fourth-list-levels-with-one-step-undo
  (doseq [[initial label before-depth after-depth indentation]
          [[(bullets (item "A" (bullets (item "B") (item "C")))) "C" 2 3 "    "]
           [(bullets (item "A" (bullets (item "B" (bullets (item "C") (item "D")))))) "D" 3 4 "      "]]]
    (with-list initial
      (fn [{:keys [^js editor source prefix suffix projection]}]
        (select-item! editor label)
        (let [original (model editor)]
          (is (= before-depth (list-depth editor)))
          (is (tab! editor false) "The actual native Tab keymap handles a sink with a preceding sibling")
          (is (= after-depth (list-depth editor)))
          (let [nested (model editor) patched (document/apply-model source projection nested)]
            (is (= (document/topology original) (document/topology nested)))
            (is (str/starts-with? patched prefix))
            (is (str/ends-with? patched suffix))
            (is (re-find (re-pattern (str "(?m)^" indentation "[-+*] " label "(?:\\r?$)")) patched))
            (.undo (.-commands editor))
            (is (= (document/normalized original) (document/normalized (model editor))))
            (is (= source (document/apply-model source projection (model editor))))))))))

(deftest native-shift-tab-lifts-one-level-and-does-not-cross-semantic-owners
  (with-list (bullets (item "A" (bullets (item "B" (bullets (item "C"))))))
    (fn [{:keys [^js editor source prefix suffix projection]}]
      (select-item! editor "C")
      (let [before (model editor)]
        (is (= 3 (list-depth editor)))
        (is (tab! editor true))
        (is (= 2 (list-depth editor)))
        (let [patched (document/apply-model source projection (model editor))]
          (is (str/starts-with? patched prefix))
          (is (str/ends-with? patched suffix))
          (is (re-find #"(?m)^  [-+*] C(?:\r?$)" patched)))
        (.undo (.-commands editor))
        (is (= (document/normalized before) (document/normalized (model editor))))
        (select-item! editor "A")
        (let [first-item (model editor)]
          (is (not (tab! editor false)) "A first item has no preceding sibling to become its parent")
          (is (= first-item (model editor))))))))
