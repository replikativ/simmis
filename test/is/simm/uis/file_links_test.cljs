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

(deftest links-follow-edit-portably-and-retire-native-controls
  (let [room "0490e6e3-a857-48ae-a47f-d78fd328628e"
        base {:room room :path "wiki/start.md" :enabled? true}
        el (.createElement js/document "simmis-rich-editor")
        model {:type "doc" :content [{:type "memoryRegion" :attrs {:key "region:0"}
                                      :content [{:type "paragraph" :content [{:type "text" :text "Related page"
                                                                            :marks [{:type "link" :attrs {:href "other.md"}}]}]}]}]}
        followed (atom []) opened (atom []) original-open (.-open js/window)
        click! #(.dispatchEvent % (js/MouseEvent. "click" #js {:bubbles true :cancelable true}))
        popup #(.querySelector js/document ".memory-link-actions")]
    (.setAttribute el "data-model" (js/JSON.stringify (clj->js model)))
    (.setAttribute el "data-file-base" (js/JSON.stringify (clj->js base)))
    (.setAttribute el "data-document" "true")
    (.setAttribute el "data-read-only" "false")
    (aset el "onsimmis-follow-link" (fn [e] (.preventDefault e) (swap! followed conj (.-detail e))))
    (set! (.-open js/window) (fn [& args] (swap! opened conj args) nil))
    (.appendChild (.-body js/document) el)
    (try
      (let [editor (.-simmisRichEditor el)
            before (js/JSON.stringify (.getJSON editor))
            anchor (.querySelector el "a[data-memory-source-href]")
            expected (attachments/resolve-link base "other.md")]
        (.removeAllRanges (js/window.getSelection))
        (click! anchor)
        (is (= [expected] @followed) "A plain click follows without changing the document")
        (is (= before (js/JSON.stringify (.getJSON editor))))
        (let [shift-click (js/MouseEvent. "click" #js {:bubbles true :cancelable true :shiftKey true})
              range (.createRange js/document)]
          (is (false? (.dispatchEvent anchor shift-click)))
          (is (= [expected] @followed))
          (.selectNodeContents range anchor)
          (.addRange (js/window.getSelection) range)
          (let [down (js/PointerEvent. "pointerdown" #js {:bubbles true :cancelable true :button 0 :pointerId 1 :clientX 10 :clientY 10})]
            (is (false? (.dispatchEvent anchor down)) "Link presses do not focus/rebuild the editor")
            (.dispatchEvent anchor (js/PointerEvent. "pointerup" #js {:bubbles true :pointerId 1 :clientX 10 :clientY 10}))
            (click! anchor)
            (is (= [expected expected] @followed) "A prior selection does not block an intentional link click"))
          (.dispatchEvent anchor (js/PointerEvent. "pointerdown" #js {:bubbles true :cancelable true :button 0 :pointerId 2 :clientX 10 :clientY 10}))
          (.dispatchEvent anchor (js/PointerEvent. "pointermove" #js {:bubbles true :pointerId 2 :clientX 40 :clientY 10}))
          (.dispatchEvent anchor (js/PointerEvent. "pointerup" #js {:bubbles true :pointerId 2 :clientX 40 :clientY 10}))
          (is (false? (click! anchor)))
          (is (= [expected expected] @followed) "Dragging through linked text never navigates")
          (.removeAllRanges (js/window.getSelection)))
        (.dispatchEvent anchor (js/MouseEvent. "click" #js {:bubbles true :cancelable true :ctrlKey true}))
        (is (= 1 (count @opened)))
        (is (= (str (.-origin js/location) expected) (ffirst @opened)))
        ;; Move focus from the editor into the link, as Tab would.
        (.blur anchor)
        (.focus anchor)
        (.dispatchEvent anchor (js/FocusEvent. "focusin" #js {:bubbles true}))
        (is (some? (popup)))
        (.dispatchEvent anchor (js/KeyboardEvent. "keydown" #js {:key "ArrowDown" :bubbles true :cancelable true}))
        (is (= "Open" (.-textContent (.-activeElement js/document))) "Keyboard users can enter the actions")
        (.focus anchor)
        (.dispatchEvent anchor (js/KeyboardEvent. "keydown" #js {:key "Enter" :bubbles true :cancelable true}))
        (is (= [expected expected expected] @followed))
        (is (nil? (popup)))
        (.dispatchEvent anchor (js/MouseEvent. "pointerover" #js {:bubbles true}))
        (click! (.querySelector (popup) "button:nth-child(2)"))
        (let [input (.querySelector (popup) "input")]
          (is (= "other.md" (.-value input)) "Edit the source address, not its resolved display URL")
          (.dispatchEvent input (js/Event. "scroll"))
          (is (some? (popup)) "Horizontal scrolling in a long address keeps the form open")
          (.dispatchEvent js/window (js/Event. "scroll"))
          (is (some? (popup)) "Viewport scrolling repositions visible link actions")
          (set! (.-value input) "javascript:alert(1)")
          (click! (.querySelector (popup) "button"))
          (is (= before (js/JSON.stringify (.getJSON editor))) "Unsafe destinations never reach the model")
          (is (seq (.-textContent (.querySelector (popup) "[role=status]"))))
          (set! (.-value input) "../notes/revised.md")
          (click! (.querySelector (popup) "button")))
        (is (nil? (popup)))
        (let [new-link (.querySelector el "a[data-memory-source-href]")]
          (is (= "../notes/revised.md" (.getAttribute new-link "data-memory-source-href")))
          (is (= (attachments/resolve-link base "../notes/revised.md") (.getAttribute new-link "href")))
          (.dispatchEvent new-link (js/MouseEvent. "pointerover" #js {:bubbles true}))
          (is (some? (popup)))
          (.setAttribute el "data-file-base" (js/JSON.stringify (clj->js (assoc base :enabled? false))))
          (is (nil? (popup)) "Changing the target closes controls holding the old address")
          (is (nil? (.getAttribute new-link "href")))
          (.setAttribute el "data-file-base" (js/JSON.stringify (clj->js base)))
          (.dispatchEvent new-link (js/MouseEvent. "pointerover" #js {:bubbles true}))
          (.remove el)
          (is (nil? (popup)) "Unmount releases popovers and event listeners")))
      (finally (.remove el) (set! (.-open js/window) original-open)))))

(deftest malformed-http-links-are-inert-and-atomic-inline-content-keeps-its-range
  (let [el (.createElement js/document "simmis-rich-editor")
        link {:type "link" :attrs {:href "notes.md"}}
        model {:type "doc" :content [{:type "memoryRegion" :attrs {:key "region:0"}
                                      :content [{:type "paragraph"
                                                 :content [{:type "text" :text "Linked " :marks [link]}
                                                           {:type "footnoteReference" :attrs {:raw "[^long-reference]" :label "long-reference"} :marks [link]}
                                                           {:type "text" :text " tail" :marks [link]}
                                                           {:type "text" :text " untouched"}
                                                           {:type "text" :text " Invalid" :marks [{:type "link" :attrs {:href "https://["}}]}]}]}]}
        click! #(.dispatchEvent % (js/MouseEvent. "click" #js {:bubbles true :cancelable true}))]
    (.setAttribute el "data-model" (js/JSON.stringify (clj->js model)))
    (.appendChild (.-body js/document) el)
    (try
      (let [editor (.-simmisRichEditor el) anchor (.querySelector el "a[data-memory-source-href='notes.md']")
            invalid (.querySelector el "a[data-memory-source-href='https://[']")]
        (is (some? editor) "Malformed allowed-scheme URLs cannot break editor mounting")
        (is (nil? (.getAttribute invalid "href")))
        (.dispatchEvent anchor (js/MouseEvent. "pointerover" #js {:bubbles true}))
        (let [popup (.querySelector js/document ".memory-link-actions")]
          (click! (.querySelector popup "button:nth-child(2)"))
          (set! (.-value (.querySelector popup "input")) "revised.md")
          (click! (.querySelector popup "button")))
        (let [doc (js->clj (js/JSON.parse (js/JSON.stringify (.getJSON editor))) :keywordize-keys true)
              nodes (get-in doc [:content 0 :content 0 :content])]
          (is (= ["revised.md" "revised.md" "revised.md" nil "https://["]
                 (mapv #(get-in % [:marks 0 :attrs :href]) nodes)))
          (is (= " untouched" (:text (nth nodes 3))))
          (is (= "[^long-reference]" (get-in nodes [1 :attrs :raw])))))
      (finally (.remove el)))))
