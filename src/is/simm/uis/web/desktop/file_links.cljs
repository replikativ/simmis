(ns is.simm.uis.web.desktop.file-links
  "Portable link marks, with disposable navigation/edit controls owned by one
   native editor. Link destinations and edits remain in the document model."
  (:require ["@tiptap/extension-link" :default Link]
            ["@tiptap/core" :refer [getMarkRange]]
            [is.simm.documents.attachments :as attachments]
            [is.simm.documents.links :as links]
            [is.simm.documents.rich-text :as rich]))

(defn- base [^js host]
  (when-let [value (.getAttribute host "data-file-base")]
    (js->clj (js/JSON.parse value) :keywordize-keys true)))

(defn- parsed-url [href]
  (try (js/URL. href (.-href js/location)) (catch :default _ nil)))

(defn- internal-route [href]
  (when-let [url (parsed-url href)]
    (let [route (str (.-pathname url) (.-search url))]
    (when (and (= (.-origin url) (.-origin js/location)) (empty? (.-hash url)) (links/parse-route route)) route))))

(defn- paint! [^js host ^js dom raw]
  (let [href (when (rich/safe-link? raw) (attachments/resolve-link (base host) raw))]
    (if (and href (rich/safe-link? href) (parsed-url href))
      (do (.setAttribute dom "href" href)
          (.setAttribute dom "target" (if (internal-route href) "_self" "_blank"))
          (.setAttribute dom "tabindex" "0"))
      (do (.removeAttribute dom "href") (.removeAttribute dom "tabindex")))))

(defn refresh! [^js host]
  (when-let [close! (.-simmisCloseLinkActions host)] (close!))
  (doseq [dom (array-seq (.querySelectorAll host "a[data-memory-source-href]"))]
    (paint! host dom (.getAttribute dom "data-memory-source-href"))))

(defn- follow! [^js host ^js anchor separate?]
  (when (.hasAttribute anchor "href")
    (let [href (.-href anchor)]
      (if-let [route (when-not separate? (internal-route href))]
        ;; The foreign element bridges to its owning Spindel context. Without
        ;; an application handler, retain ordinary browser navigation.
        (when (.dispatchEvent host (js/CustomEvent. "simmis-follow-link"
                                                    #js {:cancelable true :detail route}))
          (.assign js/location href))
        (js/window.open href "_blank" "noopener,noreferrer")))))

(defn attach! [^js host ^js editor]
  (let [popup (.createElement js/document "div")
        current (volatile! nil) press (volatile! nil)
        close! (fn [] (vreset! current nil) (.remove popup))
        anchor-at (fn [target]
                    (let [a (when (instance? js/Element target) (.closest target "a[data-memory-source-href]"))]
                      (when (and a (.contains host a) (.hasAttribute a "href")) a)))
        status! (fn [text]
                  (when-let [status (.querySelector popup "[role=status]")]
                    (set! (.-textContent status) text)))
        button! (fn [label action]
                  (let [b (.createElement js/document "button")]
                    (set! (.-type b) "button") (set! (.-textContent b) label)
                    (.addEventListener b "click" action)
                    (.appendChild popup b) b))
        position! (fn [anchor]
                    (let [rect (.getBoundingClientRect anchor)]
                      (set! (.. popup -style -left) (str (max 8 (min (.-left rect) (- (.-innerWidth js/window) 370))) "px"))
                      (set! (.. popup -style -top) (str (max 8 (min (+ (.-bottom rect) 4)
                                                                 (- (.-innerHeight js/window) (.-offsetHeight popup) 8))) "px"))))
        reposition! (fn [event]
                      ;; Inputs scroll horizontally while editing long URLs.
                      ;; Only movement of the source/viewport affects anchoring.
                      (when (and @current (not (and (instance? js/Node (.-target event))
                                                                  (.contains popup (.-target event)))))
                        (let [anchor @current rect (.getBoundingClientRect anchor)]
                          (if (and (.-isConnected anchor) (pos? (.-width rect))
                                   (pos? (.-bottom rect)) (< (.-top rect) (.-innerHeight js/window)))
                            (position! anchor)
                            (close!)))))
        show! (fn [anchor]
                (when-not (identical? anchor @current)
                  (vreset! current anchor)
                  (set! (.-textContent popup) "")
                  (button! "Open" (fn [event]
                                    (follow! host anchor (or (.-ctrlKey event) (.-metaKey event))) (close!)))
                  (button! "Edit link"
                           (fn [_]
                             (let [from (.posAtDOM (.-view editor) anchor 0)
                                   range (getMarkRange (.resolve (.. editor -state -doc) from)
                                                       (.. editor -schema -marks -link)
                                                       #js {:href (.getAttribute anchor "data-memory-source-href")})
                                   input (.createElement js/document "input")]
                               (set! (.-textContent popup) "")
                               (set! (.-type input) "text")
                               (set! (.-value input) (.getAttribute anchor "data-memory-source-href"))
                               (.setAttribute input "aria-label" "Link address")
                               (.appendChild popup input)
                               (let [save! (fn [_]
                                             (let [href (.-value input)]
                                               (if (and range (.-isEditable editor) (rich/safe-link? href) (parsed-url href))
                                                 (do (.. editor (chain) (focus)
                                                         (setTextSelection range)
                                                         (setLink #js {:href href}) (run))
                                                     (close!))
                                                 (status! "Enter a safe link address."))))]
                                 (button! "Save" save!)
                                 (button! "Cancel" (fn [_] (close!) (.focus (.-view editor))))
                                 (.addEventListener input "keydown" (fn [e] (when (= "Enter" (.-key e)) (.preventDefault e) (save! e)))))
                               (let [status (.createElement js/document "span")]
                                 (.setAttribute status "role" "status") (.appendChild popup status))
                               (position! anchor) (.focus input) (.select input))))
                  (set! (.-disabled (.querySelector popup "button:nth-child(2)")) (not (.-isEditable editor)))
                  (button! "Copy address"
                           (fn [_]
                             (if-let [clipboard (.-clipboard js/navigator)]
                               (-> (.writeText clipboard (.-href anchor))
                                   (.then #(when (identical? anchor @current) (status! "Copied")))
                                   (.catch #(when (identical? anchor @current) (status! "Copy unavailable; use the browser link menu."))))
                               (status! "Copy unavailable; use the browser link menu."))))
                  (let [status (.createElement js/document "span")]
                    (.setAttribute status "role" "status") (.appendChild popup status))
                  (.appendChild (.-body js/document) popup)
                  (position! anchor)))
        over! (fn [e] (when-let [anchor (anchor-at (.-target e))] (show! anchor)))
        down! (fn [e]
                (when (= 0 (.-button e))
                  (vreset! press {:pointer (.-pointerId e) :x (.-clientX e) :y (.-clientY e) :down? true})
                  (when (and (anchor-at (.-target e)) (not (.-shiftKey e)))
                    ;; Focusing the editor can publish a selection and replace
                    ;; its DOM between press and release, losing the click.
                    ;; Links navigate without first entering the text editor.
                    (.preventDefault e))))
        move! (fn [e]
                (when (and (:down? @press) (= (:pointer @press) (.-pointerId e))
                           (> (+ (js/Math.abs (- (.-clientX e) (:x @press)))
                                 (js/Math.abs (- (.-clientY e) (:y @press)))) 5))
                  (vswap! press assoc :dragged? true)))
        up! (fn [e]
              (when (= (:pointer @press) (.-pointerId e))
                (move! e)
                (vswap! press assoc :down? false)))
        cancel-press! (fn [_] (vreset! press nil))
        click! (fn [e]
                 (when-let [anchor (anchor-at (.-target e))]
                   (.preventDefault e)
                   ;; Shift-click extends a text selection. Preserve dragging
                   ;; through a link instead of turning a copy gesture into navigation.
                   (let [dragged? (:dragged? @press)]
                     (vreset! press nil)
                     (when-not (or (.-shiftKey e) dragged?)
                       (.stopPropagation e)
                       (follow! host anchor (or (.-ctrlKey e) (.-metaKey e))) (close!)))))
        key! (fn [e]
               (cond
                 (= "Escape" (.-key e)) (do (close!) (when (.contains popup (.-target e)) (.focus (.-view editor))))
                 (and (= "ArrowDown" (.-key e)) (anchor-at (.-target e)))
                 (do (.preventDefault e) (.stopPropagation e)
                     (show! (anchor-at (.-target e))) (.focus (.querySelector popup "button")))
                 (and (= "Enter" (.-key e)) (anchor-at (.-target e)))
                 (do (.preventDefault e) (.stopPropagation e)
                     (follow! host (anchor-at (.-target e)) (or (.-ctrlKey e) (.-metaKey e))) (close!))))
        outside! (fn [e] (when-not (or (.contains popup (.-target e)) (anchor-at (.-target e))) (close!)))
        bridge! (fn [e] (when-let [handler (aget host "onsimmis-follow-link")] (handler e)))]
    (set! (.-className popup) "memory-link-actions")
    (.setAttribute popup "role" "group") (.setAttribute popup "aria-label" "Link actions")
    (.setAttribute popup "contenteditable" "false")
    (set! (.-simmisCloseLinkActions host) close!)
    (.addEventListener popup "keydown" key!)
    (doseq [event ["pointerover" "focusin"]] (.addEventListener host event over!))
    (.addEventListener host "pointerdown" down! true)
    (.addEventListener js/document "pointermove" move! true)
    (.addEventListener js/document "pointerup" up! true)
    (.addEventListener js/document "pointercancel" cancel-press! true)
    (.addEventListener host "click" click! true)
    (.addEventListener host "keydown" key!)
    (.addEventListener host "simmis-follow-link" bridge!)
    (.addEventListener js/document "pointerdown" outside! true)
    (.addEventListener js/document "focusin" outside! true)
    (.addEventListener js/window "scroll" reposition! true)
    (.addEventListener js/window "resize" reposition!)
    (.on editor "update" close!)
    (fn []
      (close!)
      (vreset! press nil)
      (set! (.-simmisCloseLinkActions host) nil)
      (doseq [event ["pointerover" "focusin"]] (.removeEventListener host event over!))
      (.removeEventListener host "pointerdown" down! true)
      (.removeEventListener js/document "pointermove" move! true)
      (.removeEventListener js/document "pointerup" up! true)
      (.removeEventListener js/document "pointercancel" cancel-press! true)
      (.removeEventListener host "click" click! true)
      (.removeEventListener host "keydown" key!)
      (.removeEventListener host "simmis-follow-link" bridge!)
      (.removeEventListener js/document "pointerdown" outside! true)
      (.removeEventListener js/document "focusin" outside! true)
      (.removeEventListener js/window "scroll" reposition! true)
      (.removeEventListener js/window "resize" reposition!)
      (.off editor "update" close!))))

(defn extension [^js host]
  (.configure
   (.extend Link
            #js {:addMarkView
                 (fn []
                   (fn [props]
                     (let [dom (.createElement js/document "a") raw (.. props -mark -attrs -href)]
                       (.setAttribute dom "data-memory-source-href" (or raw ""))
                       (.setAttribute dom "rel" "noopener noreferrer")
                       (.setAttribute dom "title" (or (.. props -mark -attrs -title) "Click to follow · Ctrl/⌘-click opens separately · ↓ for link actions"))
                       (paint! host dom raw)
                       #js {:dom dom :contentDOM dom
                            :ignoreMutation (fn [record] (and (= "attributes" (.-type record)) (identical? dom (.-target record))))})))})
   #js {:openOnClick false :autolink false :linkOnPaste false :isAllowedUri rich/safe-link?}))
