(ns is.simm.uis.web.desktop.rich-controls
  "Native command/geometry adapter. Durable selection/control snapshots belong
   to the draft; observers, frame handles and focus belong to one DOM element.")

(def groups
  [[{:op :paragraph :label "Paragraph" :icon :pilcrow :node "paragraph"}
    {:op :heading :label "Heading 2" :icon :heading-2 :node "heading" :attrs {:level 2}}]
   [{:op :bold :label "Bold" :icon :bold :node "bold"}
    {:op :italic :label "Italic" :icon :italic :node "italic"}
    {:op :strike :label "Strikethrough" :icon :strikethrough :node "strike"}
    {:op :code :label "Inline code" :icon :code :node "code"}]
   [{:op :bullet-list :label "Bullet list" :icon :list :node "bulletList"}
    {:op :ordered-list :label "Numbered list" :icon :list-ordered :node "orderedList"}]])

(defn- operation-chain [^js chain op]
  (case op
    :bold (.toggleBold chain) :italic (.toggleItalic chain) :strike (.toggleStrike chain)
    :code (.toggleCode chain) :paragraph (.setParagraph chain)
    :heading (.toggleHeading chain #js {:level 2})
    :bullet-list (.toggleBulletList chain) :ordered-list (.toggleOrderedList chain)
    nil))

(defn- ancestor [^js position type]
  (loop [depth (.-depth position)]
    (when (pos? depth)
      (let [^js node (.node position depth)]
        (if (= type (.. node -type -name)) (aget (.-attrs node) "key")
            (recur (dec depth)))))))

(defn region [^js editor]
  (let [selection (.. editor -state -selection)
        from (ancestor (.-$from selection) "memoryRegion")
        to (ancestor (.-$to selection) "memoryRegion")]
    (when (= from to) from)))

(defn format-state [^js el ^js editor]
  (let [protected? (= "true" (.getAttribute el "data-document"))
        same-region? (or (not protected?) (some? (region editor)))]
    (into {} (for [{:keys [op node attrs]} (mapcat identity groups)]
               [op {:active? (if attrs (.isActive editor node (clj->js attrs)) (.isActive editor node))
                    :enabled? (boolean
                               (and (.-isEditable editor)
                                    ;; Native can() does not include our topology
                                    ;; filter. Restrict block transforms to one
                                    ;; editable owner; the guard stays final.
                                    (or (#{:bold :italic :strike :code} op) same-region?)
                                    (try (when-let [^js chain (operation-chain (.chain (.can editor)) op)] (.run chain))
                                         (catch :default _ false))))}]))))

(defn- owned-element [^js target]
  (when-let [^js container (.closest target ".memory-document, .memory-region-editor")]
    (or (.querySelector container "simmis-rich-editor:focus-within")
        (let [els (.querySelectorAll container "simmis-rich-editor")]
          (when (= 1 (.-length els)) (aget els 0))))))

(defn command! [event op]
  (when-let [^js el (owned-element (.-target event))]
    (when-let [^js editor (.-simmisRichEditor el)]
      (when (get-in (format-state el editor) [op :enabled?])
        (.focus (.-view editor))
        (when-let [^js chain (operation-chain (.chain editor) op)] (.run chain))))))

(defn toolbar-keydown! [event]
  (let [^js toolbar (.-currentTarget event)
        buttons (vec (array-seq (.querySelectorAll toolbar "button:not(:disabled)")))
        index (.indexOf (clj->js buttons) (.-target event))
        key (.-key event)]
    (cond
      (and (seq buttons) (contains? #{"ArrowLeft" "ArrowRight" "Home" "End"} key))
      (let [n (count buttons) next (case key "Home" 0 "End" (dec n) "ArrowLeft" (mod (dec index) n) (mod (inc index) n))]
        (.preventDefault event)
        (doseq [^js b buttons] (set! (.-tabIndex b) -1))
        (let [^js button (nth buttons next)] (set! (.-tabIndex button) 0) (.focus button)))
      (= key "Escape")
      (when-let [^js el (owned-element toolbar)]
        (.preventDefault event)
        (when-let [^js editor (.-simmisRichEditor el)] (.. editor -commands (focus)))
        (set! (.-simmisToolbarSuppressed el) true)
        (.setAttribute toolbar "data-visible" "false")))))

(defn refresh! [^js el]
  (set! (.-simmisToolbarSuppressed el) false)
  (when-let [schedule (.-simmisScheduleControls el)] (schedule)))

(defn request-editor-focus! [event]
  (when-let [^js panel (.closest (.-target event) ".memory-panel")]
    (when-let [^js el (.querySelector panel "simmis-rich-editor")]
      ;; The request belongs to this panel and exact draft scope, not a global
      ;; active editor. Consume on editable transition or replacement mount.
      (set! (.-simmisRichFocusScope panel) (.getAttribute el "data-focus-scope")))))

(defn consume-focus-request! [^js el ^js editor]
  (when-let [^js panel (.closest el ".memory-panel")]
    (let [scope (.getAttribute el "data-focus-scope")]
      (when (and scope (.-isEditable editor) (= scope (.-simmisRichFocusScope panel)))
        (set! (.-simmisRichFocusScope panel) nil)
        (.focus (.-view editor))))))

(defn focus-toolbar! [^js el]
  (set! (.-simmisToolbarSuppressed el) false)
  (when-let [position (.-simmisPositionControls el)] (position))
  (when-let [^js container (.closest el ".memory-document")]
    (when-let [^js button (.querySelector container ".memory-document-toolbar button:not(:disabled)")]
      (.focus button))))

(defn attach! [^js el ^js editor]
  (when-let [^js container (.closest el ".memory-document")]
    (when-let [^js toolbar (.querySelector container ".memory-document-toolbar")]
      (let [^js panel (.closest el ".memory-panel")
            position! (fn []
                        (when (and (.-isConnected el) (identical? editor (.-simmisRichEditor el)))
                          (let [active (.-activeElement js/document)
                                visible? (and (.-isEditable editor) (not (.-simmisToolbarSuppressed el))
                                              (or (.contains el active) (.contains toolbar active)))]
                            (.setAttribute toolbar "data-visible" (str (boolean visible?)))
                            (when visible?
                              (let [^js dom (.domAtPos (.-view editor) (.. editor -state -selection -head))
                                    ^js node (.-node dom)
                                    ^js element (if (= 1 (.-nodeType node)) node (.-parentElement node))
                                    ^js anchor (or (.closest element ".memory-semantic-block")
                                                   (.closest element ".memory-continuous-region") el)
                                    ^js rect (.getBoundingClientRect anchor)
                                    ^js root (.getBoundingClientRect container)
                                    ^js clip (if panel (.getBoundingClientRect panel) #js {:top 0 :left 0 :right (.-innerWidth js/window) :bottom (.-innerHeight js/window)})
                                    top (max (+ (.-top clip) 4) (- (.-top rect) (.-offsetHeight toolbar) 6))
                                    left (max (+ (.-left clip) 4) (min (.-left rect) (- (.-right clip) (.-offsetWidth toolbar) 4)))]
                                (doseq [^js old (array-seq (.querySelectorAll container "[data-active-block]"))]
                                  (.removeAttribute old "data-active-block"))
                                (.setAttribute anchor "data-active-block" "true")
                                (when (or (< (.-bottom rect) (.-top clip)) (> (.-top rect) (.-bottom clip)))
                                  (.setAttribute toolbar "data-visible" "false"))
                                (set! (.. toolbar -style -top) (str (- top (.-top root)) "px"))
                                (set! (.. toolbar -style -left) (str (- left (.-left root)) "px")))))))
            schedule! (fn [& _]
                        ;; Geometry is measured after DOM layout. This frame is
                        ;; not a synchronization delay; teardown cancels it.
                        (when-not (.-simmisToolbarFrame el)
                          (set! (.-simmisToolbarFrame el)
                                (js/requestAnimationFrame (fn [] (set! (.-simmisToolbarFrame el) nil) (position!))))))
            observer (js/ResizeObserver. schedule!)]
        (set! (.-simmisPositionControls el) position!)
        (set! (.-simmisScheduleControls el) schedule!)
        (doseq [^js target [el toolbar] event ["focusin" "focusout"]] (.addEventListener target event schedule!))
        (.addEventListener js/window "scroll" schedule! true)
        (.addEventListener js/window "resize" schedule!)
        (doseq [target [el toolbar container]] (.observe observer target))
        (schedule!)
        (fn []
          (.disconnect observer)
          (doseq [^js target [el toolbar] event ["focusin" "focusout"]] (.removeEventListener target event schedule!))
          (.removeEventListener js/window "scroll" schedule! true)
          (.removeEventListener js/window "resize" schedule!)
          (when-let [frame (.-simmisToolbarFrame el)] (js/cancelAnimationFrame frame))
          (set! (.-simmisToolbarFrame el) nil)
          (set! (.-simmisScheduleControls el) nil)
          (set! (.-simmisPositionControls el) nil))))))
