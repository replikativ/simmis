(ns is.simm.uis.web.desktop.rich-editor
  "One controlled Tiptap region, no global handles or alternative draft state."
  (:require ["@tiptap/core" :refer [Editor Node Extension]]
            ["@tiptap/pm/state" :refer [Plugin Selection TextSelection AllSelection NodeSelection]]
            ["@tiptap/pm/gapcursor" :refer [GapCursor]]
            ["@tiptap/starter-kit" :default StarterKit]
            [is.simm.documents.rich-text :as rich]
            [is.simm.documents.rich-document :as document]
            [is.simm.uis.web.desktop.rich-controls :as controls]
            [is.simm.uis.web.desktop.code-highlight :as code-highlight]
            [is.simm.uis.web.desktop.chart-view :as charts]
            [is.simm.uis.web.desktop.math-view :as math]
            [is.simm.uis.web.desktop.editor-jump :as jump]
            [org.replikativ.spindel.dom.foreign]
            [org.replikativ.spindel.engine.core :as ec])
  (:require-macros [org.replikativ.spindel.dom.foreign :refer [foreign-node]]))

(defn- data [^js el]
  (js/JSON.parse (or (.getAttribute el "data-model") "{\"type\":\"doc\",\"content\":[]}")))

(defn- read-only? [^js el] (= "true" (.getAttribute el "data-read-only")))

(defn- json-model [model]
  ;; ProseMirror attributes use null-prototype objects. js->clj deliberately
  ;; leaves those as native objects; a JSON boundary restores ordinary data
  ;; before rich validation, equality checks and context-backed draft storage.
  (js/JSON.parse (js/JSON.stringify model)))

(defn- portable-model [model]
  (js->clj (json-model model) :keywordize-keys true))

(defn- restore-selection! [^js el ^js editor]
  (when-let [raw (.getAttribute el "data-selection")]
    (let [^js envelope (js/JSON.parse raw)
          scoped? (some? (aget envelope "owner"))
          ^js value (if scoped? (aget envelope "selection") envelope)
          ^js doc (.. editor -state -doc)
          clamp #(max 0 (min (.. doc -content -size) (if (number? %) % 0)))]
      (when (and value (or (not scoped?) (= (aget envelope "owner") (.getAttribute el "data-owner"))))
        (let [^js native (aget value "native")
              selection (cond
                          (= "all" (some-> native (aget "type"))) (AllSelection. doc)
                          (= "gapcursor" (some-> native (aget "type")))
                          (let [position (.resolve doc (clamp (aget native "pos")))]
                            (if (.valid GapCursor position)
                              (GapCursor. position)
                              (.near Selection position)))
                          (and (= "node" (some-> native (aget "type")))
                               (some? (.nodeAt doc (clamp (aget native "anchor"))))
                               (.isSelectable NodeSelection (.nodeAt doc (clamp (aget native "anchor")))))
                          (.create NodeSelection doc (clamp (aget native "anchor")))
                          :else
                          (.between TextSelection
                                    (.resolve doc (clamp (if native (aget native "anchor") (aget value "from"))))
                                    (.resolve doc (clamp (if native (aget native "head") (aget value "to"))))))]
          (.dispatch (.-view editor) (.setSelection (.. editor -state -tr) selection)))))))

(defn- region-at [position]
  (some (fn [depth]
          (let [node (.node position depth)]
            (when (= "memoryRegion" (.. node -type -name))
              (aget (.-attrs node) "key"))))
        (range (.-depth position) 0 -1)))

(defn- navigate-boundary! [^js view event]
  ;; Native motion owns wrapped lines, lists and selection extension. At the
  ;; last visual line of a region, skip the noneditable semantic wrappers and
  ;; enter the adjacent text region instead of creating an invisible gap stop.
  (when (and (#{"ArrowDown" "ArrowUp"} (.-key event))
             (not (or (.-shiftKey event) (.-altKey event) (.-ctrlKey event) (.-metaKey event)))
             (.-editable view))
    (let [selection (.. view -state -selection)
          direction (if (= "ArrowDown" (.-key event)) 1 -1)
          position (.-$head selection)
          gap? (instance? GapCursor selection)]
      (when (and (.-empty selection)
                 (or gap? (and (instance? TextSelection selection)
                               (.endOfTextblock view (if (pos? direction) "down" "up")))))
        (let [start (if gap? position
                        (.resolve (.. view -state -doc)
                                  (if (pos? direction) (.after position) (.before position))))
              next (.findFrom Selection start direction true)]
          (when (and next (or gap? (not= (region-at position) (region-at (.-$head next)))))
            (.dispatch view (.scrollIntoView (.setSelection (.. view -state -tr) next)))
            true))))))

(defn- paint-chart! [^js el ^js projection block-id]
  (let [data (some-> (.-simmisChartData el) (aget block-id))]
    ;; A native element-owned render cache, not semantic state. ProseMirror may
    ;; update a block on every keystroke; keep its unchanged SVG/table intact.
    (when-not (identical? data (.-simmisChartProjection projection))
      (set! (.-simmisChartProjection projection) data)
      (charts/paint! projection
                     (when data
                       (update (js->clj data :keywordize-keys true) :kind
                               #(case % "line" :line "bar" :bar "point" :point nil)))))))

(defn- block-node-view [el]
  (fn [props]
    (let [dom (.createElement js/document "div")
          content (.createElement js/document "div")
          link (.createElement js/document "a")
          projection (.createElement js/document "div")
          paint! (fn [node]
                   (let [id (aget (.-attrs node) "blockId")]
                     (.setAttribute dom "data-block-id" id)
                     (.toggleAttribute dom "data-linked-block" (= id (.getAttribute el "data-linked-block-id")))
                     (let [href (aget (js/JSON.parse (or (.getAttribute el "data-block-links") "{}")) id)]
                       (set! (.-hidden link) (not href))
                       (if href (.setAttribute link "href" href) (.removeAttribute link "href")))
                     (.setAttribute projection "data-chart-block" id)
                     (paint-chart! el projection id)))]
      (set! (.-className dom) "memory-semantic-block")
      (set! (.-className content) "memory-semantic-content")
      (set! (.-className link) "memory-block-link")
      (set! (.-textContent link) "↗")
      (.setAttribute link "contenteditable" "false")
      (.setAttribute link "aria-label" "Copy block link")
      (.setAttribute link "title" "Copy block link · browser menu opens or copies the address")
      (.setAttribute link "target" "_blank")
      (.setAttribute link "rel" "noopener noreferrer")
      (set! (.-onclick link)
            (fn [event]
              (when-not (or (.-ctrlKey event) (.-metaKey event) (.-shiftKey event) (.-altKey event))
                (.preventDefault event)
                (when-let [clipboard (.-clipboard js/navigator)]
                  (-> (.writeText clipboard (.-href link))
                      (.then #(.setAttribute link "title" "Block link copied"))
                      (.catch #(.setAttribute link "title" "Use the browser menu to copy this link")))))))
      (set! (.-className projection) "memory-chart-slot")
      (.setAttribute projection "contenteditable" "false")
      (.appendChild dom link) (.appendChild dom content) (.appendChild dom projection)
      (paint! (.-node props))
      #js {:dom dom :contentDOM content
           :update (fn [node]
                     (when (= "memoryBlock" (.. node -type -name)) (paint! node) true))
           :ignoreMutation (fn [mutation]
                             (and (not= "selection" (.-type mutation))
                                  (or (.contains projection (.-target mutation)) (.contains link (.-target mutation))
                                      (and (= "attributes" (.-type mutation)) (= dom (.-target mutation))
                                           (#{"data-block-id" "data-linked-block"} (.-attributeName mutation))))))
           :stopEvent (fn [event] (or (.contains projection (.-target event)) (.contains link (.-target event))))
           :destroy (fn [] (charts/paint! projection nil)
                      (set! (.-onclick link) nil)
                      (set! (.-simmisChartProjection projection) nil))})))

(defn- boundary-node [name content el]
  (.create Node
           #js {:name name :group "block" :content content
                :atom (= name "memoryOpaque") :isolating true :defining true
                :renderText (when (= name "memoryOpaque")
                              (fn [props] (aget (.. props -node -attrs) "label")))
                :addAttributes (fn [] #js {:key #js {:default nil}
                                           :blockId #js {:default nil}
                                           :label #js {:default nil}})
                ;; Copied HTML may carry text, never another block's identity.
                :parseHTML (fn [] #js [])
                :addNodeView (when (= name "memoryBlock") (fn [] (block-node-view el)))
                :renderHTML (fn [props]
                              (let [^js attrs (.. props -node -attrs)
                                    dom-attrs #js {:class (case name "memoryBlock" "memory-semantic-block"
                                                               "memoryRegion" "memory-continuous-region" "memory-opaque")
                                                  :data-block-id (.-blockId attrs)}]
                                (if (= name "memoryOpaque")
                                  #js ["pre" dom-attrs (or (.-label attrs) "")]
                                  #js ["div" dom-attrs 0])))}))

(defn- footnote-node []
  (.create Node
           #js {:name "footnoteReference" :group "inline" :inline true :atom true
                :addAttributes (fn [] #js {:sourceKey #js {:default nil}
                                           :raw #js {:default nil} :label #js {:default nil}})
                ;; HTML/clipboard text may never import projection-local keys.
                :parseHTML (fn [] #js [])
                :renderText (fn [props] (aget (.. props -node -attrs) "raw"))
                ;; Clipboard HTML retains spelling as inert ordinary text, not
                ;; the compact label or an importable identity-bearing node.
                :renderHTML (fn [props] #js ["span" #js {} (aget (.. props -node -attrs) "raw")])
                :addNodeView
                (fn []
                  (fn [props]
                    (let [^js dom (.createElement js/document "sup")
                          paint! (fn [^js node]
                                   (set! (.-className dom) "memory-footnote-reference")
                                   (set! (.-textContent dom) (aget (.-attrs node) "label"))
                                   (.setAttribute dom "title" (aget (.-attrs node) "raw"))
                                   (.setAttribute dom "aria-label" (str "Footnote " (aget (.-attrs node) "label"))))]
                      (paint! (.-node props))
                      #js {:dom dom :update (fn [^js node]
                                             (when (= "footnoteReference" (.. node -type -name))
                                               (paint! node) true))})))}))

(defn- math-node [name display?]
  (.create Node
           #js {:name name :group (if display? "block" "inline") :inline (not display?) :atom true
                :addAttributes (fn [] #js {:sourceKey #js {:default nil}
                                           :raw #js {:default nil} :tex #js {:default nil}})
                :parseHTML (fn [] #js [])
                :renderText (fn [props] (aget (.. props -node -attrs) "raw"))
                :renderHTML (fn [props] #js [(if display? "div" "span") #js {}
                                            (aget (.. props -node -attrs) "raw")])
                :addNodeView
                (fn []
                  (fn [props]
                    (let [dom (.createElement js/document (if display? "div" "span"))
                          paint! (fn [^js node]
                                   (.removeAttribute dom "aria-label")
                                   (math/paint! dom (aget (.-attrs node) "tex") (aget (.-attrs node) "raw") display?))]
                      (paint! (.-node props))
                      #js {:dom dom :update (fn [^js node]
                                             (when (= name (.. node -type -name)) (paint! node) true))
                           :destroy #(.replaceChildren dom)})))}))

(defn- boundary-guard [^js el]
  (.create Extension
           #js {:name "memoryBoundaryGuard"
                :addProseMirrorPlugins
                (fn []
                  #js [(Plugin.
                        #js {:filterTransaction
                             (fn [^js transaction ^js state]
                               (if-not (and (= "true" (.getAttribute el "data-document")) (.-docChanged transaction))
                                 true
                                 (let [allowed? (try (= (document/topology (portable-model (.toJSON (.-doc state))))
                                                        (document/topology (portable-model (.toJSON (.-doc transaction)))))
                                                     (catch :default _ false))]
                                   (when-not allowed?
                                     (.dispatchEvent el (js/CustomEvent. "input" #js {:bubbles true :composed true
                                                                                     :detail #js {:blocked true}})))
                                   allowed?)))})])}))

(defn disconnect! [^js el]
  (when-let [release (.-simmisReleaseJump el)]
    (release) (set! (.-simmisReleaseJump el) nil))
  (set! (.-simmisChartData el) nil)
  (when-let [release (.-simmisReleaseControls el)]
    (release)
    (set! (.-simmisReleaseControls el) nil))
  (when-let [^js editor (.-simmisRichEditor el)]
    (.destroy editor)
    (set! (.-simmisRichEditor el) nil)))

(defn- notify! [^js el ^js editor]
  (controls/refresh! el)
  (when (and (identical? editor (.-simmisRichEditor el)) (not (.-simmisApplying el)) (not (read-only? el)))
    (.dispatchEvent el
                    (js/CustomEvent. "input"
                                     #js {:bubbles true :composed true
                                          :detail #js {:model (json-model (.getJSON editor))
                                                       :selection #js {:from (.. editor -state -selection -from)
                                                                       :to (.. editor -state -selection -to)
                                                                       :native (.. editor -state -selection (toJSON))
                                                                       :region (controls/region editor)
                                                                       :format (clj->js (controls/format-state el editor))}}}))))

(defn- consume-jump! [^js el ^js editor]
  (jump/consume!
   el (fn [request]
        ;; Typing can occur between rendering props and this native lifecycle
        ;; callback. Never move a caret in an unreported native draft.
        (when (= (document/normalized (js->clj (data el) :keywordize-keys true))
                 (document/normalized (portable-model (.getJSON editor))))
          (let [doc (.. editor -state -doc) found (volatile! nil)]
            (.descendants doc (fn [^js node pos]
                                (if @found false
                                    (when (and (= "memoryBlock" (.. node -type -name))
                                               (= (aget request "block-id") (aget (.-attrs node) "blockId")))
                                      (vreset! found [node pos]) false))))
            (when-let [[^js node pos] @found]
              (let [text-position (volatile! nil)]
                (.descendants node (fn [^js child relative]
                                     (cond @text-position false
                                           (.-isTextblock child) (do (vreset! text-position (+ pos relative 2)) false)
                                           :else true)))
                (let [selection (if-let [position @text-position]
                                  (.create TextSelection doc position)
                                  (.create NodeSelection doc pos))]
                  (.dispatch (.-view editor) (.scrollIntoView (.setSelection (.. editor -state -tr) selection)))
                  (.focus (.-view editor))
                  #js {:from (.-from selection) :to (.-to selection) :native (.toJSON selection)
                       :region (controls/region editor) :format (clj->js (controls/format-state el editor))}))))))))

(defn mount! [^js el]
  (when-not (.-simmisRichEditor el)
    (set! (.-simmisChartData el) (js/JSON.parse (or (.getAttribute el "data-charts") "{}")))
    (let [^js editor (Editor.
                  #js {:element el :content (data el) :editable (not (read-only? el))
                       :extensions #js [(boundary-node "memoryBlock" "(memoryBlock|memoryRegion|memoryOpaque)*" el)
                                        (boundary-node "memoryRegion" "(paragraph|heading|bulletList|orderedList|codeBlock|horizontalRule|displayMath)+" el)
                                        (boundary-node "memoryOpaque" "" el)
                                        (footnote-node) (math-node "inlineMath" false) (math-node "displayMath" true)
                                        (boundary-guard el) (code-highlight/extension)
                                        (.configure StarterKit
                                                    #js {:underline false :blockquote false :trailingNode false
                                                         :link #js {:openOnClick false
                                                                   :autolink false :linkOnPaste false
                                                                   :isAllowedUri rich/safe-link?}})]
                       :editorProps #js {:attributes #js {:class "memory-rich-content"
                                                         :role "textbox" :aria-multiline "true"
                                                         :aria-label "Rich document editor"}
                                         :handleKeyDown (fn [view event]
                                                          (or (when (and (.-altKey event) (= "F10" (.-key event)))
                                                                (.preventDefault event) (controls/focus-toolbar! el) true)
                                                              (navigate-boundary! view event)))}
                       :onUpdate (fn [event] (notify! el (.-editor event)))
                       :onFocus (fn [event] (notify! el (.-editor event)))
                       :onSelectionUpdate (fn [event] (notify! el (.-editor event)))})]
      (set! (.-simmisRichEditor el) editor)
      (set! (.-simmisReleaseJump el) (jump/attach! el))
      (set! (.-simmisReleaseControls el) (controls/attach! el editor))
      (restore-selection! el editor)
      (controls/consume-focus-request! el editor)
      ;; A rebuilt editor may already own focus before onFocus can publish a
      ;; snapshot. Initial control state must not wait for a caret movement.
      (notify! el editor)
      (consume-jump! el editor))))

(defn- sync-selection! [^js el ^js editor]
  (restore-selection! el editor))

(defn sync! [^js el attribute old new]
  (when (not= old new)
    (when-let [^js editor (.-simmisRichEditor el)]
      (set! (.-simmisApplying el) true)
      (try (case attribute
        "data-owner" (do (disconnect! el) (mount! el))
        "data-model" (when-not (= (document/normalized (js->clj (data el) :keywordize-keys true))
                                  (document/normalized (portable-model (.getJSON editor))))
                       ;; Authoritative source/rollback: a new editor also
                       ;; resets its native undo history.
                       (disconnect! el) (mount! el))
        "data-read-only" (do (.setEditable editor (not (read-only? el)))
                             (controls/consume-focus-request! el editor))
        "data-selection" (sync-selection! el editor)
        "data-charts" (do
                        (set! (.-simmisChartData el) (js/JSON.parse (or new "{}")))
                        (doseq [projection (array-seq (.querySelectorAll el ".memory-chart-slot"))]
                          (paint-chart! el projection (.getAttribute projection "data-chart-block"))))
        "data-block-links" (doseq [link (array-seq (.querySelectorAll el ".memory-block-link"))]
                             (let [id (.getAttribute (.-parentElement link) "data-block-id")
                                   href (aget (js/JSON.parse (or new "{}")) id)]
                               (set! (.-hidden link) (not href))
                               (if href (.setAttribute link "href" href) (.removeAttribute link "href"))))
        "data-linked-block-id" (doseq [block (array-seq (.querySelectorAll el ".memory-semantic-block"))]
                                 (.toggleAttribute block "data-linked-block" (= new (.getAttribute block "data-block-id"))))
        nil)
        (consume-jump! el (.-simmisRichEditor el))
        (finally (set! (.-simmisApplying el) false))))))

(when-not (.get js/customElements "simmis-rich-editor")
  (.define js/customElements "simmis-rich-editor"
           (js* "(class extends HTMLElement {
             connectedCallback() { (~{})(this); }
             disconnectedCallback() { (~{})(this); }
             attributeChangedCallback(name, oldValue, newValue) { (~{})(this, name, oldValue, newValue); }
             static get observedAttributes() { return ['data-owner', 'data-model', 'data-read-only', 'data-selection', 'data-charts', 'data-block-links', 'data-linked-block-id', 'data-jump']; }
           })" mount! disconnect! sync!)))

(defn view [{:keys [id model selection read-only? on-change document? on-blocked charts block-links linked-block jump before-jump on-jumped]}]
  (foreign-node
   {:tag :simmis-rich-editor :key id :class "memory-rich-editor"
    :data-owner (pr-str [id (:fork-id ec/*execution-context*)])
    :data-focus-scope (pr-str [(first id) (second id) (:fork-id ec/*execution-context*)])
    :data-model (js/JSON.stringify (clj->js model))
    :data-charts (js/JSON.stringify (clj->js (or charts {})))
    :data-block-links (js/JSON.stringify (clj->js (or block-links {})))
    :data-linked-block-id (or linked-block "")
    :data-jump (when jump (js/JSON.stringify (clj->js jump)))
    :on-simmis-before-jump #(jump/before! % before-jump)
    :on-simmis-jumped #(jump/acknowledge! % on-jumped)
    :data-selection (when selection (js/JSON.stringify
                                     #js {:owner (pr-str [id (:fork-id ec/*execution-context*)])
                                          :selection (clj->js selection)}))
    :data-read-only (str (boolean read-only?))
    :data-document (str (boolean document?))
    :on-input (fn [event]
                (when (and (= (.-target event) (.-currentTarget event)) (.-detail event) (not read-only?))
                  (if (aget (.-detail event) "blocked")
                    (when on-blocked (on-blocked))
                    (on-change (js->clj (.. event -detail -model) :keywordize-keys true)
                               (js->clj (.. event -detail -selection) :keywordize-keys true)))))}))

(defn command! [event operation] (controls/command! event operation))
