(ns is.simm.uis.web.desktop.rich-editor
  "One controlled Tiptap region, no global handles or alternative draft state."
  (:require ["@tiptap/core" :refer [Editor Node Extension]]
            ["@tiptap/pm/state" :refer [Plugin]]
            ["@tiptap/starter-kit" :default StarterKit]
            [is.simm.documents.rich-text :as rich]
            [is.simm.documents.rich-document :as document]
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

(defn- boundary-node [name content]
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
                :renderHTML (fn [props]
                              (let [^js attrs (.. props -node -attrs)
                                    dom-attrs #js {:class (case name "memoryBlock" "memory-semantic-block"
                                                               "memoryRegion" "memory-continuous-region" "memory-opaque")
                                                  :data-block-id (.-blockId attrs)}]
                                (if (= name "memoryOpaque")
                                  #js ["pre" dom-attrs (or (.-label attrs) "")]
                                  #js ["div" dom-attrs 0])))}))

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
  (when-let [^js editor (.-simmisRichEditor el)]
    (.destroy editor)
    (set! (.-simmisRichEditor el) nil)))

(defn- notify! [^js el ^js editor]
  (when (and (identical? editor (.-simmisRichEditor el)) (not (.-simmisApplying el)) (not (read-only? el)))
    (.dispatchEvent el
                    (js/CustomEvent. "input"
                                     #js {:bubbles true :composed true
                                          :detail #js {:model (json-model (.getJSON editor))
                                                       :selection #js {:from (.. editor -state -selection -from)
                                                                       :to (.. editor -state -selection -to)}}}))))

(defn mount! [^js el]
  (when-not (.-simmisRichEditor el)
    (let [^js editor (Editor.
                  #js {:element el :content (data el) :editable (not (read-only? el))
                       :extensions #js [(boundary-node "memoryBlock" "(memoryBlock|memoryRegion|memoryOpaque)*")
                                        (boundary-node "memoryRegion" "(paragraph|heading|bulletList|orderedList|codeBlock|horizontalRule)+")
                                        (boundary-node "memoryOpaque" "")
                                        (boundary-guard el)
                                        (.configure StarterKit
                                                    #js {:underline false :blockquote false :trailingNode false
                                                         :link #js {:openOnClick false
                                                                   :autolink false :linkOnPaste false
                                                                   :isAllowedUri rich/safe-link?}})]
                       :editorProps #js {:attributes #js {:class "memory-rich-content"
                                                         :aria-label "Rich document region"}}
                       :onUpdate (fn [event] (notify! el (.-editor event)))
                       :onSelectionUpdate (fn [event] (notify! el (.-editor event)))})]
      (set! (.-simmisRichEditor el) editor)
      (when-let [raw (.getAttribute el "data-selection")]
        (let [selection (js/JSON.parse raw)]
          (try (.. editor -commands (setTextSelection selection)) (catch :default _ nil)))))))

(defn- sync-selection! [^js el ^js editor]
  (when-let [raw (.getAttribute el "data-selection")]
    (let [value (js/JSON.parse raw) max (.. editor -state -doc -content -size)
          clamp #(cljs.core/max 0 (min max (if (number? %) % 0)))]
      (.. editor -commands (setTextSelection #js {:from (clamp (.-from value)) :to (clamp (.-to value))})))))

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
        "data-read-only" (.setEditable editor (not (read-only? el)))
        "data-selection" (sync-selection! el editor)
        nil)
        (finally (set! (.-simmisApplying el) false))))))

(when-not (.get js/customElements "simmis-rich-editor")
  (.define js/customElements "simmis-rich-editor"
           (js* "(class extends HTMLElement {
             connectedCallback() { (~{})(this); }
             disconnectedCallback() { (~{})(this); }
             attributeChangedCallback(name, oldValue, newValue) { (~{})(this, name, oldValue, newValue); }
             static get observedAttributes() { return ['data-owner', 'data-model', 'data-read-only', 'data-selection']; }
           })" mount! disconnect! sync!)))

(defn view [{:keys [id model selection read-only? on-change document? on-blocked]}]
  (foreign-node
   {:tag :simmis-rich-editor :key id :class "memory-rich-editor"
    :data-owner (pr-str [id (:fork-id ec/*execution-context*)])
    :data-model (js/JSON.stringify (clj->js model))
    :data-selection (when selection (js/JSON.stringify (clj->js selection)))
    :data-read-only (str (boolean read-only?))
    :data-document (str (boolean document?))
    :on-input (fn [event]
                (when (and (= (.-target event) (.-currentTarget event)) (.-detail event) (not read-only?))
                  (if (aget (.-detail event) "blocked")
                    (when on-blocked (on-blocked))
                    (on-change (js->clj (.. event -detail -model) :keywordize-keys true)
                               (js->clj (.. event -detail -selection) :keywordize-keys true)))))}))

(defn command! [event operation]
  (when-let [container (some-> (.-target event) (.closest ".memory-document, .memory-region-editor"))]
    (when-let [^js el (.querySelector container "simmis-rich-editor:focus-within")]
    (when-let [^js editor (.-simmisRichEditor el)]
      (let [chain (.focus (.chain editor))]
        (case operation
          :bold (.run (.toggleBold chain))
          :italic (.run (.toggleItalic chain))
          :strike (.run (.toggleStrike chain))
          :bullet-list (.run (.toggleBulletList chain))
          :ordered-list (.run (.toggleOrderedList chain))
          :paragraph (.run (.setParagraph chain))
          :heading (.run (.toggleHeading chain #js {:level 2}))
          :code (.run (.toggleCode chain))
          nil))))))
