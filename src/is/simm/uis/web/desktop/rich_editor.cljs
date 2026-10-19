(ns is.simm.uis.web.desktop.rich-editor
  "One controlled Tiptap region, no global handles or alternative draft state."
  (:require ["@tiptap/core" :refer [Editor]]
            ["@tiptap/starter-kit" :default StarterKit]
            [is.simm.documents.rich-text :as rich]
            [org.replikativ.spindel.dom.foreign]
            [org.replikativ.spindel.engine.core :as ec])
  (:require-macros [org.replikativ.spindel.dom.foreign :refer [foreign-node]]))

(defn- data [^js el]
  (js/JSON.parse (or (.getAttribute el "data-model") "{\"type\":\"doc\",\"content\":[]}")))

(defn- read-only? [^js el] (= "true" (.getAttribute el "data-read-only")))

(defn disconnect! [^js el]
  (when-let [^js editor (.-simmisRichEditor el)]
    (.destroy editor)
    (set! (.-simmisRichEditor el) nil)))

(defn- notify! [^js el ^js editor]
  (when (and (identical? editor (.-simmisRichEditor el)) (not (.-simmisApplying el)) (not (read-only? el)))
    (.dispatchEvent el
                    (js/CustomEvent. "input"
                                     #js {:bubbles true :composed true
                                          :detail #js {:model (.getJSON editor)
                                                       :selection #js {:from (.. editor -state -selection -from)
                                                                       :to (.. editor -state -selection -to)}}}))))

(defn mount! [^js el]
  (when-not (.-simmisRichEditor el)
    (let [^js editor (Editor.
                  #js {:element el :content (data el) :editable (not (read-only? el))
                       :extensions #js [(.configure StarterKit
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
        "data-model" (when-not (= (rich/normalize (js->clj (data el) :keywordize-keys true))
                                  (rich/normalize (js->clj (.getJSON editor) :keywordize-keys true)))
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

(defn view [{:keys [id model selection read-only? on-change]}]
  (foreign-node
   {:tag :simmis-rich-editor :key id :class "memory-rich-editor"
    :data-owner (pr-str [id (:fork-id ec/*execution-context*)])
    :data-model (js/JSON.stringify (clj->js model))
    :data-selection (when selection (js/JSON.stringify (clj->js selection)))
    :data-read-only (str (boolean read-only?))
    :on-input (fn [event]
                (when (and (= (.-target event) (.-currentTarget event)) (.-detail event) (not read-only?))
                  (on-change (js->clj (.. event -detail -model) :keywordize-keys true)
                             (js->clj (.. event -detail -selection) :keywordize-keys true))))}))

(defn command! [event operation]
  (when-let [container (some-> (.-target event) (.closest ".memory-document, .memory-region-editor"))]
    (when-let [el (.querySelector container "simmis-rich-editor:focus-within")]
    (when-let [editor (.-simmisRichEditor el)]
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
