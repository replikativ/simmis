(ns is.simm.uis.web.desktop.source-editor
  "A controlled CodeMirror foreign view. Source/selection live in caller signals;
   native editor handles live only on their own DOM element, never in a global
   registry or a forked computation. Disconnect releases the native editor."
  (:require ["@codemirror/state" :refer [EditorState Compartment]]
            ["@codemirror/view" :refer [EditorView keymap lineNumbers drawSelection]]
            ["@codemirror/commands" :refer [defaultKeymap history historyKeymap]]
            ["@codemirror/language" :refer [StreamLanguage syntaxHighlighting defaultHighlightStyle]]
            ["@codemirror/lang-markdown" :refer [markdown]]
            ["@codemirror/lang-json" :refer [json]]
            ["@codemirror/legacy-modes/mode/clojure" :refer [clojure]]
            [org.replikativ.spindel.engine.core :as ec]
            [clojure.string :as str])
  (:require-macros [org.replikativ.spindel.dom.foreign :refer [foreign-node]]))

(defn language [path]
  (let [path (str/lower-case (or path ""))]
    (cond
      (re-find #"\.(md|markdown)$" path) "markdown"
      (re-find #"\.(clj|cljs|cljc|edn|bb)$" path) "clojure"
      (str/ends-with? path ".json") "json"
      :else "text")))

(defn- language-extension [name]
  (case name
    "markdown" (markdown)
    "clojure" (.define StreamLanguage clojure)
    "json" (json)
    #js []))

(defn- source [^js el] (or (.getAttribute el "data-source") ""))
(defn- readonly? [^js el] (= "true" (.getAttribute el "data-read-only")))

(defn- selection [^js el length]
  (let [raw (.getAttribute el "data-selection")
        ^js value (when raw (js/JSON.parse raw))
        clamp #(max 0 (min length (if (number? %) % 0)))]
    #js {:anchor (clamp (some-> value .-anchor)) :head (clamp (some-> value .-head))}))

(defn- notify! [^js el ^js update]
  (when (and (identical? (.-view update) (.-simmisEditorView el))
             (not (.-simmisApplying el))
             (or (.-docChanged update) (.-selectionSet update)))
    (let [^js state (.-state update) ^js main (.. state -selection -main)]
      (.dispatchEvent el
                      (js/CustomEvent.
                       "input" #js {:bubbles true :composed true
                                    :detail #js {:source (.toString (.-doc state))
                                                 :selection #js {:anchor (.-anchor main)
                                                                 :head (.-head main)}}})))))

(defn- editor-state! [^js el]
  (let [text (source el)
          ^js lang (Compartment.)
          ^js permissions (Compartment.)
          state (.create EditorState
                         #js {:doc text :selection (selection el (count text))
                              :extensions
                              #js [;; Keep CR characters in mixed/CRLF documents;
                                   ;; never normalize source merely by opening it.
                                   (.of (.-lineSeparator EditorState) "\n")
                                   (history) (lineNumbers) (drawSelection)
                                   (.of keymap (.concat
                                                #js [#js {:key "Mod-Enter"
                                                          :run (fn [_]
                                                                 (if (= "true" (.getAttribute el "data-evaluate"))
                                                                   (do (.dispatchEvent el (js/CustomEvent. "input" #js {:bubbles true :composed true :detail #js {:evaluate true}})) true)
                                                                   false))}]
                                                defaultKeymap historyKeymap))
                                   (syntaxHighlighting defaultHighlightStyle)
                                   (.of lang (language-extension (.getAttribute el "data-language")))
                                   (.of permissions #js [(.of (.-readOnly EditorState) (readonly? el))
                                                        (.of (.-editable EditorView) (not (readonly? el)))])
                                   (.of (.-contentAttributes EditorView)
                                        #js {:aria-label (or (.getAttribute el "aria-label") "File source")
                                             :spellcheck "false"})
                                   (.of (.-updateListener EditorView) #(notify! el %))]})]
      (set! (.-simmisLanguage el) lang)
      (set! (.-simmisPermissions el) permissions)
      state))

(defn mount! [^js el]
  (when-not (.-simmisEditorView el)
    (set! (.-simmisEditorView el)
          (EditorView. #js {:state (editor-state! el) :parent el}))))

(defn disconnect! [^js el]
  (when-let [^js view (.-simmisEditorView el)]
    (.destroy view)
    (set! (.-simmisEditorView el) nil)
    (set! (.-simmisLanguage el) nil)
    (set! (.-simmisPermissions el) nil)))

(defn sync! [^js el attribute old new]
  (when (not= old new)
    (when-let [^js view (.-simmisEditorView el)]
      (set! (.-simmisApplying el) true)
      (try
        (case attribute
          "data-source"
          (let [text (source el)]
            (when (not= text (.toString (.. view -state -doc)))
              ;; An authoritative replacement (reload, rollback, remote source)
              ;; is NOT a local edit. A fresh state prevents native undo from
              ;; reintroducing text from the previous document revision.
              (.setState view (editor-state! el))))
          "data-owner"
          (let [focused? (.-hasFocus view)]
            ;; Different room/document/execution context: no shared native
            ;; handle or undo history. Draft source/selection come from props.
            (disconnect! el)
            (mount! el)
            (when focused? (.focus (.-simmisEditorView el))))
          "data-selection"
          (let [^js next (selection el (.. view -state -doc -length))
                ^js current (.. view -state -selection -main)]
            (when (or (not= (.-anchor next) (.-anchor current))
                      (not= (.-head next) (.-head current)))
              (.dispatch view #js {:selection next})))
          "data-language"
          (.dispatch view #js {:effects (.reconfigure (.-simmisLanguage el)
                                                     (language-extension new))})
          "data-read-only"
          (.dispatch view #js {:effects (.reconfigure (.-simmisPermissions el)
                                                     #js [(.of (.-readOnly EditorState) (readonly? el))
                                                          (.of (.-editable EditorView) (not (readonly? el)))])})
          nil)
        (finally (set! (.-simmisApplying el) false))))))

(defn- register! []
  (when-not (.get js/customElements "simmis-source-editor")
    ;; The custom element owns its native resource even when moved/unmounted:
    ;; Spindel's unmount callback intentionally receives nil. The class is only
    ;; a device adapter; all application data comes from controlled attributes.
    (.define js/customElements "simmis-source-editor"
             (js* "(class extends HTMLElement {
               connectedCallback() { (~{})(this); }
               disconnectedCallback() { (~{})(this); }
               attributeChangedCallback(name, oldValue, newValue) { (~{})(this, name, oldValue, newValue); }
               static get observedAttributes() { return ['data-owner', 'data-source', 'data-selection', 'data-language', 'data-read-only']; }
             })"
                  (fn [el] (mount! el)) (fn [el] (disconnect! el))
                  (fn [el a old new] (sync! el a old new))))))

(register!)

(defn view [{:keys [id path value selection read-only? on-change on-evaluate label]}]
  (foreign-node
   {:tag :simmis-source-editor :key id :class "source-editor"
    :data-owner (pr-str [id (:fork-id ec/*execution-context*)])
    :aria-label (or label "File source")
    :data-source (or value "") :data-language (language path)
    :data-selection (js/JSON.stringify (clj->js (or selection {:anchor 0 :head 0})))
    :data-read-only (str (boolean read-only?))
    :data-evaluate (str (boolean on-evaluate))
    :on-input (fn [^js event]
                ;; Ignore the contenteditable's native bubbling input event;
                ;; only our source/selection snapshot is application input.
                (when (and (= (.-target event) (.-currentTarget event))
                           (.-detail event) (not read-only?))
                  (let [^js detail (.-detail event)]
                    (if (.-evaluate detail)
                      (when on-evaluate (on-evaluate))
                      (on-change (.-source detail)
                                 {:anchor (.. detail -selection -anchor)
                                  :head (.. detail -selection -head)})))))}))
