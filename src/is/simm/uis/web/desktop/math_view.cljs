(ns is.simm.uis.web.desktop.math-view
  "Element-owned formula projection. Each render has its own macro scope;
   opening a formula never executes code or changes document/editor state."
  (:require ["katex" :as katex]
            [org.replikativ.spindel.dom.foreign])
  (:require-macros [org.replikativ.spindel.dom.foreign :refer [foreign-node]]))

(defn paint! [^js element tex raw display?]
  (.replaceChildren element)
  (set! (.-className element) (if display? "memory-math memory-math-display" "memory-math"))
  (.setAttribute element "title" "Formula · edit its Markdown source to change it")
  (try
    (when-not (and (string? tex) (<= (count tex) 8192))
      (throw (js/Error. "Formula exceeds the projection limit")))
    (katex/render tex element #js {:displayMode (boolean display?) :trust false
                                  :output "htmlAndMathml" :throwOnError true :strict "error"
                                  :maxSize 20 :maxExpand 200 :globalGroup false
                                  :macros (js/Object.create nil)})
    (catch :default _
      (.replaceChildren element (.createTextNode js/document (or raw "")))
      (.setAttribute element "aria-label" "Formula source; preview unavailable"))))

(defn- sync! [^js element]
  (.removeAttribute element "aria-label")
  (paint! element (.getAttribute element "data-tex") (.getAttribute element "data-raw")
          (= "true" (.getAttribute element "data-display"))))

(when-not (.get js/customElements "simmis-math")
  (.define js/customElements "simmis-math"
           (js* "(class extends HTMLElement {
             connectedCallback() { (~{})(this); }
             disconnectedCallback() { this.replaceChildren(); }
             attributeChangedCallback() { if (this.isConnected) (~{})(this); }
             static get observedAttributes() { return ['data-tex', 'data-raw', 'data-display']; }
           })" sync! sync!)))

(defn view [node]
  (foreign-node {:tag :simmis-math :key (get-in node [:attrs :sourceKey])
                 :data-tex (get-in node [:attrs :tex]) :data-raw (get-in node [:attrs :raw])
                 :data-display (str (= "displayMath" (:type node)))}))
