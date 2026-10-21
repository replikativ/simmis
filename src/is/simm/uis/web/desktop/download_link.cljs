(ns is.simm.uis.web.desktop.download-link
  "Object URLs belong to DOM devices, never fork-copied application state."
  (:require [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.dom.foreign])
  (:require-macros [org.replikativ.spindel.dom.foreign :refer [foreign-node]]))

(defn disconnect! [^js el]
  (when-let [url (.-simmisDownloadUrl el)]
    (js/URL.revokeObjectURL url)
    (set! (.-simmisDownloadUrl el) nil))
  (.replaceChildren el))

(defn mount! [^js el]
  (disconnect! el)
  (when-let [payload (.getAttribute el "data-bytes")]
    (let [binary (js/atob payload) bytes (js/Uint8Array. (count binary))
          ^js link (.createElement js/document "a")]
      (dotimes [i (count binary)] (aset bytes i (.charCodeAt binary i)))
      (let [url (js/URL.createObjectURL (js/Blob. #js [bytes] #js {:type "application/octet-stream"}))]
        (set! (.-simmisDownloadUrl el) url)
        (set! (.-href link) url)
        (set! (.-download link) (.getAttribute el "data-name"))
        (set! (.-textContent link) (str "Save " (.getAttribute el "data-name")))
        (.appendChild el link)))))

(when-not (.get js/customElements "simmis-download")
  (.define js/customElements "simmis-download"
           (js* "(class extends HTMLElement {
             connectedCallback() { (~{})(this); }
             disconnectedCallback() { (~{})(this); }
             attributeChangedCallback(n, a, b) { if(this.isConnected && a !== b) (~{})(this); }
             static get observedAttributes() { return ['data-bytes', 'data-name', 'data-owner']; }
           })" mount! disconnect! mount!)))

(defn view [id name base64]
  (foreign-node {:tag :simmis-download :key id :data-name name :data-bytes base64
                 :data-owner (pr-str [id (:fork-id ec/*execution-context*)])}))
