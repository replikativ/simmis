(ns is.simm.uis.web.desktop.source-preview
  "Disposable, inert views of parsed Markdown that remains source-edited."
  (:require [is.simm.documents.rich-text :as rich]
            [is.simm.uis.web.desktop.code-highlight :as highlight]
            [org.replikativ.spindel.dom.foreign])
  (:require-macros [org.replikativ.spindel.dom.foreign :refer [foreign-node]]))

(def ^:private tags #{:p :span :br :strong :em :s :code :pre :a :blockquote
                     :ul :ol :li :h1 :h2 :h3 :h4 :h5 :h6
                     :table :thead :tbody :tr :th :td :caption :img})

(defn- internal-media? [value]
  (and (string? value)
       (boolean (re-matches #"/blobs/workspaces/[0-9a-f-]{36}\?path=[^\s<>\"#]+&content=[0-9a-f-]{36}&target=[0-9a-f-]{36}" value))))

(defn- native-node [scene]
  (if (string? scene)
    (.createTextNode js/document scene)
    (let [{:keys [tag attrs children]} scene tag (keyword tag)]
      (when-not (contains? tags tag) (throw (js/Error. "Unsupported Markdown display element")))
      (let [dom (.createElement js/document (name tag))]
        (doseq [[key value] attrs]
          (when (and (contains? #{:class :scope :start :href :rel :target :data-language :src :alt :loading} key)
                     (some? value) (or (not= :href key) (rich/safe-link? value))
                     (or (not= :src key) (and (= :img tag) (internal-media? value))))
            (.setAttribute dom (name key) (str value))))
        (doseq [child children] (.appendChild dom (native-node child)))
        (when (and (= :code tag) (contains? attrs :data-language))
          (let [parts (highlight/segments (.-textContent dom) (:data-language attrs))]
            (.replaceChildren dom)
            (doseq [{:keys [text class]} parts]
              (if class
                (let [span (.createElement js/document "span")]
                  (set! (.-className span) class) (set! (.-textContent span) text) (.appendChild dom span))
                (.appendChild dom (.createTextNode js/document text))))))
        dom))))

(defn paint! [dom projection]
  (.replaceChildren dom)
  (set! (.-className dom) "memory-markdown-preview")
  (when-let [scene (:scene projection)] (.appendChild dom (native-node scene))))

(defn- index-of [el]
  (let [raw (.getAttribute el "data-source-previews") cached (.-simmisSourcePreviews el)]
    (if (and cached (= raw (.-raw cached))) (.-value cached)
        (let [value (when raw (js->clj (js/JSON.parse raw) :keywordize-keys true))]
          (set! (.-simmisSourcePreviews el) #js {:raw raw :value value}) value))))

(defn refresh! [el]
  (when-let [views (.-simmisSourcePreviewViews el)] (.forEach views (fn [paint] (paint)))))

(defn node-view [el]
  (fn [props]
    (let [dom (.createElement js/document "div") node (volatile! (.-node props)) last-value (volatile! ::none)
          paint (fn []
                  (let [attrs (js->clj (js/JSON.parse (js/JSON.stringify (.-attrs @node))) :keywordize-keys true)
                        projection (get (index-of el) (keyword (:key attrs)))
                        projection (when (= (:label attrs) (:source projection)) projection)
                        value [(:label attrs) projection]]
                    (when-not (= value @last-value)
                      (vreset! last-value value)
                      (if projection
                        (paint! dom projection)
                        (let [pre (.createElement js/document "pre")]
                          (.replaceChildren dom) (set! (.-className dom) "memory-opaque")
                          (set! (.-textContent pre) (:label attrs)) (.appendChild dom pre))))))]
      (.setAttribute dom "contenteditable" "false")
      (when-not (.-simmisSourcePreviewViews el) (set! (.-simmisSourcePreviewViews el) (js/Set.)))
      (.add (.-simmisSourcePreviewViews el) paint)
      (paint)
      #js {:dom dom :update (fn [next-node]
                             (when (= "memoryOpaque" (.. next-node -type -name))
                               (vreset! node next-node) (paint) true))
           :stopEvent (fn [_] true) :ignoreMutation (fn [_] true)
           :destroy (fn [] (.delete (.-simmisSourcePreviewViews el) paint) (.replaceChildren dom))})))

(defn- sync! [el]
  (paint! el (js->clj (js/JSON.parse (or (.getAttribute el "data-preview") "null")) :keywordize-keys true)))

(when-not (.get js/customElements "simmis-markdown-preview")
  (.define js/customElements "simmis-markdown-preview"
           (js* "(class extends HTMLElement {
             connectedCallback() { (~{})(this); }
             attributeChangedCallback() { if (this.isConnected) (~{})(this); }
             static get observedAttributes() { return ['data-preview']; }
           })" sync! sync!)))

(defn view [id projection]
  (foreign-node {:tag :simmis-markdown-preview :key id
                 :data-preview (js/JSON.stringify (clj->js projection))}))
