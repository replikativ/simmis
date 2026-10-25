(ns is.simm.uis.web.desktop.file-links
  "Link MarkViews resolve relative room paths without changing source marks."
  (:require ["@tiptap/extension-link" :default Link]
            [is.simm.documents.attachments :as attachments]
            [is.simm.documents.rich-text :as rich]))

(defn- base [host]
  (when-let [value (.getAttribute host "data-file-base")]
    (js->clj (js/JSON.parse value) :keywordize-keys true)))

(defn- paint! [host dom raw]
  (let [href (when (rich/safe-link? raw) (attachments/resolve-link (base host) raw))]
    (if (and href (rich/safe-link? href)) (.setAttribute dom "href" href) (.removeAttribute dom "href"))))

(defn refresh! [host]
  (doseq [dom (array-seq (.querySelectorAll host "a[data-memory-source-href]"))]
    (paint! host dom (.getAttribute dom "data-memory-source-href"))))

(defn extension [host]
  (.configure
   (.extend Link
            #js {:addMarkView
                 (fn []
                   (fn [props]
                     (let [dom (.createElement js/document "a")
                           raw (.. props -mark -attrs -href)]
                       (.setAttribute dom "data-memory-source-href" (or raw ""))
                       (.setAttribute dom "target" "_blank")
                       (.setAttribute dom "rel" "noopener noreferrer")
                       (.setAttribute dom "title" (or (.. props -mark -attrs -title) "Ctrl/⌘-click to open link"))
                       (paint! host dom raw)
                       (set! (.-onclick dom)
                             (fn [event]
                               (when (and (or (.-ctrlKey event) (.-metaKey event)) (.hasAttribute dom "href"))
                                 (.preventDefault event)
                                 (js/window.open (.-href dom) "_blank" "noopener,noreferrer"))))
                       ;; ProseMirror's MarkView parseRule retains mark.attrs;
                       ;; display hrefs never become the edited/clipboard model.
                       #js {:dom dom :contentDOM dom
                            :ignoreMutation (fn [record] (and (= "attributes" (.-type record)) (identical? dom (.-target record))))
                            :destroy (fn [] (set! (.-onclick dom) nil))})))})
   #js {:openOnClick false :autolink false :linkOnPaste false :isAllowedUri rich/safe-link?}))
