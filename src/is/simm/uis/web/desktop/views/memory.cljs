(ns is.simm.uis.web.desktop.views.memory
  "One room file tree and source-preserving document views. Reading and saving
   code never evaluates it or changes a participant's SCI interpreter."
  (:require [clojure.string :as str]
            [org.replikativ.spindel.dom.elements :as el]
            [org.replikativ.spindel.incremental.interval :as iv]
            [org.replikativ.spindel.effects.track :refer [track]]
            [is.simm.documents.rich-text :as rich]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.memory-actions :as actions]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.uis.web.desktop.source-editor :as source]
            [is.simm.uis.web.desktop.rich-editor :as rich-editor]
            [is.simm.uis.web.desktop.download-link :as download])
  (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]]))

(declare tree model-node)

(defn- children [node] (mapv model-node (:content node)))

(defn- marked-text [node]
  (reduce (fn [value mark]
            (case (:type mark)
              "bold" (el/strong {} value)
              "italic" (el/em {} value)
              "strike" (el/span {:style {:text-decoration "line-through"}} value)
              "code" (el/code {} value)
              "link" (let [href (get-in mark [:attrs :href])]
                       (if (rich/safe-link? href)
                         (el/a {:href href :target "_blank" :rel "noopener noreferrer"} value)
                         value))
              value)) (:text node) (:marks node)))

(defn- model-node [node]
  ;; Deliberately no innerHTML, scripts, embedded images, or widget execution.
  (let [content (children node)]
    (case (:type node)
      "doc" (el/div {} content)
      "text" (marked-text node)
      "paragraph" (el/p {} content)
      "heading" (case (get-in node [:attrs :level])
                  1 (el/h1 {} content) 2 (el/h2 {} content) 3 (el/h3 {} content)
                  4 (el/h4 {} content) 5 (el/h5 {} content) (el/h6 {} content))
      "bulletList" (el/ul {} content)
      "orderedList" (el/ol {:start (get-in node [:attrs :start] 1)} content)
      "listItem" (el/li {} content)
      "codeBlock" (el/pre {} (el/code {} content))
      "hardBreak" (el/br {})
      "horizontalRule" (el/hr {})
      nil)))

(defn- tree [folder state a depth]
  (el/div {:class "memory-tree-branch"}
          (for [{:keys [path name kind]} (get-in state [:folders folder :entries])
                :when (not= name ".keep")]
            (let [folder? (= kind :folder) expanded? (get-in state [:expanded path])]
              (el/div {:key path}
                      (el/button {:class (str "memory-tree-item" (when (= path (:selected state)) " selected"))
                                  :style {:padding-left (str (+ 10 (* 14 depth)) "px")}
                                  :title path
                                  :aria-expanded (when folder? (str (boolean expanded?)))
                                  :on-click (fn [_] ((if folder? (:toggle a) (:open a)) path))}
                                 (str (if folder? (if expanded? "▾ " "▸ ") "") name))
                      (when expanded?
                        (if (get-in state [:folders path])
                          (tree path state a (inc depth))
                          (el/p {:class "memory-note"} "Loading…"))))))))

(defn- source-view [path draft a write?]
  (source/view {:id [(:owner a) path] :path path :label "Memory file source"
                :value (:text draft) :selection (:selection draft) :read-only? (not write?)
                :on-change (fn [text selection] ((:change a) path editor/changed text selection))}))

(defn- region-editor [path draft a write?]
  (let [{:keys [token model selection]} (:rich-edit draft)]
    (el/div {:class "memory-region-editor"}
            (el/div {:class "memory-toolbar"}
                    (for [[op label] [[:bold "Bold"] [:italic "Italic"] [:strike "Strike"]
                                     [:paragraph "Paragraph"] [:heading "Heading"]
                                     [:bullet-list "Bullets"] [:ordered-list "Numbered"] [:code "Code"]]]
                      (el/button {:key op :disabled (not write?)
                                  :on-mousedown #(.preventDefault %)
                                  :on-click #(rich-editor/command! % op)} label)))
            (rich-editor/view {:id [(:owner a) path token] :model model :selection selection :read-only? (not write?)
                               :on-change (fn [model selection] ((:change-rich a) path token model selection))})
            (el/div {:class "memory-toolbar"}
                    (el/button {:disabled (not write?) :on-click (fn [_] ((:apply-rich a) path))} "Apply to draft")
                    (el/button {:on-click (fn [_] ((:cancel-rich a) path))} "Cancel region edit")))))

(defn- document-view [path draft a write?]
  (let [analysis (:analysis draft) active (:rich-edit draft)]
    (el/div {:class "memory-document"}
            (when (seq (:frontmatter analysis))
              (el/details {:class "memory-properties"}
                          (el/summary {} "Document properties")
                          (for [[key value] (sort-by key (get-in analysis [:frontmatter :properties]))]
                            (el/div {:key key}
                                    (el/strong {} (str key ": ")) (el/code {} (pr-str value))))))
            (when (seq (:diagnostics analysis))
              (el/p {:class "proposal-error"} "Some document structure needs attention. Source preserves it exactly."))
            (if-not analysis
              (el/button {:on-click (fn [_] ((:analyze a) path))} "Analyze current draft")
              (if (= :outline (:mode draft))
                (el/div {:class "memory-outline"}
                        (for [block (editor/outline (get-in analysis [:semantic :blocks]))]
                          (el/div {:key (str (:id block)) :class "memory-outline-block"
                                   :style {:margin-left (str (* 18 (:depth block)) "px")}}
                                  (el/details {:class "memory-properties"}
                                              (el/summary {} "Block identity")
                                              (el/code {} (str (name (:identity-status block)) " · " (:id block))))
                                  (for [node (:syntax block)]
                                    (if-let [model (rich/model node)]
                                      (model-node model)
                                      (el/pre {} (subs (:text draft) (:from node) (:to node)))))
                                  (el/button {:on-click (fn [_]
                                                          ((:change a) path assoc :mode :source
                                                           :selection {:anchor (:from block) :head (:to block)}))}
                                             "Edit source"))))
                (for [slot (:slots analysis)]
                  (let [selected? (and active (= (:from slot) (get-in active [:slot :from])))
                        text (subs (:text draft) (:from slot) (:to slot))
                        structural? (boolean (re-matches #"(?s)\s*<!-- /?simmis:block[^>]*-->\s*" text))]
                    (when-not structural?
                      (el/div {:key (str (:from slot) ":" (:to slot)) :class "memory-region"}
                              (cond
                                selected? (region-editor path draft a write?)
                                (:model slot) (model-node (:model slot))
                                :else (el/div {:class "memory-source-region"}
                                              (el/small {} (str (name (:kind slot)) " · preserved source"))
                                              (el/pre {} text)))
                              (when (and (= :rich (:mode draft)) (not active))
                                (el/button {:disabled (and (:model slot) (not write?))
                                            :on-click (fn [_]
                                                        (if (:model slot)
                                                          ((:start-rich a) path slot)
                                                          ((:change a) path assoc :mode :source
                                                           :selection {:anchor (:from slot) :head (:to slot)})))}
                                           (if (:model slot) "Edit region" "Open in Source"))))))))))))

(defn- view [data state a]
  (let [path (:selected state) draft (get-in state [:drafts path])
        write? (true? (get-in state [:permissions :write?]))
        markdown? (= :markdown (get-in draft [:source :kind]))
        text? (string? (:text draft)) rich? (:rich-edit draft)
        office? (= :office (get-in draft [:source :kind])) office (:office draft)
        folder (or (:current-folder state) "")
        ask-path (fn [label default action]
                   (when-let [p (js/prompt label default)]
                     (when-not (str/blank? p) (action p))))]
    (el/div {:class "memory-panel"}
            (el/div {:class "memory-toolbar"}
                    (el/h2 {} (str (or (:room-name data) "Room") " · Memory"))
                    (el/button {:on-click (fn [_] ((:refresh a)))} "Refresh")
                    (when write?
                      (el/button {:on-click (fn [_] (ask-path "New file path (relative to the room)"
                                                                             (if (seq folder) (str folder "/note.md") "wiki/note.md")
                                                                             (:create a)))} "New file"))
                    (when write?
                      (el/button {:on-click (fn [_] (ask-path "New folder path (relative to the room)"
                                                            (str (when (seq folder) (str folder "/")) "new-folder")
                                                            (:mkdir a)))} "New folder"))
                    (when write?
                      (el/label {:class "memory-upload"} "Upload file"
                                (el/input {:type "file" :aria-label "Upload to Memory"
                                           :on-change (fn [e]
                                                        (when-let [file (aget (.. e -target -files) 0)]
                                                          ((:upload a) folder file))
                                                        (set! (.. e -target -value) ""))}))))
            (for [[slot error] (:errors state)]
              (el/p {:key (pr-str slot) :class "proposal-error" :role "alert"} error))
            (el/div {:class "memory-layout"}
                    (el/aside {:class "memory-tree" :aria-label "Room files"}
                              (el/button {:class "memory-tree-item" :on-click (fn [_] ((:root a)))} "Room files /")
                              (tree "" state a 0))
                    (el/div {:class "memory-detail"}
                            (if-not draft
                              (el/p {:class "memory-note"} "Select a file. Markdown is knowledge wherever you put it; wiki/ is just a convenient folder. Code is shared source, not a shared interpreter.")
                              (el/div {}
                                      (el/h3 {} path)
                                      (when-let [content (get-in draft [:source :entry :content])]
                                        (el/details {:class "memory-properties"}
                                                    (el/summary {} "Saved file revision")
                                                    (el/code {} (str content))))
                                      (el/div {:class "memory-toolbar"}
                                              (el/span {} (if (or (editor/dirty? draft) rich?) "Unsaved draft" "Saved version"))
                                              (when text?
                                                (el/button {:disabled (or (not write?) rich? (get-in state [:loading [:save path]])
                                                                         (not (editor/dirty? draft)))
                                                            :on-click (fn [_] ((:save a) path))} "Save"))
                                              (el/button {:on-click (fn [_] ((:reload a) path))} "Check saved version")
                                              (when (get-in draft [:source :entry])
                                                (el/button {:on-click (fn [_] ((:download a) path))} "Download"))
                                              (when (= path (get-in state [:download :path]))
                                                (download/view [(:owner a) path] (last (str/split path #"/"))
                                                               (get-in state [:download :base64])))
                                              (when write?
                                                (el/button {:disabled (or (editor/dirty? draft) rich?)
                                                            :on-click (fn [_] (ask-path "Move file to" path #((:move a) path %)))} "Move")))
                                      (when (:incoming draft)
                                        (el/div {:class "memory-conflict" :role "status"}
                                                (el/p {} "The saved file changed. Your draft is retained; saving it will refuse the stale version.")
                                                (el/button {:on-click (fn [_] ((:use-incoming a) path))} "Discard draft and use saved version")))
                                      (when (:earlier-save-receipt draft)
                                        (el/p {:class "memory-note"} "An earlier draft's save completed after this version was selected. This draft was kept unchanged; check the saved version before another save."))
                                      (when text?
                                        (el/div {:class "memory-toolbar" :aria-label "Document views"}
                                                (for [[mode label] (if markdown? [[:read "Read"] [:rich "Rich text"] [:outline "Outline"] [:source "Source"]]
                                                                      [[:source "Code / text"]])]
                                                  (el/button {:key mode :class (when (= mode (:mode draft)) "selected")
                                                              :disabled (boolean rich?)
                                                              :on-click (fn [_] ((:mode a) path mode))} label))))
                                      (when (and text? (not markdown?))
                                        (el/p {:class "memory-note"} "Saving changes the versioned room file only. It does not evaluate code, reload an agent, or grant capabilities."))
                                      (cond
                                        office?
                                        (el/div {:class "memory-office"}
                                                (el/p {:class "memory-note"} "Office container · advanced XML part editor, not a Word/Excel layout editor. Only the selected part changes; other part contents are preserved. Signed documents may require re-signing after edits.")
                                                (if-not office
                                                  (el/button {:on-click (fn [_] ((:office-open a) path nil))} "Inspect document parts")
                                                  (el/div {}
                                                          (el/select {:aria-label "Office XML part" :value (:part office)
                                                                      :disabled (or (editor/dirty? draft) (get-in state [:loading [:office-save path]]))
                                                                      :on-change (fn [e] ((:office-open a) path (.. e -target -value)))}
                                                                     (for [part (:parts office) :when (:editable? part)]
                                                                       (el/option {:key (:name part) :value (:name part)} (:name part))))
                                                          (source/view {:id [(:owner a) path (:part office)] :path (:part office)
                                                                        :label "Office XML part source" :value (:text office)
                                                                        :selection (:selection office) :read-only? (not write?)
                                                                        :on-change (fn [text selection]
                                                                                     ((:change a) path update :office assoc :text text :selection selection))})
                                                          (el/button {:disabled (or (not write?) (not (editor/dirty? draft))
                                                                                   (get-in state [:loading [:office-save path]]))
                                                                      :on-click (fn [_] ((:office-save a) path))} "Save XML part"))))
                                        (not text?) (el/p {:class "memory-note"} "Binary file. Download preserves the original bytes; document-specific editing is not available in this view yet.")
                                        (= :source (:mode draft)) (source-view path draft a write?)
                                        :else (document-view path draft a write?)))))))))

(defn render [data]
  (spin
   (let [states (iv/get-new (track sig/memory-editor-state))
         a (actions/controller data) state (get states (:owner a))]
     ((:ensure-root a))
     (view data state a))))
