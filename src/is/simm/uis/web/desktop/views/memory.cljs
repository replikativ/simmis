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
            [is.simm.uis.web.desktop.views.memory-navigation :as navigation]
            [is.simm.uis.web.desktop.download-link :as download])
  (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]]))

(declare model-node)

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

(defn- source-view [path draft a write?]
  (source/view {:id [(:owner a) path] :path path :label "Memory file source"
                :value (:text draft) :selection (:selection draft) :read-only? (not write?)
                :on-change (fn [text selection] ((:change a) path editor/changed text selection))}))

(defn- rich-toolbar [write?]
  (el/div {:class "memory-toolbar memory-document-toolbar"}
                    (for [[op label] [[:bold "Bold"] [:italic "Italic"] [:strike "Strike"]
                                     [:paragraph "Paragraph"] [:heading "Heading"]
                                     [:bullet-list "Bullets"] [:ordered-list "Numbered"] [:code "Code"]]]
                      (el/button {:key op :disabled (not write?)
                                  :on-mousedown #(.preventDefault %)
                                  :on-click #(rich-editor/command! % op)} label)))
            )

(defn- document-view [path draft a write?]
  (let [base (:rich-base draft) analysis (or (:analysis base) (:analysis draft))
        source (or (:text base) (:text draft))
        rich? (and (= :rich (:mode draft)) base)]
    (el/div {:class "memory-document"}
            (when rich? (rich-toolbar write?))
            (when (and (= :rich (:mode draft)) (not base) analysis)
              (el/p {:class "memory-note"} "This document needs Source editing for its size or structure. Its original source is retained."))
            (when (seq (:frontmatter analysis))
              (el/details {:class "memory-properties"}
                          (el/summary {} "Document properties")
                          (for [[key value] (editor/property-entries (get-in analysis [:frontmatter :properties]))]
                            (el/div {:key (pr-str key)}
                                    (el/strong {} (str (if (string? key) key (pr-str key)) ": ")) (el/code {} (pr-str value))))))
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
                                    (if-let [model (or (get-in base [:models (:from node)]) (rich/model node))]
                                      (model-node model)
                                      (el/pre {} (subs source (:from node) (:to node)))))
                                  (el/button {:on-click (fn [_]
                                                          ((:change a) path assoc :mode :source
                                                           :selection (editor/source-selection draft (:from block) (:to block))))}
                                             "Edit source"))))
                (for [slot (:slots analysis)]
                  (let [model (or (get-in base [:models (:from slot)]) (:model slot))
                        text (subs source (:from slot) (:to slot))
                        structural? (boolean (re-matches #"(?s)\s*<!-- /?simmis:block[^>]*-->\s*" text))]
                    (when-not structural?
                      (el/div {:key (str (:from slot) ":" (:to slot)) :class "memory-region"}
                              (cond
                                (and rich? model)
                                (rich-editor/view {:id [(:owner a) path (:id base) (:from slot)] :model model
                                                   :selection (get-in base [:selections (:from slot)])
                                                   :read-only? (not write?)
                                                   :on-change (fn [model selection]
                                                                ((:rich-change a) path (:id base) (:from slot) model selection))})
                                model (model-node model)
                                :else (el/div {:class "memory-source-region"}
                                              (el/small {} (str (name (:kind slot)) " · preserved source"))
                                              (el/pre {} text)))
                              (when (and (= :rich (:mode draft)) (nil? model))
                                (el/button {
                                            :on-click (fn [_]
                                                        ((:change a) path assoc :mode :source
                                                         :selection (editor/source-selection draft (:from slot) (:to slot))))}
                                           "Open in Source")))))))))))

(defn- view [data state a]
  (let [path (:selected state) draft (get-in state [:drafts path])
        write? (true? (get-in state [:permissions :write?]))
        markdown? (= :markdown (get-in draft [:source :kind]))
        text? (string? (:text draft))
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
                                                                             #(navigation/create-file! (:room-id data) (:room-name data) % (:col-id data))))} "New file"))
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
                    (el/div {:class "memory-detail"}
                            (if-not draft
                              (el/p {:class "memory-note"} "Choose a file under Memory in the left navigation. Markdown is knowledge wherever you put it; wiki/ is just a convenient folder.")
                              (el/div {}
                                      (el/nav {:class "memory-breadcrumbs" :aria-label "File path"}
                                              (el/button {:on-click (fn [_] (navigation/focus-folder! (:room-id data) ""))}
                                                         (str (or (:room-name data) "Room") " / "))
                                              (for [[i part] (map-indexed vector (str/split path #"/"))]
                                                (if (< i (dec (count (str/split path #"/"))))
                                                  (el/button {:key i :on-click (fn [_] (navigation/focus-folder! (:room-id data)
                                                                                                               (str/join "/" (take (inc i) (str/split path #"/")))))} (str part " / "))
                                                  (el/span {:key i} part))))
                                      (when-let [content (get-in draft [:source :entry :content])]
                                        (el/details {:class "memory-properties"}
                                                    (el/summary {} "Saved file revision")
                                                    (el/code {} (str content))))
                                      (el/div {:class "memory-toolbar"}
                                              (el/span {} (if (editor/dirty? draft) "Unsaved draft" "Saved version"))
                                              (when text?
                                                (el/button {:disabled (or (not write?) (get-in state [:loading [:save path]])
                                                                         (not (editor/dirty? draft)))
                                                            :on-click (fn [_] ((:save a) path))} "Save"))
                                              (el/button {:on-click (fn [_] ((:reload a) path))} "Check for changes")
                                              (when (get-in draft [:source :entry])
                                                (el/button {:on-click (fn [_] ((:download a) path))} "Download"))
                                              (when (= path (get-in state [:download :path]))
                                                (download/view [(:owner a) path] (last (str/split path #"/"))
                                                               (get-in state [:download :base64])))
                                              (when write?
                                                (el/button {:disabled (editor/dirty? draft)
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
                                                              :on-click (fn [_] ((:mode a) path mode))} label))))
                                      (when (and text? (not markdown?))
                                        (el/p {:class "memory-note"} "Saving changes the versioned room file only. It does not evaluate code, reload an agent, or grant capabilities."))
                                      (when (and text? (re-find #"(?i)\.(clj|cljc|bb)$" path))
                                        (el/div {:class "memory-evaluation"}
                                                (el/p {:class "memory-note"} "Target: your room-scoped SCI session · separate from agents · no room tools injected. Definitions persist until the room closes or the server restarts. Evaluates the current draft, not necessarily the saved file.")
                                                (for [[scope label] [[:selection "Evaluate selection"] [:form "Evaluate top-level form"] [:file "Evaluate file"]]]
                                                  (el/button {:key scope :disabled (or (not (get-in state [:permissions :evaluate?]))
                                                                                                      (get-in state [:loading [:evaluate path]])
                                                                                                      (and (= scope :selection) (= (get-in draft [:selection :anchor]) (get-in draft [:selection :head]))))
                                                              :on-mousedown #(.preventDefault %)
                                                              :on-click (fn [_] ((:evaluate a) path scope))} label))
                                                (when (get-in state [:loading [:evaluate path]]) (el/p {:role "status"} "Evaluating…"))
                                                (when-let [result (get-in state [:evaluations path])]
                                                  (el/details {:open true :class "memory-evaluation-result"}
                                                              (el/summary {} (str (if (:success result) "Result" "Evaluation failed") " · " (:path result)
                                                                                                (when (not= (:text draft) (:input-text result)) " · draft changed since evaluation")))
                                                              (el/pre {} (if (:success result) (:value result) (:error result)))
                                                              (when (seq (:stdout result)) (el/pre {} (str "stdout\n" (:stdout result))))
                                                              (when (seq (:stderr result)) (el/pre {} (str "stderr\n" (:stderr result))))
                                                              (el/details {} (el/summary {} "Evaluated source") (el/pre {} (:code result)))))))
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
     ((:ensure-path a))
     (view data state a))))

(defn render-context [data]
  (spin
   (let [states (iv/get-new (track sig/memory-editor-state))
         a (actions/controller data) state (get states (:owner a)) path (:selected state)
         history (get-in state [:history path]) preview (get-in state [:history-preview path])]
     (el/div {:class "memory-history context-section"}
             (el/div {:class "context-section-title"} "File history")
             (when path
               (el/div {}
                       (el/button {:disabled (get-in state [:loading [:history path]])
                                   :on-click (fn [_] ((:history a) path))} "Refresh history")
                       (el/p {:class "memory-note"} "Current branch · 31-commit ancestry window, including merges · renames are separate paths. Preview does not replace your draft.")
                       (for [version (:versions history)
                             :let [message (or (first (str/split-lines (or (:message version) ""))) "")
                                   message (subs message 0 (min 140 (count message)))]]
                         (el/button {:key (str (:commit version)) :class "memory-history-entry"
                                     :on-click (fn [_] ((:history-file a) path (:commit version)))}
                                    (str (:time version) " · " message
                                         (when (:boundary? version) " · snapshot boundary (parent unavailable)")
                                         (when-not (:entry version) (if (:boundary? version) " · absent" " · deleted")))))
                       (when (and history (empty? (:versions history))) (el/p {} "No file changes in this history window."))
                       (when (:more? history) (el/p {} "Older commits exist outside this window."))
                       (when (seq (:unavailable history))
                         (el/p {} (str (count (:unavailable history)) " commits have no materialized snapshot; their file changes cannot be previewed here.")))
                       (when-let [error (or (get-in state [:errors [:history path]]) (get-in state [:errors [:history-file path]]))]
                         (el/p {:role "alert"} error))
                       (when preview
                         (el/div {}
                                 (el/p {} (str "Read-only revision " (:commit preview)))
                                 (if (:deleted? preview) (el/p {} "File was deleted in this revision.")
                                   (source/view {:id [(:owner a) :history path (:commit preview)] :path path
                                                 :value (:text preview) :read-only? true :label "Historical file source"}))))))))))
