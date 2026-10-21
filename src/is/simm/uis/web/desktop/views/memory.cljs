(ns is.simm.uis.web.desktop.views.memory
  "One room file tree and source-preserving document views. Reading and saving
   code never evaluates it or changes a participant's SCI interpreter."
  (:require [clojure.string :as str]
            [org.replikativ.spindel.dom.elements :as el]
            [org.replikativ.spindel.dom.addressing :as addressing]
            [org.replikativ.spindel.incremental.interval :as iv]
            [org.replikativ.spindel.effects.track :refer [track]]
            [is.simm.documents.rich-text :as rich]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.memory-actions :as actions]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.uis.web.desktop.memory-sync :as sync]
            [is.simm.uis.web.desktop.source-editor :as source]
            [is.simm.uis.web.desktop.rich-editor :as rich-editor]
            [is.simm.uis.web.desktop.markdown :as md]
            [superficie.api :refer [toSup]]
            [is.simm.uis.web.desktop.views.memory-navigation :as navigation]
            [is.simm.uis.web.desktop.download-link :as download])
  (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]]))

(declare model-node)

(defn- children [node]
  ;; Recursive children must be constructed inside their actual parent element,
  ;; with distinct sibling slots. Prebuilding them aliases per-address caches.
  (mapv (fn [index child]
          (addressing/with-slot-fn index #(model-node child)))
        (range) (:content node)))

(defn- marked-text [node]
  ;; Preserve the previous outer-last mark order, but construct each inner
  ;; wrapper under the outer wrapper's DOM scope, not as a prebuilt value.
  (letfn [(render [marks]
            (if-let [mark (peek marks)]
              (let [remaining (pop marks)]
                (case (:type mark)
                  "bold" (el/strong {} (render remaining))
                  "italic" (el/em {} (render remaining))
                  "strike" (el/span {:style {:text-decoration "line-through"}} (render remaining))
                  "code" (el/code {} (render remaining))
                  "link" (let [href (get-in mark [:attrs :href])]
                           (if (rich/safe-link? href)
                             (el/a {:href href :target "_blank" :rel "noopener noreferrer"}
                                   (render remaining))
                             (render remaining)))
                  (render remaining)))
              (:text node)))]
    (render (vec (:marks node)))))

(defn- model-node [node]
  ;; Deliberately no innerHTML, scripts, embedded images, or widget execution.
  (case (:type node)
    "doc" (el/div {} (children node))
    "text" (marked-text node)
    "paragraph" (el/p {} (children node))
    "heading" (case (get-in node [:attrs :level])
                1 (el/h1 {} (children node)) 2 (el/h2 {} (children node)) 3 (el/h3 {} (children node))
                4 (el/h4 {} (children node)) 5 (el/h5 {} (children node)) (el/h6 {} (children node)))
    "bulletList" (el/ul {} (children node))
    "orderedList" (el/ol {:start (get-in node [:attrs :start] 1)} (children node))
    "listItem" (el/li {} (children node))
    "codeBlock" (el/pre {} (el/code {} (children node)))
    "hardBreak" (el/br {})
    "horizontalRule" (el/hr {})
    nil))

(defn- source-view [path draft a write?]
  (source/view {:id [(:owner a) path] :path path :label "Memory file source"
                :value (:text draft) :selection (:selection draft) :read-only? (not write?)
                :on-change (fn [text selection] ((:change a) path editor/changed text selection))}))

(defn- superficie-view [text]
  (let [result (try (let [converted (when (<= (count text) 65536) (toSup text))]
                      (if (string? converted) {:text converted} {:error? true}))
                    (catch :default _ {:error? true}))]
    (el/div {:class "memory-superficie"}
            (el/p {:class "memory-note"} "Superficie · read-only projection of the current Clojure source. Switch to Clojure to edit; saving and evaluation always use the original source.")
            (if (:error? result)
              (el/p {:role "status"} "This draft cannot be rendered as Superficie. The original source is unchanged; use Clojure to inspect or edit it.")
              (el/pre {:class "code-panel-sup"}
                      (el/code {:class "hljs language-superficie"
                                :innerHTML (md/highlight-code (:text result) "superficie")}))))))

(defn- repl-view [state a preference]
  (let [syntax (editor/repl-syntax state preference)
        input (editor/repl-input state syntax)
        running? (get-in state [:loading :repl])
        evaluate? (get-in state [:sandbox-permissions :evaluate?])
        history (get-in state [:repl :history])]
    (el/details {:class "memory-repl"}
                (el/summary {} "Room REPL · separate sandbox · Clojure / Superficie")
                (el/p {:class "memory-note"} "Separate scratch input; never saves or rewrites the file. Both syntaxes use your same human SCI session. Ctrl+Enter evaluates. User-defined macros can be called normally; custom Superficie block-shape registration is not exposed.")
                (el/div {:class "memory-toolbar" :aria-label "REPL input syntax"}
                        (el/span {} "Input syntax")
                        (for [[mode label] [[:clojure "Clojure input"] [:superficie "Superficie input"]]]
                          (el/button {:key mode :class (when (= syntax mode) "selected")
                                      :on-click (fn [_] ((:repl-change a) assoc :syntax mode))} label))
                        (el/button {:on-click (fn [_] ((:repl-change a) dissoc :syntax))} "Follow code preference")
                        (el/label {} "Namespace "
                                  (el/input {:aria-label "REPL namespace" :value (get-in state [:repl :namespace] "user")
                                             :on-input (fn [e] ((:repl-change a) assoc :namespace (.. e -target -value)))})))
                (el/div {}
                        (source/view {:id [(:owner a) :repl syntax] :path (if (= syntax :clojure) "scratch.clj" "scratch.sup")
                                      :label (str (name syntax) " REPL input") :value input
                                      :selection (get-in state [:repl :inputs syntax :selection])
                                      :on-evaluate (fn [] (when (and (not (false? evaluate?)) (not running?) (not (str/blank? input)))
                                                            ((:repl-evaluate a) syntax)))
                                      :on-change (fn [text selection]
                                                   ((:repl-change a) assoc-in [:inputs syntax] {:text text :selection selection}))}))
                (el/button {:disabled (or running? (false? evaluate?) (str/blank? input))
                            :on-click (fn [_] ((:repl-evaluate a) syntax))}
                           (cond running? "Evaluating…" (nil? evaluate?) "Try Room REPL" :else "Evaluate REPL input"))
                (when-not evaluate?
                  (el/p {:class "memory-note"}
                        (if (nil? evaluate?) "Permission is unknown. An explicit attempt asks the server to check current room permissions; it never grants file access."
                            "Room-owner evaluation permission is required.")))
                (for [receipt history]
                  (el/details {:key (str (:id receipt)) :open (= (:id receipt) (:id (first history)))
                               :class "memory-repl-receipt"}
                              (el/summary {} (str (if (:success receipt) "Result" "Failed") " · " (name (:input-syntax receipt))
                                                  " · " (:namespace receipt)))
                              (el/pre {} (if (:success receipt) (:value receipt) (:error receipt)))
                              (when (:partial? receipt) (el/p {} "Earlier forms may have executed; inspect the session before retrying."))
                              (when (:session-retired? receipt) (el/p {} "Timed out. This interpreter was retired; the next evaluation starts a fresh session."))
                              (when (seq (:stdout receipt)) (el/pre {} (str "stdout\n" (:stdout receipt))))
                              (when (seq (:stderr receipt)) (el/pre {} (str "stderr\n" (:stderr receipt))))
                              (el/details {} (el/summary {} "Submitted input") (el/pre {} (:input-source receipt)))
                              (when (:generated-clojure receipt)
                                (el/details {} (el/summary {} "Clojure submitted for evaluation")
                                            (when (:generated-truncated? receipt) (el/p {} "Generated-source preview is truncated."))
                                            (el/pre {} (:generated-clojure receipt)))))))))

(defn- rich-toolbar [write?]
  (el/div {:class "memory-toolbar memory-document-toolbar"}
          (for [[op label] [[:bold "Bold"] [:italic "Italic"] [:strike "Strike"]
                            [:paragraph "Paragraph"] [:heading "Heading"]
                            [:bullet-list "Bullets"] [:ordered-list "Numbered"] [:code "Code"]]]
            (el/button {:key op :disabled (not write?)
                        :on-mousedown #(.preventDefault %)
                        :on-click #(rich-editor/command! % op)} label))))

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
                                (el/button {:on-click (fn [_]
                                                        ((:change a) path assoc :mode :source
                                                                     :selection (editor/source-selection draft (:from slot) (:to slot))))}
                                           "Open in Source")))))))))))

(defn- retained-properties-view [path state a]
  (let [orphaned (for [[[p target :as scope] drafts] (:property-drafts state)
                       :when (= path p)
                       [id pd] drafts
                       :let [model (get-in state [:property-models scope])
                             field (first (filter #(= id (:id %)) (:fields model)))]
                       :when (or (= :unavailable (:status model)) (nil? field)
                                 (not= target (get-in state [:property-targets path]))
                                 (not= (:alias pd) (:alias field)))]
                   [target id pd])]
    (when (seq orphaned)
      (el/div {:class "memory-conflict" :aria-label "Retained property drafts"}
              (el/h3 {} "Retained field drafts")
              (el/p {:class "memory-note"} "These fields are no longer available in this view. Their original values are retained for copying; they are never transferred to a different property automatically.")
              (for [[target id pd] orphaned]
                (el/div {:key (pr-str [target id])}
                        (el/label {} (or (:label pd) (:alias pd) (str id))
                                  (el/textarea {:read-only true :aria-label (str "Retained field " id)
                                                :value (if (get-in pd [:input :null]) "null (not set)" (get-in pd [:input :text]))}))
                        (el/button {:on-click (fn [_] ((:property-discard a) path target id))} "Discard retained field draft")))))))

(defn- properties-view [path draft state a write?]
  (let [target (get-in state [:property-targets path]) scope [path target]
        model (get-in state [:property-models scope])
        current? (and model ((:property-current? a) path target model))
        busy? (or (get-in state [:loading [:property-save path]]) (get-in state [:loading [:save path]]))
        editable? (and write? current? (not busy?) (not (editor/dirty? draft))
                       (= (:expected model) (get-in draft [:source :entry])))]
    (el/div {:class "memory-properties" :aria-label "Typed document properties"}
            (if-not target
              (el/button {:on-click (fn [_] ((:open-properties a) path {:kind :page}))} "Show properties")
              (el/div {}
                      (el/h3 {} "Properties")
                      (cond
                        (not current?) (el/p {:class "memory-note" :role "status"} "Checking saved properties…")
                        (= :unavailable (:status model))
                        (el/p {:class "memory-note"}
                              (if (= :unprovisioned-canonical-space (:reason model))
                                "Typed properties require a governed knowledge workspace. This existing workspace is unchanged; source metadata remains available below."
                                "This file needs an explicit identity and valid Markdown declarations before typed properties can be edited."))
                        :else
                        (el/div {}
                                (el/p {:class "memory-note"}
                                      (str "Types: " (str/join ", " (map :name (:types model)))
                                           ". Properties edit the saved Markdown source."))
                                (when (editor/dirty? draft)
                                  (el/p {:class "memory-note"} "Save or discard your file draft before applying property changes."))
                                (for [field (:fields model)
                                      :let [pd (get-in state [:property-drafts scope (:id field)])
                                            input (editor/property-input model field pd)
                                            stale? (not (editor/property-draft-current? model pd))
                                            enabled? (and editable? (:editable? field) (not stale?))]]
                                  (el/div {:key (str (:id field)) :class "memory-property-field"}
                                          (el/label {} (str (:label field) (when (:required? field) " *"))
                                                    (if (= :db.type/boolean (:value-type field))
                                                      (el/select {:aria-label (:label field) :disabled (not enabled?)
                                                                  :value (if (:null input) "null" (:text input))
                                                                  :on-change (fn [event]
                                                                               (let [value (.. event -target -value)]
                                                                                 ((:property-input a) path target field
                                                                                  (if (= value "null") {:null true} {:text value}))))}
                                                                 (when-not (:required? field) (el/option {:value "null"} "Not set"))
                                                                 (el/option {:value "true"} "True")
                                                                 (el/option {:value "false"} "False"))
                                                      (el/input {:type "text" :aria-label (:label field) :disabled (not enabled?)
                                                                 :value (or (:text input) "")
                                                                 :on-input (fn [event]
                                                                             ((:property-input a) path target field {:text (.. event -target -value)}))})))
                                          (when (:null input) (el/small {} "Not set"))
                                          (when-not (:editable? field)
                                            (el/small {} (str "Read-only · " (name (:reason field)))))
                                          (when (and (:editable? field) (not (:required? field)))
                                            (el/button {:disabled (not enabled?)
                                                        :on-click (fn [_] ((:property-input a) path target field {:null true}))} "Clear"))
                                          (when (:editable? field)
                                            (el/button {:disabled (or (not enabled?) (nil? pd))
                                                        :on-click (fn [_] ((:property-save a) path target field))} "Apply"))
                                          (when stale?
                                            (el/p {:class "memory-note" :role "status"} "Saved source or property definitions changed. Your entered value was retained; discard it before editing the current version."))
                                          (when pd
                                            (el/button {:on-click (fn [_] ((:property-discard a) path target (:id field)))} "Discard field draft")))))))))))

(defn- view [data state a syntax-pref]
  (let [path (:selected state) draft (get-in state [:drafts path])
        verified? (actions/verified? state @sig/memory-connection)
        workspace-write? (and verified? (true? (get-in state [:permissions :write?])))
        coherent? (and verified? (not (:target-stale? draft))
                       (= (:target-stamp state) (get-in draft [:source :target-stamp])))
        write? (and workspace-write? coherent?)
        markdown? (= :markdown (get-in draft [:source :kind]))
        text? (string? (:text draft))
        clojure? (and text? (re-find #"(?i)\.(clj|cljc|cljs|bb)$" (or path "")))
        code-view (editor/code-view draft syntax-pref)
        office? (= :office (get-in draft [:source :kind])) office (:office draft)
        folder (or (:current-folder state) "")
        ask-path (fn [label default action]
                   (when-let [p (js/prompt label default)]
                     (when-not (str/blank? p) (action p))))]
    (el/div {:class "memory-panel"}
            (el/div {:class "memory-toolbar"}
                    (el/h2 {} (str (or (:room-name data) "Room") " · Memory"))
                    (el/button {:on-click (fn [_] ((:refresh a)))} "Refresh")
                    (when workspace-write?
                      (el/button {:on-click (fn [_] (ask-path "New file path (relative to the room)"
                                                              (if (seq folder) (str folder "/note.md") "wiki/note.md")
                                                              #(navigation/create-file! (:room-id data) (:room-name data) % (:col-id data) (:target-stamp state))))} "New file"))
                    (when workspace-write?
                      (el/button {:on-click (fn [_] (ask-path "New folder path (relative to the room)"
                                                              (str (when (seq folder) (str folder "/")) "new-folder")
                                                              (:mkdir a)))} "New folder"))
                    (when workspace-write?
                      (el/label {:class "memory-upload"} "Upload file"
                                (el/input {:type "file" :aria-label "Upload to Memory"
                                           :on-change (fn [e]
                                                        (when-let [file (aget (.. e -target -files) 0)]
                                                          ((:upload a) folder file))
                                                        (set! (.. e -target -value) ""))}))))
            (when-not verified?
              (el/p {:class "memory-note" :role "status"}
                    "Workspace is not verified. Your drafts are retained; Refresh checks the currently authorized workspace without relying on live updates."))
            (when (:target-stale? draft)
              (el/div {:class "memory-conflict" :role "status"}
                      (el/p {} "Workspace changed. This draft is from the previous workspace and will not be saved into the new one.")
                      (el/p {:class "memory-note"} "Inspect your retained source or XML below. Load the current copy and explicitly choose it before publishing.")))
            (when-let [cancelled (:cancelled-upload state)]
              (el/p {:class "memory-note" :role "status"}
                    (str "Upload cancelled for " (:path cancelled) ": the originating workspace is no longer verified. No automatic retry was sent.")))
            (for [[slot error] (:errors state)]
              (el/p {:key (pr-str slot) :class "proposal-error" :role "alert"} error))
            (when-let [error (sync/problem @sync/state)]
              (el/p {:class "proposal-error" :role "alert"} error))
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
                                                                          (get-in state [:loading [:property-save path]])
                                                                          (:remote-deleted? draft)
                                                                          (not (editor/dirty? draft)))
                                                            :on-click (fn [_] ((:save a) path))} "Save"))
                                              (el/button {:on-click (fn [_] ((:reload a) path))} "Check for changes")
                                              (when (get-in draft [:source :entry])
                                                (el/button {:disabled (not coherent?) :on-click (fn [_] ((:download a) path))} "Download"))
                                              (when (and coherent? (= (:target-stamp state) (get-in state [:download :target-stamp]))
                                                         (= path (get-in state [:download :path])))
                                                (download/view [(:owner a) path] (last (str/split path #"/"))
                                                               (get-in state [:download :base64])))
                                              (when write?
                                                (el/button {:disabled (or (editor/dirty? draft) (editor/property-drafts? state path)
                                                                          (get-in state [:loading [:property-save path]]))
                                                            :on-click (fn [_] (ask-path "Move file to" path #((:move a) path %)))} "Move")))
                                      (when (or (:incoming draft) (:incoming-entry draft))
                                        (el/div {:class "memory-conflict" :role "status"}
                                                (if (:remote-deleted? draft)
                                                  (el/p {} "This file was deleted or moved. Your buffer is retained for recovery; it will not be recreated automatically.")
                                                  (el/div {}
                                                          (el/p {} "The saved file changed. Your draft is retained; saving it will refuse the stale version.")
                                                          (if (:incoming draft)
                                                            (el/button {:on-click (fn [_] ((:use-incoming a) path))} "Discard draft and use saved version")
                                                            (el/p {:class "memory-note"} "The new revision has not loaded yet. Reload to retry if the read failed."))))))
                                      (when (:earlier-save-receipt draft)
                                        (el/p {:class "memory-note"} "An earlier draft's save completed after this version was selected. This draft was kept unchanged; check the saved version before another save."))
                                      (when markdown? (properties-view path draft state a write?))
                                      (retained-properties-view path state a)
                                      (when (and text? (not clojure?))
                                        (el/div {:class "memory-toolbar" :aria-label "Document views"}
                                                (el/span {} "File view")
                                                (for [[mode label] (if markdown? [[:read "Read"] [:rich "Rich text"] [:outline "Outline"] [:source "Source"]]
                                                                       [[:source "Code / text"]])]
                                                  (el/button {:key mode :class (when (= mode (:mode draft)) "selected")
                                                              :on-click (fn [_] ((:mode a) path mode))} label))))
                                      (when clojure?
                                        (el/div {:class "memory-toolbar" :aria-label "Code syntax view"}
                                                (el/span {} "File view")
                                                (for [[mode label] [[:clojure "Clojure"] [:superficie "Superficie"]]]
                                                  (el/button {:key mode :class (when (= mode code-view) "selected")
                                                              :on-click (fn [_] ((:change a) path assoc :code-view mode))} label))
                                                (el/button {:class (when (nil? (:code-view draft)) "selected")
                                                            :on-click (fn [_] ((:change a) path dissoc :code-view))} "Follow preference")))
                                      (when (and text? (not markdown?))
                                        (el/p {:class "memory-note"} "Saving changes the versioned room file only. It does not evaluate code, reload an agent, or grant capabilities."))
                                      (when (and text? (re-find #"(?i)\.(clj|cljc|bb)$" path))
                                        (el/div {:class "memory-evaluation"}
                                                (el/p {:class "memory-note"} "Target: your room-scoped SCI session · separate sandbox, not the file workspace · no room tools injected. Definitions persist until the room closes or the server restarts. Sends the current draft, not necessarily the saved file. The server checks room permissions on every explicit run.")
                                                (for [[scope label] [[:selection "Send selection to Room REPL"]
                                                                     [:form "Send form to Room REPL"]
                                                                     [:file "Send draft to Room REPL"]]]
                                                  (el/button {:key scope :disabled (or (false? (get-in state [:sandbox-permissions :evaluate?]))
                                                                                       (and (not= :file scope) (= :superficie code-view))
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
                                                  (el/button {:disabled (not coherent?) :on-click (fn [_] ((:office-open a) path nil))} "Inspect document parts")
                                                  (el/div {}
                                                          (el/select {:aria-label "Office XML part" :value (:part office)
                                                                      :disabled (or (not coherent?) (editor/dirty? draft) (get-in state [:loading [:office-save path]]))
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
                                        (and clojure? (= :superficie code-view)) (superficie-view (:text draft))
                                        (= :source (:mode draft)) (source-view path draft a write?)
                                        :else (document-view path draft a write?))
                                      (when (and text? (re-find #"(?i)\.(clj|cljc|bb)$" path))
                                        (repl-view state a syntax-pref)))))))))

(defn render [data]
  (spin
   (let [connection (iv/get-new (track sig/memory-connection))
         stream (iv/get-new (track sync/state))
         states (iv/get-new (track sig/memory-editor-state))
         syntax-pref (iv/get-new (track sig/syntax-pref))
         a (actions/controller data) state (get states (:owner a))]
     ((:ensure-path a))
     ((:ensure-sync a) connection stream)
     ((:ensure-properties a))
     (view data state a syntax-pref))))

(defn render-context [data]
  (spin
   (let [connection (iv/get-new (track sig/memory-connection))
         stream (iv/get-new (track sync/state))
         states (iv/get-new (track sig/memory-editor-state))
         a (actions/controller data) state (get states (:owner a)) path (:selected state)
         draft (get-in state [:drafts path])
         coherent? (and (actions/verified? state connection) (not (:target-stale? draft))
                        (= (:target-stamp state) (get-in draft [:source :target-stamp])))
         history (get-in state [:history path]) preview (get-in state [:history-preview path])]
     ((:ensure-path a))
     ((:ensure-sync a) connection stream)
     (el/div {:class "memory-history context-section"}
             (el/div {:class "context-section-title"} "File history")
             (when path
               (el/div {}
                       (el/button {:disabled (or (not coherent?) (get-in state [:loading [:history path]]))
                                   :on-click (fn [_] ((:history a) path))} "Refresh history")
                       (el/p {:class "memory-note"} "Current branch · 31-commit ancestry window, including merges · renames are separate paths. Preview does not replace your draft.")
                       (for [version (:versions history)
                             :let [message (or (first (str/split-lines (or (:message version) ""))) "")
                                   message (subs message 0 (min 140 (count message)))]]
                         (el/button {:key (str (:commit version)) :class "memory-history-entry" :disabled (not coherent?)
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
