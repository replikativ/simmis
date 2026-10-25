(ns is.simm.uis.web.desktop.views.memory
  "One room file tree and source-preserving document views. Reading and saving
   code never evaluates it or changes a participant's SCI interpreter."
  (:require [clojure.string :as str]
            [org.replikativ.spindel.dom.elements :as el]
            [org.replikativ.spindel.dom.addressing :as addressing]
            [org.replikativ.spindel.incremental.interval :as iv]
            [org.replikativ.spindel.effects.track :refer [track]]
            [is.simm.documents.rich-text :as rich]
            [is.simm.documents.links :as links]
            [is.simm.documents.attachments :as attachments]
            [is.simm.documents.markdown-scene :as markdown-scene]
            [is.simm.documents.inline-charts :as inline-charts]
            [is.simm.documents.chart-contract :as chart-contract]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.memory-actions :as actions]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.uis.web.desktop.memory-cells :as cells]
            [is.simm.uis.web.desktop.memory-declarations :as declarations]
            [is.simm.uis.web.desktop.memory-sync :as sync]
            [is.simm.uis.web.desktop.source-editor :as source]
            [is.simm.uis.web.desktop.office-view :as office-view]
            [is.simm.uis.web.desktop.rich-editor :as rich-editor]
            [is.simm.uis.web.desktop.source-preview :as source-preview]
            [is.simm.uis.web.desktop.code-highlight :as code-highlight]
            [is.simm.uis.web.desktop.chart-view :as charts]
            [is.simm.uis.web.desktop.cell-result-view :as cell-result]
            [is.simm.uis.web.desktop.math-view :as math]
            [is.simm.uis.web.desktop.rich-controls :as rich-controls]
            [is.simm.uis.web.desktop.markdown :as md]
            [superficie.api :refer [toSup]]
            [is.simm.uis.web.desktop.views.memory-navigation :as navigation]
            [is.simm.uis.web.desktop.views.core :as vc]
            [is.simm.uis.web.desktop.download-link :as download])
  (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]]))

(declare model-node)

(defn- children [node options]
  ;; Recursive children must be constructed inside their actual parent element,
  ;; with distinct sibling slots. Prebuilding them aliases per-address caches.
  (mapv (fn [index child]
          (addressing/with-slot-fn index #(model-node child (update options :path (fnil conj []) index))))
        (range) (:content node)))

(defn- marked-text [node options]
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
                  "link" (let [href (attachments/resolve-link (:file-base options) (get-in mark [:attrs :href]))]
                           (if (rich/safe-link? href)
                             (el/a {:href href :target "_blank" :rel "noopener noreferrer"}
                                   (render remaining))
                             (render remaining)))
                  (render remaining)))
              (:text node)))]
    (render (vec (:marks node)))))

(defn- code-content [node options]
  (el/code {}
           (if-let [parts (get (:highlights options) (:path options))]
             (mapv (fn [i {:keys [text class]}]
                     (addressing/with-slot-fn i #(if class (el/span {:class class} text) text)))
                   (range) parts)
             (children node options))))

(defn- model-node
  ([node] (model-node node {}))
  ([node options]
  ;; Deliberately no innerHTML, scripts, embedded images, or widget execution.
  (case (:type node)
    "doc" (el/div {} (children node options))
    "text" (marked-text node options)
    ("inlineMath" "displayMath") (math/view node)
    "footnoteReference" (el/sup {:class "memory-footnote-reference"
                                 :title (get-in node [:attrs :raw])
                                 :aria-label (str "Footnote " (get-in node [:attrs :label]))}
                                (marked-text (assoc node :text (get-in node [:attrs :label])) options))
    "paragraph" (el/p {} (children node options))
    "heading" (case (get-in node [:attrs :level])
                1 (el/h1 {} (children node options)) 2 (el/h2 {} (children node options)) 3 (el/h3 {} (children node options))
                4 (el/h4 {} (children node options)) 5 (el/h5 {} (children node options)) (el/h6 {} (children node options)))
    "bulletList" (el/ul {} (children node options))
    "orderedList" (el/ol {:start (get-in node [:attrs :start] 1)} (children node options))
    "listItem" (el/li {} (children node options))
    "codeBlock" (if-let [preview (get (:previews options) (:path options))]
                  (el/div {:class "memory-inline-chart"}
                          (el/pre {} (code-content node options))
                          (el/div {:class "memory-note"} "Chart preview")
                          (if (= :ready (:status preview))
                            (charts/view (:path options) (:data preview))
                            (el/p {:class "memory-note"} "Chart JSON is invalid or exceeds the supported bounds.")))
                  (el/pre {} (code-content node options)))
    "hardBreak" (el/br {})
    "horizontalRule" (el/hr {})
    "table" (el/table {:class "memory-data-table"} (el/tbody {} (children node options)))
    "tableRow" (el/tr {} (children node options))
    "tableHeader" (el/th {:style {:text-align (or (get-in node [:attrs :align]) "left")}} (children node options))
    "tableCell" (el/td {:style {:text-align (or (get-in node [:attrs :align]) "left")}} (children node options))
    nil)))

(defn- source-view [path draft a write?]
  (source/view {:id [(:owner a) path] :path path :label "Memory file source"
                :value (:text draft) :selection (:selection draft) :read-only? (not write?)
                :jump ((:jump a) path)
                :before-jump #((:before-jump a) path %)
                :on-jumped (fn [jump selection] ((:jumped a) path jump :source selection))
                :on-change (fn [text selection] ((:edit a) path editor/changed text selection))}))

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
                                                  " · " (:namespace receipt)
                                                  (when-let [source (:source receipt)]
                                                    (str " · " (:path source) " · "
                                                         (case (cells/receipt-status state receipt)
                                                           :saved-current "saved version matches"
                                                           :draft-changed "draft changed"
                                                           :saved-changed "saved source changed"
                                                           "source not verified")))))
                              (when-let [source (:source receipt)]
                                (el/details {}
                                            (el/summary {} "Saved cell provenance")
                                            (el/p {:class "memory-note"}
                                                  (str (:path source) " · revision " (:revision source)
                                                       " · source span " (:from source) "–" (:to source)))
                                            (el/pre {} (:original source))))
                              (when-let [diagnostic (:presentation-diagnostic receipt)]
                                (el/p {:class "memory-note"} diagnostic))
                              (when (and (= (:id receipt) (:id (first history)))
                                         (= :table (get-in receipt [:presentation :kind]))
                                         (= :saved-current (cells/receipt-status state receipt)))
                                (cell-result/table-view (:presentation receipt)))
                              (el/pre {} (if (:success receipt) (:value receipt) (:error receipt)))
                              (when (:value-truncated? receipt) (el/p {:class "memory-note"} "Value preview is truncated."))
                              (when (:partial? receipt) (el/p {} "Earlier forms may have executed; inspect the session before retrying."))
                              (when (:session-quarantined? receipt)
                                (el/p {} "Evaluation stopped. This session stays unavailable until its worker and cleanup finish; the next admitted evaluation starts fresh."))
                              (when (seq (:stdout receipt)) (el/pre {} (str "stdout\n" (:stdout receipt))))
                              (when (seq (:stderr receipt)) (el/pre {} (str "stderr\n" (:stderr receipt))))
                              (el/details {} (el/summary {} "Submitted input") (el/pre {} (:input-source receipt)))
                              (when (:generated-clojure receipt)
                                (el/details {} (el/summary {} "Clojure submitted for evaluation")
                                            (when (:generated-truncated? receipt) (el/p {} "Generated-source preview is truncated."))
                                            (el/pre {} (:generated-clojure receipt)))))))))

(defn- rich-toolbar [write? selection source!]
  (let [format (:format selection)
        first-enabled (:op (first (filter #(get-in format [(:op %) :enabled?]) (mapcat identity rich-controls/groups))))]
    ;; Visibility/geometry are owned by the attached native controls adapter.
    ;; A constant rendered data-visible would race its focus/measurement update.
    (el/div {:class "memory-document-toolbar" :role "toolbar" :aria-label "Block formatting"
             :on-keydown rich-controls/toolbar-keydown!}
            (for [[index group] (map-indexed vector rich-controls/groups)]
              (el/div {:key index :role "group" :class "memory-format-group"
                       :aria-label (nth ["Text structure" "Text formatting" "Lists"] index)}
                      (for [{:keys [op label icon]} group]
                        (el/button {:key op :type "button" :title label :aria-label label
                                    :aria-pressed (str (boolean (get-in format [op :active?])))
                                    :disabled (or (not write?) (not (get-in format [op :enabled?])))
                                    :tabindex (if (= first-enabled op) 0 -1)
                                    :on-mousedown #(.preventDefault %)
                                    :on-click #(rich-editor/command! % op)}
                                   (vc/icon icon {:aria-hidden "true"})))))
            (el/div {:class "memory-format-group" :role "group" :aria-label "Source editing"}
                    (el/button {:type "button" :aria-label "Edit local Markdown" :title "Edit selected content as Markdown"
                                :tabindex (if first-enabled -1 0) :disabled (or (not write?) (nil? (:region selection)))
                                :on-mousedown #(.preventDefault %)
                                :on-click (fn [_] (source! (:region selection)))}
                               (vc/icon :file-code {:aria-hidden "true"}))))))

(defn- document-view [path draft a write? image-urls file-base]
  (let [base (:rich-base draft) rich? (and (= :rich (:mode draft)) base)
        analysis (if rich? (:analysis base) (:analysis draft))
        source (if rich? (:text base) (:text draft))
        chart-models (when-let [models (:chart-models a)] (models path))
        inline-previews (when-let [previews (:inline-chart-previews a)] (previews path))
        inline-limit (max 0 (- (if-let [budget (:chart-budget a)] (budget path) chart-contract/max-mounted) (count chart-models)))
        outline-blocks (when (= :outline (:mode draft))
                         (loop [remaining (editor/outline (get-in analysis [:semantic :blocks])) index 0 out []]
                           (if-let [block (first remaining)]
                             (let [models (mapv rich/model (:syntax block))]
                               (recur (next remaining) (+ index (count models))
                                      (conj out (assoc block :models models :model-start index
                                                      :chart (get chart-models (str (:id block)))))))
                             out)))
        chart-ends (into {} (for [block (mapcat #(tree-seq (comp seq :children) :children %)
                                                 (get-in analysis [:semantic :blocks]))
                                  :let [chart (get chart-models (str (:id block)))] :when chart]
                              [(:to block) [(:id block) chart]]))
        ;; Chart data is an explicit item dependency, not an invisible closure
        ;; beside an unchanged syntax slot in the incremental loop.
        slots (mapv #(assoc % :chart (get chart-ends (:to %))) (:slots analysis))
        read-models (if outline-blocks (vec (mapcat :models outline-blocks))
                       (mapv #(or (when rich? (get-in base [:models (:from %)])) (:model %)) slots))
        highlights (when-not rich? (code-highlight/document-segments read-models))
        source-previews (markdown-scene/index source analysis image-urls file-base)
        inline-positions (inline-charts/positions read-models inline-previews inline-limit)
        slots (mapv #(assoc % :inline-positions inline-positions) slots)]
    (el/div {:class "memory-document"}
            (when rich? (rich-toolbar write? (:document-selection base) #((:local-source-open a) path %)))
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
              (if (and rich? (:document base))
                (rich-editor/view {:id [(:owner a) path (:id base) :document]
                                   :model (or (:document-model base) (get-in base [:document :model]))
                                   :selection (:document-selection base) :document? true
                                   :charts chart-models
                                   :source-previews source-previews
                                   :file-base file-base
                                   :inline-charts (when (seq inline-previews) {:entries inline-previews :limit inline-limit})
                                   :cell-config ((:cell-config a) path)
                                   :on-run-cell #((:run-cell a) path %)
                                   :block-links ((:block-links a) path)
                                   :linked-block ((:linked-block a) path)
                                   :jump ((:jump a) path)
                                   :before-jump #((:before-jump a) path %)
                                   :on-jumped (fn [jump selection] ((:jumped a) path jump :rich selection))
                                   :read-only? (or (not write?) (some? (:local-source draft)))
                                   :on-blocked #((:document-blocked a) path %)
                                   :on-change (fn [model selection]
                                                ((:document-change a) path (:id base) model selection))})
              (if (= :outline (:mode draft))
                (el/div {:class "memory-outline"}
                        (for [block (mapv #(assoc % :inline-positions inline-positions) outline-blocks)]
                          (el/div {:key (str (:id block)) :class "memory-outline-block"
                                   :style {:margin-left (str (* 18 (:depth block)) "px")}}
                                  (el/details {:class "memory-properties"}
                                              (el/summary {} "Block identity")
                                              (el/code {} (str (name (:identity-status block)) " · " (:id block))))
                                  (when (= :explicit (:identity-status block))
                                    (el/button {:on-click (fn [_] ((:open-properties a) path {:kind :block :id (:id block)}))}
                                               "Block properties"))
                                  (mapv (fn [index node model]
                                          (addressing/with-slot-fn
                                            index
                                            #(if model
                                               (model-node model {:path [(+ (:model-start block) index)]
                                                                  :highlights highlights
                                                                  :previews (:inline-positions block) :file-base file-base})
                                               (if-let [preview (get source-previews (str "opaque:" (:from node)))]
                                                 (source-preview/view (:from node) preview)
                                                 (el/pre {} (subs source (:from node) (:to node)))))))
                                        (range) (:syntax block) (:models block))
                                  (when-let [chart (:chart block)] (charts/view [path (:id block)] chart))
                                  (el/button {:on-click (fn [_]
                                                          ((:change a) path assoc :mode :source
                                                                       :selection {:anchor (:from block) :head (:to block)}))}
                                             "Edit source"))))
                (for [[slot-index slot] (map-indexed vector slots)]
                  (let [model (or (when rich? (get-in base [:models (:from slot)])) (:model slot))
                        text (subs source (:from slot) (:to slot))
                        structural? (boolean (re-matches #"(?s)\s*<!-- /?simmis:block[^>]*-->\s*" text))]
                    (if structural?
                      (when-let [[id chart] (:chart slot)] (charts/view [path id] chart))
                      (el/div {:key (str (:from slot) ":" (:to slot)) :class "memory-region"}
                              (cond
                                (and rich? model)
                                (rich-editor/view {:id [(:owner a) path (:id base) (:from slot)] :model model
                                                   :file-base file-base
                                                   :selection (get-in base [:selections (:from slot)])
                                                   :read-only? (not write?)
                                                   :on-change (fn [model selection]
                                                                ((:rich-change a) path (:id base) (:from slot) model selection))})
                                model (model-node model {:path [slot-index] :highlights highlights :previews (:inline-positions slot) :file-base file-base})
                                (get source-previews (str "opaque:" (:from slot)))
                                (source-preview/view (:from slot) (get source-previews (str "opaque:" (:from slot))))
                                :else (el/div {:class "memory-source-region"}
                                              (el/small {} (str (name (:kind slot)) " · preserved source"))
                                              (el/pre {} text)))
                              (when (and (= :rich (:mode draft)) (nil? model))
                                (el/button {:on-click (fn [_]
                                                        ((:change a) path assoc :mode :source
                                                                     :selection (editor/source-selection draft (:from slot) (:to slot))))}
                                           "Open in Source"))))))))))))

(defn- retained-properties-view [path state a]
  (let [orphaned (for [[[p target :as scope] drafts] (:property-drafts state)
                       :when (= path p)
                       [id pd] drafts
                       :let [model (get-in state [:property-models scope])
                             field (first (filter #(= id (:id %)) (:fields model)))]
                       :when (or (= :unavailable (:status model)) (nil? field)
                                 (not (some #{target} (declarations/visible-targets state path)))
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

(defn- declaration-view [path target draft a enabled? busy?]
  (let [preview (:preview draft)
        fields (if preview (:fields preview)
                   (mapv (fn [{:keys [property alias]}]
                           {:id property :label alias :new? true :editable? false :reason :preview-required})
                         (:additions draft)))
        names (into {} (map (juxt :id :name)) (:type-choices draft))]
    (el/div {:class "memory-properties" :aria-label "Type assignment"}
            (el/p {:class "memory-note"} "Choose existing types. Required fields and values are saved together in Markdown; the structural Page or Block role is always retained.")
            (for [id (:types draft)]
              (el/span {:key (str id)}
                       (get names id (str id)) " "
                       (when (contains? names id)
                         (el/button {:disabled (not enabled?) :aria-label (str "Remove type " (get names id))
                                     :on-click (fn [_] ((:declaration-types a) path target (filterv #(not= id %) (:types draft))))} "×"))))
            (el/select {:aria-label "Add a type" :value "" :disabled (not enabled?)
                        :on-change (fn [event]
                                     (when-let [id (parse-uuid (.. event -target -value))]
                                       ((:declaration-types a) path target (conj (:types draft) id))))}
                       (el/option {:value ""} "Add an existing type…")
                       (for [{:keys [id name]} (:type-choices draft) :when (not (some #{id} (:types draft)))]
                         (el/option {:key (str id) :value (str id)} name)))
            (when (:conflict? draft)
              (el/p {:class "memory-conflict" :role "status"} "Source or definitions changed. Your choices are retained; cancel this assignment and reopen it against the current version."))
            (when (and (not preview) (not (:conflict? draft)))
              (el/p {:class "memory-note" :role "status"} "Checking type definitions…"))
            (for [field fields :when (or (:needs-binding? field) (:new? field))]
              (el/div {:key (str (:id field)) :class "memory-property-field"}
                      (el/label {} (str (:label field) (when (:required? field) " *")))
                      (if (:new? field)
                        (let [input (get-in draft [:inputs (:id field)])]
                          (el/div {}
                                  (if (= :db.type/boolean (:value-type field))
                                    (el/select {:aria-label (str "New " (:label field))
                                                :disabled (or (not enabled?) (not (:editable? field)))
                                                :value (if (:null input) "null" (:text input))
                                                :on-change (fn [event]
                                                             (let [value (.. event -target -value)]
                                                               ((:declaration-input a) path target (:id field)
                                                                                       (if (= value "null") {:null true} {:text value}))))}
                                               (el/option {:value "null"} (if (:required? field) "Choose…" "Not set"))
                                               (el/option {:value "true"} "True")
                                               (el/option {:value "false"} "False"))
                                    (el/input {:type "text" :aria-label (str "New " (:label field))
                                               :disabled (or (not enabled?) (not (:editable? field)))
                                               :value (or (:text input) "")
                                               :on-input (fn [event] ((:declaration-input a) path target (:id field) {:text (.. event -target -value)}))}))
                                  (when-not (:editable? field) (el/small {} (str "Read-only · " (name (:reason field)))))
                                  (el/button {:disabled (not enabled?)
                                              :on-click (fn [_] ((:declaration-unbind a) path target (:id field)))} "Remove new binding")))
                        (el/button {:disabled (not enabled?)
                                    :on-click (fn [_] ((:declaration-bind a) path target field (:label field)))}
                                   (str "Add " (:label field) " field")))))
            (el/button {:disabled (or (not enabled?) (not (declarations/ready? draft)))
                        :on-click (fn [_] ((:declaration-save a) path target))} "Apply types and fields")
            (el/button {:disabled busy? :on-click (fn [_] ((:declaration-discard a) path target))} "Cancel assignment"))))

(defn- retained-declarations-view [path state a]
  (for [[[p target] draft] (:declaration-drafts state)
        :when (and (= path p) draft (not (some #{target} (declarations/visible-targets state path))))]
    (el/div {:key (pr-str target) :class "memory-conflict" :aria-label "Retained type assignment"}
            (el/h3 {} (str "Retained " (name (:kind target)) " type assignment"))
            (el/p {:class "memory-note"} "This assignment belongs to another property scope. Its choices and entered values are retained, not transferred to the selected scope.")
            (declaration-view path target draft a false (declarations/busy? state path)))))

(defn- property-field-view [path target field model state a editable?]
  (let [pd (get-in state [:property-drafts [path target] (:id field)])
        input (editor/property-input model field pd)
        stale? (not (editor/property-draft-current? model pd))
        enabled? (and editable? (:editable? field) (not stale?))
        options (get-in model [:reference-options (get-in field [:relation :codomain])])
        selected (first (filter #(= (:id %) (:text input)) (:items options)))
        commit! (fn [] (when enabled? ((:property-save a) path target field)))
        changed! (fn [value] ((:property-input a) path target field value))]
    (el/div {:class "memory-property-field"}
            (el/label {} (str (:label field) (when (:required? field) " *"))
                      (cond
                        (:relation field)
                        (el/div {:class "memory-reference-field"}
                                (if-let [target-path (:path selected)]
                                  (el/button {:type "button" :title target-path
                                              :on-click (fn [_] ((:open a) target-path))} (:label selected))
                                  (el/span {} (cond selected (:label selected) (:null input) "Not set" :else "Unresolved reference")))
                                (el/details {}
                                            (el/summary {} "Change…")
                                            (el/input {:type "search" :aria-label (str "Find " (:label field))
                                                       :placeholder "Find by name or path" :disabled (not enabled?)
                                                       :value (get-in state [:reference-search [path target (:id field)]] "")
                                                       :on-input (fn [e] ((:reference-search a) path target (:id field) (.. e -target -value)))})
                                            (el/select {:aria-label (:label field) :value (or (:text input) "")
                                                        :disabled (or (not enabled?) (not= :ready (:status options)))
                                                        :on-change (fn [e]
                                                                     (changed! {:text (.. e -target -value)})
                                                                     (commit!))}
                                                       (when-not selected (el/option {:value (or (:text input) "") :disabled true} "Choose a reference…"))
                                                       (for [item (:items options)
                                                             :let [query (str/lower-case (get-in state [:reference-search [path target (:id field)]] ""))]
                                                             :when (or (= (:id selected) (:id item))
                                                                       (str/includes? (str/lower-case (str (:label item) " " (:path item))) query))]
                                                         (el/option {:key (:id item) :value (:id item)}
                                                                    (str (:label item) (when (:path item) (str " · " (:path item)))))))
                                            (when-not (= :ready (:status options))
                                              (el/p {:class "memory-note"} "References are unavailable until the knowledge files are valid."))
                                            (when (false? (:complete? options))
                                              (el/p {:class "memory-note"} "Showing the first 200 compatible references."))))
                        (= :db.type/boolean (:value-type field))
                        (el/select {:aria-label (:label field) :disabled (not enabled?)
                                    :value (if (:null input) "null" (:text input))
                                    :on-change (fn [event]
                                                 (let [value (.. event -target -value)]
                                                   (changed! (if (= value "null") {:null true} {:text value}))
                                                   (commit!)))}
                                   (when-not (:required? field) (el/option {:value "null"} "Not set"))
                                   (el/option {:value "true"} "Yes")
                                   (el/option {:value "false"} "No"))
                        (:editable? field)
                        (el/input {:type "text" :aria-label (:label field) :disabled (not enabled?)
                                   :inputmode (when (#{:db.type/long :db.type/double} (:value-type field)) "decimal")
                                   :value (or (:text input) "")
                                   :on-input (fn [event] (changed! {:text (.. event -target -value)}))
                                   :on-blur (fn [_] (commit!))
                                   :on-keydown (fn [e]
                                                 (case (.-key e)
                                                   "Enter" (do (.preventDefault e) (commit!))
                                                   "Escape" (do (.preventDefault e) ((:property-discard a) path target (:id field))) nil))})
                        :else (el/span {:class "memory-property-readonly"} (if (:null input) "Not set" (:text input)))))
            (when-not (:editable? field)
              (el/small {:title (name (:reason field))} "Managed by type or source"))
            (when (and (:editable? field) (not (:required? field))
                       (not (false? (get-in field [:relation :clearable?]))) (not (:null input)))
              (el/button {:disabled (not enabled?) :title (str "Clear " (:label field))
                          :on-click (fn [_] (changed! {:null true}) (commit!))} "Clear"))
            (when stale?
              (el/p {:class "memory-note" :role "status"} "Saved source or property definitions changed. Your entered value was retained; discard it before editing the current version."))
            (when pd
              (el/button {:on-click (fn [_] ((:property-discard a) path target (:id field)))} "Discard field draft")))))

(defn- type-chips [model]
  (for [{:keys [id name]} (declarations/assigned-types model)]
    (el/span {:key (str id) :class "memory-type-chip" :title (str "Assigned type: " name)} (last (str/split name #"/")))))

(defn- scoped-properties-view [path target draft state a write?]
  (let [scope [path target]
        model (get-in state [:property-models scope])
        declaration-draft (get-in state [:declaration-drafts scope])
        owned? (and (actions/verified? state (:connection state))
                    (not (:target-stale? draft))
                    (= (:target-stamp state) (get-in draft [:source :target-stamp])))
        current? (and owned? model ((:property-current? a) path target model))
        display? (and owned? model
                      (= (:target-stamp state) (:target-stamp model))
                      (= (:connection state) (first (:request-marker model))))
        busy? (declarations/busy? state path)
        groups (declarations/field-groups model (get-in state [:property-drafts scope]))
        editable? (and write? current? (not busy?) (not (editor/dirty? draft)) (not (declarations/drafts? state path))
                       (= (:expected model) (get-in draft [:source :entry])))]
    (el/section {:class "memory-properties memory-semantic-form"
                 :aria-label (if (= :page (:kind target)) "Page properties" "Block properties")}
              (el/div {}
                      (when (= :block (:kind target))
                        (el/div {:class "memory-property-heading"}
                                (el/strong {} "Selected block")
                                (when (and current? (not= :unavailable (:status model))) (type-chips model))
                                (el/button {:on-click (fn [_] ((:open-properties a) path {:kind :page}))} "Close block properties")))
                      (when (and declaration-draft (or (not current?) (= :unavailable (:status model))))
                        (declaration-view path target declaration-draft a false busy?))
                      (cond
                        (not (or current? display?)) (el/p {:class "memory-note" :role "status"} "Checking saved properties…")
                        (= :unavailable (:status model))
                        (el/details {}
                          (el/summary {} "Typed properties unavailable")
                          (el/p {:class "memory-note"}
                              (if (= :unprovisioned-canonical-space (:reason model))
                                "Typed properties require a governed knowledge workspace. This existing workspace is unchanged; source metadata remains available below."
                                "This file needs an explicit identity and valid Markdown declarations before typed properties can be edited.")))
                        :else
                        (el/div {}
                                (el/div {:class "memory-property-heading"}
                                        (el/strong {} (if (= :page (:kind target)) "Properties" "Block fields"))
                                        (el/span {:class "memory-property-status" :role "status"}
                                                 (cond (not current?) "Checking…" busy? "Saving…" :else ""))
                                        (when-not declaration-draft
                                          (el/button {:disabled (or (not editable?) (editor/property-drafts? state path))
                                                      :on-click (fn [_] ((:declaration-open a) path target))} "Manage properties")))
                                (if declaration-draft
                                  (declaration-view path target declaration-draft a
                                                    (and write? current? (not busy?)
                                                         (not (editor/dirty? draft)) (not (editor/property-drafts? state path))
                                                         (declarations/current? declaration-draft model)) busy?))
                                (when (editor/dirty? draft)
                                  (el/p {:class "memory-note"} "Property changes are paused while your file draft is saving or has unsaved changes."))
                                (for [field (:primary groups)]
                                  (el/div {:key (str (:id field))}
                                          (property-field-view path target field model state a editable?)))
                                (when (seq (:available groups))
                                  (el/details {:class "memory-available-properties"}
                                              (el/summary {} "Other metadata")
                                              (for [field (:available groups)]
                                                (el/div {:key (str (:id field))}
                                                        (property-field-view path target field model state a editable?)))))))))))

(defn- properties-view [path draft state a write?]
  (let [target (get-in state [:property-targets path])
        blocks (->> (editor/outline (get-in draft [:analysis :semantic :blocks]))
                    (filter #(= :explicit (:identity-status %))) (take 65) vec)]
    (el/div {}
            (for [visible (declarations/visible-targets state path)]
              (el/div {:key (pr-str visible)}
                      (scoped-properties-view path visible draft state a write?)))
            (when (seq blocks)
              (el/details {:class "memory-block-properties"}
                          (el/summary {} "Inspect block properties")
                          (el/select {:aria-label "Property scope"
                                      :value (if (= :block (:kind target)) (str (:id target)) "page")
                                      :on-change (fn [event]
                                                   (let [value (.. event -target -value)]
                                                     (if (= value "page")
                                                       ((:open-properties a) path {:kind :page})
                                                       (when-let [id (parse-uuid value)]
                                                         ((:open-properties a) path {:kind :block :id id})))))}
                                     (el/option {:value "page"} "Page properties only")
                                     (when (and (= :block (:kind target))
                                                (not (some #(= (:id target) (:id %)) (take 64 blocks))))
                                       (el/option {:value (str (:id target))} "Selected block"))
                                     (for [block (take 64 blocks)]
                                       (el/option {:key (str (:id block)) :value (str (:id block))}
                                                  (str "Block " (subs (str (:id block)) 0 8)))))
                          (when (> (count blocks) 64)
                            (el/p {:class "memory-note"} "Showing the first 64 explicit blocks; use Structure to inspect another block.")))))))

(defn- local-source-view [path draft state a write?]
  (when-let [local (:local-source draft)]
    (let [busy? (get-in state [:loading [:local-source path]])]
      (el/section {:class "memory-local-source" :role "dialog" :aria-label "Local Markdown editor"
                   :on-keydown (fn [event]
                                 (when (and (= "Escape" (.-key event)) (not (.-defaultPrevented event))
                                            (not (.-isComposing event)))
                                   (.preventDefault event)
                                   (if (= (:text local) (:opened-text local))
                                     (do (rich-controls/request-editor-focus! event) ((:local-source-cancel a) path))
                                     ;; Do not discard source on Escape, or
                                     ;; intercept a completion's handled Escape.
                                     (when-let [button (.querySelector (.-currentTarget event) "[data-source-cancel]")]
                                       (.focus button)))))}
                  (el/div {:class "memory-local-source-heading"}
                          (el/strong {} "Selected content · Markdown")
                          (el/button {:type "button" :data-source-cancel "true"
                                      :on-click (fn [event] (rich-controls/request-editor-focus! event)
                                                  ((:local-source-cancel a) path))}
                                     (if (= (:text local) (:opened-text local)) "Close" "Discard edits")))
                  (el/p {:class "memory-note"} "Valid edits save automatically. Block IDs, children and metadata are protected; incomplete input stays here until it is valid.")
                  (source/view {:id [(:owner a) path :local-source (:id local)] :path "selection.md"
                                :label "Local Markdown source" :value (:text local) :selection (:selection local) :auto-focus? true
                                :read-only? (not write?)
                                :on-change (fn [text selection] ((:local-source-change a) path text selection))})
                  (el/div {:class "memory-note" :role "status"}
                          (cond busy? "Validating…"
                                (get-in state [:errors [:local-source path]]) "Not applied — source retained"
                                (= (:text local) (:opened-text local)) "Up to date"
                                :else "Source retained"))))))

(defn- prompt-path! [label initial action]
  (when-let [path (js/prompt label initial)]
    (when-not (str/blank? path) (action path))))

(defn- file-header [data state a draft workspace-write? write? coherent? syntax-pref]
  (let [path (:selected state) folder (or (:current-folder state) "")
        text? (string? (:text draft)) markdown? (= :markdown (get-in draft [:source :kind]))
        clojure? (and text? (re-find #"(?i)\.(clj|cljc|cljs|bb)$" (or path "")))]
    (el/div {:class "memory-file-header"}
            (el/nav {:class "memory-breadcrumbs" :aria-label "File path"}
                    (el/button {:on-click (fn [_] (navigation/focus-folder! (:room-id data) ""))}
                               (or (:room-name data) "Memory"))
                    (for [[i part] (map-indexed vector (when path (str/split path #"/")))]
                      (el/span {:key i :class "memory-path-part"}
                               (el/span {:aria-hidden "true"} "/")
                               (if (< i (dec (count (str/split path #"/"))))
                                 (el/button {:on-click (fn [_] (navigation/focus-folder! (:room-id data)
                                                                                       (str/join "/" (take (inc i) (str/split path #"/")))))} part)
                                 (el/span {:class "memory-file-name" :title path} part)))))
            (when markdown?
              (let [target {:kind :page} model (get-in state [:property-models [path target]])]
                (when (and coherent? model
                           (or ((:property-current? a) path target model)
                               (and (= (:target-stamp state) (:target-stamp model))
                                    (= (:connection state) (first (:request-marker model)))))
                           (not= :unavailable (:status model)))
                  (el/div {:class "memory-type-chips" :aria-label "Assigned page types"}
                          (type-chips model)))))
            (when draft
              (el/span {:class (str "memory-save-state" (when (editor/dirty? draft) " dirty"))
                        :role "status" :title (if (:office draft) "Office XML parts are saved explicitly."
                                                 "Changes save automatically to this workspace, not to other branches.")}
                       (cond (get-in state [:loading [:save path]]) "Saving…"
                             (or (:incoming draft) (:incoming-entry draft)) "Changed elsewhere"
                             (get-in state [:autosave-holds path]) "Not saved"
                             (editor/dirty? draft) "Unsaved"
                             :else "Saved")))
            (when (and text? (editor/dirty? draft)
                       (or (not (get-in state [:autosave path])) (get-in state [:autosave-holds path])))
              (el/button {:class "memory-save-button"
                          :disabled (or (not write?) (get-in state [:loading [:save path]])
                                        (get-in state [:loading [:property-save path]])
                                        (declarations/drafts? state path) (:remote-deleted? draft)
                                        (and (:local-source draft)
                                             (not= (get-in draft [:local-source :text]) (get-in draft [:local-source :opened-text])))
                                        (not (editor/dirty? draft)))
                          :on-click (fn [_] ((:save a) path))} "Save"))
            (when markdown?
              (el/select {:aria-label "Document view" :value (name (:mode draft)) :disabled (some? (:local-source draft))
                          :on-change (fn [event] ((:mode a) path (keyword (.. event -target -value))))}
                         (for [[mode label] [[:rich "Edit"] [:read "Read"] [:source "Markdown"] [:outline "Structure"]]]
                           (el/option {:key mode :value (name mode)} label))))
            (when clojure?
              (el/select {:aria-label "Code syntax view" :value (name (or (:code-view draft) :default))
                          :on-change (fn [event]
                                       (let [mode (keyword (.. event -target -value))]
                                         (if (= :default mode) ((:change a) path dissoc :code-view)
                                             ((:change a) path assoc :code-view mode))))}
                         (el/option {:value "default"} (str "Preference: " (name (or syntax-pref :clojure))))
                         (el/option {:value "clojure"} "Clojure")
                         (el/option {:value "superficie"} "Superficie")))
            (el/details {:class "memory-file-menu"}
                        (el/summary {:aria-label "File and workspace actions" :title "File and workspace actions"} "⋯")
                        (el/div {:class "memory-file-menu-content"}
                                (when markdown?
                                  (el/button {:on-click (fn [_] ((:open-properties a) path {:kind :page}))} "Properties"))
                                (when draft
                                  (el/button {:on-click (fn [_] ((:reload a) path))} "Check for changes"))
                                (when (get-in draft [:source :entry])
                                  (if (get-in draft [:source :media])
                                    (when coherent?
                                      (el/a {:href (links/media-route {:room (:room-id data) :path path
                                                                       :content (get-in draft [:source :entry :content])
                                                                       :target (get-in draft [:source :target-stamp])})
                                             :download (last (str/split path #"/"))} "Download"))
                                    (el/button {:disabled (not coherent?) :on-click (fn [_] ((:download a) path))} "Download")))
                                (when (and draft write?)
                                  (el/button {:disabled (or (editor/dirty? draft) (editor/property-drafts? state path)
                                                            (declarations/drafts? state path) (get-in state [:loading [:property-save path]]))
                                              :on-click (fn [_] (prompt-path! "Move file to" path #((:move a) path %)))} "Move"))
                                (el/hr {})
                                (when workspace-write?
                                  (el/button {:on-click (fn [_] (prompt-path! "New file path (relative to the room)"
                                                                            (if (seq folder) (str folder "/note.md") "wiki/note.md")
                                                                            #(navigation/create-file! (:room-id data) (:room-name data) % (:col-id data) (:target-stamp state))))} "New file"))
                                (when workspace-write?
                                  (el/button {:on-click (fn [_] (prompt-path! "New folder path (relative to the room)"
                                                                            (str (when (seq folder) (str folder "/")) "new-folder") (:mkdir a)))} "New folder"))
                                (when workspace-write?
                                  (el/label {:class "memory-upload"} "Upload file"
                                            (el/input {:type "file" :aria-label "Upload to Memory"
                                                       :on-change (fn [event]
                                                                    (when-let [file (aget (.. event -target -files) 0)] ((:upload a) folder file))
                                                                    (set! (.. event -target -value) ""))})))
                                (el/button {:on-click (fn [_] ((:refresh a)))} "Refresh workspace")
                                (when-let [content (get-in draft [:source :entry :content])]
                                  (el/details {:class "memory-properties"}
                                              (el/summary {} "Saved file revision") (el/code {} (str content)))))))))

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
        office? (= :office (get-in draft [:source :kind]))]
    (el/div {:class "memory-panel"}
            (file-header data state a draft workspace-write? write? coherent? syntax-pref)
            (when (get-in state [:loading :reference])
              (el/p {:class "memory-note" :role "status"} "Locating block in the current workspace…"))
            (when-let [error (get-in state [:errors :reference])]
              (el/div {:class "proposal-error" :role "alert"}
                      error (el/button {:on-click (fn [_] ((:retry-reference a)))} "Try block link again")))
            (when (and (= :resolved (get-in state [:block-reference :reply :status]))
                       (not (get-in state [:block-reference :applied?])) draft)
              (el/div {:class "memory-note" :role "status"}
                      (if (editor/dirty? draft)
                        "Your draft is retained. Save or reconcile it before jumping to the linked block."
                        (if ((:jump a) path)
                          (el/button {:on-click (fn [_] ((:mode a) path :source))} "Show linked block in Markdown")
                          (el/button {:on-click (fn [_] ((:retry-reference a)))} "File changed · locate block again")))))
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
                                      (when (and coherent? (= (:target-stamp state) (get-in state [:download :target-stamp]))
                                                 (= path (get-in state [:download :path])))
                                        (download/view [(:owner a) path] (last (str/split path #"/"))
                                                       (get-in state [:download :base64])))
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
                                      (retained-declarations-view path state a)
                                      (local-source-view path draft state a write?)
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
                                        (get-in draft [:source :media])
                                        (let [kind (get-in draft [:source :kind])
                                              url (when coherent?
                                                    (links/media-route {:room (:room-id data) :path path
                                                                        :content (get-in draft [:source :entry :content])
                                                                        :target (get-in draft [:source :target-stamp])}))]
                                          (if url
                                            (el/div {:class "memory-media"}
                                                    (case kind
                                                      :image (el/img {:src url :alt path :style {:max-width "100%" :max-height "70vh" :object-fit "contain"}})
                                                      :video (el/video {:src url :controls true :preload "metadata" :style {:max-width "100%" :max-height "70vh"}})
                                                      :audio (el/audio {:src url :controls true :preload "metadata"}))
                                                    (el/a {:href url :download (last (str/split path #"/"))} "Download original"))
                                            (el/p {:class "memory-note"} "Reopen this media file in the current workspace.")))
                                        office?
                                        (office-view/view {:path path :draft draft :state state :actions a :write? write? :coherent? coherent?})
                                        (not text?) (el/p {:class "memory-note"} "Binary file. Download preserves the original bytes; document-specific editing is not available in this view yet.")
                                        (and clojure? (= :superficie code-view)) (superficie-view (:text draft))
                                        (= :source (:mode draft)) (source-view path draft a write?)
                                        :else (document-view path draft a write?
                                                             (when coherent?
                                                               (attachments/urls (:room-id data) path (:analysis draft)
                                                                                 (:observed-entries state) (:target-stamp state)))
                                                             {:room (:room-id data) :path path :enabled? coherent?})))))))))

(defn render [data]
  (spin
   (let [connection (iv/get-new (track sig/memory-connection))
         stream (iv/get-new (track sync/state))
         _metadata (iv/get-new (track sig/memory-room-metadata))
         _layout (iv/get-new (track sig/layout-columns))
         rooms (iv/get-new (track sig/user-rooms))
         room-name (or (some (fn [room]
                              (when (= (str (:room-id data)) (str (:room/id room)))
                                (not-empty (:room/name room))))
                            (:rooms rooms))
                       (not-empty (:room-name data))
                       (str "Memory · " (:room-id data)))
         states (iv/get-new (track sig/memory-editor-state))
         syntax-pref (iv/get-new (track sig/syntax-pref))
         a (actions/controller data) state (get states (:owner a))]
     ((:ensure-path a))
     ((:ensure-sync a) connection stream)
     ((:ensure-properties a))
     ((:ensure-charts a))
     (view (assoc data :room-name room-name) state a syntax-pref))))

(defn- backlinks-view [a]
  (let [{:keys [open? reply error loading?]} ((:backlinks a))]
    (el/details {:class "memory-backlinks" :open open?
                 :on-toggle (fn [event] ((:backlinks-open a) (.. event -target -open)))}
                (el/summary {} "Backlinks")
                (when open?
                  (el/div {}
                          (el/p {:class "memory-note"} "Links from saved pages in this workspace.")
                          (cond
                            error (el/div {:class "proposal-error" :role "status"}
                                          error (el/button {:on-click (fn [_] ((:retry-backlinks a)))} "Try again"))
                            (or loading? (nil? reply)) (el/p {:class "memory-note" :role "status"} "Checking saved links…")
                            (= :missing (:status reply)) (el/p {:class "memory-note"} "This page is not in the saved workspace.")
                            (empty? (:results reply)) (el/p {:class "memory-note"} "No saved Simmis links to this page yet."))
                          (when (and reply (not (:complete? reply)))
                            (el/p {:class "memory-note"} "Only part of the matching links fits in this view."))
                          (for [result (:results reply)]
                            (el/div {:key [(:path result) (:from result)]}
                                    (el/a {:href (:url result) :title (:path result)
                                           :on-click (fn [event]
                                                       (when-not (or (.-metaKey event) (.-ctrlKey event) (.-shiftKey event) (.-altKey event))
                                                         (.preventDefault event) ((:open-backlink a) result)))}
                                          (:path result)
                                          (when (:block-id result) " · block")))))))))

(defn render-context [data]
  (spin
   (let [connection (iv/get-new (track sig/memory-connection))
         stream (iv/get-new (track sync/state))
         _metadata (iv/get-new (track sig/memory-room-metadata))
         _layout (iv/get-new (track sig/layout-columns))
         _rooms (iv/get-new (track sig/user-rooms))
         states (iv/get-new (track sig/memory-editor-state))
         syntax-pref (iv/get-new (track sig/syntax-pref))
         a (actions/controller data) state (get states (:owner a)) path (:selected state)
         draft (get-in state [:drafts path])
         coherent? (and (actions/verified? state connection) (not (:target-stale? draft))
                        (= (:target-stamp state) (get-in draft [:source :target-stamp])))
         history (get-in state [:history path]) preview (get-in state [:history-preview path])]
     ((:ensure-path a))
     ((:ensure-sync a) connection stream)
     ((:ensure-backlinks a))
     (el/div {:class "memory-history context-section"}
             (when (and path (or (= :markdown (get-in draft [:source :kind]))
                                 (re-find #"(?i)\.(clj|cljc|bb)$" path)))
               (repl-view state a syntax-pref))
             (when (= :markdown (get-in draft [:source :kind])) (backlinks-view a))
             (el/div {:class "context-section-title"} "File history")
             (when (seq (:evaluation-history state))
               (el/details {:class "memory-evaluation-history"}
                           (el/summary {} "Recent file evaluations")
                           (el/p {:class "memory-note"} "Last 20 receipts in this editor session, including earlier drafts and moved files. These are execution observations, not saved file revisions.")
                           (for [receipt (:evaluation-history state)]
                             (el/details {:key (str (:id receipt))}
                                         (el/summary {} (str (:path receipt) " · " (if (:success receipt) "Result" "Failed")
                                                             (when (:stale? receipt) " · draft changed before receipt arrived")))
                                         (el/pre {} (if (:success receipt) (:value receipt) (:error receipt)))
                                         (when (false? (:worker-finished? receipt))
                                           (el/p {:class "memory-note"} "Worker exit had not been confirmed when this receipt arrived. Earlier forms may have executed."))
                                         (when (seq (:stdout receipt)) (el/pre {} (str "stdout\n" (:stdout receipt))))
                                         (when (seq (:stderr receipt)) (el/pre {} (str "stderr\n" (:stderr receipt))))
                                         (el/details {} (el/summary {} "Evaluated source") (el/pre {} (:code receipt)))))))
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
