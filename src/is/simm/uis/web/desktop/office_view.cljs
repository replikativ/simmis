(ns is.simm.uis.web.desktop.office-view
  "Native inert document reading and the existing explicit XML-part editor."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [org.replikativ.spindel.dom.addressing :as addressing]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.uis.web.desktop.source-editor :as source]))

(declare block-view)

(defn- children [blocks options]
  (mapv (fn [index block] (addressing/with-slot-fn index #(block-view block options))) (range) blocks))

(defn- text-view [run {:keys [tokens on-correct]}]
  (if (= :placeholder (:kind run))
    (el/span {:class "office-placeholder" :role "note"} (str "[" (:text run) "]"))
    (let [token (get tokens (:source-index run))
          editable? (and token on-correct)
          attrs {:class (str "office-text"
                          (when (get-in run [:marks :bold]) " office-bold")
                          (when (get-in run [:marks :italic]) " office-italic")
                          (when (get-in run [:marks :strike]) " office-strike")
                          (when (get-in run [:marks :underline]) " office-underline"))}]
      (if editable?
        (el/button {:class "office-correctable" :type "button" :title "Correct this text"
                    :on-click (fn [_] (on-correct token (:text run)))} (el/span {:class (:class attrs)} (:text run)))
        (el/span {:class (:class attrs)} (:text run))))))

(defn- paragraph-view [block options]
  (let [content (fn []
                  (into (if (:list? block)
                          [(addressing/with-slot-fn 0 #(el/span {:class "office-list-indicator" :aria-label "List item; numbering not rendered"} "• "))]
                          [])
                        (if (seq (:runs block))
                          (mapv (fn [index run] (addressing/with-slot-fn (inc index) #(text-view run options))) (range) (:runs block))
                          [(addressing/with-slot-fn 1 #(el/br {}))])))]
    (case (:heading block)
      1 (el/h1 {} (content))
      2 (el/h2 {} (content))
      3 (el/h3 {} (content))
      4 (el/h4 {} (content))
      5 (el/h5 {} (content))
      6 (el/h6 {} (content))
      (el/p {:class "office-paragraph"} (content)))))

(defn- table-view [block options]
  (el/div {:class "office-table-wrap"}
          (el/table {:aria-label "Document table"}
                    (el/tbody {}
                              (mapv
                               (fn [ri row]
                                 (addressing/with-slot-fn
                                   ri
                                   #(el/tr {}
                                           (mapv
                                            (fn [ci cell]
                                              (addressing/with-slot-fn
                                                ci
                                                (fn []
                                                  (if (:header? row)
                                                    (el/th {:scope "col"} (children (:blocks cell) options))
                                                    (el/td {} (children (:blocks cell) options))))))
                                            (range) (:cells row)))))
                               (range) (:rows block))))))

(defn block-view
  ([block] (block-view block nil))
  ([block options]
   (case (:kind block)
     :paragraph (paragraph-view block options)
     :table (table-view block options)
     (el/p {:class "office-placeholder" :role "note"} (str "[" (:text block) "]")))))

(defn reading-view
  ([reading] (reading-view reading nil))
  ([reading options]
  (el/div {:class "office-document" :aria-label "Saved Word document"}
          (if (= :ready (:status reading))
            (el/div {}
                    (el/p {:class "memory-note"} (:notice reading))
                    (el/div {:class "office-body"} (children (:blocks reading) options)))
            (el/p {:class "memory-note"} "Simplified reading is not available for this document. The original file and Advanced XML remain available."))
          (when (seq (:diagnostics reading))
            (el/div {:class "office-diagnostics" :aria-label "Document reading notes"}
                    (for [note (:diagnostics reading)] (el/p {:key note :class "memory-note"} note)))))))

(defn- correction-form [path draft actions busy? write?]
  (when-let [correction (:office-correction draft)]
    (el/div {:class "office-correction-form" :role "group" :aria-label "Correct Word text"}
            (el/label {} "Correct selected text"
                      (el/input {:type "text" :aria-label "Replacement text" :value (:text correction) :maxlength 10000
                                 :disabled (or busy? (not write?))
                                 :on-input (fn [e] ((:change actions) path assoc-in [:office-correction :text] (.. e -target -value)))}))
            (when-not (editor/office-correction-current? draft)
              (el/p {:class "memory-note" :role "status"} "The saved document or XML draft changed. Copy your correction and select the current saved text again."))
            (el/button {:disabled (or busy? (not write?) (not (editor/office-correction-current? draft))
                                      (= (:text correction) (:original correction)))
                        :on-click (fn [_] ((:office-correct actions) path))} "Apply correction")
            (el/button {:disabled busy? :on-click (fn [_] ((:change actions) path dissoc :office-correction))} "Cancel"))))

(defn view [{:keys [path draft state actions write? coherent?]}]
  (let [office (:office draft) reading (:reading office)
        advanced? (or (nil? reading) (= :xml (:office-mode draft)))
        loading? (get-in state [:loading [:office path]])
        correcting? (get-in state [:loading [:office-correct path]])
        correction? (and write? coherent? (not correcting?) (not (editor/dirty? draft))
                         (= (:reading-entry office) (editor/office-reading-entry draft))
                         (not (get-in state [:loading [:office-save path]])))
        on-correct (when correction?
                     (fn [token text] ((:change actions) path assoc :office-correction
                                      {:path path :token token :text text :original text :entry (:reading-entry office)})))]
    (el/div {:class "memory-office"}
            (if-not office
              (if loading?
                (el/p {:role "status" :class "memory-note"} "Opening document…")
                (el/button {:disabled (not coherent?) :on-click (fn [_] ((:office-open actions) path nil))} "Open document"))
              (el/div {}
                      (when reading
                        (el/div {:class "office-mode-controls" :role "group" :aria-label "Office view"}
                                (el/button {:aria-pressed (if advanced? "false" "true")
                                            :on-click (fn [_] ((:change actions) path assoc :office-mode :document))} "Document")
                                (el/button {:aria-pressed (if advanced? "true" "false")
                                            :on-click (fn [_] ((:change actions) path assoc :office-mode :xml))} "Advanced XML")))
                      (correction-form path draft actions correcting? (and write? coherent?))
                      (if advanced?
                        (el/div {}
                                (el/p {:class "memory-note"} "XML part editing. Other part contents are preserved. Edited signed documents may require re-signing.")
                                (el/select {:aria-label "Office XML part" :value (or (:part office) "")
                                            :disabled (or correcting? (not coherent?) (editor/dirty? draft) (get-in state [:loading [:office-save path]]))
                                            :on-change (fn [e] ((:office-open actions) path (.. e -target -value)))}
                                           (when-not (:part office) (el/option {:value "" :disabled true} "Select an XML part"))
                                           (for [part (:parts office) :when (:editable? part)]
                                             (el/option {:key (:name part) :value (:name part)} (:name part))))
                                (when-let [error (:xml-error office)] (el/p {:class "memory-note"} error))
                                (when (string? (:text office))
                                  (source/view {:id [(:owner actions) path (:part office)] :path (:part office)
                                                :label "Office XML part source" :value (:text office)
                                                :selection (:selection office) :read-only? (or correcting? (not write?))
                                                :on-change (fn [text selection]
                                                             ((:change actions) path update :office assoc :text text :selection selection))}))
                                (el/button {:disabled (or correcting? (not write?) (not (string? (:text office))) (= (:text office) (:saved-text office))
                                                          (get-in state [:loading [:office-save path]]))
                                            :on-click (fn [_] ((:office-save actions) path))} "Save XML part"))
                        (el/div {}
                                (when (not= (:text office) (:saved-text office))
                                  (el/p {:class "memory-note" :role "status"} "Unsaved XML changes are retained and are not shown in this saved-document view."))
                                (when (not= (:reading-entry office) (editor/office-reading-entry draft))
                                  (el/p {:class "memory-note" :role "status"} "This preview belongs to an earlier saved revision."))
                                (when (get-in state [:loading [:office-reading path]])
                                  (el/p {:class "memory-note" :role "status"} "Updating saved document…"))
                                (when (and on-correct (seq (get-in reading [:corrections :tokens])))
                                  (el/p {:class "memory-note"} "Click ordinary text to make a selective correction."))
                                (reading-view reading {:tokens (get-in reading [:corrections :tokens]) :on-correct on-correct}))))))))
