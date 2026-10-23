(ns is.simm.uis.web.desktop.office-view
  "Native inert document reading and the existing explicit XML-part editor."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [org.replikativ.spindel.dom.addressing :as addressing]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.uis.web.desktop.source-editor :as source]))

(declare block-view)

(defn- children [blocks]
  (mapv (fn [index block] (addressing/with-slot-fn index #(block-view block))) (range) blocks))

(defn- text-view [run]
  (if (= :placeholder (:kind run))
    (el/span {:class "office-placeholder" :role "note"} (str "[" (:text run) "]"))
    (el/span {:class (str "office-text"
                          (when (get-in run [:marks :bold]) " office-bold")
                          (when (get-in run [:marks :italic]) " office-italic")
                          (when (get-in run [:marks :strike]) " office-strike")
                          (when (get-in run [:marks :underline]) " office-underline"))}
             (:text run))))

(defn- paragraph-view [block]
  (let [content (fn []
                  (into (if (:list? block)
                          [(addressing/with-slot-fn 0 #(el/span {:class "office-list-indicator" :aria-label "List item; numbering not rendered"} "• "))]
                          [])
                        (if (seq (:runs block))
                          (mapv (fn [index run] (addressing/with-slot-fn (inc index) #(text-view run))) (range) (:runs block))
                          [(addressing/with-slot-fn 1 #(el/br {}))])))]
    (case (:heading block)
      1 (el/h1 {} (content))
      2 (el/h2 {} (content))
      3 (el/h3 {} (content))
      4 (el/h4 {} (content))
      5 (el/h5 {} (content))
      6 (el/h6 {} (content))
      (el/p {:class "office-paragraph"} (content)))))

(defn- table-view [block]
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
                                                    (el/th {:scope "col"} (children (:blocks cell)))
                                                    (el/td {} (children (:blocks cell)))))))
                                            (range) (:cells row)))))
                               (range) (:rows block))))))

(defn block-view [block]
  (case (:kind block)
    :paragraph (paragraph-view block)
    :table (table-view block)
    (el/p {:class "office-placeholder" :role "note"} (str "[" (:text block) "]"))))

(defn reading-view [reading]
  (el/div {:class "office-document" :aria-label "Saved Word document"}
          (if (= :ready (:status reading))
            (el/div {}
                    (el/p {:class "memory-note"} (:notice reading))
                    (el/div {:class "office-body"} (children (:blocks reading))))
            (el/p {:class "memory-note"} "Simplified reading is not available for this document. The original file and Advanced XML remain available."))
          (when (seq (:diagnostics reading))
            (el/div {:class "office-diagnostics" :aria-label "Document reading notes"}
                    (for [note (:diagnostics reading)] (el/p {:key note :class "memory-note"} note))))))

(defn view [{:keys [path draft state actions write? coherent?]}]
  (let [office (:office draft) reading (:reading office)
        advanced? (or (nil? reading) (= :xml (:office-mode draft)))
        loading? (get-in state [:loading [:office path]])]
    (el/div {:class "memory-office"}
            (if-not office
              (if loading?
                (el/p {:role "status" :class "memory-note"} "Opening document…")
                (el/button {:disabled (not coherent?) :on-click (fn [_] ((:office-open actions) path nil))} "Open document"))
              (el/div {}
                      (when reading
                        (el/div {:class "office-mode-controls" :role "group" :aria-label "Office view"}
                                (el/button {:aria-pressed (not advanced?)
                                            :on-click (fn [_] ((:change actions) path assoc :office-mode :document))} "Document")
                                (el/button {:aria-pressed (boolean advanced?)
                                            :on-click (fn [_] ((:change actions) path assoc :office-mode :xml))} "Advanced XML")))
                      (if advanced?
                        (el/div {}
                                (el/p {:class "memory-note"} "XML part editing. Other part contents are preserved. Edited signed documents may require re-signing.")
                                (el/select {:aria-label "Office XML part" :value (or (:part office) "")
                                            :disabled (or (not coherent?) (editor/dirty? draft) (get-in state [:loading [:office-save path]]))
                                            :on-change (fn [e] ((:office-open actions) path (.. e -target -value)))}
                                           (when-not (:part office) (el/option {:value "" :disabled true} "Select an XML part"))
                                           (for [part (:parts office) :when (:editable? part)]
                                             (el/option {:key (:name part) :value (:name part)} (:name part))))
                                (when-let [error (:xml-error office)] (el/p {:class "memory-note"} error))
                                (when (string? (:text office))
                                  (source/view {:id [(:owner actions) path (:part office)] :path (:part office)
                                                :label "Office XML part source" :value (:text office)
                                                :selection (:selection office) :read-only? (not write?)
                                                :on-change (fn [text selection]
                                                             ((:change actions) path update :office assoc :text text :selection selection))}))
                                (el/button {:disabled (or (not write?) (not (string? (:text office))) (not (editor/dirty? draft))
                                                          (get-in state [:loading [:office-save path]]))
                                            :on-click (fn [_] ((:office-save actions) path))} "Save XML part"))
                        (el/div {}
                                (when (not= (:text office) (:saved-text office))
                                  (el/p {:class "memory-note" :role "status"} "Unsaved XML changes are retained and are not shown in this saved-document view."))
                                (when (not= (:reading-entry office) (editor/office-reading-entry draft))
                                  (el/p {:class "memory-note" :role "status"} "This preview belongs to an earlier saved revision."))
                                (when (get-in state [:loading [:office-reading path]])
                                  (el/p {:class "memory-note" :role "status"} "Updating saved document…"))
                                (reading-view reading))))))))
