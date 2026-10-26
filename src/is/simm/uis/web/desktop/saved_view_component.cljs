(ns is.simm.uis.web.desktop.saved-view-component
  "A fixed host table/form over saved-view DTOs. Controls stay outside the
   contenteditable document; the original fence remains ordinary Markdown."
  (:require [clojure.string :as str]
            [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.uis.web.desktop.memory-saved-views :as views]
            [is.simm.uis.web.desktop.memory-declarations :as declarations]
            [is.simm.uis.web.desktop.memory-edit-owners :as edit-owners]
            [is.simm.uis.web.desktop.property-controls :as properties]
            [is.simm.uis.web.desktop.saved-table-component :as table]))

(defn- unavailable [reason]
  (case reason
    :scorecard-not-found "This Scorecard is absent from the current Room. No other Room was searched."
    :display-limit "This Scorecard exceeds the bounded report presentation size."
    (:archive-index-limit :archive-note-limit :archive-byte-limit)
    "These archive folders exceed the bounded view limits. Select fewer session folders."
    :invalid-source "Some selected documents need valid identities or properties before this view can be shown."
    :page-not-in-selection "This page is absent or outside the view’s folders or types."
    (:too-many-files :too-many-source-bytes :too-many-worktree-entries :too-many-syntax-nodes)
    "This view exceeds the current document limits. Narrow its source folders."
    :unsupported-column-property "This view needs ordinary source properties; some selected fields are managed elsewhere."
    "This saved view is unavailable. Inspect its Markdown and source files."))

(defn- row-form [path view reply state a write?]
  (when-let [selected (get-in state [:page-view-edits [path view]])]
    (let [row (some #(when (= (select-keys selected [:id :path]) (select-keys % [:id :path])) %) (:rows reply))
          row-path (:path selected) target {:kind :page} model (get-in state [:property-models [row-path target]])
          draft (get-in state [:drafts row-path])
          identity? (views/row-identity-current? state selected)
          current? (and identity? model ((:property-current? a) row-path target model))
          other-edits? ((:property-blocked? a) row-path)
          busy? (declarations/busy? state row-path)
          ids (set (keep :property (:columns reply)))
          fields (filterv #(contains? ids (:id %)) (:fields model))
          editable? (and row write? current? (not busy?) (not other-edits?)
                         (not (editor/dirty? (get-in state [:drafts path])))
                         (not (editor/dirty? draft)) (not (declarations/drafts? state row-path))
                         (= (:expected model) (get-in draft [:source :entry])))]
      (el/section {:class "memory-page-form" :aria-label "Selected page properties"}
                  (el/div {:class "memory-saved-view-heading"}
                          (el/strong {} (or (:title row) "Selected page"))
                          (when identity? (table/page-link row-path "Open page" a))
                          (el/span {:role "status" :class "memory-note"} (when busy? "Saving…")))
                  (when-let [message (get-in state [:page-view-edit-errors [path view]])]
                    (el/p {:class "memory-note" :role "status"} message))
                  (cond
                    (nil? row) (el/p {:class "memory-note" :role "status"}
                                    "This page is outside the current result or has moved. Your field drafts are retained.")
                    (get-in state [:errors [:file row-path]])
                    (el/p {:class "memory-note"} "This page could not be loaded. Its source was not changed. "
                          (el/button {:on-click (fn [_] ((:retry-page-view-row a) path view))} "Try again"))
                    (nil? draft) (el/p {:class "memory-note" :role "status"} "Loading page properties…")
                    (not identity?) (el/p {:class "memory-note" :role "status"}
                                          "The file no longer matches this page. Reload the view before editing.")
                    (not current?) (el/p {:class "memory-note" :role "status"} "Checking current property definitions…")
                    (= :unavailable (:status model)) (el/p {:class "memory-note"} "Typed properties are unavailable for this page.")
                    :else
                    (el/div {}
                            (when other-edits? (el/p {:class "memory-note" :role "status"} edit-owners/message))
                            (when (or (editor/dirty? draft) (editor/dirty? (get-in state [:drafts path])))
                              (el/p {:class "memory-note"} "Property changes pause while the file draft is saving."))
                            (when (empty? fields)
                              (el/p {:class "memory-note"} "This view has no property fields on this page. Open the page to manage its properties."))
                            (properties/fields-view row-path target fields model state a editable?)))))))

(defn view [path state a write?]
  (for [entry (views/entries state path)
        :let [id (:view entry) key [path id] opened? (get-in state [:page-view-open key])
              reply (views/visible state path id) loading? (get-in state [:loading [:saved-view path id]])
              error (get-in state [:errors [:saved-view path id]])
              pages? (views/pages? state path id)
              receipt? (views/receipt? state path id)
              form? (and pages? (= :form (get-in entry [:descriptor :presentation])))]]
    (el/section {:key (str (:from entry) ":" id) :class "memory-saved-view" :aria-label "Saved view"}
                (el/div {:class "memory-saved-view-heading"}
                        (el/strong {} (cond pages? (if form? "Page form" "Page table")
                                            receipt? "Benchmark results" :else "Screen observations"))
                        (when (= :ready (:status entry))
                          (el/span {:class "memory-note"}
                                   (str/join ", " (map #(if (= "" %) "Memory" %) (get-in entry [:descriptor :source :roots])))))
                        (if (= :ready (:status entry))
                          (el/button {:type "button" :aria-expanded (boolean opened?)
                                      :on-click (fn [_] ((:toggle-saved-view a) path id))}
                                     (if opened? "Hide view" "Open view"))
                          (el/span {:class "memory-note"} "Invalid declaration · edit its Markdown")))
                (when opened?
                  (cond
                    (and receipt? (nil? (:receipt-context state)))
                    (el/p {:class "memory-note" :role "status"} "Waiting for current Room access and receipt store identity…")
                    (and (nil? reply) error)
                    (el/p {:class "memory-note" :role "status"}
                          "View unavailable. "
                          (el/button {:on-click (fn [_] ((:retry-saved-view a) path id))} "Try again"))
                    (nil? reply) (el/p {:class "memory-note" :role "status"} "Loading view…")
                    (= :unavailable (:status reply))
                    (el/p {:class "memory-note" :role "status"} (unavailable (:reason reply)))
                    :else
                    (el/div {}
                            (el/p {:class "memory-note" :role "status"}
                          (str (count (:rows reply)) " of " (:total reply)
                               (cond pages? " matching pages" receipt? " candidates · saved Scorecard"
                                     :else " archived observation intervals")
                                       (when loading? " · Updating…")))
                            (when-not form?
                              (table/view (:columns reply) (:rows reply)
                                          (assoc a :media-context {:room (:room-id a) :target (:target-stamp state)})
                                          (when pages? {:control-label "Properties"
                                           :row-control
                                           (fn [row]
                                             (el/button {:type "button"
                                                         :aria-label (str "Inspect properties for " (:title row))
                                                         :on-click (fn [_] ((:edit-page-view-row a) path id row))}
                                                        (if write? "Edit" "Inspect")))})))
                            (when receipt?
                              (el/p {:class "memory-note"}
                                    "Model cost uses reported usage; it is not the wallet or grant total. A dash means unknown."
                                    (when (false? (:complete? reply)) " Only the declared candidate prefix is shown; matrix statistics are unchanged.")))
                            (when (and (not pages?) (not receipt?))
                              (el/p {:class "memory-note"}
                                    "Screenshots show matching files / referenced files. Recording alignment is estimated."
                                    (when (false? (:complete? reply)) " Only the declared interval prefix is shown.")))
                            (when (and pages? (or (false? (get-in reply [:display :complete?]))
                                                 (some #(false? (:label-complete? %)) (:columns reply))))
                              (el/p {:class "memory-note"} "Some previews are shortened. Open a page for its full source values."))
                            (when pages? (row-form path id reply state a write?))))))))
