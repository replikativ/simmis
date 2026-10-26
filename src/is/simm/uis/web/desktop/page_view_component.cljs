(ns is.simm.uis.web.desktop.page-view-component
  "A fixed host table/form over saved-view DTOs. Controls stay outside the
   contenteditable document; the original fence remains ordinary Markdown."
  (:require [clojure.string :as str]
            [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.uis.web.desktop.memory-page-views :as views]
            [is.simm.uis.web.desktop.memory-declarations :as declarations]
            [is.simm.uis.web.desktop.memory-edit-owners :as edit-owners]
            [is.simm.uis.web.desktop.property-controls :as properties]))

(defn- file-link [path text a]
  (el/button {:type "button" :class "memory-page-link" :title path
              :on-click (fn [_] ((:open a) path))} text))

(defn- cell [value a]
  (case (:kind value)
    :null (el/span {:class "memory-note"} "—")
    :text (el/span {} (:text value) (when (false? (:complete? value)) "…"))
    (:number :uuid :instant) (el/span {} (:text value))
    :boolean (el/span {} (if (:value value) "Yes" "No"))
    :page (file-link (:path value) (:text value) a)
    :reference (if (:path value)
                 (file-link (:path value) (:label value) a)
                 (el/span {:title (str (:id value))} "Reference"))
    :list (el/span {}
                   (for [[index item] (map-indexed vector (:items value))]
                     (el/span {:key index} (when (pos? index) ", ") (cell item a)))
                   (when (false? (:complete? value)) "…"))
    :truncated (el/span {:class "memory-note"} "Preview omitted")
    :unresolved-reference (el/span {:class "memory-note"} "Unresolved reference")
    (el/span {:class "memory-note"} "Source value")))

(defn- unavailable [reason]
  (case reason
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
                  (el/div {:class "memory-page-view-heading"}
                          (el/strong {} (or (:title row) "Selected page"))
                          (when identity? (file-link row-path "Open page" a))
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
                            (for [field fields]
                              (el/div {:key (str (:id field))}
                                      (properties/field-view row-path target field model state a editable?)))))))))

(defn view [path state a write?]
  (for [entry (views/entries state path)
        :let [id (:view entry) key [path id] opened? (get-in state [:page-view-open key])
              reply (views/visible state path id) loading? (get-in state [:loading [:page-view path id]])
              error (get-in state [:errors [:page-view path id]])
              form? (= :form (get-in entry [:descriptor :presentation]))]]
    (el/section {:key (str (:from entry) ":" id) :class "memory-page-view" :aria-label "Saved page view"}
                (el/div {:class "memory-page-view-heading"}
                        (el/strong {} (if form? "Page form" "Page table"))
                        (when (= :ready (:status entry))
                          (el/span {:class "memory-note"}
                                   (str/join ", " (map #(if (= "" %) "Memory" %) (get-in entry [:descriptor :source :roots])))))
                        (if (= :ready (:status entry))
                          (el/button {:type "button" :aria-expanded (boolean opened?)
                                      :on-click (fn [_] ((:toggle-page-view a) path id))}
                                     (if opened? "Hide view" "Open view"))
                          (el/span {:class "memory-note"} "Invalid declaration · edit its Markdown")))
                (when opened?
                  (cond
                    (and (nil? reply) error)
                    (el/p {:class "memory-note" :role "status"}
                          "View unavailable. "
                          (el/button {:on-click (fn [_] ((:retry-page-view a) path id))} "Try again"))
                    (nil? reply) (el/p {:class "memory-note" :role "status"} "Loading view…")
                    (= :unavailable (:status reply))
                    (el/p {:class "memory-note" :role "status"} (unavailable (:reason reply)))
                    :else
                    (el/div {}
                            (el/p {:class "memory-note" :role "status"}
                                  (str (count (:rows reply)) " of " (:total reply) " matching pages"
                                       (when loading? " · Updating…")))
                            (when-not form?
                              (el/div {:class "memory-page-table"}
                                      (el/table {}
                                                (el/thead {} (el/tr {}
                                                                  (for [[index column] (map-indexed vector (:columns reply))]
                                                                    (el/th {:key index :scope "col"} (:label column)))
                                                                  (el/th {:scope "col"} "Properties")))
                                                (el/tbody {}
                                                          (for [row (:rows reply)]
                                                            (el/tr {:key (str (:id row))}
                                                                   (for [[index value] (map-indexed vector (:cells row))]
                                                                     (el/td {:key index} (cell value a)))
                                                                   (el/td {} (el/button {:type "button"
                                                                                        :aria-label (str "Inspect properties for " (:title row))
                                                                                        :on-click (fn [_] ((:edit-page-view-row a) path id row))}
                                                                                       (if write? "Edit" "Inspect")))))))))
                            (when (or (false? (get-in reply [:display :complete?]))
                                      (some #(false? (:label-complete? %)) (:columns reply)))
                              (el/p {:class "memory-note"} "Some previews are shortened. Open a page for its full source values."))
                            (row-form path id reply state a write?)))))))
