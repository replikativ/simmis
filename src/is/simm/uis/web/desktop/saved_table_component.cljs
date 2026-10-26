(ns is.simm.uis.web.desktop.saved-table-component
  "Typed saved-report presentation. Data never supplies callbacks or edit
   bindings; trusted host callers optionally supply navigation and controls."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.documents.links :as links]
            [is.simm.documents.saved-view-values :as values]))

(defn media-url [value context]
  (links/media-route (merge (select-keys context [:room :target]) (select-keys value [:path :content]))))

(defn page-link [path text a]
  (if-let [open (:open a)]
    (el/button {:type "button" :class "memory-page-link" :title path
                :on-click (fn [_] (open path))} text)
    (el/span {:title path} text)))

(defn cell [value a]
  (case (:kind value)
    :null (el/span {:class "memory-note"} "—")
    :text (el/span {} (:text value) (when (false? (:complete? value)) "…"))
    (:number :interval) (el/span {} (values/scalar-text value))
    (:uuid :instant) (el/span {} (:text value))
    :boolean (el/span {} (if (:value value) "Yes" "No"))
    :page (page-link (:path value) (:text value) a)
    :media (if-let [url (media-url value (:media-context a))]
             (el/a {:href url :target "_blank" :rel "noopener noreferrer"} (:text value))
             (el/span {} (:text value)))
    :reference (if (:path value)
                 (page-link (:path value) (:label value) a)
                 (el/span {:title (str (:id value))} "Reference"))
    :list (el/span {}
                   (for [[index item] (map-indexed vector (:items value))]
                     (el/span {:key index} (when (pos? index) ", ") (cell item a)))
                   (when (false? (:complete? value)) "…"))
    :truncated (el/span {:class "memory-note"} "Preview omitted")
    :unresolved-reference (el/span {:class "memory-note"} "Unresolved reference")
    (el/span {:class "memory-note"} "Source value")))

(defn view [columns rows navigation {:keys [control-label row-control]}]
  (el/div {:class "memory-data-table"}
          (el/table {}
                    (el/thead {}
                              (el/tr {}
                                     (for [[index column] (map-indexed vector columns)]
                                       (el/th {:key index :scope "col"} (:label column)))
                                     (when row-control (el/th {:scope "col"} control-label))))
                    (el/tbody {}
                              (for [row rows]
                                (el/tr {:key (str (:id row))}
                                       (for [[index value] (map-indexed vector (:cells row))]
                                         (el/td {:key index} (cell value navigation)))
                                       (when row-control (el/td {} (row-control row)))))))))
