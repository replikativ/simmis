(ns is.simm.uis.value-view
  "Shared display of bounded typed values. Host adapters optionally provide
   navigation; data never supplies callbacks, DOM attributes or edit bindings."
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
    :missing (el/span {:class "memory-note"} "Not provided")
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
                 (el/span {:title (str (:id value))}
                          (if (string? (:label value)) (:label value) "Reference")))
    :list (el/span {}
                   (for [[index item] (map-indexed vector (:items value))]
                     (el/span {:key index} (when (pos? index) ", ") (cell item a)))
                   (when (false? (:complete? value)) "…"))
    :truncated (el/span {:class "memory-note"} "Preview omitted")
    :source-value (el/code {:class "memory-source-value"} (:text value) (when (false? (:complete? value)) "…"))
    :unavailable-value (el/span {:class "memory-note"} "Value unavailable")
    :unresolved-reference (el/span {:class "memory-note"}
                                    (case (:reason value)
                                      :lookup-unavailable "Reference lookup unavailable"
                                      :lookup-incomplete "Reference lookup incomplete"
                                      "Unresolved reference"))
    (el/span {:class "memory-note"} "Source value")))
