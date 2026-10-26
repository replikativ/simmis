(ns is.simm.uis.web.desktop.saved-table-component
  "Shared collection presentation. Navigation and row controls are supplied
   by the host, independently of the source-specific portable cell data."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.value-view :as value-view]))

;; Preserve existing call sites while values also serve other content hosts.
(def media-url value-view/media-url)
(def page-link value-view/page-link)
(def cell value-view/cell)

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
