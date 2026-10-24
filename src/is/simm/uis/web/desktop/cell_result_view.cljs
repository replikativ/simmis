(ns is.simm.uis.web.desktop.cell-result-view
  "Read-only projections of host-validated saved-cell results. No user DOM,
   attributes, handlers or objects cross this boundary."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.chart-view :as charts]))

(defn chart? [presentation] (contains? #{:chart "chart"} (:kind presentation)))

(defn- chart-data [data]
  (update data :kind #(case % "line" :line "bar" :bar "point" :point %)))

(defn table-view [presentation]
  (el/div {:class "memory-cell-table"}
          (el/table {}
                    (el/thead {} (el/tr {} (for [[i label] (map-indexed vector (:columns presentation))]
                                            (el/th {:key i :scope "col"} label))))
                    (el/tbody {} (for [[i row] (map-indexed vector (:rows presentation))]
                                   (el/tr {:key i}
                                          (for [[j value] (map-indexed vector row)]
                                            (el/td {:key j} value))))))))

(defn paint! [^js container presentation]
  (.replaceChildren container)
  (set! (.-className container) "memory-cell-presentation")
  (when presentation
    (if (chart? presentation)
      (charts/paint! container (chart-data (:data presentation)))
      (let [table (.createElement js/document "table") head (.createElement js/document "thead")
            heading (.createElement js/document "tr") body (.createElement js/document "tbody")]
        (doseq [label (:columns presentation)]
          (let [cell (.createElement js/document "th")]
            (.setAttribute cell "scope" "col") (set! (.-textContent cell) label) (.appendChild heading cell)))
        (.appendChild head heading)
        (doseq [values (:rows presentation)]
          (let [row (.createElement js/document "tr")]
            (doseq [value values]
              (let [cell (.createElement js/document "td")]
                (set! (.-textContent cell) value) (.appendChild row cell)))
            (.appendChild body row)))
        (.append table head body)
        (set! (.-className container) "memory-cell-table")
        (.appendChild container table)))))
