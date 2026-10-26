(ns is.simm.documents.saved-view-values
  "Text presentation shared by host tables and portable report snapshots.")

(defn scalar-text [value]
  (case (:kind value)
    :null "—"
    :number (str (when (:approximate? value) "≈") (when (= :usd (:unit value)) "$") (:text value)
                 (when (= :percent (:unit value)) "%"))
    :interval (:text value)
    :text (str (:text value) (when (false? (:complete? value)) "…"))
    :boolean (if (:value value) "Yes" "No")
    :truncated "Preview omitted"
    nil))
