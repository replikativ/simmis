(ns is.simm.uis.web.desktop.room-actions)

(defn tab-render-data
  "Attach the identity of the tab and column that own a rendered action.

   A content renderer is reusable, while its button must act on the exact tab
   instance that produced it. Room identity, a title, and a position are all
   shared or mutable; this pair is not."
  [col-id tab]
  (assoc (:data tab) :col-id col-id :tab-id (:id tab)))

(defn add-tab-to-column
  "Append `tab` and make it active in the explicitly identified column.

   Callers resolve the target from the UI that owns the action. This small,
   pure layout operation deliberately does not consult active-column-id."
  [columns col-id tab]
  (let [target-idx (or (first (keep-indexed (fn [idx column]
                                               (when (= (:id column) col-id) idx))
                                             columns))
                       0)]
    (update-in columns [target-idx]
               (fn [column]
                 (-> column
                     (update :tabs conj tab)
                     (assoc :active-tab (:id tab)))))))

(defn close-tab-in-layout
  "Remove exactly `tab-id` from exactly `col-id`, preserving layout invariants.

   `default-layout` is supplied by the caller so this remains a pure layout
   operation. It deliberately never searches tab content: the caller already
   has the identity of the button's owning tab."
  [columns col-id tab-id default-layout]
  (if-let [col-idx (first (keep-indexed (fn [idx column]
                                          (when (= (:id column) col-id) idx))
                                        columns))]
    (let [column (nth columns col-idx)
          remaining-tabs (vec (remove #(= (:id %) tab-id) (:tabs column)))]
      (if (empty? remaining-tabs)
        (let [new-columns (vec (concat (subvec columns 0 col-idx)
                                        (subvec columns (inc col-idx))))
              n (count new-columns)]
          (if (zero? n)
            (default-layout)
            (mapv #(assoc % :width (/ 1.0 n)) new-columns)))
        (let [new-active (if (= (:active-tab column) tab-id)
                           (:id (first remaining-tabs))
                           (:active-tab column))]
          (assoc-in columns [col-idx]
                    (assoc column :tabs remaining-tabs :active-tab new-active)))))
    columns))

(defn room-header-tab
  "Describe a room-header tab action with the column that rendered it.

   The header button fires before the enclosing column's click handler, so its
   destination must be carried from the render path rather than inferred from
   the currently active column."
  [col-id action room-id room-name]
  (case action
    :screens [:screens {:room-id room-id :room-name room-name}
              {:title (str room-name " Screens") :new-tab? true :col-id col-id}]
    :video [:video {:room-id room-id :room-name room-name}
            {:title (str room-name " Call") :new-tab? true :col-id col-id}]
    :files [:files {:room-id room-id}
            {:title (str room-name " Files") :new-tab? true :col-id col-id}]
    :settings [:room-settings {:room-id room-id}
               {:title (str room-name " Settings") :new-tab? true :col-id col-id}]))
