(ns is.simm.uis.web.desktop.memory-owner
  "Pure semantic buffer identity and current layout location. No authority."
  (:refer-clojure :exclude [key]))

(defn key [session-id {:keys [room-id col-id tab-id]}]
  [session-id (str room-id) (if (some? tab-id) :tab col-id) tab-id])

(defn current-tab
  "A tab may move columns, close or be reused for another view/room. Refuse
   missing/ambiguous IDs and never let an old callback retarget an unrelated tab."
  [columns room-id tab-id]
  (when (some? tab-id)
    (let [matches (for [column columns tab (:tabs column) :when (= tab-id (:id tab))]
                    {:col-id (:id column) :tab tab})
          match (when (= 1 (count matches)) (first matches))
          tab (:tab match)]
      (when (and match (= :memory (:type tab)) (= (str room-id) (str (get-in tab [:data :room-id]))))
        match))))
