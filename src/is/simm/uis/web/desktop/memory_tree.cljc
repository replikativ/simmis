(ns is.simm.uis.web.desktop.memory-tree
  "Pure room-file navigation helpers; paths are identities, not display labels."
  (:require [clojure.string :as str]))

(defn breadcrumbs [folder]
  (mapv (fn [index part] {:label part :path (str/join "/" (take (inc index) (str/split folder #"/")))})
        (range) (remove str/blank? (str/split (or folder "") #"/"))))

(defn sidebar-width [width viewport]
  (int (max 180 (min 640 (* viewport 0.65) width))))

(defn file-tab [columns col-id room path]
  (some (fn [column]
          (when (= col-id (:id column))
            (some (fn [tab] (when (and (= :memory (:type tab)) (= (str room) (str (get-in tab [:data :room-id])))
                                      (= path (get-in tab [:data :path]))) tab)) (:tabs column)))) columns))
