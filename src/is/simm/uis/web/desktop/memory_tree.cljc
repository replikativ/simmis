(ns is.simm.uis.web.desktop.memory-tree
  "Pure room-file navigation helpers; paths are identities, not display labels."
  (:require [clojure.string :as str]))

(defn presentation
  "Navigation hints only, never type assignments, executable meaning or access
   authority. Markdown participates in knowledge at ANY repository path."
  [{:keys [path kind]}]
  (let [path (str/lower-case (or path ""))]
    (cond
      (= :folder kind)
      (if (= "wiki" (last (str/split path #"/")))
        {:role :knowledge :icon "book-open" :label "Wiki folder (organizational convention)"}
        {:role :folder :icon "folder" :label "Folder"})
      (re-find #"\.(md|markdown)$" path)
      {:role :knowledge :icon "book-open" :label "Markdown knowledge document"}
      (re-find #"\.(clj|cljc|cljs)$" path)
      {:role :code :icon "file-code" :label "Clojure source"}
      :else {:role :file :icon "file" :label "File"})))

(defn breadcrumbs [folder]
  (mapv (fn [index part] {:label part :path (str/join "/" (take (inc index) (str/split folder #"/")))})
        (range) (remove str/blank? (str/split (or folder "") #"/"))))

(defn active-folders
  "Listings currently rendered or explicitly requested. Expansion flags below
   a collapsed ancestor do not make that subtree visible."
  [state]
  (let [focus (or (:focus-folder state) "")
        visible (loop [pending (if (:root-expanded? state) [focus] []) seen #{}]
                  (if-let [path (peek pending)]
                    (if (contains? seen path) (recur (pop pending) seen)
                        (let [children (keep (fn [{:keys [path kind]}]
                                               (when (and (= :folder kind) (get-in state [:expanded path])) path))
                                             (get-in state [:folders path :entries]))]
                          (recur (into (pop pending) children) (conj seen path))))
                    seen))
        pending (keep (fn [[slot loading?]]
                        (when (and loading? (vector? slot) (= :folder (first slot))) (second slot)))
                      (:loading state))]
    (into #{""} (concat visible (keys (:folder-intents state)) pending))))

(defn trim-folder-cache
  "Retain at most 64 listings, except an oversized active set which the request
   limit reports explicitly. Never evict visible or in-flight listings."
  [state]
  (let [active (active-folders state)
        pinned (select-keys (:folders state) active)
        spare (take (max 0 (- 64 (count pinned)))
                    (sort (remove active (keys (:folders state)))))]
    (assoc state :folders (merge pinned (select-keys (:folders state) spare)))))

(defn invalidate-closed-folders [state]
  (update state :folders select-keys (active-folders state)))

(defn sidebar-width [width viewport]
  (int (max 180 (min 640 (* viewport 0.65) width))))

(defn file-tab [columns col-id room path]
  (some (fn [column]
          (when (= col-id (:id column))
            (some (fn [tab] (when (and (= :memory (:type tab)) (= (str room) (str (get-in tab [:data :room-id])))
                                       (= path (get-in tab [:data :path]))) tab)) (:tabs column)))) columns))
