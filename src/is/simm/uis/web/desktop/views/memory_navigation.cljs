(ns is.simm.uis.web.desktop.views.memory-navigation
  "Lazy room file trees in the navigation rail, independent of editor drafts."
  (:require [clojure.string :as str]
            [org.replikativ.spindel.dom.elements :as el]
            [org.replikativ.spindel.incremental.interval :as iv]
            [org.replikativ.spindel.effects.track :refer [track]]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.memory-tree :as tree]
            [is.simm.uis.web.desktop.memory-sync :as sync]
            [is.simm.uis.web.desktop.memory-actions :as actions]
            [is.simm.uis.web.desktop.views.core :as vc])
  (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]]))

(defn open-file! [room title path & [target-col target-stamp]]
  (let [ctx (or ec/*execution-context* runtime)]
    (binding [ec/*execution-context* ctx]
      (let [columns @sig/layout-columns col (or target-col (sig/get-active-column-id columns))]
        (if-let [tab (tree/file-tab columns col room path)]
          (do (sig/set-active-tab! col (:id tab))
              ((:observe-navigation (actions/controller {:room-id room :col-id col :tab-id (:id tab)})) target-stamp))
          (sig/open-tab! :memory {:room-id room :room-name title :path path :navigation-target-stamp target-stamp}
                         {:title (last (str/split path #"/")) :new-tab? true :col-id col}))))))

(defn create-file! [room title path col origin]
  (let [ctx (or ec/*execution-context* runtime)]
    (binding [ec/*execution-context* ctx]
      (if-not (uuid? origin)
        ((:discover (actions/controller {:room-id room :col-id :navigation :state-signal sig/memory-navigation-state})))
        (if (tree/file-tab @sig/layout-columns col room path)
          (open-file! room title path col origin)
          ;; Origin is present at tab birth, BEFORE a synchronous render may
          ;; request metadata. The fresh controller must discover independently.
          (sig/open-tab! :memory {:room-id room :room-name title :path path
                                  :create-origin-stamp origin :create-intent-id (random-uuid)}
                         {:title (last (str/split path #"/")) :new-tab? true :col-id col}))))))

(defn controller [room]
  (actions/controller {:room-id room :col-id :navigation :state-signal sig/memory-navigation-state}))

(defn focus-folder! [room folder] ((:focus-folder (controller room)) folder))

(declare folder-tree)

(defn- file-row [item state open! depth]
  (let [{:keys [role icon label]} (tree/presentation item)]
    (el/button {:key (:path item) :class (str "memory-tree-item memory-kind-" (name role)
                                              (when (= (:path item) (:selected state)) " selected"))
                :style {:padding-left (str (+ 12 (min 72 (* depth 10))) "px")}
                :title (str label " · " (:path item)) :on-click (fn [_] (open! (:path item)))}
               (vc/icon icon {:class "memory-file-kind" :aria-hidden "true"})
               (el/span {:class "memory-tree-label"} (:name item)))))

(defn- folder-tree [folder state a open! depth]
  (el/div {:class "memory-tree-branch"}
          (for [{:keys [path name kind] :as item} (get-in state [:folders folder :entries]) :when (not= name ".keep")]
            (if (not= :folder kind)
              (file-row item state open! depth)
              (let [expanded? (get-in state [:expanded path])
                    presentation (tree/presentation item)]
                (el/div {:key path}
                        (el/div {:class "memory-folder-row"}
                                (el/button {:class (str "memory-tree-item memory-kind-" (cljs.core/name (:role presentation)))
                                            :style {:padding-left (str (+ 8 (min 72 (* depth 10))) "px")}
                                            :title (str (:label presentation) " · " path) :aria-expanded (str (boolean expanded?))
                                            :on-click (fn [_] ((:toggle a) path))}
                                           (el/span {:aria-hidden "true"} (if expanded? "▾" "▸"))
                                           (vc/icon (:icon presentation) {:class "memory-file-kind" :aria-hidden "true"})
                                           (el/span {:class "memory-tree-label"} name))
                                (el/button {:class "memory-folder-focus" :title (str "Focus folder " path)
                                            :aria-label (str "Focus folder " path)
                                            :on-click (fn [_] ((:focus-folder a) path))} "↳"))
                        (when expanded?
                          (if (get-in state [:folders path]) (folder-tree path state a open! (inc depth))
                              (el/p {:class "memory-note"} "Loading…")))))))))

(defn render [room]
  (spin
   (let [connection (iv/get-new (track sig/memory-connection))
         stream (iv/get-new (track sync/state))
         _metadata (iv/get-new (track sig/memory-room-metadata))
         _layout (iv/get-new (track sig/layout-columns))
         _rooms (iv/get-new (track sig/user-rooms))
         states (iv/get-new (track sig/memory-navigation-state))
         id (:room/id room) title (:room/name room) a (controller id)
         state (get states (:owner a)) folder (or (:focus-folder state) "")
         open! (fn [path]
                 (if ((:verified? a))
                   (do ((:select-path a) path) (open-file! id title path nil (:target-stamp ((:state a)))))
                   ((:discover a))))
         search (when (= (:query state) (get-in state [:search :query])) (:search state))]
     (when (:root-expanded? state) ((:ensure-sync a) connection))
     ((:maintain-metadata a))
     (el/div {:key (str "workspace-" id) :class "nav-subsection memory-navigation"}
             (el/button {:class "nav-item memory-room-toggle" :aria-expanded (str (boolean (:root-expanded? state)))
                         :on-click (fn [_] ((:toggle-root a)))}
                        (el/span {} (if (:root-expanded? state) "▾" "▸"))
                        (el/span {:class "nav-item-label"} title))
             (when (:root-expanded? state)
               (el/div {:class "memory-nav-files"}
                       (el/div {:class "memory-file-search"}
                               (el/input {:aria-label (str "Find files in " title) :placeholder "Find file… (Enter)" :value (or (:query state) "")
                                          :on-input (fn [e] ((:set-query a) (.. e -target -value)))
                                          :on-keydown (fn [e] (when (= "Enter" (.-key e)) ((:search a))))})
                               (when (seq (:query state))
                                 (el/button {:aria-label "Search files" :on-click (fn [_] ((:search a)))} "↵"))
                               (when (seq (:query state))
                                 (el/button {:aria-label "Clear file search" :on-click (fn [_] ((:set-query a) ""))} "×")))
                       (when (get-in state [:loading :search]) (el/p {:class "memory-note"} "Searching filenames…"))
                       (when (seq folder)
                         (el/div {:class "memory-focus-path"}
                                 (el/button {:on-click (fn [_] ((:focus-folder a) "")) :title "Show room root"} "/")
                                 (for [{:keys [label path]} (tree/breadcrumbs folder)]
                                   (el/button {:key path :title path :on-click (fn [_] ((:focus-folder a) path))} label))))
                       (for [[slot error] (:errors state)] (el/p {:key (pr-str slot) :role "alert" :class "proposal-error"} error))
                       (when-let [error (sync/problem stream)]
                         (el/p {:role "alert" :class "proposal-error"} error))
                       (when (or (get-in state [:errors :watch]) (sync/problem stream))
                         (el/button {:on-click (fn [_] ((:refresh a)))} "Retry live updates"))
                       (if search
                         (el/div {:class "memory-file-matches"}
                                 (for [item (:entries search)] (file-row (assoc item :name (:path item)) state open! 0))
                                 (when (empty? (:entries search)) (el/p {:class "memory-note"} "No matching files"))
                                 (when (:more? search) (el/p {:class "memory-note"} "More than 100 matches—narrow your search.")))
                         (folder-tree folder state a open! 0))))))))
