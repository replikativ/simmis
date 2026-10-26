(ns is.simm.uis.entity-view
  "Display-only entity presentation shared by panes and static rendering.
   Consume an already acquired field model. No commands, navigation callbacks,
   data fetching, globals or component-local mutable state."
  (:require [clojure.string :as str]
            [is.simm.documents.property-presentation :as presentation]
            [is.simm.uis.value-view :as values]
            [org.replikativ.spindel.dom.elements :as el]))

(defn view [model {:keys [title density layout class]}]
  (el/section {:class (str/join " " (remove nil? ["semantic-entity"
                                                   (if (= :compact density) "semantic-entity--compact" "semantic-entity--normal")
                                                   (if (= :columns layout) "semantic-entity--columns" "semantic-entity--stacked")
                                                   (when (string? class) class)]))
               :aria-label (if (string? title) title "Entity properties")
               :data-entity-id (some-> model :entity :id str)}
              (when (string? title) (el/h3 {} title))
              (cond
                (or (nil? model) (= :pending (:status model)))
                (el/p {:class "memory-note" :role "status"} "Loading properties…")
                (or (= :unavailable (:status model)) (not (vector? (:fields model))))
                (el/p {:class "memory-note" :role "status"} "Properties unavailable")
                (empty? (:fields model))
                (el/p {:class "memory-note"} "No properties selected")
                :else
                (el/dl {}
                       (for [field (:fields model)]
                         (el/div {:key (str (:id field)) :class "semantic-entity-field"
                                  :data-property-id (str (:id field))}
                                 (el/dt {} (if (string? (:label field)) (:label field) "Property"))
                                 (el/dd {} (values/cell (presentation/cell model field) {}))))))))
