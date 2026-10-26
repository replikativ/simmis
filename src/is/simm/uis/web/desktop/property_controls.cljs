(ns is.simm.uis.web.desktop.property-controls
  "Shared source-backed property controls for page forms and saved views."
  (:require [clojure.string :as str]
            [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.memory-editor :as editor]))

(defn field-view [path target field model state a editable?]
  (let [pd (get-in state [:property-drafts [path target] (:id field)])
        input (editor/property-input model field pd)
        stale? (not (editor/property-draft-current? model pd))
        blocked? (when-let [blocked? (:property-blocked? a)] (blocked? path))
        enabled? (and editable? (:editable? field) (not stale?) (not blocked?))
        options (get-in model [:reference-options (get-in field [:relation :codomain])])
        selected (first (filter #(= (:id %) (:text input)) (:items options)))
        commit! (fn [] (when enabled? ((:property-save a) path target field)))
        changed! (fn [value] ((:property-input a) path target field value))]
    (el/div {:class "memory-property-field"}
            (el/label {} (str (:label field) (when (:required? field) " *"))
                      (cond
                        (:relation field)
                        (el/div {:class "memory-reference-field"}
                                (if-let [target-path (:path selected)]
                                  (el/button {:type "button" :title target-path
                                              :on-click (fn [_] ((:open a) target-path))} (:label selected))
                                  (el/span {} (cond selected (:label selected) (:null input) "Not set" :else "Unresolved reference")))
                                (el/details {}
                                            (el/summary {} "Change…")
                                            (el/input {:type "search" :aria-label (str "Find " (:label field))
                                                       :placeholder "Find by name or path" :disabled (not enabled?)
                                                       :value (get-in state [:reference-search [path target (:id field)]] "")
                                                       :on-input (fn [e] ((:reference-search a) path target (:id field) (.. e -target -value)))})
                                            (el/select {:aria-label (:label field) :value (or (:text input) "")
                                                        :disabled (or (not enabled?) (not= :ready (:status options)))
                                                        :on-change (fn [e]
                                                                     (changed! {:text (.. e -target -value)})
                                                                     (commit!))}
                                                       (when-not selected (el/option {:value (or (:text input) "") :disabled true} "Choose a reference…"))
                                                       (for [item (:items options)
                                                             :let [query (str/lower-case (get-in state [:reference-search [path target (:id field)]] ""))]
                                                             :when (or (= (:id selected) (:id item))
                                                                       (str/includes? (str/lower-case (str (:label item) " " (:path item))) query))]
                                                         (el/option {:key (:id item) :value (:id item)}
                                                                    (str (:label item) (when (:path item) (str " · " (:path item)))))))
                                            (when-not (= :ready (:status options))
                                              (el/p {:class "memory-note"} "References are unavailable until the knowledge files are valid."))
                                            (when (false? (:complete? options))
                                              (el/p {:class "memory-note"} "Showing the first 200 compatible references."))))
                        (= :db.type/boolean (:value-type field))
                        (el/select {:aria-label (:label field) :disabled (not enabled?)
                                    :value (if (:null input) "null" (:text input))
                                    :on-change (fn [event]
                                                 (let [value (.. event -target -value)]
                                                   (changed! (if (= value "null") {:null true} {:text value}))
                                                   (commit!)))}
                                   (when-not (:required? field) (el/option {:value "null"} "Not set"))
                                   (el/option {:value "true"} "Yes")
                                   (el/option {:value "false"} "No"))
                        (:editable? field)
                        (el/input {:type "text" :aria-label (:label field) :disabled (not enabled?)
                                   :inputmode (when (#{:db.type/long :db.type/double} (:value-type field)) "decimal")
                                   :value (or (:text input) "")
                                   :on-input (fn [event] (changed! {:text (.. event -target -value)}))
                                   :on-blur (fn [_] (commit!))
                                   :on-keydown (fn [e]
                                                 (case (.-key e)
                                                   "Enter" (do (.preventDefault e) (commit!))
                                                   "Escape" (do (.preventDefault e) ((:property-discard a) path target (:id field))) nil))})
                        :else (el/span {:class "memory-property-readonly"} (if (:null input) "Not set" (:text input)))))
            (when-not (:editable? field)
              (el/small {:title (name (:reason field))} "Managed by type or source"))
            (when (and (:editable? field) (not (:required? field))
                       (not (false? (get-in field [:relation :clearable?]))) (not (:null input)))
              (el/button {:disabled (not enabled?) :title (str "Clear " (:label field))
                          :on-click (fn [_] (changed! {:null true}) (commit!))} "Clear"))
            (when stale?
              (el/p {:class "memory-note" :role "status"} "Saved source or property definitions changed. Your entered value was retained; discard it before editing the current version."))
            (when pd
              (el/button {:on-click (fn [_] ((:property-discard a) path target (:id field)))} "Discard field draft")))))
