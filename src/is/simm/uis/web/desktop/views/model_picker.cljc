(ns is.simm.uis.web.desktop.views.model-picker
  "Shared model-row rendering and interaction semantics.

   Both pickers receive server-authored availability rows. This namespace owns
   the browser guard and accessible disabled presentation; the server still
   recomputes availability before every write."
  (:require [clojure.string :as str]
            #?(:cljs [org.replikativ.spindel.dom.elements :as el])
            #?(:cljs [is.simm.uis.web.desktop.views.core :as vc]))
  #?(:cljs (:require-macros [org.replikativ.spindel.dom.elements :as el])))

(defn explanation-id
  "Stable DOM id for a row's availability explanation."
  [value]
  (str "model-availability-"
       (str/replace (str value) #"[^A-Za-z0-9_-]" "-")))

(defn activation-key?
  "Keys that activate an element with `role=button`."
  [key]
  (contains? #{"Enter" " " "Spacebar"} key))

(defn activation-allowed?
  "Browser-side guard. Only authoritative `:available?` rows may activate."
  [row]
  (true? (:available? row)))

(defn semantic-attrs
  "Pure accessibility/disabled attributes for a picker row."
  [{:keys [value available? availability-explanation]}]
  (let [disabled? (not (true? available?))]
    {:role "button"
     :tabindex 0
     :aria-disabled (if disabled? "true" "false")
     :aria-describedby (when (and disabled? availability-explanation)
                         (explanation-id value))}))

(defn agent-model-rows
  "Picker rows for ONE agent, built from that agent's model resolution.

   The room and agent ride IN every row. `ifor-each` memoizes on item equality
   and cannot see a closure variable, so rows that carried only the model value
   were equal across two agents: the node — and the click handler closed over
   the previously viewed agent — got reused, and the write landed on the wrong
   agent. Keeping `:room-id` and `:agent-id` in the item makes rows from two
   inspector targets genuinely different items, and the handler reads both the
   write target and the completion-refresh target back out of the row it is
   given instead of closing over them.

   The first row is the inheritance choice; the rest are the catalog choices.
   `:selected?` stays in the item too, for the same reason (see settings.cljc)."
  [room-id agent-id agent-name resolution choices]
  (let [override? (:configured? resolution)
        auto-family? (and (:family resolution) (:auto? resolution))
        chosen-value (or (:preferred-model resolution)
                         (:candidate resolution)
                         (:model resolution))
        stamp (fn [row selected?]
                (assoc row
                       :room-id room-id
                       :agent-id agent-id
                       :agent-name agent-name
                       :selected? selected?))]
    (into [(stamp (:inheritance-choice resolution) (not override?))]
          (map (fn [c]
                 (stamp c (boolean
                           (and override?
                                (if auto-family?
                                  (= (:value c) (:family resolution))
                                  (= (:value c) chosen-value)))))))
          choices)))

(defn- event-key [event]
  #?(:cljs (.-key event)
     :clj (:key event)))

(defn- prevent-default! [event]
  #?(:cljs (.preventDefault event)
     :clj nil))

(defn option-attrs
  "Semantic row attrs plus guarded pointer/keyboard activation.

   `on-select` receives the whole ROW, not just its value. The row is the only
   thing a memoized node carries, so the target it names (see
   `agent-model-rows`) is the one identity a handler can trust."
  [row on-select]
  (merge
   (semantic-attrs row)
   {:on-click (fn [_]
                (when (activation-allowed? row)
                  (on-select row)))
    :on-key-down (fn [event]
                   (when (activation-key? (event-key event))
                     (prevent-default! event)
                     (when (activation-allowed? row)
                       (on-select row))))}))

#?(:cljs
   (defn render-option
     "Render one model choice with its availability and reasoning facts."
     [{:keys [value label kind provider-label no-reasoning?
              reasoning-copy reasoning-explanation selected? available?
              availability-label availability-explanation] :as row}
     on-select]
     (let [disabled? (not available?)
           explanation-id (explanation-id value)
           attrs (option-attrs row on-select)]
       (el/div
         ;; Spindel's element macro recognizes attributes only when this form is
         ;; a literal map. Passing `(merge ...)` here renders that map as text.
         {:key value
          :class (vc/class-names "settings-model-option"
                                 (when selected? "selected")
                                 (when (= kind :inheritance)
                                   "settings-model-option--inheritance")
                                 (when disabled? "settings-model-option--disabled"))
          :role (:role attrs)
          :tabindex (:tabindex attrs)
          :aria-disabled (:aria-disabled attrs)
          :aria-describedby (:aria-describedby attrs)
          :on-click (:on-click attrs)
          :on-key-down (:on-key-down attrs)}
         (el/div {:class (vc/class-names "settings-model-name"
                                         (when (= kind :version)
                                           "settings-model-name--version"))}
           label)
         (when no-reasoning?
           (el/div {:class "settings-model-tag"
                    :data-tooltip reasoning-explanation}
             (str "reasoning " reasoning-copy)))
         (when disabled?
           (el/div {:id explanation-id
                    :class "settings-model-availability"
                    :role "note"
                    :tabindex 0
                    :aria-label (str availability-label ": " availability-explanation)
                    :data-tooltip availability-explanation}
             (vc/icon "circle-alert" {:class "settings-model-availability-icon"
                                       :aria-hidden "true"})
             (el/span {} availability-label)))
         (el/div {:class (vc/class-names "settings-model-provider"
                                         (when disabled?
                                           "settings-model-provider--unavailable"))}
           provider-label)
         (when selected?
           (vc/icon "check" {:class "settings-model-check"})))))
   :clj
   (defn render-option
     "Clojure-side shape used only when a .cljc view is loaded in tests."
     [row _on-select]
     row))
