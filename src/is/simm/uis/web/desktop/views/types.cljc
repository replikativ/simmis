(ns is.simm.uis.web.desktop.views.types
  "Type and property UI components for the Spindel framework.

   Provides:
   - type-tag: Colored pill showing type name
   - type-tags-row: Row of type tags with [+] button
   - type-selector-dropdown: Search + select + create type
   - property-box: Collapsible container for properties
   - property-row: Single property with label and value

   Based on categorical schema where types are Objects and properties are Morphisms."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [org.replikativ.spindel.incremental.interval :as iv]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.signal :refer [->SignalRef ensure-signal-initialized!]]
            [org.replikativ.spindel.effects.track :refer [track]]
            [is.simm.uis.web.desktop.views.core :as vc]
            [clojure.string :as str])
  #?(:cljs (:require-macros [org.replikativ.spindel.dom.elements :as el]
                            [org.replikativ.spindel.dom.foreach :refer [ifor-each]]
                            [org.replikativ.spindel.spin.cps :refer [spin]])))

;; =============================================================================
;; Color Utilities
;; =============================================================================

(def color-palette
  "Pastel color palette for type tags."
  {:blue "#BFDBFE"
   :green "#BBF7D0"
   :yellow "#FEF08A"
   :orange "#FED7AA"
   :red "#FECACA"
   :purple "#DDD6FE"
   :pink "#FBCFE8"
   :teal "#99F6E4"
   :gray "#E5E7EB"})

(defn color-kw->hex
  "Convert color keyword to hex value, with fallback."
  [color-kw]
  (get color-palette color-kw "#BFDBFE"))

(defn uuid-to-color
  "Generate a deterministic pastel color from a UUID."
  [uuid]
  (let [uuid-str (str uuid)
        hash-code (reduce + (map int (take 8 uuid-str)))
        colors (vec (vals color-palette))]
    (nth colors (mod hash-code (count colors)))))

(defn type-color
  "Get color for a type. Uses :type/color if available, otherwise UUID-based."
  [type-entity]
  (if-let [color-kw (or (:prop/color type-entity) (:type/color type-entity))]
    (color-kw->hex color-kw)
    (uuid-to-color (:entity/uuid type-entity))))

;; =============================================================================
;; Name Formatting
;; =============================================================================

(defn format-type-name
  "Format entity name for display.
   E.g., 'S/Company' -> 'Company'"
  [entity-name]
  (when entity-name
    (last (str/split entity-name #"/"))))

(defn format-property-name
  "Format property name for display.
   E.g., 'S/Company/website' -> 'Website'
        'S/Page/created-at' -> 'Created At'"
  [entity-name]
  (when entity-name
    (let [base-name (last (str/split entity-name #"/"))
          spaced (str/replace base-name #"[-_]" " ")]
      (str/join " " (map str/capitalize (str/split spaced #"\s+"))))))

;; =============================================================================
;; Type Tag Component
;; =============================================================================

(defn type-tag
  "Render a colored pill showing type name.

   Props:
   - type: Type entity with :entity/uuid and :entity/name
   - on-click: Optional click handler (receives type)
   - on-remove: Optional remove handler (shows X button on hover)"
  [{:keys [type on-click on-remove hovering?]}]
  (let [title (format-type-name (:entity/name type))
        bg-color (type-color type)
        is-primitive? (:object/primitive? type)
        clickable? (and on-click (not is-primitive?))]
    (el/span {:class (vc/class-names "type-tag"
                                     (when clickable? "type-tag--clickable")
                                     (when is-primitive? "type-tag--primitive"))
              ;; Per-type hue as a CSS var — the stylesheet derives a
              ;; theme-appropriate left border + faint tint from it
              ;; (a solid pastel fill clashes with dark mode).
              :style {:--tag-color bg-color}
              :on-click (when clickable?
                          (fn [e]
                            #?(:cljs
                               (do
                                 (.stopPropagation e)
                                 (on-click type))
                               :clj nil)))}
      ;; Per-type icon (:object/icon, a Lucide name) when set — e.g. S/Person → users.
      (when-let [ic (:object/icon type)]
        (vc/icon ic {:class "type-tag-icon"}))
      (str "#" title)
      ;; Remove button (X) - always render when on-remove provided, hidden by default
      ;; CSS or JS hover handlers will show it
      (when on-remove
        (el/button {:class "type-tag-remove"
                    :style {:display "none"}  ;; Hidden by default, shown on hover
                    :on-click (fn [e]
                                #?(:cljs
                                   (do
                                     (.stopPropagation e)
                                     (on-remove type))
                                   :clj nil))}
          "×")))))

(defn type-tag-with-hover
  "Type tag wrapper that tracks hover state for remove button.
   Uses data attribute to track hover since Spindel doesn't have local state."
  [{:keys [type on-click on-remove]}]
  (el/span {:class "type-tag-wrapper"
            :on-mouseenter (fn [e]
                             #?(:cljs
                                (when-let [btn (.querySelector (.-currentTarget e) ".type-tag-remove")]
                                  (set! (.. btn -style -display) "flex"))
                                :clj nil))
            :on-mouseleave (fn [e]
                             #?(:cljs
                                (when-let [btn (.querySelector (.-currentTarget e) ".type-tag-remove")]
                                  (set! (.. btn -style -display) "none"))
                                :clj nil))}
    (type-tag {:type type
               :on-click on-click
               :on-remove on-remove
               :hovering? false})))

;; =============================================================================
;; Type Tags Row Component
;; =============================================================================

(defn type-tags-row
  "Render a row of type tags with optional add button.

   Props:
   - types: Vector of type entities
   - on-tag-click: Handler when a tag is clicked (receives type)
   - on-remove-tag: Handler when remove button clicked (receives type)
   - on-add-click: Handler when [+] button clicked (opens selector)"
  [{:keys [types on-tag-click on-remove-tag on-add-click]}]
  (el/div {:class "type-tags-row"}
    ;; Existing type tags
    (when (seq types)
      (el/div {:class "type-tags"}
        (ifor-each #(str (:entity/uuid %)) types
          (fn [t]
            (el/span {:key (str (:entity/uuid t))}
              (if on-remove-tag
                (type-tag-with-hover {:type t
                                      :on-click on-tag-click
                                      :on-remove on-remove-tag})
                (type-tag {:type t
                           :on-click on-tag-click})))))))
    ;; Add type button
    (when on-add-click
      (el/button {:class "add-type-button"
                  :on-click (fn [e]
                              #?(:cljs
                                 (do
                                   (.stopPropagation e)
                                   (on-add-click e))
                                 :clj nil))}
        "+"))))

;; =============================================================================
;; Type Selector Dropdown Component
;; =============================================================================

(defn type-selector-dropdown
  "Dropdown for selecting or creating a type.

   Props:
   - available-types: Vector of available type entities
   - search-text: Current search text (managed externally)
   - on-search-change: Handler when search text changes
   - on-select: Handler when a type is selected
   - on-create: Handler when creating a new type (receives name)
   - on-close: Handler to close the dropdown"
  [{:keys [available-types search-text on-search-change on-select on-create on-close]}]
  (let [search-lower (when search-text (str/lower-case search-text))
        ;; Filter types by search
        filtered-types (if (str/blank? search-text)
                         available-types
                         (filter (fn [t]
                                   (str/includes?
                                    (str/lower-case (or (format-type-name (:entity/name t)) ""))
                                    search-lower))
                                 available-types))
        ;; Check if search matches exactly
        exact-match? (some (fn [t]
                             (= (str/lower-case (or (format-type-name (:entity/name t)) ""))
                                search-lower))
                           available-types)
        ;; Show create option?
        show-create? (and (not (str/blank? search-text))
                          (not exact-match?)
                          on-create)]
    (el/div {:class "type-selector-dropdown"
             :on-click (fn [e] #?(:cljs (.stopPropagation e) :clj nil))}
      ;; Search input
      (el/div {:class "type-selector-search"}
        (el/input {:type "text"
                   :placeholder "Search or create type..."
                   :value (or search-text "")
                   :auto-focus true
                   :on-change (fn [e]
                                #?(:cljs
                                   (when on-search-change
                                     (on-search-change (.. e -target -value)))
                                   :clj nil))
                   :on-keydown (fn [e]
                                 #?(:cljs
                                    (when (= (.-key e) "Escape")
                                      (when on-close (on-close)))
                                    :clj nil))}))

      ;; Create new type option
      (when show-create?
        (el/div {:class "type-selector-item type-selector-item--create"
                 :on-click (fn [e]
                             #?(:cljs
                                (do
                                  (.stopPropagation e)
                                  (on-create search-text)
                                  (when on-close (on-close)))
                                :clj nil))}
          (el/span {} (str "Create new type: " search-text))))

      ;; Existing type options
      (if (seq filtered-types)
        (el/div {:class "type-selector-list"}
          (ifor-each #(str (:entity/uuid %)) filtered-types
            (fn [t]
              (el/div {:key (str (:entity/uuid t))
                       :class "type-selector-item"
                       :on-click (fn [e]
                                   #?(:cljs
                                      (do
                                        (.stopPropagation e)
                                        (on-select t)
                                        (when on-close (on-close)))
                                      :clj nil))}
                (el/span {:class "type-selector-item-name"}
                  (format-type-name (:entity/name t)))))))
        (when-not show-create?
          (el/div {:class "type-selector-empty"}
            (if (str/blank? search-text)
              "No types available"
              "No matching types")))))))

;; =============================================================================
;; Type Tags with Add (Combined Component)
;; =============================================================================

(defn type-tags-with-add
  "Complete type tags component with add functionality.

   Uses DOM-based toggle for dropdown visibility to avoid signal tracking issues.

   Props:
   - types: Current types on the entity
   - available-types: All available types to choose from
   - on-tag-click: Handler when tag clicked
   - on-remove-tag: Handler when tag removed
   - on-add-type: Handler when type added (receives type-entity)
   - on-create-type: Handler when new type created"
  [{:keys [types available-types on-tag-click on-remove-tag on-add-type on-create-type]}]
  (let [container-id (str "type-selector-" (random-uuid))
        toggle-dropdown! (fn [e]
                           #?(:cljs
                              (do
                                (.stopPropagation e)
                                (when-let [container (js/document.getElementById container-id)]
                                  (.toggle (.-classList container) "type-selector--open")))
                              :clj nil))
        close-dropdown! (fn []
                          #?(:cljs
                             (when-let [container (js/document.getElementById container-id)]
                               (.remove (.-classList container) "type-selector--open"))
                             :clj nil))
        handle-select (fn [type-entity]
                        (when on-add-type
                          (on-add-type type-entity))
                        (close-dropdown!))]
    (el/div {:class "type-tags-container"
             :id container-id}
      ;; Type tags row with + button
      (type-tags-row {:types types
                      :on-tag-click on-tag-click
                      :on-remove-tag on-remove-tag
                      :on-add-click toggle-dropdown!})

      ;; Type selector dropdown (always rendered, visibility controlled by CSS)
      (type-selector-dropdown {:available-types available-types
                               :search-text ""
                               :on-search-change nil  ;; Search state not needed for MVP
                               :on-select handle-select
                               :on-create on-create-type
                               :on-close close-dropdown!}))))

;; =============================================================================
;; Property Display (Read-only value rendering)
;; =============================================================================

(defn property-display
  "Render a property value in read-only mode.

   Props:
   - property-type: Keyword like :text, :checkbox, :date, :color, :relation
   - value: The property value"
  [{:keys [property-type value]}]
  (cond
    ;; Boolean - show checkmark or X
    (boolean? value)
    (el/span {:class "property-display property-display--boolean"}
      (if value "✓" "✗"))

    ;; Nil/empty
    (nil? value)
    (el/span {:class "property-display property-display--empty"}
      "(empty)")

    ;; Color - show swatch
    (= property-type :color)
    (el/div {:class "property-display property-display--color"}
      (el/span {:class "property-color-swatch"
                :style {:background-color (color-kw->hex value)}})
      (el/span {} (name value)))

    ;; Date - format nicely
    (= property-type :date)
    (el/span {:class "property-display property-display--date"}
      #?(:cljs
         (try
           (let [date (js/Date. value)]
             (if (js/isNaN (.getTime date))
               (str value)
               (.toLocaleDateString date "en-US" #js {:year "numeric"
                                                       :month "short"
                                                       :day "numeric"})))
           (catch :default _
             (str value)))
         :clj (str value)))

    ;; Relation - show linked entity name
    (and (map? value) (or (:S.Page/title value) (:entity/name value)))
    (el/span {:class "property-display property-display--relation"}
      (or (:S.Page/title value) (:entity/name value)))

    ;; Vector (multi-select or multi-relation)
    (vector? value)
    (el/span {:class "property-display property-display--multi"}
      (str/join ", " (map #(if (map? %)
                             (or (:S.Page/title %) (:S.Option/name %) (:entity/name %) "?")
                             (str %))
                          value)))

    ;; Default - just stringify
    :else
    (el/span {:class "property-display"} (str value))))

;; =============================================================================
;; Property Row Component
;; =============================================================================

(defn property-row
  "Single property with label and value, supporting inline editing.

   Uses DOM-based toggle for edit mode (no local state needed).

   Props:
   - property: Morphism entity with :entity/name, :morphism/property-type
   - value: Current value
   - on-save: Handler when value is saved (receives property, new-value)
   - on-property-click: Handler when property label clicked (navigate to property)"
  [{:keys [property value on-save on-property-click]}]
  (let [prop-name (:entity/name property)
        prop-title (format-property-name prop-name)
        prop-type (or (:morphism/property-type property) :text)
        row-id (str "prop-row-" (:entity/uuid property))

        ;; Toggle edit mode via DOM class
        start-editing! (fn [e]
                         #?(:cljs
                            (do
                              (.stopPropagation e)
                              (when-let [row (js/document.getElementById row-id)]
                                (.add (.-classList row) "editing")
                                ;; Focus the input after a short delay
                                (js/setTimeout
                                  (fn []
                                    (when-let [input (.querySelector row ".property-edit-input")]
                                      (.focus input)
                                      (.select input)))
                                  50)))
                            :clj nil))

        stop-editing! (fn []
                        #?(:cljs
                           (when-let [row (js/document.getElementById row-id)]
                             (.remove (.-classList row) "editing"))
                           :clj nil))

        save-value! (fn [new-value]
                      #?(:cljs
                         (do
                           (when on-save
                             (on-save property new-value))
                           (stop-editing!))
                         :clj nil))]

    (el/div {:class "property-row"
             :id row-id}
      ;; Label
      (el/div {:class "property-label"
               :on-click (if on-property-click
                           (fn [e]
                             #?(:cljs
                                (do
                                  (.stopPropagation e)
                                  (on-property-click property))
                                :clj nil))
                           start-editing!)}
        (str prop-title ":"))

      ;; Display value (shown when not editing)
      (el/div {:class "property-value-display"
               :on-click (when on-save start-editing!)}
        (property-display {:property-type prop-type
                           :value value}))

      ;; Edit input (shown when editing)
      (el/div {:class "property-value-edit"}
        (case prop-type
          :checkbox
          (el/input {:class "property-edit-input"
                     :type "checkbox"
                     :checked (boolean value)
                     :on-change (fn [e]
                                  #?(:cljs
                                     (save-value! (.. e -target -checked))
                                     :clj nil))})

          :number
          (el/input {:class "property-edit-input"
                     :type "number"
                     :value (if (some? value) (str value) "")
                     :on-blur (fn [e]
                                #?(:cljs
                                   (let [v (.. e -target -value)]
                                     (save-value! (when (seq v) (js/parseFloat v))))
                                   :clj nil))
                     :on-keydown (fn [e]
                                   #?(:cljs
                                      (case (.-key e)
                                        "Enter" (let [v (.. e -target -value)]
                                                  (save-value! (when (seq v) (js/parseFloat v))))
                                        "Escape" (stop-editing!)
                                        nil)
                                      :clj nil))})

          :date
          (el/input {:class "property-edit-input"
                     :type "date"
                     :value (or value "")
                     :on-change (fn [e]
                                  #?(:cljs
                                     (save-value! (.. e -target -value))
                                     :clj nil))
                     :on-keydown (fn [e]
                                   #?(:cljs
                                      (when (= (.-key e) "Escape")
                                        (stop-editing!))
                                      :clj nil))})

          ;; Default: text input
          (el/input {:class "property-edit-input"
                     :type "text"
                     :value (or value "")
                     :placeholder (str "Enter " prop-title "...")
                     :on-blur (fn [e]
                                #?(:cljs
                                   (save-value! (.. e -target -value))
                                   :clj nil))
                     :on-keydown (fn [e]
                                   #?(:cljs
                                      (case (.-key e)
                                        "Enter" (save-value! (.. e -target -value))
                                        "Escape" (stop-editing!)
                                        nil)
                                      :clj nil))}))))))

;; =============================================================================
;; Property Box Component
;; =============================================================================

(defn property-box
  "Collapsible box showing all properties for a page.

   Uses a spin-scoped signal for collapse/expand state. The signal ID is derived
   from ec/*spin-id* so it is stable across parent re-runs regardless of chain-head
   drift — the spin ID is bound by await-spin before the body executes.

   Props:
   - properties: Vector of morphism entities
   - values: Map of {morphism-name value}
   - on-save: Handler when a property value is saved (receives property, new-value)
   - on-property-click: Handler when property label clicked
   - on-add-property: Handler when add property button clicked (for type pages)"
  [{:keys [properties values on-save on-property-click on-add-property]}]
  (spin
    (let [;; Stable signal ID derived from this spin's own ID — never varies with chain-head
          collapsed (->SignalRef (keyword (str ec/*spin-id*) "collapsed") true)
          _ (ensure-signal-initialized! collapsed)
          tracked (track collapsed)
          collapsed? (:new tracked)]
      (el/div {:class (str "property-box" (when-not collapsed? " property-box-open"))}
        ;; Header (clickable to toggle)
        (el/div {:class "property-box-header"
                 :on-click (fn [e]
                             #?(:cljs (.stopPropagation e) :clj nil)
                             (swap! collapsed not))}
          (el/span {:class "property-box-toggle"} (if collapsed? "▶" "▼"))
          (el/span {:class "property-box-title"}
            (str "Properties (" (count properties) ")")))

        ;; Content (conditionally rendered based on collapsed? signal)
        (when-not collapsed?
          (el/div {:class "property-box-content"}
            (if (seq properties)
              (el/div {:class "property-rows"}
                ;; The VALUE rides in the item. `ifor-each` memoises on item
                ;; equality, and the property entities do not change when a
                ;; value does — so with the value read from the `values`
                ;; closure, a saved value landed and the row kept showing the
                ;; old one until a reload (CLAUDE.md, spindel sharp edge 2).
                (ifor-each #(str (:entity/uuid (:property %)))
                           (mapv (fn [prop] {:property prop
                                             :value (get values (:entity/name prop))})
                                 properties)
                  (fn [{:keys [property value]}]
                    (el/div {:key (str (:entity/uuid property))}
                      (property-row {:property property
                                     :value value
                                     :on-save on-save
                                     :on-property-click on-property-click})))))
              (el/div {:class "property-box-empty"}
                "No properties available."))

            ;; Add property button (for type pages)
            (when on-add-property
              (el/button {:class "add-property-button"
                          :on-click (fn [e]
                                      #?(:cljs (.stopPropagation e) :clj nil)
                                      (on-add-property))}
                "+ Add property"))))))))

;; =============================================================================
;; Instances Box Component (for type pages)
;; =============================================================================

(defn instance-value-cell
  "Render a single value cell in the instances table."
  [{:keys [value property-type]}]
  (cond
    (nil? value)
    (el/span {:class "instances-table-empty"} "(empty)")

    (boolean? value)
    (el/span {:class "instances-table-bool"} (if value "✓" "✗"))

    (map? value)
    ;; Relation - show linked entity name
    (el/span {:class "instances-table-relation"}
      (or (:S.Page/title value) (:entity/name value) "?"))

    (vector? value)
    ;; Multi-value
    (el/span {:class "instances-table-multi"}
      (str/join ", " (map #(if (map? %)
                             (or (:S.Page/title %) (:entity/name %) "?")
                             (str %))
                          value)))

    :else
    (el/span {} (str value))))

(defn instances-table
  "Table showing instances with their property values.

   Props:
   - properties: Vector of morphism entities (table columns)
   - instances: Vector of {:entity/uuid :S.Page/title :values {prop-name value}}
   - on-instance-click: Handler when instance name clicked"
  [{:keys [properties instances on-instance-click]}]
  (el/table {:class "instances-table"}
    ;; Table header
    (el/thead {}
      (el/tr {}
        (el/th {:class "instances-table-name-header"} "Name")
        (ifor-each #(str (:entity/uuid %)) properties
          (fn [prop]
            (el/th {:key (str (:entity/uuid prop))
                    :class "instances-table-header"}
              (format-property-name (:entity/name prop)))))))

    ;; Table body
    (el/tbody {}
      (ifor-each #(str (:entity/uuid %)) instances
        (fn [inst]
          (el/tr {:key (str (:entity/uuid inst))
                  :class "instances-table-row"}
            ;; Name cell (clickable)
            (el/td {:class "instances-table-name-cell"
                    :on-click (when on-instance-click
                                (fn [e]
                                  #?(:cljs
                                     (do
                                       (.stopPropagation e)
                                       (on-instance-click inst))
                                     :clj nil)))}
              (or (:S.Page/title inst) (:entity/name inst) "Untitled"))

            ;; Property value cells. The value rides in the item: these
            ;; items are the property entities, which do not change when an
            ;; instance's value does, and `ifor-each` memoises on item
            ;; equality (sharp edge 2) — a closed-over `inst` kept the cell
            ;; showing the old value.
            (ifor-each #(str (:entity/uuid (:property %)))
                       (mapv (fn [prop] {:property prop
                                         :value (get-in inst [:values (:entity/name prop)])})
                             properties)
              (fn [{prop :property value :value}]
                (el/td {:key (str (:entity/uuid inst) "-" (:entity/name prop))
                        :class "instances-table-cell"}
                  (instance-value-cell {:value value
                                        :property-type (:morphism/property-type prop)}))))))))))

(defn instances-box
  "Box showing all instances of a type as a table.

   Uses native HTML <details> element for expand/collapse.

   Props:
   - type-name: Name of the type (for display)
   - properties: Vector of morphism entities (table columns)
   - instances: Vector of {:entity/uuid :S.Page/title :values {prop-name value}}
   - on-instance-click: Handler when instance clicked"
  [{:keys [type-name properties instances on-instance-click]}]
  ;; Use native <details>/<summary> - starts open by default for type pages
  (el/details {:class "instances-box" :open true}
    ;; Summary is the clickable header
    (el/summary {:class "instances-box-header"}
      (el/span {:class "instances-box-title"}
        (str (format-type-name type-name) " instances (" (count instances) ")")))

    ;; Content (shown when details is open)
    (el/div {:class "instances-box-content"}
      (if (seq instances)
        (instances-table {:properties properties
                          :instances instances
                          :on-instance-click on-instance-click})
        (el/div {:class "instances-box-empty"}
          "No instances of this type yet.")))))

;; =============================================================================
;; Property Input Components (for inline editing)
;; =============================================================================

(defn text-input
  "Text input for property editing.

   Props:
   - value: Current value
   - placeholder: Placeholder text
   - on-save: Handler when value saved (receives new value)
   - on-cancel: Handler when editing cancelled"
  [{:keys [value placeholder on-save on-cancel]}]
  (el/input {:class "property-input"
             :type "text"
             :value (or value "")
             :placeholder (or placeholder "Enter value...")
             :auto-focus true
             :on-blur (fn [e]
                        #?(:cljs
                           (when on-save
                             (on-save (.. e -target -value)))
                           :clj nil))
             :on-keydown (fn [e]
                           #?(:cljs
                              (case (.-key e)
                                "Enter" (do
                                          (.preventDefault e)
                                          (when on-save
                                            (on-save (.. e -target -value))))
                                "Escape" (when on-cancel (on-cancel))
                                nil)
                              :clj nil))}))

(defn number-input
  "Number input for property editing.

   Props:
   - value: Current numeric value
   - on-save: Handler when value saved
   - on-cancel: Handler when cancelled"
  [{:keys [value on-save on-cancel]}]
  (el/input {:class "property-input"
             :type "number"
             :value (if (some? value) (str value) "")
             :auto-focus true
             :on-blur (fn [e]
                        #?(:cljs
                           (when on-save
                             (let [v (.. e -target -value)]
                               (on-save (when (seq v) (js/parseFloat v)))))
                           :clj nil))
             :on-keydown (fn [e]
                           #?(:cljs
                              (case (.-key e)
                                "Enter" (do
                                          (.preventDefault e)
                                          (when on-save
                                            (let [v (.. e -target -value)]
                                              (on-save (when (seq v) (js/parseFloat v))))))
                                "Escape" (when on-cancel (on-cancel))
                                nil)
                              :clj nil))}))

(defn checkbox-input
  "Checkbox input for boolean properties.

   Props:
   - value: Current boolean value
   - on-save: Handler when value toggled (receives new value)"
  [{:keys [value on-save]}]
  (el/div {:class "property-checkbox-wrapper"}
    (el/input {:class "property-checkbox"
               :type "checkbox"
               :checked (boolean value)
               :on-change (fn [e]
                            #?(:cljs
                               (when on-save
                                 (on-save (.. e -target -checked)))
                               :clj nil))})))

(defn date-input
  "Date input for date properties.

   Props:
   - value: Current date value (ISO string or Date)
   - on-save: Handler when value saved
   - on-cancel: Handler when cancelled"
  [{:keys [value on-save on-cancel]}]
  (let [;; Format value for date input (YYYY-MM-DD)
        formatted-value #?(:cljs
                           (when value
                             (try
                               (let [date (if (string? value) (js/Date. value) value)]
                                 (when-not (js/isNaN (.getTime date))
                                   (.toISOString date)))
                               (catch :default _ nil)))
                           :clj nil)]
    (el/input {:class "property-date-input"
               :type "date"
               :value (or (when formatted-value (subs formatted-value 0 10)) "")
               :auto-focus true
               :on-change (fn [e]
                            #?(:cljs
                               (when on-save
                                 (let [v (.. e -target -value)]
                                   (on-save (when (seq v) v))))
                               :clj nil))
               :on-keydown (fn [e]
                             #?(:cljs
                                (when (= (.-key e) "Escape")
                                  (when on-cancel (on-cancel)))
                                :clj nil))})))

(defn url-input
  "URL input for URL properties.

   Props:
   - value: Current URL value
   - on-save: Handler when value saved
   - on-cancel: Handler when cancelled"
  [{:keys [value on-save on-cancel]}]
  (el/input {:class "property-input"
             :type "url"
             :value (or value "")
             :placeholder "https://example.com"
             :auto-focus true
             :on-blur (fn [e]
                        #?(:cljs
                           (when on-save
                             (on-save (.. e -target -value)))
                           :clj nil))
             :on-keydown (fn [e]
                           #?(:cljs
                              (case (.-key e)
                                "Enter" (do
                                          (.preventDefault e)
                                          (when on-save
                                            (on-save (.. e -target -value))))
                                "Escape" (when on-cancel (on-cancel))
                                nil)
                              :clj nil))}))

(defn email-input
  "Email input for email properties.

   Props:
   - value: Current email value
   - on-save: Handler when value saved
   - on-cancel: Handler when cancelled"
  [{:keys [value on-save on-cancel]}]
  (el/input {:class "property-input"
             :type "email"
             :value (or value "")
             :placeholder "email@example.com"
             :auto-focus true
             :on-blur (fn [e]
                        #?(:cljs
                           (when on-save
                             (on-save (.. e -target -value)))
                           :clj nil))
             :on-keydown (fn [e]
                           #?(:cljs
                              (case (.-key e)
                                "Enter" (do
                                          (.preventDefault e)
                                          (when on-save
                                            (on-save (.. e -target -value))))
                                "Escape" (when on-cancel (on-cancel))
                                nil)
                              :clj nil))}))

(defn color-picker
  "Color picker for color properties.
   Uses a simple palette of predefined colors.

   Props:
   - value: Current color keyword
   - on-save: Handler when color selected (receives color keyword)"
  [{:keys [value on-save]}]
  (el/div {:class "property-color-picker"}
    (ifor-each (comp name first) color-palette
      (fn [[color-kw hex]]
        (el/div {:key (name color-kw)
                 :class (vc/class-names "property-color-option"
                                        (when (= value color-kw) "selected"))
                 :style {:background-color hex}
                 :on-click (fn [e]
                             #?(:cljs
                                (do
                                  (.stopPropagation e)
                                  (when on-save (on-save color-kw)))
                                :clj nil))})))))

(defn select-input
  "Select dropdown for single-select properties.

   Props:
   - value: Current selected option UUID
   - options: Vector of {:value uuid-str :label string}
   - on-save: Handler when option selected"
  [{:keys [value options on-save]}]
  (el/select {:class "property-select"
              :value (or value "")
              :on-change (fn [e]
                           #?(:cljs
                              (when on-save
                                (let [v (.. e -target -value)]
                                  (on-save (when (seq v) v))))
                              :clj nil))}
    (el/option {:value ""} "Select...")
    (ifor-each :value options
      (fn [{:keys [value label]}]
        (el/option {:key value :value value} label)))))

(defn multi-select-input
  "Multi-select for multi-select properties.
   Uses checkboxes for now (could be enhanced later).

   Props:
   - value: Vector of selected option UUIDs
   - options: Vector of {:value uuid-str :label string}
   - on-save: Handler when selection changes"
  [{:keys [value options on-save]}]
  (let [current (vec (or value []))
        selected-set (set current)]
    (el/div {:class "property-multi-select"}
      ;; the selection rides in the items (ifor-each memoises on them): both
      ;; the box's checked state and the value its handler edits
      (ifor-each :value (mapv #(assoc % :checked? (contains? selected-set (:value %))
                                        :current current)
                              options)
        (fn [{opt-value :value opt-label :label :keys [checked? current]}]
          (el/label {:key opt-value
                     :class "property-multi-option"}
            (el/input {:type "checkbox"
                       :checked checked?
                       :on-change (fn [e]
                                    #?(:cljs
                                       (when on-save
                                         (let [checked? (.. e -target -checked)
                                               new-value (if checked?
                                                           (conj current opt-value)
                                                           (vec (remove #(= % opt-value) current)))]
                                           (on-save new-value)))
                                       :clj nil))})
            (el/span {} opt-label)))))))

;; =============================================================================
;; Property Input Dispatcher
;; =============================================================================

(defn property-input
  "Dispatch to appropriate input component based on property type.

   Props:
   - property-type: Keyword like :text, :number, :checkbox, :date, etc.
   - value: Current value
   - options: Options for select/multi-select
   - on-save: Handler when value saved
   - on-cancel: Handler when editing cancelled"
  [{:keys [property-type value options on-save on-cancel]}]
  (case property-type
    :text (text-input {:value value :on-save on-save :on-cancel on-cancel})
    :number (number-input {:value value :on-save on-save :on-cancel on-cancel})
    :checkbox (checkbox-input {:value value :on-save on-save})
    :date (date-input {:value value :on-save on-save :on-cancel on-cancel})
    :url (url-input {:value value :on-save on-save :on-cancel on-cancel})
    :email (email-input {:value value :on-save on-save :on-cancel on-cancel})
    :color (color-picker {:value value :on-save on-save})
    :select (select-input {:value value :options options :on-save on-save})
    :multi-select (multi-select-input {:value value :options options :on-save on-save})
    ;; Default to text input
    (text-input {:value value :on-save on-save :on-cancel on-cancel})))

;; =============================================================================
;; Property Definitions (for type pages)
;; =============================================================================

(def property-types
  "Available property types with display labels."
  [{:value :text :label "Text"}
   {:value :number :label "Number"}
   {:value :checkbox :label "Checkbox"}
   {:value :date :label "Date"}
   {:value :url :label "URL"}
   {:value :email :label "Email"}
   {:value :color :label "Color"}
   {:value :select :label "Select"}
   {:value :multi-select :label "Multi-select"}
   {:value :relation :label "Relation"}])

(defn property-definition-row
  "Render a single property definition in the Property Definitions section.

   Props:
   - property: Morphism entity with :entity/name, :morphism/dst, :morphism/cardinality, etc.
   - on-click: Handler when property row clicked (for editing)
   - on-remove: Handler when remove button clicked"
  [{:keys [property on-click on-remove]}]
  (let [prop-name (:entity/name property)
        prop-title (format-property-name prop-name)
        prop-type (or (:morphism/property-type property) :text)
        target-type (:morphism/dst property)
        target-name (when target-type (format-type-name (:entity/name target-type)))
        cardinality (or (:morphism/cardinality property) :one)
        optional? (:morphism/optional? property)]
    (el/div {:class "property-def-row"
             :on-click (when on-click
                         (fn [e]
                           #?(:cljs
                              (do
                                (.stopPropagation e)
                                (on-click property))
                              :clj nil)))}
      ;; Property name
      (el/div {:class "property-def-name"}
        (el/span {:class "property-def-title"} prop-title)
        (when optional?
          (el/span {:class "property-def-optional"} "(optional)")))

      ;; Property type/target info
      (el/div {:class "property-def-info"}
        ;; Property type badge
        (el/span {:class "property-def-type"}
          (name prop-type))

        ;; Target type (for relations)
        (when (and target-type (not (:object/primitive? target-type)))
          (el/span {:class "property-def-target"}
            (str "→ " target-name)))

        ;; Cardinality indicator
        (when (= cardinality :many)
          (el/span {:class "property-def-cardinality"} "[]")))

      ;; Remove button
      (when on-remove
        (el/button {:class "property-def-remove"
                    :on-click (fn [e]
                                #?(:cljs
                                   (do
                                     (.stopPropagation e)
                                     (on-remove property))
                                   :clj nil))}
          "×")))))

(defn add-property-form
  "Form for adding a new property to a type.

   Uses DOM-based toggle for visibility.

   Props:
   - available-types: Vector of all types (for relation target selection)
   - on-save: Handler when form submitted (receives {:name, :target-type, :cardinality, :optional?, :property-type})
   - on-cancel: Handler when form cancelled"
  [{:keys [available-types on-save on-cancel]}]
  (let [form-id (str "add-property-form-" (random-uuid))
        get-form-values (fn []
                          #?(:cljs
                             (when-let [form (js/document.getElementById form-id)]
                               (let [name-input (.querySelector form "[name='property-name']")
                                     type-select (.querySelector form "[name='property-type']")
                                     target-select (.querySelector form "[name='target-type']")
                                     cardinality-select (.querySelector form "[name='cardinality']")
                                     optional-checkbox (.querySelector form "[name='optional']")]
                                 {:name (when name-input (.-value name-input))
                                  :property-type (when type-select (keyword (.-value type-select)))
                                  :target-type-uuid (when (and target-select (seq (.-value target-select)))
                                                      (.-value target-select))
                                  :cardinality (when cardinality-select (keyword (.-value cardinality-select)))
                                  :optional? (when optional-checkbox (.-checked optional-checkbox))}))
                             :clj nil))]
    (el/div {:class "add-property-form"
             :id form-id}
      (el/div {:class "add-property-form-header"}
        (el/span {} "Add Property")
        (el/button {:class "add-property-form-close"
                    :on-click (fn [e]
                                #?(:cljs (.stopPropagation e) :clj nil)
                                (when on-cancel (on-cancel)))}
          "×"))

      (el/div {:class "add-property-form-body"}
        ;; Property name
        (el/div {:class "form-field"}
          (el/label {} "Name")
          (el/input {:type "text"
                     :name "property-name"
                     :placeholder "e.g., website, start-date"
                     :auto-focus true}))

        ;; Property type
        (el/div {:class "form-field"}
          (el/label {} "Type")
          (el/select {:name "property-type"}
            (ifor-each #(name (:value %)) property-types
              (fn [{:keys [value label]}]
                (el/option {:key (name value) :value (name value)} label)))))

        ;; Target type (for relations)
        (el/div {:class "form-field"}
          (el/label {} "Target Type (for relations)")
          (el/select {:name "target-type"}
            (el/option {:value ""} "None (use property type)")
            (ifor-each #(str (:entity/uuid %)) available-types
              (fn [t]
                (el/option {:key (str (:entity/uuid t))
                            :value (str (:entity/uuid t))}
                  (format-type-name (:entity/name t)))))))

        ;; Cardinality
        (el/div {:class "form-field"}
          (el/label {} "Cardinality")
          (el/select {:name "cardinality"}
            (el/option {:value "one"} "One (single value)")
            (el/option {:value "many"} "Many (list of values)")))

        ;; Optional
        (el/div {:class "form-field form-field-checkbox"}
          (el/label {}
            (el/input {:type "checkbox"
                       :name "optional"})
            (el/span {} " Optional (can be empty)"))))

      (el/div {:class "add-property-form-footer"}
        (el/button {:class "btn btn-secondary"
                    :on-click (fn [e]
                                #?(:cljs (.stopPropagation e) :clj nil)
                                (when on-cancel (on-cancel)))}
          "Cancel")
        (el/button {:class "btn btn-primary"
                    :on-click (fn [e]
                                #?(:cljs
                                   (do
                                     (.stopPropagation e)
                                     (when on-save
                                       (on-save (get-form-values))))
                                   :clj nil))}
          "Add Property")))))

(defn property-definitions-box
  "Box showing all property definitions for a type.

   Uses native HTML <details> element for expand/collapse.

   Props:
   - type-name: Name of the type (e.g., 'S/Company')
   - properties: Vector of morphism entities (properties defined on this type)
   - available-types: Vector of all types (for new property target selection)
   - on-property-click: Handler when property row clicked
   - on-add-property: Handler when new property is saved (receives form values)
   - on-remove-property: Handler when property removed"
  [{:keys [type-name properties available-types on-property-click on-add-property on-remove-property]}]
  (let [container-id (str "property-defs-" (random-uuid))
        toggle-form! (fn [e]
                       #?(:cljs
                          (do
                            (.stopPropagation e)
                            (when-let [container (js/document.getElementById container-id)]
                              (.toggle (.-classList container) "show-add-form")))
                          :clj nil))
        close-form! (fn []
                      #?(:cljs
                         (when-let [container (js/document.getElementById container-id)]
                           (.remove (.-classList container) "show-add-form"))
                         :clj nil))]
    ;; Use native <details>/<summary> - starts open by default for type pages
    (el/details {:class "property-defs-box" :open true}
      ;; Summary is the clickable header
      (el/summary {:class "property-defs-header"}
        (el/span {:class "property-defs-title"}
          (str "Property Definitions (" (count properties) ")"))
        ;; Add button in header
        (el/button {:class "property-defs-add-btn"
                    :on-click toggle-form!}
          "+"))

      ;; Content with form toggle container
      (el/div {:class "property-defs-content"
               :id container-id}
        ;; Property list
        (if (seq properties)
          (el/div {:class "property-defs-list"}
            (ifor-each #(str (:entity/uuid %)) properties
              (fn [prop]
                (el/div {:key (str (:entity/uuid prop))}
                  (property-definition-row {:property prop
                                            :on-click on-property-click
                                            :on-remove on-remove-property})))))
          (el/div {:class "property-defs-empty"}
            "No properties defined yet."))

        ;; Add property form (hidden by default, toggled by button)
        (el/div {:class "property-defs-form-container"}
          (add-property-form {:available-types available-types
                              :on-save (fn [values]
                                         (when on-add-property
                                           (on-add-property values))
                                         (close-form!))
                              :on-cancel close-form!}))))))
