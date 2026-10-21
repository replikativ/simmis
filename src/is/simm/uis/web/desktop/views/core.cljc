(ns is.simm.uis.web.desktop.views.core
  "Shared view utilities and helpers.

   Provides:
   - Lucide icon helper (renders icon elements)
   - Common styling helpers
   - Reusable small components"
  (:require [org.replikativ.spindel.dom.elements :as dom]
            [clojure.string :as str]
            #?(:cljs [is.simm.uis.web.desktop.sanitize :as sanitize]))
  #?(:cljs (:require-macros [org.replikativ.spindel.dom.elements :as el])))

;; =============================================================================
;; Naming
;; =============================================================================

(def room-nouns
  "What the UI calls a room, by `:room/type`.

   The label follows the type rather than forcing one noun on all of them:
   \"Team\" fits a group room and is plainly wrong for one human and their
   assistant. doc/archive/navigation-redesign-plan.md, Phase 5.

   The word `room` stays in the code, the schema and the agent vocabulary —
   dvergr owns the concept and we just made `dvergr.room` discoverable to
   agents, so a vocabulary split would undo that. The synonym is stated
   explicitly in the agent's context instead (see `room_agents`)."
  ;; Four types are in use: :group and :project (both several people and their
  ;; agents working together — a Team), :personal-ai, :telegram-mirror.
  {:group "Team"
   :project "Team"
   :personal-ai "Assistant"
   :telegram-mirror "Telegram"})

(defn room-noun
  "Display noun for a room type. Unknown types read as \"Room\" — the generic
   word is a safe fallback in a way a guessed specific one is not."
  [room-type]
  (get room-nouns room-type "Room"))

(defn team-like?
  "Does this room read correctly under a \"Teams\" heading? Used to decide
   whether a nav row needs its own noun — asking about the LABEL rather than
   listing types keeps one place to change when a type is added."
  [room-type]
  (= "Team" (room-noun room-type)))

;; =============================================================================
;; Utility Functions (moved up for use by other functions)
;; =============================================================================

(defn class-names
  "Join class names, filtering nil values.

   Usage:
     (class-names \"base\" (when active? \"active\") nil \"extra\")
     => \"base active extra\""
  [& classes]
  (->> classes
       (filter some?)
       (str/join " ")))

(defn with-key
  "Add :key to a vnode for list rendering.

   Usage:
     (map #(with-key (page-item %) (:entity/uuid %)) pages)"
  [vnode key]
  (assoc vnode :key (str key)))

;; =============================================================================
;; Lucide Icon Helper
;; =============================================================================

;; Lucide icons are loaded via CDN in the HTML and accessed via JS.
;; We create placeholder elements that get replaced by Lucide's JS.

(defn icon
  "Render a Lucide icon.

   Usage:
     (icon :chevron-right)
     (icon :file-text {:class \"w-4 h-4\"})

   The icon name should match Lucide's naming (kebab-case).
   See: https://lucide.dev/icons/"
  ([icon-name]
   (icon icon-name {}))
  ([icon-name attrs]
   (let [icon-class (str "lucide-" (name icon-name))
         extra-class (:class attrs)
         combined-class (if extra-class
                          (str icon-class " " extra-class)
                          icon-class)]
     ;; Use simple-element for dynamic icon creation
     (dom/simple-element :i (merge {:data-lucide (name icon-name)
                                    :class combined-class}
                                   (dissoc attrs :class))
                         []))))

;; =============================================================================
;; Common Styled Components
;; =============================================================================

(defn icon-button
  "Render an icon button with consistent styling.

   Usage:
     (icon-button :plus {:on-click #(...)})
     (icon-button :trash {:class \"danger\" :on-click #(...)})"
  [icon-name attrs]
  (let [base-class "icon-button"
        extra-class (:class attrs)
        combined-class (if extra-class
                         (str base-class " " extra-class)
                         base-class)]
    ;; Use simple-element for dynamic attribute merging
    (dom/simple-element :button
                        (merge {:type "button"}
                               attrs
                               {:class combined-class})
                        [(icon icon-name)])))

(defn button-primary
  "Render a primary styled button.
   Note: Due to macro limitations, only accepts single child."
  ([attrs]
   (button-primary attrs nil))
  ([attrs child]
   (dom/button {:class (class-names "btn btn-primary" (:class attrs))
                :type "button"}
     child)))

(defn button-secondary
  "Render a secondary styled button.
   Note: Due to macro limitations, only accepts single child."
  ([attrs]
   (button-secondary attrs nil))
  ([attrs child]
   (dom/button {:class (class-names "btn btn-secondary" (:class attrs))
                :type "button"}
     child)))

(defn loading-spinner
  "Render a loading spinner."
  ([]
   (loading-spinner {}))
  ([attrs]
   (dom/div (merge {:class "loading-spinner"} attrs)
     (dom/div {:class "spinner"}))))

(defn empty-state
  "Render an empty state message.

   Usage:
     (empty-state {:icon :inbox :title \"No pages\" :message \"Create your first page\"})"
  [{:keys [icon-name title message action-label on-action]}]
  (dom/div {:class "empty-state"}
    (when icon-name
      (dom/div {:class "empty-state-icon"}
        (icon icon-name)))
    (when title
      (dom/h3 {:class "empty-state-title"} title))
    (when message
      (dom/p {:class "empty-state-message"} message))
    (when (and action-label on-action)
      (button-primary {:on-click on-action} action-label))))

;; =============================================================================
;; List Item Components
;; =============================================================================

(defn list-item
  "Render a clickable list item.

   Usage:
     (list-item {:key \"id\" :on-click #(...) :selected? true} \"Content\")"
  ([attrs]
   (list-item attrs nil))
  ([{:keys [key on-click selected? class]} child]
   (dom/simple-element :li
                       {:key key
                        :class (class-names "list-item"
                                           (when selected? "selected")
                                           class)
                        :on-click on-click}
                       (if (vector? child) child [child]))))

(defn nav-item
  "Render a navigation item with icon and label.

   Usage:
     (nav-item {:icon :file-text :label \"Pages\" :selected? true :on-click #(...)})"
  [{:keys [icon-name label selected? on-click badge]}]
  (dom/li {:class (str "nav-item" (when selected? " selected"))
           :on-click on-click}
    (when icon-name (icon icon-name {:class "nav-icon"}))
    (dom/span {:class "nav-label"} label)
    (when badge
      (dom/span {:class "nav-badge"} (str badge)))))

;; =============================================================================
;; Layout Components
;; =============================================================================

(defn sidebar-section
  "Render a collapsible sidebar section.

   Usage:
     (sidebar-section {:title \"Pages\" :collapsed? false :on-toggle #(...)}
       children-vec)"
  ([opts]
   (sidebar-section opts []))
  ([{:keys [title collapsed? on-toggle]} children]
   (dom/div {:class (class-names "sidebar-section" (when collapsed? "collapsed"))}
     (dom/div {:class "sidebar-section-header"
               :on-click on-toggle}
       (icon (if collapsed? :chevron-right :chevron-down) {:class "toggle-icon"})
       (dom/span {:class "sidebar-section-title"} title))
     (when-not collapsed?
       (dom/simple-element :div {:class "sidebar-section-content"}
                           (if (vector? children) children [children]))))))

(defn panel
  "Render a panel with optional header and content.

   Usage:
     (panel {:title \"Details\"} children-vec)"
  ([opts]
   (panel opts []))
  ([{:keys [title class]} children]
   (dom/div {:class (class-names "panel" class)}
     (when title
       (dom/div {:class "panel-header"}
         (dom/h3 {:class "panel-title"} title)))
     (dom/simple-element :div {:class "panel-content"}
                         (if (vector? children) children [children])))))

;; =============================================================================
;; Form Components
;; =============================================================================

(defn text-input
  "Render a text input field.

   Usage:
     (text-input {:placeholder \"Search...\" :value q :on-change #(...)})"
  [attrs]
  (dom/input (merge {:type "text"
                     :class "text-input"}
                    attrs)))

(defn search-input
  "Render a search input with icon.

   Usage:
     (search-input {:value q :on-change #(...) :placeholder \"Search pages...\"})"
  [{:keys [value on-change placeholder on-clear]}]
  (dom/div {:class "search-input-wrapper"}
    (icon :search {:class "search-icon"})
    (text-input {:value value
                 :on-change on-change
                 :placeholder (or placeholder "Search...")
                 :class "search-input"})
    (when (and on-clear (seq value))
      (icon-button :x {:class "clear-search"
                       :on-click on-clear}))))


;; =============================================================================
;; Content-native diff rendering (static — NOT TipTap)
;; =============================================================================
;;
;; Shared because two surfaces ask the same question in opposite directions:
;; the proposal card asks "how would this fork differ from the present?", and
;; the timeline's audit panel asks "how does the present differ from then?".
;; One renderer, so a change never reads differently depending on which way you
;; approached it.

(defn static-block
  "One block's HTML, read-only. The diff never mounts an editor."
  [content extra-class]
  (el/div {:class (str "proposal-block " extra-class)
           ;; Proposal content is untrusted even before it is accepted.
           :innerHTML #?(:cljs (sanitize/html (or content ""))
                         :clj (or content ""))}))

(defn block-op-view
  "Render one block-level operation: `{:op :before :after}`, where `:before`
   and `:after` are `{:content <html>}`. An edit shows both, removal-first, so
   the eye reads it as a replacement rather than two unrelated blocks."
  [{:keys [op before after]}]
  (case op
    :block/add    (static-block (:content after) "diff-add")
    :block/remove (static-block (:content before) "diff-remove")
    :block/edit   (el/div {:class "diff-edit"}
                    (static-block (:content before) "diff-remove")
                    (static-block (:content after) "diff-add"))
    (el/div {:class "proposal-block"} (str op))))
