(ns is.simm.uis.web.desktop.views.personal-connections
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.personal-connections :as personal]))

(defn- token-form [actions offer row loading?]
  (el/form {:class "mail-settings-grid"
            :on-submit (fn [event]
                         (.preventDefault event)
                         (when (.. event -currentTarget -isConnected)
                           (let [elements (.. event -currentTarget -elements)
                                 input (.namedItem elements "token") token (.-value input)
                                 name (if row (:name row) (.-value (.namedItem elements "name")))]
                             (set! (.-value input) "")
                             ((:save actions) offer (or (:id row) (str (random-uuid))) (:revision row) name token))))}
           (when-not row
             (el/input {:class "settings-input" :name "name" :aria-label "Connection name"
                        :placeholder "Connection name, e.g. Work GitHub" :maxlength 80 :required true}))
           (el/input {:class "settings-input" :name "token" :type "password" :autocomplete "off"
                      :aria-label (if row (str "Replace token for " (:name row)) "GitHub token")
                      :placeholder "Fine-grained GitHub token" :minlength 20 :maxlength 512 :required true})
           (el/button {:type "submit" :class "settings-btn settings-btn--primary" :disabled loading?}
                      (if row "Save replacement" "Connect GitHub"))))

(defn view [state]
  ;; The owning column tracks this projection, like other settings sections.
  (let [a (personal/controller) _ ((:ensure a)) offer ((:basis a)) loading? (:loading? state)]
    (el/div {:class "settings-section"}
            (el/h3 {:class "settings-section-title"} "Connections")
            (el/p {:class "settings-section-desc"}
                  "Connect GitHub once, then authorize selected repositories in Memory settings. Tokens are encrypted on this server and are not shared with room members.")
            (when loading? (el/p {:class "memory-note" :role "status"} "Updating connections…"))
            (when-let [error (:error state)] (el/p {:class "proposal-error" :role "status"} error))
            (for [row (:rows state)]
              (el/div {:key (:id row) :class "settings-field"}
                      (el/strong {} (:name row))
                      (el/span {:class "settings-role-badge"} (if (:configured? row) "GitHub · configured" "Disconnected"))
                      (el/details {} (el/summary {} (if (:configured? row) "Replace token" "Reconnect"))
                                  (token-form a offer row loading?))
                      (when (:configured? row)
                        (el/button {:class "settings-btn" :disabled loading?
                                    :on-click (fn [event] (when (.. event -currentTarget -isConnected)
                                                            ((:revoke a) offer (:id row) (:revision row))))} "Disconnect everywhere"))))
            (token-form a offer nil loading?)
            (el/p {:class "settings-section-desc"} "Use a fine-grained token with Contents read and write for the repositories you want to publish to. Access is checked when you sync.")
            (el/button {:class "settings-btn" :disabled loading? :on-click (fn [_] ((:reload a) offer))} "Reload connections"))))
