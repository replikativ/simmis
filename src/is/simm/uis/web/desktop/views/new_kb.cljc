(ns is.simm.uis.web.desktop.views.new-kb
  "Knowledge base creation form rendered with spindel."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.views.core :as vc]
            [is.simm.uis.web.desktop.signals :as sig]
            #?(:cljs [is.simm.uis.web.desktop.chat-remote :as cr])
            #?(:cljs [is.simm.uis.web.desktop.remote :as rem])
            #?(:cljs [is.simm.runtimes.web :as web]))
  #?(:cljs (:require-macros [org.replikativ.spindel.dom.elements :as el])))

(defn render-new-kb-content
  "Render knowledge base creation form."
  []
  (let [current-user #?(:cljs @sig/current-user :clj nil)]
    (el/div {:class "settings-page"}
      (el/div {:class "settings-header"}
        (vc/icon "database" {:class "settings-header-icon"})
        (el/h2 {} "New Wiki"))

      (el/div {:class "settings-sections"}
        ;; KB name
        (el/div {:class "settings-section"}
          (el/h3 {:class "settings-section-title"} "Name")
          (el/p {:class "settings-section-desc"}
            "Each wiki is a separate database for pages and data.")
          (el/input {:id "new-kb-name"
                     :type "text"
                     :class "settings-input"
                     :placeholder "e.g. Research Notes, Work Projects"
                     :style {:width "100%"}}))

        ;; Create button
        (el/div {:class "settings-section"}
          (el/button {:class "settings-btn settings-btn--primary"
                      :style {:width "100%" :padding "10px"}
                      :on-click (fn [_]
                                  #?(:cljs
                                     (let [name-el (.getElementById js/document "new-kb-name")
                                           kb-name (when name-el (.-value name-el))]
                                       (when (and kb-name (seq kb-name) current-user)
                                         (let [s (cr/create-kb! web/server-id (:id current-user) kb-name)]
                                           (s (rem/on-ok "Could not create the wiki." (fn [_result]
                                                ;; Reload rooms+KBs to refresh nav
                                                (let [rs (cr/load-rooms!
                                                           web/server-id (:id current-user))]
                                                  (rs (fn [result] (reset! sig/user-rooms result))
                                                      (fn [err] (js/console.error "[new-kb] reload error:" err))))))
                                              (rem/on-error "Could not create the wiki.")))))
                                     :clj nil))}
            "Create Wiki"))))))
