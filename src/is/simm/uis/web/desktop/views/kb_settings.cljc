(ns is.simm.uis.web.desktop.views.kb-settings
  "Knowledge base settings — view and manage KB sharing, attached rooms, and deletion."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.views.core :as vc]
            [is.simm.uis.web.desktop.signals :as sig]
            #?(:cljs [is.simm.uis.web.desktop.chat-remote :as cr])
            #?(:cljs [is.simm.uis.web.desktop.remote :as rem])
            #?(:cljs [is.simm.runtimes.web :as web]))
  #?(:cljs (:require-macros [org.replikativ.spindel.dom.elements :as el]
                            [org.replikativ.spindel.dom.foreach :refer [ifor-each]])))

(defn- reload-kb-details! [kb-id-str]
  #?(:cljs
     (let [s (cr/load-kb-details! web/server-id kb-id-str)]
       (s (fn [result] (reset! sig/admin-data result))
          (fn [err] (js/console.error "[kb-settings] reload error:" err))))
     :clj nil))

(defn- reload-nav! []
  #?(:cljs
     (when-let [user @sig/current-user]
       (let [s (cr/load-rooms! web/server-id (:id user))]
         (s (fn [result] (reset! sig/user-rooms result))
            (fn [err] (js/console.error "[kb-settings] reload nav error:" err)))))
     :clj nil))

(defn render-kb-settings
  "Render KB settings page. data is the loaded KB details map."
  [data]
  (let [kb (:kb data)
        attached-rooms (:attached-rooms data)
        shared-parties (:shared-parties data)
        all-humans (:all-humans data)
        current-user #?(:cljs @sig/current-user :clj nil)
        kb-id-str (str (:kb/id kb))
        is-owner? (and current-user (= (str (:kb/owner kb)) (:id current-user)))]

    (el/div {:class "settings-page"}
      ;; Header
      (el/div {:class "settings-header"}
        (vc/icon "database" {:class "settings-header-icon"})
        (el/h2 {} (str (or (:kb/name kb) "Wiki") " — Settings")))

      (el/div {:class "settings-sections"}

        ;; --- Info Section ---
        (el/div {:class "settings-section"}
          (el/h3 {:class "settings-section-title"} "Details")
          (el/div {:class "settings-field"}
            (el/label {} "Name")
            (el/div {:class "settings-value"} (or (:kb/name kb) "—")))
          (el/div {:class "settings-field"}
            (el/label {} "Owner")
            (el/div {:class "settings-value settings-role-badge"}
              (if is-owner? "You" "Shared with you"))))

        ;; --- Shared Users Section ---
        (el/div {:class "settings-section"}
          (el/h3 {:class "settings-section-title"}
            (str "Shared With (" (count shared-parties) ")"))
          (el/div {:class "settings-env-list"}
            (if (seq shared-parties)
              (ifor-each #(str (:party/id %)) shared-parties
                (fn [p]
                  (let [pid (:party/id p)
                        name (:party/display-name p)
                        email (:party/email p)]
                    (el/div {:key (str pid) :class "settings-env-row"}
                      (el/span {:class "settings-env-key"} (or name email "Unknown"))
                      (el/span {:class "settings-env-value"} email)
                      (when is-owner?
                        (el/button {:class "settings-env-delete"
                                    :title "Unshare"
                                    :on-click (fn [_]
                                                #?(:cljs
                                                   (let [s (cr/unshare-kb! web/server-id kb-id-str (str pid))]
                                                     (s (rem/on-ok "Could not unshare the wiki." (fn [_] (reload-kb-details! kb-id-str)))
                                                        (rem/on-error "Could not unshare the wiki.")))
                                                   :clj nil))}
                          (vc/icon "x")))))))
              (el/div {:class "settings-empty"} "Not shared with anyone")))

          ;; Share with party dropdown (owner only)
          (when (and is-owner? all-humans)
            (el/div {:class "settings-env-add"}
              (el/select {:id "share-kb-user-select"
                          :class "settings-input"
                          :style {:flex "1"}}
                (el/option {:value ""} "Select user to share with...")
                (let [excluded (into #{(:id current-user)}
                                     (map #(str (:party/id %)) shared-parties))
                      shareable (filterv #(not (contains? excluded (str (:party/id %)))) all-humans)]
                  (ifor-each #(str (:party/id %)) shareable
                    (fn [p]
                      (let [pid (:party/id p)
                            name (:party/display-name p)
                            email (:party/email p)]
                        (el/option {:key (str pid) :value (str pid)}
                          (str (or name email))))))))
              (el/button {:class "settings-btn settings-btn--primary"
                          :on-click (fn [_]
                                      #?(:cljs
                                         (let [sel (.getElementById js/document "share-kb-user-select")
                                               uid (.-value sel)]
                                           (when (and uid (seq uid))
                                             (let [s (cr/share-kb! web/server-id kb-id-str uid)]
                                               (s (rem/on-ok "Could not share the wiki." (fn [_]
                                                    (set! (.-value sel) "")
                                                    (reload-kb-details! kb-id-str)))
                                                  (rem/on-error "Could not share the wiki.")))))
                                         :clj nil))}
                "Share"))))

        ;; --- Attached Rooms Section ---
        (el/div {:class "settings-section"}
          (el/h3 {:class "settings-section-title"}
            (str "Attached to Rooms (" (count attached-rooms) ")"))
          (el/p {:class "settings-section-desc"}
            "Rooms that use this wiki. Agents in these rooms can read its pages.")
          (el/div {:class "settings-env-list"}
            (if (seq attached-rooms)
              (ifor-each #(str (:room/id %)) attached-rooms
                (fn [room]
                  (el/div {:key (str (:room/id room)) :class "settings-env-row"}
                    (el/span {:class "settings-env-key"} (or (:room/name room) "Room"))
                    (el/span {:class "settings-env-value"} (name (or (:room/type room) :group))))))
              (el/div {:class "settings-empty"} "Not attached to any rooms"))))

        ;; --- Danger Zone ---
        (when is-owner?
          (el/div {:class "settings-section"}
            (el/h3 {:class "settings-section-title"} "Danger Zone")
            (el/button {:class "settings-btn"
                        :style {:background "#dc3545" :color "white" :width "100%" :padding "10px"}
                        :on-click (fn [_]
                                    #?(:cljs
                                       (when (js/confirm "Delete this wiki? This cannot be undone.")
                                         (let [s (cr/delete-kb! web/server-id kb-id-str)]
                                           (s (rem/on-ok "Could not delete the wiki." (fn [_]
                                                (reload-nav!)
                                                (reset! sig/admin-data nil)))
                                              (rem/on-error "Could not delete the wiki."))))
                                       :clj nil))}
              "Delete Wiki")))))))
