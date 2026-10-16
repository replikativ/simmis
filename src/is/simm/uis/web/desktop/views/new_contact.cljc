(ns is.simm.uis.web.desktop.views.new-contact
  "Add-a-contact tab — handle field + directory of human parties on this instance."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.views.core :as vc]
            [is.simm.uis.web.desktop.signals :as sig]
            #?(:cljs [is.simm.uis.web.desktop.chat-remote :as cr])
            #?(:cljs [is.simm.uis.web.desktop.remote :as rem])
            #?(:cljs [is.simm.runtimes.web :as web]))
  #?(:cljs (:require-macros [org.replikativ.spindel.dom.elements :as el]
                            [org.replikativ.spindel.dom.foreach :refer [ifor-each]])))

#?(:cljs
   (defn- add-by-id! [current-user target-id]
     (let [s (cr/add-contact! web/server-id (:id current-user) target-id)]
       (s (rem/on-ok "Could not add that contact." (fn [_]
            (let [rs (cr/load-rooms! web/server-id (:id current-user))]
              (rs (fn [r] (reset! sig/user-rooms r))
                  (fn [err] (js/console.error "[new-contact] reload error:" err))))))
          (rem/on-error "Could not add that contact.")))))

(defn render-new-contact-content
  "all-humans is a list of party maps or nil.
   existing-contact-ids is a set of party-id strings already in contacts."
  [all-humans existing-contact-ids]
  (let [current-user #?(:cljs @sig/current-user :clj nil)]
    (el/div {:class "settings-page"}
      (el/div {:class "settings-header"}
        (vc/icon "user-plus" {:class "settings-header-icon"})
        (el/h2 {} "New Contact"))

      (el/div {:class "settings-sections"}

        ;; --- Add by handle ---
        (el/div {:class "settings-section"}
          (el/h3 {:class "settings-section-title"} "Add by handle")
          (el/p {:class "settings-section-desc"}
            "Enter a party handle (e.g. demo, or demo@simm.is once federation lands).")
          (el/div {:class "settings-env-add"}
            (el/input {:id "new-contact-handle"
                       :type "text"
                       :class "settings-input"
                       :placeholder "handle"
                       :style {:flex "1"}})
            (el/button {:class "settings-btn settings-btn--primary"
                        :on-click (fn [_]
                                    #?(:cljs
                                       (let [input (.getElementById js/document "new-contact-handle")
                                             handle (.trim (.-value input))]
                                         (when (and current-user (seq handle))
                                           (let [s (cr/add-contact-by-handle!
                                                     web/server-id (:id current-user) handle)]
                                             (s (fn [result]
                                                  (if (= :ok (:status result))
                                                    (do (set! (.-value input) "")
                                                        (let [rs (cr/load-rooms! web/server-id
                                                                                 (:id current-user))]
                                                          (rs (fn [r] (reset! sig/user-rooms r))
                                                              (fn [err]
                                                                (js/console.error "[new-contact] reload error:" err)))))
                                                    (js/alert (str "Handle not found: " handle))))
                                                (rem/on-error "Could not add that contact.")))))
                                       :clj nil))}
              "Add")))

        ;; --- Directory ---
        (el/div {:class "settings-section"}
          (el/h3 {:class "settings-section-title"} "Directory")
          (el/p {:class "settings-section-desc"}
            "People on this instance you can add as contacts.")
          (if all-humans
            (el/div {:class "settings-env-list"}
              (let [addable (filterv (fn [h]
                                       (and current-user
                                            (not= (str (:party/id h)) (:id current-user))
                                            (not (contains? existing-contact-ids
                                                            (str (:party/id h))))))
                                     all-humans)]
                (if (seq addable)
                  (ifor-each #(str (:party/id %)) addable
                    (fn [h]
                      (let [pid (str (:party/id h))
                            name (:party/display-name h)
                            handle (:party/handle h)
                            email (:party/email h)]
                        (el/div {:key pid :class "settings-env-row"}
                          (el/span {:class "settings-env-key"}
                            (or name email "Unknown"))
                          (el/span {:class "settings-env-value"}
                            (if handle (str "@" handle) email))
                          (el/button {:class "settings-btn settings-btn--secondary"
                                      :on-click (fn [_]
                                                  #?(:cljs (add-by-id! current-user pid)
                                                     :clj nil))}
                            "Add")))))
                  (el/div {:class "settings-empty"} "Everyone already in your contacts."))))
            (el/div {:class "settings-loading"} "Loading…")))))))
