(ns is.simm.uis.web.desktop.views.settings
  "Settings tab rendered with spindel.

   Sections:
   1. Profile (display name, email — read-only)
   2. Environment Variables (key/value editor)
   3. Model Preference (dropdown)
   4. Code View (Clojure vs Superficie syntax toggle)
   5. Usage & Budget (remaining, breakdown)"
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.views.core :as vc]
            [is.simm.uis.web.desktop.views.model-picker :as model-picker]
            [is.simm.uis.web.desktop.signals :as sig]
            #?(:cljs [is.simm.uis.web.desktop.settings-remote :as sr])
            #?(:cljs [is.simm.uis.web.desktop.chat-remote :as chat-remote])
            #?(:cljs [is.simm.uis.web.desktop.mail-remote :as mail-remote])
            #?(:cljs [is.simm.uis.web.desktop.message-notify-sync :as mns])
            #?(:cljs [is.simm.runtimes.web :as web]))
  #?(:cljs (:require-macros [org.replikativ.spindel.dom.elements :as el]
                            [org.replikativ.spindel.dom.foreach :refer [ifor-each]])))

;; =============================================================================
;; Reload Helper
;; =============================================================================

#?(:cljs
   (defn- reload-settings!
     "Reload settings from server directly into the signal.
      Keeps current data visible until new data arrives (no loading flash)."
     []
     (when-let [user @sig/current-user]
       (let [s (sr/load-settings! web/server-id (:id user))]
         (s (fn [result]
               (reset! sig/settings-data result)
               (when-let [syn (get-in result [:ui-prefs :ui-pref/syntax])]
                 (reset! sig/syntax-pref syn)))
            (fn [err] (js/console.error "[settings] reload error:" err)))))))

#?(:cljs
   (defn- mail-form-config []
     (let [value #(.-value (.getElementById js/document %))]
       {:name (value "mail-name-input")
        :email (value "mail-email-input")
        :imap {:host (value "mail-host-input")
               :port (js/parseInt (value "mail-port-input") 10)
               :user (value "mail-user-input")
               :pass (value "mail-password-input")
               :insecure? (.-checked (.getElementById js/document
                                                       "mail-insecure-input"))}})))

;; =============================================================================
;; Settings Component
;; =============================================================================

(defn render-settings-content
  "Render the settings tab content.
   data is the loaded settings map."
  [data]
  (let [profile (:profile data)
        env-vars (:env-vars data)
        budget (:budget data)
        current-user #?(:cljs @sig/current-user :clj nil)]

    (el/div {:class "settings-page"}
      ;; Header
      (el/div {:class "settings-header"}
        (vc/icon "settings" {:class "settings-header-icon"})
        (el/h2 {} "Settings"))

      (el/div {:class "settings-sections"}

        ;; --- Profile Section ---
        (el/div {:class "settings-section"}
          (el/h3 {:class "settings-section-title"} "Profile")
          (el/div {:class "settings-field"}
            (el/label {} "Email")
            (el/div {:class "settings-value"} (or (:party/email profile) "—")))
          (el/div {:class "settings-field"}
            (el/label {} "Name")
            (el/div {:class "settings-value"} (or (:party/display-name profile) "—")))
          (el/div {:class "settings-field"}
            (el/label {} "Role")
            (el/div {:class "settings-value settings-role-badge"}
              (if (= (:party/role profile) :admin) "Admin" "User"))))

        ;; --- Mail knowledge sources ---
        (el/div {:class "settings-section"}
          (el/h3 {:class "settings-section-title"} "Email accounts")
          (el/p {:class "settings-section-desc"}
            "Connect an IMAP mailbox for read-only browse and search. Credentials are encrypted server-side and are never returned to this browser.")
          (el/div {:class "settings-env-list"}
            (if (seq (:mail-accounts data))
              (ifor-each #(str (:mail-account/id %)) (:mail-accounts data)
                (fn [account]
                  (el/div {:key (str (:mail-account/id account))
                           :class "settings-env-row"}
                    (el/span {:class "settings-env-key"}
                      (:mail-account/name account))
                    (el/span {:class "settings-env-value"}
                      (:mail-account/email account))
                    (el/span {:class "settings-role-badge"}
                      (name (or (:mail-account/status account) :configured)))
                    (el/button {:class "settings-btn"
                                :on-click (fn [_]
                                            #?(:cljs
                                               (let [s (mail-remote/sync-mail-account!
                                                        web/server-id
                                                        (str (:mail-account/id account)))]
                                                 (s (fn [_] (reload-settings!))
                                                    #(js/console.error "[mail] sync failed" %)))
                                               :clj nil))}
                      "Sync"))))
              (el/div {:class "settings-empty"} "No email accounts configured")))
          (el/div {:class "mail-settings-grid"}
            (el/input {:id "mail-name-input" :class "settings-input"
                       :placeholder "Account name"})
            (el/input {:id "mail-email-input" :class "settings-input" :type "email"
                       :placeholder "you@example.com" :autocomplete "email"})
            (el/input {:id "mail-host-input" :class "settings-input"
                       :placeholder "imap.example.com"})
            (el/input {:id "mail-port-input" :class "settings-input" :type "number"
                       :value "993" :placeholder "993"})
            (el/input {:id "mail-user-input" :class "settings-input"
                       :placeholder "IMAP username" :autocomplete "username"})
            (el/input {:id "mail-password-input" :class "settings-input" :type "password"
                       :placeholder "IMAP password" :autocomplete "new-password"})
            (el/label {:class "mail-insecure-label"}
              (el/input {:id "mail-insecure-input" :type "checkbox"})
              "Allow an unverified TLS certificate")
            (el/div {:class "mail-settings-actions"}
              (el/button {:class "settings-btn"
                          :on-click (fn [_]
                                      #?(:cljs
                                         (let [s (mail-remote/test-mail-connection!
                                                  web/server-id (mail-form-config))]
                                           (s (fn [result]
                                                (js/alert
                                                 (str "Connected. "
                                                      (count (:folders result))
                                                      " folders found.")))
                                              #(js/alert (str "Connection failed: " %))))
                                         :clj nil))}
                "Test connection")
              (el/button {:class "settings-btn settings-btn--primary"
                          :on-click (fn [_]
                                      #?(:cljs
                                         (let [s (mail-remote/save-mail-account!
                                                  web/server-id (mail-form-config))]
                                           (s (fn [_]
                                                (set! (.-value (.getElementById
                                                               js/document
                                                               "mail-password-input")) "")
                                                (reload-settings!))
                                              #(js/alert (str "Could not save account: " %))))
                                         :clj nil))}
                "Save account"))))

        ;; --- Environment Variables Section ---
        (el/div {:class "settings-section"}
          (el/h3 {:class "settings-section-title"} "Environment Variables")
          (el/p {:class "settings-section-desc"}
            "Configure API keys for integrations. Allowed prefixes: SLACK_, GITHUB_, OPENAI_, ANTHROPIC_, FIREWORKS_, GOOGLE_")
          (el/div {:class "settings-env-list"}
            (if (seq env-vars)
              (ifor-each :key env-vars
                (fn [{:keys [key value]}]
                  (el/div {:key key :class "settings-env-row"}
                    (el/span {:class "settings-env-key"} key)
                    (el/span {:class "settings-env-value"}
                      (str (subs value 0 (min 4 (count value))) "****"))
                    (el/button {:class "settings-env-delete"
                                :title "Remove"
                                :on-click (fn [_]
                                            #?(:cljs
                                               (when-let [user @sig/current-user]
                                                 (let [s (sr/delete-env-var!
                                                           web/server-id (:id user) key)]
                                                   (s (fn [_]
                                                        ;; Reset to nil triggers reload on next render
                                                        (reload-settings!))
                                                      (fn [err] (js/console.error "[settings] delete error:" err)))))
                                               :clj nil))}
                      (vc/icon "x")))))
              (el/div {:class "settings-empty"} "No environment variables set")))
          ;; Add new env var form
          (el/div {:class "settings-env-add"}
            (el/input {:id "env-key-input"
                       :type "text"
                       :class "settings-input"
                       :placeholder "KEY_NAME"})
            (el/input {:id "env-value-input"
                       :type "password"
                       :class "settings-input"
                       :placeholder "value"})
            (el/button {:class "settings-btn settings-btn--primary"
                        :on-click (fn [_]
                                    #?(:cljs
                                       (let [key-el (.getElementById js/document "env-key-input")
                                             val-el (.getElementById js/document "env-value-input")
                                             k (.-value key-el)
                                             v (.-value val-el)]
                                         (when (and (seq k) (seq v))
                                           (when-let [user @sig/current-user]
                                             (let [s (sr/save-env-var!
                                                       web/server-id (:id user) k v)]
                                               (s (fn [_]
                                                    ;; Reset to nil triggers reload on next render
                                                    (reload-settings!))
                                                  (fn [err]
                                                    (js/console.error "[settings] save error:" err)))))))
                                       :clj nil))}
              "Add")))

        ;; --- Model Preference Section ---
        (el/div {:class "settings-section"}
          (el/h3 {:class "settings-section-title"} "Model Preference")
          (el/p {:class "settings-section-desc"}
            "The preference your inheriting agents use. Latest tracks new releases; an explicit choice is the preferred version and may resolve only to a newer version in the same family and provider if withdrawn. Saving switches currently joined inheriting agents in place, preserving their conversation state and inboxes; agents with explicit overrides are unchanged.")
          (el/div {:class "settings-model-list"}
            ;; `selected?` lives IN the item; the key stays the model value.
            ;; ifor-each memoizes on item equality and cannot see a closure
            ;; variable, so computing it inside the render function would leave
            ;; the old row ticked. Putting it in the KEY is worse: the key is
            ;; identity, so a row that gained a tick read as a NEW row and the
            ;; list rendered both copies side by side.
            (ifor-each :value
              (mapv (fn [c]
                      (assoc c :selected? (= (:value c) (:party/preferred-model profile))))
                    (:model-choices data))
              (fn [row]
                (model-picker/render-option
                 row
                 (fn [{:keys [value]}]
                   #?(:cljs
                      (when-let [user @sig/current-user]
                        (let [s (sr/save-preferred-model!
                                 web/server-id (:id user) value)]
                          (s (fn [_] (reload-settings!))
                             (fn [err]
                               (js/console.error "[settings] save error:" err)))))
                      :clj nil)))))))

        ;; --- Code View Section ---
        (let [current-syntax (or (get-in data [:ui-prefs :ui-pref/syntax]) :clojure)]
          (el/div {:class "settings-section"}
            (el/h3 {:class "settings-section-title"} "Code View")
            (el/p {:class "settings-section-desc"}
              "Choose how code blocks are displayed in the wiki editor.")
            (el/div {:class "settings-model-list"}
              (ifor-each first [[:clojure "Clojure" "Native S-expression syntax (editable)"]
                                [:superficie "Superficie" "Readable indented syntax (read-only)"]]
                (fn [[kw label desc]]
                  (let [selected? (= kw current-syntax)]
                    (el/div {:key (str kw)
                             :class (vc/class-names "settings-model-option"
                                                    (when selected? "selected"))
                             :on-click (fn [_]
                                         #?(:cljs
                                            (do (reset! sig/syntax-pref kw)
                                                (when-let [user @sig/current-user]
                                                  (let [s (sr/save-syntax-pref!
                                                            web/server-id (:id user) kw)]
                                                    (s (fn [_] (reload-settings!))
                                                       (fn [err] (js/console.error "[settings] save syntax error:" err))))))
                                            :clj nil))}
                      (el/div {:class "settings-model-name"} label)
                      (el/div {:class "settings-model-provider"} desc)
                      (when selected?
                        (vc/icon "check" {:class "settings-model-check"})))))))))

        ;; --- Usage & Budget Section ---
        (el/div {:class "settings-section"}
          (el/h3 {:class "settings-section-title"} "Usage & Budget")
          (if budget
            (let [total (:total budget)
                  used (:used budget)
                  remaining (:remaining budget)
                  pct (if (pos? total) (* 100 (/ used total)) 0)]
              (el/div {:class "settings-budget"}
                (el/div {:class "settings-budget-bar"}
                  (el/div {:class "settings-budget-fill"
                           :style {:width (str (min 100 pct) "%")}}))
                (el/div {:class "settings-budget-stats"}
                  (el/span {} (str "Used: $" (/ used 1000000.0) " / $" (/ total 1000000.0)))
                  (el/span {} (str "Remaining: $" (/ remaining 1000000.0))))))
            (el/div {:class "settings-empty"}
              "No budget configured — usage is unlimited")))

        ;; --- Knowledge Bases Section ---
        (let [kbs (:knowledge-bases data)]
          (el/div {:class "settings-section"}
            (el/h3 {:class "settings-section-title"}
              (str "Wikis (" (count kbs) ")"))
            (el/p {:class "settings-section-desc"}
              "Your personal wikis. Each wiki is a separate database for pages and data.")
            (el/div {:class "settings-env-list"}
              (if (seq kbs)
                (ifor-each #(str (:kb/id %)) kbs
                  (fn [kb]
                    (el/div {:key (str (:kb/id kb)) :class "settings-env-row"}
                      (el/span {:class "settings-env-key"} (:kb/name kb))
                      (el/span {:class "settings-env-value settings-role-badge"}
                        (if (and current-user
                                 (= (str (:kb/owner kb)) (:id current-user)))
                          "owned"
                          "shared")))))
                (el/div {:class "settings-empty"} "No wikis")))
            ;; Create new KB
            (el/div {:class "settings-env-add"}
              (el/input {:id "new-kb-name"
                         :type "text"
                         :class "settings-input"
                         :placeholder "New wiki name"})
              (el/button {:class "settings-btn settings-btn--primary"
                          :on-click (fn [_]
                                      #?(:cljs
                                         (let [input (.getElementById js/document "new-kb-name")
                                               kb-name (.-value input)]
                                           (when (and kb-name (seq kb-name))
                                             (when-let [user @sig/current-user]
                                               (let [s (chat-remote/create-kb!
                                                         web/server-id (:id user) kb-name)]
                                                 (s (fn [_]
                                                      (set! (.-value input) "")
                                                      (reload-settings!))
                                                    (fn [err]
                                                      (js/console.error "[settings] create KB error:" err)))))))
                                         :clj nil))}
                "Create"))))

        ;; --- Notifications Section ---
        (el/div {:class "settings-section"}
          (el/h3 {:class "settings-section-title"} "Notifications")
          (el/p {:class "settings-section-desc"}
            "Get a browser notification when someone @mentions you in a chat. You are only interrupted for mentions; everything else stays quiet.")
          #?(:cljs
             (cond
               (not (mns/supported?))
               (el/div {:class "settings-empty"} "This browser does not support notifications.")
               (mns/enabled?)
               (el/div {:class "settings-value settings-role-badge"} "Enabled")
               :else
               (el/button {:class "settings-btn settings-btn--primary"
                           :on-click (fn [_] (mns/request-permission!))}
                 "Enable notifications"))
             :clj nil))

        ;; --- Local Data Section (reset the client's cached replicas) ---
        (el/div {:class "settings-section"}
          (el/h3 {:class "settings-section-title"} "Local Data")
          (el/p {:class "settings-section-desc"}
            "simmis caches your databases in this browser for fast reads. If the app won't load, or a page looks stale or broken, reset the local copy — your data lives on the server and re-syncs on reload.")
          (el/button {:class "settings-btn"
                      :on-click (fn [_]
                                  #?(:cljs
                                     (when (js/confirm "Reset local data? This clears this browser's cached databases and reloads. Your server data is not affected.")
                                       (web/wipe-all-local-data!))
                                     :clj nil))}
            "Reset local data"))))))
