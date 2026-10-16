(ns is.simm.uis.web.desktop.views.admin
  "Admin dashboard tab rendered with spindel.

   Sections:
   1. System stats (total users, total LLM usage, top models)
   2. User list (email, name, role, created, last login, budget)
   3. Per-user budget controls"
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.views.core :as vc]
            [is.simm.uis.web.desktop.signals :as sig]
            #?(:cljs [is.simm.uis.web.desktop.admin-remote :as ar])
            #?(:cljs [is.simm.runtimes.web :as web])
            #?(:cljs [org.replikativ.spindel.engine.core :as rtc])
            #?(:cljs [is.simm.uis.web.desktop.runtime :refer [runtime]]))
  #?(:cljs (:require-macros [org.replikativ.spindel.dom.elements :as el]
                            [org.replikativ.spindel.dom.foreach :refer [ifor-each]])))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- format-date [d]
  ;; `d` is ISO-8601 (`remote/wire-instant`). An unparseable value does not
  ;; throw — `js/Date.` yields an invalid date that prints "Invalid Date" —
  ;; so check for it rather than catch.
  #?(:cljs (let [date (some-> d js/Date.)]
             (if (and date (not (js/isNaN (.getTime date))))
               (.toLocaleDateString date "en-US"
                                    #js {:year "numeric" :month "short" :day "numeric"})
               "—"))
     :clj "—"))

(defn- format-dollars [microdollars]
  (str "$" (.toFixed (/ microdollars 1000000.0) 2)))

;; =============================================================================
;; Admin Component
;; =============================================================================

(defn render-admin-content
  "Render the admin dashboard content.
   data is the loaded admin data map (or nil if loading)."
  [data]
  (let [parties (:parties data)
        stats (:stats data)
        loading? (nil? data)]

    (el/div {:class "admin-page"}
      ;; Header
      (el/div {:class "admin-header"}
        (vc/icon "shield" {:class "settings-header-icon"})
        (el/h2 {} "Admin Dashboard"))

      (if loading?
        (el/div {:class "settings-loading"}
          (el/p {} "Loading admin data..."))

        (el/div {:class "settings-sections"}

          ;; --- System Stats ---
          (el/div {:class "admin-stats"}
            (el/div {:class "admin-stat-card"}
              (el/div {:class "admin-stat-value"} (str (or (:total-humans stats) 0)))
              (el/div {:class "admin-stat-label"} "Humans"))
            (el/div {:class "admin-stat-card"}
              (el/div {:class "admin-stat-value"} (str (or (:total-agents stats) 0)))
              (el/div {:class "admin-stat-label"} "Agents"))
            (el/div {:class "admin-stat-card"}
              (el/div {:class "admin-stat-value"}
                (format-dollars (or (:total-cost-microdollars stats) 0)))
              (el/div {:class "admin-stat-label"} "Total LLM Cost"))
            (el/div {:class "admin-stat-card"}
              (el/div {:class "admin-stat-value"}
                (str (count (or (:model-usage stats) []))))
              (el/div {:class "admin-stat-label"} "Models Used")))

          ;; --- Top Models ---
          (when (seq (:model-usage stats))
            (el/div {:class "settings-section"}
              (el/h3 {:class "settings-section-title"} "Model Usage")
              (el/div {:class "settings-env-list"}
                (ifor-each :model (:model-usage stats)
                  (fn [{:keys [model count total-cost]}]
                    (el/div {:key model :class "settings-env-row"}
                      (el/span {:class "settings-env-key"} model)
                      (el/span {:class "settings-env-value"} (str count " calls"))
                      (el/span {:class "settings-env-value"} (format-dollars (or total-cost 0)))))))))

          ;; --- Parties ---
          (el/div {:class "settings-section"}
            (el/h3 {:class "settings-section-title"} "Parties")
            (el/table {:class "admin-user-table"}
              (el/thead {}
                (el/tr {}
                  (el/th {} "Email")
                  (el/th {} "Name")
                  (el/th {} "Role")
                  (el/th {} "Created")
                  (el/th {} "Last Login")
                  (el/th {} "Budget")))
              (el/tbody {}
                (ifor-each #(str (:party/id %)) parties
                  (fn [party]
                    (let [pid (str (:party/id party))
                          budget (:budget party)]
                      (el/tr {:key pid}
                        (el/td {} (or (:party/email party) "—"))
                        (el/td {} (or (:party/display-name party) "—"))
                        (el/td {}
                          (el/span {:class "settings-role-badge"}
                            (if (= (:party/role party) :admin) "Admin" "User")))
                        (el/td {} (format-date (:party/created party)))
                        (el/td {} (format-date (:party/last-login party)))
                        (el/td {}
                          (if budget
                            (str (format-dollars (:used budget 0))
                                 " / "
                                 (format-dollars (:total budget 0)))
                            "Unlimited"))))))))))))))
