(ns is.simm.uis.web.desktop.views.benchmark-result
  "Actions beside an immutable benchmark-result message. The message's native
   receipt stays authoritative; rendering never publishes report files."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.benchmark-report-actions :as reports]))

(defn actions [owner scorecard state]
  (let [controller (reports/controller owner)]
    (el/div {:class "workflow-note workflow-output-review benchmark-result-actions"}
            (if (:result state)
              (el/button {:class "btn btn-secondary" :type "button"
                          :on-click (fn [_] ((:open controller) scorecard))} "Open report in Memory")
              (el/button {:class "btn btn-secondary" :type "button" :disabled (boolean (:loading? state))
                          :on-click (fn [_] ((:publish controller) scorecard))}
                         (cond (:loading? state) "Creating report…"
                               (:pending state) "Retry report publication"
                               :else "Create report in Memory")))
            (when (= :preserved (get-in state [:result :index :status]))
              (el/p {:role "status"} "Report created; your existing Reports index was kept."))
            (when-let [error (:error state)] (el/p {:role "alert"} error)))))
