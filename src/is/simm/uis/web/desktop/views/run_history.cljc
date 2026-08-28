(ns is.simm.uis.web.desktop.views.run-history
  "Bounded room-level discovery of durable causal execution scopes."
  (:require [clojure.string :as str]
            [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.run-detail :as run-detail]
            [is.simm.uis.web.desktop.views.chat :as chat]
            [is.simm.uis.web.desktop.views.core :as vc])
  #?(:cljs (:require-macros [org.replikativ.spindel.dom.elements :as el])))

(defn- status-label [status]
  (case status
    :running "Working"
    :cancelling "Stopping"
    :completed "Completed"
    :failed "Failed"
    :cancelled "Cancelled"
    :waiting "Waiting"
    (some-> status name str/capitalize)))

(defn- short-id [id]
  (let [s (str id)] (subs s 0 (min 8 (count s)))))

(defn- time-label [millis]
  #?(:cljs (when millis (chat/msg-timestamp (js/Date. millis)))
     :clj (some-> millis str)))

(defn- duration-label [{:keys [status started-at ended-at updated-at]}]
  (when started-at
    (let [active? (contains? #{:running :cancelling} status)
          now #?(:cljs (js/Date.now) :clj (System/currentTimeMillis))
          end (if active? now (or ended-at updated-at now))
          millis (max 0 (- end started-at))]
      (cond
        (< millis 1000) (str millis " ms")
        (< millis 60000) (str #?(:cljs (.toFixed (/ millis 1000) 1)
                                 :clj (format "%.1f" (double (/ millis 1000))))
                               " s")
        (< millis 3600000) (str (quot millis 60000) " min")
        :else (str (quot millis 3600000) " h "
                   (quot (mod millis 3600000) 60000) " min")))))

(declare run-node)

(defn- run-node [{:keys [run children parent-missing?]} depth on-open-run]
  (let [status (or (:status run) :unknown)
        has-children? (seq children)]
    (el/div {:key (:id run)
             :class "run-history-node"
             :data-depth depth
             :data-run-id (:id run)}
      (el/button {:class "run-history-card"
                  :on-click (fn [event]
                              (when on-open-run (on-open-run event run)))}
        (el/span {:class (str "run-history-status-dot run-history-status-dot--"
                             (name status))})
        (el/span {:class "run-history-card-main"}
          (el/span {:class "run-history-card-title"}
            (or (:actor-name run) (some-> (:actor run) name) "Agent Run"))
          (el/span {:class "run-history-card-detail"}
            (status-label status)
            (when-let [duration (duration-label run)]
              (el/span {} (str " · " duration)))
            (when parent-missing?
              (el/span {:class "run-history-orphan"} " · parent outside window"))))
        (el/span {:class "run-history-card-tail"}
          (el/span {:class "run-history-time"}
            (time-label (or (:started-at run) (:created-at run))))
          (el/span {:class "run-history-id"} (short-id (:id run))))
        (vc/icon (if has-children? "git-branch" "chevron-right")
                 {:class "run-history-open-icon"}))
      (when has-children?
        (el/div {:class "run-history-children"}
          (map #(run-node % (inc depth) on-open-run) children))))))

(defn view
  [{:keys [room-name runs on-open-run on-back-room on-refresh]}]
  (let [forest (run-detail/causal-forest runs)
        active-count (count (filter #(contains? #{:running :cancelling}
                                                  (:status %)) runs))
        failed-count (count (filter #(= :failed (:status %)) runs))]
    (el/div {:class "run-history"}
      (el/header {:class "run-history-header"}
        (el/button {:class "run-history-back"
                    :title "Back to room"
                    :on-click (fn [_] (when on-back-room (on-back-room)))}
          (vc/icon "arrow-left"))
        (el/div {:class "run-history-heading"}
          (el/div {:class "run-history-kicker"}
            (vc/icon "orbit")
            (or room-name "Room"))
          (el/h2 {} "Runs")
          (el/p {} "Recent execution scopes, grouped by explicit containment."))
        (el/button {:class "run-history-refresh"
                    :title "Refresh Runs"
                    :on-click (fn [_] (when on-refresh (on-refresh)))}
          (vc/icon "refresh-cw")))
      (el/main {:class "run-history-body"}
        (el/div {:class "run-history-summary"}
          (el/span {} (str (count runs) " recent"))
          (when (pos? active-count)
            (el/span {:class "run-history-summary-active"}
              (str active-count " active")))
          (when (pos? failed-count)
            (el/span {:class "run-history-summary-failed"}
              (str failed-count " failed"))))
        (if (seq forest)
          (el/div {:class "run-history-list"}
            (map #(run-node % 0 on-open-run) forest))
          (el/div {:class "run-history-empty"}
            (vc/icon "orbit")
            (el/h3 {} "No recent Runs")
            (el/p {} "Agent executions in this room will appear here.")))))))
