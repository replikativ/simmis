(ns is.simm.uis.web.desktop.views.run-inspector
  "Focused projection of one causal execution scope. The inspector is a lens
   over the room DB, not a second transcript or mutable workflow model."
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

(defn- duration-label [{:keys [started-at ended-at]}]
  (when started-at
    (let [end (or ended-at #?(:cljs (js/Date.now)
                              :clj (System/currentTimeMillis)))
          millis (max 0 (- end started-at))]
      (cond
        (< millis 1000) (str millis " ms")
        (< millis 60000) (str #?(:cljs (.toFixed (/ millis 1000) 1)
                                 :clj (format "%.1f" (double (/ millis 1000))))
                               " s")
        (< millis 3600000) (str (quot millis 60000) " min")
        :else (str (quot millis 3600000) " h " (quot (mod millis 3600000) 60000) " min")))))

(defn- short-id [id]
  (let [s (str id)]
    (subs s 0 (min 8 (count s)))))

(defn- timestamp-label [millis]
  #?(:cljs (when millis (chat/msg-timestamp (js/Date. millis)))
     :clj (str millis)))

(defn- tool-call->eval-entry [call actor-name syntax-pref]
  {:entity/uuid (:id call)
   :S.EvalEntry/tool (:name call)
   :S.EvalEntry/code (:input call)
   :S.EvalEntry/result (:result call)
   :S.EvalEntry/success? (not (:error? call))
   :S.EvalEntry/status (run-detail/tool-status-label call)
   :S.EvalEntry/duration-ms (:duration-ms call)
   :S.EvalEntry/approval (run-detail/authorization-label call)
   :S.EvalEntry/agent-name actor-name
   :S.EvalEntry/evaluated-at #?(:cljs (some-> (:started-at call) js/Date.)
                                :clj (:started-at call))
   :syntax-pref syntax-pref})

(defn- activity-segment [segment actor-name syntax-pref]
  (if (= :call (:kind segment))
    (chat/eval-entry-view
     (tool-call->eval-entry (:call segment) actor-name syntax-pref))
    (el/details {:key (str "activity-" (:id (first (:calls segment))))
                 :class (str "run-activity-group run-activity-group--"
                             (name (:variant segment)))}
      (el/summary {:class "run-activity-group-summary"}
        (vc/icon (case (:family segment)
                   :read "search"
                   :write "file-pen"
                   :execute "square-terminal"
                   :observe "eye"
                   :coordinate "network"
                   "workflow")
                 {:class "run-activity-group-icon"})
        (el/span {:class "run-activity-group-label"} (:label segment))
        (el/span {:class "run-activity-group-count"}
          (str (count (:calls segment)) " exact calls"))
        (vc/icon "chevron-down" {:class "run-activity-group-chevron"}))
      (el/div {:class "run-activity-group-body"}
        (map #(chat/eval-entry-view
               (tool-call->eval-entry % actor-name syntax-pref))
             (:calls segment))))))

(defn- message-card [message label on-open-message]
  (el/div {:key (str label "-" (:id message))
           :class "run-message-card"}
    (el/div {:class "run-section-eyebrow"} label)
    (el/button {:class "run-message-link"
                :title "Show this message in the room"
                :on-click (fn [_]
                            (when on-open-message
                              (on-open-message (:id message))))}
      (el/div {:class "run-message-meta"}
        (el/span {:class "run-message-author"}
          (or (:author-name message) "Unknown participant"))
        (el/span {:class "run-message-time"}
          (timestamp-label (:sent-at message))))
      (el/div {:class "run-message-content"}
        (or (:content message) "Message content is not available.")))))

(defn- related-run-button [run relation on-open-run]
  (let [status (:status run)]
    (el/button {:key (:id run)
                :class "run-related-link"
                :on-click (fn [_] (when on-open-run (on-open-run run)))}
      (vc/icon (if (= relation :parent) "corner-left-up" "git-branch")
               {:class "run-related-icon"})
      (el/span {:class "run-related-main"}
        (el/span {:class "run-related-relation"} (name relation))
        (el/span {:class "run-related-actor"}
          (or (:actor-name run) (some-> (:actor run) name) "Run")))
      (el/span {:class "run-related-id"} (short-id (:id run)))
      (when status
        (el/span {:class (str "run-status run-status--" (name status))}
          (status-label status))))))

(defn- provenance-view [run]
  (let [facts (remove (comp nil? second)
                      [["Kind" (some-> (:kind run) name)]])]
    (when (seq facts)
      (el/details {:class "run-provenance"}
        (el/summary {:class "run-provenance-summary"}
          (vc/icon "fingerprint")
          "Execution provenance")
        (el/dl {:class "run-provenance-grid"}
          (map-indexed
           (fn [i [label value]]
             (el/div {:key (str label "-" i) :class "run-provenance-fact"}
               (el/dt {} label)
               (el/dd {} (str value))))
           facts))))))

(defn- settlement-view [run on-promote]
  (when-let [settlement-status (:settlement-status run)]
    (let [review? (= :review settlement-status)
          world-live? (:world-live? run)]
      (el/section {:class (str "run-section run-world run-world--"
                              (name settlement-status))}
        (el/div {:class "run-section-heading"}
          (el/div {}
            (el/h3 {} "Work world")
            (el/p {} (run-detail/settlement-label run)))
          (el/span {:class (str "run-world-status run-world-status--"
                               (name settlement-status))}
            (-> settlement-status name str/capitalize)))
        (el/div {:class "run-world-facts"}
          (when-let [policy (:settlement-policy run)]
            (el/span {} (str "Policy · " (name policy))))
          (when-let [isolation (:isolation run)]
            (el/span {} (str "Isolation · " (name isolation))))
          (when-let [world-id (:world-id run)]
            (el/span {:class "run-world-id"} world-id))
          (when review?
            (el/span {:class (str "run-world-availability run-world-availability--"
                                  (if world-live? "live" "unavailable"))}
              (if world-live?
                "Capability retained"
                "Capability unavailable"))))
        (when review?
          (el/div {}
            (el/p {:class "run-world-guidance"}
              (if world-live?
                "This isolated world is available for durable proposal promotion."
                "The durable Run remains auditable, but this process no longer holds its settlement capability."))
            (when (and world-live? on-promote)
              (el/button {:class "btn btn-affirm btn-sm"
                          :on-click (fn [_] (on-promote run))}
                "File as proposal"))))))))

(defn view
  "Render one Run. `live-run` wins for transient :cancelling status; durable
   detail supplies history, effects, outputs, and structural relations."
  [{:keys [detail live-run fallback-run room-name syntax-pref on-back-room
           on-open-message on-open-run on-cancel on-promote]}]
  (let [durable-run (:run detail)
        run (when (or fallback-run durable-run live-run)
              (merge fallback-run durable-run live-run))
        tool-calls (:tool-calls detail)
        activity (run-detail/group-tool-activity tool-calls)
        outputs (remove #(= (:id %) (:id (:trigger detail))) (:messages detail))
        parent-id (:parent-id run)
        known-parent (or (:parent detail) (when parent-id {:id parent-id}))
        status (or (:status run) :loading)
        active? (contains? #{:running :cancelling} status)]
    (el/div {:class "run-inspector" :data-run-id (:id run)}
      (el/header {:class "run-inspector-header"}
        (el/button {:class "run-inspector-back"
                    :title "Back to room"
                    :on-click (fn [_] (when on-back-room (on-back-room)))}
          (vc/icon "arrow-left"))
        (el/div {:class "run-inspector-title-block"}
          (el/div {:class "run-inspector-kicker"}
            (vc/icon "orbit")
            (or room-name "Room")
            (el/span {} "Execution scope"))
          (el/h2 {:class "run-inspector-title"}
            (or (:actor-name run) (some-> (:actor run) name) "Agent Run"))
          (el/div {:class "run-inspector-meta"}
            (el/span {:class (str "run-status run-status--" (name status))}
              (status-label status))
            (when-let [duration (duration-label run)]
              (el/span {} duration))
            (el/span {:class "run-inspector-id"} (short-id (:id run)))))
        (when active?
          (el/button {:class "run-inspector-stop"
                      :disabled (= :cancelling (:status run))
                      :on-click (fn [_]
                                  (when on-cancel (on-cancel (:id run))))}
            (vc/icon "square")
            (if (= :cancelling (:status run)) "Stopping" "Stop"))))

      (el/main {:class "run-inspector-body"}
        (cond
          (nil? run)
          (el/div {:class "run-inspector-empty"}
            (vc/icon "loader")
            (el/h3 {} "Loading Run…")
            (el/p {} "Waiting for this room replica to provide the execution record."))

          :else
          (el/div {:class "run-inspector-sections"}
            (when-let [trigger (:trigger detail)]
              (el/section {:class "run-section"}
                (message-card trigger "Triggered by" on-open-message)))

            (when (or parent-id (seq (:children detail)))
              (el/section {:class "run-section"}
                (el/div {:class "run-section-heading"}
                  (el/h3 {} "Causal structure")
                  (el/p {} "Explicit containment, separate from message chronology."))
                (el/div {:class "run-related-list"}
                  (when known-parent
                    (related-run-button known-parent :parent on-open-run))
                  (map #(related-run-button % :child on-open-run)
                       (:children detail)))))

            (settlement-view run on-promote)

            (el/section {:class "run-section"}
              (el/div {:class "run-section-heading"}
                (el/h3 {} "Activity")
                (el/p {}
                  (cond
                    (seq tool-calls)
                    (str (count tool-calls) " exact tool call"
                         (when (not= 1 (count tool-calls)) "s")
                         ", compressed without hiding failures.")
                    active? "Waiting for the first recorded effect."
                    :else "No tool effects were recorded for this Run.")))
              (when (seq activity)
                (el/div {:class "run-activity"}
                  (map #(activity-segment % (:actor-name run) syntax-pref)
                       activity))))

            (when (seq outputs)
              (el/section {:class "run-section"}
                (el/div {:class "run-section-heading"}
                  (el/h3 {} "Messages")
                  (el/p {} "Durable room messages produced inside this Run."))
                (el/div {:class "run-output-list"}
                  (map #(message-card % "Output" on-open-message) outputs))))

            (when (or (:reason run) (:error run))
              (el/section {:class "run-section run-section--diagnostic"}
                (el/h3 {} (if (:error run) "Run failed" "Run ended"))
                (el/p {} (or (:error run) (some-> (:reason run) name) (:reason run)))))

            (provenance-view run)))))))
