(ns is.simm.uis.web.desktop.views.workflow-setup
  "Inline human confirmation of saved setup, using the ordinary workflow journal."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.workflow-actions :as actions]
            [is.simm.uis.web.desktop.workflow-editor :as editor]
            [is.simm.documents.workflow-links :as links]))

(defn view [workflow state ui]
  (let [draft (get-in state [:drafts workflow]) offered (:setup-offer draft)
        scheduled? (= :schedule (:action offered)) benchmark? (= :benchmark (:action offered))
        journal (or (:schedule-submission draft) (:submission draft))
        reply (:reply journal)
        schedules (editor/workflow-activations state (:room-id ui) workflow)
        payload (:payload offered)
        execution (if scheduled? (:execution payload) payload)
        allowance (get-in state [:readiness :allowance])
        funded? (and (= :observed (:status allowance))
                     (>= (js/Number (:microdollars allowance)) (get-in execution [:limits :budget-microdollars] 1))
                     (>= (js/Number (:model-dispatches allowance)) (get-in execution [:limits :max-dispatches] 1)))
        ready? (and draft (= :ready (:readiness-status state))
                    (editor/source-current? draft) (get-in draft [:source :inspection :ok])
                    (not (:access-denied? state)) (:merge? (:permissions state))
                    (or (not benchmark?) (:operator? (:permissions state))))
        error (or (get-in state [:errors :setup]) (get-in state [:errors :submit])
                  (get-in state [:errors :schedule]))]
    (el/section {:class "workflow-chat-card" :aria-label (str "Workflow setup · " workflow)}
      (el/strong {} (or (get-in draft [:source :definition :title]) workflow))
      (when (seq schedules)
        (el/p {} (str "Already configured: " (count schedules) " schedule(s). ")
          (el/button {:type "button" :on-click (fn [_] ((:open-schedules ui)))} "Manage schedules")))
      (cond
        (:access-denied? state) (el/p {:role "status"} "This room is no longer available.")
        journal
        (el/div {}
          (el/p {:role "status"}
            (if reply (if (:schedule-submission draft) "Schedule saved; view its current status." "Request acknowledged; view its current status.")
                "The request is unresolved. It will not be submitted again automatically."))
          (if-let [job (:job reply)]
            (el/button {:type "button" :on-click (fn [_] ((:open-run ui) job))} "View Run")
            (if (and reply (:schedule-submission draft))
              (el/button {:type "button" :on-click (fn [_] ((:open-schedules ui)))} "Manage schedule")
              (el/button {:type "button" :on-click (fn [_] ((:open-advanced ui) workflow))} "Inspect request"))))
        (and offered (not (:setup-dismissed? draft)))
        (el/div {}
          (el/p {} (case (:action offered) :schedule (editor/cadence-label (:cadence payload))
                                        :benchmark "Run a benchmark once" "Run once"))
          (el/p {} (str "Model: " (:model-label offered) " · "
                       (case (:model-source offered) :user-preference "your model preference" :deployment-default "server default" :human-choice "your chosen model" "proposed exact model")
                       ". Connection is checked at execution."))
          (el/p {} (str "At most $" (/ (get-in execution [:limits :budget-microdollars]) 1000000)
                       " and " (get-in execution [:limits :max-dispatches]) " model requests"
                       (when scheduled? " per occurrence") ". Nothing merges automatically."))
          (when scheduled?
            (el/p {} "Repeats until paused or held. Each occurrence draws from the shared finite allowance; it is never refilled automatically."))
          (el/p {} (if (= "discard" (get-in execution [:limits :settlement]))
                     "Capture output and discard the execution world; wiki changes need separate review."
                     "Retain the execution world as a proposal for review."))
          (el/p {} (str "Results in this room’s Memory: " (pr-str (:outputs offered))))
          (when benchmark?
            (el/p {} (str "Checks the saved workflow and checker on "
                         (count (:cases payload)) " case(s), " (:repetitions payload) " repetition(s). "
                         "Candidate worlds are discarded. A result summary returns to this chat; "
                         "creating its Memory report is a separate action.")))
          (when (and benchmark? (not (:operator? (:permissions state))))
            (el/p {:role "status"} "Benchmarking executes the saved checker and requires an installation operator with room authority."))
          (el/details {}
            (el/summary {} "What this workflow does")
            (el/p {} (:task offered))
            (el/p {} (str "Saved revision: " (:expected-owned-source execution)))
            (el/p {} (str "Execution ceilings: " (get-in execution [:limits :max-model-steps])
                         " steps, " (/ (get-in execution [:limits :timeout-ms]) 1000) " seconds.")))
          (when-not ((:offer-current? ui) workflow offered)
            (el/p {:role "status"} "Setup or permissions changed. Review the current proposal again."))
          (when-not funded?
            (el/p {:role "status"} "This room needs sufficient execution allowance. Advanced lets its owner provide a finite allowance; confirmation never adds it automatically."))
          (el/div {:class "workflow-toolbar"}
            (el/button {:type "button" :disabled (not (and ready? funded? ((:offer-current? ui) workflow offered)))
                        :on-click (fn [event]
                                    (when (.. event -currentTarget -isConnected)
                                      ((:accept-setup ui) workflow offered)))}
              (cond scheduled? (if (seq schedules) "Confirm additional schedule" "Confirm schedule")
                    benchmark? "Confirm benchmark" :else "Confirm and run"))
            (el/button {:type "button" :disabled (not ready?)
                        :on-click (fn [_] ((:prepare-setup ui) workflow nil))} "Review current setup")
            (el/button {:type "button" :on-click (fn [_] ((:change ui) workflow assoc :setup-dismissed? true))} "Dismiss")))
        :else
        (el/div {}
          (el/p {} "Review the current saved workflow and its setup here. Detailed configuration is optional; opening this card starts nothing.")
          (el/button {:type "button" :disabled (not ready?)
                      :on-click (fn [_] ((:prepare-setup ui) workflow nil))} "Review setup")))
      (when error (el/p {:role "alert"} error))
      (el/div {:class "workflow-toolbar"}
        (el/button {:type "button" :disabled (nil? draft) :on-click (fn [_] ((:open-advanced ui) workflow))} "Advanced")
        (el/a {:href (links/route {:room (:room-id ui) :workflow workflow :action :benchmark})} "Benchmark")))))

(defn owner [workflow] [:chat-workflow workflow])

(defn- controller [{:keys [room-id workflow tab-id col-id]}]
  (actions/controller {:room-id room-id :tab-id tab-id :workflow-state-owner (owner workflow) :col-id col-id
                       :workflow-name workflow :workflow-action :run}))

(defn observe! [data]
  ;; Called by the owning column Spin, outside the keyed message renderer.
  ((:observe (controller data))))

(defn panel [{:keys [workflow state] :as data}]
  ;; Ordinary DOM only: message-view is not a CPS call boundary.
  (view workflow state (controller data)))
