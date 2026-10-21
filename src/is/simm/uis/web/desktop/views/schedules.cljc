(ns is.simm.uis.web.desktop.views.schedules
  "Pinned workflow activations and the separate legacy agent-task agenda."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [org.replikativ.spindel.dom.foreach]
            [org.replikativ.spindel.incremental.interval :as iv]
            [org.replikativ.spindel.effects.track :refer [track]]
            [is.simm.uis.web.desktop.views.core :as vc]
            [is.simm.uis.web.desktop.signals :as sig]
            #?(:cljs [org.replikativ.spindel.engine.core :as rtc])
            #?(:cljs [is.simm.uis.web.desktop.runtime :refer [runtime]])
            #?(:cljs [is.simm.uis.web.desktop.chat-remote :as chat-remote])
            #?(:cljs [is.simm.uis.web.desktop.workflows-remote :as workflows-remote])
            #?(:cljs [is.simm.uis.web.desktop.views.mermaid :as mmd])
            #?(:cljs [is.simm.runtimes.web :as web]))
  #?(:cljs (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]]
                            [org.replikativ.spindel.dom.foreach :refer [ifor-each]])))

;; =============================================================================
;; Loading
;; =============================================================================

#?(:cljs
   (defn controller
     "Schedule I/O belongs to the rendering world and authenticated session.
      Request tokens fence late replies; activation data and operation ledgers
      share a signal, but refresh never replaces a pending mutation ledger."
     []
     (let [ctx (or rtc/*execution-context* runtime)
           session (web/current-session)
           session-id (:id session)
           current? #(and session-id (web/current-session? session))
           state #(binding [rtc/*execution-context* ctx] @sig/schedules-data)
           update! (fn [f & args]
                     (binding [rtc/*execution-context* ctx]
                       (when (current?) (apply swap! sig/schedules-data f args))))
           ensure! #(when (current?)
                      (when-not (= session-id (:session-id (state)))
                        (update! (constantly {:session-id session-id}))))
           matches? (fn [slot token]
                      (and (current?) (= session-id (:session-id (state)))
                           (= token (get-in (state) [:requests slot]))))]
       (letfn [(load! []
                 (ensure!)
                 (when (and (current?) (not (:loading? (state))) (not (:updating (state))))
                   (let [token (str (random-uuid))
                         finish! (fn [f]
                                   (when (matches? :load token)
                                     (update! #(-> (f %) (assoc :loading? false)
                                                   (update :requests dissoc :load)))))
                         fail! (fn [_] (finish! #(assoc % :error "Could not load schedules. Check access and refresh.")))]
                     (update! #(-> % (assoc :loading? true) (dissoc :error)
                                   (assoc-in [:requests :load] token)))
                     (try
                       ;; A render-triggered load is a root I/O observation,
                       ;; not a child of the render Spin it will invalidate.
                       (binding [rtc/*execution-context* ctx rtc/*spin-id* nil]
                         (let [s (chat-remote/load-schedules! web/server-id)]
                           (s (fn [reply]
                                (try
                                  (finish! #(merge % (select-keys reply [:schedules :workflow-schedules :diagrams :shapes])
                                                   {:loaded-at (js/Date.now)}))
                                  (catch :default error (fail! error))))
                              fail!)))
                       (catch :default error (fail! error))))))
               (toggle! [row]
                 (ensure!)
                 (when (and (current?) (not (:updating (state)))
                            (or (:enabled? row) (not (:resume-blocked row))))
                   (let [token (str (random-uuid))
                         enabled? (not (:enabled? row))
                         finish! (fn [f]
                                   (when (matches? :toggle token)
                                     (update! #(-> (f %) (dissoc :updating)
                                                   (update :requests dissoc :toggle)))
                                     true))
                         fail! (fn [_]
                                 (finish! #(assoc % :error "Could not change this schedule. Refresh before trying again.")))]
                     ;; A refresh begun before this mutation cannot restore its
                     ;; stale enabled state. Reload only after mutation settles.
                     (update! #(-> % (assoc :updating (:id row) :loading? false)
                                   (dissoc :error) (update :requests dissoc :load)
                                   (assoc-in [:requests :toggle] token)))
                     (try
                       (binding [rtc/*execution-context* ctx rtc/*spin-id* nil]
                         (let [s (workflows-remote/set-workflow-schedule-enabled!
                                  web/server-id (:room-id row) {:id (:id row) :enabled? enabled?})]
                           (s (fn [reply]
                                (try
                                  (when (finish!
                                         #(update % :workflow-schedules
                                                  (fn [rows] (mapv (fn [r]
                                                                     (if (= (:id row) (:id r))
                                                                       (assoc r :enabled? (:enabled? reply)) r)) rows))))
                                    (load!))
                                  (catch :default error (fail! error))))
                              fail!)))
                       (catch :default error (fail! error))))))]
         {:session-id session-id :refresh load! :toggle toggle!
          :open-run (fn [row job event]
                      (binding [rtc/*execution-context* ctx]
                        (when (current?)
                          (let [column (some-> event .-currentTarget (.closest ".column[data-col-id]")
                                               (.getAttribute "data-col-id"))]
                            (sig/open-tab! :run-inspector
                                           {:room-id (:room-id row) :room-name (:room-name row) :run-id job}
                                           (cond-> {:title "Scheduled Run" :new-tab? true}
                                             column (assoc :col-id column)))))))}))))

#?(:cljs
   (defn load-schedules! [] ((:refresh (controller)))))

(declare cadence-str)

#?(:cljs
   (defn- workflow-row [row]
     (el/div {:class "schedule-row" :key (:id row)}
             (el/div {:class "schedule-main"}
                     (el/h3 {} (str (:room-name row) " · " (:name row)))
                     (el/p {} (str (if (:enabled? row) "Active" "Paused") " · " (name (:kind row))
                                   " · " (cadence-str (:cadence row)) " · " (get-in row [:cadence :tz])))
                     (el/p {:title "Immutable source, execution settings and firing policy together identify this deployment."}
                           (str "Deployment revision: " (:revision row)))
                     (el/p {} (str "Workflow bundle: " (:bundle row)))
                     (el/p {} (str "Next due: " (.toLocaleString (js/Date. (:next-fire row)))))
                     (when-let [reason (:reason row)] (el/p {:role "status"} (str "Held: " (name reason))))
                     (when (= :legacy-recreate (:resume-blocked row))
                       (el/p {:role "status"}
                             "This older deployment is held. Recreate it from the workflow editor to run it; existing Runs are unchanged."))
                     (el/button {:disabled (boolean (or (:updating? row)
                                                        (and (not (:enabled? row)) (:resume-blocked row))))
                                 :on-click (fn [_] ((:on-toggle row) row))}
                                (if (:this-updating? row) "Updating…" (if (:enabled? row) "Pause" "Resume")))
                     (for [o (:occurrences row)]
                       (el/div {:key (:id o)}
                               (el/span {} (str (.toLocaleString (js/Date. (:intended-at o))) " · "
                                                (name (:state o))
                                                (when (:status o) (str " · " (name (:status o))))
                                                (when (pos? (:coalesced o)) (str " · coalesced " (:coalesced o)))))
                               (when-let [job (:job o)]
                                 (el/button {:on-click (fn [event] ((:on-open-run row) row job event))}
                                            "Open Run and results"))))))))

;; =============================================================================
;; Formatting
;; =============================================================================

(defn- fire-time-str [epoch-ms]
  #?(:cljs (when epoch-ms
             (let [d (js/Date. epoch-ms)
                   pad #(if (< % 10) (str "0" %) (str %))]
               (str (pad (.getHours d)) ":" (pad (.getMinutes d)))))
     :clj nil))

(defn- day-key
  "Group key: days-from-today (0 today, 1 tomorrow, ...), nil when no fire."
  [epoch-ms]
  #?(:cljs (when epoch-ms
             (let [now (js/Date.)
                   today (.setHours (js/Date.) 0 0 0 0)
                   that (.setHours (js/Date. epoch-ms) 0 0 0 0)]
               (js/Math.round (/ (- that today) 86400000))))
     :clj nil))

(defn- day-label [days epoch-ms]
  #?(:cljs (cond
             (nil? days) "Unscheduled"
             (zero? days) "Today"
             (= 1 days) "Tomorrow"
             :else (.toLocaleDateString (js/Date. epoch-ms) js/undefined
                                        #js {:weekday "long" :month "short" :day "numeric"}))
     :clj (str days)))

(defn cadence-str
  "Format both normalized workflow specs and legacy schedule rows exactly."
  [{:keys [kind every at interval-ms every-ms on on-day once]}]
  (let [ms (or interval-ms every-ms)]
    (cond
      ms (if (zero? (mod ms 60000))
           (str "every " (quot ms 60000) "m")
           (str "every " ms "ms"))
      (or every (= :recurring kind))
      (str "every " (some-> every name)
           (when at (str " @ " at))
           (when on (str " on " (name on)))
           (when on-day (str " (day " on-day ")")))
      (or once (= :once kind)) "once"
      :else (some-> kind name))))

;; =============================================================================
;; Rendering
;; =============================================================================

(defn- schedule-row [s]
  (el/div {:key (:id s) :class "schedule-row"}
          (el/span {:class "schedule-time"} (or (fire-time-str (:next-fire s)) "—"))
          (el/div {:class "schedule-main"}
                  (el/div {:class "schedule-task"} (:task s))
                  (el/div {:class "schedule-meta"}
                          (el/span {:class "schedule-room"} (:room-name s))
                          (when (:agent-name s)
                            (el/span {:class "schedule-agent"} (:agent-name s)))
                          (el/span {:class "schedule-cadence"} (cadence-str s))))))

#?(:cljs
   (defn render-schedules
     "Agenda tab content. Spin tracking sig/schedules-data; triggers a
      load when the signal is still empty (idempotent via loading?)."
     []
     (let [actions (controller)]
       (spin
        (let [raw-data (iv/get-new (track sig/schedules-data))
              data (when (and (:session-id actions)
                              (= (:session-id actions) (:id (web/current-session)))
                              (= (:session-id actions) (:session-id raw-data))) raw-data)
              _ (when (nil? data) ((:refresh actions)))
              schedules (vec (sort-by #(or (:next-fire %) js/Number.MAX_SAFE_INTEGER)
                                      (:schedules data)))
              groups (->> schedules
                          (group-by #(day-key (:next-fire %)))
                          (sort-by (fn [[k _]] (if (nil? k) 9999999 k)))
                          vec)
              workflow-rows (mapv #(assoc % :updating? (boolean (:updating data))
                                          :this-updating? (= (:id %) (:updating data))
                                          :on-toggle (:toggle actions) :on-open-run (:open-run actions))
                                  (:workflow-schedules data))]
          (el/div {:class "content-schedules"}
                  (el/div {:class "schedules-header"}
                          (el/h2 {} "Schedules")
                          (el/button {:class "schedules-refresh"
                                      :disabled (boolean (or (:loading? data) (:updating data)))
                                      :title "Refresh"
                                      :on-click (fn [_] ((:refresh actions)))}
                                     (vc/icon "refresh-cw")))
                  (when-let [error (:error data)] (el/p {:role "alert"} error))
                  (el/h3 {} "Pinned workflow deployments")
                  (el/p {} "Create an activation from a saved workflow. These use the shared room allowance and never merge automatically.")
                  (if (seq workflow-rows)
                    (ifor-each :id workflow-rows workflow-row)
                    (el/p {} "No pinned workflow schedules yet."))
                  (el/h3 {} "Legacy agent tasks")
           ;; Per-room workflow topology diagrams (katzen → mermaid). The map
           ;; view above the agenda: one node per background workflow → the KB
           ;; it feeds — so you see WHAT is running at a glance, not per-line.
                  (when-let [diagrams (seq (:diagrams data))]
                    (let [shapes  (:shapes data)
                          by-room (group-by :room-name (:schedules data))]
                      (el/div {:class "workflow-diagrams"}
                              (el/div {:class "workflow-diagrams-title"} "Workflows")
                              (ifor-each first (vec diagrams)
                                         (fn [[room-name code]]
                                           (el/div {:key (str "wf-" room-name) :class "workflow-room"}
                                                   (el/div {:class "workflow-room-label"} room-name)
                                                   (mmd/diagram code)
                       ;; agent-authored per-workflow shapes (design B): the
                       ;; actual steps of each workflow that has declared one
                                                   (ifor-each :id
                                                              (vec (filter #(contains? shapes (:id %)) (get by-room room-name)))
                                                              (fn [s]
                                                                (el/div {:key (str "shape-" (:id s)) :class "workflow-shape"}
                                                                        (el/div {:class "workflow-shape-label"}
                                                                                (or (:description s) (:task s)))
                                                                        (mmd/diagram (get shapes (:id s))))))))))))
                  (cond
                    (nil? data)
                    (el/p {:class "schedules-empty"} "Loading schedules…")

                    (empty? schedules)
                    (el/div {:class "schedules-empty"}
                            (el/p {} "No legacy agent tasks.")
                            (el/p {:class "schedules-hint"}
                                  "Ask an agent to schedule a recurring task — e.g. “run the research flow every morning at 9”."))

                    :else
                    (el/div {:class "schedules-groups"}
                            (ifor-each first groups
                                       (fn [[days ss]]
                                         (el/div {:key (str "day-" days) :class "schedule-group"}
                                                 (el/div {:class "schedule-day-label"}
                                                         (day-label days (:next-fire (first ss))))
                                                 (el/div {:class "schedule-group-rows"}
                                                         (ifor-each :id (vec ss) schedule-row)))))))))))))
