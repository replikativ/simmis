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
            #?(:cljs [is.simm.uis.web.desktop.observation-io :as observation-io])
            #?(:cljs [is.simm.uis.web.desktop.connection-epoch :as connection])
            #?(:cljs [is.simm.uis.web.desktop.memory-sync :as sync])
            #?(:cljs [is.simm.uis.web.desktop.refs :as refs])
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
                           (= token (get-in (state) [:requests slot]))))
           observe! (observation-io/controller ctx session state update!)
           generation #(binding [rtc/*execution-context* ctx]
                         (let [stream @sync/state]
                           [(:connection stream) (:status stream) (:control-dirty stream)
                            @sig/user-rooms (:retry (state)) (:updating (state))
                            (:stream-registered (state))]))]
       (letfn [(observe-all! []
                 (ensure!)
                 (let [socket (connection/snapshot)
                       owner [socket (:fork-id (rtc/home-context ctx))]]
                   (when (not= owner (:observation-owner (state)))
                     (update! #(-> % (assoc :observation-owner owner :requests {})
                                   (dissoc :updating :loading?))))
                   (observe! :stream #(vector (:retry (state)))
                             #(workflows-remote/prepare-workflow-metadata-stream! web/server-id)
                             (fn [s _] (-> s (dissoc :stream-error)
                                           (update :stream-registered (fnil inc 0))))
                             (fn [s error] (assoc s :stream-error error))
                             (fn [_] (sync/ensure! socket)))
                   (when-not (:updating (state))
                     (observe! :index generation
                               #(chat-remote/load-schedules! web/server-id)
                               (fn [s reply] (-> (merge s (select-keys reply [:schedules :workflow-schedules :diagrams :shapes]))
                                                 (assoc :loaded-at (js/Date.now)) (dissoc :error)))
                               (fn [s error] (-> s (dissoc :schedules :workflow-schedules :diagrams :shapes)
                                                 (assoc :error error)))))))
               (load! []
                 (ensure!)
                 (update! update :retry (fnil inc 0)))
               (toggle! [row]
                 (ensure!)
                 (when (and (current?) (connection/snapshot) (not (:updating (state)))
                            (or (:enabled? row) (not (:resume-blocked row))))
                   (let [token (str (random-uuid)) socket (connection/snapshot)
                         enabled? (not (:enabled? row))
                         finish! (fn [f]
                                   (when (and (connection/current? socket) (matches? :toggle token))
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
         {:session-id session-id :observe observe-all!
          :refresh #(do (load!) (binding [rtc/*execution-context* ctx] (sync/retry! (connection/snapshot))))
          :toggle toggle!
          :open-proposal (fn [proposal]
                           (binding [rtc/*execution-context* ctx]
                             (when (current?) (refs/open! {:kind :proposal :id proposal}))))
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
   (defn workflow-row [row]
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
                     (when (= :reconciliation-required (:resume-blocked row))
                       (el/p {:role "status"}
                             "This deployment has an unresolved admission. Resume is unavailable until it is reconciled; adding allowance does not resolve it."))
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
                                            "Open Run and results"))
                               (case (get-in o [:filing :status])
                                 :proposed (el/button {:on-click (fn [_] ((:on-open-proposal row) (get-in o [:filing :proposal])))}
                                                      "Review wiki proposal")
                                 :no-op (el/span {:class "schedule-outcome"} "No wiki changes")
                                 nil)))))))

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
     (spin
      (let [data-i (track sig/schedules-data)
            socket-i (track sig/memory-connection)
            stream-i (track sync/state)
            roster-i (track sig/user-rooms)
            raw-data (iv/get-new data-i)
            _socket (iv/get-new socket-i)
            _stream (iv/get-new stream-i)
            _roster (iv/get-new roster-i)
            actions (controller)
            data (when (and (:session-id actions)
                            (= (:session-id actions) (:id (web/current-session)))
                            (= (:session-id actions) (:session-id raw-data))) raw-data)
            _ ((:observe actions))
            schedules (vec (sort-by #(or (:next-fire %) js/Number.MAX_SAFE_INTEGER)
                                    (:schedules data)))
            groups (->> schedules
                        (group-by #(day-key (:next-fire %)))
                        (sort-by (fn [[k _]] (if (nil? k) 9999999 k)))
                        vec)
            workflow-rows (mapv #(assoc % :updating? (boolean (:updating data))
                                        :this-updating? (= (:id %) (:updating data))
                                        :on-toggle (:toggle actions) :on-open-run (:open-run actions)
                                        :on-open-proposal (:open-proposal actions))
                                (:workflow-schedules data))]
        (el/div {:class "content-schedules"}
                (el/div {:class "schedules-header"}
                        (el/h2 {} "Schedules")
                        (when (or (:error data) (:stream-error data) (= :error (:status _stream)))
                          (el/button {:class "schedules-refresh"
                                      :disabled (boolean (or (:loading? data) (:updating data)))
                                      :title "Retry updates"
                                      :on-click (fn [_] ((:refresh actions)))}
                                     "Retry updates")))
                (when-let [error (:error data)] (el/p {:role "alert"} error))
                (when-let [error (or (:stream-error data) (sync/problem _stream))]
                  (el/p {:role "alert"} error))
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
                                                       (ifor-each :id (vec ss) schedule-row))))))))))))
