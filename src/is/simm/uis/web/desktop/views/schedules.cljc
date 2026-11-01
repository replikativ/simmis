(ns is.simm.uis.web.desktop.views.schedules
  "Workflows overview over existing saved activations, agent code and room links."
  (:require [org.replikativ.spindel.dom.elements :as el]
            #?(:cljs [org.replikativ.spindel.incremental.interval :as iv])
            #?(:cljs [org.replikativ.spindel.effects.track :refer [track]])
            #?(:cljs [is.simm.uis.web.desktop.signals :as sig])
            [is.simm.uis.web.desktop.disclosure :as disclosure]
            [is.simm.uis.web.desktop.workflow-editor :as workflow-editor]
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
  #?(:cljs (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]])))

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
                           [(connection/snapshot) (:fork-id (rtc/home-context ctx))
                            (:connection stream) (:status stream) (:control-dirty stream)
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
                   (let [current-generation (generation)]
                     (when (not= current-generation (:index-generation (state)))
                       (update! assoc :index-generation current-generation :index-status :checking)))
                   (when-not (:updating (state))
                     (observe! :index generation
                               #(chat-remote/load-schedules! web/server-id)
                               (fn [s reply] (-> (merge s (select-keys reply [:schedules :workflow-schedules :diagrams :shapes]))
                                                 (assoc :loaded-at (js/Date.now) :index-status :ready) (dissoc :error)))
                               (fn [s error] (-> s (dissoc :schedules :workflow-schedules :diagrams :shapes)
                                                 (assoc :error error :index-status :unavailable)))))))
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
                                   (dissoc :error :notice) (update :requests dissoc :load)
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
                       (catch :default error (fail! error))))))
               (recover-update! [row]
                 (ensure!)
                 (when (and (current?) (connection/snapshot) (:failed-update row) (not (:updating (state))))
                   (let [inspected (:failed-update row)
                         previous (get-in (state) [:recovery-commands (:id row)])
                         request (if (= inspected (select-keys previous (keys inspected))) previous
                                     (assoc inspected :request-id (str (random-uuid))
                                            :reason "Make this evidence eligible for a fresh wiki update"))
                         token (str (random-uuid)) socket (connection/snapshot)
                         finish! (fn [f]
                                   (when (and (connection/current? socket) (matches? :recover token))
                                     (update! #(-> (f %) (dissoc :updating) (update :requests dissoc :recover)))
                                     true))
                         fail! (fn [_]
                                 ;; Retain the exact command for an explicit
                                 ;; retry if its acknowledgement was lost.
                                 (finish! #(assoc % :error "Could not confirm recovery. Check updates before trying again.")))]
                     (update! #(-> % (assoc :updating (:id row) :loading? false)
                                   (dissoc :error :notice) (update :requests dissoc :load)
                                   (assoc-in [:requests :recover] token)
                                   (assoc-in [:recovery-commands (:id row)] request)))
                     (try
                       (binding [rtc/*execution-context* ctx rtc/*spin-id* nil]
                         (let [operation (workflows-remote/recover-failed-workflow-update! web/server-id (:room-id row) request)]
                           (operation
                            (fn [reply]
                              (try
                                (when-not (and (= :recovered (:status reply)) (= :fresh-update (:mode reply))
                                               (= (:batch request) (:batch reply)) (= (:request request) (:request reply))
                                               (false? (:dispatched? reply)) (false? (:schedule-resumed? reply))
                                               (string? (:operation reply)) (parse-uuid (:operation reply)))
                                  (throw (js/Error. "Invalid recovery receipt")))
                                (when (finish! #(-> % (assoc :notice "Evidence recovered. The workflow is still paused; Resume is a separate action.")
                                                    (update :recovery-commands dissoc (:id row))))
                                  (load!))
                                (catch :default error (fail! error))))
                            fail!)))
                       (catch :default error (fail! error))))))]
         {:session-id session-id :observe observe-all!
          :index-current? #(= (:index-generation %) (generation))
          :set-expanded (fn [kind id open?]
                          (when (and (current?) (contains? #{:history :map :code} kind))
                            (update! update-in [:disclosures kind]
                                     (fn [ids] (if open? (conj (set ids) id) (disj (set ids) id))))))
          :refresh #(do (load!) (binding [rtc/*execution-context* ctx] (sync/retry! (connection/snapshot))))
          :toggle toggle! :recover-update recover-update!
          :open-room (fn [room event]
                       (binding [rtc/*execution-context* ctx]
                         (when (current?)
                           (let [column (some-> event .-currentTarget (.closest ".column[data-col-id]")
                                                (.getAttribute "data-col-id"))]
                             (sig/open-tab! :workflows
                                            {:room-id (str (:room/id room)) :room-name (:room/name room)}
                                            (cond-> {:title (str (:room/name room) " · Workflows") :new-tab? true}
                                              column (assoc :col-id column)))))))
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
   (defn- occurrence-view [row occurrence]
     (el/div {:class "workflow-occurrence" :key (:id occurrence)}
             (el/span {} (str (.toLocaleString (js/Date. (:intended-at occurrence))) " · "
                              (name (or (:status occurrence) (:state occurrence) :unknown))))
             (when-let [label (workflow-editor/batch-label (:batch occurrence))]
               (el/span {:class "schedule-outcome"} label))
             (when-let [job (:job occurrence)]
               (el/button {:on-click (fn [event] ((:on-open-run row) row job event))} "Results"))
             (case (get-in occurrence [:filing :status])
               :proposed (el/button {:on-click (fn [_] ((:on-open-proposal row) (get-in occurrence [:filing :proposal])))}
                                    "Review proposal")
               :no-op (el/span {:class "schedule-outcome"} "No wiki changes")
               nil))))

#?(:cljs
   (defn workflow-row [row]
     (let [occurrences (sort-by :intended-at > (:occurrences row))]
       (el/div {:class "schedule-row workflow-card" :key (:id row)}
               (el/div {:class "schedule-main"}
                       (el/h3 {} (:name row))
                       (el/p {:class "workflow-note"}
                             (str (:room-name row) " · " (if (= :benchmark (:kind row)) "Benchmark" "Saved workflow")
                                  " · " (if (:enabled? row) "Scheduled" "Paused")))
                       (el/p {} (str (cadence-str (:cadence row)) " · " (get-in row [:cadence :tz])
                                     (when (:enabled? row) (str " · Next: " (.toLocaleString (js/Date. (:next-fire row)))))))
                       (when-let [reason (:reason row)] (el/p {:role "status"} (str "Held: " (name reason))))
                       (when (:resume-blocked row)
                         (el/p {:role "status"} (case (:resume-blocked row)
                                                  :legacy-recreate "Open the room workflow to recreate this older activation."
                                                  :batch-recovery-required "The failed evidence batch needs recovery before this workflow can resume; adding allowance does not resolve it."
                                                  "Admission needs reconciliation before this workflow can resume; adding allowance does not resolve it.")))
                       (when (:failed-update row)
                         (el/button {:disabled (boolean (:updating? row))
                                     :on-click (fn [_] ((:on-recover-update row) row))}
                                    (if (:this-updating? row) "Recovering…" "Recover update")))
                       (when (and (:recovered-update? row) (not (:enabled? row)))
                         (el/p {:role "status"} "The failed update was recovered. Resume schedules future work."))
                       (el/button {:disabled (boolean (or (:updating? row)
                                                          (and (not (:enabled? row)) (:resume-blocked row))))
                                   :on-click (fn [_] ((:on-toggle row) row))}
                                  (if (:this-updating? row) "Updating…" (if (:enabled? row) "Pause" "Resume")))
                       (when (:on-open-room row)
                         (el/button {:on-click (fn [event] ((:on-open-room row)
                                                            {:room/id (:room-id row) :room/name (:room-name row)} event))}
                                    "Open room workflows"))
                       (when-let [latest (first occurrences)] (occurrence-view row latest))
                       (el/details {:class "workflow-disclosure" :open (boolean (:history-open? row))
                                    :on-toggle #(disclosure/remember-open! % (fn [open?] ((:on-expand row) :history (:id row) open?)))}
                                   (el/summary {} "History and execution details")
                                   (el/p {} (str "Deployment revision: " (:revision row)))
                                   (el/p {} (str "Workflow bundle: " (:bundle row)))
                                   (for [occurrence (rest occurrences)] (occurrence-view row occurrence))))))))

;; =============================================================================
;; Formatting
;; =============================================================================

(defn- fire-time-str [epoch-ms]
  #?(:cljs (when epoch-ms
             (let [d (js/Date. epoch-ms)
                   pad #(if (< % 10) (str "0" %) (str %))]
               (str (pad (.getHours d)) ":" (pad (.getMinutes d)))))
     :clj nil))

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
                          (el/span {:class "schedule-cadence"} (cadence-str s)))
                  (when-let [shape (:shape s)]
                    #?(:cljs (mmd/diagram shape) :clj (el/pre {} shape)))
                  (when (:code s)
                    (el/details {:class "workflow-disclosure" :open (boolean (:code-open? s))
                                 :on-toggle #(disclosure/remember-open! % (fn [open?] ((:on-expand s) :code (:id s) open?)))}
                                (el/summary {} "Invoked code")
                                (el/pre {} (if (string? (:code s)) (:code s) (pr-str (:code s)))))))))

#?(:cljs
   (defn overview [data rooms actions stream-problem]
     (let [index-ready? (= :ready (:index-status data))
           rows (mapv #(assoc % :updating? (boolean (:updating data))
                              :this-updating? (= (:id %) (:updating data))
                              :on-toggle (:toggle actions) :on-recover-update (:recover-update actions) :on-open-run (:open-run actions)
                              :on-open-room (:open-room actions) :on-open-proposal (:open-proposal actions)
                              :history-open? (contains? (get-in data [:disclosures :history]) (:id %))
                              :on-expand (:set-expanded actions))
                      (when index-ready? (:workflow-schedules data)))
           schedules (sort-by #(or (:next-fire %) js/Number.MAX_SAFE_INTEGER) (when index-ready? (:schedules data)))]
       (el/div {:class "content-schedules workflow-overview"}
               (el/div {:class "schedules-header"}
                       (el/h2 {} "Workflows")
                       (when (or (:error data) (:stream-error data) stream-problem)
                         (el/button {:disabled (boolean (:updating data))
                                     :on-click (fn [_] ((:refresh actions)))} "Retry updates")))
               (when-let [notice (:notice data)] (el/p {:role "status"} notice))
               (el/p {:class "workflow-note"} "Reusable code agents can invoke. Open a room’s saved workflows, or review scheduled work and its results.")
               (when-let [error (:error data)] (el/p {:role "alert"} error))
               (when-let [error (or (:stream-error data) stream-problem)] (el/p {:role "alert"} error))
               (el/h3 {} "Saved workflows")
               (el/p {:class "workflow-note"} "Workflows can run on demand. Open a room to inspect its saved code or discuss changes with its agents.")
               (el/div {:class "workflow-room-links"}
                       (for [room (sort-by :room/name rooms)]
                         (el/button {:key (str (:room/id room))
                                     :on-click (fn [event] ((:open-room actions) room event))} (:room/name room))))
               (when (and data (empty? rooms)) (el/p {} "No rooms available."))
               (el/h3 {} "Scheduled work")
               (when (and (not index-ready?) (not (:error data))) (el/p {:role "status"} "Checking scheduled work…"))
               (when (and index-ready? (empty? rows) (empty? schedules))
                 (el/p {} "No scheduled work. Ask an agent to prepare a recurring workflow in its room."))
               (for [row rows] (workflow-row row))
               (for [[room-name code] (sort-by key (when index-ready? (:diagrams data)))]
                 (el/details {:key room-name :class "workflow-disclosure workflow-room"
                              :open (contains? (get-in data [:disclosures :map]) room-name)
                              :on-toggle #(disclosure/remember-open! % (fn [open?] ((:set-expanded actions) :map room-name open?)))}
                             (el/summary {} (str room-name " · Workflow map"))
                             (mmd/diagram code)))
               (for [schedule schedules]
                 (schedule-row (assoc schedule :shape (get-in data [:shapes (:id schedule)])
                                      :code-open? (contains? (get-in data [:disclosures :code]) (:id schedule))
                                      :on-expand (:set-expanded actions))))))))

#?(:cljs
   (defn render-schedules
     "Existing reactive tab identity; Workflows projects saved code and triggers."
     []
     (spin
      (let [data-i (track sig/schedules-data)
            socket-i (track sig/memory-connection)
            stream-i (track sync/state)
            roster-i (track sig/user-rooms)
            raw-data (iv/get-new data-i)
            _socket (iv/get-new socket-i)
            stream (iv/get-new stream-i)
            roster (iv/get-new roster-i)
            actions (controller)
            data (when (and (:session-id actions)
                            (= (:session-id actions) (:id (web/current-session)))
                            (= (:session-id actions) (:session-id raw-data))) raw-data)]
        ((:observe actions))
        (overview (cond-> data
                    (and data (not ((:index-current? actions) data))) (assoc :index-status :checking))
                  (when data (:rooms roster)) actions (sync/problem stream))))))
