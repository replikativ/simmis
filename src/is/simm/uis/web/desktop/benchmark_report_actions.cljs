(ns is.simm.uis.web.desktop.benchmark-report-actions
  "One explicit report request per context-owned tab/Scorecard. Pending target
   and digest survive uncertain callbacks; opening a result never publishes."
  (:require [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.benchmark-report-remote :as remote]
            [is.simm.uis.web.desktop.benchmark-report-state :as report-state]
            [is.simm.uis.web.desktop.views.memory-navigation :as navigation]
            [is.simm.runtimes.web :as web]))

(defn owner-key [session data]
  [(:id session) (str (:room-id data)) (:tab-id data) (get data :tab-type :leaderboard)])

(defn tab-current? [columns {:keys [room-id tab-id] :as data}]
  (let [matches (for [column columns tab (:tabs column) :when (= tab-id (:id tab))]
                  {:column column :tab tab})
        {:keys [tab]} (when (= 1 (count matches)) (first matches))]
    (and (some? tab-id) (contains? #{:leaderboard :chat :chat-thread} (get data :tab-type :leaderboard))
         (= (get data :tab-type :leaderboard) (:type tab))
         (= (str room-id) (str (get-in tab [:data :room-id]))))))

(defn controller [data]
  (let [ctx (or ec/*execution-context* runtime) session (web/current-session)
        room (str (:room-id data)) key (owner-key session data)
        state #(binding [ec/*execution-context* ctx] (get @sig/benchmark-report-publications key))
        update! (fn [f & args]
                  (binding [ec/*execution-context* ctx]
                    (when (web/current-session? session)
                      (apply swap! sig/benchmark-report-publications update key f args))))
        owned? #(binding [ec/*execution-context* ctx]
                  (and (web/current-session? session) (tab-current? @sig/layout-columns data)))]
    (when-let [epoch (when (owned?) (connection/snapshot))]
      (when (not= (state) (report-state/retire-stale (state) epoch))
        (update! report-state/retire-stale epoch)))
    {:publish
     (fn [scorecard]
       (binding [ec/*execution-context* ctx ec/*spin-id* nil]
         (when (and (owned?) (connection/snapshot) (not (get-in (state) [scorecard :loading?])))
           (let [epoch (connection/snapshot) token (random-uuid)
                 current? #(binding [ec/*execution-context* ctx]
                             (and (owned?) (connection/current? epoch)
                                  (= token (get-in (state) [scorecard :token]))))
                 fail! (fn [_]
                         (when (current?)
                           (update! update scorecard assoc :loading? false
                                    :error "Report publication did not finish. Retry the same request in its original Memory.")))
                 publish! (fn [intent]
                            (binding [ec/*execution-context* ctx ec/*spin-id* nil]
                              (when (current?)
                            ;; Freeze the original digest/target BEFORE dispatch.
                                (update! update scorecard assoc :pending intent)
                                (try
                                  ((remote/publish-report! web/server-id room (dissoc intent :source))
                                   (fn [result]
                                     (when (current?)
                                       (if (report-state/receipt-current? room intent result)
                                         (update! update scorecard #(-> % (assoc :loading? false :result result)
                                                                        (dissoc :pending :error)))
                                         (fail! nil))))
                                   fail!)
                                  (catch :default error (fail! error))))))]
             (update! update scorecard assoc :loading? true :token token :epoch epoch :error nil)
             (if-let [intent (get-in (state) [scorecard :pending])]
               (publish! intent)
               (try
                 ((remote/prepare-report! web/server-id room {:scorecard (parse-uuid scorecard)})
                  (fn [prepared]
                    (if-let [intent (report-state/intent room scorecard prepared)]
                      (publish! intent)
                      (fail! nil)))
                  fail!)
                 (catch :default error (fail! error))))))))
     :open
     (fn [scorecard]
       (when (owned?)
         (when-let [path (get-in (state) [scorecard :result :report :path])]
           (binding [ec/*execution-context* ctx ec/*spin-id* nil]
             (let [column (first (for [column @sig/layout-columns tab (:tabs column)
                                       :when (= (:tab-id data) (:id tab))] (:id column)))]
               (navigation/open-file! room (:room-name data) path column))))))}))
