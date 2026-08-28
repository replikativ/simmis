(ns is.simm.uis.web.desktop.run-sync
  "Client bridge from room-private Dvergr Run topics into Spindel UI state."
  (:require [clojure.core.async :refer [go <!] :include-macros true]
            [kabel.pubsub :as pubsub]
            [kabel.pubsub.protocol :as proto]
            [org.replikativ.spindel.engine.core :as rtc]
            [is.simm.uis.web.desktop.chat-remote :as remote]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.runtimes.web :as web]))

(defn run-topic [room-id]
  (keyword "runs" (str room-id)))

(defn- upsert-run [runs r]
  (->> (conj (vec (remove #(= (:id %) (:id r)) runs)) r)
       (sort-by (juxt #(or (:started-at %) 0) :id) #(compare %2 %1))
       (take 24)
       vec))

(defn apply-event
  "Pure lifecycle reducer, kept separate from signal mutation for focused tests."
  [state room-id {:keys [type run]}]
  (let [room-id (str room-id)]
    (case type
      :run/started
      (-> state
          (update-in [room-id :active] (fnil upsert-run []) run)
          (update-in [room-id :recent] (fnil upsert-run []) run))

      :run/cancel-requested
      (-> state
          (update-in [room-id :active] (fnil upsert-run []) run)
          (update-in [room-id :recent] (fnil upsert-run []) run))

      :run/finished
      (-> state
          (update-in [room-id :active]
                     (fn [runs] (vec (remove #(= (:id %) (:id run)) runs))))
          (update-in [room-id :recent] (fnil upsert-run []) run))

      state)))

(defn- apply-event! [room-id event]
  (binding [rtc/*execution-context* runtime]
    (swap! sig/room-runs apply-event room-id event)))

(defn- install-snapshot! [room-id snapshot]
  (binding [rtc/*execution-context* runtime]
    (swap! sig/room-runs assoc (str room-id)
           {:active (vec (:active snapshot))
            :recent (vec (:recent snapshot))})))

;; This atom records network subscriptions only. The mutable application value
;; is sig/room-runs above, hence follows Spindel execution-context forks.
(defonce ^:private subscriptions (atom #{}))
(defonce ^:private loading (atom #{}))

(defn- load-snapshot!
  [room-id on-loaded]
  (let [room-id (str room-id)]
    (when-not (contains? @loading room-id)
      (swap! loading conj room-id)
      (binding [rtc/*execution-context* runtime]
        (let [s (remote/load-room-runs! web/server-id room-id)]
          (s (fn [snapshot]
               (swap! loading disj room-id)
               (install-snapshot! room-id snapshot)
               (when on-loaded (on-loaded)))
             (fn [error]
               (swap! loading disj room-id)
               (swap! subscriptions disj room-id)
               (js/console.warn "[run-sync] snapshot failed" room-id error))))))))

(defn refresh-room!
  "Fetch an authoritative live + bounded durable snapshot."
  [room-id]
  (load-snapshot! room-id nil))

(defn ensure-room!
  "Subscribe once to one authorized room and seed its Run projection."
  [room-id]
  (let [room-id (str room-id)]
    (when (and room-id
               (not= room-id "personal-ai-placeholder")
               (not (contains? @subscriptions room-id)))
      (swap! subscriptions conj room-id)
      ;; The initial RPC lazily registers topics for rooms created after boot.
      ;; Subscribe only after that succeeds, then refresh once more to close the
      ;; registration-to-subscription gap.
      (load-snapshot!
       room-id
       (fn []
         (let [topic (run-topic room-id)
               strategy (proto/pub-sub-only-strategy #(apply-event! room-id %))]
           (go
             (let [result (<! (pubsub/subscribe! @web/client #{topic}
                                                 {:strategies {topic strategy}}))]
               (if (:error result)
                 (do (swap! subscriptions disj room-id)
                     (js/console.warn "[run-sync] subscribe failed" room-id
                                      (pr-str result)))
                 (refresh-room! room-id))))))))))

(defn cancel!
  "Request cooperative cancellation of one active Run in its room."
  [room-id run-id]
  (binding [rtc/*execution-context* runtime]
    (let [s (remote/cancel-room-run! web/server-id (str room-id) (str run-id))]
      (s (fn [result]
           (when (= :not-active (:status result))
             (refresh-room! room-id)))
         (fn [error]
           (js/console.warn "[run-sync] cancellation failed" run-id error))))))
