(ns is.simm.uis.web.desktop.github-sync
  "Context-owned explicit Git review. Unknown writes are observed, never resent."
  (:require [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.memory-actions :as memory]
            [is.simm.uis.web.desktop.github-remote :as remote]
            [is.simm.runtimes.web :as web]))

(def terminal #{:applied :satisfied-observed :closed})

(defn- reply! [reply]
  (when-not (and (map? reply) (#{:not-observed :preparing :ready :unconfirmed :held :applied :satisfied-observed :closed} (:status reply))
                 (or (nil? (:changes reply))
                     (and (vector? (:changes reply)) (<= (count (:changes reply)) 2048)
                          (every? #(and (string? (:path %)) (<= 1 (count (:path %)) 512)
                                        (#{:added :modified :deleted} (:kind %))) (:changes reply))))
                 (or (not= :ready (:status reply))
                     (and (string? (:ready-id reply)) (= (:ready-id reply) (some-> (:ready-id reply) parse-uuid str))
                          (vector? (:changes reply)) (= (:total reply) (count (:changes reply))))))
    (throw (ex-info "Git sync response is unavailable" {})))
  reply)

(defn action-basis [state memory epoch]
  {:client-id (get-in state [:intent :client-id]) :ready-id (get-in state [:reply :ready-id])
   :status (get-in state [:reply :status]) :sync-id (get-in memory [:github-source :sync-id])
   :source (select-keys (:github-source memory) [:repository :ref :import-head])
   :target-stamp (:target-stamp memory) :connection epoch})

(defn controller [data memory-state]
  (let [ctx (or ec/*execution-context* runtime) session (web/current-session)
        room (str (:room-id data)) key [(:id session) room]
        current? #(and (:id session) (web/current-session? session))
        state #(binding [ec/*execution-context* ctx] (get @sig/github-sync-state key))
        update! (fn [f & args] (binding [ec/*execution-context* ctx]
                                 (when (current?) (apply swap! sig/github-sync-state update key f args))))
        live? #(and (current?) (identical? (ec/home-context ctx) (ec/home-context runtime)))
        owner-call? #(or (nil? ec/*execution-context*)
                         (identical? (ec/home-context ec/*execution-context*) (ec/home-context ctx)))
        basis #(binding [ec/*execution-context* ctx] (action-basis (state) (memory-state) (connection/snapshot)))
        valid? #(binding [ec/*execution-context* ctx]
                  (let [m (memory-state)]
                    (and (live?) (connection/snapshot)
                         (some (fn [r] (= room (str (:room/id r)))) (:rooms @sig/user-rooms))
                         (memory/verified? m @sig/memory-connection)
                         (true? (get-in m [:permissions :evaluate?]))
                         (true? (get-in m [:github-source :available?])))))]
    (letfn [(invoke! [operation request]
              (binding [ec/*execution-context* ctx]
                (when (and (valid?) (not (:loading? (state))))
                  (let [token (random-uuid) epoch (connection/snapshot) stamp (:target-stamp (memory-state))
                        owns? #(and (current?) (= token (:token (state))) (not= token (:finished (state))))
                        failed (fn [_]
                                 (binding [ec/*execution-context* ctx]
                                   (when (owns?) (update! assoc :loading? false :finished token :verified-on nil
                                                          :reply {:status :unconfirmed}
                                                          :error "The outcome is unconfirmed. Check this same sync before taking another action."))))
                        done (fn [reply]
                               (binding [ec/*execution-context* ctx]
                                 (when (owns?)
                                   (try
                                     (let [reply (reply! reply)]
                                       (when (or (and (:client-id reply) (:client-id request)
                                                      (not= (:client-id reply) (:client-id request)))
                                                 (and (:sync-id reply) (:sync-id request)
                                                      (not= (:sync-id reply) (:sync-id request))))
                                         (throw (ex-info "Git response belongs to another intent" {})))
                                       (update! #(-> % (assoc :reply reply :loading? false :finished token
                                                              :verified-on (when (and (= epoch (connection/snapshot))
                                                                                      (= stamp (:target-stamp (memory-state))) (valid?))
                                                                             [epoch stamp]))
                                                     (cond-> (terminal (:status reply)) (assoc :open? false))
                                                     (cond-> (and (= :retire operation) (= :closed (:status reply)))
                                                       (assoc :retired-sync-id (:sync-id request)))
                                                     (cond-> (and (terminal (:status reply)) (:sync-id reply))
                                                       (assoc :retired-sync-id (:sync-id reply)))
                                                     (dissoc :error))))
                                     (catch :default error (failed error))))))]
                    (update! assoc :token token :operation operation :loading? true :error nil
                             :request-on epoch :observed-on [epoch stamp (get-in (state) [:intent :client-id])])
                    (try
                      (binding [ec/*spin-id* nil]
                        ((case operation
                           :observe (remote/load-github-memory-sync! web/server-id room request)
                           :retire (remote/retire-github-memory-sync! web/server-id room request)
                           (remote/sync-github-memory! web/server-id room operation request)) done failed))
                      (catch :default error (failed error)))))))
            (check! [] (when-let [id (get-in (state) [:intent :client-id])] (invoke! :observe {:client-id id})))
            (ensure! []
              (when (owner-call?)
                (binding [ec/*execution-context* ctx]
                  (when (and (:loading? (state)) (not= (:request-on (state)) (connection/snapshot)))
                    (update! assoc :loading? false :verified-on nil :reply {:status :unconfirmed}))
                  (when (and (valid?) (nil? (:intent (state))))
                    (when-let [client (get-in (memory-state) [:github-source :sync-client-id])]
                      (update! assoc :intent {:client-id client} :open? true)))
                  (when (and (valid?) (:intent (state)) (not (:loading? (state)))
                             (not= (:observed-on (state)) [(connection/snapshot) (:target-stamp (memory-state))
                                                           (get-in (state) [:intent :client-id])]))
                    (check!)))))
            (prepare! [offer]
              (when (and (owner-call?) (= offer (basis)))
                (binding [ec/*execution-context* ctx]
                  (when (and (valid?) (not (:loading? (state)))
                             (or (not (get-in (memory-state) [:github-source :sync-active?]))
                                 (= (get-in (memory-state) [:github-source :sync-id]) (:retired-sync-id (state))))
                             (or (nil? (:intent (state))) (terminal (get-in (state) [:reply :status]))))
                    (let [request {:client-id (str (random-uuid)) :expected-target-stamp (str (:target-stamp (memory-state)))}]
                      (update! assoc :intent request :open? true :reply nil :verified-on nil)
                      (invoke! :prepare request))))))
            (apply! [offer]
              (when (and (owner-call?) (= offer (basis)))
                (binding [ec/*execution-context* ctx]
                  (when (and (= :ready (get-in (state) [:reply :status]))
                             (= (:verified-on (state)) [(connection/snapshot) (:target-stamp (memory-state))]))
                    (invoke! :apply {:client-id (:client-id offer) :ready-id (:ready-id offer)})))))
            (intent-action! [offer operation]
              (when (and (owner-call?) (= offer (basis)))
                (when-let [client (get-in (state) [:intent :client-id])]
                  (invoke! operation {:client-id client}))))]
      {:owner key :state state :basis basis :ensure ensure! :sync prepare! :check #(intent-action! % :observe) :apply apply!
       :recover #(intent-action! % :recover) :discard #(intent-action! % :discard)
       :retire (fn [offer] (when (and (owner-call?) (= offer (basis)))
                             (when-let [id (:sync-id offer)]
                               (when (and (not= id (:retired-sync-id (state)))
                                          (or (nil? (:intent (state))) (terminal (get-in (state) [:reply :status]))))
                                 (invoke! :retire {:sync-id id})))))
       :open #(when (owner-call?) (update! assoc :open? %))
       :can-apply? #(and (valid?) (= :ready (get-in (state) [:reply :status]))
                         (= (:verified-on (state)) [(connection/snapshot) (:target-stamp (memory-state))]))
       :enabled? valid?})))
