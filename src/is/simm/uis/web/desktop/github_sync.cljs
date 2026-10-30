(ns is.simm.uis.web.desktop.github-sync
  "Context-owned explicit Git review. Unknown writes are observed, never resent."
  (:require [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.memory-actions :as memory]
            [is.simm.uis.web.desktop.refs :as refs]
            [is.simm.uis.web.desktop.github-remote :as remote]
            [is.simm.runtimes.web :as web]))

(def terminal #{:applied :satisfied-observed :closed :filed :not-started :unchanged})

(defn- uuid-string? [value]
  (and (string? value) (= value (some-> value parse-uuid str))))

(defn- observation-basis [epoch memory client]
  ;; Replicated progress only requests a fresh read of the same journal. It
  ;; never establishes an outcome or authorizes another write.
  [epoch (:target-stamp memory) client
   (select-keys (:github-source memory) [:sync-active? :sync-client-id :last-sync])])

(defn- reply! [reply]
  (when-not (and (map? reply) (#{:not-observed :preparing :ready :unconfirmed :held :applied :satisfied-observed :closed :filed :not-started :unchanged :pushed :rejected} (:status reply))
                 (or (not (contains? reply :cleanup-pending?)) (boolean? (:cleanup-pending? reply)))
                 (or (not= :filed (:status reply))
                     (and (string? (:proposal-id reply))
                          (= (:proposal-id reply) (some-> (:proposal-id reply) parse-uuid str))))
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
        room (str (:room-id data)) push? (= :push (:github-direction data))
        key (cond-> [(:id session) room] push? (conj :push))
        direction-key [(:id session) room :last-direction]
        current? #(and (:id session) (web/current-session? session))
        state #(binding [ec/*execution-context* ctx] (get @sig/github-sync-state key))
        update! (fn [f & args] (binding [ec/*execution-context* ctx]
                                 (when (current?) (apply swap! sig/github-sync-state update key f args))))
        live? #(and (current?) (identical? (ec/home-context ctx) (ec/home-context runtime)))
        owner-call? #(or (nil? ec/*execution-context*)
                         (identical? (ec/home-context ec/*execution-context*) (ec/home-context ctx)))
        scope #(binding [ec/*execution-context* ctx]
                 [(connection/snapshot) (:target-stamp (memory-state))
                  (select-keys (:github-source (memory-state)) [:repository :ref :import-head])])
        basis #(binding [ec/*execution-context* ctx] (action-basis (state) (memory-state) (connection/snapshot)))
        valid? #(binding [ec/*execution-context* ctx]
                  (let [m (memory-state)]
                    (and (live?) (connection/snapshot)
                         (some (fn [r] (= room (str (:room/id r)))) (:rooms @sig/user-rooms))
                         (memory/verified? m @sig/memory-connection)
                         (true? (get-in m [:permissions :evaluate?]))
                         (true? (get-in m [:github-source :available?])))))]
    (letfn [(follow-up! [operation reply coherent? verification]
              (when (and push? (coherent?) (not (:loading? (state))))
                (let [client (get-in (state) [:intent :client-id])]
                  (cond
                    (and (= :apply operation) (= :unconfirmed (:status reply))
                         (not= client (:auto-observed-client (state))))
                    (let [cycle {:client-id client :scope (scope)}]
                      (update! assoc :auto-observed-client client :push-check-cycle cycle)
                      (when (coherent?) (invoke! :observe {:client-id client} cycle)))

                    ;; Only an authenticated same-journal read proves Sending.
                    ;; A generic transport failure never grants recovery.
                    (and (= :observe operation) (= :unconfirmed (:status reply))
                         (= {:client-id client :scope (scope)} verification (:push-check-cycle (state)))
                         (= :push (:operation-kind reply))
                         (uuid-string? (:sync-id reply)) (uuid-string? (:client-id reply))
                         (= client (:client-id reply))
                         (= {:client-id client :sync-id (:sync-id reply)} (:push-journal (state)))
                         (not= client (:auto-verified-client (state))))
                    (do (update! assoc :auto-verified-client client)
                        (when (coherent?) (invoke! :recover {:client-id client})))))))
            (invoke! [operation request & [verification]]
              (binding [ec/*execution-context* ctx]
                (when (and (valid?) (not (:loading? (state))))
                  (let [token (random-uuid) epoch (connection/snapshot) stamp (:target-stamp (memory-state))
                        source (select-keys (:github-source (memory-state)) [:repository :ref :import-head])
                        owns? #(and (current?) (= token (:token (state))) (not= token (:finished (state))))
                        coherent? #(and (current?) (= token (:token (state))) (valid?)
                                        (= epoch (connection/snapshot)) (= stamp (:target-stamp (memory-state)))
                                        (= source (select-keys (:github-source (memory-state)) [:repository :ref :import-head])))
                        failed (fn [_]
                                 (when (owner-call?)
                                   (binding [ec/*execution-context* ctx]
                                     (when (owns?)
                                       (update! assoc :loading? false :finished token :verified-on nil
                                                :reply {:status :unconfirmed}
                                                :error (if push? "Publication is not confirmed. Your saved edits are kept locally."
                                                           "The outcome is unconfirmed. Check this same sync before taking another action."))
                                       (follow-up! operation {:status :unconfirmed} coherent? verification)))))
                        done (fn [reply]
                               (when (owner-call?)
                                 (binding [ec/*execution-context* ctx]
                                   (when (owns?)
                                     (try
                                       (let [reply (reply! reply)
                                             journal (when (and push? (= :push (:operation-kind reply))
                                                                (uuid-string? (:client-id reply)) (uuid-string? (:sync-id reply)))
                                                       (select-keys reply [:client-id :sync-id]))
                                             known (:push-journal (state))]
                                         (when (and journal (= (:client-id journal) (:client-id known))
                                                    (not= journal known))
                                           (throw (ex-info "Git response changed its journal identity" {})))
                                         (when (or (and (:client-id reply) (:client-id request)
                                                        (not= (:client-id reply) (:client-id request)))
                                                   (and (:sync-id reply) (:sync-id request)
                                                        (not= (:sync-id reply) (:sync-id request))))
                                           (throw (ex-info "Git response belongs to another intent" {})))
                                         (update! #(-> % (assoc :reply reply :loading? false :finished token
                                                                :verified-on (when (coherent?) [epoch stamp]))
                                                       (cond-> journal (assoc :push-journal journal))
                                                       (cond-> (terminal (:status reply)) (assoc :open? false))
                                                       (cond-> (and (= :retire operation) (= :closed (:status reply)))
                                                         (assoc :retired-sync-id (:sync-id request)))
                                                       (cond-> (and (terminal (:status reply)) (:sync-id reply))
                                                         (assoc :retired-sync-id (:sync-id reply)))
                                                       (dissoc :error)))
                                         (when (and (#{:prepare-incoming :prepare-policy} operation) (= :filed (:status reply))
                                                    (= epoch (connection/snapshot)) (= stamp (:target-stamp (memory-state))))
                                           (refs/open! {:kind :proposal :id (:proposal-id reply)}))
                                         (when (and push? (coherent?) (not= :recover operation) (#{:pushed :rejected} (:status reply)))
                                         ;; Known terminal cleanup cannot resend. If its
                                         ;; reply is lost, observation finds this same row.
                                           (invoke! :recover {:client-id (get-in (state) [:intent :client-id])}))
                                         (when (and (not push?) (not= :recover operation)
                                                    (true? (:cleanup-pending? reply)) (#{:filed :applied} (:status reply)))
                                         ;; Only positively recorded outcomes,
                                         ;; never unknown Applying or a replay
                                         ;; of fetch/integration. One attempt.
                                           (invoke! :recover {:client-id (get-in (state) [:intent :client-id])}))
                                         (follow-up! operation reply coherent? verification))
                                       (catch :default error (failed error)))))))]
                    (update! assoc :token token :operation operation :loading? true :error nil
                             :request-on epoch :observed-on (observation-basis epoch (memory-state) (get-in (state) [:intent :client-id])))
                    (swap! sig/github-sync-state assoc direction-key (if push? :push :incoming))
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
                  (when (and (valid?) (= push? (= :push (get-in (memory-state) [:github-source :sync-kind])))
                             (not (:loading? (state)))
                             (or (nil? (:intent (state)))
                                 (and (terminal (get-in (state) [:reply :status]))
                                      (not (get-in (state) [:reply :cleanup-pending?])))))
                    (when-let [client (get-in (memory-state) [:github-source :sync-client-id])]
                      (when (not= client (get-in (state) [:intent :client-id]))
                        (update! assoc :intent {:client-id client} :open? true :reply nil :verified-on nil
                                 :push-journal nil :push-check-cycle nil :auto-observed-client nil :auto-verified-client nil))))
                  (when (and (valid?) (:intent (state)) (not (:loading? (state)))
                             (not= (:observed-on (state)) (observation-basis (connection/snapshot) (memory-state)
                                                                             (get-in (state) [:intent :client-id]))))
                    (check!)))))
            (prepare! [offer]
              (when (and (owner-call?) (= offer (basis)))
                (binding [ec/*execution-context* ctx]
                  (when (and (valid?) (not (:loading? (state)))
                             (or (not (get-in (memory-state) [:github-source :sync-active?]))
                                 (= (get-in (memory-state) [:github-source :sync-id]) (:retired-sync-id (state))))
                             (or (nil? (:intent (state))) (terminal (get-in (state) [:reply :status]))))
                    (let [request {:client-id (str (random-uuid)) :expected-target-stamp (str (:target-stamp (memory-state)))}]
                      (update! assoc :intent request :open? true :reply nil :verified-on nil
                               :push-journal nil :push-check-cycle nil :auto-observed-client nil :auto-verified-client nil)
                      (invoke! (if push? :prepare-push :prepare-policy) request))))))
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
      {:owner key :direction-key direction-key :direction (if push? :push :incoming) :state state :basis basis :ensure ensure! :sync prepare! :check #(intent-action! % :observe) :apply apply!
       :recover #(intent-action! % :recover) :discard #(intent-action! % :discard)
       :check-status (fn [offer]
                       (when (and (owner-call?) (= offer (basis)) (valid?) (not (:loading? (state))))
                         ;; One explicit click may request a fresh verification
                         ;; cycle after a failed automatic check.
                         (let [client (get-in (state) [:intent :client-id]) cycle {:client-id client :scope (scope)}]
                           (when client
                             (update! assoc :auto-verified-client nil :push-check-cycle cycle)
                             (when (and (= offer (basis)) (= (:scope cycle) (scope)))
                               (invoke! :observe {:client-id client} cycle))))))
       :retire (fn [offer] (when (and (owner-call?) (= offer (basis)))
                             (when-let [id (:sync-id offer)]
                               (when (and (not= id (:retired-sync-id (state)))
                                          (or (nil? (:intent (state))) (terminal (get-in (state) [:reply :status]))))
                                 (invoke! :retire {:sync-id id})))))
       :open #(when (owner-call?) (update! assoc :open? %))
       :open-proposal (fn [] (when (and (owner-call?) (current?))
                               (when-let [id (get-in (state) [:reply :proposal-id])]
                                 (refs/open! {:kind :proposal :id id}))))
       :can-apply? #(and (valid?) (= :ready (get-in (state) [:reply :status]))
                         (= (:verified-on (state)) [(connection/snapshot) (:target-stamp (memory-state))]))
       :enabled? valid?})))
