(ns is.simm.uis.web.desktop.personal-connections
  "Personal vault metadata projection. Secrets pass once from DOM to RPC."
  (:require [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.github-remote :as remote]
            [is.simm.runtimes.web :as web]))

(defn metadata? [rows]
  (and (vector? rows) (<= (count rows) 64)
       (every? (fn [row]
                 (and (= #{:id :provider :name :revision :configured?} (set (keys row)))
                      (= :github (:provider row)) (string? (:name row)) (<= 1 (count (:name row)) 80)
                      (boolean? (:configured? row))
                      (every? #(and (string? %) (= % (some-> % parse-uuid str))) [(:id row) (:revision row)]))) rows)))

(defn controller []
  (let [ctx (or ec/*execution-context* runtime) session (web/current-session)
        current? #(and (web/current-session? session)
                       (identical? (ec/home-context ctx) (ec/home-context runtime))
                       (or (nil? ec/*execution-context*)
                           (identical? (ec/home-context ec/*execution-context*) (ec/home-context ctx))))
        state #(binding [ec/*execution-context* ctx] @sig/personal-connections-state)
        scope #(binding [ec/*execution-context* ctx] [(connection/snapshot) (:id @sig/current-user)])
        update! (fn [& args] (binding [ec/*execution-context* ctx] (when (current?) (apply swap! sig/personal-connections-state args))))]
    (letfn [(request! [offer operation request]
              (when (and (current?) (= offer (scope)) (not (:loading? (state))))
                (let [ticket (random-uuid)
                      owns? #(and (current?) (= ticket (:ticket (state))) (not= ticket (:finished (state))))
                      fail (fn [_]
                             (when (owns?)
                               (update! assoc :loading? false :finished ticket :observed offer
                                        :error "Connection update not confirmed. Reload connections to inspect the saved state; your token will not be sent again.")))
                      done (fn [rows]
                             (when (owns?)
                               (if (and (= offer (scope)) (metadata? rows))
                                 (update! assoc :loading? false :finished ticket :observed offer :rows rows :error nil)
                                 (fail nil))))]
                  (update! assoc :ticket ticket :loading? true :requested offer :error nil)
                  (try (binding [ec/*spin-id* nil]
                         ((remote/personal-connection-operation! web/server-id operation request) done fail))
                       (catch :default _ (fail nil))))))
            (ensure! []
              (when (current?)
                (when (and (:loading? (state)) (not= (:requested (state)) (scope)))
                  (update! assoc :ticket (random-uuid) :loading? false :rows nil :observed nil :error nil))
                (when (and (not (:loading? (state))) (not= (:observed (state)) (scope)))
                  (request! (scope) :list {}))))]
      {:state state :basis scope :ensure ensure!
       :reload #(request! % :list {})
       :save (fn [offer id revision name token] (request! offer :save {:id id :revision revision :name name :token token}))
       :revoke (fn [offer id revision] (request! offer :revoke {:id id :revision revision}))})))
