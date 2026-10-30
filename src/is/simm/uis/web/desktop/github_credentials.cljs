(ns is.simm.uis.web.desktop.github-credentials
  "Context-owned credential metadata only. Tokens pass once from an uncontrolled
   password field to the authenticated RPC; they are never put in signals."
  (:require [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.github-remote :as remote]
            [is.simm.runtimes.web :as web]))

(defn- uuid-string? [s] (and (string? s) (= s (some-> s parse-uuid str))))

(defn controller [actions memory-state]
  (let [ctx (or ec/*execution-context* runtime) session (web/current-session)
        room (second (:owner actions)) key (:owner actions) state (:state actions)
        current? #(web/current-session? session)
        owner-call? #(and (current?) ((:enabled? actions))
                          (or (nil? ec/*execution-context*)
                              (identical? (ec/home-context ec/*execution-context*) (ec/home-context ctx))))
        scope #(binding [ec/*execution-context* ctx]
                 [(connection/snapshot) (:target-stamp (memory-state))
                  (select-keys (:github-source (memory-state)) [:repository :ref])])
        basis #(binding [ec/*execution-context* ctx]
                 {:action ((:basis actions)) :scope (:credential-observed (state))
                  :credential (select-keys (:credential-reply (state)) [:connection-id :revision])})
        update! (fn [f & args]
                  (binding [ec/*execution-context* ctx]
                    (when (current?) (apply swap! sig/github-sync-state update key f args))))]
    (letfn [(request! [offer operation token]
              (when (and (owner-call?) (= offer (basis)) (not (:credential-loading? (state))))
                (binding [ec/*execution-context* ctx]
                  (let [[epoch stamp source :as observed] (scope)
                        reply (:credential-reply (state)) ticket (random-uuid)
                        valid-save? (and (= observed (:credential-observed (state)))
                                         (uuid-string? (:connection-id reply)))
                        request (case operation
                                  :status {:expected-target-stamp (str stamp)}
                                  :save {:connection-id (:connection-id reply) :revision (:revision reply) :token token}
                                  :revoke {:connection-id (:connection-id reply) :revision (:revision reply)})
                        owns? #(and (current?) (= ticket (:credential-ticket (state)))
                                    (not= ticket (:credential-finished (state))))
                        fail (fn [_]
                               (binding [ec/*execution-context* ctx]
                                 (when (owns?)
                                   (update! assoc :credential-loading? false :credential-finished ticket :credential-observed observed
                                            :credential-reply nil :credential-error "GitHub access could not be updated. Reload its settings and try again."))))
                        done (fn [result]
                               (binding [ec/*execution-context* ctx]
                                 (when (owns?)
                                   (if (and (= observed (scope))
                                            (= #{:configured? :revision :connection-id :target-stamp :repository :ref} (set (keys result)))
                                            (boolean? (:configured? result)) (uuid-string? (:connection-id result))
                                            (or (nil? (:revision result)) (uuid-string? (:revision result)))
                                            (= (str stamp) (:target-stamp result))
                                            (= source (select-keys result [:repository :ref])))
                                     (update! assoc :credential-loading? false :credential-finished ticket :credential-error nil
                                              :credential-observed observed :credential-reply result)
                                     (fail nil)))))]
                    (when (or (= :status operation) valid-save?)
                      (update! assoc :credential-ticket ticket :credential-loading? true :credential-error nil
                               :credential-requested observed)
                      (try
                        (binding [ec/*spin-id* nil]
                          ((remote/github-credential-operation! web/server-id room operation request) done fail))
                        (catch :default error (fail error))))))))
            (ensure! []
              (when (owner-call?)
                (when (and (:credential-loading? (state)) (not= (scope) (:credential-requested (state))))
                  ;; Retire a lost callback, never repeat its save or token.
                  ;; The next request observes this new scope's status only.
                  (update! assoc :credential-ticket (random-uuid) :credential-loading? false
                           :credential-reply nil :credential-observed nil :credential-error nil))
                (when (and (not (:credential-loading? (state))) (not= (scope) (:credential-observed (state))))
                  (request! (basis) :status nil))))]
      {:ensure ensure! :basis basis
       :open (fn [open?] (when (owner-call?) (update! assoc :credential-open? open?)))
       :reload #(request! % :status nil)
       :save #(request! %1 :save %2)
       :revoke #(request! % :revoke nil)})))
