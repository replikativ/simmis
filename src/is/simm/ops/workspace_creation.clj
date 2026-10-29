(ns is.simm.ops.workspace-creation
  "Fixed human provisioning service. Queued work retains its original App;
   observing an outcome never provisions, publishes, retries or funds anything."
  (:require [is.simm.model.knowledge-rooms :as rooms]
            [is.simm.model.workspace-intent :as intent]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [taoensso.telemere :as log]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- safe [body]
  (try (body)
       (catch Throwable error
         (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
         ;; Native intent may already exist; this is NOT a rejection receipt
         ;; authorizing a resend. The human can observe the same frozen intent.
         (let [correlation (str (random-uuid))]
           (log/log! {:level :error :id ::unconfirmed
                      :msg "Workspace creation operation could not confirm its outcome"
                      :data {:correlation-id correlation} :error error})
           {:status :unavailable :correlation-id correlation}))))

(defn prepare
  "Validate/capture before queuing; returned thunk performs one fixed action.
   No client-selected creator, repository profile, context, path or service."
  [party operation wire-request]
  (when-not (and (uuid? party) (#{:create :observe} operation))
    (throw (ex-info "Authentication required" {:type :authentication-required})))
  (let [request (intent/request! wire-request)
        invoke (application/capture-operation)]
    (fn []
      (safe
       #(invoke
         (fn [host]
           (let [root (:execution-ctx host)
                 current! (fn [] (lifetime/check-current!) (reads/current-host-control-db! host))]
             (when (and (ec/execution-context-bound?)
                        (not (identical? (ec/home-context (ec/current-execution-context))
                                         (ec/home-context root))))
               (throw (ex-info "Workspace creation requires its original host" {})))
             (binding [ec/*execution-context* root]
               (let [snapshot (current!)]
                 (intent/human! snapshot party)
                 (when (= :create operation)
                   (intent/authorize! snapshot party request)
                   (rooms/create-for-intent! party wire-request current!))
                 (intent/observe (current!) party request))))))))))
