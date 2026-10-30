(ns is.simm.ops.personal-connections
  "Personal GitHub vault operations on the original App, independently of Rooms."
  (:require [datahike.api :as d]
            [is.simm.model.personal-connections :as accounts]
            [is.simm.model.mail-credentials :as envelope]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- refuse! [] (throw (ex-info "Personal connection could not be updated" {:type ::refused})))

(defn- human! [db by]
  (when-not (and (uuid? by) (= :human (let [p (d/entity db [:party/id by])]
                                        (or (:actor/kind p) (:party/type p))))) (refuse!)))

(defn- uuid [v] (when (and (string? v) (= v (some-> v parse-uuid str))) (parse-uuid v)))

(defn prepare [by operation request]
  (let [fields (case operation :list #{} :save #{:id :revision :name :token} :revoke #{:id :revision} nil)
        id (uuid (:id request)) revision (uuid (:revision request))]
    (when-not (and fields (map? request) (= fields (set (keys request)))
                   (or (= :list operation) (and id (or (nil? (:revision request)) revision)))
                   (or (not= :save operation)
                       (and (string? (:name request)) (<= 1 (count (:name request)) 80)
                            (string? (:token request)) (re-matches #"[A-Za-z0-9_]{20,512}" (:token request))))) (refuse!))
    (let [invoke (application/capture-operation)]
      (fn []
        (invoke
         (fn [host]
           (binding [ec/*execution-context* (:execution-ctx host)]
             (let [db (reads/current-host-control-db! host)
                   conn (:control-conn (::reads/owner host)) ticket lifetime/*ticket*
                   _ (human! db by)]
               (when-not (= :list operation)
                 (let [row (accounts/current-in db by id)
                       _ (when-not (= revision (:integration.account/revision row)) (refuse!))
                       next (random-uuid)
                       encrypted (when (= :save operation)
                                   (envelope/encrypt id {:version 1 :kind :github-account :by by :account-id id :token (:token request)}))]
                   (d/transact conn
                               [[:db.fn/call
                                 (fn [current]
                                   ;; The serialized Datahike writer is another
                                   ;; thread. Observe the captured retained
                                   ;; ticket; never impersonate its caller.
                                   (when-not (and (lifetime/retained? ticket) (lifetime/ready? (:owner ticket))) (refuse!))
                                   (human! current by)
                                   (if (= :save operation)
                                     (accounts/replace-tx current by id row next (:name request) encrypted)
                                     (accounts/revoke-tx current by id row next)))]])))
               ;; Explicit safe projection only; no ciphertext or token returns.
               (accounts/list-in (reads/current-host-control-db! host) by)))))))))
