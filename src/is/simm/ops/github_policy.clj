(ns is.simm.ops.github-policy
  "Fixed human Memory settings for incoming review and owned interval checks."
  (:require [datahike.api :as d]
            [is.simm.model.git-policy :as policy]
            [is.simm.model.github-connections :as connections]
            [is.simm.model.github-integrations :as integrations]
            [is.simm.ops.github-host :as host]))

(defn- refuse! [] (throw (ex-info "Git policy settings changed" {:type ::refused})))

(defn- uuid [value]
  (when (and (string? value) (= value (some-> value parse-uuid str))) (parse-uuid value)))

(defn status! [context]
  (let [{:keys [connection]} (host/connection! context)
        id (connections/connection-id connection)
        row (policy/effective @(:conn context) (:room connection) id)]
    {:connection-id (str id) :target-stamp (str (get-in context [:bound :target-stamp]))
     :revision (str (:git.policy/revision row))
     :enabled? (:git.policy/enabled? row) :interval-seconds (:git.policy/interval-seconds row)
     :review-mode (:git.policy/review-mode row)
     :status (:git.policy/status row)
     :last-check (some-> ^java.util.Date (:git.policy/last-check row) .toInstant str)
     :next-check (some-> ^java.util.Date (:git.policy/next-check row) .toInstant str)}))

(defn- save! [context request]
  (let [{:keys [connection]} (host/connection! context)
        id (connections/connection-id connection) room (:room connection)
        revision (uuid (:revision request)) by (get-in context [:resolved :party])
        fresh (random-uuid) now (java.util.Date.)]
    (when-not (= id (uuid (:connection-id request))) (refuse!))
    (d/transact (:conn context)
                [[:db.fn/call
                  (fn [db]
                    (host/writer-current! context db)
                    (when-not (= id (:github.integration/connection-id (integrations/current-in db room))) (refuse!))
                    (policy/configure-tx db room id revision fresh by (:settings request) now))]])
    (status! context)))

(defn prepare [by room operation request]
  (when-not (and (map? request)
                 (case operation
                   :status (and (= #{:expected-target-stamp} (set (keys request))) (uuid (:expected-target-stamp request)))
                   :save (and (= #{:connection-id :revision :settings} (set (keys request)))
                              (uuid (:connection-id request)) (uuid (:revision request)) (map? (:settings request)))
                   false)) (refuse!))
  ;; Control-only CAS settings must remain usable during a native Git fetch.
  ;; Keep the original shared lease and merge authority, without taking the
  ;; exclusive Git key or global native import slot.
  (host/capture by room :observe
                (fn [context]
                  (if (= :status operation)
                    (do (when-not (= (uuid (:expected-target-stamp request)) (get-in context [:bound :target-stamp])) (refuse!))
                        (status! context))
                    (save! context request)))))
