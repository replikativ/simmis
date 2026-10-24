(ns is.simm.runtimes.room-apps
  "Private room-app serving through native path rules and current Simmis ReBAC.
   App lifetime covers the synchronous read; this is not a Room lease or iframe
   capability. Browser script isolation is part of the serving response."
  (:require [dvergr.web.apps :as apps]
            [is.simm.model.access :as access]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.http-auth :as auth]
            [org.replikativ.spindel.engine.core :as ec]
            [taoensso.telemere :as log]))

(def ^:private isolation-headers
  {"Content-Security-Policy" "sandbox allow-scripts" "Referrer-Policy" "no-referrer"})

(defn- response [status text]
  {:status status :headers {"Content-Type" "text/plain; charset=utf-8"} :body text})

(defn- authorize! [host party room-id]
  (when-not (and (uuid? room-id)
                 (access/can? (reads/current-host-control-db! host) party :read {:room room-id}))
    (throw (ex-info "Room app read refused" {:type ::denied}))))

(defn- unavailable? [error]
  (or (instance? java.io.IOException error)
      (contains? #{:is.simm.runtimes.application-ops/held
                   :is.simm.runtimes.operation-lifetime/refused
                   :is.simm.ops.owned-reads/refused} (:type (ex-data error)))))

(defn handle [request]
  (let [party (auth/party-id request)
        result
        (if-not party
          (response 401 "authentication required")
          (try
            (application/with-operation!
             (fn [host]
               (binding [ec/*execution-context* (:execution-ctx host)]
                 (let [served-room (volatile! nil)
                       result (or (apps/handle
                                   request nil
                                   (fn [row _slug]
                                     ;; Native resolution happens ONCE. A prechecked
                                     ;; slug cannot donate its room's authority.
                                     (authorize! host party (:room/id row))
                                     (vreset! served-room (:room/id row))))
                                  (response 404 "not found"))]
                   (reads/current-host-control-db! host)
                   (when-let [room @served-room] (authorize! host party room))
                   result))))
            (catch Throwable error
              (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
              (cond
                (= ::denied (:type (ex-data error))) (response 403 "room read refused")
                (unavailable? error) (do (log/log! {:level :warn :id ::read-unavailable :error error})
                                         (response 503 "room app unavailable"))
                :else (do (log/log! {:level :error :id ::serve-failed :error error})
                          (response 500 "room app failed"))))))]
    (update result :headers merge isolation-headers)))
