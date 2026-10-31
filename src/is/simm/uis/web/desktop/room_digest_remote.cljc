(ns is.simm.uis.web.desktop.room-digest-remote
  "Fixed optional digest RPCs. Original App/human are captured before queuing."
  (:refer-clojure :exclude [await])
  (:require [org.replikativ.spindel.distributed.macros :refer [defn-spin-remote] :include-macros true]
            [org.replikativ.spindel.effects.await :refer [await]]
            #?(:clj [org.replikativ.spindel.blocking :as blocking])
            #?(:clj [is.simm.model.access :as access])
            #?(:clj [is.simm.model.work-requests :as requests])
            #?(:clj [is.simm.ops.dvergr :as ops])
            #?(:clj [is.simm.ops.room-digest :as digest])
            #?(:clj [is.simm.ops.room-summary-settings :as summary-settings])
            #?(:clj [is.simm.ops.digest-notification :as notification])
            #?(:clj [is.simm.ops.file-output-adoption :as adoption])
            #?(:clj [is.simm.runtimes.application-ops :as application])
            #?(:clj [is.simm.runtimes.rpc-errors :as rpc-errors])))

#?(:clj
   (defn prepare-operation [party room operation request]
     (when-not (and (uuid? party) (string? room) (= room (some-> (parse-uuid room) str))
                    (#{:preview :install :launch :result :publish :summary-settings :configure-summary} operation))
       (throw (ex-info "Invalid digest request" {:type :validation-error})))
     (let [invoke (application/capture-operation)
           principal (ops/party-principal party)
           request-id (when (and (= :launch operation) (uuid? (:request-id request)))
                        (requests/request-id party (parse-uuid room) (:request-id request)))]
       (fn []
         (try
           (invoke (fn [host]
                     (case operation
                       :summary-settings (summary-settings/read! host principal room)
                       :configure-summary (summary-settings/configure! host principal room request)
                       :preview (digest/preview! host principal room request)
                       :install (digest/install! host principal room request)
                       :launch (let [result (digest/launch! host principal room request)]
                                 (when-not (= request-id (:id result))
                                   (throw (ex-info "Uncertain digest admission" {})))
                                 {:status :submitted :request request-id})
                       :result (digest/result! host principal room request)
                       :publish (notification/publish! host principal room request))))
           (catch Throwable error
             (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
             (cond-> {:status (if (#{:install :launch :publish :configure-summary} operation) :inspection-required :unavailable)}
               request-id (assoc :request request-id)
               (= :configure-summary operation)
               (assoc :error (ex-data (rpc-errors/sanitize error "configure-room-summary"))))))))))

(defn-spin-remote preview-room-digest! [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (await (blocking/blocking (prepare-operation (access/authenticated-party-id) room-id-str :preview request))) :cljs nil)))

(defn-spin-remote install-room-digest! [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (await (blocking/blocking (prepare-operation (access/authenticated-party-id) room-id-str :install request))) :cljs nil)))

(defn-spin-remote launch-room-digest! [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (await (blocking/blocking (prepare-operation (access/authenticated-party-id) room-id-str :launch request))) :cljs nil)))

(defn-spin-remote read-room-digest! [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (await (blocking/blocking (prepare-operation (access/authenticated-party-id) room-id-str :result request))) :cljs nil)))


(defn-spin-remote publish-room-digest! [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (await (blocking/blocking (prepare-operation (access/authenticated-party-id) room-id-str :publish request))) :cljs nil)))

(defn-spin-remote read-room-summary-settings! [server-id room-id-str]
  (spin-remote server-id [room-id-str]
               #?(:clj (await (blocking/blocking (prepare-operation (access/authenticated-party-id) room-id-str :summary-settings nil))) :cljs nil)))

(defn-spin-remote configure-room-summary! [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (await (blocking/blocking (prepare-operation (access/authenticated-party-id) room-id-str :configure-summary request))) :cljs nil)))
