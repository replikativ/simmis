(ns is.simm.ops.knowledge-output-service
  "Application adapters for governed existing workflow output. Capture the
   original installation synchronously before queueing a browser operation."
  (:require [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.owned-knowledge-output :as owned]
            [is.simm.runtimes.application-ops :as application]))

(defn- safe [failure f]
  (try (f)
       (catch Throwable error
         (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
         failure)))

(defn- principal [party]
  (when-not (uuid? party) (throw (ex-info "Authentication required" {})))
  {:principal/kind :human :principal/id (str "party/" party)})

(defn- prepare [party room-id id operation failure]
  (safe failure
        #(let [human (principal party) invoke (application/capture-operation)]
           (fn [] (safe failure (fn [] (invoke (fn [host] (operation host human room-id id)))))))))

(defn prepare-collect [party room-id request-id]
  (let [failure {:status :unavailable} result (prepare party room-id request-id owned/collect! failure)]
    (if (fn? result) result (constantly failure))))

(defn prepare-inspect-job [party room-id job-id]
  (let [failure {:status :none} result (prepare party room-id job-id owned/inspect-job! failure)]
    (if (fn? result) result (constantly failure))))

(defn collect!
  "Synchronous trusted-host/REPL convenience. Queued adapters must use
   prepare-collect before crossing their blocking seam instead."
  [party room-id request-id]
  (safe {:status :unavailable}
        #(let [human (principal party)]
           (application/with-operation! (fn [host] (owned/collect! host human room-id request-id))))))
