(ns is.simm.uis.web.desktop.benchmark-report-remote
  "Fixed authenticated report actions; capture the original App before queueing."
  (:refer-clojure :exclude [await])
  (:require [org.replikativ.spindel.distributed.macros :refer [defn-spin-remote] :include-macros true]
            [org.replikativ.spindel.effects.await :refer [await]]
            #?(:clj [org.replikativ.spindel.blocking :as blocking])
            #?(:clj [is.simm.model.access :as access])
            #?(:clj [is.simm.ops.benchmark-reports :as reports])
            #?(:clj [is.simm.ops.dvergr :as ops])
            #?(:clj [is.simm.ops.file-output-adoption :as adoption])
            #?(:clj [is.simm.runtimes.application-ops :as application])))

#?(:clj
   (defn- unavailable! [error]
     (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
     (throw (ex-info "Report publication did not finish. Retry in its original Memory; existing authored files are preserved."
                     {:type :memory/report-unavailable}))))

#?(:clj
   (defn prepare-operation [party room-id action request]
     (when-not (and (uuid? party) (string? room-id) (= room-id (some-> room-id parse-uuid str))
                    (#{:prepare :publish} action))
       (throw (ex-info "Authenticated room required" {:type :authentication-required})))
     (try
       (let [invoke (application/capture-operation) principal (ops/party-principal party)]
         (fn []
           (try
             (invoke (fn [host] ((case action :prepare reports/prepare! :publish reports/publish!)
                                 host principal room-id request)))
             (catch Throwable error (unavailable! error)))))
       (catch Throwable error (unavailable! error)))))

(defn-spin-remote prepare-report! [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [body (prepare-operation (access/authenticated-party-id) room-id-str :prepare request)]
                         (await (blocking/blocking body)))
                  :cljs nil)))

(defn-spin-remote publish-report! [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [body (prepare-operation (access/authenticated-party-id) room-id-str :publish request)]
                         (await (blocking/blocking body)))
                  :cljs nil)))
