(ns is.simm.uis.web.desktop.chart-remote
  "Fixed saved-chart read. Capture identity and the original application owner
   before queuing blocking work; no user-selected spec/world/schema input."
  (:refer-clojure :exclude [await])
  (:require [org.replikativ.spindel.distributed.macros :refer [defn-spin-remote] :include-macros true]
            [org.replikativ.spindel.effects.await :refer [await]]
            #?(:clj [org.replikativ.spindel.blocking :as blocking])
            #?(:clj [is.simm.model.access :as access])
            #?(:clj [is.simm.ops.owned-charts :as charts])
            #?(:clj [is.simm.ops.file-output-adoption :as adoption])
            #?(:clj [is.simm.runtimes.application-ops :as application])))

#?(:clj
   (defn- unavailable! [error]
     (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
     ;; A helper's message, ex-data, tag or cause is never a wire contract.
     (throw (ex-info "This saved chart is unavailable." {:type :memory/chart-unavailable}))))

#?(:clj
   (defn prepare-operation [party room-id request]
     (when-not (uuid? party)
       (throw (ex-info "Authentication required" {:type :authentication-required})))
     (when-not (and (string? room-id) (= room-id (some-> (parse-uuid room-id) str)))
       (throw (ex-info "Invalid room identity" {:type :validation-error})))
     (try
       (let [invoke (application/capture-operation)
             principal {:principal/kind :human :principal/id (str "party/" party)}]
         (fn []
           (try (invoke (fn [host] (charts/read! host principal room-id request)))
                (catch Throwable error (unavailable! error)))))
       (catch Throwable error (unavailable! error)))))

(defn-spin-remote load-memory-chart!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [body (prepare-operation (access/authenticated-party-id) room-id-str request)]
                         (await (blocking/blocking body)))
                  :cljs nil)))
