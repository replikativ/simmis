(ns is.simm.uis.web.desktop.page-view-remote
  "Fixed saved page-view RPC. Capture caller and App before blocking work."
  (:refer-clojure :exclude [await])
  (:require [org.replikativ.spindel.distributed.macros :refer [defn-spin-remote] :include-macros true]
            [org.replikativ.spindel.effects.await :refer [await]]
            #?(:clj [org.replikativ.spindel.blocking :as blocking])
            #?(:clj [is.simm.model.access :as access])
            #?(:clj [is.simm.ops.owned-page-views :as views])
            #?(:clj [is.simm.ops.file-output-adoption :as adoption])
            #?(:clj [is.simm.runtimes.application-ops :as application])))

#?(:clj
   (defn- unavailable! [error]
     (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
     (throw (ex-info "This saved page view is unavailable." {:type :memory/page-view-unavailable}))))

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
           (try (invoke (fn [host] (views/read! host principal room-id request)))
                (catch Throwable error (unavailable! error)))))
       (catch Throwable error (unavailable! error)))))

(defn-spin-remote load-memory-page-view!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [body (prepare-operation (access/authenticated-party-id) room-id-str request)]
                         (await (blocking/blocking body)))
                  :cljs nil)))
