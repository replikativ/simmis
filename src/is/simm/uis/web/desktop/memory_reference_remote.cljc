(ns is.simm.uis.web.desktop.memory-reference-remote
  "Capture authenticated human/application ownership before the blocking read."
  (:refer-clojure :exclude [await])
  (:require [org.replikativ.spindel.distributed.macros :refer [defn-spin-remote] :include-macros true]
            [org.replikativ.spindel.effects.await :refer [await]]
            #?(:clj [org.replikativ.spindel.blocking :as blocking])
            #?(:clj [is.simm.model.access :as access])
            #?(:clj [is.simm.ops.owned-block-references :as references])
            #?(:clj [is.simm.ops.owned-backlinks :as backlinks])
            #?(:clj [is.simm.ops.file-output-adoption :as adoption])
            #?(:clj [is.simm.runtimes.application-ops :as application])))

#?(:clj
   (defn- unavailable! [error]
     (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
     (throw (ex-info "This reference is unavailable in the current workspace."
                     {:type :memory/reference-unavailable}))))

#?(:clj
   (defn- prepare-read [party room-id request read!]
     (when-not (uuid? party)
       (throw (ex-info "Authentication required" {:type :authentication-required})))
     (when-not (and (string? room-id) (= room-id (some-> (parse-uuid room-id) str)))
       (throw (ex-info "Invalid room identity" {:type :validation-error})))
     (try
       (let [invoke (application/capture-operation)
             principal {:principal/kind :human :principal/id (str "party/" party)}]
         (fn []
           (try (invoke (fn [host] (read! host principal room-id request)))
                (catch Throwable error (unavailable! error)))))
       (catch Throwable error (unavailable! error)))))

#?(:clj
   (defn prepare-operation [party room-id request]
     (prepare-read party room-id request references/read!)))

#?(:clj
   (defn prepare-backlinks-operation [party room-id request]
     (prepare-read party room-id request backlinks/read!)))

(defn-spin-remote resolve-memory-block!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [body (prepare-operation (access/authenticated-party-id) room-id-str request)]
                         (await (blocking/blocking body)))
                  :cljs nil)))

(defn-spin-remote load-memory-backlinks!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [body (prepare-backlinks-operation (access/authenticated-party-id) room-id-str request)]
                         (await (blocking/blocking body)))
                  :cljs nil)))
