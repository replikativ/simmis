(ns is.simm.uis.web.desktop.github-remote
  "Authenticated fixed GitHub Memory operations, off the FRP drain."
  (:refer-clojure :exclude [await])
  (:require [org.replikativ.spindel.distributed.macros :refer [defn-spin-remote] :include-macros true]
            [org.replikativ.spindel.effects.await :refer [await]]
            #?(:clj [org.replikativ.spindel.blocking :as blocking])
            #?(:clj [is.simm.model.access :as access])
            #?(:clj [is.simm.ops.github-sync :as sync])
            #?(:clj [is.simm.ops.github-sync-data :as facts])
            #?(:clj [is.simm.ops.file-output-adoption :as adoption])
            #?(:clj [taoensso.telemere :as log])))

#?(:clj
   (defn prepare-action [party room operation request]
     (when-not (and (string? room) (= room (some-> room parse-uuid str)))
       (throw (ex-info "Invalid room identity" {:type :validation-error})))
     (let [body (sync/prepare party (parse-uuid room) operation request)]
       (fn []
         (try (body)
              (catch Throwable error
                (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
                (let [correlation (str (random-uuid))]
                  (log/log! {:level :error :id ::unconfirmed :msg "GitHub Memory outcome is unconfirmed"
                             :data {:correlation-id correlation} :error error})
                  {:status :unconfirmed :reason (facts/error-reason error) :correlation-id correlation})))))))

(defn-spin-remote load-github-memory-sync!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [body (prepare-action (access/authenticated-party-id) room-id-str :observe request)]
                         (await (blocking/blocking body))) :cljs nil)))

(defn-spin-remote sync-github-memory!
  [server-id room-id-str operation request]
  (spin-remote server-id [room-id-str operation request]
               #?(:clj (do
                         (when-not (#{:prepare :apply :recover :discard} operation)
                           (throw (ex-info "Invalid Git operation" {:type :validation-error})))
                         (let [body (prepare-action (access/authenticated-party-id) room-id-str operation request)]
                           (await (blocking/blocking body)))) :cljs nil)))

(defn-spin-remote retire-github-memory-sync!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [body (prepare-action (access/authenticated-party-id) room-id-str :retire request)]
                         (await (blocking/blocking body))) :cljs nil)))
