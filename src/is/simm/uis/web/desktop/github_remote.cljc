(ns is.simm.uis.web.desktop.github-remote
  "Authenticated fixed GitHub Memory operations, off the FRP drain."
  (:refer-clojure :exclude [await])
  (:require [org.replikativ.spindel.distributed.macros :refer [defn-spin-remote] :include-macros true]
            [org.replikativ.spindel.effects.await :refer [await]]
            #?(:clj [org.replikativ.spindel.blocking :as blocking])
            #?(:clj [is.simm.model.access :as access])
            #?(:clj [is.simm.ops.github-service :as service])
            #?(:clj [is.simm.ops.personal-connections :as personal])
            #?(:clj [is.simm.ops.github-policy :as git-policy])
            #?(:clj [is.simm.ops.github-decisions :as decisions])
            #?(:clj [is.simm.ops.github-sync-data :as facts])
            #?(:clj [is.simm.ops.file-output-adoption :as adoption])
            #?(:clj [taoensso.telemere :as log])))

#?(:clj
   (defn prepare-action [party room operation request]
     (when-not (and (string? room) (= room (some-> room parse-uuid str)))
       (throw (ex-info "Invalid room identity" {:type :validation-error})))
     (let [body (try (service/prepare party (parse-uuid room) operation request)
                     (catch Exception error (fn [] (throw error))))]
       (fn []
         (try (body)
              (catch Throwable error
                (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
                (let [correlation (str (random-uuid))]
                  (log/log! {:level :error :id ::unconfirmed :msg "GitHub Memory outcome is unconfirmed"
                             :data {:correlation-id correlation} :error error})
                  {:status (if (and (#{:prepare :prepare-incoming :prepare-policy :prepare-push} operation)
                                    (= :is.simm.ops.github-host/not-started (:type (ex-data error))))
                             :not-started :unconfirmed)
                   :reason (facts/error-reason error) :correlation-id correlation})))))))

(defn-spin-remote load-github-memory-sync!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [body (prepare-action (access/authenticated-party-id) room-id-str :observe request)]
                         (await (blocking/blocking body))) :cljs nil)))

(defn-spin-remote sync-github-memory!
  [server-id room-id-str operation request]
  (spin-remote server-id [room-id-str operation request]
               #?(:clj (do
                         (when-not (#{:prepare :prepare-incoming :prepare-policy :prepare-push :apply :recover :discard} operation)
                           (throw (ex-info "Invalid Git operation" {:type :validation-error})))
                         (let [body (prepare-action (access/authenticated-party-id) room-id-str operation request)]
                           (await (blocking/blocking body)))) :cljs nil)))

(defn-spin-remote retire-github-memory-sync!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [body (prepare-action (access/authenticated-party-id) room-id-str :retire request)]
                         (await (blocking/blocking body))) :cljs nil)))

(defn-spin-remote github-proposal-operation!
  [server-id room-id-str operation request]
  (spin-remote server-id [room-id-str operation request]
               #?(:clj (do
                         (when-not (and (string? room-id-str) (= room-id-str (some-> room-id-str parse-uuid str)))
                           (throw (ex-info "Invalid room identity" {:type :validation-error})))
                         (let [body (decisions/prepare (access/authenticated-party-id) (parse-uuid room-id-str) operation request)]
                           (await (blocking/blocking body)))) :cljs nil)))

(defn-spin-remote github-credential-operation!
  [server-id room-id-str operation request]
  (spin-remote server-id [room-id-str operation request]
               #?(:clj (do
                         (when-not (and (string? room-id-str) (= room-id-str (some-> room-id-str parse-uuid str)))
                           (throw (ex-info "Invalid room identity" {:type :validation-error})))
                         (let [body (service/prepare-credential (access/authenticated-party-id) (parse-uuid room-id-str) operation request)]
                           (await (blocking/blocking body)))) :cljs nil)))

(defn-spin-remote personal-connection-operation!
  [server-id operation request]
  (spin-remote server-id [operation request]
               #?(:clj (let [body (personal/prepare (access/authenticated-party-id) operation request)]
                         (await (blocking/blocking body))) :cljs nil)))

(defn-spin-remote git-policy-operation!
  [server-id room-id-str operation request]
  (spin-remote server-id [room-id-str operation request]
               #?(:clj (do
                         (when-not (and (string? room-id-str) (= room-id-str (some-> room-id-str parse-uuid str)))
                           (throw (ex-info "Invalid Memory identity" {:type :validation-error})))
                         (let [body (git-policy/prepare (access/authenticated-party-id) (parse-uuid room-id-str) operation request)]
                           (await (blocking/blocking body)))) :cljs nil)))
