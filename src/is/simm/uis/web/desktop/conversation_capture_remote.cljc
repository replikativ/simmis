(ns is.simm.uis.web.desktop.conversation-capture-remote
  "Fixed human evidence-capture RPCs. Identity and original installation are
   captured before entering the blocking worker, never re-selected by it."
  (:refer-clojure :exclude [await])
  (:require [org.replikativ.spindel.distributed.macros :refer [defn-spin-remote] :include-macros true]
            [org.replikativ.spindel.effects.await :refer [await]]
            #?(:clj [org.replikativ.spindel.blocking :as blocking])
            #?(:clj [is.simm.model.access :as access])
            #?(:clj [is.simm.ops.conversation-capture :as capture])
            #?(:clj [is.simm.ops.file-output-adoption :as adoption])
            #?(:clj [is.simm.runtimes.application-ops :as application])
            #?(:clj [is.simm.uis.web.desktop.conversation-capture :as protocol])))

#?(:clj
   (defn prepare-operation
     "HOST-only queued body. Capturing does no native I/O and grants no lease.
      Failure after publication starts is conservatively inspection-required."
     [party room-id operation request]
     (when-not (uuid? party)
       (throw (ex-info "Authentication required" {:type :authentication-required})))
     (when-not (and (string? room-id) (protocol/uuid-text? room-id)
                    (#{:preview :publish} operation))
       (throw (ex-info "Invalid capture request" {:type :validation-error})))
     (let [invoke (application/capture-operation)
           principal {:principal/kind :human :principal/id (str "party/" party)}]
       (fn []
         (try
           (invoke
            (fn [host]
              (case operation
                :preview (let [p (capture/preview! host principal room-id request)
                               reply (assoc p :status :preview)]
                           (when-not (protocol/preview? reply)
                             (throw (ex-info "Invalid capture preview" {})))
                           reply)
                :publish (let [p (capture/publish! host principal room-id request)
                               receipt (:write p)
                               reply {:status :saved :capture-id (:capture-id p) :path (:path p)
                                      :target-stamp (:target-stamp p) :entry (:entry receipt)
                                      :commit (some-> (:commit receipt) str)}]
                           (when-not (and (true? (:saved? receipt)) (protocol/saved? reply))
                             (throw (ex-info "Invalid capture acknowledgement" {})))
                           reply))))
           (catch Throwable error
             (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
             ;; No helper-provided error reason may manufacture a safe replay.
             {:status (if (= :publish operation) :inspection-required :unavailable)}))))))

(defn-spin-remote preview-conversation-capture!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [body (prepare-operation (access/authenticated-party-id)
                                                    room-id-str :preview request)]
                         (await (blocking/blocking body)))
                  :cljs nil)))

(defn-spin-remote publish-conversation-capture!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [body (prepare-operation (access/authenticated-party-id)
                                                    room-id-str :publish request)]
                         (await (blocking/blocking body)))
                  :cljs nil)))
