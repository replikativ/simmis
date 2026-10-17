(ns is.simm.uis.web.desktop.results-remote
  "Thin authenticated browser access to the same captured-result service as MCP."
  (:require [org.replikativ.spindel.distributed.macros :refer [defn-spin-remote] :include-macros true]
            [org.replikativ.spindel.distributed.core :as dist]
            #?(:clj [is.simm.model.access :as access])
            #?(:clj [is.simm.agents.room-agents :as rooms])
            #?(:clj [is.simm.runtimes.context :as context])
            #?(:clj [is.simm.ops.dvergr :as ops])
            #?(:clj [is.simm.ops.results :as results])))

#?(:clj
   (defn read-result [party room-id attempt-id options reader]
     (when-not party (throw (ex-info "Authentication required" {:error :authentication-required})))
     (context/with-server-context
       (let [room (rooms/live-room (java.util.UUID/fromString room-id))]
         (when-not room (throw (ex-info "The room is not live" {:error :room-not-live})))
         ;; The results service invokes canonical room-scoped Attempt authority
         ;; on EVERY request, including subsequent download chunks.
         (reader context/server-context (ops/party-principal party)
                 (merge options {:room (name (:id room)) :id attempt-id}))))))

(defn-spin-remote load-attempt-files!
  [server-id room-id-str attempt-id-str offset]
  (spin-remote server-id [room-id-str attempt-id-str offset]
               #?(:clj (read-result (access/authenticated-party-id) room-id-str attempt-id-str
                                    {:offset offset} results/attempt-files)
                  :cljs nil)))

(defn-spin-remote load-attempt-file!
  [server-id room-id-str attempt-id-str path offset]
  (spin-remote server-id [room-id-str attempt-id-str path offset]
               #?(:clj (read-result (access/authenticated-party-id) room-id-str attempt-id-str
                                    {:path path :offset offset} results/attempt-file)
                  :cljs nil)))
