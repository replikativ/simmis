(ns is.simm.uis.web.desktop.experiments-remote
  "Spin-remotes for experiment rooms: a room's leaderboard, the experiment
   rooms nobody has joined yet (listed by `chat-remote/load-rooms!` for
   admins), and joining one. Thin pass-throughs over
   `is.simm.ops.experiments`; uuids cross the wire as strings.

   The leaderboard is policed by the room (`{:room id}`, :read): a member reads
   it. Joining an experiment room nobody has joined is for admins only: such a
   room has no members, so no room grant could admit anyone to it."
  (:require [org.replikativ.spindel.distributed.macros :refer [defn-spin-remote]
             :include-macros true]
            [org.replikativ.spindel.distributed.core :as dist]
            #?(:clj [is.simm.ops.experiments :as experiments])
            #?(:clj [is.simm.model.access :as access])
            #?(:clj [is.simm.runtimes.context :as ctx])))

(defn-spin-remote load-leaderboard!
  [server-id room-id-str]
  (spin-remote server-id [room-id-str]
    (let [rid (identity room-id-str)]
      #?(:clj (ctx/with-server-context
                (or (experiments/room-leaderboard (java.util.UUID/fromString rid))
                    (throw (ex-info "The room is not live on this server"
                                    {:error :room-not-live :room-id rid}))))
         :cljs nil))))

(defn-spin-remote join-experiment-room!
  [server-id room-id-str]
  (spin-remote server-id [room-id-str]
    (let [rid (identity room-id-str)]
      #?(:clj (ctx/with-server-context
                (if-let [party (access/authenticated-party-id)]
                  (let [{:room/keys [id content-db-scope]}
                        (experiments/join! (java.util.UUID/fromString rid) party)]
                    {:room-id (str id) :db-scope (str content-db-scope)})
                  (throw (ex-info "authentication required" {:error :authentication-required}))))
         :cljs nil))))
