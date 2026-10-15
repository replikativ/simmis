(ns is.simm.authority.dvergr-ops-test
  "dvergr ops invoked by simmis with a simmis principal: the leaderboard
   (`is.simm.ops.experiments`), which reads `:scorecard/list`,
   `:scorecard/detail`, `:experiment/progress` and `:experiment/report` through
   `dvergr.ops/invoke` for `party-principal` of the reading party.

   dvergr consults its `:authorize` seam on every such call; simmis installs
   none (`dvergr.principal/configure!` is never called), so dvergr's default,
   allow-all, answers. Today the only check is simmis's own `rpc-policy` row in
   front of `load-leaderboard!` — sound for that one remote, but every other
   caller of these ops (P1's MCP projection among them) inherits allow-all.

   The team room is wired to a real dvergr room holding a completed experiment
   (`experiments-test`'s fixture); only `live-room` is redefined to find it."
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.agents.room-agents :as ra]
            [is.simm.authority.known-hole :refer [known-hole]]
            [is.simm.authority.world :as world]
            [is.simm.ops.experiments :as experiments]
            [is.simm.ops.experiments-test]
            [is.simm.uis.web.desktop.experiments-remote]))

(def ^:private with-experiment-room @#'is.simm.ops.experiments-test/with-experiment-room)

(defn- scored? [board]
  (= 1 (count (:experiments board))))

(deftest the-leaderboard-is-read-for-the-room
  (world/with-world [_ w]
    (with-experiment-room
      (fn [dvergr-room]
        (with-redefs [ra/live-room (fn [rid] (when (= rid (:room w)) dvergr-room))]
          (let [remote (fn [kind] (world/inbound! (world/principal w kind) "load-leaderboard!"
                                                  {:room-id-str (str (:room w))}))]
            (testing "legitimate: a member reads it, through the remote and through the ops"
              (is (scored? (:result (remote :member))))
              (is (scored? (experiments/room-leaderboard (:room w) (:member w)))))
            (testing "the simmis gate refuses a non-member at the remote"
              (is (world/refused? (remote :non-member)))
              (is (world/refused? (remote :anon))))
            (known-hole
             "§4 P1 dvergr :authorize seam not installed"
             "simmis installs no dvergr authorizer, so dvergr's ops answer allow-all: a non-member's principal reads the team room's leaderboard straight through the ops"
             (scored? (experiments/room-leaderboard (:room w) (:non-member w))))))))))
