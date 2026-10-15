(ns is.simm.authority.dvergr-ops-test
  "dvergr ops invoked by simmis with a simmis principal: the leaderboard
   (`is.simm.ops.experiments`), which reads `:scorecard/list`,
   `:scorecard/detail`, `:experiment/progress` and `:experiment/report` through
   `dvergr.ops/invoke` for `party-principal` of the reading party.

   dvergr consults its `:authorize` seam on every such call, and simmis hands
   it `access/dvergr-authorize` with each one (`is.simm.ops.dvergr/invoke`): the
   ops refuse a party that may not read the room themselves, behind the
   `rpc-policy` row in front of `load-leaderboard!`.

   The team room is wired to a real dvergr room holding a completed experiment
   (`experiments-test`'s fixture): `live-room` is redefined to find it, and the
   world's team room carries that dvergr room's slug, which is how the
   authorizer maps dvergr's `[:room \"<id>\"]` back to a simmis room."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [dvergr.principal :as principal]
            [dvergr.room.store :as rstore]
            [is.simm.agents.room-agents :as ra]
            [is.simm.model.access :as access]
            [is.simm.ops.dvergr :as ops]
            [is.simm.authority.world :as world]
            [is.simm.ops.experiments :as experiments]
            [is.simm.ops.experiments-test]
            [is.simm.uis.web.desktop.experiments-remote]))

(def ^:private with-experiment-room @#'is.simm.ops.experiments-test/with-experiment-room)

(defn- scored? [board]
  (= 1 (count (:experiments board))))

(defn- denied? [f]
  (try (f) false
       (catch clojure.lang.ExceptionInfo e
         (= ::principal/denied (:type (ex-data e))))))

(deftest the-leaderboard-is-read-for-the-room
  (world/with-world [conn w]
    (with-experiment-room
      (fn [dvergr-room]
        (d/transact conn [{:room/id (:room w) :room/slug (rstore/room-id->slug (:id dvergr-room))}])
        (with-redefs [ra/live-room (fn [rid] (when (= rid (:room w)) dvergr-room))]
          (let [remote (fn [kind] (world/inbound! (world/principal w kind) "load-leaderboard!"
                                                  {:room-id-str (str (:room w))}))]
            (testing "legitimate: a member reads it, through the remote and through the ops"
              (is (scored? (:result (remote :member))))
              (is (scored? (experiments/room-leaderboard (:room w) (:member w)))))
            (testing "the simmis gate refuses a non-member at the remote"
              (is (world/refused? (remote :non-member)))
              (is (world/refused? (remote :anon))))
            (testing "§4 P1 closed: dvergr's ops refuse a non-member's principal themselves"
              (is (denied? #(experiments/room-leaderboard (:room w) (:non-member w))))
              (is (denied? #(experiments/room-leaderboard (:room w) (:admin w)))
                  ":admin reads no room it is not in"))
            (testing "a principal that names no party is refused"
              (is (denied? #(ops/invoke (:ctx dvergr-room) {:principal/id "telegram/1"
                                                            :principal/kind :human}
                                        :scorecard/list {:room dvergr-room}))))))))))

(deftest dvergr-descriptors-map-onto-can
  (world/with-world [conn w]
    (d/transact conn [{:room/id (:room w) :room/slug "team"}])
    (let [db @conn
          p #(ops/party-principal (get w %))
          allow? (fn [kind request] (:allow? (access/dvergr-authorize db (p kind) request)))
          room (fn [action] {:op :x/y :action action :resource [:room "team"] :resolved {:room "team"}})
          job (fn [action owner] {:op :job/cancel :action action :resource [:job "j"]
                                  :resolved {:room "team" :owner owner}})]
      (testing "[:room …]: the same action on the simmis room"
        (is (allow? :member (room :read)))
        (is (allow? :member (room :write)))
        (is (not (allow? :member (room :admin))) ":admin on a room is the owner's")
        (is (allow? :owner (room :admin)))
        (is (not (allow? :non-member (room :read))))
        (is (not (allow? :member {:op :x/y :action :read :resource [:room "nowhere"]
                                  :resolved nil}))
            "a handle that resolves to nothing"))
      (testing "[:job …]: reading is reading its room; cancelling is the starter's or the owner's"
        (is (allow? :member (job :read :dvergr)))
        (is (allow? :member (job :write (keyword "party" (str (:member w))))))
        (is (not (allow? :member (job :write (keyword "party" (str (:owner w)))))))
        (is (allow? :owner (job :write (keyword "party" (str (:member w))))))
        (is (not (allow? :non-member (job :read :dvergr)))))
      (testing "[:global]: reads for any party, writes for :admin"
        (is (allow? :member {:op :agent/list :action :read :resource [:global]}))
        (is (not (allow? :member {:op :room/create :action :write :resource [:global]})))
        (is (allow? :admin {:op :room/create :action :write :resource [:global]})))
      (testing "[:agent …] is :admin's; an unknown descriptor is nobody's"
        (is (not (allow? :owner {:op :agent/update :action :admin :resource [:agent "a"]})))
        (is (allow? :admin {:op :agent/update :action :admin :resource [:agent "a"]}))
        (is (not (allow? :admin {:op :x/y :action :read :resource [:experiment "e"]}))))
      (testing "dvergr itself denies when the authorizer throws"
        (is (false? (:allow? (principal/decide {:config {:auth {:authorize (fn [_ _] (throw (ex-info "boom" {})))}}}
                                               (p :owner) (room :read)))))))))
