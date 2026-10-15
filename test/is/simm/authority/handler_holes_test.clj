(ns is.simm.authority.handler-holes-test
  "Holes this suite found that the P1 plan does not list, each EXECUTED end to
   end through the network-inbound path (`world/inbound!`: the real gate, the
   real handler, the real model functions over the world's system DB).

   They share one shape: the policy row polices ONE id (the room, the KB) and
   the handler acts on a SECOND id the client supplies, unchecked."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [is.simm.agents.room-agents :as ra]
            [is.simm.authority.known-hole :refer [known-hole]]
            [is.simm.authority.world :as world]
            [is.simm.model.access :as access]
            [is.simm.model.parties :as parties]
            [is.simm.model.rooms :as rooms]
            [is.simm.uis.web.desktop.chat-remote]))

(defn- room-party-ids [room-id]
  (set (:room/parties (rooms/get-room room-id))))

(deftest remove-agent-from-room-deletes-only-the-rooms-agents
  ;; The handler retracts `agent-id-str` from the room, then DELETES the party
  ;; outright when it is in no room any more — without asking whether it was in
  ;; this room, or an agent at all.
  (world/with-world [_ w]
    (with-redefs [ra/unassign-room-agent! (fn [& _])]
      (testing "legitimate: the owner removes the team's agent; it is gone"
        (is (= {:status :ok}
               (:result (world/inbound! (world/principal w :owner) "remove-agent-from-room!"
                                        {:room-id-str (str (:room w))
                                         :agent-id-str (str (:team-agent w))}))))
        (is (not (contains? (room-party-ids (:room w)) (:team-agent w))))
        (is (nil? (parties/get-party (:team-agent w)))))
      (testing "a non-member is refused at the gate"
        (is (world/refused? (world/inbound! (world/principal w :non-member) "remove-agent-from-room!"
                                            {:room-id-str (str (:room w))
                                             :agent-id-str (str (:admin w))})))
        (is (some? (parties/get-party (:admin w)))))
      (known-hole
       "UNLISTED remove-agent-from-room! deletes any party"
       "a member names a HUMAN who is in no room (here: the admin) as the agent to remove; the handler deletes that party's account"
       (world/inbound! (world/principal w :member) "remove-agent-from-room!"
                       {:room-id-str (str (:room w)) :agent-id-str (str (:admin w))})
       (nil? (parties/get-party (:admin w)))))))

(deftest attach-kb-to-room-requires-authority-over-the-kb
  (world/with-world [_ w]
    (with-redefs [ra/reset-room-context! (fn [_])]
      (testing "legitimate: the owner attaches their own KB to their room"
        (is (= {:status :ok}
               (:result (world/inbound! (world/principal w :owner) "attach-kb-to-room!"
                                        {:room-id-str (str (:room w))
                                         :kb-id-str (str (:kb-private w))}))))
        (is (true? (access/can? (world/principal w :member) :write (:kb-private-scope w)))
            "and the room's members now reach it — what attaching is for"))
      (testing "before the attack the member has no access to the stranger's KB"
        (is (false? (access/can? (world/principal w :member) :read (:kb-stranger-scope w)))))
      (known-hole
       "UNLISTED attach-kb-to-room! does not check the KB"
       "a member attaches ANOTHER party's KB (by id) to a room they are in, granting themselves :read-write on it"
       (world/inbound! (world/principal w :member) "attach-kb-to-room!"
                       {:room-id-str (str (:room w)) :kb-id-str (str (:kb-stranger w))})
       (access/can? (world/principal w :member) :write (:kb-stranger-scope w))))))

(deftest sharing-a-kb-is-the-owners
  (world/with-world [_ w]
    (testing "legitimate: the owner shares their KB with the member"
      (is (= {:status :ok}
             (:result (world/inbound! (world/principal w :owner) "share-kb!"
                                      {:kb-id-str (str (:kb-private w))
                                       :party-id-str (str (:member w))}))))
      (is (true? (access/can? (world/principal w :member) :read (:kb-private-scope w)))))
    (testing "the member's room grant on the Handbook is :read-write: write, not merge"
      (is (true? (access/can? (world/principal w :member) :write (:kb-granted-scope w))))
      (is (false? (access/can? (world/principal w :member) :merge (:kb-granted-scope w)))))
    (known-hole
     "UNLISTED share-kb! escalates a room grant"
     "a :read-write grantee shares the KB with themselves; :kb/shared-with is owner-equivalent, so they can now MERGE"
     (world/inbound! (world/principal w :member) "share-kb!"
                     {:kb-id-str (str (:kb-granted w)) :party-id-str (str (:member w))})
     (access/can? (world/principal w :member) :merge (:kb-granted-scope w)))))

(deftest deleting-a-kb-is-the-owners
  (world/with-world [_ w]
    (testing "a non-member is refused and the KB survives"
      (is (world/refused? (world/inbound! (world/principal w :non-member) "delete-kb!"
                                          {:kb-id-str (str (:kb-granted w))})))
      (is (some? (d/q '[:find ?e . :in $ ?k :where [?e :kb/id ?k]]
                      @(is.simm.model.system-db/get-conn) (:kb-granted w)))))
    (testing "legitimate: the owner deletes their own KB"
      (world/inbound! (world/principal w :owner) "delete-kb!" {:kb-id-str (str (:kb-private w))})
      (is (nil? (d/q '[:find ?e . :in $ ?k :where [?e :kb/id ?k]]
                     @(is.simm.model.system-db/get-conn) (:kb-private w)))))
    (known-hole
     "UNLISTED delete-kb! is a :write"
     "a member of a room holding a :read-write grant deletes the KB itself"
     (world/inbound! (world/principal w :member) "delete-kb!" {:kb-id-str (str (:kb-granted w))})
     (nil? (d/q '[:find ?e . :in $ ?k :where [?e :kb/id ?k]]
                @(is.simm.model.system-db/get-conn) (:kb-granted w))))))

(deftest adding-a-party-to-a-room
  (world/with-world [_ w]
    (testing "legitimate: the owner adds a human to their room"
      (world/inbound! (world/principal w :owner) "add-room-party!"
                      {:room-id-str (str (:room w)) :party-id-str (str (:admin w))})
      (is (contains? (room-party-ids (:room w)) (:admin w))))
    (testing "a non-member cannot add themselves"
      (is (world/refused? (world/inbound! (world/principal w :non-member) "add-room-party!"
                                          {:room-id-str (str (:room w))
                                           :party-id-str (str (:non-member w))})))
      (is (not (contains? (room-party-ids (:room w)) (:non-member w)))))
    (known-hole
     "UNLISTED add-room-party! takes another party's agent"
     "anyone who may add parties adds ANOTHER party's agent (here the stranger's), which then answers in this room on its owner's model and budget"
     (world/inbound! (world/principal w :owner) "add-room-party!"
                     {:room-id-str (str (:room w)) :party-id-str (str (:stranger-agent w))})
     (contains? (room-party-ids (:room w)) (:stranger-agent w)))
    (known-hole
     "UNLISTED add-room-party! is a :write"
     "a plain member (not the owner) changes the room's membership, and with it who reaches every KB the room is granted"
     (world/inbound! (world/principal w :member) "add-room-party!"
                     {:room-id-str (str (:room w)) :party-id-str (str (:non-member w))})
     (and (contains? (room-party-ids (:room w)) (:non-member w))
          (access/can? (world/principal w :non-member) :write (:kb-granted-scope w))))))

(deftest raising-the-room-budget
  (world/with-world [_ w]
    (testing "legitimate: the owner sets the budget"
      (world/inbound! (world/principal w :owner) "update-room-budget!"
                      {:room-id-str (str (:room w)) :budget-dollars 20})
      (is (= 20.0 (rooms/get-room-budget-dollars (:room w)))))
    (testing "a non-member is refused and the budget stands"
      (is (world/refused? (world/inbound! (world/principal w :non-member) "update-room-budget!"
                                          {:room-id-str (str (:room w)) :budget-dollars 999})))
      (is (= 20.0 (rooms/get-room-budget-dollars (:room w)))))
    (known-hole
     "UNLISTED update-room-budget! is a :write"
     "a plain member raises the room's spend cap — egress authorized by someone who does not pay"
     (world/inbound! (world/principal w :member) "update-room-budget!"
                     {:room-id-str (str (:room w)) :budget-dollars 5000})
     (= 5000.0 (rooms/get-room-budget-dollars (:room w))))))
