(ns is.simm.authority.handler-holes-test
  "The handler holes this suite found that the P1 plan does not list, each
   EXECUTED end to end through the network-inbound path (`world/inbound!`: the
   real gate, the real handler, the real model functions over the world's
   system DB).

   They shared one shape: the policy row policed ONE id (the room, the KB) and
   the handler acted on a SECOND id the client supplied, unchecked — or the row
   policed an owner-level act as a plain :write. All are closed now (P1 fix 1);
   each test asserts that the attack is refused AND has no effect, next to the
   legitimate use of the same path."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [is.simm.agents.room-agents :as ra]
            [is.simm.authority.world :as world]
            [is.simm.model.access :as access]
            [is.simm.model.drives :as drives]
            [is.simm.model.knowledge-bases :as kbs]
            [is.simm.model.user-rooms-broadcast :as urb]
            [is.simm.model.model-catalog :as model-catalog]
            [is.simm.model.parties :as parties]
            [is.simm.model.rooms :as rooms]
            [is.simm.model.system-db :as system-db]
            [is.simm.uis.web.desktop.branching-remote]
            [is.simm.uis.web.desktop.chat-remote]))

(defn- room-party-ids [room-id]
  (set (:room/parties (rooms/get-room room-id))))

(defn- ok? [resp] (= {:status :ok} (:result resp)))

(defn- refused-by-handler?
  "The handler ran and refused: an error carrying `:type :not-authorized`."
  [resp]
  (boolean (some->> (:error resp) (re-find #":type :not-authorized"))))

(defn- kb-exists? [kb-id]
  (some? (d/q '[:find ?e . :in $ ?k :where [?e :kb/id ?k]] @(system-db/get-conn) kb-id)))

;; =============================================================================
;; Agents in a room
;; =============================================================================

(deftest remove-agent-from-room-removes-only-the-rooms-agents
  ;; The handler retracts `agent-id-str` from the room, then DELETES the party
  ;; when it is in no room any more. It now first requires an AGENT that IS in
  ;; this room, removable by the caller (`access/agent-manageable?`).
  (world/with-world [_ w]
    (with-redefs [ra/unassign-room-agent! (fn [& _])]
      (testing "a non-member is refused at the gate"
        (is (world/refused? (world/inbound! (world/principal w :non-member) "remove-agent-from-room!"
                                            {:room-id-str (str (:room w))
                                             :agent-id-str (str (:admin w))})))
        (is (some? (parties/get-party (:admin w)))))
      (testing "a member names a HUMAN who is in no room (the admin): refused, the account survives"
        (is (refused-by-handler?
             (world/inbound! (world/principal w :member) "remove-agent-from-room!"
                             {:room-id-str (str (:room w)) :agent-id-str (str (:admin w))})))
        (is (some? (parties/get-party (:admin w)))))
      (testing "a member names a human who IS in the room (the owner): refused"
        (is (refused-by-handler?
             (world/inbound! (world/principal w :member) "remove-agent-from-room!"
                             {:room-id-str (str (:room w)) :agent-id-str (str (:owner w))})))
        (is (contains? (room-party-ids (:room w)) (:owner w)))
        (is (some? (parties/get-party (:owner w)))))
      (testing "the owner names an agent that is NOT in the room (the stranger's): refused, it survives"
        (is (refused-by-handler?
             (world/inbound! (world/principal w :owner) "remove-agent-from-room!"
                             {:room-id-str (str (:room w)) :agent-id-str (str (:stranger-agent w))})))
        (is (some? (parties/get-party (:stranger-agent w)))))
      (testing "a member removes the OWNER's agent: refused, it stays in the room"
        (is (refused-by-handler?
             (world/inbound! (world/principal w :member) "remove-agent-from-room!"
                             {:room-id-str (str (:room w)) :agent-id-str (str (:team-agent w))})))
        (is (contains? (room-party-ids (:room w)) (:team-agent w))))
      (testing "legitimate: the owner removes the team's (their own) agent; it is gone"
        (is (ok? (world/inbound! (world/principal w :owner) "remove-agent-from-room!"
                                 {:room-id-str (str (:room w))
                                  :agent-id-str (str (:team-agent w))})))
        (is (not (contains? (room-party-ids (:room w)) (:team-agent w))))
        (is (nil? (parties/get-party (:team-agent w))))))))

(deftest a-members-own-agent
  ;; add-agent-to-room! used to take the agent's OWNER from the client: a
  ;; member could mint an agent owned by — answering on the budget of — the
  ;; room owner. The owner is the principal now.
  (world/with-world [_ w]
    (with-redefs [ra/unassign-room-agent! (fn [& _])
                  ra/assign-room-agent! (fn [& _])]
      (let [resp (world/inbound! (world/principal w :member) "add-agent-to-room!"
                                 {:room-id-str (str (:room w))
                                  ;; naming the OWNER as the agent's owner
                                  :owner-id-str (str (:owner w))
                                  :agent-name "Member's Agent" :template-id-str ""})
            agent-id (some-> resp :result :agent-id parse-uuid)]
        (testing "the agent is created in the room, owned by the member who asked"
          (is (some? agent-id) (pr-str resp))
          (is (contains? (room-party-ids (:room w)) agent-id))
          (is (= (:member w) (get-in (parties/get-party agent-id) [:party/owner :party/id]))))
        (testing "the room owner may take another party's agent out of THEIR room, but not delete it"
          (is (ok? (world/inbound! (world/principal w :owner) "remove-agent-from-room!"
                                   {:room-id-str (str (:room w)) :agent-id-str (str agent-id)})))
          (is (not (contains? (room-party-ids (:room w)) agent-id)))
          (is (some? (parties/get-party agent-id))))
        (testing "a member removes their own agent, which (in no room) is then deleted"
          (rooms/add-party! (:room w) agent-id)
          (is (ok? (world/inbound! (world/principal w :member) "remove-agent-from-room!"
                                   {:room-id-str (str (:room w)) :agent-id-str (str agent-id)})))
          (is (nil? (parties/get-party agent-id))))))))

;; =============================================================================
;; Membership
;; =============================================================================

(deftest adding-a-party-to-a-room
  (world/with-world [_ w]
    (testing "legitimate: the owner adds a human to their room"
      (is (ok? (world/inbound! (world/principal w :owner) "add-room-party!"
                               {:room-id-str (str (:room w)) :party-id-str (str (:admin w))})))
      (is (contains? (room-party-ids (:room w)) (:admin w))))
    (testing "legitimate: the owner adds their own agent"
      (rooms/remove-party! (:room w) (:team-agent w))
      (is (ok? (world/inbound! (world/principal w :owner) "add-room-party!"
                               {:room-id-str (str (:room w)) :party-id-str (str (:team-agent w))})))
      (is (contains? (room-party-ids (:room w)) (:team-agent w))))
    (testing "a non-member cannot add themselves"
      (is (world/refused? (world/inbound! (world/principal w :non-member) "add-room-party!"
                                          {:room-id-str (str (:room w))
                                           :party-id-str (str (:non-member w))})))
      (is (not (contains? (room-party-ids (:room w)) (:non-member w)))))
    (testing "the owner cannot add ANOTHER party's agent (it would answer on its owner's model and budget)"
      (is (refused-by-handler?
           (world/inbound! (world/principal w :owner) "add-room-party!"
                           {:room-id-str (str (:room w)) :party-id-str (str (:stranger-agent w))})))
      (is (not (contains? (room-party-ids (:room w)) (:stranger-agent w)))))
    (testing "a plain member cannot change the membership (and so who reaches the room's KBs)"
      (is (world/refused? (world/inbound! (world/principal w :member) "add-room-party!"
                                          {:room-id-str (str (:room w))
                                           :party-id-str (str (:non-member w))})))
      (is (not (contains? (room-party-ids (:room w)) (:non-member w))))
      (is (false? (access/can? (world/principal w :non-member) :write (:kb-granted-scope w)))))))

(deftest removing-a-party-from-a-room
  (world/with-world [_ w]
    (testing "a plain member cannot remove another member"
      (is (world/refused? (world/inbound! (world/principal w :member) "remove-room-party!"
                                          {:room-id-str (str (:room w))
                                           :party-id-str (str (:owner w))})))
      (is (contains? (room-party-ids (:room w)) (:owner w))))
    (testing "legitimate: the owner removes a member"
      (is (ok? (world/inbound! (world/principal w :owner) "remove-room-party!"
                               {:room-id-str (str (:room w)) :party-id-str (str (:member w))})))
      (is (not (contains? (room-party-ids (:room w)) (:member w)))))))

(deftest creating-a-room-with-members
  ;; create-room! provisions a dvergr room; what matters here is which member
  ;; ids reach it, so the model call is recorded instead of run.
  (world/with-world [_ w]
    (let [calls (atom [])]
      (with-redefs [rooms/create-room! (fn [creator nm kind members]
                                         (swap! calls conj {:creator creator :members (set members)})
                                         {:room/id (random-uuid) :room/name nm :room/type kind})]
        (testing "another party's agent cannot be an initial member: refused, nothing is created"
          (is (refused-by-handler?
               (world/inbound! (world/principal w :member) "create-room!"
                               {:creator-id-str (str (:member w)) :room-name "Mine"
                                :member-id-strs [(str (:owner w)) (str (:stranger-agent w))]})))
          (is (empty? @calls)))
        (testing "legitimate: humans as initial members (the new-room picker's flow)"
          (is (some? (:result (world/inbound! (world/principal w :member) "create-room!"
                                              {:creator-id-str (str (:member w)) :room-name "Mine"
                                               :member-id-strs [(str (:owner w)) (str (:admin w))]}))))
          (is (= [{:creator (:member w) :members #{(:owner w) (:admin w)}}] @calls)))
        (testing "legitimate: the creator's own agent"
          (reset! calls [])
          (is (some? (:result (world/inbound! (world/principal w :owner) "create-room!"
                                              {:creator-id-str (str (:owner w)) :room-name "Ours"
                                               :member-id-strs [(str (:team-agent w))]}))))
          (is (= 1 (count @calls))))))))

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
    (testing "a plain member is refused — spend is authorized by the owner, not raised by whoever can speak"
      (is (world/refused? (world/inbound! (world/principal w :member) "update-room-budget!"
                                          {:room-id-str (str (:room w)) :budget-dollars 5000})))
      (is (= 20.0 (rooms/get-room-budget-dollars (:room w)))))))

;; =============================================================================
;; Attaching KBs and drives
;; =============================================================================

(deftest attach-kb-to-room-requires-authority-over-the-kb
  (world/with-world [_ w]
    (with-redefs [ra/reset-room-context! (fn [_])]
      (testing "legitimate: the owner attaches their own KB to their room"
        (is (ok? (world/inbound! (world/principal w :owner) "attach-kb-to-room!"
                                 {:room-id-str (str (:room w))
                                  :kb-id-str (str (:kb-private w))})))
        (is (true? (access/can? (world/principal w :member) :write (:kb-private-scope w)))
            "and the room's members now reach it — what attaching is for"))
      (testing "legitimate: the stranger attaches their own KB to their own room"
        (is (ok? (world/inbound! (world/principal w :non-member) "attach-kb-to-room!"
                                 {:room-id-str (str (:other-room w))
                                  :kb-id-str (str (:kb-stranger w))}))))
      (testing "a member attaches ANOTHER party's KB to a room they are in: refused, no access gained"
        (is (refused-by-handler?
             (world/inbound! (world/principal w :member) "attach-kb-to-room!"
                             {:room-id-str (str (:room w)) :kb-id-str (str (:kb-stranger w))})))
        (is (false? (access/can? (world/principal w :member) :read (:kb-stranger-scope w))))))))

(deftest attach-drive-to-room-requires-authority-over-the-drive
  (world/with-world [_ w]
    (with-redefs [ra/reset-room-context! (fn [_])]
      (testing "before: nobody but its owner reaches either drive"
        (is (false? (access/can? (world/principal w :member) :read {:drive (:drive-owner w)})))
        (is (false? (access/can? (world/principal w :member) :read {:drive (:drive-stranger w)}))))
      (testing "legitimate: the owner attaches their drive; the room's members reach it"
        (is (ok? (world/inbound! (world/principal w :owner) "attach-drive-to-room!"
                                 {:room-id-str (str (:room w))
                                  :drive-id-str (str (:drive-owner w))})))
        (is (true? (access/can? (world/principal w :member) :write {:drive (:drive-owner w)}))))
      (testing "a member attaches the STRANGER's drive: refused, no access gained"
        (is (refused-by-handler?
             (world/inbound! (world/principal w :member) "attach-drive-to-room!"
                             {:room-id-str (str (:room w))
                              :drive-id-str (str (:drive-stranger w))})))
        (is (false? (access/can? (world/principal w :member) :read {:drive (:drive-stranger w)}))))
      (testing "the admin role may grant a drive; a member of the room may not grant the owner's"
        (is (true? (access/can? (world/principal w :admin) :grant {:drive (:drive-stranger w)})))
        (is (false? (access/can? (world/principal w :member) :grant {:drive (:drive-owner w)})))))))

;; =============================================================================
;; KB ownership: share, unshare, delete
;; =============================================================================

(deftest sharing-a-kb-is-the-owners
  (world/with-world [_ w]
    (testing "legitimate: the owner shares their KB with the member"
      (is (ok? (world/inbound! (world/principal w :owner) "share-kb!"
                               {:kb-id-str (str (:kb-private w))
                                :party-id-str (str (:member w))})))
      (is (true? (access/can? (world/principal w :member) :read (:kb-private-scope w)))))
    (testing "a share confers :read-write — write, but neither merge nor grant"
      (is (true? (access/can? (world/principal w :member) :write (:kb-private-scope w))))
      (is (false? (access/can? (world/principal w :member) :merge (:kb-private-scope w))))
      (is (false? (access/can? (world/principal w :member) :grant {:kb (:kb-private w)}))))
    (testing "a sharee cannot re-share, unshare or delete"
      (is (world/refused? (world/inbound! (world/principal w :member) "share-kb!"
                                          {:kb-id-str (str (:kb-private w))
                                           :party-id-str (str (:non-member w))})))
      (is (world/refused? (world/inbound! (world/principal w :member) "delete-kb!"
                                          {:kb-id-str (str (:kb-private w))})))
      (is (kb-exists? (:kb-private w))))
    (testing "a :read-write room grantee sharing the KB with themselves is refused, and stays below merge"
      (is (world/refused? (world/inbound! (world/principal w :member) "share-kb!"
                                          {:kb-id-str (str (:kb-granted w))
                                           :party-id-str (str (:member w))})))
      (is (false? (access/can? (world/principal w :member) :merge (:kb-granted-scope w)))))
    (testing "legitimate: the owner revokes the share"
      (is (ok? (world/inbound! (world/principal w :owner) "unshare-kb!"
                               {:kb-id-str (str (:kb-private w))
                                :party-id-str (str (:member w))})))
      (is (false? (access/can? (world/principal w :member) :read (:kb-private-scope w)))))))

(deftest deleting-a-kb-is-the-owners
  (world/with-world [_ w]
    (testing "a non-member is refused and the KB survives"
      (is (world/refused? (world/inbound! (world/principal w :non-member) "delete-kb!"
                                          {:kb-id-str (str (:kb-granted w))})))
      (is (kb-exists? (:kb-granted w))))
    (testing "a member of a room holding a :read-write grant is refused and the KB survives"
      (is (world/refused? (world/inbound! (world/principal w :member) "delete-kb!"
                                          {:kb-id-str (str (:kb-granted w))})))
      (is (kb-exists? (:kb-granted w))))
    (testing "legitimate: the owner deletes their own KB"
      (world/inbound! (world/principal w :owner) "delete-kb!" {:kb-id-str (str (:kb-private w))})
      (is (not (kb-exists? (:kb-private w)))))
    (testing "legitimate: the admin role deletes a KB it does not own"
      (world/inbound! (world/principal w :admin) "delete-kb!" {:kb-id-str (str (:kb-stranger w))})
      (is (not (kb-exists? (:kb-stranger w)))))))

;; =============================================================================
;; Room details: what a viewer is shown
;; =============================================================================

(deftest room-details-show-the-viewer-only-what-they-may-attach
  ;; The attach pickers used to list every member's KBs and drives, private
  ;; ones included — the ids the attach holes then accepted.
  (world/with-world [_ w]
    (with-redefs [ra/describe-model-resolution (fn [_] {})
                  model-catalog/choices (fn [] [])]
      (let [details (fn [kind]
                      (:result (world/inbound! (world/principal w kind) "load-room-details!"
                                               {:room-id-str (str (:room w))})))
            ids (fn [k idk d] (set (map idk (get d k))))
            member (details :member)
            owner (details :owner)]
        (testing "a member is not shown the owner's private KB or drive, and is not offered owner controls"
          (is (some? (:room member)))
          (is (not (contains? (ids :available-kbs :kb/id member) (:kb-private w))))
          (is (not (contains? (ids :available-drives :drive/id member) (:drive-owner w))))
          (is (false? (:can-grant? member))))
        (testing "a member still sees what the ROOM holds — the KBs they reach through it"
          (is (contains? (ids :knowledge-bases :kb/id member) (:kb-granted w)))
          (is (every? #(not (contains? % :kb/shared-with)) (:knowledge-bases member))))
        (testing "the owner is offered their own KBs and drives, and the owner controls"
          (is (contains? (ids :available-kbs :kb/id owner) (:kb-private w)))
          (is (contains? (ids :available-drives :drive/id owner) (:drive-owner w)))
          (is (true? (:can-grant? owner))))))))

;; =============================================================================
;; Proposal forks
;; =============================================================================

(defn- add-fork!
  "Put a fork of the Handbook on branch `branch-name`, written by `author`,
   into the world's open proposal."
  [conn w branch-name author]
  (d/transact conn [{:proposal/id (:proposal w)
                     :proposal/forks [{:proposal.fork/scope (:kb-granted-scope w)
                                       :proposal.fork/branch branch-name
                                       :proposal.fork/author author}]}]))

(deftest an-open-proposal-fork-is-its-authors-and-its-deciders
  ;; doc/proposals-and-time-travel.md §5: a fork is a contribution under review.
  ;; Writing to it or discarding it is its author's, or a decider's (:merge on
  ;; the store). Any other writer of the KB used to be able to do both.
  (world/with-world [conn w]
    (add-fork! conn w "proposal-agent" (:team-agent w))
    (add-fork! conn w "proposal-member" (:member w))
    (let [decide (fn [kind fn-name args]
                   (boolean (access/authorize-remote (world/principal w kind) fn-name args)))
          block (fn [branch] (cond-> {:db-scope (:kb-granted-scope w) :block-uuid (random-uuid)}
                               branch (assoc :branch-kw branch)))
          discard (fn [branch] {:db-scope-str (str (:kb-granted-scope w)) :branch-kw branch})]
      (testing "a member (a :read-write grantee) may not write or discard another party's open fork"
        (is (false? (decide :member "update-block-content" (block :proposal-agent))))
        (is (false? (decide :member "delete-block" (block :proposal-agent))))
        (is (false? (decide :member "discard-kb-branch!" (discard :proposal-agent)))))
      (testing "but reads it like the store"
        (is (true? (access/can? (world/principal w :member) :read
                                {:branch {:scope (:kb-granted-scope w) :branch :proposal-agent}}))))
      (testing "legitimate: the author writes and discards their own fork"
        (is (true? (decide :member "update-block-content" (block :proposal-member))))
        (is (true? (decide :member "discard-kb-branch!" (discard :proposal-member)))))
      (testing "legitimate: a decider (the owner, :merge on the KB) writes and discards any fork"
        (is (true? (decide :owner "update-block-content" (block :proposal-agent))))
        (is (true? (decide :owner "discard-kb-branch!" (discard :proposal-agent)))))
      (testing "legitimate: trunk and branches that are no proposal fork answer as the store does"
        (is (true? (decide :member "update-block-content" (block nil))))
        (is (true? (decide :member "update-block-content" (block :db))))
        (is (true? (decide :member "update-block-content" (block :my-scratch))))
        (is (true? (decide :member "discard-kb-branch!" (discard :my-scratch))))
        (is (false? (decide :non-member "update-block-content" (block :my-scratch)))))
      (testing "once the fork is decided its branch is an ordinary branch again"
        (let [f (d/q '[:find ?f . :in $ ?b :where [?f :proposal.fork/branch ?b]]
                     @conn "proposal-agent")]
          (d/transact conn [{:db/id f :proposal.fork/status :dismissed}]))
        (is (true? (decide :member "update-block-content" (block :proposal-agent))))))))

(defn- fork-status [conn branch]
  (d/q '[:find ?st . :in $ ?b
         :where [?f :proposal.fork/branch ?b] [(get-else $ ?f :proposal.fork/status :open) ?st]]
       @conn branch))

(deftest dismissing-a-fork-is-its-authors-and-its-deciders
  ;; dismiss-fork! / dismiss-proposal! delete fork branches. They used to need
  ;; only room :write, so any member could throw away another party's
  ;; contribution while it was under review. The world's proposal holds
  ;; "member-fork" (the member's) and "agent-fork" (the owner's agent's).
  (world/with-world [conn w]
    (let [fork-args (fn [branch] {:proposal-id-str (str (:proposal w)) :note ""
                                  :scope-str (str (:kb-granted-scope w)) :branch branch})
          decide (fn [kind fn-name args]
                   (boolean (access/authorize-remote (world/principal w kind) fn-name args)))]
      (testing "a member dismissing the AGENT's fork is refused, and the fork stays open"
        (is (world/refused? (world/inbound! (world/principal w :member) "dismiss-fork!"
                                            (fork-args "agent-fork"))))
        (is (= :open (fork-status conn "agent-fork"))))
      (testing "a member dismissing the whole proposal (which holds a fork not theirs) is refused"
        (is (world/refused? (world/inbound! (world/principal w :member) "dismiss-proposal!"
                                            {:proposal-id-str (str (:proposal w)) :note ""})))
        (is (= :open (fork-status conn "agent-fork")))
        (is (= :open (fork-status conn "member-fork"))))
      (testing "a non-member is refused even naming a fork of their own scope"
        (is (false? (decide :non-member "dismiss-fork!" (fork-args "member-fork")))))
      (testing "a fork that is not in THIS proposal is refused"
        (is (false? (decide :owner "dismiss-fork!" (fork-args "no-such-fork")))))
      (testing "legitimate: the author withdraws their own fork; a decider refuses any"
        (is (true? (decide :member "dismiss-fork!" (fork-args "member-fork"))))
        (is (true? (decide :owner "dismiss-fork!" (fork-args "agent-fork"))))
        (is (true? (decide :owner "dismiss-proposal!" {:proposal-id-str (str (:proposal w))}))))
      (testing "legitimate: the proposal's filer withdraws the whole proposal"
        (d/transact conn [{:proposal/id (:proposal w) :proposal/author (:member w)}])
        (is (true? (decide :member "dismiss-proposal!" {:proposal-id-str (str (:proposal w))}))))
      (testing "once the agent's fork is decided, the member controls every OPEN fork"
        (d/transact conn [{:proposal/id (:proposal w) :proposal/author (:owner w)}])
        (let [f (d/q '[:find ?f . :where [?f :proposal.fork/branch "agent-fork"]] @conn)]
          (d/transact conn [{:db/id f :proposal.fork/status :dismissed}]))
        (is (true? (decide :member "dismiss-proposal!" {:proposal-id-str (str (:proposal w))})))))))

(deftest detaching-is-the-room-owners-or-the-resource-owners
  (world/with-world [_ w]
    (with-redefs [ra/reset-room-context! (fn [_])]
      (kbs/attach-kb-to-room! (:room w) (:kb-private w))
      (drives/attach-drive-to-room! (:room w) (:drive-owner w))
      (testing "a plain member cannot take the room's KB or drive away"
        (is (world/refused? (world/inbound! (world/principal w :member) "detach-kb-from-room!"
                                            {:room-id-str (str (:room w))
                                             :kb-id-str (str (:kb-granted w))})))
        (is (world/refused? (world/inbound! (world/principal w :member) "detach-drive-from-room!"
                                            {:room-id-str (str (:room w))
                                             :drive-id-str (str (:drive-owner w))})))
        (is (true? (access/can? (world/principal w :member) :write (:kb-granted-scope w))))
        (is (true? (access/can? (world/principal w :member) :write {:drive (:drive-owner w)}))))
      (testing "legitimate: the room's owner detaches a KB and a drive"
        (is (ok? (world/inbound! (world/principal w :owner) "detach-kb-from-room!"
                                 {:room-id-str (str (:room w)) :kb-id-str (str (:kb-private w))})))
        (is (false? (access/can? (world/principal w :member) :read (:kb-private-scope w))))
        (is (ok? (world/inbound! (world/principal w :owner) "detach-drive-from-room!"
                                 {:room-id-str (str (:room w)) :drive-id-str (str (:drive-owner w))})))
        (is (false? (access/can? (world/principal w :member) :read {:drive (:drive-owner w)}))))
      (testing "legitimate: a resource's owner withdraws it from a room they are not in"
        (kbs/attach-kb-to-room! (:other-room w) (:kb-private w))
        (drives/attach-drive-to-room! (:other-room w) (:drive-owner w))
        (is (true? (access/can? (world/principal w :non-member) :read (:kb-private-scope w))))
        (is (ok? (world/inbound! (world/principal w :owner) "detach-kb-from-room!"
                                 {:room-id-str (str (:other-room w)) :kb-id-str (str (:kb-private w))})))
        (is (ok? (world/inbound! (world/principal w :owner) "detach-drive-from-room!"
                                 {:room-id-str (str (:other-room w)) :drive-id-str (str (:drive-owner w))})))
        (is (false? (access/can? (world/principal w :non-member) :read (:kb-private-scope w))))
        (is (false? (access/can? (world/principal w :non-member) :read {:drive (:drive-owner w)})))))))

(deftest changing-an-agents-assignment
  ;; update-agent-assignment! took any agent id: a member could re-role the
  ;; owner's agent, or write an assignment for an agent outside the room.
  (world/with-world [conn w]
    (d/transact conn [{:room/id (:room w) :room/slug "team-test"}])
    (let [assigned (atom [])]
      (with-redefs [ra/assign-room-agent! (fn [slug actor opts]
                                            (swap! assigned conj [slug actor opts])
                                            {:assignment/id (random-uuid)})
                    urb/notify-parties! (fn [& _])]
        (let [call (fn [kind agent]
                     (world/inbound! (world/principal w kind) "update-agent-assignment!"
                                     {:room-id-str (str (:room w)) :agent-id-str (str (agent w))
                                      :role-kw :lead :response-policy-kw :always}))]
          (testing "a member changing the OWNER's agent is refused, nothing is assigned"
            (is (refused-by-handler? (call :member :team-agent)))
            (is (empty? @assigned)))
          (testing "naming an agent that is not in the room is refused"
            (is (refused-by-handler? (call :owner :stranger-agent)))
            (is (empty? @assigned)))
          (testing "naming a human is refused"
            (is (refused-by-handler? (call :owner :member)))
            (is (empty? @assigned)))
          (testing "legitimate: the room owner (and agent owner) changes it"
            (is (= :ok (:status (:result (call :owner :team-agent)))))
            (is (= 1 (count @assigned)))))))))

(deftest a-rooms-drive-answers-to-the-rooms-owner
  ;; load-room-drive! used to provision the room's drive OWNED BY whoever opened
  ;; the Files tab first — a plain member then held owner authority over the
  ;; room's files.
  (world/with-world [conn w]
    (with-redefs [drives/create-drive-database! (fn [_])
                  drives/connect-drive-database (fn [_] nil)
                  ra/reset-room-context! (fn [_])]
      (testing "a member opening the Files tab first provisions a drive the ROOM'S OWNER owns"
        (world/inbound! (world/principal w :member) "load-room-drive!"
                        {:room-id-str (str (:room w)) :cut-ms nil})
        (let [drive (first (drives/get-room-drives (:room w)))]
          (is (some? drive))
          (is (= (:owner w) (:drive/owner drive)))
          (is (false? (access/can? (world/principal w :member) :grant {:drive (:drive/id drive)})))
          (testing "and the member still uses it through the room"
            (is (true? (access/can? (world/principal w :member) :write {:drive (:drive/id drive)}))))))
      (testing "a drive provisioned the OLD way (owned by the member) answers to the room's owner"
        (let [legacy (random-uuid) sys (random-uuid)]
          (d/transact conn [{:db/id "s" :system/id sys :system/type :fs :system/name "Scratch Drive"}
                            {:drive/id legacy :drive/name "Scratch Drive" :drive/owner (:member w)
                             :drive/system-id sys :drive/created (java.util.Date.)}])
          (drives/attach-drive-to-room! (:room-nogrant w) legacy)
          (is (false? (access/can? (world/principal w :member) :grant {:drive legacy})))
          (is (true? (access/can? (world/principal w :owner) :grant {:drive legacy})))
          (is (true? (access/can? (world/principal w :member) :write {:drive legacy}))
              "the member keeps what the room grants")
          (testing "so the member cannot carry the room's files into a room of their own"
            (is (refused-by-handler?
                 (world/inbound! (world/principal w :member) "attach-drive-to-room!"
                                 {:room-id-str (str (:room w)) :drive-id-str (str legacy)}))))
          (testing "a drive the member created themselves stays theirs"
            (is (true? (access/can? (world/principal w :non-member) :grant
                                    {:drive (:drive-stranger w)})))))))))
