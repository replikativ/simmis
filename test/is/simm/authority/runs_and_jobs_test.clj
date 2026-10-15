(ns is.simm.authority.runs-and-jobs-test
  "Runs (decision §2.9.2): SEEING a run is reading its room; STOPPING one is
   for whoever started it, the room owner, or `:admin` (from inside the room).

   The runs here are real dvergr live Runs (`agent-run/start!`) in rooms with an
   in-memory dvergr store holding their trigger messages, and cancellation is
   observed on dvergr's own flag (`cancel-requested?`). Only the room → live
   Room lookup is redefined, since the world's rooms are not hydrated.

   A Run records the agent that runs and its trigger message, not who caused
   it; `runs/run-starter` derives the starter, and its cases are the first
   test below."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [dvergr.agent.run :as agent-run]
            [dvergr.discourse :as disc]
            [dvergr.room.store :as rstore]
            [dvergr.room.store.memory :as memory]
            [is.simm.agents.room-agents :as ra]
            [is.simm.agents.runs :as runs]
            [is.simm.authority.world :as world]
            [is.simm.model.access :as access]
            [is.simm.uis.web.desktop.chat-remote]))

(defn- actor [w kind] (keyword "party" (str (get w kind))))

(defn- live-room [id] {:id id :store (memory/make) :meta (atom {})})

(defn- start-run!
  "Start a live Run in live Room `room`, by `by`:
     a principal kind (:member, :owner, …) — triggered by that party's message
     :scheduler                           — triggered by the scheduler's message
     [:delegated kind]                    — an agent's message on behalf of `kind`
     [:agent kind]                        — the agent party `kind`'s own message
     [:job kind-or-nil]                   — a job `kind` started (nil: the process)
     :untracked                           — `register-live!`, no trigger in the store
   Returns the Run map."
  [w room by]
  (let [agent-actor (actor w :team-agent)
        post! (fn [from metadata]
                (let [msg (disc/message from agent-actor "go" nil metadata)]
                  (rstore/-store-message! (:store room) (rstore/conversation-id room) msg)
                  msg))]
    (cond
      (= :untracked by)
      (let [rid (agent-run/register-live! (:id room) agent-actor nil)]
        (some #(when (= rid (:run/id %)) %) (agent-run/active-runs (:id room))))

      (= :scheduler by)
      (agent-run/start! room agent-actor (post! :scheduler {:source :scheduler}) nil)

      (vector? by)
      (let [[how kind] by]
        (case how
          :delegated (agent-run/start! room agent-actor
                                       (post! (actor w :stranger-agent) {:from (actor w kind)}) nil)
          :agent (agent-run/start! room agent-actor (post! (actor w kind) nil) nil)
          :job (agent-run/start! room (if kind (actor w kind) :dvergr) (random-uuid) nil
                                 {:kind :workflow})))

      :else
      (agent-run/start! room agent-actor (post! (actor w by) nil) nil))))

(defn- with-live-rooms
  "`(f start!)` with the world's team and other room live as dvergr rooms;
   `(start! room by)` begins a live Run in `:team`/`:other` (see `start-run!`)
   and returns its id. Every Run started is finished afterwards."
  [w f]
  (let [team (live-room (keyword "authority-matrix" (str "team-" (:room w))))
        other (live-room (keyword "authority-matrix" (str "other-" (:other-room w))))
        started (atom [])]
    (with-redefs [ra/live-room (fn [rid] (condp = rid
                                           (:room w) team
                                           (:other-room w) other
                                           nil))]
      (try
        (f (fn [room by]
             (let [rid (:run/id (start-run! w (if (= room :team) team other) by))]
               (swap! started conj rid)
               rid)))
        (finally
          (doseq [rid @started]
            (agent-run/finish! rid (if (agent-run/cancel-requested? rid) :cancelled :completed))))))))

(deftest who-started-a-run
  (world/with-world [_ w]
    (let [room (live-room (keyword "authority-matrix" (str "starter-" (random-uuid))))
          starter (fn [by]
                    (let [r (start-run! w room by)]
                      (try (runs/run-starter room r)
                           (finally (agent-run/finish! (:run/id r) :completed)))))]
      (is (= (:member w) (starter :member)) "a message: its author")
      (is (= (:owner w) (starter :owner)))
      (is (= (:owner w) (starter [:delegated :owner]))
          "delegated work: the message's logical initiator, not the agent that relayed it")
      (is (= (:stranger-agent w) (starter [:agent :stranger-agent]))
          "an agent's message: that agent party")
      (is (nil? (starter :scheduler)) "a schedule: dvergr records no owner")
      (is (= (:member w) (starter [:job :member])) "a job: the party that started it")
      (is (nil? (starter [:job nil])) "a job of the process")
      (is (nil? (starter :untracked)) "a Run whose trigger is in no store"))))

(defn- cancel! [w kind room run-id]
  (world/inbound! (world/principal w kind) "cancel-room-run!"
                  {:room-id-str (str (get w room)) :run-id-str (str run-id)}))

(defn- can-cancel? [w kind run-id]
  (get-in (world/inbound! (world/principal w kind) "can-cancel-room-run!"
                          {:room-id-str (str (:room w)) :run-id-str (str run-id)})
          [:result :can-cancel?]))

(deftest seeing-a-run-is-reading-its-room
  (world/with-world [_ w]
    (with-live-rooms w
      (fn [start!]
        (let [run (start! :team :member)
              see (fn [kind] (world/inbound! (world/principal w kind) "run-world-live!"
                                             {:room-id-str (str (:room w)) :run-id-str (str run)}))]
          (is (= (str run) (get-in (see :member) [:result :run-id])) "a member sees it")
          (is (= (str run) (get-in (see :owner) [:result :run-id])) "the owner sees it")
          (is (world/refused? (see :non-member)) "a non-member does not")
          (is (world/refused? (see :anon)) "nor does anonymous"))))))

(deftest cancelling-a-run
  (world/with-world [conn w]
    (with-live-rooms w
      (fn [start!]
        (testing "legitimate: the room owner cancels a run a member started"
          (let [run (start! :team :member)]
            (is (true? (can-cancel? w :owner run)))
            (is (= :cancelling (get-in (cancel! w :owner :room run) [:result :status])))
            (is (true? (agent-run/cancel-requested? run)))))

        (testing "legitimate: a member cancels the run they started"
          (let [run (start! :team :member)]
            (is (true? (can-cancel? w :member run)))
            (is (= :cancelling (get-in (cancel! w :member :room run) [:result :status])))
            (is (true? (agent-run/cancel-requested? run)))))

        (testing "§2.9.2 closed: a member does not cancel a run someone else started"
          (let [run (start! :team :owner)]
            (is (false? (can-cancel? w :member run)) "and is not offered the control")
            (is (world/refused? (cancel! w :member :room run)))
            (is (false? (agent-run/cancel-requested? run)) "the run keeps running")
            (is (= :cancelling (get-in (cancel! w :owner :room run) [:result :status]))
                "while its starter stops it")))

        (testing "a run nobody determinable started (a schedule) is the owner's"
          (let [run (start! :team :scheduler)]
            (is (false? (can-cancel? w :member run)))
            (is (world/refused? (cancel! w :member :room run)))
            (is (false? (agent-run/cancel-requested? run)))
            (is (true? (can-cancel? w :owner run)))
            (is (= :cancelling (get-in (cancel! w :owner :room run) [:result :status])))))

        (testing "a non-member is refused at the gate; the run keeps running"
          (let [run (start! :team :member)]
            (is (world/refused? (cancel! w :non-member :room run)))
            (is (world/refused? (cancel! w :admin :room run))
                "nor does :admin reach into a room it is not in")
            (is (false? (agent-run/cancel-requested? run)))))

        (testing "naming one's OWN room does not reach a run in another"
          ;; the stranger passes the gate on the room they own; the handler
          ;; binds the opaque run id to that room and finds nothing
          (let [run (start! :team :member)]
            (is (= :not-active (get-in (cancel! w :non-member :other-room run) [:result :status])))
            (is (false? (agent-run/cancel-requested? run)))))

        (testing ":admin, once in the room, stops a run it did not start"
          (d/transact conn [{:room/id (:room w) :room/parties [[:party/id (:admin w)]]}])
          (let [run (start! :team :owner)]
            (is (true? (can-cancel? w :admin run)))
            (is (= :cancelling (get-in (cancel! w :admin :room run) [:result :status])))))

        (testing "a run that is not live offers no stop control"
          (is (false? (can-cancel? w :owner (random-uuid)))))))))

(deftest the-cancel-rule
  (world/with-world [conn w]
    (let [db @conn
          may? (fn [kind starter] (access/may-cancel-run? db (world/principal w kind) (:room w) starter))]
      (is (true? (may? :member (:member w))) "its starter")
      (is (false? (may? :member (:owner w))) "not another member's")
      (is (false? (may? :member nil)) "not one nobody started")
      (is (true? (may? :owner (:member w))) "the room owner, any run")
      (is (true? (may? :owner nil)))
      (is (false? (may? :non-member (:non-member w))) "never from outside the room, even as its starter")
      (is (false? (may? :admin nil)) "an admin outside the room")
      (is (false? (may? :anon nil))))))
