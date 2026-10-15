(ns is.simm.authority.runs-and-jobs-test
  "Runs (decision §2.9.2): SEEING a run is reading its room; CANCELLING one is
   for the principal who started it, or the room owner.

   The runs here are real dvergr live Runs (`agent-run/register-live!`, with
   no chat context and no store), and
   cancellation is observed on dvergr's own flag (`cancel-requested?`). Only
   the room → live Room lookup is redefined, since the world's rooms are not
   hydrated.

   NOTE FOR THE FIX: a Run records `:run/actor` (the agent that runs) and
   `:run/trigger` (a message id), but not the principal who caused it, so
   \"the principal who started it\" is not yet answerable from the Run. The
   fix has to record it (or resolve it through the trigger's author)."
  (:require [clojure.test :refer [deftest is testing]]
            [dvergr.agent.run :as agent-run]
            [is.simm.agents.room-agents :as ra]
            [is.simm.authority.known-hole :refer [known-hole]]
            [is.simm.authority.world :as world]
            [is.simm.uis.web.desktop.chat-remote]))

(defn- with-live-rooms
  "`(f start!)` with the world's team and other room live as dvergr rooms
   `team-id`/`other-id`; `start!` begins a live Run in one of them, and every
   Run started is torn down afterwards."
  [w f]
  (let [team-id (keyword "authority-matrix" (str "team-" (:room w)))
        other-id (keyword "authority-matrix" (str "other-" (:other-room w)))
        started (atom [])]
    (with-redefs [ra/live-room (fn [rid] (condp = rid
                                           (:room w) {:id team-id}
                                           (:other-room w) {:id other-id}
                                           nil))]
      (try
        (f (fn [room]
             (let [rid (agent-run/register-live! (if (= room :team) team-id other-id)
                                                 :party/team-agent nil)]
               (swap! started conj rid)
               rid)))
        (finally
          (doseq [rid @started]
            (agent-run/finish! rid (if (agent-run/cancel-requested? rid) :cancelled :completed))))))))

(defn- cancel! [w kind room run-id]
  (world/inbound! (world/principal w kind) "cancel-room-run!"
                  {:room-id-str (str (get w room)) :run-id-str (str run-id)}))

(deftest seeing-a-run-is-reading-its-room
  (world/with-world [_ w]
    (with-live-rooms w
      (fn [start!]
        (let [run (start! :team)
              see (fn [kind] (world/inbound! (world/principal w kind) "run-world-live!"
                                             {:room-id-str (str (:room w)) :run-id-str (str run)}))]
          (is (= (str run) (get-in (see :member) [:result :run-id])) "a member sees it")
          (is (= (str run) (get-in (see :owner) [:result :run-id])) "the owner sees it")
          (is (world/refused? (see :non-member)) "a non-member does not")
          (is (world/refused? (see :anon)) "nor does anonymous"))))))

(deftest cancelling-a-run
  (world/with-world [_ w]
    (with-live-rooms w
      (fn [start!]
        (testing "legitimate: the room owner cancels a run in their room"
          (let [run (start! :team)]
            (is (= :cancelling (get-in (cancel! w :owner :room run) [:result :status])))
            (is (true? (agent-run/cancel-requested? run)))))

        (testing "a non-member is refused at the gate; the run keeps running"
          (let [run (start! :team)]
            (is (world/refused? (cancel! w :non-member :room run)))
            (is (false? (agent-run/cancel-requested? run)))))

        (testing "naming one's OWN room does not reach a run in another"
          ;; the stranger passes the gate on the room they own; the handler
          ;; binds the opaque run id to that room and finds nothing
          (let [run (start! :team)]
            (is (= :not-active (get-in (cancel! w :non-member :other-room run) [:result :status])))
            (is (false? (agent-run/cancel-requested? run)))))

        (known-hole
         "§2.9.2 cancel-room-run! any member cancels any run"
         "a member who neither started the run nor owns the room cancels it (the gate is room :write, the handler checks only that the run is in the room)"
         (let [run (start! :team)]   ; not started by the member
           (cancel! w :member :room run)
           (agent-run/cancel-requested? run)))))))
