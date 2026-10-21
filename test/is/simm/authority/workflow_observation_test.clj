(ns is.simm.authority.workflow-observation-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.room.registry :as registry]
            [is.simm.authority.world :as world]
            [is.simm.agents.room-agents :as rooms]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.runtimes.context :as context]
            [is.simm.runtimes.memory-changes :as changes]
            [is.simm.uis.web.desktop.workflows-remote]))

(deftest real-inbound-dashboard-interest-captures-human-without-room-hydration
  (world/with-world [conn w]
    (let [registrations (atom [])
          forbidden (fn [& _] (throw (AssertionError. "Metadata registration must not open a Room")))]
      (changes/start-owned! conn context/server-context (fn [& _]))
      (try
        (with-redefs [rooms/live-room forbidden registry/lookup forbidden owned/resolve! forbidden
                      changes/register-topic! (fn [_ party] (swap! registrations conj party))]
          (doseq [kind [:owner :member :non-member :admin]]
            ;; There is deliberately NO room argument. The interest discloses
            ;; no content; the later snapshot still filters room authority.
            (is (= {:watching? true}
                   (:result (world/inbound! (world/principal w kind)
                                            "prepare-workflow-metadata-stream!" {}))))
            (is (= (get w kind) (last @registrations))))
          (doseq [kind [:anon :team-agent]]
            (let [before @registrations]
              (is (world/refused? (world/inbound! (world/principal w kind)
                                                  "prepare-workflow-metadata-stream!" {})))
              (is (= before @registrations)))))
        (finally (changes/stop!))))))
