(ns is.simm.ops.work-fixture-test
  "Fixture ownership regressions; no inference/provider calls."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.connections :as connections]
            [datahike.tx-preds :as predicates]
            [dvergr.discourse :as discourse]
            [is.simm.ci-metrics :as metrics]
            [is.simm.ops.work-test :as fixture]
            [kontor.governance :as governance]))

(deftest a-successfully-closed-fixture-retires-its-own-chat-store
  (let [before (:registries (metrics/resources))
        cfg (volatile! nil)
        value (#'fixture/with-room
                (fn [room]
                  (vreset! cfg (:config @(:conn (:store room))))
                  :completed))]
    (is (= :completed value))
    (is (false? (d/database-exists? @cfg)))
    (is (= before (:registries (metrics/resources))))))

(deftest failed-room-close-preserves-the-original-governed-chat-for-recovery
  (let [before (:registries (metrics/resources))
        close! discourse/close-room!
        captured (volatile! nil)
        failure (ex-info "Physical Room close refused" {})]
    (try
      (is (identical?
           failure
           (with-redefs [discourse/close-room! (fn [& _] (throw failure))]
             (try (#'fixture/with-room
                    (fn [room]
                      (let [conn (:conn (:store room))]
                        (vreset! captured {:room room :conn conn :cfg (:config @conn)}))))
                  nil (catch Throwable error error)))))
      (let [{:keys [conn cfg]} @captured]
        (is (some #(and (identical? conn (:conn %)) (pos? (:count %)))
                  (vals @connections/*connections*))
            "The exact original connection must still have an acquisition")
        (is (true? (d/database-exists? cfg)))
        (is (ifn? (predicates/tx-pred-for (get-in cfg [:store :id])))))
      (finally
        ;; Test recovery gets the real close acknowledgment before disposal.
        (when-let [{:keys [room conn cfg]} @captured]
          (close! room)
          (governance/ungovern! conn)
          (d/release conn)
          (d/delete-database cfg))))
    (is (false? (d/database-exists? (:cfg @captured))))
    (is (= before (:registries (metrics/resources))))))
