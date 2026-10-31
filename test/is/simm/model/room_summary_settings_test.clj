(ns is.simm.model.room-summary-settings-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.model.access :as access]
            [is.simm.model.access-test :as access-fixture]
            [is.simm.model.room-summary-settings :as settings]
            [is.simm.model.workflow-schedules :as schedules]))

(deftest default-configuration-is-off-and-replacements-fence-admission
  (let [conn (@#'access-fixture/test-db)
        party access-fixture/owner room access-fixture/room-id
        make! #(schedules/create! conn {:id (random-uuid) :room room :by party
                                        :now (java.util.Date. 0) :next-fire (java.util.Date. 60000)
                                        :enabled? false
                                        :target {:kind :workflow :cadence {:interval-ms 60000 :tz "UTC"}
                                                 :execution {:name "summarize-room-daily"}}})]
    (try
      (d/transact conn schedules/schema)
      (is (nil? (settings/current-in @conn room)))
      (let [a (make!) id (:schedule.activation/id a)]
        (is (= id (settings/configure! conn party room nil nil a true)))
        (is (:schedule.activation/enabled? (schedules/activation conn id)))
        (let [old (schedules/activation conn id)]
          (is (thrown? clojure.lang.ExceptionInfo
                       (settings/configure! conn access-fixture/stranger room id old old false)))
          (settings/configure! conn party room id old nil false)
          (is (false? (:schedule.activation/enabled? (schedules/activation conn id))))
          (is (thrown? clojure.lang.ExceptionInfo
                       (settings/configure! conn party room id old old true))
              "A stale form cannot revive the old enabled control row"))
        (let [current (schedules/activation conn id)]
          (settings/configure! conn party room id current current true)
          (schedules/claim-due! conn (schedules/activation conn id) {:interval-ms 60000} (java.util.Date. 60000))
          (let [occurrence (first (schedules/occurrences conn id)) b (make!)]
            (schedules/admit! conn (:schedule.occurrence/id occurrence))
            (is (thrown? clojure.lang.ExceptionInfo
                         (settings/configure! conn party room id (schedules/activation conn id) b true))
                "Already-admitting old work forbids a second enabled activation")
            (settings/configure! conn party room id (schedules/activation conn id) nil false)
            (is (false? (:schedule.activation/enabled? (schedules/activation conn id))))
            (is (= :admitting (:schedule.occurrence/state (schedules/occurrence conn (:schedule.occurrence/id occurrence))))))))
      (finally (d/release conn)))))
