(ns is.simm.model.summary-schedule-inputs-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [hasch.core :as hasch]
            [is.simm.model.room-summaries-test :as fixture]
            [is.simm.model.summary-records-test :as stores]
            [is.simm.model.workflow-schedules :as schedules]
            [is.simm.ops.summary-inputs :as inputs]))

(deftest scheduled-daily-input-retains-exact-bytes-and-does-not-reconstruct-admission-ack
  (@#'stores/with-store
   (fn [conn]
     (d/transact conn schedules/schema)
     (let [activation (schedules/create! conn {:id (random-uuid) :room fixture/room :by (random-uuid)
                                               :now (java.util.Date. 0) :next-fire (java.util.Date. 60000)
                                               :target {:kind :workflow :cadence {:interval-ms 60000 :tz "UTC"}}})
           aid (:schedule.activation/id activation)
           inventory (inputs/inventory fixture/room fixture/period
                                       {:source-store fixture/store
                                        :rows [{:id (random-uuid) :author fixture/room :actor-kind :human
                                                :timestamp (.getTime (:from fixture/period)) :text "Exact original"}]})
           frozen (inputs/freeze inventory #inst "2026-10-31T00:00:00Z" [])]
       (schedules/claim-due! conn activation {:interval-ms 60000} (java.util.Date. 60000))
       (let [occurrence (first (schedules/occurrences conn aid)) oid (:schedule.occurrence/id occurrence)
             claim (schedules/admit! conn oid)
             revision (:schedule.activation/revision activation)
             id (hasch/uuid [:simmis/scheduled-summary-input-v1 revision (:coverage-id frozen)])
             target {:repository (random-uuid) :store (random-uuid) :backend :memory :branch :main :ref "main"
                     :registry-system (random-uuid) :runtime-system "summary-test"}
             value {:version 3 :kind :room-summary :occurrence-id oid :input-id id :target-revision revision
                    :coverage-id (:coverage-id frozen) :selection (:selection frozen) :inputs (:inputs frozen)
                    :summary-origin {:target target :target-stamp (random-uuid) :previous nil :generation 1}}]
         (is (= :capture (schedules/select-input! conn oid claim id false)))
         (let [ack (schedules/retain-owned-input! conn oid claim value)]
           (is (= value (schedules/retained-owned-input conn ack)))
           (is (thrown? clojure.lang.ExceptionInfo (schedules/retain-owned-input! conn oid claim value)))
           (is (= value (schedules/retained-owned-input conn (schedules/occurrence conn oid))))))))))
