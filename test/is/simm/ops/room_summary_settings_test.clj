(ns is.simm.ops.room-summary-settings-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.documents.files :as files]
            [is.simm.documents.space :as space]
            [is.simm.model.workflow-schedules :as schedules]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.owned-schedule-fixture :as fixture]
            [is.simm.ops.room-summary-settings :as settings]
            [is.simm.ops.workflow-readiness :as readiness]
            [is.simm.starter.daily-room-summary :as starter]))

(deftest ordinary-existing-memory-can-configure-and-disable-daily-summaries
  (fixture/with-fixture
   true
   (fn [{:keys [conn w host owned]}]
     (d/transact conn schedules/schema)
     (let [principal (ops/party-principal (:owner w)) handle (str (:room w))
           model "codex-subscription-sol"
           options {:models [{:id model :label "Luna" :provider-label "Codex" :credential-status :present}]
                    :default-model {:status :selected :id model :label "Luna" :source :user-preference}}]
       (is (false? (space/provisioned? @owned)))
       (with-redefs [readiness/read-on-host! (fn [& _] options)]
         (let [enabled (settings/configure! host principal handle
                                           {:expected-activation nil :request-id (random-uuid) :enabled? true
                                            :at "02:00" :timezone "UTC" :save-transcript? false :model nil})
               id (:activation enabled)
               row (schedules/activation conn id)]
           (is (true? (:enabled? enabled)))
           (is (= model (:model enabled)))
           (is (= (:models options) (:models enabled)))
           (is (files/entry @owned (str "workflows/" starter/name "/workflow.edn")))
           (is (files/entry @owned "summaries/index.md"))
           (is (false? (space/provisioned? @owned)) "Never relabel an existing repository")
           (is (nil? (:schedule.activation/active-occurrence row)) "Configuration dispatches no work")
           (is (false? (:enabled? (settings/configure! host principal handle
                                                      {:expected-activation id :request-id (random-uuid) :enabled? false
                                                       :at "02:00" :timezone "UTC" :save-transcript? false :model nil}))))))))))
