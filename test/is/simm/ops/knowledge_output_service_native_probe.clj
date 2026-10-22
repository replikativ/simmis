(ns is.simm.ops.knowledge-output-service-native-probe
  "OPT-IN actual manual admission/native execution/filing with a fixed SCI or
   Echo candidate, never model quality evidence or a paid-provider smoke test."
  (:require [clojure.test :refer [is]]
            [dvergr.jobs :as jobs]
            [dvergr.system.rooms :as native-rooms]
            [is.simm.agents.room-agents :as rooms]
            [is.simm.model.access :as access]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.knowledge-filing :as filing]
            [is.simm.ops.knowledge-filing-test :as fixture]
            [is.simm.ops.knowledge-output-service :as service]
            [is.simm.ops.work :as work]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.application-ops-test :as app-fixture]
            [is.simm.runtimes.context :as context]))

(defn probe
  "Separate fixture-only topology resolver plus real application/Room ownership.
   Tests early collection refusal, completed result, immutable retry, proposed
   output and no-op in both registration orders. No Attempt is fabricated."
  [owned-first? mode]
  (@#'fixture/native-workspace-probe*
   owned-first? mode
   (fn [{:keys [conn room room-id principal submission prepared]}]
     (let [party (access/actor->party-uuid (:principal/id principal))
           entered (promise) release (promise)
           prepared (update-in prepared [:evaluator :capture]
                               (fn [capture]
                                 (fn [context]
                                   (deliver entered true)
                                   @release
                                   (capture context))))]
       (with-redefs [context/server-context (:ctx room)
                     native-rooms/room-ctx-for #(when (= room-id %) (:ctx room))
                     rooms/live-room (fn [id]
                                       (when-not (= room-id id)
                                         (throw (ex-info "Unexpected fixture Room" {})))
                                       room)]
         (@#'app-fixture/isolated
          (fn []
            (application/start!)
            (let [admitted (work/submit-prepared! conn (:ctx room) principal room-id room submission prepared)
                  request (:id admitted) job (:job admitted)]
              (try
                (is (true? (deref entered 10000 nil)))
                (is (= {:status :unavailable} (service/collect! party room-id request)))
                (is (nil? (dispositions/disposition (requests/get-request conn request))))
                (finally (deliver release true)))
              (let [status (jobs/status job 25000)
                    result (service/collect! party room-id request)
                    expected (if (= :sci-write mode) :proposed :no-op)
                    retry (with-redefs [filing/prepare
                                        (fn [& _] (throw (AssertionError. "Retry recertified an existing disposition")))]
                            (service/collect! party room-id request))]
                (is (= :completed (:status status)))
                (is (= expected (:status result)))
                (is (= (str request) (:request result)))
                (is (= result retry))
                (is (= (= :proposed expected) (boolean (:proposal result))))
                {:owned-first? owned-first? :mode mode :job-status (:status status)
                 :collected result :retry retry :model-executed? false})))))))))
