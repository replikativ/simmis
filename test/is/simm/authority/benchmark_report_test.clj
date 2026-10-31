(ns is.simm.authority.benchmark-report-test
  (:require
            [clojure.test :refer [deftest is]]
            [is.simm.authority.world :as world]
            [is.simm.ops.benchmark-reports :as reports]
            [is.simm.ops.benchmark-reports-test :as fixture]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [is.simm.uis.web.desktop.benchmark-report-remote :as remote]))

(deftest real-inbound-report-actions-use-current-room-write-and-original-app
  (@#'fixture/with-fixture
   (fn [{:keys [host owner w id]}]
     (with-redefs-fn {#'application/installation (atom {:host host :owner owner})}
       (fn []
         (doseq [kind [:non-member :anon :admin]]
           (is (world/refused? (world/inbound! (world/principal w kind) "prepare-report!"
                                               {:room-id-str (str (:room w)) :request {:scorecard id}}))))
         (let [prepared (:result (world/inbound! (world/principal w :member) "prepare-report!"
                                                 {:room-id-str (str (:room w)) :request {:scorecard id}}))
               request {:scorecard id :expected-target-stamp (:target-stamp prepared) :expected-plan (:plan prepared)}
               published (:result (world/inbound! (world/principal w :member) "publish-report!"
                                                  {:room-id-str (str (:room w)) :request request}))]
           (is (= id (get-in prepared [:source :scorecard])))
           (is (true? (:complete? published)))
           (is (= :created (get-in published [:report :status])))
           (let [queued (remote/prepare-operation (:member w) (str (:room w)) :publish request)
                 calls (atom 0)]
             (lifetime/fence! owner)
             (with-redefs [reports/publish! (fn [& _] (swap! calls inc))]
               (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Report publication did not finish" (queued)))
               (is (zero? @calls))))))))))
