(ns is.simm.ops.benchmark-notifications-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [dvergr.agent.experiment :as experiment]
            [dvergr.discourse :as discourse]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.benchmark-notifications :as notifications]
            [is.simm.documents.benchmark-result :as result]
            [is.simm.ops.benchmark-reports-test :as fixture]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- with-result [f]
  (@#'fixture/with-fixture
   (fn [{:keys [conn w room id owner principal] :as context}]
     (d/transact conn requests/schema)
     (let [card (binding [ec/*execution-context* (:ctx room)] (experiment/scorecard-bounded room id))
           job (random-uuid)
           {:keys [request]} (requests/claim! conn {:party (:member w) :room (:room w)
                                                    :client-id (random-uuid) :kind :benchmark
                                                    :intent {:experiment (:scorecard/experiment card)}})
           request-id (:work.request/id request)]
       (requests/record-job! conn request-id (:work.request/claim request) job)
       (f (assoc context :request-id request-id :job job :card card
                 :value {:experiment (:scorecard/experiment card) :scorecard card
                         :execution {:cleanup-group job}}
                 :original (assoc request :work.request/job job)
                 :complete (fn [status value]
                             (let [original (assoc request :work.request/job job)
                                   value (if (map? value)
                                           (merge {:experiment (:scorecard/experiment card)
                                                   :execution {:cleanup-group job}} value) value)]
                               (notifications/record-result! conn original room status value)
                               (lifetime/with-operation!
                                 owner #(notifications/complete! conn principal (:room w) room original))))))))))

(defn- messages [room]
  (binding [ec/*execution-context* (:ctx room)]
    (filterv #(= :benchmark/result (get-in % [:metadata :kind])) (discourse/log room))))

(deftest exact-scorecard-is-published-as-one-durable-system-reference
  (with-result
    (fn [{:keys [conn room request-id id value complete host principal handle]}]
      (let [reply (complete :completed value)
            row (requests/get-request conn request-id)
            [message] (messages room)]
        (is (= id (:scorecard reply)))
        (is (= id (:work.request/result-scorecard row)))
        (is (:work.request/result-published? row))
        (is (= :_room-log (:to message)) "No agent is activated by its result notification")
        (is (= {:kind :benchmark-result :id id} (get-in message [:metadata :object])))
        (is (re-find #"\| Candidate \|" (:content message)))
        (is (re-find #"List-price cost" (:content message)))
        (is (= reply (complete :completed value)))
        (is (= reply (notifications/retry! host principal handle request-id)))
        (is (= 1 (count (messages room))))))))

(deftest lost-message-ack-retries-the-reserved-envelope-without-rerunning-work
  (with-result
    (fn [{:keys [conn room request-id value complete host principal handle]}]
      (let [post discourse/post!]
        (with-redefs [discourse/post! (fn [& args] (apply post args) (throw (ex-info "Lost ACK" {})))]
          (is (thrown? clojure.lang.ExceptionInfo (complete :completed value))))
        (let [before (first (messages room))]
          (is (false? (:work.request/result-published? (requests/get-request conn request-id))))
          (is (= :published (:status (notifications/retry! host principal handle request-id))))
          (is (= [before] (messages room)) "Same ID, timestamp, content and source after retry"))))))

(deftest a-pending-envelope-survives-a-second-completion-observation
  (with-result
    (fn [{:keys [conn room request-id value complete]}]
      (with-redefs [discourse/post! (fn [& _] (throw (ex-info "Unavailable" {})))]
        (is (thrown? clojure.lang.ExceptionInfo (complete :completed value))))
      (let [ref (:work.request/result-message (requests/get-request conn request-id))]
        (is (= :published (:status (complete :completed value))))
        (is (= ref (:work.request/result-message (requests/get-request conn request-id))))
        (is (= 1 (count (messages room))))))))

(deftest incomplete-and-failed-jobs-never-show-a-fake-zero-scorecard
  (doseq [[status value expected] [[:completed {:scorecard {:incomplete {:cells 1}}} :incomplete]
                                   [:failed (ex-info "Infrastructure" {}) :failed]
                                   [:cancelled nil :cancelled]]]
    (with-result
      (fn [{:keys [conn room request-id complete]}]
        (complete status value)
        (let [row (requests/get-request conn request-id) [message] (messages room)]
          (is (= expected (:work.request/result-status row)))
          (is (nil? (:work.request/result-scorecard row)))
          (is (nil? (get-in message [:metadata :object])))
          (is (re-find #"not a zero score" (:content message))))))))

(deftest foreign-result-permission-revocation-and-store-remap-refuse
  (with-result
    (fn [{:keys [conn w room value complete request-id host principal handle]}]
      (testing "returned Experiment must be the exact retained one"
        (is (thrown? clojure.lang.ExceptionInfo
                     (complete :completed (assoc value :experiment {}))))
        (is (nil? (:work.request/result-message (requests/get-request conn request-id)))))
      (with-redefs [discourse/post! (fn [& _] (throw (ex-info "Unavailable" {})))]
        (is (thrown? clojure.lang.ExceptionInfo (complete :completed value))))
      (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
      (is (thrown? clojure.lang.ExceptionInfo (notifications/retry! host principal handle request-id)))
      (d/transact conn [{:room/id (:room w) :room/parties [[:party/id (:member w)]]
                         :room/content-db-scope (random-uuid)}])
      (is (thrown? clojure.lang.ExceptionInfo (notifications/retry! host principal handle request-id)))
      (is (empty? (messages room))))))

(deftest durable-result-survives-original-app-retirement-before-envelope-reservation
  (with-result
    (fn [{:keys [conn room request-id value complete owner host principal handle]}]
      (lifetime/fence! owner)
      (is (thrown? clojure.lang.ExceptionInfo (complete :completed value)))
      (let [row (requests/get-request conn request-id) replacement (lifetime/create)]
        (is (:work.request/result-scorecard row))
        (is (:work.request/result-at row))
        (is (nil? (:work.request/result-message row)))
        (lifetime/open! replacement)
        (try
          (is (= :published
                 (:status (notifications/retry!
                           (assoc-in host [:config :host-lifecycle :with-operation]
                                     #(lifetime/with-operation! replacement %))
                           principal handle request-id))))
          (is (= 1 (count (messages room))))
          (finally (lifetime/fence! replacement) (lifetime/join! replacement 5000)))))))

(deftest a-changed-request-claim-cannot-adopt-the-original-completion
  (with-result
    (fn [{:keys [conn request-id value complete]}]
      (d/transact conn [[:db/add [:work.request/id request-id] :work.request/claim (random-uuid)]])
      (is (thrown? clojure.lang.ExceptionInfo (complete :completed value)))
      (is (nil? (:work.request/result-at (requests/get-request conn request-id)))))))

(deftest retry-of-reserved-bytes-does-not-reformat-or-read-receipts
  (with-result
    (fn [{:keys [room value complete host principal handle request-id]}]
      (with-redefs [discourse/post! (fn [& _] (throw (ex-info "Unavailable" {})))]
        (is (thrown? clojure.lang.ExceptionInfo (complete :completed value))))
      (with-redefs [result/content (fn [& _] (throw (AssertionError. "Retry reformatted")))
                    experiment/scorecard-bounded (fn [& _] (throw (AssertionError. "Retry read Scorecard")))]
        (is (= :published (:status (notifications/retry! host principal handle request-id)))))
      (is (= 1 (count (messages room)))))))
