(ns is.simm.ops.digest-notification-test
  "Real Request journal, immutable envelopes and native durable post; certified
   digest read is isolated here and has its own retained-result verification."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.discourse :as discourse]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.conversation-capture :as capture]
            [is.simm.ops.conversation-capture-test :as fixture]
            [is.simm.ops.digest-notification :as notification]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.room-digest :as digest]
            [is.simm.uis.web.desktop.room-digest :as ui]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- with-result [f]
  (@#'fixture/with-fixture
   (fn [{:keys [conn host w room] :as data}]
     (d/transact conn requests/schema)
     (let [store (binding [ec/*execution-context* (:ctx room)]
                   (get-in @(:conn (:store room)) [:config :store :id]))
           _ (d/transact conn [{:room/id (:room w) :room/content-db-scope store}])
           request (:request (requests/claim! conn {:party (:owner w) :room (:room w) :client-id (random-uuid)
                                                   :kind :workflow :intent {:digest true}}))
           id (:work.request/id request) job (random-uuid)
           _ (requests/record-job! conn id (:work.request/claim request) job)
           result {:status :verified :room (:room w) :request id :digest-id (random-uuid)
                   :digest {:summary {:text "A generated navigation summary."}}}
           principal (ops/party-principal (:member w))
           sent {:request-id id :digest-id (:digest-id result)}]
       (with-redefs [digest/result! (fn [_ _ handle opts]
                                     (is (= (str (:room w)) handle))
                                     (is (= {:request-id id} opts)) result)]
         (f (assoc data :principal principal :handle (str (:room w)) :result result :sent sent
                   :publish #(notification/publish! host principal (str (:room w)) sent))))))))

(defn- messages [room]
  (binding [ec/*execution-context* (:ctx room)]
    (filterv #(= :conversation-digest (get-in % [:metadata :kind])) (discourse/log room))))

(deftest explicit-room-writer-keeps-one-durable-generated-digest-without-self-trigger
  (with-result
    (fn [{:keys [host w room conn sent result publish]}]
      (let [reply (publish) again (publish) [message] (messages room)
            row (requests/get-request conn (:request-id sent))]
        (is (= reply again))
        (is (= :published (:status reply)))
        (is (= :digest-published (:work.request/result-status row)))
        (is (true? (:work.request/result-published? row)))
        (is (= 1 (count (messages room))))
        (is (= :_room-log (:to message)) "No agent target or broadcast wake")
        (is (= :simmis/conversation-digests (:from message)) "Publishing human is not the summary author")
        (is (= {:request (:request-id sent) :digest-id (:digest-id result)}
               (ui/notification-reference (:metadata message))))
        (is (= (:id message) (notification/message-id (:request-id sent) (:digest-id result))))
        (is (empty? (:artifacts (capture/preview-room! host (ops/party-principal (:owner w))
                                                      (str (:room w)) {})))
            "The collector excludes generated digest notifications")))))

(deftest lost-native-acknowledgement-never-reposts-or-reawakens
  (with-result
    (fn [{:keys [room conn sent publish]}]
      (let [post discourse/post! calls (atom 0)]
        (with-redefs [discourse/post! (fn [& args]
                                       (swap! calls inc) (apply post args)
                                       (throw (ex-info "Lost native ACK" {})))]
          (is (thrown? clojure.lang.ExceptionInfo (publish))))
        (is (= 1 (count (messages room))))
        (is (= :digest-posting (:work.request/result-status (requests/get-request conn (:request-id sent)))))
        (with-redefs [discourse/post! (fn [& _] (is false "Replayed an uncertain post"))]
          (is (= :inspection-required (:status (publish)))))
        (is (= 1 @calls))
        (is (= 1 (count (messages room))))))))

(deftest changed-digest-readonly-and-foreign-room-cannot-reserve-or-post
  (with-result
    (fn [{:keys [host conn w room principal handle sent publish]}]
      (is (thrown? clojure.lang.ExceptionInfo
                   (notification/publish! host principal handle (assoc sent :digest-id (random-uuid)))))
      (is (nil? (:work.request/result-message (requests/get-request conn (:request-id sent)))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (notification/publish! host principal handle (assoc sent :request-id (random-uuid)))))
      (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
      (is (thrown? clojure.lang.ExceptionInfo (publish)))
      (is (empty? (messages room))))))
