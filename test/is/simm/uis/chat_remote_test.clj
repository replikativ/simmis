(ns is.simm.uis.chat-remote-test
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.uis.web.desktop.chat-remote :as chat-remote]))

(deftest assignment-summary-is-a-thin-room-local-projection
  (testing "a party actor keeps stable identity, role and response policy"
    (is (= {:actor-id "3a22aa40-a936-4c93-8f10-f43bd3199ab2"
            :role :reviewer
            :response-policy :mention}
           (chat-remote/assignment-summary
            {:assignment/id #uuid "7d21a82b-86b6-4a3f-8124-408ab1b00138"
             :assignment/actor-id
             (keyword "party" "3a22aa40-a936-4c93-8f10-f43bd3199ab2")
             :assignment/role :reviewer
             :assignment/response-policy :mention
             :assignment/config {:private-provider-token "must-not-leak"}}))))
  (testing "non-party actors do not acquire fabricated Simmis identities"
    (is (nil? (chat-remote/assignment-summary
               {:assignment/actor-id :system/scheduler
                :assignment/role :observer
                :assignment/response-policy :manual})))))

(deftest effective-agent-policy-does-not-require-a-write
  (let [id #uuid "72b123b7-ec0a-41bc-b451-84faf1a9993a"]
    (is (= {:actor-id (str id)
            :role :specialist
            :response-policy :always}
           (chat-remote/agent-assignment-summary
            {:party/id id :party/auto-respond? true} nil)))
    (is (= {:actor-id (str id)
            :role :observer
            :response-policy :manual}
           (chat-remote/agent-assignment-summary
            {:party/id id :party/auto-respond? true}
            {:assignment/role :observer
             :assignment/response-policy :manual})))))
