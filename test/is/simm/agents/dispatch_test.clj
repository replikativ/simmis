(ns is.simm.agents.dispatch-test
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.agents.dispatch :as dispatch]))

(defn- party [id type handle auto?]
  {:party/id id
   :party/type type
   :party/handle handle
   :party/display-name handle
   :party/auto-respond? auto?})

(deftest assignment-policies-select-recipients
  (let [always (party (random-uuid) :agent "always" true)
        mentioned (party (random-uuid) :agent "réviseur" true)
        manual (party (random-uuid) :agent "manual" true)
        human (party (random-uuid) :human "alice" false)
        assignments [{:assignment/actor-id (dispatch/party->actor-id always)
                      :assignment/response-policy :always}
                     {:assignment/actor-id (dispatch/party->actor-id mentioned)
                      :assignment/response-policy :mention}
                     {:assignment/actor-id (dispatch/party->actor-id manual)
                      :assignment/response-policy :manual}]
        parties [always mentioned manual human]]
    (testing "only always-on agents wake without mentions"
      (is (= [always]
             (:recipients (dispatch/plan-message-dispatch
                           parties assignments "What changed?")))))
    (testing "a mention-policy agent joins always-on recipients when named"
      (let [plan (dispatch/plan-message-dispatch
                  parties assignments "@RÉVISEUR please check this")]
        (is (= [always mentioned] (:recipients plan)))
        (is (= #{"réviseur"} (:mentions plan)))
        (is (= #{(dispatch/party->actor-id mentioned)}
               (set (:audience plan))))))
    (testing "manual agents remain audience without being auto-invoked"
      (let [plan (dispatch/plan-message-dispatch
                  parties assignments "@manual please keep this for later")]
        (is (= [always] (:recipients plan)))
        (is (= #{(dispatch/party->actor-id manual)}
               (set (:audience plan))))))
    (testing "human mentions are valid audience, not agent recipients"
      (let [plan (dispatch/plan-message-dispatch
                  parties assignments "@alice can you decide?")]
        (is (= [always] (:recipients plan)))
        (is (= #{(dispatch/party->actor-id human)}
               (set (:audience plan))))))))

(deftest legacy-auto-response-remains-the-default
  (let [auto (party (random-uuid) :agent "auto" true)
        quiet (party (random-uuid) :agent "quiet" false)]
    (is (= [auto]
           (:recipients (dispatch/plan-message-dispatch
                         [auto quiet] [] "hello"))))))

(deftest invalid-mentions-never-wake-everyone
  (let [one (party (random-uuid) :agent "Case" true)
        two (party (random-uuid) :agent "case" true)]
    (testing "unknown explicit handles fail closed"
      (let [error (try
                    (dispatch/plan-message-dispatch [one] [] "hello @missing")
                    nil
                    (catch Exception e e))]
        (is (= :dispatch/invalid-mentions (:type (ex-data error))))
        (is (= #{"missing"} (:unknown (ex-data error))))))
    (testing "case-insensitively ambiguous handles fail closed"
      (let [error (try
                    (dispatch/plan-message-dispatch [one two] [] "hello @CASE")
                    nil
                    (catch Exception e e))]
        (is (= :dispatch/invalid-mentions (:type (ex-data error))))
        (is (= #{"case"} (set (keys (:ambiguous (ex-data error))))))))))
