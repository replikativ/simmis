(ns is.simm.agents.tool-authorization-test
  (:require [clojure.test :refer [deftest is testing]]
            [dvergr.tools :as dvergr-tools]
            [is.simm.agents.tool-authorization :as auth]
            [is.simm.model.access :as access]))

(deftest tool-actions-deny-new-effects-open-ended-write-authority
  (is (= :read (auth/room-tool-action "read_file")))
  (is (= :read (auth/room-tool-action "knowledge_search")))
  (is (= :write (auth/room-tool-action "write_file")))
  (is (= :write (auth/room-tool-action "clojure_eval")))
  (is (= :write (auth/room-tool-action "new_external_effect"))))

(deftest room-decision-uses-the-shared-rebac-seam
  (let [party-id (random-uuid)
        room-id (random-uuid)]
    (with-redefs [access/can? (fn [subject action resource]
                                (and (= party-id subject)
                                     (= :write action)
                                     (= {:room room-id} resource)))]
      (is (= {:decision :authorized
              :sources #{:agent-tool-grant :simmis-rebac}
              :subject-type :party
              :subject-id (str party-id)
              :action :write
              :resource-type :room
              :resource-id (str room-id)}
             (auth/room-decision party-id room-id "shell"))))

    (testing "a revoked relation fails closed with useful provenance"
      (with-redefs [access/can? (constantly false)]
        (let [decision (auth/room-decision party-id room-id "read_file")]
          (is (= :denied (:decision decision)))
          (is (= :read (:action decision)))
          (is (string? (:reason decision))))))))

(deftest authorizers-are-late-bound-on-every-tool
  (let [allowed? (atom true)
        tools (with-redefs [access/can? (fn [& _] @allowed?)]
                (auth/authorize-room-tools
                 {"read_file" {:name "read_file"}}
                 (random-uuid) (random-uuid)))
        authorize (get-in tools ["read_file" :authorize])]
    (with-redefs [access/can? (fn [& _] @allowed?)]
      (is (= :authorized (:decision (authorize {} {}))))
      (reset! allowed? false)
      (is (= :denied (:decision (authorize {} {})))))))

(deftest released-dvergr-enforces-the-simmis-decision-before-the-effect
  (let [called (atom 0)
        party-id (random-uuid)
        room-id (random-uuid)
        tools (auth/authorize-room-tools
               {"write_file" {:name "write_file"
                              :execute (fn [& _]
                                         (swap! called inc)
                                         {:type :success})}}
               party-id room-id)]
    (with-redefs [access/can? (constantly false)]
      (let [result (dvergr-tools/execute "write_file" {} {:tools tools})]
        (is (zero? @called))
        (is (= :error (:type result)))
        (is (= :denied (get-in result [:authorization :decision])))))
    (with-redefs [access/can? (constantly true)]
      (let [result (dvergr-tools/execute "write_file" {} {:tools tools})]
        (is (= 1 @called))
        (is (= :success (:type result)))
        (is (= #{:agent-tool-grant :simmis-rebac}
               (get-in result [:authorization :sources])))))))
