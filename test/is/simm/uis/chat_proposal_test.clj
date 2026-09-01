(ns is.simm.uis.chat-proposal-test
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.uis.web.desktop.proposal-card :as proposal-card]))

(defn- model [proposal]
  (proposal-card/action-model {:status :open :proposal proposal}))

(deftest proposal-card-state-is-a-pure-canonical-projection
  (testing "terminal and loading states expose no actions"
    (is (= "Checking…"
           (:status-label (proposal-card/action-model {:status :checking}))))
    (is (:accept-disabled?
         (proposal-card/action-model {:status :accepted}))))
  (testing "the diff must be loaded before accepting"
    (let [m (model {:forks []})]
      (is (= "Checking…" (:accept-label m)))
      (is (:accept-disabled? m))
      (is (false? (:request-disabled? m)))))
  (testing "a fully available proposal is actionable"
    (let [m (model {:diffs []
                    :forks [{:branch "work" :status nil
                             :may-merge? true :capability-live? true}]})]
      (is (= "Accept" (:accept-label m)))
      (is (false? (:accept-disabled? m)))
      (is (false? (:dismiss-disabled? m)))))
  (testing "whole-proposal authority and capability constraints match the inspector"
    (let [unlandable (model {:diffs []
                             :forks [{:status nil :may-merge? false}]})
          unavailable (model {:diffs []
                              :forks [{:status nil :capability-live? false}]})]
      (is (= "Cannot land all" (:accept-label unlandable)))
      (is (:accept-disabled? unlandable))
      (is (false? (:request-disabled? unlandable)))
      (is (= "Unavailable" (:accept-label unavailable)))
      (is (:dismiss-disabled? unavailable))))
  (testing "a conflict warning arms the explicit second press"
    (let [m (model {:diffs [] :forks [] :accept-warned? true})]
      (is (= "Accept anyway" (:accept-label m)))
      (is (:accept-force? m)))))

(deftest proposal-state-participates-in-keyed-item-equality
  (let [message {:entity/uuid #uuid "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"
                 :message/metadata {:object {:kind :proposal :id "p-1"}}}
        checking (proposal-card/attach-state message {:status :checking})
        accepted (proposal-card/attach-state message {:status :accepted})]
    (is (= (:entity/uuid checking) (:entity/uuid accepted))
        "the durable timeline identity stays stable")
    (is (not= checking accepted)
        "an unchanged message invalidates its keyed VNode when Proposal state changes")
    (is (= "Accepted"
           (:status-label
            (proposal-card/action-model
             (get accepted proposal-card/proposal-state-key)))))))

(deftest proposal-load-errors-are-not-presented-as-authorization-facts
  (let [m (proposal-card/action-model
           {:status :error :proposal {:error "temporary fetch failure"}})]
    (is (= "Load failed" (:status-label m)))
    (is (= "temporary fetch failure" (:action-error m)))
    (is (:accept-disabled? m))))
