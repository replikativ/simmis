(ns is.simm.ops.proposal-identity-conflict-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.ops.proposals-fork-test :as fixture]
            [is.simm.ops.proposals :as proposals]
            [is.simm.model.system-db :as system-db]
            [is.simm.runtimes.branching :as branching]
            [is.simm.runtimes.context :as ctx]
            [is.simm.uis.web.desktop.proposal-card :as card]))

(deftest different-page-identities-block-before-any-contribution-lands
  (ctx/with-server-context
    (let [system (#'fixture/fresh-system-db)]
      (with-redefs [system-db/get-conn (fn [] system)]
        (let [[scope trunk] (#'fixture/fresh-kb)
              fork (#'fixture/fork-on scope "proposal" "Shared title")
              [other-scope] (#'fixture/fresh-kb)
              other (#'fixture/fork-on other-scope "unrelated" "Independent page")
              trunk-id (random-uuid)]
          (d/transact trunk [{:entity/uuid trunk-id :S.Page/title "Shared title"}])
          (let [conflicts (branching/kb-conflicts scope :db (:branch fork))]
            (is (some #(and (= :page-identity-conflict (:kind %)) (:blocking? %)) conflicts)))
          ;; The guard exists at actual landing as well as preflight.
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"reconcile"
                                (branching/merge-kb! scope (:branch fork) :db)))
          (let [pid (proposals/file-proposal! {:title "Joint change" :forks [other fork]})
                result (proposals/accept-proposal! pid :force? true)]
            (is (seq (:blocked result)))
            (is (seq (:blocked (proposals/accept-fork! pid scope (:branch fork) :force? true))))
            (is (= :open (:proposal/status (proposals/get-proposal pid))))
            (is (not (#'fixture/page-titled? other-scope "Independent page")))
            (is (= trunk-id (d/q '[:find ?u . :where [?e :S.Page/title "Shared title"] [?e :entity/uuid ?u]] @trunk)))))))))

(deftest refresh-retains-failure-with-fresh-contribution-status
  (let [old [{:id "p" :action-error "Wiki did not merge" :forks [{:status nil}]}]
        fresh [{:id "p" :forks [{:status :accepted}]}]]
    (is (= [{:id "p" :action-error "Wiki did not merge" :forks [{:status :accepted}]}]
           (card/retain-action-feedback old fresh)))
    (is (= [] (card/retain-action-feedback old [])))
    (is (= fresh (card/retain-action-feedback [{:id "other" :action-error "Unrelated"}] fresh)))))
