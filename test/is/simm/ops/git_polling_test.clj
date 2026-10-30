(ns is.simm.ops.git-polling-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [dvergr.system.rooms :as rooms]
            [geschichte.git.http :as http]
            [geschichte.repo :as repo]
            [is.simm.model.git-policy :as policy]
            [is.simm.model.github-sync :as journal]
            [is.simm.model.github-integrations :as integrations]
            [is.simm.ops.git-polling :as polling]
            [is.simm.ops.owned-mcp :as mcp]
            [is.simm.ops.github-sync-test :as fixtures]
            [is.simm.runtimes.operation-lifetime :as lifetime]))

(use-fixtures :each
  (fn [f]
    (with-redefs-fn {#'rooms/storage-gc-homes (atom #{})
                     #'rooms/storage-gc-admission (atom {:ever-admitted? false :prohibited-by nil})} f)))

(defn- enable! [{:keys [conn room-id creator]} enabled?]
  (let [connection (:github.integration/connection-id (integrations/current-in @conn room-id))
        current (policy/effective @conn room-id connection)]
    (d/transact conn (policy/configure-tx @conn room-id connection (:git.policy/revision current) (random-uuid) creator
                                          {:enabled? enabled? :interval-seconds 60 :review-mode :fast-forward} (java.util.Date. 0)))))

(deftest unchanged-remote-is-discovered-without-fetching-and-not-checked-again-before-due
  (#'fixtures/with-imported
   (fn [{:keys [conn room-id root owner] :as environment}]
     (d/transact conn (concat policy/schema journal/schema))
     (enable! environment true)
     (let [original (mcp/host conn root owner) ls-remote http/ls-remote! checks (atom 0)]
       (with-redefs [http/ls-remote! (fn [& args] (swap! checks inc) (apply ls-remote args))
                     http/fetch! (fn [& _] (throw (AssertionError. "Unchanged refs must not fetch")))]
         (lifetime/with-operation! owner #(polling/tick-owned! original (java.util.Date.)))
         (is (= 1 @checks))
         (is (= :unchanged (:git.policy/status (policy/current-in @conn room-id))))
         (is (nil? (journal/active-in @conn room-id)))
         (lifetime/with-operation! owner #(polling/tick-owned! original (java.util.Date.)))
         (is (= 1 @checks))
         (enable! environment false)
         (lifetime/with-operation! owner #(polling/tick-owned! original (java.util.Date. Long/MAX_VALUE)))
         (is (= 1 @checks)))))))

(deftest changed-public-ref-applies-through-the-original-host-without-an-outbound-token
  (#'fixtures/with-imported
   (fn [{:keys [conn room-id parent root owner] :as environment}]
     (d/transact conn (concat policy/schema journal/schema))
     (enable! environment true)
     (#'fixtures/c2! environment)
     (let [original (mcp/host conn root owner)]
       (lifetime/with-operation! owner #(polling/tick-owned! original (java.util.Date.)))
       (is (= :synced (:git.policy/status (policy/current-in @conn room-id))))
       (is (= "New upstream report\n" (String. ^bytes (repo/read parent "reports/new.md") "UTF-8")))
       (is (= (:github.integration/head (integrations/current-in @conn room-id))
              (:geschichte.commit/id (repo/head-commit parent))))
       (is (nil? (journal/active-in @conn room-id)))))))
