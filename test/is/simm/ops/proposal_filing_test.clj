(ns is.simm.ops.proposal-filing-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.system-db :as sdb]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.proposals :as proposals]))

(defn- fixture []
  (let [party (random-uuid) room (random-uuid) client (random-uuid)
        manifest (random-uuid)
        proposal (hasch/uuid [:simmis/knowledge-file-proposal-v1 manifest])
        row {:work.request/id (requests/request-id party room client)
             :work.request/client-id client :work.request/by party :work.request/room room
             :work.request/kind :workflow :work.request/intent-id (random-uuid)
             :work.request/publication-basis-id (random-uuid)
             :work.request/payload-ref (random-uuid) :work.request/job (random-uuid)
             :work.request/state :submitted :work.request/return-pending? false}
        filing {:id proposal :title "Prepared filing"
                :forks [{:scope (random-uuid) :branch :fixture :system-type :kb}]}
        disposition (dispositions/make
                     {:request (:work.request/id row) :intent (:work.request/intent-id row)
                      :job (:work.request/job row) :basis (:work.request/publication-basis-id row)
                      :candidate (random-uuid) :attempt-content (random-uuid)
                      :output (random-uuid) :kind :proposed :manifest manifest :proposal proposal})]
    ;; Deliberately a storage composition fixture, not a certified file-output
    ;; bridge. Certification and typed retained manifests are tested separately.
    {:row row :filing filing :disposition disposition}))

(defn- with-store [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (d/transact conn (concat sdb/schema dispositions/schema))
        (f conn)
        (finally (d/release conn) (d/delete-database cfg))))))

(defn- composed-tx [db binding entity disposition]
  (into (proposals/filing-tx-data db entity)
        (dispositions/tx-data db binding disposition)))

(deftest entity-construction-neither-reads-a-connection-nor-publishes
  (let [{:keys [filing]} (fixture)]
    (with-redefs [sdb/get-conn (fn [] (throw (AssertionError. "Unexpected control read")))
                  d/transact (fn [& _] (throw (AssertionError. "Unexpected write")))]
      (let [entity (proposals/prepare-filing filing)]
        (is (= (:id filing) (:proposal/id entity)))
        (is (= :open (:proposal/status entity)))
        (is (= (:proposal/filing-hash entity)
               (:proposal/filing-hash (proposals/prepare-filing filing))))))))

(deftest composed-filing-commits-both-once-without-resetting-lifecycle
  (with-store
    (fn [conn]
      (let [{:keys [row filing disposition]} (fixture)
            entity (proposals/prepare-filing filing)
            commit! #(d/transact conn [[:db.fn/call composed-tx row entity disposition]])]
        (d/transact conn [row])
        (commit!)
        (is (= disposition (dispositions/disposition
                            (requests/get-request conn (:request disposition)))))
        (is (= 1 (count (d/q '[:find ?e :where [?e :proposal/id _]] @conn))))
        (d/transact conn [[:db/add [:proposal/id (:id filing)] :proposal/status :dismissed]])
        (commit!)
        (is (= :dismissed (:proposal/status (d/entity @conn [:proposal/id (:id filing)]))))
        (is (= 1 (count (d/q '[:find ?e :where [?e :proposal.fork/scope _]] @conn))))
        (is (thrown? Exception
                     (d/transact conn [[:db.fn/call composed-tx row
                                        (proposals/prepare-filing (assoc filing :title "Conflicting"))
                                        disposition]])))
        (is (= disposition (dispositions/disposition
                            (requests/get-request conn (:request disposition)))))))))

(deftest either-composition-failure-rolls-back-the-whole-filing
  (with-store
    (fn [conn]
      (let [{:keys [row filing disposition]} (fixture)
            entity (proposals/prepare-filing filing)]
        (d/transact conn [row])
        (is (thrown? Exception
                     (d/transact conn [[:db.fn/call composed-tx
                                        (assoc row :work.request/job (random-uuid)) entity disposition]])))
        (is (nil? (d/entity @conn [:proposal/id (:id filing)])))
        (is (nil? (dispositions/disposition (requests/get-request conn (:request disposition)))))
        (is (thrown? Exception
                     (d/transact conn [[:db.fn/call composed-tx row entity disposition]
                                       [:db.fn/call (fn [_] (throw (ex-info "Host rejection" {})))]])))
        (is (nil? (d/entity @conn [:proposal/id (:id filing)])))
        (is (nil? (dispositions/disposition (requests/get-request conn (:request disposition)))))))))
