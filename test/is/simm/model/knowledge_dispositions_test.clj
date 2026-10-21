(ns is.simm.model.knowledge-dispositions-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.work-requests :as requests]))

(defn- fixture []
  (let [party (random-uuid) room (random-uuid) client (random-uuid)
        row {:work.request/id (requests/request-id party room client)
             :work.request/client-id client :work.request/by party :work.request/room room
             :work.request/kind :workflow :work.request/intent-id (random-uuid)
             :work.request/publication-basis-id (random-uuid)
             :work.request/payload-ref (random-uuid) :work.request/job (random-uuid)
             :work.request/state :submitted :work.request/return-pending? false}
        fields {:request (:work.request/id row) :intent (:work.request/intent-id row)
                :job (:work.request/job row) :basis (:work.request/publication-basis-id row)
                :candidate (random-uuid) :attempt-content (random-uuid)
                :output (random-uuid) :kind :no-op}]
    {:row row :fields fields :value (dispositions/make fields)}))

(defn- proposed [fields]
  (let [manifest (random-uuid)]
    (dispositions/make
     (assoc fields :kind :proposed :manifest manifest
            :proposal (hasch/uuid [:simmis/knowledge-file-proposal-v1 manifest])))))

(defn- with-store [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :keep-history? false :schema-flexibility :write}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (d/transact conn (concat requests/schema dispositions/schema))
        (f conn)
        (finally (d/release conn))))))

(defn- transact! [conn binding value]
  (d/transact conn [[:db.fn/call dispositions/tx-data binding value]]))

(deftest semantic-identity-and-closed-typed-contract
  (let [{:keys [fields value]} (fixture)
        proposal (proposed fields)]
    (is (= value (dispositions/make fields)))
    (is (= value (dispositions/validate! value)))
    (is (= proposal (dispositions/validate! proposal)))
    (doseq [field [:request :intent :job :candidate :attempt-content :basis :output]]
      (is (not= (:id value) (:id (dispositions/make (assoc fields field (random-uuid))))))
      (is (thrown? clojure.lang.ExceptionInfo (dispositions/make (dissoc fields field)))))
    (doseq [bad [(assoc fields :id (random-uuid))
                 (assoc fields :version 2)
                 (assoc fields :unexpected true)
                 (assoc fields :kind :completed)
                 (assoc fields :manifest (random-uuid))
                 (assoc fields :proposal (random-uuid))
                 (assoc fields :kind :proposed)
                 (assoc fields :kind :proposed :manifest (random-uuid) :proposal (random-uuid))
                 (assoc fields :output nil)]]
      (is (thrown? clojure.lang.ExceptionInfo (dispositions/make bad))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (dispositions/validate! (assoc value :candidate (random-uuid)))))))

(deftest request-binding-requires-canonical-settled-workflow
  (let [{:keys [row]} (fixture)]
    (is (= row (dispositions/request-binding (assoc row :db/id 42))))
    (doseq [bad [(dissoc row :work.request/return-pending?)
                 (assoc row :work.request/return-pending? true)
                 (assoc row :work.request/kind :benchmark)
                 (assoc row :work.request/state :admitting)
                 (assoc row :work.request/id (random-uuid))
                 (dissoc row :work.request/payload-ref)
                 (dissoc row :work.request/publication-basis-id)]]
      (is (thrown? clojure.lang.ExceptionInfo (dispositions/request-binding bad))))))

(deftest typed-receipt-roundtrip-and-exact-replay
  (doseq [kind [:no-op :proposed]]
    (with-store
      (fn [conn]
        (let [{:keys [row fields value]} (fixture)
              value (if (= kind :proposed) (proposed fields) value)]
          (d/transact conn [row])
          ;; This model test exercises tx-data, not host certification or the
          ;; future atomic Proposal composition boundary.
          (is (nil? (dispositions/disposition (requests/get-request conn (:request value)))))
          (transact! conn row value)
          (is (= value (dispositions/disposition (requests/get-request conn (:request value)))))
          (is (= [] (dispositions/tx-data @conn row value)))
          (transact! conn row value)
          (is (= 1 (count (requests/requests conn))))
          (is (= 1 (count (d/q '[:find ?e :where [?e :knowledge.disposition/id _]] @conn))))
          (doseq [field [:candidate :attempt-content :output]]
            (is (thrown? Exception
                         (transact! conn row (dispositions/make
                                              (assoc (dissoc value :id :version) field (random-uuid)))))))
          (is (thrown? Exception
                       (transact! conn row (if (= kind :no-op) (proposed fields)
                                               (dispositions/make fields)))))
          (is (= value (dispositions/disposition (requests/get-request conn (:request value))))))))))

(deftest concurrent-identical-filings-share-the-existing-request-entity
  (with-store
    (fn [conn]
      (let [{:keys [row value]} (fixture)
            _ (d/transact conn [row])
            start (promise)
            workers (mapv (fn [_] (future @start (transact! conn row value))) (range 4))]
        (deliver start true)
        (doseq [worker workers]
          (is (not= ::timeout (deref worker 10000 ::timeout))))
        (is (= value (dispositions/disposition (requests/get-request conn (:request value)))))
        (is (= 1 (count (requests/requests conn))))))))

(deftest missing-changed-and-mismatched-request-refuse-before-writing
  (with-store
    (fn [conn]
      (let [{:keys [row fields value]} (fixture)]
        (is (thrown? Exception (transact! conn row value)))
        (d/transact conn [row])
        (doseq [field [:intent :job :basis]]
          (is (thrown? Exception
                       (transact! conn row (dispositions/make (assoc fields field (random-uuid)))))))
        (doseq [attr [:work.request/job :work.request/intent-id :work.request/payload-ref
                      :work.request/publication-basis-id]]
          (d/transact conn [[:db/add [:work.request/id (:request value)] attr (random-uuid)]])
          (is (thrown? Exception (transact! conn row value)))
          (d/transact conn [[:db/add [:work.request/id (:request value)] attr (get row attr)]]))
        (is (nil? (dispositions/disposition (requests/get-request conn (:request value)))))))))

(deftest partial-or-self-inconsistent-existing-receipt-is-never-repaired-by-replay
  (with-store
    (fn [conn]
      (let [{:keys [row value]} (fixture)]
        (d/transact conn [row])
        (d/transact conn [[:db/add [:work.request/id (:request value)]
                           :knowledge.disposition/id (:id value)]])
        (is (thrown? Exception (transact! conn row value)))
        (is (= #{:knowledge.disposition/id}
               (set (filter #(= "knowledge.disposition" (namespace %))
                            (keys (requests/get-request conn (:request value)))))))))))

(deftest enclosing-transaction-failure-leaves-no-disposition
  (with-store
    (fn [conn]
      (let [{:keys [row value]} (fixture)]
        (d/transact conn [row])
        (is (thrown? Exception
                     (d/transact conn
                                 [[:db.fn/call dispositions/tx-data row value]
                                  [:db.fn/call (fn [_] (throw (ex-info "proposal filing failed" {})))]])))
        (is (nil? (dispositions/disposition (requests/get-request conn (:request value)))))))))
