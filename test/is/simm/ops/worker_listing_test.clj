(ns is.simm.ops.worker-listing-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.mcp.json-rpc :as rpc]
            [dvergr.mcp.server :as server]
            [dvergr.mcp.surface :as surface]
            [dvergr.ops :as dops]
            [dvergr.room.store :as store]
            [is.simm.authority.world :as world]
            [is.simm.model.workers :as workers]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.work-api :as api]
            [is.simm.ops.work-test]
            [is.simm.ops.worker-control :as control]
            [is.simm.ops.worker-files-test :as fixture]
            [is.simm.ops.worker-service :as service]
            [jsonista.core :as json]))

(deftest maximally-escaped-configurations-retain-complete-mcp-structured-content
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [room]
       (let [previous (dops/operations)
             previous-host @@#'server/embedded-host
             config (assoc fixture/config :model (apply str (repeat 200 \u0001))
                                          :version (apply str (repeat 200 \u0002)))
             principal (assoc (ops/party-principal (:owner w))
                              :principal/tools #{"simmis_local-worker-list"})]
         (d/transact conn [{:room/id (:room w) :room/slug (store/room-id->slug (:id room))}])
         (dotimes [_ 10]
           (workers/enroll! conn {:id (random-uuid) :by (:owner w) :room (:room w)
                                 :secret (workers/new-secret) :config config}))
         (try
           (server/set-host! (ops/host (:ctx room)))
           (api/install!) (control/install!)
           (let [ctx (server/session-context (fn [_]) (surface/selection {:profile :admin}) principal)]
             (rpc/handle-message ctx {:jsonrpc "2.0" :id 1 :method "initialize"
                                      :params {:protocolVersion "2025-06-18" :capabilities {}
                                               :clientInfo {:name "worker-list-bound" :version "1"}}})
             (rpc/handle-message ctx {:jsonrpc "2.0" :method "notifications/initialized"})
             (let [response (rpc/handle-message ctx {:jsonrpc "2.0" :id 2 :method "tools/call"
                                                     :params {:name "simmis_local-worker-list"
                                                              :arguments {:room (str (:room w))}}})
                   text (get-in response [:result :content 0 :text])]
               (is (not (:error response)))
               (is (false? (get-in response [:result :isError])))
               (is (map? (get-in response [:result :structuredContent])))
               (is (< (count text) 60000))
               (let [result (json/read-value text json/keyword-keys-object-mapper)]
                 (is (= 10 (count (:workers result))))
                 (is (every? #(= config (:config %)) (:workers result))))))
           (finally
             (doseq [op (concat (keys api/specifications) (keys control/specifications))]
               (if-let [spec (get previous op)] (dops/register-op! op spec) (dops/unregister-op! op)))
             (server/sync-op-tools!)
             (server/set-host! previous-host))))))))

(deftest enrollment-hints-are-own-room-human-only-and-paginated
  (world/with-world [conn w]
    (let [principal (ops/party-principal (:owner w))
          ids (sort-by str (repeatedly 12 random-uuid))
          enroll! (fn [id by room]
                    (workers/enroll! conn {:id id :by by :room room :secret (workers/new-secret)
                                          :config fixture/config}))]
      (doseq [id ids] (enroll! id (:owner w) (:room w)))
      (enroll! (random-uuid) (:member w) (:room w))
      (enroll! (random-uuid) (:owner w) (:other-room w))
      (workers/revoke! conn (last ids))
      (let [page (service/list-own conn principal (:room w) nil)
            more (service/list-own conn principal (:room w) (:next-after page))]
        (is (= (vec (take 10 ids)) (mapv :worker (:workers page))))
        (is (= [(nth ids 10)] (mapv :worker (:workers more))))
        (is (nil? (:next-after more)))
        (doseq [row (concat (:workers page) (:workers more))]
          (is (= #{:worker :generation :config-id :config} (set (keys row))))
          (is (= fixture/config (:config row))))
        (is (not-any? #(re-find #"credential|secret|payload|ref$" (name %))
                      (mapcat keys (:workers page)))))
      (doseq [other [(ops/party-principal (:member w))
                     (ops/party-principal (:non-member w))
                     (assoc principal :principal/kind :worker)]]
        (is (thrown? clojure.lang.ExceptionInfo (service/list-own conn other (:room w) nil))))
      (is (thrown? clojure.lang.ExceptionInfo (service/list-own conn principal (:room w) "bad")))
      (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:owner w)]]
                        [:db/add [:room/id (:room w)] :room/owner [:party/id (:non-member w)]]])
      (is (thrown? clojure.lang.ExceptionInfo (service/list-own conn principal (:room w) nil))))))
