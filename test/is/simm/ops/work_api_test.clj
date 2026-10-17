(ns is.simm.ops.work-api-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.mcp.server :as server]
            [dvergr.model.registry :as models]
            [dvergr.ops :as dops]
            [dvergr.room.store :as store]
            [is.simm.authority.world :as world]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.work-api :as api]
            [is.simm.ops.work-test]
            [is.simm.ops.worker-control :as control]
            [is.simm.runtimes.auth-config :as auth]
            [is.simm.runtimes.worker-maintenance :as maintenance]))

(deftest canonical-uuid-crosses-the-real-op-authorizer-and-worker-service
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [room]
       (let [previous (dops/operations)
             slug (store/room-id->slug (:id room))
             host (ops/host (:ctx room))
             config {:adapter "codex" :version "test-package" :model "fixture-codex" :policy "local-trusted-v1"}
             args {:room (str (:room w)) :worker (str (random-uuid)) :config config}]
         (workspace/ensure-workspace! room)
         (d/transact conn [{:room/id (:room w) :room/slug slug}])
         (try
           (api/install!) (control/install!)
           (with-redefs [control/admission-open? (constantly true)
                         maintenance/claims-ready? (constantly true)
                         models/get-model (constantly {:provider :codex-subscription})
                         auth/get-auth-config (constantly {:jwt {:secret "canonical-room-fixture"}})]
             (is (= slug (api/room-handle (:room args))))
             (is (= (:room w) (second (api/room! host (:room args)))))
             (is (= [] (:names (dops/invoke host (ops/party-principal (:member w))
                                            :simmis/workflow-list {:room (:room args)}))))
             (is (= [] (:names (dops/invoke host (ops/party-principal (:member w))
                                            :simmis/workflow-list {:room slug}))))
             (let [enrolled (dops/invoke host (ops/party-principal (:owner w)) :simmis/local-worker-enroll args)]
               (is (= (:room args) (str (:room enrolled))))
               (is (= (:worker args) (str (:worker enrolled)))))
             (doseq [party [(:member w) (:non-member w) (:admin w)]]
               (is (thrown? clojure.lang.ExceptionInfo
                            (dops/invoke host (ops/party-principal party)
                                         :simmis/local-worker-enroll (assoc args :worker (str (random-uuid)))))))
             (is (thrown? clojure.lang.ExceptionInfo
                          (dops/invoke host (ops/party-principal (:owner w)) :simmis/workflow-list
                                       {:room (str (random-uuid))})))
             ;; A UUID whose row no longer exists must not resolve a same-
             ;; spelled Dvergr slug, and an intervening retarget must refuse.
             (is (nil? (api/room-handle (str (random-uuid)))))
             (let [unknown (str (random-uuid))]
               (with-redefs [dops/resolve-room (fn [_ handle] (when (= unknown handle) room))]
                 (is (thrown? clojure.lang.ExceptionInfo (api/room! host unknown)))
                 (is (nil? (dops/resolve-resource host (api/room-resource {:room unknown}))))))
             (with-redefs [api/room-handle (constantly slug)]
               (is (thrown? clojure.lang.ExceptionInfo (api/room! host (str (:other-room w))))))
             (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:owner w)]]
                               [:db/add [:room/id (:room w)] :room/owner [:party/id (:non-member w)]]])
             (is (thrown? clojure.lang.ExceptionInfo
                          (dops/invoke host (ops/party-principal (:owner w)) :simmis/local-worker-enroll args))))
           (finally
             (doseq [op (concat (keys api/specifications) (keys control/specifications))]
               (if-let [spec (get previous op)] (dops/register-op! op spec) (dops/unregister-op! op)))
             (server/sync-op-tools!))))))))
