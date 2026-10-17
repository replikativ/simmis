(ns is.simm.authority.local-workflows-remote-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.mcp.server :as server]
            [dvergr.ops :as dops]
            [dvergr.room.store :as store]
            [is.simm.agents.room-agents :as rooms]
            [is.simm.authority.world :as world]
            [is.simm.model.workers :as workers]
            [is.simm.ops.work-test]
            [is.simm.ops.worker-benchmarks :as benchmarks]
            [is.simm.ops.worker-control :as control]
            [is.simm.ops.worker-files-test :as fixture]
            [is.simm.ops.worker-service :as service]
            [is.simm.ops.worker-work :as work]
            [is.simm.ops.dvergr :as ops]
            [is.simm.runtimes.context :as context]
            [is.simm.runtimes.worker-maintenance :as maintenance]
            [is.simm.uis.web.desktop.workflows-remote]))

(deftest local-browser-bindings-use-current-human-and-canonical-room
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [room]
       (let [previous (dops/operations)
             principal (ops/party-principal (:owner w))
             worker (random-uuid) bundle (random-uuid) seen (atom [])
             request {:request-id (str (random-uuid)) :worker (str worker)
                      :name "echo" :expected-bundle (str bundle)
                      :limits {:timeout-ms 300000 :settlement "review"}}
             record (fn [& args] (swap! seen conj args) {:job (random-uuid)})
             actual-workflow work/submit-workflow! actual-benchmark benchmarks/submit!]
         (d/transact conn [{:room/id (:room w) :room/slug (store/room-id->slug (:id room))}])
         (service/enroll! conn principal (:room w) "local-browser-fixture" worker fixture/config)
         (try
           (control/install!)
           (with-redefs [context/server-context (:ctx room)
                         rooms/live-room #(when (= % (:room w)) room)
                         control/admission-open? (constantly true)
                         maintenance/claims-ready? (constantly true)
                         work/submit-workflow! record benchmarks/submit! record work/provision! record]
             (let [inbound (fn [kind op args]
                             (world/inbound! (world/principal w kind) op
                                             (merge {:room-id-str (str (:room w))} args)))
                   listing (inbound :owner "load-local-workflow-workers!" {:options {}})]
               (is (= [worker] (mapv :worker (get-in listing [:result :workers]))))
               (doseq [[op args] [["submit-local-workflow-run!" {:request request}]
                                 ["submit-local-workflow-benchmark!"
                                  {:request (-> request (assoc :repetitions 1)
                                                (assoc-in [:limits :settlement] "discard"))}]
                                 ["provision-local-workflow-allowance!"
                                  {:request {:request-id (str (random-uuid)) :max-worker-admissions 2}}]]]
                 (is (:result (inbound :owner op args)))
                 (doseq [other [:member :non-member :admin :anon]]
                   (is (:error (inbound other op args)))))
               (is (= 3 (count @seen)))
               (doseq [args @seen]
                 (is (= [conn (:ctx room) principal (:room w) room] (vec (take 5 args)))))
               (doseq [args (take 2 @seen)]
                 (is (= worker (:worker (last args))))
                 (is (= bundle (:expected-bundle (last args)))))
               (doseq [other [:member :non-member :admin :anon]]
                 (is (:error (inbound other "load-local-workflow-workers!" {:options {}}))))
               (is (:error (inbound :owner "load-local-workflow-workers!" {:options {:by (str (:member w))}})))
               (is (:error (inbound :owner "submit-local-workflow-run!" {:request (assoc request :model "override")})))
               (is (:result (inbound :owner "submit-local-workflow-run!"
                                     {:request (assoc request :room (str (:other-room w)))})))
               (is (= (:room w) (nth (last @seen) 3)))
               (with-redefs [work/submit-workflow! actual-workflow benchmarks/submit! actual-benchmark]
                 ;; A normal room owner is not an installation operator.
                 (is (:error (inbound :owner "submit-local-workflow-benchmark!"
                                      {:request (-> request (assoc :repetitions 1)
                                                    (assoc-in [:limits :settlement] "discard"))})))
                 (doseq [[by room-id] [[(:member w) (:room w)] [(:owner w) (:other-room w)]]]
                   (let [foreign (random-uuid)]
                     (workers/enroll! conn {:id foreign :by by :room room-id
                                           :secret (workers/new-secret) :config fixture/config})
                     (is (:error (inbound :owner "submit-local-workflow-run!"
                                          {:request (assoc request :worker (str foreign))})))))
                 (workers/revoke! conn worker)
                 (is (empty? (get-in (inbound :owner "load-local-workflow-workers!" {:options {}})
                                     [:result :workers])))
                 (is (:error (inbound :owner "submit-local-workflow-run!" {:request request}))))
               (with-redefs [control/admission-open? (constantly false)
                             maintenance/claims-ready? (constantly false)]
                 (is (:result (inbound :owner "load-local-workflow-workers!" {:options {}})))
                 (doseq [op ["submit-local-workflow-run!" "provision-local-workflow-allowance!"]]
                   (is (:error (inbound :owner op {:request (if (= op "submit-local-workflow-run!") request
                                                                               {:request-id (str (random-uuid)) :max-worker-admissions 1})})))))))
           (finally
             (doseq [op (keys control/specifications)]
               (if-let [spec (get previous op)] (dops/register-op! op spec) (dops/unregister-op! op)))
             (server/sync-op-tools!))))))))
