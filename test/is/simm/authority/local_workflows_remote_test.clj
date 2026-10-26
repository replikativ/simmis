(ns is.simm.authority.local-workflows-remote-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.mcp.server :as server]
            [dvergr.ops :as dops]
            [is.simm.agents.room-agents :as rooms]
            [is.simm.authority.world :as world]
            [is.simm.authority.application-fixture :as application-fixture]
            [is.simm.model.workers :as workers]
            [is.simm.ops.owned-worker-admission-test :as owned-fixture]
            [is.simm.ops.owned-worker-admission :as owned]
            [is.simm.ops.worker-control :as control]
            [is.simm.ops.worker-files-test :as fixture]
            [is.simm.ops.worker-work :as work]
            [is.simm.ops.dvergr :as ops]
            [is.simm.runtimes.context :as context]
            [is.simm.runtimes.worker-maintenance :as maintenance]
            [is.simm.uis.web.desktop.workflows-remote]))

(deftest local-browser-bindings-use-current-human-and-canonical-room
  (doseq [order [true false]]
    (@#'owned-fixture/with-worker
     order
     (fn [{:keys [conn w room worker] :as data}]
      (application-fixture/with-application conn (:ctx room)
       (fn [_]
       (let [previous (dops/operations)
             principal (ops/party-principal (:owner w))
             internal ((:request data) :local-worker-workflow)
             bundle (:expected-bundle internal) seen (atom [])
             request (reduce (fn [input field] (update input field str)) internal
                             [:request-id :worker :expected-bundle :expected-owned-source])
             record (fn [& args] (swap! seen conj args) {:job (random-uuid)})
             actual-workflow owned/submit-workflow! actual-benchmark owned/submit-benchmark!]
         (try
           (control/install!)
           (with-redefs [context/server-context (:ctx room)
                         rooms/live-room #(when (= % (:room w)) room)
                         control/admission-open? (constantly true)
                         maintenance/claims-ready? (constantly true)
                         owned/submit-workflow! record owned/submit-benchmark! record work/provision! record]
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
               (doseq [args (take 2 @seen)]
                 (is (identical? conn (get-in (first args) [:is.simm.ops.owned-reads/owner :control-conn])))
                 (is (= [principal (str (:room w))] (vec (take 2 (rest args)))))
                 (is (= worker (:worker (last args))))
                 (is (= bundle (:expected-bundle (last args))))
                 (is (= (:expected-owned-source internal) (:expected-owned-source (last args)))))
               (is (= [conn (:ctx room) principal (:room w) room] (vec (take 5 (nth @seen 2)))))
               (doseq [other [:member :non-member :admin :anon]]
                 (is (:error (inbound other "load-local-workflow-workers!" {:options {}}))))
               (is (:error (inbound :owner "load-local-workflow-workers!" {:options {:by (str (:member w))}})))
               (is (:error (inbound :owner "submit-local-workflow-run!" {:request (assoc request :model "override")})))
               (is (:error (inbound :owner "submit-local-workflow-run!"
                                    {:request (assoc request :room (str (:other-room w)))})))
               (is (= 3 (count @seen)))
               (with-redefs [owned/submit-workflow! actual-workflow owned/submit-benchmark! actual-benchmark]
                 ;; A normal room owner is not an installation operator.
                 (d/transact conn [[:db/retract [:party/id (:owner w)] :party/role :admin]])
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
             (server/sync-op-tools!))))))))))
