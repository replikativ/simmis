(ns is.simm.ops.worker-work-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.run :as run]
            [dvergr.jobs :as jobs]
            [dvergr.room.store :as store]
            [dvergr.resource :as resource]
            [hasch.core :as hasch]
            [is.simm.authority.world :as world]
            [is.simm.model.work-requests :as requests]
            [is.simm.model.workers :as workers]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.work :as work]
            [is.simm.ops.knowledge-admission :as knowledge-admission]
            [is.simm.ops.work-test :as work-fixture]
            [is.simm.ops.worker-credentials :as credentials]
            [is.simm.ops.worker-files-test :as file-fixture]
            [is.simm.ops.worker-service :as service]
            [is.simm.ops.worker-wait :as wait]
            [is.simm.ops.worker-work :as local]
            [is.simm.ops.workflow-runs-test :as workflow-fixture]
            [is.simm.ops.workflows :as workflows]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- observe-assignment [conn]
  (let [key (random-uuid) published (promise)]
    (d/listen conn key
              (fn [report]
                (doseq [datom (:tx-data report)
                        :when (and (= :simmis.worker.assignment/id (:a datom)) (:added datom))]
                  (deliver published (:v datom)))))
    {:id published :close! #(d/unlisten conn key)}))

(defn- with-worker [conn w f]
  (@#'work-fixture/with-room
   (fn [room]
     (workspace/ensure-workspace! room)
     (workspace/seed! room {"/docs/input.md" "original" "/private.txt" "not selected"})
     (let [principal (ops/party-principal (:owner w)) id (random-uuid)
           enrolled (service/enroll! conn principal (:room w) "worker-work-fixture" id file-fixture/config)
           input {:request-id (random-uuid) :worker id :task "Write the selected input and out/result.md"
                  :paths ["docs/input.md"] :limits (assoc file-fixture/limits :settlement "discard")}
           allowance {:request-id (random-uuid) :max-worker-admissions 2}
           watcher (observe-assignment conn)]
       (try (f {:room room :host-ctx ec/*execution-context* :principal principal :enrolled enrolled
                :input input :allowance allowance :published (:id watcher)})
            (finally ((:close! watcher))))))))

(defn- claim! [conn enrolled id]
  (let [proof (workers/new-secret)
        principal (credentials/authenticate conn {:bearer (:credential enrolled)
                                                  :headers {"simmis-lease-proof" proof}})
        claimed (service/claim! conn principal {:run (str id) :nonce (str (random-uuid))})]
    (credentials/authenticate conn {:bearer (credentials/lease-token id (:lease claimed) proof)})))

(defn- stored-attempt [room]
  (when-let [entry (first (store/-list-attempts (:store room) (store/conversation-id room) {}))]
    (store/-load-attempt (:store room) (store/conversation-id room) (:attempt/id entry))))

(deftest worker-workflow-retains-host-publication-binding-before-any-execution
  (world/with-world [conn w]
    (with-worker conn w
      (fn [{:keys [room host-ctx principal input]}]
        (workspace/seed! room {"/inbox/evidence.md" "Evidence" "/wiki/context.md" "Prior knowledge"})
        (workflows/write! room {:principal/id "fixture"}
                          {:name "echo" :files workflow-fixture/knowledge-source})
        (let [request (merge (select-keys input [:request-id :worker :limits])
                             (select-keys (workflow-fixture/input workflow-fixture/knowledge-source)
                                          [:name :expected-bundle]))]
          (with-redefs [knowledge-admission/default-target workflow-fixture/fixture-default-target
                        work/submit-prepared!
                        (fn [_ _ _ _ _ admitted prepared]
                          (:request (requests/claim!
                                     conn {:party (:owner w) :room (:room w)
                                           :client-id (:request-id admitted)
                                           :kind (:request-kind prepared) :intent (:basis prepared)})))]
            (let [row (local/submit-workflow! conn host-ctx principal (:room w) room request)
                  basis (requests/publication-basis conn (:work.request/id row))]
              (is (= :local-worker-workflow (:work.request/kind row)))
              (is (contains? (:files basis) "wiki/context.md"))
              (is (= (:id basis) (:work.request/publication-basis-id row)))
              (is (nil? (:work.request/job row))))))))))

(deftest production-adapter-completes-one-funded-task-and-retries-never-repeat-it
  (world/with-world [conn w]
    (with-worker conn w
      (fn [{:keys [room host-ctx principal enrolled input allowance published]}]
        (let [provisioned (local/provision! conn host-ctx principal (:room w) room allowance)]
          (is (= provisioned (local/provision! conn host-ctx principal (:room w) room allowance)))
          (is (= 2M (get (resource/balance room) local/admission-unit)))
          (let [submitted (local/submit! conn host-ctx principal (:room w) room input)
                id (deref published 10000 nil)
                lease (claim! conn enrolled id)
                output {:files {"docs/input.md" (file-fixture/encoded "updated")
                                "out/result.md" (file-fixture/encoded "answer")}
                        :summary "done" :exit-code 0}]
            (is (uuid? id))
            (is (:base64 (service/input conn lease {:run (str id)})))
            (service/complete! conn lease {:run (str id) :output output})
            (is (:settled? (jobs/status (:job submitted) 25000)))
            (let [attempt (stored-attempt room)]
              (is (= 1.0 (get-in attempt [:attempt/receipt :attempt/reward])))
              (is (= :local-worker (get-in attempt [:attempt/receipt :attempt/provider])))
              (is (= :host (get-in attempt [:attempt/receipt :attempt/metrics :submission-channel])))
              (is (= :local-worker (get-in attempt [:attempt/receipt :attempt/metrics :execution-kind])))
              (is (false? (get-in attempt [:attempt/receipt :attempt/metrics :spend :priced?])))
              (is (= (file-fixture/encoded "answer") (get-in attempt [:attempt/evidence :files "/out/result.md"])))
              (is (= {"docs/input.md" "updated" "out/result.md" "answer"}
                     (into {} (map (fn [[path file]] [(subs path 1)
                                                      (String. (.decode (java.util.Base64/getDecoder) ^String (:base64 file)) "UTF-8")]))
                           (get-in attempt [:attempt/evidence :files])))))
            (is (= "original" (get (workspace/read-tree room "/docs") "/docs/input.md")))
            (is (= 1M (get (resource/balance room) local/admission-unit)))
            (is (= {local/admission-unit 1M}
                   (:resources (store/-resource-receipt (:store room) (hasch/uuid [:simmis/local-worker-admission id])))))
            (is (= 1 (:max-worker-admissions (work/status conn host-ctx principal (:room w) (:id submitted)))))
            (is (false? (:return-pending? (work/status conn host-ctx principal (:room w) (:id submitted)))))
            (workspace/seed! room {"/docs/input.md" "newer parent content"})
            (workers/revoke! conn (:worker enrolled))
            (is (= (:job submitted) (:job (local/submit! conn host-ctx principal (:room w) room input))))
            (is (= 1 (count (jobs/jobs room))))
            (is (thrown? clojure.lang.ExceptionInfo
                         (local/submit! conn host-ctx principal (:room w) room (assoc input :task "different"))))))))))

(deftest native-money-and-dispatch-allowance-cannot-fund-local-worker-admissions
  (world/with-world [conn w]
    (with-worker conn w
      (fn [{:keys [room host-ctx principal input]}]
        (let [grant {resource/microdollars 10000M resource/model-dispatches 4M}]
          (resource/mint! room {:id (random-uuid) :resources grant})
          (is (thrown? clojure.lang.ExceptionInfo (local/submit! conn host-ctx principal (:room w) room input)))
          (let [id (requests/request-id (:owner w) (:room w) (:request-id input))
                submitted (work/status conn host-ctx principal (:room w) id)]
            (is (:settled? (jobs/status (:job submitted) 25000)))
            (is (false? (:return-pending? submitted)))
            (is (empty? (d/q '[:find [?id ...] :where [_ :simmis.worker.assignment/id ?id]] @conn)))
            (is (= 0M (get (resource/balance room) local/admission-unit 0M)))
            (is (= 10000M (get (resource/balance room) resource/microdollars)))
            (is (= 4M (get (resource/balance room) resource/model-dispatches)))))))))

(deftest worker-submission-and-allowance-remain-closed-human-owner-capabilities
  (world/with-world [conn w]
    (with-worker conn w
      (fn [{:keys [room host-ctx principal enrolled input allowance]}]
        (doseq [party [(:member w) (:non-member w) (:admin w)]]
          (is (thrown? clojure.lang.ExceptionInfo
                       (local/provision! conn host-ctx (ops/party-principal party) (:room w) room allowance)))
          (is (thrown? clojure.lang.ExceptionInfo
                       (local/submit! conn host-ctx (ops/party-principal party) (:room w) room input))))
        (let [worker (credentials/authenticate conn {:bearer (:credential enrolled)})]
          (is (thrown? clojure.lang.ExceptionInfo (local/provision! conn host-ctx worker (:room w) room allowance)))
          (is (thrown? clojure.lang.ExceptionInfo (local/submit! conn host-ctx worker (:room w) room input))))
        (doseq [extra [{:resources {local/admission-unit 1000}} {:eval true} {:by (:owner w)}]]
          (is (thrown? clojure.lang.ExceptionInfo
                       (local/provision! conn host-ctx principal (:room w) room (merge allowance extra))))
          (is (thrown? clojure.lang.ExceptionInfo
                       (local/submit! conn host-ctx principal (:room w) room (merge input extra)))))
        (is (thrown? clojure.lang.ExceptionInfo
                     (local/submit! conn host-ctx principal (:room w) room (assoc input :worker (random-uuid)))))
        (is (empty? (requests/requests conn)))
        (is (zero? (get (resource/balance room) local/admission-unit 0M)))))))

(deftest nonzero-exit-and-revocation-after-completion-cannot-certify-success
  (doseq [revoke? [false true]]
    (world/with-world [conn w]
      (with-worker conn w
        (fn [{:keys [room host-ctx principal enrolled input allowance published]}]
          (local/provision! conn host-ctx principal (:room w) room allowance)
          (let [await wait/await-stop!]
            (with-redefs [wait/await-stop! (fn [watch]
                                             (let [receipt (await watch)]
                                               (when revoke? (workers/revoke! conn (:worker enrolled)))
                                               receipt))]
              (let [submitted (local/submit! conn host-ctx principal (:room w) room input)
                    id (deref published 10000 nil) lease (claim! conn enrolled id)
                    diagnostic (file-fixture/encoded "diagnostic")]
                (try
                  (service/complete! conn lease {:run (str id)
                                                 :output {:files {"out/result.md" diagnostic}
                                                          :summary "finished" :exit-code (if revoke? 0 7)}})
                  (is (:settled? (jobs/status (:job submitted) 25000)))
                  (let [attempt (stored-attempt room)]
                    (is (zero? (get-in attempt [:attempt/receipt :attempt/reward])))
                    (is (= :failed (get-in attempt [:attempt/evidence :trace :runs 0 :run/status])))
                    (if revoke?
                      (do (is (= {} (get-in attempt [:attempt/evidence :files])))
                          (is (false? (get-in attempt [:attempt/evidence :worker :imported?]))))
                      (do (is (= diagnostic (get-in attempt [:attempt/evidence :files "/out/result.md"])))
                          (is (false? (get-in attempt [:attempt/receipt :attempt/checks :exit-zero?]))))))
                  (is (= {} (workspace/read-tree room "/out")))
                  (finally
                    (service/stop! conn lease {:run (str id)})
                    (jobs/status (:job submitted) 25000)))))))))))

(deftest production-cancellation-retains-physical-ownership-until-the-lease-stops
  (world/with-world [conn w]
    (with-worker conn w
      (fn [{:keys [room host-ctx principal enrolled input allowance published]}]
        (local/provision! conn host-ctx principal (:room w) room allowance)
        (let [submitted (local/submit! conn host-ctx principal (:room w) room input)
              id (deref published 10000 nil) lease (claim! conn enrolled id)
              key (random-uuid) cancelled (promise)]
          (d/listen conn key (fn [report]
                               (when (some #(and (= :simmis.worker.assignment/cancel-at (:a %)) (:added %))
                                           (:tx-data report))
                                 (deliver cancelled true))))
          (try
            (run/cancel-run! (:job submitted))
            (is (= true (deref cancelled 10000 nil)))
            (is (false? (:stop-ack? (service/heartbeat! conn lease {:run (str id)}))))
            (is (thrown? clojure.lang.ExceptionInfo
                         (evaluation/await-cleanups-for! room (:job submitted) 1)))
            (is (:return-pending? (work/status conn host-ctx principal (:room w) (:id submitted))))
            (service/stop! conn lease {:run (str id)})
            (is (:settled? (jobs/status (:job submitted) 25000)))
            (is (false? (:return-pending? (work/status conn host-ctx principal (:room w) (:id submitted)))))
            (is (= 1M (get (resource/balance room) local/admission-unit)))
            (finally
              (d/unlisten conn key)
              (service/stop! conn lease {:run (str id)})
              (jobs/status (:job submitted) 25000))))))))

(deftest ambiguous-publication-after-claim-needs-real-stop-before-settlement
  (world/with-world [conn w]
    (with-worker conn w
      (fn [{:keys [room host-ctx principal enrolled input allowance]}]
        (local/provision! conn host-ctx principal (:room w) room allowance)
        (let [enqueue workers/enqueue! owned (promise) key (random-uuid) cancelled (promise)]
          (d/listen conn key (fn [report]
                               (when (some #(and (= :simmis.worker.assignment/cancel-at (:a %)) (:added %))
                                           (:tx-data report))
                                 (deliver cancelled true))))
          (with-redefs [workers/enqueue! (fn [c assignment]
                                           (enqueue c assignment)
                                           (let [lease (claim! c enrolled (:id assignment))]
                                             (deliver owned {:assignment assignment :lease lease}))
                                           (throw (ex-info "response lost after claim" {})))]
            (let [submitted (local/submit! conn host-ctx principal (:room w) room input)
                  {:keys [assignment lease]} (deref owned 10000 nil)
                  id (:id assignment)]
              (try
                (is (= true (deref cancelled 10000 nil)))
                (is (= :quarantined (:simmis.worker.assignment/state (workers/assignment conn id))))
                (is (nil? (:simmis.worker.assignment/stop-ack-at (workers/assignment conn id))))
                (is (:return-pending? (work/status conn host-ctx principal (:room w) (:id submitted))))
                (service/stop! conn lease {:run (str id)})
                (is (:settled? (jobs/status (:job submitted) 25000)))
                (is (= :stopped (:simmis.worker.assignment/state (enqueue conn assignment))))
                (is (zero? (get-in (stored-attempt room) [:attempt/receipt :attempt/reward])))
                (finally
                  (d/unlisten conn key)
                  (when lease (service/stop! conn lease {:run (str id)}))
                  (jobs/status (:job submitted) 25000))))))))))

(deftest failed-publication-before-insert-is-closed-against-a-delayed-enqueue
  (world/with-world [conn w]
    (with-worker conn w
      (fn [{:keys [room host-ctx principal input allowance published]}]
        (local/provision! conn host-ctx principal (:room w) room allowance)
        (let [enqueue workers/enqueue! assignment (promise)]
          (with-redefs [workers/enqueue! (fn [_ input] (deliver assignment input)
                                           (throw (ex-info "publication failed before insert" {})))]
            (let [submitted (local/submit! conn host-ctx principal (:room w) room input)
                  original (deref assignment 10000 nil)
                  id (deref published 10000 nil)]
              (is (:settled? (jobs/status (:job submitted) 25000)))
              (is (= :withdrawn (:simmis.worker.assignment/state (workers/assignment conn id))))
              (is (:simmis.worker.assignment/stop-ack-at (workers/assignment conn id)))
              (is (= :withdrawn (:simmis.worker.assignment/state (enqueue conn original))))
              (is (zero? (get-in (stored-attempt room) [:attempt/receipt :attempt/reward])))
              (is (= 1M (get (resource/balance room) local/admission-unit))))))))))

(deftest known-revocation-before-publication-never-offers-work-to-the-worker
  (world/with-world [conn w]
    (with-worker conn w
      (fn [{:keys [room host-ctx principal enrolled input allowance published]}]
        (local/provision! conn host-ctx principal (:room w) room allowance)
        (let [consume resource/consume! enqueue workers/enqueue! offers (atom 0)]
          (with-redefs [resource/consume! (fn [& args]
                                            (let [receipt (apply consume args)]
                                              (binding [ec/*execution-context* host-ctx]
                                                (workers/revoke! conn (:worker enrolled)))
                                              receipt))
                        workers/enqueue! (fn [& args] (swap! offers inc) (apply enqueue args))]
            (let [submitted (local/submit! conn host-ctx principal (:room w) room input)
                  id (deref published 10000 nil)]
              (is (:settled? (jobs/status (:job submitted) 25000)))
              (is (zero? @offers))
              (is (= :withdrawn (:simmis.worker.assignment/state (workers/assignment conn id))))
              (is (:simmis.worker.assignment/stop-ack-at (workers/assignment conn id)))
              (is (zero? (get-in (stored-attempt room) [:attempt/receipt :attempt/reward]))))))))))
