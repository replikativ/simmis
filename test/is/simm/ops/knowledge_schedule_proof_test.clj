(ns is.simm.ops.knowledge-schedule-proof-test
  "Actual frozen evidence, retained deployments and serialized joint linkage.
   Provider preparation and Job bodies are model-free. Actual native Jobs
   exercise admission/settlement; no provider or live dispatcher starts."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [dvergr.jobs :as jobs]
            [dvergr.resource :as resource]
            [org.replikativ.spindel.core :as sp]
            [org.replikativ.spindel.engine.core :as ec]
            [geschichte.content :as content]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-schedule-batches :as scheduled]
            [is.simm.model.workflow-schedules :as schedules]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.knowledge-batch-proof :as proof]
            [is.simm.ops.knowledge-batch-proof-test :as fixture]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.dvergr :as principals]
            [is.simm.ops.work :as work]
            [is.simm.ops.work-test :as work-fixture]
            [is.simm.ops.workflows :as workflows]))

(defn- with-schedule [f]
  (@#'fixture/fixture
   (fn [{:keys [conn w ready submission prepared] :as data}]
     (let [target {:version 2 :kind :workflow :room (:room w) :by (:owner w)
                   :cadence {:interval-ms 60000 :tz "UTC"} :execution (dissoc submission :request-id)
                   :host-policy {:source :owned-retained-source-v2}
                   :model-policy {(:model submission) (random-uuid)} :creation-channel :host
                   :snapshot (:snapshot ready) :source-origin (get-in prepared [:basis :source-origin])}
           a (schedules/create! conn {:id (random-uuid) :room (:room w) :by (:owner w) :target target
                                      :now (java.util.Date. 0) :next-fire (java.util.Date. 60000)})
           _ (schedules/claim-due! conn a (:cadence target) (java.util.Date. 60000))
           o (first (schedules/occurrences conn (:schedule.activation/id a)))
           claim (schedules/admit! conn (:schedule.occurrence/id o))
           value (scheduled/binding-value
                  {:activation (:schedule.activation/id a) :revision (:schedule.activation/revision a)
                   :target-ref (:schedule.activation/target-ref a) :by (:owner w) :room (:room w)
                   :occurrence (:schedule.occurrence/id o) :claim claim :batch (:batch-origin ready)})
           ack (scheduled/retain! conn value)
           anchor {:value value :reference (:schedule.occurrence/batch-ref ack)}
           input (-> (:basis prepared)
                     (assoc-in [:submission :request-id] (:occurrence value))
                     (assoc :knowledge/schedule {:binding (:id value)}
                            :schedule-origin {:activation (:activation value) :revision (:revision value)
                                              :occurrence (:occurrence value) :creation-channel :host}))]
       (f (assoc data :target target :activation a :value value :anchor anchor :intent input))))))

(defn- joint! [{:keys [conn w intent anchor]}]
  (let [p (requests/prepare-claim {:party (:owner w) :room (:room w) :client-id (get-in intent [:submission :request-id])
                                   :kind :workflow :intent intent
                                   :limits (update (get-in intent [:submission :limits]) :settlement keyword)})
        {:keys [row claim]} (requests/claim-row p)]
    (artifact/publish-value!
     (artifact/datahike-store conn) (:intent p)
     (fn [ref]
       (d/transact conn
                   [[:db.fn/call
                     (fn [db]
                       (let [row (assoc row :work.request/payload-ref ref)]
                         (vec (concat (scheduled/request-tx db anchor row intent)
                                      (requests/claim-tx db row (:digest p))
                                      (batches/request-tx db (:knowledge/batch intent) row (:digest p))))))]])))
    (:request (requests/claim-result @conn p claim))))

(deftest scheduled-proof-verifies-original-deployment-and-complete-source-free-request
  (with-schedule
    (fn [{:keys [conn w intent anchor value] :as data}]
      (is (= anchor (proof/schedule-input! conn (:owner w) (:room w) intent)))
      (let [request (joint! data)
            forbidden (fn [& _] (throw (AssertionError. "Scheduled history may not open live source/provider")))]
        (with-redefs [workflows/snapshot-in forbidden content/read-by-id forbidden work/prepare forbidden
                      reads/resolve-host-read! forbidden reads/resolve-host-room! forbidden]
          (is (= (:batch value) (:origin (proof/request! conn request)))))
        ;; A newer deployment/active slot does not replace the historical one.
        (d/transact conn [{:db/id [:schedule.activation/id (:activation value)]
                           :schedule.activation/revision (random-uuid) :schedule.activation/target-ref (random-uuid)
                           :schedule.activation/active-occurrence (random-uuid)}
                          {:db/id [:knowledge.batch/id (get-in value [:batch :batch])] :knowledge.batch/state :closed}])
        (with-redefs [workflows/snapshot-in forbidden work/prepare forbidden]
          (is (= (:batch value) (:origin (proof/request! conn request)))))
        (d/transact conn [[:db/retract [:schedule.occurrence/id (:occurrence value)]
                           :schedule.occurrence/request (:work.request/id request)]])
        (is (thrown? Exception (proof/request! conn request)))))))

(deftest retained-schedule-checks-exact-submission-source-channel-and-markers
  (with-schedule
    (fn [{:keys [conn w intent]}]
      (doseq [changed [(dissoc intent :schedule-origin) (dissoc intent :knowledge/schedule)
                       (assoc-in intent [:schedule-origin :creation-channel] :mcp)
                       (assoc-in intent [:submission :model] "a different provider")
                       (assoc-in intent [:submission :limits :max-dispatches] 1)
                       (assoc-in intent [:source-origin :revision] (random-uuid))
                       (assoc-in intent [:source "README.md"] "Different source")
                       (assoc-in intent [:submission :inputs] {"/in/extra" "Unretained input"})]]
        (is (thrown? Exception (proof/schedule-input! conn (:owner w) (:room w) changed))))
      (is (thrown? Exception (proof/schedule-input! conn (:member w) (:room w) intent))))))

(deftest schedule-proof-refuses-a-changed-canonical-binding-artifact
  (with-schedule
    (fn [{:keys [conn w value intent]}]
      (d/transact conn [{:db/id [:schedule.occurrence/id (:occurrence value)] :schedule.occurrence/batch-ref (random-uuid)}])
      (is (thrown? Exception (proof/schedule-input! conn (:owner w) (:room w) intent))))))

(deftest actual-native-claim-kernel-composes-the-scheduled-occurrence-pointer
  (with-schedule
    (fn [{:keys [conn host room w prepared intent anchor value]}]
      (let [prepared (assoc prepared :basis intent :request-kind :workflow :knowledge/schedule-binding anchor)
            args [conn (:execution-ctx host) (principals/party-principal (:owner w)) (:room w) room
                  (:submission intent) prepared]
            result (apply @#'work/claim-batch-prepared! args)
            request (:request result)]
        (is (true? (:claimed? result)))
        (is (= (:work.request/id request)
               (:schedule.occurrence/request (schedules/occurrence conn (:occurrence value)))))
        (is (= (:batch value) (:origin (proof/request! conn request))))
        (is (false? (:claimed? (apply @#'work/claim-batch-prepared! args))))
        (is (nil? (:work.request/job request)))
        ;; Native handoff checks the same original occurrence after insertion,
        ;; without incorrectly requiring its now-submitted batch to be ready.
        (is (= (:owner w) (apply @#'work/batch-admission-current! (concat (take 5 args) [prepared]))))
        (d/transact conn [{:db/id [:schedule.occurrence/id (:occurrence value)] :schedule.occurrence/claim (random-uuid)}])
        (is (thrown? Exception (apply @#'work/batch-admission-current! (concat (take 5 args) [prepared]))))))))

(deftest scheduled-native-job-handoff-rechecks-the-original-claim
  (doseq [damage [:none :claim :occurrence-link :batch-link :request-intent]]
    (with-schedule
      (fn [{:keys [conn host room w prepared intent value]}]
        (let [retire? (not= :none damage)
              started (atom 0) original-intent (atom nil)
              start-job! jobs/start! grant (work/resources fixture/limits)
              prepared (assoc prepared :basis intent :request-kind :workflow
                              :start! (fn [_] (swap! started inc) (sp/spin :done)))
              submit #(work/submit-prepared! conn (:execution-ctx host) (principals/party-principal (:owner w))
                                             (:room w) room (:submission intent) prepared)]
          (binding [ec/*execution-context* (:ctx room)]
            (resource/mint! room {:id (random-uuid) :resources grant}))
          (with-redefs [jobs/start!
                        (fn [& args]
                          (let [rid (requests/request-id (:owner w) (:room w) (:occurrence value))]
                            (case damage
                              :none nil
                              :claim (d/transact conn [{:db/id [:schedule.occurrence/id (:occurrence value)]
                                                        :schedule.occurrence/claim (random-uuid)}])
                              :occurrence-link (d/transact conn [[:db/retract [:schedule.occurrence/id (:occurrence value)]
                                                                  :schedule.occurrence/request rid]])
                              :batch-link (d/transact conn [{:db/id [:knowledge.batch/id (get-in value [:batch :batch])]
                                                             :knowledge.batch/request-intent (random-uuid)}])
                              :request-intent
                              (do
                                (reset! original-intent (:work.request/intent-id (requests/get-request conn rid)))
                                (d/transact conn [{:db/id [:work.request/id rid]
                                                   :work.request/intent-id (random-uuid)}]))))
                          (apply start-job! args))]
            (if retire?
              (is (thrown? Exception (submit)))
              (let [result (submit) status (jobs/status (:job result) 15000)]
                (is (= :completed (:status status)))
                (is (:settled? status))
                (is (= (:batch value) (:origin (proof/request! conn (requests/get-request conn (:id result)))))))))
          (when (= :request-intent damage)
            (let [rid (requests/request-id (:owner w) (:room w) (:occurrence value))
                  job (:work.request/job (requests/get-request conn rid))
                  reconcile! #(d/transact conn [{:db/id [:work.request/id rid]
                                                 :work.request/intent-id @original-intent}])]
              (try
                ;; A tampered canonical intent holds cleanup and its lease.
                ;; HOST must restore that exact identity before recovery.
                (let [held (@#'work-fixture/await-job-cleanup job)]
                  (is (= :before-finish (:stage held)))
                  (is (:work.request/return-pending? (requests/get-request conn rid)))
                  (reconcile!)
                  (is (:settled? (jobs/recover! job held :retry)))
                  (is (false? (:work.request/return-pending? (requests/get-request conn rid)))))
                (finally
                  (when-let [remaining (jobs/recovery-state job)]
                    (reconcile!)
                    (jobs/recover! job remaining :retry))))))
          (is (= (if retire? 0 1) @started))
          (is (= grant (binding [ec/*execution-context* (:ctx room)] (resource/balance room)))))))))
