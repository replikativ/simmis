(ns is.simm.ops.knowledge-batch-execution-test
  "Real native workflow fork/evaluation/Attempt and filing without an LLM.
   Provider preparation and registry-to-memory provisioning are fixture seams;
   native execution, certification and filing are real."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.agent.roster :as roster]
            [dvergr.jobs :as jobs]
            [dvergr.resource :as resource]
            [dvergr.runtime.preparation :as preparation]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.knowledge-processing :as processing]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.system-db :as sdb]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.knowledge-batch-proof-test :as base]
            [is.simm.ops.owned-knowledge-admission-test :as fixture]
            [is.simm.ops.owned-workflow-admission :as admission]
            [is.simm.ops.knowledge-publish :as publish]
            [is.simm.ops.proposal-publication :as publication]
            [is.simm.ops.work :as work]
            [org.replikativ.spindel.engine.core :as ec]))

(deftest native-batch-workflow-forks-certifies-and-files-a-no-op-without-model-calls
  (@#'fixture/fixture
   (fn [{:keys [conn host room w principal handle input]}]
     (d/transact conn (into dispositions/schema processing/schema))
     (let [input (assoc-in input [:limits :settlement] "discard")
           grant (work/resources (:limits input))
           team (roster/make-agent (roster/make-roster {:id :batch-execution/team})
                                   {:id :candidate :program {:kind :echo}})]
       (with-redefs [work/prepare (fn [input]
                                   (is (empty? (get @(:meta room) preparation/leases-key)))
                                   (-> (@#'base/model-free-preparation input)
                                       (assoc :team team :agent :candidate)
                                       (assoc-in [:basis :candidate] (roster/agent team :candidate))))
                     sdb/get-conn (constantly conn)
                     publication/publish! (fn [& _] (throw (AssertionError. "No-op has no Proposal")))]
         (let [result (admission/submit! host principal handle input)
               status (jobs/status (:job result) 25000)]
           (is (= :completed (:status status)))
           (is (:settled? status))
           (let [row (requests/get-request conn (:id result))
                 value (publish/file-completed-request! conn principal (:room w) room (:id result))
                 batch (batches/batch @conn (:work.request/knowledge-batch row))]
             (is (= :no-op (:kind value)))
             (is (uuid? (:candidate value)))
             (is (uuid? (:attempt-content value)))
             (is (= (:id value) (:knowledge.batch/disposition batch)))
             (is (= :closed (:knowledge.batch/state batch)))
             (is (= :no-op (:knowledge.batch/outcome batch)))
             (is (nil? (:knowledge.stream/active-batch (streams/stream @conn (:knowledge.batch/stream batch)))))
             (is (= 1 (count (filter #(and (= (:knowledge.batch/id batch) (:knowledge.item/batch %))
                                           (= :processed (:knowledge.item/state %)))
                                      (streams/items @conn (:knowledge.batch/stream batch))))))
             (is (= 1 (count (streams/pending-items @conn (:knowledge.batch/stream batch)))))
             (is (= grant (binding [ec/*execution-context* (:ctx room)] (resource/balance room)))))))))))
