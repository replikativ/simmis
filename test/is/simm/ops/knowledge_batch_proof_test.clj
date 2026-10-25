(ns is.simm.ops.knowledge-batch-proof-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.agent.environment :as environment]
            [dvergr.artifact :as artifact]
            [geschichte.content :as content]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.knowledge-batch-proof :as proof]
            [is.simm.ops.owned-knowledge-batches-test :as fixture]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflow-runs :as workflows]
            [is.simm.ops.workflows :as source]))

(def limits {:budget-microdollars 10000 :max-dispatches 4 :max-model-steps 4 :timeout-ms 30000 :settlement "review"})

(defn- model-free-preparation [{:keys [task limits]}]
  ;; Exercise the real workflow/input/environment construction without looking
  ;; up or preparing any provider. This fixture never starts a Job or model.
  {:basis {}
   :environment (environment/make-environment
                  {:id :test/model-free-batch :task task
                   :verifier {:id :simmis/work-completion :version 1 :basis "simmis-work-completion-v1"}
                   :world {:isolation :ctx :settlement :review :effects work/effects :resources (work/resources limits)}
                   :limits {:timeout-ms 30000}})
   :evaluator work/completion})

(defn- fixture [f]
  (@#'fixture/with-fixture
   (fn [{:keys [conn batch-input] :as data}]
     (d/transact conn requests/schema)
     (let [ready (@#'fixture/prepare data)
           submission {:request-id (random-uuid) :name "wiki" :expected-bundle (:expected-bundle batch-input)
                       :expected-owned-source (:expected-source batch-input)
                       :model "provider-must-not-be-resolved" :limits limits}
           original (:source-origin ready)
           prepared (with-redefs [work/prepare model-free-preparation]
                      (workflows/bind-publication
                       (workflows/prepare (:snapshot ready) (dissoc submission :expected-owned-source) (:captured ready))
                       {:captured (:captured ready) :publication-basis (:publication-basis ready)}))
           prepared (-> prepared
                        (assoc-in [:basis :submission] submission)
                        (assoc-in [:basis :knowledge/batch] (:batch-origin ready))
                        (assoc-in [:basis :source-origin]
                                  {:revision (:source-revision original) :target-stamp (:target-stamp original)
                                   :target (:target original) :manifest (get-in ready [:snapshot :manifest])}))]
       (f (assoc data :ready ready :prepared prepared :submission submission))))))

(defn- joint-claim! [{:keys [conn w prepared submission]}]
  (let [p (requests/prepare-claim {:party (:owner w) :room (:room w) :client-id (:request-id submission)
                                   :kind :workflow :intent (:basis prepared) :limits (update limits :settlement keyword)})
        {:keys [row claim]} (requests/claim-row p)
        origin (get-in prepared [:basis :knowledge/batch])]
    (artifact/publish-value! (artifact/datahike-store conn) (:intent p)
      (fn [ref]
        (d/transact conn [[:db.fn/call (fn [db]
                                      (let [row (assoc row :work.request/payload-ref ref)]
                                        (into (requests/claim-tx db row (:digest p))
                                              (batches/request-tx db origin row (:digest p)))))]])))
    (:request (requests/claim-result @conn p claim))))

(deftest ready-proof-matches-real-frozen-program-inputs-and-publication-basis
  (fixture
    (fn [{:keys [conn w ready prepared]}]
      (let [p (proof/ready! conn (:owner w) (:room w) (:batch-origin ready))]
        (is (= p (proof/verify-input! p (:basis prepared))))
        (is (thrown? Exception (proof/verify-input! p (assoc-in (:basis prepared) [:inputs "/in/knowledge/sources/inbox/meeting.md"] "Forged"))))
        (is (thrown? Exception (proof/verify-input! p (assoc-in (:basis prepared) [:environment :environment/task] "Different task"))))
        (is (thrown? Exception (proof/verify-input! p (assoc-in (:basis prepared) [:source "fixtures/input.md"] "Different fixture"))))
        (is (thrown? Exception (proof/ready! conn (:member w) (:room w) (:batch-origin ready))))))))

(deftest historical-proof-is-source-free-and-checks-both-sides-of-request-linkage
  (fixture
    (fn [{:keys [conn w ready] :as data}]
      (let [row (joint-claim! data)
            forbidden (fn [& _] (throw (AssertionError. "Historical proof must not resolve live source or provider")))]
        (with-redefs [source/snapshot-in forbidden content/read-by-id forbidden work/prepare forbidden]
          (is (= (:batch-origin ready) (:origin (proof/request! conn row)))))
        (let [actual (batches/batch @conn (:batch ready))]
          (d/transact conn [[:db/retract [:knowledge.stream/id (:knowledge.batch/stream actual)]
                            :knowledge.stream/active-batch (:batch ready)]])
          (is (= (:batch-origin ready) (:origin (proof/request! conn row)))))
        (is (thrown? Exception (proof/ready! conn (:owner w) (:room w) (:batch-origin ready))))
        (let [actual (batches/batch @conn (:batch ready))]
          (d/transact conn [[:db/retract (:db/id actual) :knowledge.batch/request (:work.request/id row)]])
          (is (thrown? Exception (proof/request! conn row))))))))

(deftest detached-ready-batches-fail-the-pre-provider-gate
  (doseq [damage [:missing-active :wrong-active :missing-stream]]
    (fixture
      (fn [{:keys [conn w ready]}]
        (d/transact conn
          [(case damage
             :missing-active [:db/retract [:knowledge.stream/id (:stream ready)] :knowledge.stream/active-batch (:batch ready)]
             :wrong-active [:db/add [:knowledge.stream/id (:stream ready)] :knowledge.stream/active-batch (random-uuid)]
             :missing-stream [:db.fn/retractEntity [:knowledge.stream/id (:stream ready)]])])
        (is (thrown? Exception (proof/ready! conn (:owner w) (:room w) (:batch-origin ready))))))))

(deftest a-stale-request-projection-cannot-hide-corrupted-canonical-markers
  (doseq [field [:work.request/publication-basis-id :work.request/knowledge-frozen]]
    (fixture
      (fn [{:keys [conn] :as data}]
        (let [row (joint-claim! data)]
          (d/transact conn [{:db/id (:db/id row) field (random-uuid)}])
          (is (thrown? Exception (proof/request! conn row))))))))

(deftest ordinary-claim-cannot-manufacture-a-live-batch-request
  (fixture
    (fn [{:keys [conn w prepared submission]}]
      (is (= :is.simm.model.work-requests/batch-needs-joint-admission
             (try (requests/claim! conn {:party (:owner w) :room (:room w) :client-id (:request-id submission)
                                        :kind :workflow :intent (:basis prepared) :limits limits}) nil
                  (catch clojure.lang.ExceptionInfo error (:type (ex-data error)))))))))

(deftest self-rehashed-request-data-still-must-match-the-frozen-batch
  (fixture
    (fn [{:keys [conn] :as data}]
      (let [row (joint-claim! data)
            retained (requests/payload conn row)
            changed (assoc-in retained [:input :inputs "/in/knowledge/sources/inbox/meeting.md"] "Changed evidence")
            digest (hasch/uuid changed)
            batch (batches/batch @conn (:work.request/knowledge-batch row))]
        (is (thrown? Exception (proof/request! conn row changed)))
        ;; Rehashing both metadata digests is still no proof of input parity.
        (d/transact conn [{:db/id (:db/id row) :work.request/intent-id digest}
                          {:db/id (:db/id batch) :knowledge.batch/request-intent digest}])
        (is (thrown? Exception (proof/request! conn (requests/get-request conn (:work.request/id row)) changed)))))))
