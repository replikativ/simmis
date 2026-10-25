(ns is.simm.ops.knowledge-batch-adoption-test
  "Real native scoped publication and joint batch resolution. The Proposal
   fixture explicitly substitutes Attempt certification, not file adoption."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.rooms.publication :as scoped]
            [dvergr.catalog.workspace :as catalog]
            [dvergr.substrate.geschichte :as gs]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg]
            [yggdrasil.adapters.geschichte :as gy]
            [geschichte.repo :as repo]
            [geschichte.query :as query]
            [hasch.core :as hasch]
            [clojure.string :as str]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.files :as files]
            [is.simm.documents.space :as space]
            [is.simm.model.file-adoptions :as adoptions]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.file-output-controller :as controller]
            [is.simm.ops.knowledge-batch-filing-test :as fixture]
            [is.simm.ops.knowledge-filing :as filing]
            [is.simm.ops.knowledge-publish :as publish]))

(defn- canonical-memory-workspace! [room]
  ;; Shared earlier fixtures use a bare Geschichte workspace. Adoption checks
  ;; the canonical knowledge profile, so provision a genuinely NEW store before
  ;; registering it. Never relabel or migrate an already authored repository.
  (let [cfg {:attribute-refs? false :schema-flexibility :write
             :keep-history? true :branch-history? true
             :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0
             :store {:backend :memory :id (random-uuid)}}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (space/provision! conn)
      (binding [ec/*execution-context* (:ctx room)]
        (ygg/register! (gy/create conn {:system-name (str "room-canonical-test-" (random-uuid))})))
      conn)))

(defn- fixture [f]
  (let [write! repo/write!]
    (with-redefs [catalog/open-memory-workspace! canonical-memory-workspace!
                  repo/write!
                  (fn [conn path bytes & options]
                    ;; The production canonical writer assigns explicit page
                    ;; identities. Earlier raw-source fixtures do not; seed
                    ;; valid new documents before their first committed version.
                    (let [text (when (and (re-find #"\.(md|markdown)$" path) (bytes? bytes))
                                 (markdown/decode bytes))
                          bytes (if (and text (not (str/includes? text "simmis-id:")))
                                  (markdown/source-bytes
                                   (str "---\nsimmis-id: '"
                                        (hasch/uuid [(:geschichte.repo/id (query/repository @conn)) path])
                                        "'\n---\n" text))
                                  bytes)]
                      (apply write! conn path bytes options)))]
      (@#'fixture/proposed-fixture
       (fn [{:keys [conn] :as data}]
         (d/transact conn adoptions/schema)
         (f data))))))

(defn- accept! [{:keys [conn w room value]}]
  (controller/adopt! conn (:owner w) (:room w) room (:proposal value)))

(deftest native-acceptance-jointly-closes-the-batch-and-preserves-filing-history
  (fixture
   (fn [{:keys [conn w room ready owned value principal] :as data}]
     (let [result (accept! data)
           batch (batches/batch @conn (:batch ready))]
       (is (= :accepted (:status result)))
       (is (= :closed (:knowledge.batch/state batch)))
       (is (= :accepted (:knowledge.batch/outcome batch)))
       (is (= (:id (:operation result)) (:knowledge.batch/adoption batch)))
       (is (nil? (:knowledge.stream/active-batch (streams/stream @conn (:stream ready)))))
       (is (= 1 (count (filter #(= :processed (:knowledge.item/state %)) (streams/items @conn (:stream ready))))))
       (is (= 1 (count (streams/pending-items @conn (:stream ready)))))
       (is (str/ends-with? (:text (files/read-file-in @owned "wiki/from-batch.md")) "Retained proposed knowledge"))
       (with-redefs [filing/prepare (fn [& _] (throw (AssertionError. "Accepted filing may not recertify")))]
         (is (= value (publish/file-completed-request! conn principal (:room w) room (:request value)))))
       (is (:already-accepted? (adoption/recover! conn (:owner w) (:room w) room (:proposal value))))))))

(deftest lost-finalization-ack-is-observed-through-explicit-positive-recovery
  (fixture
   (fn [{:keys [conn w room ready owned value] :as data}]
     (let [transact d/transact lost? (atom false)]
       (with-redefs [d/transact
                     (fn [c tx]
                       (let [report (transact c tx)]
                         (when (and (identical? c conn)
                                    (= :accepted (:knowledge.batch/outcome (batches/batch @conn (:batch ready))))
                                    (compare-and-set! lost? false true))
                           (throw (ex-info "Lost finalization ACK" {})))
                         report))]
         (is (thrown? Exception (accept! data))))
       (is @lost?)
       (let [target-commit (d/commit-id @owned)
             result (adoption/recover! conn (:owner w) (:room w) room (:proposal value))]
         (is (= :accepted (:status result)))
         (is (:already-accepted? result))
         (is (= target-commit (d/commit-id @owned)))
         (is (= :closed (:knowledge.batch/state (batches/batch @conn (:batch ready))))))))))

(deftest accepted-records-cannot-hide-a-partial-batch-transition
  (fixture
   (fn [{:keys [conn w room ready owned value principal] :as data}]
     (accept! data)
     (let [batch (batches/batch @conn (:batch ready))
           selected (filter #(= (:batch ready) (:knowledge.item/batch %)) (streams/items @conn (:stream ready)))]
       (d/transact conn
         (into [{:db/id (:db/id batch) :knowledge.batch/state :review}
                [:db/retract (:db/id batch) :knowledge.batch/outcome :accepted]
                [:db/retract (:db/id batch) :knowledge.batch/adoption (:knowledge.batch/adoption batch)]
                [:db/add [:knowledge.stream/id (:stream ready)] :knowledge.stream/active-batch (:batch ready)]]
               (map (fn [row] {:db/id (:db/id row) :knowledge.item/state :claimed}) selected)))
       (let [target-commit (d/commit-id @owned)]
         (is (thrown? Exception (publish/file-completed-request! conn principal (:room w) room (:request value))))
         (is (thrown? Exception (adoption/recover! conn (:owner w) (:room w) room (:proposal value))))
         (is (= target-commit (d/commit-id @owned))))))))

(deftest reviewer-and-recovery-actor-are-independent-of-the-original-requester
  (fixture
   (fn [{:keys [conn w room ready value] :as data}]
     (let [reviewer (:member w) reconciler (:non-member w)]
       (d/transact conn [{:room/id (:room w) :room/owner [:party/id reviewer]}])
       ;; Real publication succeeds, but no finalization transaction is sent.
       (with-redefs [adoption/finalize! (fn [& _] (throw (ex-info "Publisher stopped before finalization" {})))]
         (is (thrown? Exception (controller/adopt! conn reviewer (:room w) room (:proposal value)))))
       (is (= :review (:knowledge.batch/state (batches/batch @conn (:batch ready)))))
       (d/transact conn [{:room/id (:room w) :room/owner [:party/id reconciler]}])
       (let [result (adoption/recover! conn reconciler (:room w) room (:proposal value))
             batch (batches/batch @conn (:batch ready))
             operation (adoptions/operation @conn (:knowledge.batch/adoption batch))]
         (is (= :accepted (:status result)))
         (is (false? (:already-accepted? result)))
         (is (= (:owner w) (:knowledge.batch/by batch)))
         (is (= reviewer (:actor operation)))
         (is (= reconciler (:reconciled-by operation)))
         (is (= :closed (:knowledge.batch/state batch))))))))

(deftest historical-acceptance-preserves-a-later-batch-and-rejects-filing-hash-corruption
  (fixture
   (fn [{:keys [conn w room ready value principal] :as data}]
     (accept! data)
     (let [later (random-uuid)]
       (d/transact conn [[:db/add [:knowledge.stream/id (:stream ready)] :knowledge.stream/active-batch later]])
       (is (= value (publish/file-completed-request! conn principal (:room w) room (:request value))))
       (is (:already-accepted? (adoption/recover! conn (:owner w) (:room w) room (:proposal value))))
       (is (= later (:knowledge.stream/active-batch (streams/stream @conn (:stream ready)))))
       (d/transact conn [[:db/add [:proposal/id (:proposal value)] :proposal/filing-hash (apply str (repeat 64 "0"))]])
       (is (thrown? Exception (publish/file-completed-request! conn principal (:room w) room (:request value))))
       (is (thrown? Exception (adoption/recover! conn (:owner w) (:room w) room (:proposal value))))))))
