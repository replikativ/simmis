(ns is.simm.ops.knowledge-publish-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.model.access :as access]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.system-db :as sdb]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.file-output-storage :as storage]
            [is.simm.ops.knowledge-admission :as admission]
            [is.simm.ops.knowledge-filing :as filing]
            [is.simm.ops.knowledge-filing-test :as fixture]
            [is.simm.ops.knowledge-publish :as publish]
            [is.simm.ops.proposal-publication :as publication]
            [is.simm.ops.proposals :as proposals]))

(defn- with-fixture
  ([f] (with-fixture {} f))
  ([options f]
   (@#'fixture/with-fixture
    options
    (fn [{:keys [conn] :as state}]
      (d/transact conn dispositions/schema)
      (let [resolve admission/default-target]
        ;; Actual authorization, immutable requests, certified Attempts, blob
        ;; retention, Proposal and writer transaction. Only live default-runtime
        ;; resolution is isolated; the upstream snapshot resolver has separate
        ;; real registry tests. Do not claim this is a provisioned-home test.
        (with-redefs [sdb/get-conn (constantly conn)
                      admission/default-target
                      (fn
                        ([rid room] (resolve rid room))
                        ([_db rid room] (resolve rid room)))
                      publication/publish! (constantly :published)]
          (f state)))))))

(defn- file! [{:keys [conn principal room-id room request]}]
  (publish/file-completed-request! conn principal room-id room request))

(defn- receipt [{:keys [conn request]}]
  (dispositions/disposition (requests/get-request conn request)))

(defn- proposal-count [conn]
  (count (d/q '[:find ?p :where [?p :proposal/id _]] @conn)))

(deftest certified-output-files-one-joint-proposal-and-retries-without-the-attempt
  (with-fixture
    (fn [{:keys [conn request] :as state}]
      (let [announcements (atom 0)
            value (with-redefs [publication/publish!
                                (fn [id]
                                  (is (= id (:proposal (receipt state))))
                                  (swap! announcements inc)
                                  :published)]
                    (file! state))]
        (is (= :proposed (:kind value)))
        (is (= value (receipt state)))
        (is (= 1 (proposal-count conn)))
        (is (= 1 @announcements))
        (is (= request (:request (storage/manifest conn (:manifest value)))))
        (d/transact conn [[:db/add [:proposal/id (:proposal value)] :proposal/status :dismissed]])
        (with-redefs [filing/prepare (fn [& _] (throw (AssertionError. "Recertified a historical filing")))]
          (is (= value (file! state))))
        (is (= :dismissed (:proposal/status (d/entity @conn [:proposal/id (:proposal value)]))))
        (is (= 1 (proposal-count conn)))))))

(deftest explicit-no-op-never-retains-blobs-or-builds-a-proposal
  (with-fixture {:output-files {}}
    (fn [{:keys [conn] :as state}]
      (with-redefs [storage/publish! (fn [& _] (throw (AssertionError. "Retained no-op bodies")))
                    proposals/prepare-filing (fn [& _] (throw (AssertionError. "Built no-op Proposal")))
                    publication/publish! (fn [& _] (throw (AssertionError. "Announced no-op Proposal")))]
        (let [value (file! state)]
          (is (= :no-op (:kind value)))
          (is (= value (receipt state)))
          (is (= value (file! state)))
          (is (zero? (proposal-count conn))))))))

(deftest lost-ack-resolves-only-from-the-exact-joint-durable-filing
  (with-fixture
    (fn [{:keys [conn] :as state}]
      (let [transact d/transact lost? (atom false)]
        (with-redefs [d/transact
                      (fn [& args]
                        (let [report (apply transact args)]
                          (when (and (receipt state) (compare-and-set! lost? false true))
                            (throw (ex-info "Lost writer ACK" {})))
                          report))]
          (is (= :proposed (:kind (file! state)))))
        (is @lost?)
        (is (= 1 (proposal-count conn)))
        (is (= (receipt state) (file! state)))))))

(deftest revocation-after-certification-refuses-both-pointers
  (with-fixture
    (fn [{:keys [conn room-id party] :as state}]
      (let [prepare filing/prepare]
        (with-redefs [filing/prepare
                      (fn [& args]
                        (let [result (apply prepare args)]
                          (d/transact conn [[:db/retract [:room/id room-id] :room/owner [:party/id party]]])
                          (is (not (access/can? @conn party :merge {:room room-id})))
                          result))]
          (is (thrown? clojure.lang.ExceptionInfo (file! state))))
        (is (nil? (receipt state)))
        (is (zero? (proposal-count conn)))))))

(deftest writer-snapshot-target-change-refuses-both-pointers
  (with-fixture
    (fn [{:keys [conn] :as state}]
      (let [target filing/current-target
            snapshots (atom [])]
        (with-redefs [filing/current-target
                      (fn
                        ([rid room] (target rid room))
                        ([db rid room]
                         (swap! snapshots conj db)
                         (assoc (target rid room) :registry-system (random-uuid))))]
          (is (thrown? clojure.lang.ExceptionInfo (file! state))))
        (is (= 1 (count @snapshots)))
        (is (nil? (receipt state)))
        (is (zero? (proposal-count conn)))))))

(deftest failure-before-joint-commit-announces-nothing
  (with-fixture
    (fn [{:keys [conn] :as state}]
      (let [announcements (atom 0)]
        (with-redefs [d/transact (fn [& _] (throw (ex-info "Writer rejected filing" {})))
                      publication/publish! (fn [& _] (swap! announcements inc))]
          (is (thrown? clojure.lang.ExceptionInfo (file! state))))
        (is (zero? @announcements))
        (is (nil? (receipt state)))
        (is (zero? (proposal-count conn)))))))

(deftest one-sided-proposal-filing-is-not-retrospectively-certified
  (with-fixture
    (fn [{:keys [conn request] :as state}]
      (let [value (file! state)]
        (d/transact conn (mapv (fn [{:db/keys [ident]}]
                                 [:db/retract [:work.request/id request] ident]) dispositions/schema))
        (is (nil? (receipt state)))
        (is (thrown? clojure.lang.ExceptionInfo (file! state)))
        (is (nil? (receipt state)))
        (is (= 1 (proposal-count conn)))
        (is (= (:proposal value) (:proposal/id (d/entity @conn [:proposal/id (:proposal value)]))))))))

(deftest one-sided-disposition-cannot-announce-a-missing-proposal
  (with-fixture
    (fn [{:keys [conn] :as state}]
      (let [value (file! state)]
        (d/transact conn [[:db.fn/retractEntity [:proposal/id (:proposal value)]]])
        (with-redefs [publication/publish! (fn [& _] (throw (AssertionError. "Announced partial filing")))]
          (is (thrown? clojure.lang.ExceptionInfo (file! state))))
        (is (zero? (proposal-count conn)))))))

(deftest corrupt-or-ambiguously-owned-retention-is-not-a-complete-filing
  (doseq [change [:empty-entries :bad-metadata :missing-body-ref :duplicate-owner]]
    (with-fixture
      (fn [{:keys [conn room-id party] :as state}]
        (let [value (file! state)
              manifest-ref [:file-output/id (:manifest value)]
              entry (:v (first (d/datoms @conn :eavt
                                         (:e (first (d/datoms @conn :avet :file-output/id (:manifest value))))
                                         :file-output/entries)))]
          (case change
            :empty-entries
            (d/transact conn [[:db/retract manifest-ref :file-output/entries]])
            :bad-metadata
            (d/transact conn [[:db/add manifest-ref :file-output/output (random-uuid)]])
            :missing-body-ref
            (d/transact conn [[:db/retract entry :file-output.entry/body-ref]])
            :duplicate-owner
            (d/transact conn [{:proposal/id (random-uuid) :proposal/title "Ambiguous owner"
                               :proposal/status :open :proposal/room room-id :proposal/author party
                               :proposal/forks [{:proposal.fork/scope (:manifest value)
                                                 :proposal.fork/authority-scope room-id
                                                 :proposal.fork/branch "duplicate"
                                                 :proposal.fork/system-type :file-output
                                                 :proposal.fork/file-output manifest-ref}]}]))
          (with-redefs [publication/publish!
                        (fn [& _] (throw (AssertionError. "Announced corrupt retained evidence")))]
            (is (thrown? clojure.lang.ExceptionInfo (file! state)) (name change)))
          (is (= value (receipt state))))))))
