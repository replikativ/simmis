(ns is.simm.ops.file-output-adoption-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.agent.episode :as episode]
            [dvergr.artifact :as artifact]
            [is.simm.documents.markdown :as markdown]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.file-output-storage :as storage]
            [is.simm.ops.knowledge-filing :as filing]
            [is.simm.ops.knowledge-outputs :as outputs]
            [is.simm.ops.knowledge-publish-test :as fixture]
            [is.simm.ops.proposals :as proposals]))

(defn- with-filed [f]
  (@#'fixture/with-fixture
   (fn [state]
     (let [receipt (@#'fixture/file! state)]
       (f (assoc state :receipt receipt :proposal (:proposal receipt) :manifest (:manifest receipt)))))))

(defn- prepare [{:keys [conn party room-id room proposal]}]
  (adoption/prepare conn party room-id room proposal))

(deftest preflight-uses-retained-evidence-not-an-expired-attempt-or-live-file
  (with-filed
    (fn [{:keys [conn workspace party proposal manifest receipt] :as state}]
      (let [control-before @conn files-before @workspace
            result (with-redefs [episode/attempt (fn [& _] (throw (AssertionError. "Attempt expired")))]
                     (prepare state))
            proof (:proof result)]
        (is (= control-before @conn))
        (is (= files-before @workspace))
        (is (= proposal (get-in proof [:binding :proposal])))
        (is (= manifest (get-in proof [:binding :component])))
        (is (= party (get-in proof [:binding :actor])))
        (is (= (:id receipt) (get-in proof [:binding :disposition])))
        (is (= (:target (:basis state)) (get-in proof [:binding :target])))
        (is (every? uuid? (vals (get-in proof [:retained :body-refs]))))
        (is (= "Historical before\r\n"
               (markdown/decode (outputs/bytes! (get-in result [:plan :bodies "wiki/existing.md" :before])))))
        (is (= "Proposed after\r\n"
               (markdown/decode (outputs/bytes! (get-in result [:plan :bodies "wiki/existing.md" :after])))))
        ;; No schema installation, claim, status change or target publication.
        (is (= :open (:proposal/status (proposals/get-proposal proposal))))))))

(deftest authorization-and-target-refusal-precede-every-body-read
  (with-filed
    (fn [{:keys [conn room-id basis] :as state}]
      (let [reads (atom 0) original storage/read-file-in]
        (with-redefs [storage/read-file-in (fn [& args] (swap! reads inc) (apply original args))]
          (is (thrown? Exception (prepare (assoc state :party (random-uuid)))))
          (is (thrown? Exception (prepare (assoc state :party nil))))
          (is (thrown? Exception (prepare (assoc state :room-id (random-uuid)))))
          (is (thrown? Exception (prepare (assoc state :conn (atom @conn)))))
          (with-redefs [filing/current-target (fn [& _] (assoc (:target basis) :store (random-uuid)))]
            (is (thrown? Exception (prepare state))))
          (is (zero? @reads))
          ;; Room membership alone does not grant merge after owner revocation.
          (d/transact conn [[:db/retract [:room/id room-id] :room/owner]])
          (is (thrown? Exception (prepare state)))
          (is (zero? @reads)))))))

(deftest mixed-review-resolved-and-partial-filing-refuse-without-reading-bodies
  (doseq [alter! [(fn [{:keys [conn proposal]}]
                    (d/transact conn [{:db/id [:proposal/id proposal]
                                       :proposal/forks [{:proposal.fork/scope (random-uuid)
                                                         :proposal.fork/branch "other"
                                                         :proposal.fork/system-type :kb}]}]))
                  (fn [{:keys [conn proposal]}]
                    (d/transact conn [[:db/add [:proposal/id proposal] :proposal/review (random-uuid)]]))
                  (fn [{:keys [conn proposal]}]
                    (d/transact conn [[:db/add [:proposal/id proposal] :proposal/status :dismissed]]))
                  (fn [{:keys [conn request]}]
                    (d/transact conn [[:db/retract [:work.request/id request] :knowledge.disposition/output]]))
                  (fn [{:keys [conn proposal]}]
                    (d/transact conn [[:db/add [:proposal/id proposal] :proposal/run (random-uuid)]]))]]
    (with-filed
      (fn [state]
        (alter! state)
        (with-redefs [storage/read-file-in (fn [& _] (throw (AssertionError. "Body read before refusal")))]
          (is (thrown? Exception (prepare state))))))))

(deftest missing-or-corrupt-retained-bytes-fail-before-any-adoption
  (doseq [value [nil {:before nil :after nil}]]
    (with-filed
      (fn [{:keys [conn proposal workspace] :as state}]
        (let [before @conn target @workspace]
          (with-redefs [artifact/get-value (constantly value)]
            (is (thrown? Exception (prepare state))))
          (is (= before @conn))
          (is (= target @workspace))
          (is (= :open (:proposal/status (proposals/get-proposal proposal)))))))))

(deftest exact-pair-reads-use-the-supplied-snapshot-not-a-later-body-reference
  (with-filed
    (fn [{:keys [conn manifest]}]
      (let [db @conn
            retained (storage/retained-in db manifest)
            entry-eid (:v (first (d/datoms db :eavt
                                           (:e (first (d/datoms db :avet :file-output/id manifest)))
                                           :file-output/entries)))
            original-ref (get-in retained [:body-refs "wiki/existing.md"])
            seen (atom nil)
            original artifact/get-value]
        (d/transact conn [[:db/add entry-eid :file-output.entry/body-ref (random-uuid)]])
        (with-redefs [artifact/get-value (fn [store ref] (reset! seen ref) (original store ref))]
          (is (map? (storage/read-file-in conn db manifest "wiki/existing.md")))
          (is (= original-ref @seen))
          ;; New snapshot names a missing blob and must refuse, never fall back.
          (is (thrown? Exception (storage/read-file conn manifest "wiki/existing.md"))))))))
