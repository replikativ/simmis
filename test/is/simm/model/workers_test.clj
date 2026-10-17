(ns is.simm.model.workers-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [is.simm.model.workers :as workers])
  (:import [java.util Date]))

(defn with-store [f]
  (let [config {:store {:backend :memory :id (random-uuid)}
                :keep-history? false :schema-flexibility :write}]
    (d/create-database config)
    (let [conn (d/connect config)]
      (try (d/transact conn workers/schema) (f conn)
           (finally (d/release conn))))))

(defn fixture [conn]
  (let [enrollment {:id (random-uuid) :by (random-uuid) :room (random-uuid)
                    :secret (workers/new-secret) :config {:adapter :fixture :version "1"}}
        assignment {:id (random-uuid) :worker (:id enrollment) :generation 1 :room (:room enrollment)
                    :request (random-uuid) :job (random-uuid)
                    :input {:prompt "fixture task" :files {"input.txt" "frozen"}}}]
    (workers/enroll! conn enrollment)
    (workers/enqueue! conn assignment)
    {:enrollment enrollment :assignment assignment}))

(defn claimed [conn]
  (let [{:keys [enrollment assignment] :as f} (fixture conn)
        secret (workers/new-secret) nonce (random-uuid)
        row (workers/claim! conn (:id assignment) (:id enrollment) 1 nonce secret (Date. 10000) 10000)]
    (assoc f :row row :secret secret :nonce nonce :lease (:simmis.worker.assignment/lease row))))

(deftest enrollment-is-immutable-idempotent-and-stores-no-plaintext-secret
  (with-store
    (fn [conn]
      (let [{:keys [enrollment]} (fixture conn)
            row (workers/enrollment conn (:id enrollment))]
        (is (= row (workers/enroll! conn enrollment)))
        (is (= 43 (count (:secret enrollment))))
        (is (not= (:secret enrollment) (:simmis.worker/credential-hash row)))
        (is (workers/matches-secret? (:simmis.worker/credential-hash row) (:secret enrollment)))
        (is (false? (workers/matches-secret? (:simmis.worker/credential-hash row) (workers/new-secret))))
        (is (= (:config enrollment) (workers/payload conn row :simmis.worker/config-ref)))
        (doseq [different [(assoc enrollment :by (random-uuid))
                           (assoc enrollment :room (random-uuid))
                           (assoc enrollment :secret (workers/new-secret))
                           (assoc-in enrollment [:config :version] "2")]]
          (is (thrown? clojure.lang.ExceptionInfo (workers/enroll! conn different))))
        (workers/revoke! conn (:id enrollment))
        (is (:simmis.worker/revoked-at (workers/revoke! conn (:id enrollment))))
        (is (:simmis.worker/revoked-at (workers/enroll! conn enrollment)) "retry never reactivates enrollment")))))

(deftest concurrent-claims-have-one-owner-and-cannot-reassign-this-run
  (with-store
    (fn [conn]
      (let [{:keys [enrollment assignment]} (fixture conn)
            ready (promise)
            claims (mapv (fn [_]
                           (let [secret (workers/new-secret) nonce (random-uuid)]
                             (future @ready
                                     (try {:row (workers/claim! conn (:id assignment) (:id enrollment) 1 nonce secret (Date. 10000) 10000)
                                           :nonce nonce :secret secret}
                                          (catch Exception e {:error (:type (ex-data e))}))))) (range 8))]
        (deliver ready true)
        (let [results (mapv #(deref % 10000 ::timeout) claims)
              winner (first (filter :row results))]
          (is (not-any? #{::timeout} results))
          (is (= 1 (count (filter :row results))))
          (is (= 7 (count (filter #(= :is.simm.model.workers/conflict (:error %)) results))))
          (is (= (:row winner) (workers/claim! conn (:id assignment) (:id enrollment) 1
                                               (:nonce winner) (:secret winner) (Date. 11000) 10000)))
          (is (= (Date. 20000) (:simmis.worker.assignment/expires-at (:row winner))) "claim retry never extends lease")
          (is (= (:input assignment) (workers/payload conn (:row winner) :simmis.worker.assignment/input-ref))))))))

(deftest lost-claim-and-completion-responses-observe-the-same-durable-receipt
  (with-store
    (fn [conn]
      (let [{:keys [enrollment assignment]} (fixture conn)
            secret (workers/new-secret) nonce (random-uuid)
            transact d/transact
            lose-response (fn [c tx] (transact c tx) (throw (ex-info "lost response" {})))
            row (with-redefs [d/transact lose-response]
                  (workers/claim! conn (:id assignment) (:id enrollment) 1 nonce secret (Date. 10000) 10000))
            lease (:simmis.worker.assignment/lease row)
            output {:files {"out/result" "answer"} :worker-attested {:exit-code 0}}]
        (is (= :claimed (:simmis.worker.assignment/state row)))
        (let [done (with-redefs [d/transact lose-response]
                     (workers/complete! conn (:id assignment) lease secret output (Date. 11000)))]
          (is (= :completed (:simmis.worker.assignment/state done)))
          (is (:simmis.worker.assignment/stop-ack-at done))
          (is (= output (workers/payload conn done :simmis.worker.assignment/output-ref)))
          (is (= done (workers/complete! conn (:id assignment) lease secret output (Date. 12000))))
          (is (thrown? clojure.lang.ExceptionInfo
                       (workers/complete! conn (:id assignment) lease secret (assoc output :other true) (Date. 12000)))))))))

(deftest expiry-fences-results-without-inventing-physical-stop-or-another-owner
  (with-store
    (fn [conn]
      (let [{:keys [enrollment assignment row secret nonce lease]} (claimed conn)
            id (:id assignment)
            expired (workers/heartbeat! conn id lease secret (Date. 20000) 10000)]
        (is (= :quarantined (:simmis.worker.assignment/state expired)))
        (is (:simmis.worker.assignment/cancel-at expired))
        (is (nil? (:simmis.worker.assignment/stop-ack-at expired)))
        (is (= (:simmis.worker.assignment/expires-at row) (:simmis.worker.assignment/expires-at expired)))
        (is (= :quarantined (:simmis.worker.assignment/state
                             (workers/claim! conn id (:id enrollment) 1 nonce secret (Date. 21000) 10000))))
        (is (thrown? clojure.lang.ExceptionInfo
                     (workers/claim! conn id (:id enrollment) 1 (random-uuid) (workers/new-secret) (Date. 21000) 10000)))
        (is (thrown? clojure.lang.ExceptionInfo (workers/complete! conn id lease secret {:files {}} (Date. 21000))))
        (is (:simmis.worker.assignment/stop-ack-at (workers/acknowledge-stop! conn id lease secret (Date. 22000))))))))

(deftest revoked-worker-cannot-renew-or-publish-but-can-confirm-existing-physical-stop
  (with-store
    (fn [conn]
      (let [{:keys [enrollment assignment secret nonce lease]} (claimed conn)
            id (:id assignment)]
        (workers/revoke! conn (:id enrollment))
        (is (thrown? clojure.lang.ExceptionInfo
                     (workers/claim! conn id (:id enrollment) 1 nonce secret (Date. 11000) 10000)))
        (is (thrown? clojure.lang.ExceptionInfo (workers/complete! conn id lease secret {:files {}} (Date. 11000))))
        (let [row (workers/heartbeat! conn id lease secret (Date. 11000) 10000)]
          (is (= :quarantined (:simmis.worker.assignment/state row)))
          (is (nil? (:simmis.worker.assignment/stop-ack-at row))))
        (doseq [[wrong-lease wrong-secret] [[(random-uuid) secret] [lease (workers/new-secret)]]]
          (is (thrown? clojure.lang.ExceptionInfo
                       (workers/acknowledge-stop! conn id wrong-lease wrong-secret (Date. 12000)))))
        (let [stopped (workers/acknowledge-stop! conn id lease secret (Date. 12000))]
          (is (= :stopped (:simmis.worker.assignment/state stopped)))
          (is (= stopped (workers/acknowledge-stop! conn id lease secret (Date. 13000)))))))))

(deftest cancellation-and-claim-are-one-serialized-ownership-decision
  (with-store
    (fn [conn]
      (dotimes [_ 4]
        (let [{:keys [enrollment assignment]} (fixture conn)
              id (:id assignment) secret (workers/new-secret) nonce (random-uuid)
              go (promise)
              claimant (future @go (try (workers/claim! conn id (:id enrollment) 1 nonce secret (Date. 10000) 10000)
                                        (catch Exception _ nil)))
              canceller (future @go (workers/cancel! conn id (Date. 11000)))]
          (deliver go true)
          (is (not= ::timeout (deref claimant 10000 ::timeout)))
          (is (not= ::timeout (deref canceller 10000 ::timeout)))
          (let [row (workers/assignment conn id)]
            (is (:simmis.worker.assignment/cancel-at row))
            (if (:simmis.worker.assignment/lease row)
              (do (is (= :quarantined (:simmis.worker.assignment/state row)))
                  (is (nil? (:simmis.worker.assignment/stop-ack-at row))))
              (do (is (= :withdrawn (:simmis.worker.assignment/state row)))
                  (is (:simmis.worker.assignment/stop-ack-at row))))))))))

(deftest generation-room-and-input-identities-cannot-drift
  (with-store
    (fn [conn]
      (let [{:keys [enrollment assignment]} (fixture conn)]
        (doseq [bad [(assoc assignment :generation 2) (assoc assignment :room (random-uuid))
                     (assoc-in assignment [:input :function] identity)
                     (assoc-in assignment [:input :prompt] "different frozen task")]]
          (is (thrown? clojure.lang.ExceptionInfo (workers/enqueue! conn bad))))
        (is (thrown? clojure.lang.ExceptionInfo
                     (workers/claim! conn (:id assignment) (:id enrollment) 2 (random-uuid)
                                     (workers/new-secret) (Date. 10000) 10000)))))))

(deftest database-transaction-diagnostics-never-receive-plaintext-lease-proofs
  (with-store
    (fn [conn]
      (let [{:keys [assignment secret lease]} (claimed conn)
            id (:id assignment)
            transactions (atom [])
            transact d/transact]
        (with-redefs [d/transact (fn [c tx] (swap! transactions conj tx) (transact c tx))]
          (workers/heartbeat! conn id lease secret (Date. 11000) 10000)
          (workers/complete! conn id lease secret {:files {}} (Date. 12000))
          (workers/acknowledge-stop! conn id lease secret (Date. 13000)))
        (is (= 3 (count @transactions)))
        (is (not (clojure.string/includes? (pr-str @transactions) secret)))
        (is (clojure.string/includes? (pr-str @transactions) (workers/secret-hash secret)))))))

(deftest revocation-during-input-publication-cannot-insert-an-unclaimable-queued-task
  (with-store
    (fn [conn]
      (let [{:keys [enrollment assignment]} (fixture conn)
            assignment (assoc assignment :id (random-uuid))
            publish artifact/publish-value!]
        (with-redefs [artifact/publish-value!
                      (fn [store value callback]
                        (workers/revoke! conn (:id enrollment))
                        (publish store value callback))]
          (is (thrown? clojure.lang.ExceptionInfo (workers/enqueue! conn assignment))))
        (is (nil? (workers/assignment conn (:id assignment))))))))

(deftest concurrent-revoke-versus-claim-never-grants-a-revoked-lease-on-retry
  (with-store
    (fn [conn]
      (dotimes [_ 4]
        (let [{:keys [enrollment assignment]} (fixture conn)
              id (:id assignment) worker (:id enrollment) secret (workers/new-secret) nonce (random-uuid)
              start (promise)
              claim (future @start (try (workers/claim! conn id worker 1 nonce secret (Date. 10000) 10000)
                                        (catch Exception _ nil)))
              revoke (future @start (workers/revoke! conn worker))]
          (deliver start true)
          (let [claimed (deref claim 10000 ::timeout)]
            (is (not= ::timeout claimed))
            (is (not= ::timeout (deref revoke 10000 ::timeout)))
            (is (:simmis.worker/revoked-at (workers/enrollment conn worker)))
            (is (thrown? clojure.lang.ExceptionInfo
                         (workers/claim! conn id worker 1 nonce secret (Date. 11000) 10000)))
            (when claimed
              (is (nil? (:simmis.worker.assignment/stop-ack-at (workers/assignment conn id))))
              (is (= :quarantined (:simmis.worker.assignment/state
                                   (workers/heartbeat! conn id (:simmis.worker.assignment/lease claimed)
                                                       secret (Date. 11000) 10000)))))))))))

(deftest concurrent-revoke-versus-completion-is-atomic-and-never-fabricates-stop
  (with-store
    (fn [conn]
      (dotimes [_ 4]
        (let [{:keys [enrollment assignment secret lease]} (claimed conn)
              id (:id assignment) start (promise)
              complete (future @start (try (workers/complete! conn id lease secret {:files {}} (Date. 11000))
                                           (catch Exception _ nil)))
              revoke (future @start (workers/revoke! conn (:id enrollment)))]
          (deliver start true)
          (let [result (deref complete 10000 ::timeout)]
            (is (not= ::timeout result))
            (is (not= ::timeout (deref revoke 10000 ::timeout)))
            (is (:simmis.worker/revoked-at (workers/enrollment conn (:id enrollment))))
            (let [row (workers/assignment conn id)]
              (if result
                (do (is (= :completed (:simmis.worker.assignment/state row)))
                    (is (:simmis.worker.assignment/stop-ack-at row)))
                (do (is (nil? (:simmis.worker.assignment/output-id row)))
                    (is (nil? (:simmis.worker.assignment/stop-ack-at row))))))))))))

(deftest worker-capacity-is-not-returned-by-lease-expiration
  (with-store
    (fn [conn]
      (let [{:keys [assignment enrollment secret lease]} (claimed conn)
            other (assoc assignment :id (random-uuid))
            other-secret (workers/new-secret) nonce (random-uuid)
            claim-other #(workers/claim! conn (:id other) (:id enrollment) 1 nonce other-secret % 10000)]
        (workers/enqueue! conn other)
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unresolved process" (claim-other (Date. 11000))))
        (workers/heartbeat! conn (:id assignment) lease secret (Date. 20000) 10000)
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unresolved process" (claim-other (Date. 21000))))
        (workers/acknowledge-stop! conn (:id assignment) lease secret (Date. 22000))
        (is (= :claimed (:simmis.worker.assignment/state (claim-other (Date. 23000)))))))))

(deftest concurrent-different-assignments-share-one-physical-worker-slot
  (with-store
    (fn [conn]
      (let [{:keys [assignment enrollment]} (fixture conn)
            other (assoc assignment :id (random-uuid))
            ready (promise)]
        (workers/enqueue! conn other)
        (let [claims (mapv (fn [task]
                             (future @ready
                                     (try (workers/claim! conn (:id task) (:id enrollment) 1
                                                          (random-uuid) (workers/new-secret) (Date. 10000) 10000)
                                          (catch Exception e {:error (:type (ex-data e))}))))
                           [assignment other])]
          (deliver ready true)
          (let [results (mapv #(deref % 10000 ::timeout) claims)]
            (is (not-any? #{::timeout} results))
            (is (= 1 (count (filter #(= :claimed (:simmis.worker.assignment/state %)) results))))
            (is (= 1 (count (filter #(= :is.simm.model.workers/busy (:error %)) results))))))))))
