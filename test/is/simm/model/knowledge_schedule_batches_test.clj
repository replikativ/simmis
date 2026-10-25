(ns is.simm.model.knowledge-schedule-batches-test
  "Real serialized retention/storage with synthetic already-verified metadata.
   Does not claim source provenance, request admission, a Job or dispatch."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-schedule-batches :as binding]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.workflow-schedules :as schedules]
            [is.simm.model.work-requests :as requests]))

(defn- with-db [f]
  (let [config {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write :keep-history? false}]
    (d/create-database config)
    (let [conn (d/connect config)]
      (try (d/transact conn (concat schedules/schema streams/schema batches/schema binding/schema requests/schema))
           (f conn)
           (finally (d/release conn))))))

(defn- seed! [conn]
  (let [activation (random-uuid) occurrence (random-uuid) revision (random-uuid)
        target-ref (random-uuid) by (random-uuid) room (random-uuid) claim (random-uuid)
        batch (random-uuid) frozen (random-uuid) stream (random-uuid) target (random-uuid)
        value (binding/binding-value {:activation activation :occurrence occurrence :revision revision
                                      :target-ref target-ref :by by :room room :claim claim
                                      :batch {:batch batch :selection batch :frozen frozen}})]
    (d/transact conn [{:schedule.activation/id activation :schedule.activation/by by :schedule.activation/room room
                      :schedule.activation/revision revision :schedule.activation/target-ref target-ref
                      :schedule.activation/enabled? true :schedule.activation/active-occurrence occurrence}
                     {:schedule.occurrence/id occurrence :schedule.occurrence/activation activation
                      :schedule.occurrence/revision revision :schedule.occurrence/claim claim
                      :schedule.occurrence/state :admitting}
                     {:knowledge.stream/id stream :knowledge.stream/by by :knowledge.stream/room room
                      :knowledge.stream/target target :knowledge.stream/active-batch batch}
                     {:knowledge.batch/id batch :knowledge.batch/by by :knowledge.batch/room room
                      :knowledge.batch/target target :knowledge.batch/stream stream :knowledge.batch/state :ready
                      :knowledge.batch/selection-id batch :knowledge.batch/frozen-id frozen
                      :knowledge.batch/selection (random-uuid) :knowledge.batch/frozen (random-uuid)}])
    value))

(defn- held [f] (try (f) nil (catch clojure.lang.ExceptionInfo error (:reason (ex-data error)))))
(defn- tx! [conn value] (d/transact conn [[:db.fn/call binding/retention-tx value (random-uuid)]]))

(deftest retained-original-ack-and-reverse-binding-without-legacy-input-state
  (with-db
    (fn [conn]
      (let [value (seed! conn) o (binding/retain! conn value)]
        (is (= value (binding/retained conn o)))
        (is (= o (binding/bound-occurrence @conn (get-in value [:batch :batch]))))
        (is (not-any? #(contains? o %) [:schedule.occurrence/input-id :schedule.occurrence/input-ref
                                       :schedule.occurrence/input-revision]))
        (is (= :already-retained (held #(tx! conn value))))
        (is (= value (binding/retained conn (schedules/occurrence conn (:occurrence value)))))))))

(deftest same-ready-batch-cannot-belong-to-a-second-occurrence
  (with-db
    (fn [conn]
      (let [first (seed! conn) _ (binding/retain! conn first)
            second (binding/binding-value (assoc (dissoc first :id :version) :occurrence (random-uuid)))]
        (d/transact conn [{:schedule.occurrence/id (:occurrence second) :schedule.occurrence/activation (:activation second)
                          :schedule.occurrence/revision (:revision second) :schedule.occurrence/claim (:claim second)
                          :schedule.occurrence/state :admitting}
                         {:db/id [:schedule.activation/id (:activation second)]
                          :schedule.activation/active-occurrence (:occurrence second)}])
        (is (= :already-retained (held #(tx! conn second))))
        (is (nil? (:schedule.occurrence/batch-ref (schedules/occurrence conn (:occurrence second)))))
        (is (= (:occurrence first)
               (:schedule.occurrence/id (binding/bound-occurrence @conn (get-in first [:batch :batch])))))))))

(deftest writer-rejects-changed-claim-revision-active-pointer-owner-or-deployment
  (doseq [[attr id-key reason]
          [[:schedule.occurrence/claim :occurrence :occurrence-changed]
           [:schedule.occurrence/revision :occurrence :occurrence-changed]
           [:schedule.activation/revision :activation :occurrence-changed]
           [:schedule.activation/active-occurrence :activation :occurrence-changed]
           [:schedule.activation/by :activation :occurrence-changed]
           [:schedule.activation/target-ref :activation :occurrence-changed]
           [:knowledge.stream/active-batch :stream :batch-not-ready]]]
    (with-db
      (fn [conn]
        (let [value (seed! conn)
              [lookup id] (if (= :stream id-key)
                            [:knowledge.stream/id (:knowledge.batch/stream (batches/batch @conn (get-in value [:batch :batch])))]
                            [(if (= :activation id-key) :schedule.activation/id :schedule.occurrence/id) (get value id-key)])]
          (d/transact conn [{:db/id [lookup id] attr (random-uuid)}])
          (is (= reason (held #(tx! conn value))) (str attr))
          (is (nil? (binding/bound-occurrence @conn (get-in value [:batch :batch])))))))))

(deftest paused-claimed-occurrence-can-retain-but-unready-batch-cannot
  (with-db
    (fn [conn]
      (let [value (seed! conn)]
        (d/transact conn [{:db/id [:schedule.activation/id (:activation value)] :schedule.activation/enabled? false}])
        (is (= value (binding/retained conn (binding/retain! conn value)))))))
  (doseq [change [{:knowledge.batch/state :submitted}
                  {:knowledge.batch/request (random-uuid)}
                  {:knowledge.batch/frozen-id (random-uuid)}]]
    (with-db
      (fn [conn]
        (let [value (seed! conn)]
          (d/transact conn [(assoc change :db/id [:knowledge.batch/id (get-in value [:batch :batch])])])
          (is (= :batch-not-ready (held #(tx! conn value)))))))))

(deftest historical-observation-does-not-require-todays-active-or-ready-state
  (with-db
    (fn [conn]
      (let [value (seed! conn) original (binding/retain! conn value)]
        (d/transact conn [{:db/id (:db/id original) :schedule.occurrence/state :linked}
                         {:db/id [:schedule.activation/id (:activation value)] :schedule.activation/active-occurrence (random-uuid)}
                         {:db/id [:knowledge.batch/id (get-in value [:batch :batch])] :knowledge.batch/state :closed}])
        (is (= value (binding/retained conn (schedules/occurrence conn (:occurrence value)))))
        (is (nil? (binding/retained conn {})))
        (is (= :partial-binding (held #(binding/retained conn (dissoc original :schedule.occurrence/batch-ref)))))
        (is (= :retained-binding-mismatch
               (held #(binding/retained conn (assoc original :schedule.occurrence/claim (random-uuid))))))))))

(deftest lost-retention-ack-is-inspection-only-and-cannot-be-reissued
  (with-db
    (fn [conn]
      (let [value (seed! conn) transact! d/transact lost (ex-info "Lost original retention ACK" {})]
        (with-redefs [d/transact (fn [c tx] (transact! c tx) (throw lost))]
          (is (identical? lost (try (binding/retain! conn value) nil (catch Exception error error)))))
        (is (= value (binding/retained conn (schedules/occurrence conn (:occurrence value)))))
        (is (= :already-retained (held #(binding/retain! conn value))))))))

(deftest invalid-or-mixed-bindings-cannot-install
  (with-db
    (fn [conn]
      (let [value (seed! conn)]
        (is (= :invalid-binding (held #(tx! conn (assoc value :unexpected "not a binding field")))))
        (is (= :invalid-binding (held #(tx! conn (assoc value :id (random-uuid))))))
        (is (= :invalid-binding (held #(binding/binding-value (assoc (dissoc value :id :version) :unexpected (repeat 1000 "x"))))))
        (d/transact conn [{:db/id [:schedule.occurrence/id (:occurrence value)] :schedule.occurrence/input-id (random-uuid)}])
        (is (= :already-retained (held #(tx! conn value))))
        (is (nil? (binding/bound-occurrence @conn (get-in value [:batch :batch]))))
        (is (nil? (binding/bound-occurrence @conn nil)))))))


(defn- request-fixture [conn]
  (let [value (seed! conn) o (binding/retain! conn value)
        anchor {:value value :reference (:schedule.occurrence/batch-ref o)}
        intent {:knowledge/batch (:batch value) :knowledge/schedule {:binding (:id value)}
                :schedule-origin {:activation (:activation value) :revision (:revision value)
                                  :occurrence (:occurrence value) :creation-channel :host}
                :submission {:request-id (:occurrence value)}}
        p (requests/prepare-claim {:party (:by value) :room (:room value) :client-id (:occurrence value)
                                   :kind :workflow :intent intent})
        request (assoc (:row (requests/claim-row p)) :work.request/payload-ref (random-uuid))]
    {:value value :anchor anchor :intent intent :request request :digest (:digest p)}))

(defn- joint! [conn {:keys [anchor intent request digest]}]
  (d/transact conn
    [[:db.fn/call
      (fn [db]
        (vec (concat (binding/request-tx db anchor request intent)
                     (requests/claim-tx db request digest)
                     (batches/request-tx db (:knowledge/batch intent) request digest))))]]))

(deftest joint-request-links-all-three-records-and-historical-replay-is-not-fresh-admission
  (with-db
    (fn [conn]
      (let [{:keys [value anchor intent request] :as f} (request-fixture conn)]
        (joint! conn f)
        (is (= (:work.request/id request) (:schedule.occurrence/request (schedules/occurrence conn (:occurrence value)))))
        (is (= (:work.request/id request) (:knowledge.batch/request (batches/batch @conn (get-in value [:batch :batch])))))
        (is (= value (binding/linked! @conn anchor (requests/get-request conn (:work.request/id request)) intent)))
        (d/transact conn [{:db/id [:knowledge.batch/id (get-in value [:batch :batch])] :knowledge.batch/state :closed}
                         {:db/id [:schedule.activation/id (:activation value)] :schedule.activation/active-occurrence (random-uuid)}])
        (is (= value (binding/linked! @conn anchor request intent)))
        (is (some? (held #(joint! conn f))))))))

(deftest schedule-markers-cannot-be-omitted-or-invented-to-cross-the-reservation
  (with-db
    (fn [conn]
      (let [{:keys [value anchor intent request]} (request-fixture conn)]
        (doseq [changed [(dissoc intent :schedule-origin) (dissoc intent :knowledge/schedule)
                         (dissoc intent :schedule-origin :knowledge/schedule)
                         (assoc-in intent [:knowledge/schedule :binding] (random-uuid))
                         (assoc-in intent [:schedule-origin :occurrence] (random-uuid))]]
          (is (= :request-binding-mismatch (held #(binding/request-tx @conn anchor request changed)))))
        (is (= :invalid-retained-anchor (held #(binding/request-tx @conn nil request intent))))
        (doseq [changed [(dissoc intent :knowledge/batch :schedule-origin :knowledge/schedule)
                         (-> intent (assoc-in [:knowledge/batch :batch] (random-uuid))
                             (dissoc :schedule-origin :knowledge/schedule))]]
          (is (= :request-binding-mismatch (held #(binding/request-tx @conn nil request changed)))))
        (let [id (random-uuid) unbound (assoc-in intent [:knowledge/batch :batch] id)
              unbound-row (assoc request :work.request/knowledge-batch id)]
          (is (= :unbound-schedule-marker (held #(binding/request-tx @conn nil unbound-row unbound))))
          (is (= [] (binding/request-tx @conn nil unbound-row (dissoc unbound :knowledge/schedule :schedule-origin)))))
        (is (nil? (requests/get-request conn (:work.request/id request))))
        (is (nil? (:schedule.occurrence/request (schedules/occurrence conn (:occurrence value)))))))))

(deftest changed-storage-anchor-or-request-identity-refuses-joint-insert
  (doseq [change [:reference :claim :revision :client :party :pointer]]
    (with-db
      (fn [conn]
        (let [{:keys [value anchor intent request] :as f} (request-fixture conn)]
          (case change
            :reference (d/transact conn [{:db/id [:schedule.occurrence/id (:occurrence value)] :schedule.occurrence/batch-ref (random-uuid)}])
            :claim (d/transact conn [{:db/id [:schedule.occurrence/id (:occurrence value)] :schedule.occurrence/claim (random-uuid)}])
            :revision (d/transact conn [{:db/id [:schedule.activation/id (:activation value)] :schedule.activation/revision (random-uuid)}])
            :pointer (d/transact conn [{:db/id [:schedule.occurrence/id (:occurrence value)] :schedule.occurrence/request (random-uuid)}])
            nil)
          (let [actual (cond-> f
                         (= change :client) (assoc-in [:request :work.request/client-id] (random-uuid))
                         (= change :party) (assoc-in [:request :work.request/by] (random-uuid)))]
            (is (some? (held #(joint! conn actual))) (str change)))
          (is (nil? (requests/get-request conn (:work.request/id request))))
          (is (nil? (:knowledge.batch/request (batches/batch @conn (get-in value [:batch :batch]))))))))))

(deftest joint-request-lost-ack-preserves-complete-inspectable-linkage
  (with-db
    (fn [conn]
      (let [{:keys [value anchor intent request] :as f} (request-fixture conn)
            transact! d/transact lost (ex-info "Lost joint request ACK" {})]
        (with-redefs [d/transact (fn [c tx] (transact! c tx) (throw lost))]
          (is (identical? lost (try (joint! conn f) nil (catch Exception error error)))))
        (is (= value (binding/linked! @conn anchor request intent)))
        (is (some? (held #(joint! conn f))))
        ;; Missing occurrence linkage is not an invitation to repair it.
        (d/transact conn [[:db/retract [:schedule.occurrence/id (:occurrence value)] :schedule.occurrence/request (:work.request/id request)]])
        (is (= :partial-request-link (held #(binding/linked! @conn anchor request intent))))
        (is (some? (held #(joint! conn f))))))))


(deftest busy-and-empty-skips-release-only-the-empty-claimed-slot
  (doseq [reason [:stream-busy :no-inputs] paused? [false true]]
    (with-db
      (fn [conn]
        (let [value (seed! conn) a (schedules/activation conn (:activation value))]
          (when paused?
            (d/transact conn [{:db/id (:db/id a) :schedule.activation/enabled? false}]))
          (schedules/skip-batch! conn a (:occurrence value) (:claim value) reason)
          (let [after (schedules/activation conn (:activation value))
                o (schedules/occurrence conn (:occurrence value))]
            (is (= :skipped (:schedule.occurrence/state o)))
            (is (= reason (:schedule.occurrence/reason o)))
            (is (= (not paused?) (:schedule.activation/enabled? after)))
            (is (nil? (:schedule.activation/active-occurrence after)))
            (is (nil? (:schedule.activation/last-processed-input after)))
            (is (= :ready (:knowledge.batch/state (batches/batch @conn (get-in value [:batch :batch])))))))))))

(deftest bound-or-changed-occurrences-cannot-be-skipped-as-busy-or-empty
  (doseq [damage [:bound :partial-binding :request :legacy-input :claim :revision :active :owner :deployment]]
    (with-db
      (fn [conn]
        (let [value (seed! conn) a (schedules/activation conn (:activation value))
              oid (:occurrence value)]
          (case damage
            :bound (binding/retain! conn value)
            :partial-binding (d/transact conn [{:db/id [:schedule.occurrence/id oid] :schedule.occurrence/batch-ref (random-uuid)}])
            :request (d/transact conn [{:db/id [:schedule.occurrence/id oid] :schedule.occurrence/request (random-uuid)}])
            :legacy-input (d/transact conn [{:db/id [:schedule.occurrence/id oid] :schedule.occurrence/input-id (random-uuid)}])
            :claim (d/transact conn [{:db/id [:schedule.occurrence/id oid] :schedule.occurrence/claim (random-uuid)}])
            :revision (d/transact conn [{:db/id (:db/id a) :schedule.activation/revision (random-uuid)}])
            :active (d/transact conn [{:db/id (:db/id a) :schedule.activation/active-occurrence (random-uuid)}])
            :owner (d/transact conn [{:db/id (:db/id a) :schedule.activation/by (random-uuid)}])
            :deployment (d/transact conn [{:db/id (:db/id a) :schedule.activation/target-ref (random-uuid)}]))
          (let [before @conn]
            (is (thrown? Exception (schedules/skip-batch! conn a oid (:claim value) :stream-busy)))
            (is (= (schedules/occurrence (atom before) oid) (schedules/occurrence conn oid)))
            (is (= (schedules/activation (atom before) (:activation value))
                   (schedules/activation conn (:activation value))))))))))


(deftest either-missing-direction-of-a-retained-binding-is-held
  (doseq [side [:batch :occurrence]]
    (with-db
      (fn [conn]
        (let [value (seed! conn) o (binding/retain! conn value) bid (get-in value [:batch :batch])]
          (is (= (:occurrence value) (:knowledge.batch/scheduled-occurrence (batches/batch @conn bid))))
          (d/transact conn
            [(if (= :batch side)
               [:db/retract [:knowledge.batch/id bid] :knowledge.batch/scheduled-occurrence (:occurrence value)]
               [:db/retract (:db/id o) :schedule.occurrence/knowledge-batch bid])])
          (is (= :partial-binding (held #(binding/bound-occurrence @conn bid))))
          (is (= :partial-binding (held #(binding/retain! conn value)))))))))


(deftest failed-admission-writes-are-serialized-and-cannot-overwrite-terminal-or-new-work
  (doseq [damage [:none :terminal :new-active :new-revision :changed-claim :request]]
    (with-db
      (fn [conn]
        (let [value (seed! conn) a (schedules/activation conn (:activation value)) oid (:occurrence value)]
          (case damage
            :none nil
            :terminal (d/transact conn [{:db/id [:schedule.occurrence/id oid] :schedule.occurrence/state :skipped
                                        :schedule.occurrence/reason :batch-abandoned}
                                       [:db/retract (:db/id a) :schedule.activation/active-occurrence oid]])
            :new-active (d/transact conn [{:db/id (:db/id a) :schedule.activation/active-occurrence (random-uuid)}])
            :new-revision (d/transact conn [{:db/id (:db/id a) :schedule.activation/revision (random-uuid)}])
            :changed-claim (d/transact conn [{:db/id [:schedule.occurrence/id oid] :schedule.occurrence/claim (random-uuid)}])
            :request (requests/claim! conn {:party (:by value) :room (:room value) :client-id oid :kind :task :intent {}}))
          (let [before-o (schedules/occurrence conn oid) before-a (schedules/activation conn (:activation value))]
            (schedules/interrupt-owned! conn a oid (:claim value) :restart)
            (if (= :none damage)
              (do (is (= :interrupted (:schedule.occurrence/state (schedules/occurrence conn oid))))
                  (is (false? (:schedule.activation/enabled? (schedules/activation conn (:activation value))))))
              (do (is (= before-o (schedules/occurrence conn oid)) (str damage))
                  (is (= before-a (schedules/activation conn (:activation value))) (str damage))))))))))

(deftest stale-outer-dispatch-errors-cannot-disable-a-later-activation-slot
  (with-db
    (fn [conn]
      (let [value (seed! conn) a (schedules/activation conn (:activation value))]
        (d/transact conn [[:db/retract (:db/id a) :schedule.activation/active-occurrence (:occurrence value)]])
        (schedules/block-owned! conn a :owned-dispatch-unavailable)
        (is (true? (:schedule.activation/enabled? (schedules/activation conn (:activation value)))))
        (let [current (schedules/activation conn (:activation value))]
          (schedules/block-owned! conn current :owned-dispatch-unavailable)
          (is (false? (:schedule.activation/enabled? (schedules/activation conn (:activation value))))))))))
