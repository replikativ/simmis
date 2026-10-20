(ns is.simm.ops.file-output-finalization-test
  "Synthetic control-journal receipt tests, NOT evidence of native publication.
   The production owner must obtain winning receipts from the native writer."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.model.file-adoptions :as adoptions]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.file-output-adoption-test :as fixture]
            [is.simm.ops.file-output-dispatch-test :as dispatch]
            [is.simm.ops.file-output-storage :as storage]
            [is.simm.ops.knowledge-filing :as filing]
            [is.simm.ops.proposals :as proposals]))

(defn- with-applying [noop? f]
  (@#'fixture/with-prepared-claim
   (fn [{:keys [conn party room-id room prepared] :as state}]
     (let [claimed (adoption/claim! conn party room-id room prepared)
           candidate (cond-> (@#'dispatch/candidate claimed)
                       noop? (assoc :publication/noop? true)
                       noop? (dissoc :publication/commit))
           applying (adoption/applying! conn party room-id room claimed candidate)
           winning (update candidate :publication/entries
                           #(mapv (fn [entry]
                                    (assoc entry :publication.entry/content
                                           {:geschichte.content/id (second (:publication.entry/content entry))})) %))]
       (f (assoc state :applying applying :winning winning))))))

(defn- finish! [{:keys [conn applying winning]}]
  (adoption/finalize! conn applying winning))

(defn- journal [{:keys [conn applying]}]
  (adoptions/operation @conn (get-in applying [:operation :id])))

(deftest original-observed-receipt-finalizes-all-three-records-atomically
  (doseq [noop? [false true]]
    (with-applying noop?
      (fn [{:keys [conn proposal party workspace applying] :as state}]
        (let [before @workspace
              result (with-redefs [storage/read-file-in (fn [& _] (throw (AssertionError. "Body IO")))
                                   filing/current-target (fn [& _] (throw (AssertionError. "Target IO")))]
                       (finish! state))
              operation (:operation result)
              p (proposals/get-proposal proposal) fork (first (:proposal/forks p))]
          (is (= {:status :accepted :proposal proposal} (dissoc result :operation)))
          (is (= operation (journal state)))
          (is (= :accepted (:state operation) (:proposal/status p) (:proposal.fork/status fork)))
          (is (= party (:actor operation) (:proposal/resolved-by p) (:proposal/resolved-by fork)))
          (is (= (:updated-at operation) (:proposal/resolved-at p) (:proposal/resolved-at fork)))
          (is (= noop? (:noop? operation)))
          (is (= (not noop?) (contains? operation :commit)))
          (is (uuid? (:head operation)))
          (is (not (contains? operation :reconciled-by)))
          (is (= before @workspace))
          (let [accepted @conn]
            (is (thrown? Exception (finish! state)))
            (is (= accepted @conn)))
          (is (= :applying (get-in applying [:operation :state])) "original ACK value stays immutable"))))))

(deftest recording-an-already-observed-fact-does-not-request-new-permission
  (with-applying false
    (fn [{:keys [conn room-id workspace] :as state}]
      (d/transact conn [[:db/retract [:room/id room-id] :room/owner]
                        [:db/retract [:room/id room-id] :room/parties]])
      (let [before @workspace]
        (with-redefs [adoption/snapshot (fn [& _] (throw (AssertionError. "Fresh merge authorization")))
                      filing/current-target (fn [& _] (throw (AssertionError. "Changed default lookup")))]
          (is (= :accepted (:status (finish! state)))))
        (is (= before @workspace))))))

(deftest invalid-receipts-refuse-before-any-control-transaction
  (with-applying false
    (fn [{:keys [conn applying winning]}]
      (let [before @conn calls (atom 0)]
        (with-redefs [d/transact (fn [& _] (swap! calls inc))]
          (doseq [bad [(assoc winning :publication/actor (random-uuid))
                       (assoc winning :publication/head (random-uuid))
                       (assoc winning :publication/noop? true)
                       (assoc-in winning [:publication/entries 0 :publication.entry/path] "/different.md")
                       (assoc-in winning [:publication/entries 0 :publication.entry/content]
                                 [:geschichte.content/id (random-uuid)])]]
            (is (thrown? clojure.lang.ExceptionInfo (adoption/finalize! conn applying bad)))))
        (is (zero? @calls))
        (is (= before @conn))))))

(deftest changed-filing-or-journal-never-becomes-an-abort-or-a-target-replay
  (doseq [change [:filing :body-ref :latest-link :token :shared-owner]]
    (with-applying false
      (fn [{:keys [conn proposal workspace applying] :as state}]
        (let [fork-eid (get-in applying [:proof :fork :db/id])
              component (get-in applying [:operation :component])
              op-id (get-in applying [:operation :id])]
          (case change
            :filing (d/transact conn [[:db/add [:proposal/id proposal] :proposal/filing-hash (apply str (repeat 64 "a"))]])
            :body-ref (let [manifest-eid (:e (first (d/datoms @conn :avet :file-output/id component)))
                            entry-eid (:v (first (d/datoms @conn :eavt manifest-eid :file-output/entries)))]
                        (d/transact conn [[:db/add entry-eid :file-output.entry/body-ref (random-uuid)]]))
            :latest-link (d/transact conn [[:db/retract fork-eid :proposal.fork/file-adoption]])
            :token (d/transact conn [[:db/add [:file.adoption/id op-id] :file.adoption/token (random-uuid)]])
            :shared-owner (d/transact conn [{:proposal/id (random-uuid) :proposal/room (get-in applying [:operation :room])
                                             :proposal/forks [fork-eid]}]))
          (let [before @conn target @workspace]
            (is (thrown? Exception (finish! state)))
            (is (= before @conn))
            (is (= target @workspace))
            (is (= :open (:proposal/status (proposals/get-proposal proposal))))))))))

(deftest failed-control-composition-cannot-partially-accept
  (with-applying false
    (fn [{:keys [conn workspace] :as state}]
      (let [before @conn target @workspace transition adoptions/transition-tx-data]
        (with-redefs [adoptions/transition-tx-data
                      (fn [& args]
                        (conj (apply transition args)
                              [:db/add (get-in state [:applying :proof :fork :db/id]) :test/unknown-finalization true]))]
          (is (thrown? Exception (finish! state))))
        (is (= before @conn))
        (is (= :applying (:state (journal state))))
        (is (= target @workspace))))))

(deftest missing-stale-and-lost-finalization-acks-do-not-read-back-success
  (doseq [mode [:missing :stale :lost]]
    (with-applying false
      (fn [{:keys [conn proposal workspace applying] :as state}]
        (let [transact d/transact read-operation adoptions/operation
              committed? (atom false) reads (atom 0) target @workspace
              error (with-redefs [d/transact (fn [& args]
                                               (let [report (apply transact args)]
                                                 (reset! committed? true)
                                                 (case mode
                                                   :missing (dissoc report :db-after)
                                                   :stale (assoc report :db-after (:db-before report))
                                                   :lost (throw (ex-info "Lost finalization ACK" {})))))
                                  adoptions/operation (fn [& args]
                                                        (when @committed? (swap! reads inc))
                                                        (apply read-operation args))]
                      (try (finish! state) nil (catch clojure.lang.ExceptionInfo error error)))]
          (is (= ::adoption/finalization-unacknowledged (:type (ex-data error))))
          (is (= (get-in applying [:operation :id]) (:operation (ex-data error))))
          (is (not (contains? (ex-data error) :token)))
          (when (#{:missing :lost} mode) (is (zero? @reads)))
          (is (= :accepted (:state (journal state))))
          (is (= :accepted (:proposal/status (proposals/get-proposal proposal))))
          (is (= target @workspace)))))))

(deftest terminal-ack-needs-exact-journal-owner-and-both-projections
  (doseq [change [:proposal-status :fork-status :actor :time :link :owner :journal]]
    (with-applying false
      (fn [{:keys [conn proposal applying workspace] :as state}]
        (let [transact d/transact target @workspace
              fork-eid (get-in applying [:proof :fork :db/id])
              operation (:operation applying)
              error (with-redefs [d/transact
                                  (fn [& args]
                                    (let [report (apply transact args)
                                          facts (case change
                                                  :proposal-status [[:db/add [:proposal/id proposal] :proposal/status :open]]
                                                  :fork-status [[:db/add fork-eid :proposal.fork/status :dismissed]]
                                                  :actor [[:db/add fork-eid :proposal/resolved-by (random-uuid)]]
                                                  :time [[:db/add fork-eid :proposal/resolved-at (java.util.Date. 0)]]
                                                  :link [[:db/retract fork-eid :proposal.fork/file-adoption]]
                                                  :owner [{:proposal/id (random-uuid) :proposal/room (:room operation)
                                                           :proposal/forks [fork-eid]}]
                                                  :journal [[:db/add [:file.adoption/id (:id operation)]
                                                             :file.adoption/head (random-uuid)]])]
                                      ;; Only corrupt the acknowledged immutable
                                      ;; report, never the actual control store.
                                      (assoc report :db-after (:db-after (d/with (:db-after report) facts)))))]
                      (try (finish! state) nil (catch clojure.lang.ExceptionInfo error error)))]
          (is (= ::adoption/finalization-unacknowledged (:type (ex-data error))))
          (is (= :accepted (:state (journal state))))
          (is (= :accepted (:proposal/status (proposals/get-proposal proposal))))
          (is (= target @workspace)))))))
