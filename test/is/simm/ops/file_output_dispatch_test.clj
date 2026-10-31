(ns is.simm.ops.file-output-dispatch-test
  "Internal control-journal callback tests, NOT native target publication."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.model.file-adoptions :as adoptions]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.file-output-adoption-test :as fixture]
            [is.simm.ops.file-output-publication :as publication]
            [is.simm.ops.file-output-storage :as storage]
            [is.simm.ops.knowledge-filing :as filing]))

(defn- with-claimed [f]
  (@#'fixture/with-prepared-claim
   (fn [{:keys [conn party room-id room prepared] :as state}]
     (f (assoc state :claimed (adoption/claim! conn party room-id room prepared))))))

(defn- candidate [{:keys [operation plan]}]
  (let [commit (random-uuid) manifest (:manifest plan)]
    (merge (publication/identity operation manifest)
           {:publication/commit commit :publication/head commit :publication/noop? false
            :publication/created-at (java.util.Date.)
            :publication/entries
            (mapv (fn [[path {:keys [after]}]]
                    {:publication.entry/path (str "/" path) :publication.entry/state :present
                     :publication.entry/content [:geschichte.content/id (:content after)]
                     :publication.entry/size (:bytes after) :publication.entry/mode (:mode after)})
                  (:entries manifest))})))

(defn- applying! [{:keys [conn party room-id room claimed]}]
  (adoption/applying! conn party room-id room claimed (candidate claimed)))

(deftest dispatch-journal-transition-needs-its-own-ack-and-never-writes-target
  (with-claimed
    (fn [{:keys [conn party room-id room claimed workspace] :as state}]
      ;; Invalid candidates cannot submit a transition. Reuse this original
      ;; acknowledged claim, then exercise its real positive transition below.
      (let [before @conn calls (atom 0)]
        (with-redefs [d/transact (fn [& _] (swap! calls inc))]
          (is (thrown? clojure.lang.ExceptionInfo
                       (adoption/applying! conn party room-id room claimed
                                           (assoc (candidate claimed) :publication/actor (random-uuid))))))
        (is (zero? @calls))
        (is (= before @conn)))
      (let [before @workspace result (with-redefs [storage/read-file-in
                                                   (fn [& _] (throw (AssertionError. "Body IO in journal transition")))]
                                       (applying! state))
            operation (:operation result)]
        (is (= :applying (:state operation)))
        (is (= (:id (:operation claimed)) (:id operation)))
        (is (= operation (adoptions/operation @conn (:id operation))))
        (is (= (dissoc claimed :operation) (dissoc result :operation)))
        (is (thrown? Exception (applying! state)))
        (is (= before @workspace))))))

(deftest applying-rechecks-authority-and-target-at-the-serialized-boundary
  (with-claimed
    (fn [{:keys [conn party room-id claimed basis] :as state}]
      (let [original (:operation claimed) fork (get-in claimed [:proof :fork :db/id])]
        (is (= original (adoptions/linked-operation! @conn fork original)))
        (with-redefs [filing/current-target (fn [& _] (assoc (:target basis) :store (random-uuid)))]
          (let [before @conn]
            (is (thrown? Exception (applying! state)))
            (is (= before @conn))))
        (is (= original (adoptions/linked-operation! @conn fork original)))
        ;; Real revocation is last; no later case borrows revoked authority.
        (d/transact conn [[:db/retract [:room/id room-id] :room/owner]])
        (let [before @conn]
          (is (thrown? Exception (applying! state)))
          (is (= before @conn)))
        (is (= :claimed (:state (adoptions/operation @conn (:id original)))))))))

(deftest missing-stale-and-lost-applying-acks-never-read-back-ownership
  (doseq [mode [:missing :stale :lost]]
    (with-claimed
      (fn [{:keys [conn claimed workspace] :as state}]
        (let [transact d/transact linked adoptions/linked-operation!
              before @workspace reads (atom 0) committed? (atom false)
              error (with-redefs [d/transact (fn [& args]
                                               (let [report (apply transact args)]
                                                 (reset! committed? true)
                                                 (case mode
                                                   :missing (dissoc report :db-after)
                                                   :stale (assoc report :db-after (:db-before report))
                                                   :lost (throw (ex-info "Injected lost ACK" {})))))
                                  adoptions/linked-operation!
                                  (fn [& args]
                                    (when @committed? (swap! reads inc))
                                    (apply linked args))]
                      (try (applying! state) nil
                           (catch clojure.lang.ExceptionInfo error error)))
              id (get-in claimed [:operation :id])]
          (is (= ::adoption/applying-unacknowledged (:type (ex-data error))))
          (is (= id (:operation (ex-data error))))
          (is (not (contains? (ex-data error) :token)))
          (when (#{:missing :lost} mode) (is (zero? @reads)))
          (is (= :applying (:state (adoptions/operation @conn id))))
          (is (= before @workspace)))))))

(deftest interrupted-dispatch-transition-retains-interrupt-and-never-returns-ownership
  (doseq [committed? [false true]]
    (with-claimed
      (fn [{:keys [conn claimed workspace] :as state}]
        (let [transact d/transact result (promise) before @workspace
              worker (Thread.
                      ^Runnable
                      (fn []
                        (with-redefs [d/transact (fn [& args]
                                                   (when committed? (apply transact args))
                                                   (throw (InterruptedException. "Injected transition interruption")))]
                          (try (applying! state) (deliver result ::unexpected)
                               (catch clojure.lang.ExceptionInfo error
                                 (deliver result {:type (:type (ex-data error))
                                                  :operation (:operation (ex-data error))
                                                  :interrupted? (.isInterrupted (Thread/currentThread))}))))))]
          (.start worker)
          (.join worker 10000)
          (is (not (.isAlive worker)))
          (let [answer (deref result 1000 ::timeout) id (get-in claimed [:operation :id])]
            (is (= ::adoption/applying-unacknowledged (:type answer)))
            (is (= id (:operation answer)))
            (is (true? (:interrupted? answer)))
            (is (= (if committed? :applying :claimed) (:state (adoptions/operation @conn id)))))
          (is (= before @workspace)))))))
